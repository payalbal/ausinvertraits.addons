## fig_generate_phylogenies.R
##
## Builds a family-level phylogenetic tree for Australian invertebrate data using
## the Open Tree of Life (via the `rotl` package) as the backbone, then grafting
## each order's constituent families as star polytomies.
##
## This mirrors the V.PhyloMaker approach used for the AusTraits plant figure:
##   - V.PhyloMaker  : GBOTB.extended backbone  → species placed within families
##   - This script   : OTL backbone (orders)     → families placed within orders
##
## Requires an internet connection for the OTL query.
##
## Output: phylogeny_data.RData
##   tax_tree      – ape phylo; tips = families, branching from OTL
##   trait_matrix  – family × trait-category record counts
##   order_node_df – order → MRCA node number (for geom_cladelabel)
##   fam_ord       – family–order lookup

## install.packages(c("ape", "rotl"))  # uncomment if needed
library(tidyverse)
library(ape)
library(rotl)


##### 1. Read and clean data #####################################################

traits <- read_csv("traits_table.csv", show_col_types = FALSE) %>%
  filter(
    !is.na(family), family != "NA",
    !is.na(order),  order  != "NA",  order != "Order",
    !is.na(trait_parent_categories), trait_parent_categories != "NA"
  )

trait_counts <- traits %>%
  group_by(family, trait_parent_categories) %>%
  summarise(records_per_family = n(), .groups = "drop") %>%
  ungroup()

fam_ord <- traits %>%
  distinct(family, order) %>%
  arrange(order, family)

orders <- sort(unique(fam_ord$order))


##### 2. Query Open Tree of Life for order-level backbone ########################
##
## tnrs_match_names() maps our order names to OTL taxonomy ids (ott_id).
## tol_induced_subtree() returns the OTL consensus tree for those ids.
##
## Flag policy: only exclude taxa flagged as "barren" (no OTL descendants),
## "extinct", or "merged" (subsumed into another taxon).  Minor flags such as
## "sibling_higher", "homonym", "incertae_sedis_inherited" are acceptable.

message("Querying Open Tree of Life for ", length(orders), " orders ...")

otl_matched <- rotl::tnrs_match_names(
  names                   = orders,
  context_name            = "Animals",
  do_approximate_matching = TRUE
)

## Convert flags to a safe character vector (rotl sometimes returns a list column)
otl_matched$flags_str <- sapply(
  seq_len(nrow(otl_matched)),
  function(i) {
    f <- otl_matched$flags[[i]]
    if (is.null(f) || length(f) == 0) "" else paste(unlist(f), collapse = ",")
  }
)

## Print full matching results for diagnosis
message("\n--- OTL matching results ---")
for (i in seq_len(nrow(otl_matched))) {
  message(sprintf(
    "  %-20s  ott_id=%-10s  score=%.2f  unique_name=%-25s  flags=%s",
    otl_matched$search_string[i],
    ifelse(is.na(otl_matched$ott_id[i]), "NA", otl_matched$ott_id[i]),
    ifelse(is.na(otl_matched$score[i]), 0, otl_matched$score[i]),
    ifelse(is.na(otl_matched$unique_name[i]), "NA", otl_matched$unique_name[i]),
    ifelse(otl_matched$flags_str[i] == "", "(none)", otl_matched$flags_str[i])
  ))
}
message("----------------------------\n")

## Keep only usable matches: have an ott_id, not barren/extinct/merged
bad_flag_pattern <- "barren|extinct|merged|suppressed"

good <- otl_matched %>%
  filter(!is.na(ott_id)) %>%
  filter(
    flags_str == "" |
      !grepl(bad_flag_pattern, flags_str, ignore.case = TRUE)
  )

## NOTE: good$search_string is lowercase (TNRS normalises input); orders is title-case.
## Use case-insensitive comparison throughout.
missed_orders <- orders[!tolower(orders) %in% good$search_string]

message("  Matched / usable : ", nrow(good), " orders")
message("  Unmatched/flagged: ", length(missed_orders),
        if (length(missed_orders) > 0)
          paste0("  (", paste(missed_orders, collapse = ", "), ")")
        else "")


##### 3. Retrieve OTL backbone tree ##############################################

backbone <- rotl::tol_induced_subtree(
  ott_ids      = rotl::ott_id(good),
  label_format = "name"
)

## ── Diagnose backbone BEFORE label cleaning ───────────────────────────────────
message("\n--- Backbone tip labels (raw) ---")
message(paste(backbone$tip.label, collapse = "\n"))
message("---------------------------------\n")

## Clean tip labels – rotl sometimes appends _ott12345 and uses underscores
backbone$tip.label <- gsub("_ott[0-9]+$", "", backbone$tip.label)
backbone$tip.label <- gsub("_",            " ", backbone$tip.label)

## Map OTL labels back to our exact (title-case) order names.
## good$search_string is LOWERCASE (TNRS normalises input); orders is title-case.
## Build a lowercase → title-case lookup first, then compose with name_map.
lc_to_titlecase <- setNames(orders, tolower(orders))      # "amphipoda" → "Amphipoda"
name_map <- setNames(
  lc_to_titlecase[good$search_string],                    # → title-case order name
  tolower(good$unique_name)                               # key: OTL canonical name
)

backbone$tip.label <- ifelse(
  tolower(backbone$tip.label) %in% names(name_map),
  name_map[tolower(backbone$tip.label)],
  backbone$tip.label
)

## Check how many backbone tips now match a known order name
matched_tips   <- backbone$tip.label[backbone$tip.label %in% orders]
unmatched_tips <- backbone$tip.label[!backbone$tip.label %in% orders]

message("--- Backbone tip labels (after cleaning) ---")
message("  Tips matching our order names : ", length(matched_tips),
        "  [", paste(matched_tips, collapse = ", "), "]")
if (length(unmatched_tips) > 0) {
  message("  Tips NOT matching order names : ", length(unmatched_tips),
          "  [", paste(unmatched_tips, collapse = ", "), "]")
  message("  (These unmatched tips are likely OTL intermediate nodes or synonyms.)")
  message("  They will be pruned before grafting.")
}
message("--------------------------------------------\n")

message("Backbone: ", ape::Ntip(backbone), " tips, ",
        ape::Nnode(backbone), " internal nodes")


##### 4. Prune backbone to keep only tips that match our order names #############
##
## tol_induced_subtree() may insert intermediate OTL nodes (e.g. class-level
## nodes) as extra tips when an input taxon is nested within another.  Drop
## those here so the backbone only has our order-level tips.

tips_to_drop <- backbone$tip.label[!backbone$tip.label %in% orders]
if (length(tips_to_drop) > 0) {
  message("Pruning ", length(tips_to_drop), " non-order tip(s): ",
          paste(tips_to_drop, collapse = ", "))
  backbone <- ape::drop.tip(backbone, tips_to_drop)
  message("Backbone after pruning: ", ape::Ntip(backbone), " tips")
}

## Any orders represented in the backbone are now confirmed; re-derive missed
## orders accordingly (backbone tips are now title-case, so direct comparison works).
matched_in_backbone <- backbone$tip.label[backbone$tip.label %in% orders]
missed_orders       <- orders[!orders %in% backbone$tip.label]

message("Orders in backbone   : ", length(matched_in_backbone))
message("Orders still missing : ", length(missed_orders),
        if (length(missed_orders) > 0)
          paste0("  (", paste(missed_orders, collapse = ", "), ")")
        else "")


##### 5. Set branch lengths ######################################################
##
## OTL trees are usually topology-only (no branch lengths).
## Apply Grafen's method then scale so all order tips sit at depth 8.
## Families will be added with branch length 1 → total tip depth = 9.

if (is.null(backbone$edge.length)) {
  message("No branch lengths in OTL tree – computing via Grafen's method")
  backbone <- ape::compute.brlen(backbone, method = "Grafen")
}

tip_depths <- ape::node.depth.edgelength(backbone)[seq_len(ape::Ntip(backbone))]
backbone$edge.length <- backbone$edge.length * (8 / max(tip_depths))

message("Backbone scaled: tip depth range = ",
        round(min(ape::node.depth.edgelength(backbone)[seq_len(ape::Ntip(backbone))]), 3),
        " – ",
        round(max(ape::node.depth.edgelength(backbone)[seq_len(ape::Ntip(backbone))]), 3))


##### 6. Graft families onto each order tip ######################################
##
## For single-family orders: rename the tip AND extend its branch by +1 so the
## family sits at depth 9 (matching multi-family tips).
##
## For multi-family orders: query OTL for a family-level induced subtree,
## scale it so all resolved family tips are at depth 1, then append any
## families absent from OTL as star-polytomy branches (also at depth 1) before
## binding the whole subtree onto the order tip.  This gives every family tip
## a total root-to-tip depth of 9.
##
## Falls back to a full star polytomy if OTL returns <2 usable families for an
## order, or if any OTL call errors.

tax_tree <- backbone

for (ord in matched_in_backbone) {

  fams    <- fam_ord$family[fam_ord$order == ord]
  tip_idx <- which(tax_tree$tip.label == ord)

  if (length(tip_idx) == 0) {
    message("  WARNING: tip '", ord, "' not found – skipping")
    next
  }

  ## ── (a) Single-family order ──────────────────────────────────────────────────
  if (length(fams) == 1) {
    edge_idx <- which(tax_tree$edge[, 2] == tip_idx)
    if (length(edge_idx) > 0)
      tax_tree$edge.length[edge_idx] <- tax_tree$edge.length[edge_idx] + 1
    tax_tree$tip.label[tip_idx] <- fams[1]
    next
  }

  ## ── (b) Multi-family order: query OTL for family topology ────────────────────
  message("  Querying OTL families for ", ord,
          " (", length(fams), " families) ...")

  ## Helper: fall back to star polytomy and graft, then skip to next order
  use_star <- function() {
    s             <- ape::stree(length(fams), type = "star", tip.label = fams)
    s$edge.length <- rep(1, nrow(s$edge))
    tax_tree      <<- ape::bind.tree(tax_tree, s, where = tip_idx, position = 0)
  }

  fam_matched <- tryCatch(
    rotl::tnrs_match_names(
      names                   = fams,
      context_name            = "Animals",
      do_approximate_matching = TRUE
    ),
    error = function(e) {
      message("    TNRS failed: ", conditionMessage(e), " – using star polytomy")
      NULL
    }
  )
  if (is.null(fam_matched)) { use_star(); next }

  ## Safe flags column
  fam_matched$flags_str <- sapply(seq_len(nrow(fam_matched)), function(i) {
    f <- fam_matched$flags[[i]]
    if (is.null(f) || length(f) == 0) "" else paste(unlist(f), collapse = ",")
  })

  lc_fams_map <- setNames(fams, tolower(fams))   # "acrididae" → "Acrididae"

  fam_good <- fam_matched %>%
    filter(!is.na(ott_id)) %>%
    filter(flags_str == "" |
             !grepl(bad_flag_pattern, flags_str, ignore.case = TRUE))

  message("    OTL matched: ", nrow(fam_good), " / ", length(fams), " families")

  if (nrow(fam_good) < 2) { use_star(); next }

  fam_raw <- tryCatch(
    rotl::tol_induced_subtree(
      ott_ids      = rotl::ott_id(fam_good),
      label_format = "name"
    ),
    error = function(e) {
      message("    tol_induced_subtree failed: ", conditionMessage(e),
              " – using star polytomy")
      NULL
    }
  )
  if (is.null(fam_raw)) { use_star(); next }

  ## Clean labels (same pattern as order backbone)
  fam_raw$tip.label <- gsub("_ott[0-9]+$", "", fam_raw$tip.label)
  fam_raw$tip.label <- gsub("_", " ",           fam_raw$tip.label)

  fam_name_map <- setNames(
    lc_fams_map[fam_good$search_string],   # → title-case family names
    tolower(fam_good$unique_name)           # key: OTL canonical (lowercased)
  )
  fam_raw$tip.label <- ifelse(
    tolower(fam_raw$tip.label) %in% names(fam_name_map),
    fam_name_map[tolower(fam_raw$tip.label)],
    fam_raw$tip.label
  )

  ## Prune OTL intermediate nodes that don't match our family names
  non_fam <- fam_raw$tip.label[!fam_raw$tip.label %in% fams]
  if (length(non_fam) == ape::Ntip(fam_raw)) {
    message("    No family tips survived label mapping – using star polytomy")
    use_star(); next
  }
  if (length(non_fam) > 0)
    fam_raw <- ape::drop.tip(fam_raw, non_fam)

  ## Scale so every resolved family tip is at depth 1 from the subtree root
  if (is.null(fam_raw$edge.length))
    fam_raw <- ape::compute.brlen(fam_raw, method = "Grafen")
  fam_d <- ape::node.depth.edgelength(fam_raw)[seq_len(ape::Ntip(fam_raw))]
  fam_raw$edge.length <- fam_raw$edge.length / max(fam_d)

  ## Append unresolved families at the subtree root with edge length 1.0.
  ## After scaling, all resolved tips are at depth 1, so unresolved tips
  ## (edge = 1 from root) will match exactly.
  resolved   <- fam_raw$tip.label
  unresolved <- fams[!fams %in% resolved]

  if (length(unresolved) > 0) {
    message("    Resolved: ", length(resolved),
            "  |  adding ", length(unresolved), " unresolved as polytomy")
    inner       <- sub("^\\((.*)\\)[^;]*;$", "\\1", ape::write.tree(fam_raw))
    unres_parts <- paste(paste0(unresolved, ":1"), collapse = ",")
    fam_subtree <- ape::read.tree(text = paste0("(", inner, ",", unres_parts, ");"))
  } else {
    message("    Resolved: ", length(resolved), " / ", length(fams),
            " families in OTL topology")
    fam_subtree <- fam_raw
  }

  ## Bind the family subtree onto the order tip in the backbone
  tax_tree <- ape::bind.tree(tax_tree, fam_subtree, where = tip_idx, position = 0)
}


##### 7. Handle unmatched orders #################################################
##
## Orders that OTL couldn't place are added to the root of the expanded tree
## as extra polytomy branches, all at the same total depth (9) as other families.
## This is the conservative equivalent of V.PhyloMaker's "S2" placement.

if (length(missed_orders) > 0) {

  message("Adding ", length(missed_orders), " unmatched orders at root ...")

  ## Build Newick fragment for each missed order
  ## Families get edge length 1; order branch from root = 8 (total depth = 9)
  missed_parts <- sapply(missed_orders, function(ord) {
    fams <- fam_ord$family[fam_ord$order == ord]
    if (length(fams) == 1) {
      paste0(fams[1], ":9")                                 # tip directly from root
    } else {
      fam_str <- paste(paste0(fams, ":1"), collapse = ",")
      paste0("(", fam_str, ")", ord, ":8")                  # order polytomy at root
    }
  })

  ## Extract inner content of the current tree (strip outermost parens + label)
  ## write.tree output format: "(inner_content)root_label:brlen;" or "(inner);"
  current_nwk   <- ape::write.tree(tax_tree)
  current_inner <- sub("^\\((.*)\\)[^;]*;$", "\\1", current_nwk)

  ## Combine: original OTL clades + missed orders all at the same root level
  combined_nwk <- paste0("(",
                          current_inner, ",",
                          paste(missed_parts, collapse = ","),
                          ");")
  tax_tree <- ape::read.tree(text = combined_nwk)

  message("  Added families: ",
          sum(sapply(missed_orders,
                     function(o) length(fam_ord$family[fam_ord$order == o]))))
}


##### 8. Final tree diagnostics ##################################################

tip_d <- ape::node.depth.edgelength(tax_tree)[seq_len(ape::Ntip(tax_tree))]
message("\n--- Final tree summary ---")
message("  Tips         : ", ape::Ntip(tax_tree))
message("  Tip depth    : min = ", round(min(tip_d), 2),
        ",  max = ", round(max(tip_d), 2),
        "  (target: all = 9)")
if (max(tip_d) - min(tip_d) > 0.05) {
  message("  *** WARNING: tip depths vary by > 0.05 – check branch-length scaling ***")
  ## Show the shallowest tips (likely renaming without extension)
  shallow <- tax_tree$tip.label[tip_d < (max(tip_d) - 0.1)]
  if (length(shallow) > 0)
    message("  Shallow tips: ", paste(shallow, collapse = ", "))
}
message("--------------------------\n")


##### 9. Trait matrix ############################################################

trait_matrix <- trait_counts %>%
  pivot_wider(
    names_from  = trait_parent_categories,
    values_from = records_per_family,
    values_fill = NA
  ) %>%
  column_to_rownames("family")

common_families <- tax_tree$tip.label[tax_tree$tip.label %in% rownames(trait_matrix)]
trait_matrix    <- trait_matrix[common_families, , drop = FALSE]


##### 10. Map orders to their MRCA node (for geom_cladelabel in plot script) ####

order_nodes <- lapply(orders, function(ord) {
  fams_in_order <- fam_ord$family[fam_ord$order == ord]
  tips_in_tree  <- fams_in_order[fams_in_order %in% tax_tree$tip.label]

  if (length(tips_in_tree) == 0) {
    node <- NA_integer_
  } else if (length(tips_in_tree) == 1) {
    node <- which(tax_tree$tip.label == tips_in_tree)
  } else {
    node <- ape::getMRCA(tax_tree, tips_in_tree)
  }
  data.frame(order = ord, node = as.integer(node),
             n_families = length(tips_in_tree))
})

order_node_df <- bind_rows(order_nodes)


##### 11. Save ##################################################################

save(tax_tree, trait_matrix, order_node_df, fam_ord,
     file = "phylogeny_data.RData")

message("=== Done ===")
message("Tree tips  : ", ape::Ntip(tax_tree))
message("Tip depths : ", round(min(ape::node.depth.edgelength(tax_tree)[
  seq_len(ape::Ntip(tax_tree))]), 1),
        " – ",
        round(max(ape::node.depth.edgelength(tax_tree)[
          seq_len(ape::Ntip(tax_tree))]), 1))
message("Matrix     : ", paste(dim(trait_matrix), collapse = " x "))
message("Saved      : phylogeny_data.RData")

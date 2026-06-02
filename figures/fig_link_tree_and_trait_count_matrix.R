## fig_link_tree_and_trait_count_matrix.R
## Circular taxonomic tree with trait count heatmap.
##
## Requires: phylogeny_data.RData  (run fig_generate_phylogenies.R first)
## Packages : ggtree, ggplot2, tidyverse, ape

## ── Install missing packages ──────────────────────────────────────────────────
required_pkgs <- c("tidyverse", "ape", "ggtree", "RColorBrewer")
new_pkgs  <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]
cran_pkgs <- new_pkgs[new_pkgs != "ggtree"]
bioc_pkgs <- new_pkgs[new_pkgs == "ggtree"]
if (length(cran_pkgs)) install.packages(cran_pkgs)
if (length(bioc_pkgs)) {
  if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
  BiocManager::install("ggtree")
}

library(tidyverse)
library(ape)
library(ggtree)


## ── 1. Load data ──────────────────────────────────────────────────────────────
load("phylogeny_data.RData")
# Objects: tax_tree, trait_matrix, order_node_df, fam_ord


## ── 2. Order colours: two alternating blues ───────────────────────────────────
## Adjust hex codes here to change the pair of blues
blue_dark  <- "#2166AC"
blue_light <- "#2166AC"

n_orders     <- nrow(order_node_df)
order_colors <- setNames(
  rep(c(blue_dark, blue_light), length.out = n_orders),
  order_node_df$order
)


## ── 3. Base circular tree ─────────────────────────────────────────────────────
base_tree <- ggtree(tax_tree, layout = "circular", size = 0.3, color = "grey50") +
  guides(color = "none")

## rotate_tree angle: moves which order sits at the top of the circle
## open_tree  angle: width (°) of the gap reserved for column-name labels
base_tree <- rotate_tree(base_tree, angle = 185)
base_tree <- open_tree(base_tree,   angle = 20)

## Extract angular positions AFTER rotation + opening (used for hjust below)
tree_layout <- base_tree$data

## Helper: returns hjust=0 (right half) or hjust=1 (left half) for a node,
## so that all order label text radiates OUTWARD regardless of position.
get_hjust <- function(node_id) {
  if (is.na(node_id)) return(0)
  row <- tree_layout[tree_layout$node == node_id, ]
  if (nrow(row) == 0 || is.na(row$angle[1])) return(0)
  ang <- row$angle[1] %% 360
  ifelse(ang > 90 & ang < 270, 1, 0)
}

## Helper: returns the angular position (0–360°) of a node.
## Used to detect top/bottom zones for order label nudging.
get_node_angle <- function(node_id) {
  if (is.na(node_id)) return(NA_real_)
  row <- tree_layout[tree_layout$node == node_id, ]
  if (nrow(row) == 0 || is.na(row$angle[1])) return(NA_real_)
  as.numeric(row$angle[1]) %% 360
}


## ── 4. Highlight order clades with faint blue background ──────────────────────
## Tree is now depth 9 (root→class=5, class→order=3, order→family=1).
## extend must reach past the heatmap outer edge (≈ tip_x + offset + width*9).
for (i in seq_len(nrow(order_node_df))) {
  if (is.na(order_node_df$node[i])) next
  base_tree <- base_tree +
    geom_highlight(
      node   = order_node_df$node[i],
      fill   = order_colors[order_node_df$order[i]],
      alpha  = 0.12,
      extend = 4      # must reach past the heatmap outer edge; increase with width
    )
}


## ── 5. Heatmap ────────────────────────────────────────────────────────────────
## Tips now at depth 9 (root→class=5, class→order=3, order→family=1).
##   offset 0.2  → heatmap inner edge at x ≈ 9.2
##   width  0.40 → heatmap spans 0.40 × 9 = 3.6  → outer edge at x ≈ 12.8
## Increase width to widen the orange bars; then also increase fam_offset and
## clade_offset below to keep family labels and arc bars outside the heatmap.
## colnames_offset_y pushes column names into the open_tree gap

col_order <- c("ecology", "microhabitat", "distribution",
               "movement", "morphology", "physiology",
               "life_history", "demography", "fire_specific")
col_order <- col_order[col_order %in% colnames(trait_matrix)]

trait_matrix_ordered <- trait_matrix[, col_order, drop = FALSE]

col_labels <- c(
  ecology       = "Ecology",
  microhabitat  = "Microhabitat",
  distribution  = "Distribution",
  movement      = "Movement",
  morphology    = "Morphology",
  physiology    = "Physiology",
  life_history  = "Life history",
  demography    = "Demography",
  fire_specific = "Fire"
)
colnames(trait_matrix_ordered) <- col_labels[colnames(trait_matrix_ordered)]

tree_breaks <- c(1, 3, 10, 30, 100, 300)

austraits_tree <- gheatmap(
  base_tree,
  trait_matrix_ordered,
  offset            = 0.2,    # gap between tree tips and heatmap inner edge
  width             = 0.40,   # heatmap radial width as fraction of tree depth
  color             = NA,     # no tile borders
  colnames          = TRUE,
  colnames_angle    = 72,     # vertical text, radiating outward
  colnames_position = "top",  # placed at heatmap outer edge
  colnames_offset_y = 1,     # angular nudge → column names land in the gap
  colnames_offset_x = -.2,     # angular nudge → column names land in the gap
  font.size         = 2.0,
  hjust             = 0
) +
  scale_fill_gradientn(
    trans    = "log1p",
    colours  = c("#fff7ec", "#fec44f", "#fe9929", "#d95f0e", "#7f2704"),
    breaks   = tree_breaks,
    labels   = as.character(tree_breaks),   # must be character, not numeric
    na.value = "grey96",
    name     = "Trait records\n(count)"
  ) +
  theme(
    ## Legend placed inside the figure (bottom-right) to avoid forced whitespace.
    ## Adjust c(x, y) in [0,1] coordinates to reposition.
    legend.position   = c(0.88, 0.07),
    legend.background = element_rect(fill = alpha("white", 0.85), color = NA),
    legend.title      = element_text(size = 9, face = "bold"),
    legend.text       = element_text(size = 8),
    plot.margin       = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
  ) +
  guides(fill = guide_colourbar(
    title.position = "top",
    label.position = "right",
    barwidth       = 4,     # narrower so it doesn't dominate
    barheight      = 0.6
  ))


## ── 6. Family tip labels — only families with > 40 total trait records ─────────
## 60 of 193 families pass this threshold; adjust the cutoff as needed.
n_tips        <- ape::Ntip(tax_tree)
family_totals <- rowSums(trait_matrix, na.rm = TRUE)
families_to_label <- names(family_totals[family_totals > 40])
message("Families labelled (>40 records): ", length(families_to_label),
        " / ", nrow(trait_matrix))

## Blank out tip labels below the threshold.
austraits_tree$data <- austraits_tree$data %>%
  mutate(label = ifelse(
    node <= n_tips & !(label %in% families_to_label),
    NA_character_,
    label
  ))

## Radial family labels.
##
## ggplot_build()$data for geom_tiplab shows ggtree's internal pre-render
## coordinates (x = radius, y = sequential tip index) — NOT Cartesian.
## The Cartesian (x, y) we need are in austraits_tree$data, pre-computed by
## ggtree's layout algorithm.  Label positions are the tip Cartesian coords
## scaled outward by (r + offset) / r.  atan2(y, x) on those Cartesian coords
## gives the true polar angle for text rotation.
##
## fam_offset must clear the heatmap outer edge.
## With width=0.40: outer edge ≈ 12.8, tips at 9 → need offset > 3.8.

fam_offset <- 4.5   # tune: increase if labels still overlap the heatmap

## austraits_tree$data is in ggtree's internal coordinate system:
##   x = depth (radius from root, ~9 for all tips)
##   y = sequential tip index
##   angle = polar angle in degrees (the one we need for rotation)
## ggtree converts (x, y) → circular visual at render time.
## geom_tiplab positions labels as x_lbl = x + fam_offset, y_lbl = y.
## We replicate that and add per-tip display_angle for text rotation.

## Empirically determined mapping (y=1 → 90°, y=193 → 110°, clockwise, 340° arc):
##   visual_angle = (90 - (y - 1) / (n_tips - 1) * (360 - open_angle)) %% 360
## Adjust start_angle (90) if rotate_tree angle changes.
## Adjust open_angle (20) if open_tree angle changes.
start_angle <- 90
open_angle  <- 20

fam_label_data <- austraits_tree$data %>%
  filter(isTip, !is.na(label)) %>%
  select(node, label, x, y) %>%
  mutate(
    x_lbl         = x + fam_offset,
    y_lbl         = y,
    visual_angle  = 90-((start_angle - (y - 1) / (n_tips - 1) * (360 - open_angle))) %% 360,
    eff_angle     = visual_angle %% 360,   # normalise to 0–360 for the flip check
    display_angle = ifelse(eff_angle > 90 & eff_angle < 270,
                           visual_angle + 180, visual_angle),
    display_hjust = ifelse(eff_angle > 90 & eff_angle < 270, 1.0, 0.0)
  )

austraits_tree <- austraits_tree +
  geom_text(
    data        = fam_label_data,
    aes(x = x_lbl, y = y_lbl, label = label,
        angle = display_angle, hjust = display_hjust),
    inherit.aes = FALSE,
    vjust       = 0.5,
    size        = 2.0,
    na.rm       = TRUE
  )

## ── 7. Order arc-labels: hjust based on angular position ──────────────────────
## clade_offset places arc bars beyond the longest family labels.
## With tips at x=9 and labels extending ~4 units, bars at x = 9 + 5.5 = 14.5.
## Increase clade_offset if label text still clips the bars.

clade_offset <- 8.0   # distance from tree tips to arc bar inner edge
## Rule of thumb: clade_offset ≈ fam_offset + ~3 (clears the longest family labels)

for (i in seq_len(nrow(order_node_df))) {
  if (is.na(order_node_df$node[i])) next

  node_id   <- order_node_df$node[i]
  n_fam     <- order_node_df$n_families[i]
  node_ang  <- get_node_angle(node_id)
  
  display_hjust_order <- case_when(
    order_node_df$order[i] %in% c("Araneae", "Cyclopoida", "Mermithida",
                 "Phasmida", "Soricomorpha", "Stylommatophora", "Unionida", "Neritopsina") ~ -0.2,
    order_node_df$order[i] %in% c("Odonata", "Calanoida", "Amphipoda", "Isopoda", "Anaspidacea", "Decapoda") ~ 0,
    order_node_df$order[i] %in% c("Orthoptera", "Blattodea", "Plecoptera", "Ephemeroptera") ~ 1.1,
    order_node_df$order[i] %in% c("Diptera", "Hymenoptera", "Lepidoptera", "Neuroptera") ~ 1.15,
    order_node_df$order[i] %in% c("Hemiptera") ~ 1.3,
    order_node_df$order[i] %in% c("Coleoptera") ~ 0,
    order_node_df$order[i] %in% c("Opiliones") ~ -0.2
    )

  off_text <- case_when(
    order_node_df$order[i] %in% c("Blattodea") ~ 0.25,
    order_node_df$order[i] %in% c("Odonata", "Calanoida", "Amphipoda", "Isopoda", "Anaspidacea", "Decapoda") ~ 0.55,
    order_node_df$order[i] %in% c("Orthoptera", "Blattodea", "Plecoptera", "Ephemeroptera") ~ 0.5,
    order_node_df$order[i] %in% c("Coleoptera") ~ 1.3,
    TRUE ~ 0.25
  )
  
  display_angle_order <- case_when(
    order_node_df$order[i] %in% c("Orthoptera", "Blattodea", "Plecoptera", "Ephemeroptera") ~ 330,
    order_node_df$order[i] %in% c("Odonata", "Calanoida", "Amphipoda", "Isopoda", "Anaspidacea", "Decapoda") ~ 45,
    TRUE ~ 0
  )

  austraits_tree <- austraits_tree +
    geom_cladelabel(
      node        = node_id,
      label       = order_node_df$order[i],   # all orders labelled
      offset      = clade_offset,
      offset.text = off_text,
      barsize     = 2,
      fontsize    = 2.8,
      angle       = display_angle_order,
      hjust       = display_hjust_order,    # radiates outward on both sides of circle
     # vjust  = 2,
      color       = order_colors[order_node_df$order[i]]
    )
}


## ── 8. Save ───────────────────────────────────────────────────────────────────
ggsave(
  filename = "fig_trait_phylogeny.pdf",
  plot     = austraits_tree,
  width    = 18, height = 18, units = "in"
)
ggsave(
  filename = "fig_trait_phylogeny.png",
  plot     = austraits_tree,
  width    = 18, height = 18, units = "in", dpi = 150
)
message("=== Figure saved: fig_trait_phylogeny.pdf / .png ===")

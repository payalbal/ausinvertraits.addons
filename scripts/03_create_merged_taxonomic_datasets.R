## ----------------------------------------------- ##
## OLD - NOT BEING USED ####
## ----------------------------------------------- ##

library(tidyverse)

# thanks to ChatGPT for this function
split_synonym <- function(synonym) {
  # Find positions of uppercase letters
  upper_positions <- unlist(gregexpr("[A-Z]", synonym))
  
  # Check if there are at least two uppercase letters
  if (length(upper_positions) >= 2) {
    split_index <- upper_positions[2]
    name <- substr(synonym, 1, split_index - 1)
    author_year <- substr(synonym, split_index, nchar(synonym))
    return(c(name, author_year))
  } else {
    return(c(synonym, NA) )  # Return NA if less than 2 uppercase letters
  }
}

afd_exact <- afd %>%
  select(accepted_name = FULL_NAME, canonical_name = FULL_NAME, accepted_name_usage_id = CONCEPT_GUID, taxon_id = CONCEPT_GUID, family = FAMILY)

afd_synonyms <- afd %>%
  select(accepted_name = FULL_NAME, synonym = SYNONYMS, accepted_name_usage_id = CONCEPT_GUID, family = FAMILY) %>%
  tidyr::separate_longer_delim(synonym, delim = "; ") %>%
  rowwise() %>%
  mutate(
    split = list(split_synonym(synonym))
  ) %>%
  unnest_wider(split, names_sep = "_") %>%
  rename(canonical_name = split_1, author_year = split_2) %>%
  mutate(
    canonical_name = str_trim(canonical_name),
    taxon_id = ifelse(accepted_name == canonical_name, accepted_name_usage_id, paste0(accepted_name_usage_id, "_", synonym))
    )
  
afd_all <- afd_exact %>%  
  bind_rows(afd_synonyms %>% select(-synonym, -author_year)) %>%
  distinct() %>%
  mutate(
    canonical_name = ifelse(is.na(canonical_name), accepted_name, canonical_name),
    taxon_id = ifelse(is.na(taxon_id), accepted_name_usage_id, taxon_id),
    taxonomic_status = ifelse(accepted_name_usage_id == taxon_id, "accepted", "taxonomic synonym"),
    taxon_rank = ifelse(stringr::str_count(canonical_name, " ") == 1, "species", "subspecies"),
    genus = stringr::word(accepted_name, start = 1),
    family = stringr::str_to_sentence(family)
  )
  

afd_all %>% write_csv("config/taxon_lists/afd_tweaked.csv")  


## ----------------------------------------------- ##
## APCalign functions required ####
## ----------------------------------------------- ##

library(APCalign)

source("~/GitHub/ausinvertraits.addons/scripts/extras/fuzzy_match.R")

update_na_with <- function(current, new) {
  
  ifelse(is.na(current), new, current)
}

redistribute <- function(data) {
  data[["checked"]] <- dplyr::bind_rows(data[["checked"]],
                                        data[["tocheck"]] %>% 
                                          dplyr::filter(checked))
  
  data[["tocheck"]] <-
    data[["tocheck"]] %>% dplyr::filter(!checked)
  data
}

extract_genus <- function(taxon_name) {
  
  taxon_name <- standardise_names(taxon_name)
  
  word_one <- stringr::str_split_i(taxon_name, " |\\/", 1) %>% stringr::str_to_sentence()
  
  # Deal with names that being with x, 
  # e.g."x Taurodium x toveyanum" or "x Glossadenia tutelata"
  i <- !is.na(word_one) & word_one =="X"
  
  word_one[i] <- 
    stringr::str_split_i(taxon_name[i], " |\\/", 2) %>% stringr::str_to_sentence() %>%  paste("x", .)
  
  word_one
}

## ----------------------------------------------- ##
## Merge taxonomic datasets ####
## ----------------------------------------------- ##


# AFD

#afd_inverts <- read_csv("~/GitHub/ausinvertraits.build/config/afd_final.csv") %>%
  
afd_inverts <- afd_final %>%
  mutate(
    taxonomic_status = stringr::str_replace(taxonomic_status, "taxonomic synonym", "synonym")
  ) %>% 
  rename(genus = GENUS, family = FAMILY, order = ORDER, class = CLASS, phylum = PHYLUM)

# iNaturalist

iNat_inverts_in_Australia <- read_csv("~/GitHub/ausinvertraits.addons/data/iNat_inverts_in_Australia.csv")

iNat_inverts <- iNat_inverts_in_Australia %>% 
  select(
    accepted_name = scientificName,
    accepted_name_usage_id = taxonID,
    taxon_id = taxonID,
    family,
    taxon_rank = taxonRank,
    genus,
    class,
    phylum
  ) %>%
  mutate(
    canonical_name = accepted_name,
    taxonomic_status = "accepted",
    taxonomic_dataset = "iNaturalist"
  )

# Can add more lists here!


# Merge together lists
taxon_list <- afd_final %>%
  bind_rows(iNat_inverts %>% mutate(taxon_name = canonical_name)) %>%
  mutate(
    # indicate relative priority of taxonomic datasets
    taxonomic_dataset = factor(taxonomic_dataset, levels = c("AFD", "iNaturalist")),
    scientific_name = FULL_NAME,
  ) %>%
  dplyr::arrange(taxonomic_dataset, canonical_name) %>%
  dplyr::group_by(canonical_name) %>%
  dplyr::mutate(
    taxonomic_datasets_all = paste0(taxonomic_dataset, collapse = "; ")
  ) %>%
  dplyr::ungroup() %>%
  dplyr::distinct(canonical_name, .keep_all = TRUE)

# Fuzzy match can't handle NA, so create a dummy variable
zzz <- "zzzz zzzz"

# Add columns that are required for various algorithm matches before splitting
taxon_list <- taxon_list %>%
  dplyr::mutate(
    word_one = extract_genus(canonical_name),
    taxon_rank = standardise_taxon_rank(taxon_rank),
    taxon_rank2 = ifelse(taxon_rank %in% c("subspecies", "species", "form", "variety"), "species", taxon_rank),
    ## strip_names removes punctuation and filler words associated with
    ## infraspecific taxa (subsp, var, f, ser)
    stripped_canonical = strip_names(canonical_name),
    ## strip_names_extra removes extra filler words associated with 
    ## species name cases (x, sp)
    ## strip_names_extra is essential for the matches involving 2 or 3 words,
    ## since you want those words to not count filler words
    stripped_canonical2 = strip_names_extra(stripped_canonical),
    stripped_scientific = strip_names(scientific_name),
    ignore_bracketed_words = stringr::str_remove(canonical_name, " \\(.*\\)"),
    ignore_bracketed_words = ifelse(is.na(ignore_bracketed_words), zzz, ignore_bracketed_words),
    ignore_bracketed_words = base::replace(ignore_bracketed_words, duplicated(ignore_bracketed_words), zzz),
    binomial = ifelse(
      taxon_rank == "species",
      word(ignore_bracketed_words, start = 1, end = 2),
      zzz
    ),
    binomial = ifelse(is.na(binomial), zzz, binomial),
    binomial = base::replace(binomial, duplicated(binomial), zzz),
    words_one_two = word(stripped_canonical2, start = 1, end = 2),
    words_one_two = ifelse(is.na(words_one_two), zzz, words_one_two),
    words_one_two = base::replace(words_one_two, duplicated(words_one_two), zzz),
    word_one = extract_genus(stripped_canonical),
    trinomial = word(ignore_bracketed_words, start = 1, end = 3),
    trinomial = ifelse(is.na(trinomial), zzz, trinomial),
    trinomial = base::replace(trinomial, duplicated(trinomial), zzz)
  )

# Need to split the taxon_list by taxon rank
# Want to retain species and infraspecies as part of the same list, whih is why the variable `taxon_rank2` is being used
resources <- taxon_list %>% split(taxon_list$taxon_rank2)

# For species- and infra-species- level names, split into "accepted" (accepted/valid names) & "synonym" (taxonomic synonyms)
resources$species <- resources$species %>% split(resources$species$taxonomic_status)

# This reflects the ranks currently indicated in the AusInverTraits database (+ a few more)
# It can be expanded.

taxon_ranks_to_check <- c("subgenus", "genus", "family", "superfamily", 
                          "subclass", "class", "subfamily", 
                          "order", "infraorder", "suborder", "superorder", "tribe")

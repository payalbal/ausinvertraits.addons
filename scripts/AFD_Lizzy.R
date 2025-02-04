library(tidyverse)
afd_Feb2025 <- read_csv("~/GitHub/ausinvertraits.addons/data/afd_Feb2025.csv")

names(afd_Feb2025)

afd_Feb2025_edited <- afd_Feb2025 %>%
  mutate(
    across(3:19, stringr::str_to_sentence)
  ) %>%
  mutate(
    taxon_rank = ifelse(is.na(SUB_SPECIES), "species", "subspecies"),
    SUBSPECIES_COUNT_IN_SPECIES = as.character(SUBSPECIES_COUNT_IN_SPECIES),
    taxon_name = FULL_NAME
    )

subgenera <- afd_Feb2025_edited %>%
  distinct(SUB_GENUS, .keep_all = T) %>%
  filter(!is.na(SUB_GENUS)) %>%
  mutate(
    across(20:38, ~ NA_character_),
    taxon_rank = "subgenus",
    taxon_name = SUB_GENUS
  )

genera <- afd_Feb2025_edited %>%
  distinct(GENUS, .keep_all = T) %>%
  filter(!is.na(GENUS)) %>%
  mutate(
    across(19:38, ~ NA_character_),
    taxon_rank = "genus",
    taxon_name = GENUS
  )

subtribe <- afd_Feb2025_edited %>%
  distinct(SUBTRIBE, .keep_all = T) %>%
  filter(!is.na(SUBTRIBE)) %>%
  mutate(
    across(18:38, ~ NA_character_),
    taxon_rank = "subtribe",
    taxon_name = SUBTRIBE
  )

tribe <- afd_Feb2025_edited %>%
  distinct(TRIBE, .keep_all = T) %>%
  filter(!is.na(TRIBE)) %>%
  mutate(
    across(17:38, ~ NA_character_),
    taxon_rank = "tribe",
    taxon_name = TRIBE
  )

supertribe <- afd_Feb2025_edited %>%
  distinct(SUPERTRIBE, .keep_all = T) %>%
  filter(!is.na(SUPERTRIBE)) %>%
  mutate(
    across(16:38, ~ NA_character_),
    taxon_rank = "supertribe",
    taxon_name = SUPERTRIBE
  )

subfamily <- afd_Feb2025_edited %>%
  distinct(SUBFAMILY, .keep_all = T) %>%
  filter(!is.na(SUBFAMILY)) %>%
  mutate(
    across(15:38, ~ NA_character_),
    taxon_rank = "subfamily",
    taxon_name = SUBFAMILY
  )

family <- afd_Feb2025_edited %>%
  distinct(FAMILY, .keep_all = T) %>%
  filter(!is.na(FAMILY)) %>%
  mutate(
    across(14:38, ~ NA_character_),
    taxon_rank = "family",
    taxon_name = FAMILY
  )

superfamily <- afd_Feb2025_edited %>%
  distinct(SUPERFAMILY, .keep_all = T) %>%
  filter(!is.na(SUPERFAMILY)) %>%
  mutate(
    across(13:38, ~ NA_character_),
    taxon_rank = "superfamily",
    taxon_name = SUPERFAMILY
  )

suborder <- afd_Feb2025_edited %>%
  distinct(SUBORDER, .keep_all = T) %>%
  filter(!is.na(SUBORDER)) %>%
  mutate(
    across(12:38, ~ NA_character_),
    taxon_rank = "suborder",
    taxon_name = SUBORDER
  )

order <- afd_Feb2025_edited %>%
  distinct(ORDER, .keep_all = T) %>%
  filter(!is.na(ORDER)) %>%
  mutate(
    across(11:38, ~ NA_character_),
    taxon_rank = "order",
    taxon_name = ORDER
  )

superorder <- afd_Feb2025_edited %>%
  distinct(SUPERORDER, .keep_all = T) %>%
  filter(!is.na(SUPERORDER)) %>%
  mutate(
    across(10:38, ~ NA_character_),
    taxon_rank = "superorder",
    taxon_name = SUPERORDER
  )

subclass <- afd_Feb2025_edited %>%
  distinct(SUBCLASS, .keep_all = T) %>%
  filter(!is.na(SUBCLASS)) %>%
  mutate(
    across(9:38, ~ NA_character_),
    taxon_rank = "subclass",
    taxon_name = SUBCLASS
  )

class <- afd_Feb2025_edited %>%
  distinct(CLASS, .keep_all = T) %>%
  filter(!is.na(CLASS)) %>%
  mutate(
    across(8:38, ~ NA_character_),
    taxon_rank = "class",
    taxon_name = CLASS
  )

superclass <- afd_Feb2025_edited %>%
  distinct(SUPERCLASS, .keep_all = T) %>%
  filter(!is.na(SUPERCLASS)) %>%
  mutate(
    across(7:38, ~ NA_character_),
    taxon_rank = "superclass",
    taxon_name = SUPERCLASS
  )

subphylum <- afd_Feb2025_edited %>%
  distinct(SUBPHYLUM, .keep_all = T) %>%
  filter(!is.na(SUBPHYLUM)) %>%
  mutate(
    across(6:38, ~ NA_character_),
    taxon_rank = "subphylum",
    taxon_name = SUBPHYLUM
  )

phylum <- afd_Feb2025_edited %>%
  distinct(PHYLUM, .keep_all = T) %>%
  filter(!is.na(PHYLUM)) %>%
  mutate(
    across(5:38, ~ NA_character_),
    taxon_rank = "phylum",
    taxon_name = PHYLUM
  )

# Merge together the dataframes for all the different taxon ranks
afd_Feb2025_all_ranks <- afd_Feb2025_edited %>%
  bind_rows(subgenera, genera, subtribe, tribe, supertribe, subfamily, family, superfamily, suborder, order, superorder, subclass, class, superclass, subphylum, phylum) %>%
  mutate(
    taxonomic_status = "accepted",
    taxonomic_dataset = "AFD",
    canonical_name = taxon_name,
    accepted_name = taxon_name,
    accepted_name_usage_id = ifelse(!is.na(CONCEPT_GUID), CONCEPT_GUID, taxon_name),
    taxon_id = ifelse(!is.na(CONCEPT_GUID), CONCEPT_GUID, taxon_name),
  ) %>%
  select(taxon_name, canonical_name, accepted_name, taxon_rank, taxonomic_status, taxonomic_dataset, accepted_name_usage_id, taxon_id, everything())

# Function to remove author names from synonyms - any uppercase word that is not at the beginning is removed
remove_uppercase <- function(x) {
  x <- str_replace(x, " d'", " ")
  
  # Split the string into words
  words <- strsplit(x, " ")[[1]]
  
  # Keep the first word and remove subsequent ones that start with uppercase letters
  result <- c(words[1], words[-1][!grepl("^[A-Z]", words[-1])])
  
  # Recombine the string
  result <- paste(result, collapse = " ")
  
  # Remove stray information
  result <- str_replace(result, " &", "")
  result <- str_replace(result, " in ", " ")
  result <- str_replace(result, " de ", " ")
  result <- str_trim(result)
  
  return(result)
}

# Test the above function on diverse names
names_to_test <- c("Abracadbrella lewiston Zabka", "Onychoteuthis armata Quoy & Gaimard",  "Chooreechillum distitans T.P. Lucas", "Cladorhiza pentaeiros Ekins, Erpenbeck & Hooper", 
                   "Lamia fasciata Montrouzier", "Dihammus fasciatus aurivarius Gressitt", "Acanthochites (Meturoplax) retrojectus Pilsbry")

modified_names <- sapply(names_to_test, remove_uppercase)


# Assemble table of synonyms
afd_Feb2025_synonyms <- afd_Feb2025_edited %>%
  tidyr::separate_longer_delim(SYNONYMS, delim = "; ") %>% 
  mutate(
    synonym_compare = paste0(FULL_NAME, " ", AUTHOR, ", ", YEAR) 
    ) %>%
  filter(SYNONYMS != synonym_compare) %>% 
  mutate(
    AUTHOR = stringr::str_replace(AUTHOR, "\\(",""),
    AUTHOR = stringr::str_replace(AUTHOR, "\\)",""),
    taxon_name_tmp = stringr::str_replace(SYNONYMS, "\\,\\s[:digit:][:digit:][:digit:][:digit:]", ""),
    taxon_name_no_author = NA,
    taxon_name_no_author2 = NA,
    taxonomic_status = "synonym",
    taxonomic_dataset = "AFD",
    accepted_name = taxon_name
  )

# Remove authors that are detected in the author list
authors <- afd_Feb2025 %>% 
  distinct(AUTHOR) %>%
  mutate(AUTHOR = paste0(AUTHOR)) %>% 
  pull(AUTHOR) %>%
  gsub("([[:punct:]])", "\\\\\\1", .)

regex_authors <- paste(authors, collapse = "$|")

i <-
  str_detect(afd_Feb2025_synonyms$taxon_name_tmp, regex_authors)

afd_Feb2025_synonyms$taxon_name_no_author2 <- str_replace(afd_Feb2025_synonyms$taxon_name_tmp, regex_authors, "")


# Further remove any upper case words that aren't at the beginning of the name
for (j in 1:nrow(afd_Feb2025_synonyms)) {
  afd_Feb2025_synonyms$taxon_name_no_author[[j]] <- remove_uppercase(afd_Feb2025_synonyms$taxon_name_no_author2[[j]])
}

# Reorder columns for checking
afd_Feb2025_synonyms <- afd_Feb2025_synonyms %>%
  mutate(
    canonical_name = str_trim(taxon_name_no_author),
    accepted_name_usage_id = ifelse(!is.na(CONCEPT_GUID), CONCEPT_GUID, canonical_name),
    taxon_id = ifelse(!is.na(CONCEPT_GUID), paste0(CONCEPT_GUID,"_",canonical_name), paste0(accepted_name,"_",canonical_name)),
  ) %>%
  select(-taxon_name_no_author2, -taxon_name_no_author, -taxon_name_tmp, -synonym_compare)

afd_final <- afd_Feb2025_all_ranks %>%
  bind_rows(afd_Feb2025_synonyms)

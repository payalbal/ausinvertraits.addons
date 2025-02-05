## AFD cleaning
## Authors: Payal Bal, Elizabeth Wenk




## ----------------------------------------------- ## 
## Set working environment ####
## ----------------------------------------------- ##
# .rs.restartR()
rm(list = ls())
gc()
# system("ps")
# system("pkill -f R")

## Packages
x <- c('dplyr', 'janitor', 'stringr', 'tidyverse', 'stringr', 
       'data.table', 'usethis', 'devtools')
lapply(x, require, character.only = TRUE)
rm(x)

## Functions 
source(file.path(getwd(),"scripts/remove_improper_names_v2.R"))

## Paths
inverts_dir <- "/Volumes/6300-PAYALB/uom_data/ausinvertraits.addons_data"
# inverts_dir <- "/tempdata/research-cifs/6300-payalb/uom_data/ausinvertraits.addons_data"
outdir <- file.path(inverts_dir, "outputs")





## ----------------------------------------------- ##
## Load Australian Faunal Directory taxonomy ####
## ----------------------------------------------- ##
afd_data <- fread(file.path(outdir, "afd_Jan2023.csv"))

names(afd_data)
sum(is.na(afd_data$FULL_NAME))
length(unique(afd_data$FULL_NAME))
nrow(afd_data)
nrow(afd_data) - length(unique(afd_data$FULL_NAME)) == sum(duplicated(afd_data$FULL_NAME))





## ----------------------------------------------- ##
## Remove improper names ####
## ----------------------------------------------- ##
species_record <- remove_improper_names(name_vector = afd_data$FULL_NAME,
                                        allow.subgenus = TRUE,
                                        allow.higher.taxa = TRUE) 

## Check if there are any NAs in updated list
is.na(species_record$updated_list) |> table()


## Check number of words in species names
sp_words <- sapply(strsplit(as.character(species_record$updated_list), " "), length)
table(sp_words)
species_record$updated_list[sp_words == 4]


## Filter records based on updated list
afd_data <- afd_data[which(afd_data$FULL_NAME %in% species_record$updated_list),]


## Remove double and single quotes from names
grep("\"",afd_data$FULL_NAME, value = TRUE)
writeLines(grep("\"",afd_data$FULL_NAME, value = TRUE)[1])
x <- grep("\"",afd_data$FULL_NAME, value = TRUE)
y <- gsub("\"","",x)
afd_data[FULL_NAME %in% x]$FULL_NAME <- y
grep("\"",afd_data$FULL_NAME, value = TRUE)

grep("\'",afd_data$FULL_NAME, value = TRUE)
x <- grep("\'",afd_data$FULL_NAME, value = TRUE)
y <- gsub("\'","",x)
afd_data[FULL_NAME %in% x]$FULL_NAME <- y
grep("\'",afd_data$FULL_NAME, value = TRUE)

setDT(afd_data, key = 'FULL_NAME')
length(unique(afd_data$FULL_NAME)); nrow(afd_data)





## ----------------------------------------------- ##
## Identify duplicates ####
## ----------------------------------------------- ##
## List duplicates comparing all columns
nrow(afd_data[duplicated(afd_data),] )


## Create FULL_NAME + AUTHOR = COMPLETE_NAME column
afd_data %>% add_column(COMPLETE_NAME = paste0(trimws(afd_data$FULL_NAME), 
                                               ", ", trimws(afd_data$AUTHOR), 
                                               " ", trimws(afd_data$YEAR)), 
                        .after = "YEAR") -> afd_data

length(unique(afd_data$FULL_NAME))
length(unique(afd_data$COMPLETE_NAME))

## Set keys
afd_data <- setDT(afd_data, key = c("FULL_NAME", "COMPLETE_NAME", "CONCEPT_GUID"))


## Duplicates in FULL_NAME (excluding first appearance)
afd_data$FULL_NAME[duplicated(afd_data$FULL_NAME)]

## Duplicates in FULL_NAME (*including* first appearance)
afd_data$FULL_NAME[duplicated(afd_data$FULL_NAME) | duplicated(afd_data$FULL_NAME, fromLast=TRUE)]

temp <- afd_data[duplicated(afd_data$FULL_NAME) | duplicated(afd_data$FULL_NAME, fromLast=TRUE)]
nrow(temp)
fwrite(temp, file.path(outdir, "afd_fullname_repeats.csv"))


## Duplicates in COMPLETE_NAME (excluding first appearance)
afd_data$COMPLETE_NAME[duplicated(afd_data$COMPLETE_NAME)]

## Duplicates in COMPLETE_NAME (*including* first appearance)
afd_data$COMPLETE_NAME[duplicated(afd_data$COMPLETE_NAME) | duplicated(afd_data$COMPLETE_NAME, fromLast=TRUE)]

temp <- afd_data[duplicated(afd_data$COMPLETE_NAME) | duplicated(afd_data$COMPLETE_NAME, fromLast=TRUE)]
nrow(temp)
fwrite(temp, file.path(outdir, "afd_completename_repeats.csv"))


## Are there duplicates in FULL_NAME & COMPLETE_NAME?
length(afd_data$FULL_NAME) != length(unique(afd_data$FULL_NAME))
nrow(afd_data[duplicated(FULL_NAME),] )

length(afd_data$COMPLETE_NAME) != length(unique(afd_data$COMPLETE_NAME))
nrow(afd_data[duplicated(COMPLETE_NAME),] )


afd_data[,COMPLETE_NAME := NULL] ## drop column





# ## ----------------------------------------------- ##
# ## Resolve duplicates & update table ####
# ## ----------------------------------------------- ##
# 
# ## Load commented sheet
# reps <- fread(file.path(outdir, "afd_fullname_repeats_JRM.csv"))
# reps[, .N, Exclude]
# reps <- reps[is.na(Exclude)]; nrow(reps)
# reps[ ,':='(Exclude = NULL, Include = NULL, JM_notes=NULL)]; dim(reps)
# reps[,COMPLETE_NAME := NULL]
# 
# 
# ## Clean up table reps table if needed
# x <- grep("\"",reps$AUTHOR, value = TRUE)
# y <- gsub("\"","",x)
# reps[AUTHOR %in% x]$AUTHOR <- y
# grep("\"",reps$AUTHOR, value = TRUE)
# 
# 
# ## New table with combined info for duplicated
# newdt <- data.table()
# ctr <-  0
# 
# for(i in unique(reps$FULL_NAME)){
#   ctr <- ctr + 1
#   temp = afd_data[FULL_NAME == i]
#   temp2 <- temp[1,]
#   
#   temp2$AUTHOR = paste(unique(c(temp2$AUTHOR, unique(temp$AUTHOR))), collapse = ", ")
#   temp2$YEAR = paste(unique(c(temp2$YEAR, unique(temp$YEAR))), collapse = ", ")
#   temp2$SYNONYMS = paste(unique(c(temp2$SYNONYMS, unique(temp$SYNONYMS))), collapse = "; ")
#   
#   temp2[FULL_NAME == i, 'InvDir_ID'] = paste0("InvDir-ID-", ctr)
#   newdt <- rbind(newdt, temp2)
# }
# 
# 
# ## Add new ID column
# afd_data[, InvDir_ID := as.character()] ## add column for in-house IDs
# 
# 
# ## Bind new rows to data
# afd_data <- rbind(afd_data, newdt)





## ----------------------------------------------- ##
## Data checks (not exhaustive) ####
## ----------------------------------------------- ##
species_record <- remove_improper_names(afd_data$FULL_NAME,
                                        allow.subgenus = TRUE,
                                        allow.higher.taxa = TRUE) 

## Counts for number of words
sp_words <- sapply(strsplit(as.character(afd_data$FULL_NAME), " "), length)
table(sp_words)
# ## stringr version
# str_count(afd_data$FULL_NAME, pattern = "\\S+") |> janitor::tabyl() 


## Counts for special characters in species names
grep("\"",afd_data$FULL_NAME, value = TRUE) ## occurrences for "
grep("\'",afd_data$FULL_NAME, value = TRUE) ## occurrences for '
grep("\\(",afd_data$FULL_NAME, value = TRUE) ## occurrences for ()
grep("\\[",afd_data$FULL_NAME, value = TRUE) ## occurrences for ()

# ## stringr version
# stringr::str_count(afd_data$FULL_NAME, pattern = regex("\\\"")) |> sum() # Counting occurrences for \"
# stringr::str_count(afd_data$FULL_NAME, pattern = regex("'")) |> sum() # Counting occurrences for '
# stringr::str_count(afd_data$FULL_NAME, pattern = regex("\\(")) |> sum() 
# stringr::str_count(afd_data$FULL_NAME, pattern = regex("\\[")) |> sum() 




## ----------------------------------------------- ##
## Save cleaned data ####
## ----------------------------------------------- ##
fwrite(afd_data, file.path(outdir, paste0("afd_Feb2025.csv")))





## ----------------------------------------------- ##
## Create rows for higher order taxa ####
## ----------------------------------------------- ##

afd_Feb2025 <- read_csv("~/GitHub/ausinvertraits.addons/data/afd_Feb2025.csv")

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

subgenera2 <- afd_Feb2025_edited %>%
  distinct(GENUS, SUB_GENUS, .keep_all = T) %>%
  filter(!is.na(SUB_GENUS)) %>%
  mutate(
    across(20:38, ~ NA_character_),
    taxon_rank = "subgenus",
    taxon_name = paste0(GENUS, " (", SUB_GENUS, ")")
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
  bind_rows(subgenera, subgenera2, genera, subtribe, tribe, supertribe, subfamily, family, superfamily, suborder, order, superorder, subclass, class, superclass, subphylum, phylum) %>%
  mutate(
    taxonomic_status = "accepted",
    taxonomic_dataset = "AFD",
    canonical_name = taxon_name,
    accepted_name = taxon_name,
    accepted_name_usage_id = ifelse(!is.na(CONCEPT_GUID), CONCEPT_GUID, taxon_name),
    taxon_id = ifelse(!is.na(CONCEPT_GUID), CONCEPT_GUID, taxon_name),
    taxon_rank = factor(taxon_rank, levels = c("subspecies", "species", "subgenus", "genus", "subtribe", "tribe", "supertribe", 
                                               "subfamily", "family", "superfamily", "suborder", "order", "superorder", 
                                               "subclass", "class", "superclass", "subphylum", "phylum")),
  ) %>%
  arrange(taxon_rank) %>%
  select(taxon_name, canonical_name, accepted_name, taxon_rank, taxonomic_status, taxonomic_dataset, accepted_name_usage_id, taxon_id, everything())

## ----------------------------------------------- ##
## Create rows for synonyms ####
## ----------------------------------------------- ##

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

# Add final columns
afd_Feb2025_synonyms <- afd_Feb2025_synonyms %>%
  mutate(
    canonical_name = str_trim(taxon_name_no_author),
    accepted_name_usage_id = ifelse(!is.na(CONCEPT_GUID), CONCEPT_GUID, canonical_name),
    taxon_id = ifelse(!is.na(CONCEPT_GUID), paste0(CONCEPT_GUID,"_",canonical_name), paste0(accepted_name,"_",canonical_name)),
  ) %>%
  select(-taxon_name_no_author2, -taxon_name_no_author, -taxon_name_tmp, -synonym_compare)

afd_final <- afd_Feb2025_all_ranks %>%
  bind_rows(afd_Feb2025_synonyms)


## ----------------------------------------------- ##
## Save data ####
## ----------------------------------------------- ##

fwrite(afd_data, file.path(outdir, paste0("afd_taxIDs_Feb2025.csv")))






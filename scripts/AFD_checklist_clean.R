
# AFD checklist cleaning
# authors: Payal Bal, Fonti Kar, Hannah Smart


 

## ----------------------------------------------- ## 
## Set working environment ####
## ----------------------------------------------- ##
# .rs.restartR()
rm(list = ls())
gc()
# system("ps")
# system("pkill -f R")

## Packages
x <- c('dplyr', 'janitor', 'galah', 'stringr', 'tidyverse', 'arrow', 'stringr', 
               'data.table', 'CoordinateCleaner', 'usethis', 'devtools')
lapply(x, require, character.only = TRUE)
rm(x)

## Functions 
source(file.path(getwd(),"scripts/remove_improper_names_v2.R"))

## Paths
# inverts_dir <- "/Volumes/6300-PAYALB/uom_data/ausinvertraits.addons_data"
inverts_dir <- "/tempdata/research-cifs/6300-payalb/uom_data/ausinvertraits.addons_data"
outdir <- file.path(inverts_dir, "outputs")




## ----------------------------------------------- ##
## Load Australian Faunal Directory taxonomy ####
## ----------------------------------------------- ##
afd_data <- read_csv(file.path(outdir, "afd_Jan2023.csv"))
afd_data <- as.data.table(afd_data)
afd_data_raw <- copy(afd_data) ## make a copy 

names(afd_data)
sum(is.na(afd_data$FULL_NAME))
length(unique(afd_data$FULL_NAME))
nrow(afd_data)

sp_words <- sapply(strsplit(as.character(afd_data$FULL_NAME), " "), length)
table(sp_words)




## ----------------------------------------------- ##
## Remove improper names ####
## ----------------------------------------------- ##
species_record <- remove_improper_names_v2(afd_data$FULL_NAME,
                                           allow.higher.taxa = FALSE,
                                           improper.species.list = TRUE)

# Double checking if there are any NAs in updated list
is.na(species_record$updated_list) |> table()
  
# Filter records based on updated list
afd_data <- afd_data[which(afd_data$FULL_NAME %in% species_record$updated_list),]


## Remove double and single quotes from names
grep("\"",afd_data$FULL_NAME, value = TRUE)
writeLines(grep("\"",afd_data$FULL_NAME, value = TRUE)[1])

x <- grep("\"",afd_data$FULL_NAME, value = TRUE)
y <- gsub("\"","",x)
afd_data[FULL_NAME %in% x]$FULL_NAME <- y
grep("\"",afd_data$FULL_NAME, value = TRUE)

x <- grep("\'",afd_data$FULL_NAME, value = TRUE)
y <- gsub("\'","",x)
afd_data[FULL_NAME %in% x]$FULL_NAME <- y
grep("\'",afd_data$FULL_NAME, value = TRUE)

setDT(afd_data, key = 'FULL_NAME')
length(unique(afd_data$FULL_NAME)); nrow(afd_data)




## ----------------------------------------------- ##
## Remove invasive species using GRIIS list ####
## ----------------------------------------------- ##

## >> Process new  GRIIS list ####
## Source: provided by Fonti Kar, ALA

## Load new GRIIS list
distribution <- fread(file.path(inverts_dir,
                                "dwca-griis-australia-v1.6/distribution.txt"))
species <- fread(file.path(inverts_dir,
                           "dwca-griis-australia-v1.6/speciesprofile.txt"))
taxa <- fread(file.path(inverts_dir,
                        "dwca-griis-australia-v1.6/taxon-edited.txt"), fill = TRUE)

griis_list <- merge(taxa, distribution, by = "id")
griis_list <- merge(griis_list, species, by = "id")
names(griis_list)
rm(distribution, species, taxa)


## Remove NAs in scientificName
nrow(griis_list)
is.na(griis_list$scientificName) |> table() 

if (nrow(griis_list[is.na(griis_list$scientificName)]) > 0) {
  griis_list <- griis_list[!is.na(scientificName)]  
}


## Clean up scientificName in GRIIS and filter by noIssues & no missing values in species
griis_names <- as.data.table(galah::search_taxa(griis_list$scientificName))


## Create list of GRIIS scientificName found (and cleaned) using galah::search_taxa()
griis_names_tidy <- griis_names[!is.na(species) & issues == "noIssue"][,.(search_term, taxon_concept_id, scientific_name, scientific_name_authorship)]

fwrite(as.data.table(griis_names_tidy), 
       file = file.path(outdir, "griis_invasive_speciesnames_resolved.csv"), 
       row.names = FALSE)

message(cat("Number of AFD species in processed GRIIS list, found & corrected using galah::search_taxa(): "),
        length(which(afd_data$FULL_NAME %in% griis_names_tidy$scientific_name)))


## Create list of GRIIS scientificName not found using galah::search_taxa() 
griis_names_untidy <- griis_names[is.na(species)]$search_term
fwrite(as.data.table(griis_names_untidy), 
       file = file.path(outdir, "griis_invasive_speciesnames_unresolved.csv"), 
       row.names = FALSE)

message(cat("Number species in GRIIS list, not found using galah::search_taxa(): "),
        length(griis_names_untidy))


## >> Filter AFD using new GRIIS list ####
## Using resolved names in GRIIS list
length(unique(afd_data$FULL_NAME)); nrow(afd_data)
afd_data <- afd_data[! afd_data$FULL_NAME %in% griis_names_tidy$scientific_name, ]
length(unique(afd_data$FULL_NAME)); nrow(afd_data)

## Using unresolved names in GRIIS list as check by JM
## Most names irrelvant (plants, other taxa), only a few to be used for filtering
## No names found in AFD checklist
temp <- fread(file.path(outdir, "griis_invasive_speciesnames_unresolved_JRM.csv"))
message(cat("Number of AFD species listed in griis_invasive_speciesnames_unresolved: "),
        length(which(afd_data$FULL_NAME %in% c(temp$griis_names_untidy, 
                                               temp$corrected_name[which(temp$corrected_name != "")]))))

x <- afd_data$FULL_NAME[afd_data$FULL_NAME %in% c(temp$griis_names_untidy, 
                                             temp$corrected_name[which(temp$corrected_name != "")])]
length(unique(afd_data$FULL_NAME)); nrow(afd_data)
afd_data <- afd_data[! afd_data$FULL_NAME %in% x, ]
length(unique(afd_data$FULL_NAME)); nrow(afd_data)


## >> Filter AFD using old GRIIS list ####
## Source: Downloaded from ALA for NESP project
griis_old <- read.csv(file.path(inverts_dir, 
                                "GRIIS_Global_Register_of_Introduced_and_Invasive_Species_Australia.csv"))

message(cat("Number of AFD species listed in old GRIIS list: "),
        length(which(afd_data$FULL_NAME %in% c(griis_old$Supplied.Name, griis_old$scientificName))))

afd_data <- afd_data[! afd_data$FULL_NAME %in% c(griis_old$Supplied.Name, griis_old$scientificName),]
length(unique(afd_data$FULL_NAME)); nrow(afd_data)





## ----------------------------------------------- ##
## Remove marine species ####
## ----------------------------------------------- ##

## Author: Fonti Kar
## Source of WoRMS: https://github.com/AtlasOfLivingAustralia/data_cleaning_workflows/blob/develop/workflow/worrms.Rmd
worms <- read_csv(file.path(inverts_dir, "worms/worms_afd_query_results.csv"))
names(worms)


## >> List of marine species from WORMS ####
## Find missing values in scientificname, valid_name
is.na(worms$scientificname) |> table()
is.na(worms$valid_name) |> table()


## Do valid name match scientific name? NO
(worms$valid_name == worms$scientificname) |> janitor::tabyl()


## Summary of records using identifiers of habitat occupancy
worms %>% select(starts_with("is")) %>% colSums(., na.rm = TRUE)


## List WoRMs species exclusively as marine using identifiers of habitat occupancy of each taxa
## i.e. TRUE for isMarine and NA/FALSE for the others
worms <- as.data.table(worms)
marine_worms <- worms[isMarine == 1 & isBrackish == 0 & isTerrestrial == 0 & isFreshwater == 0 & isExtinct == 0]


## Unmatched valid name and scientific name in WORMS
marine_worms[(! valid_name == scientificname)][,.(valid_name, scientificname)]


## >> Exclude marine species using valid_name in WORMS ####
afd_marine_1 <- afd_data[FULL_NAME %in% marine_worms$valid_name][,.(FULL_NAME)]
nrow(afd_data) - nrow(afd_marine_1)

afd_data <- afd_data[! FULL_NAME %in% marine_worms$valid_name]



## >> Exclude marine species using scientificname in WORMS ####
afd_marine_2 <- afd_data[FULL_NAME %in% marine_worms$scientificname][,.(FULL_NAME)]
nrow(afd_data) - nrow(afd_marine_2)

afd_data <- afd_data[! FULL_NAME %in% marine_worms$scientificname]
length(unique(afd_data$FULL_NAME)); nrow(afd_data)



## >> Identifying marine as per 'ECOLOGY_DESCRIPTIORS' in AFD ####
nrow(afd_data[grep("marine", afd_data$ECOLOGY_DESCRIPTIORS)])

temp <- afd_data[grep("marine", afd_data$ECOLOGY_DESCRIPTIORS)][,.(FULL_NAME, SYNONYMS, PHYLUM, SUBPHYLUM, SUPERCLASS, CLASS, SUBCLASS, SUPERORDER, ORDER, SUBORDER, SUPERFAMILY, FAMILY, SUBFAMILY, SUPERTRIBE, TRIBE, SUBTRIBE, GENUS, SPECIES, SUB_SPECIES, CONCEPT_GUID, ECOLOGY_DESCRIPTIORS)]

fwrite(as.data.table(temp), file = file.path(outdir, "afd_marine.csv"), 
       row.names = FALSE)



## Save data
afd_data <- setDT(afd_data, key = "FULL_NAME")
fwrite(afd_data, 
       file = file.path(outdir, "afd_Mar2023_clean.csv"), 
       row.names = FALSE)






## ----------------------------------------------- ##
## Synonyms ####
## ----------------------------------------------- ##
source("./scripts/get_AFDsynonyms.R")

afd_data <- fread(file.path(outdir, "afd_Mar2023_clean.csv"))
afd_data <- setDT(afd_data, key = "FULL_NAME")

out <- get_AFDsynonyms(unique(afd_data$FULL_NAME), afd_data)
saveRDS(out, file = "./outputs/afd_synonyms.rds")






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


## Are there duplicates in FULL_NAME & COMPLETE_NAME?
length(afd_data$FULL_NAME) != length(unique(afd_data$FULL_NAME))
length(afd_data$COMPLETE_NAME) != length(unique(afd_data$COMPLETE_NAME))


## Set as DT
afd_dt <- setDT(afd_data, key = c("FULL_NAME", "COMPLETE_NAME", "CONCEPT_GUID"))

## List of duplicates in FULL_NAME (excluding first appearance)
afd_dt$FULL_NAME[duplicated(afd_dt$FULL_NAME)]


## Duplicates in FULL_NAME (*including* first appearance)
dup_fullname <- afd_dt$FULL_NAME[duplicated(afd_dt$FULL_NAME) | duplicated(afd_dt$FULL_NAME, fromLast=TRUE)]
length (dup_fullname)
dup_fullname

## List of duplicates in COMPLETE_NAME (excluding first appearance)
afd_dt$COMPLETE_NAME[duplicated(afd_dt$COMPLETE_NAME)]


## Duplicates in COMPLETE_NAME (*including* first appearance)
dup_completename <- afd_dt$COMPLETE_NAME[duplicated(afd_dt$COMPLETE_NAME) | duplicated(afd_dt$COMPLETE_NAME, fromLast=TRUE)]
length(dup_completename)
dup_completename


## Write outputs
temp1 <- afd_dt[COMPLETE_NAME %in% dup_completename]
readr::write_csv(temp1, file.path(outdir, "afd_completename_repeats.csv"))

temp2 <- afd_dt[FULL_NAME %in% dup_fullname]
readr::write_csv(temp2, file.path(outdir, "afd_fullname_repeats.csv"))



## Resolve duplicates manually ####









## Counts for number of words
str_count(afd_data$FULL_NAME, pattern = "\\S+") |> janitor::tabyl()

## Counts for special characters in species names
stringr::str_count(afd_data$FULL_NAME, pattern = regex("\\\"")) |> sum() # Counting occurrences for \"
stringr::str_count(afd_data$FULL_NAME, pattern = regex("'")) |> sum() # Counting occurrences for '
stringr::str_count(afd_data$FULL_NAME, pattern = regex("\\(")) |> sum() 
stringr::str_count(afd_data$FULL_NAME, pattern = regex("\\[")) |> sum() 



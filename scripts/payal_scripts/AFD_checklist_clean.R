# ---
# AFD checklist cleaning
# authors: Payal Bal, Fonti Kar, Hannah Smart
# notes: 
#   VALID_NAME replaced by FULL_NAME in updated AFD data

# ---
 
 
## Set working environment ####
rm(list = ls())
gc()
# system("ps")
# system("pkill -f R")

## Packages
pacman::p_load(dplyr, janitor, galah, stringr, tidyverse, arrow, stringr, data.table, CoordinateCleaner)

## Functions 
source(file.path(getwd(),"scripts/remove_improper_names_v2.R"))

## Paths
inverts_dir <- "/Volumes/6300-PAYALB/uom_data/inverts_data"
outdir <- file.path(inverts_dir, "outputs")



## Load Australian Faunal Directory taxonomy ####
afd_data <- read_csv(file.path(outdir, "afd_Jan2023.csv"))
# names(afd_data)[names(afd_data) %in% "FULL_NAME"] = "VALID_NAME" # to align with existing code

names(afd_data)
sum(is.na(afd_data$FULL_NAME))

afd_species <- afd_data$FULL_NAME
length(afd_species)
length(unique(afd_species))


## Remove improper names ####
species_record <- remove_improper_names_v2(afd_species,
                                           allow.higher.taxa = FALSE,
                                           improper.species.list = TRUE)

# Double checking if there are any NAs in updated list
is.na(species_record$updated_list) |> table()
  
# Filter records based on updated list
afd_data |> 
  dplyr::filter(FULL_NAME %in% species_record$updated_list) -> afd_data



## Remove invasive species using GRIIS list ####

## Load new GRIIS
## Source of new GRIIS list (provided by Fonti Kar, ALA)
distribution <- fread(file.path(inverts_dir,
                                "dwca-griis-australia-v1.6/distribution.txt"))
species <- fread(file.path(inverts_dir,
                           "dwca-griis-australia-v1.6/speciesprofile.txt"))
taxa <- fread(file.path(inverts_dir,
                        "dwca-griis-australia-v1.6/taxon-edited.txt"), fill = TRUE)

griis_list <- taxa |>  
  full_join(distribution, by = "id") |>  
  full_join(species, by = "id")

## Check for and remove NAs in griis_list$scientificName
is.na(griis_list$scientificName) |> table() 
griis_list[is.na(griis_list$scientificName)]
griis_list <- na.omit(griis_list)


## Use galah::search_taxa() to get tidy names and filter by noIssues
griis_ala_raw <- data.frame(search_taxa(griis_list$scientificName))


## Pre-process the GRIIS list and filter by noIssues and no missing values in species
griis_ala_tidy <- griis_ala_raw |>  
  filter(issues == "noIssue",
         !is.na(species)) |>  
  dplyr::select(search_term, taxon_concept_id, scientific_name, scientific_name_authorship)


## Reporting
message(cat("Number of AFD species listed in GRIIS: "),
        length(which(afd_data$FULL_NAME %in% griis_ala_tidy$scientific_name)))

message("AFD species listed in GRIIS: ")
afd_data$FULL_NAME[which(afd_data$FULL_NAME %in% griis_ala_tidy$scientific_name)]


## Excluding GRIIS listed species from AFD list
updated_griis_afd <- afd_data$FULL_NAME[which(afd_data$FULL_NAME %in% griis_ala_tidy$scientific_name)]
afd_data <- afd_data[! afd_data$FULL_NAME %in% griis_ala_tidy$scientific_name, ]


## Filter using old GRIIS list
## Source: Downloaded from ALA for NESP project
griis_old <- read.csv(file.path(inverts_dir, 
                                "GRIIS_Global_Register_of_Introduced_and_Invasive_Species_Australia.csv"))

message(cat("Number of AFD species listed in GRIIS: "),
        length(which(afd_data$FULL_NAME %in% c(griis_old$Supplied.Name, griis_old$scientificName))))

old_griis_afd <- afd_data$FULL_NAME[which(afd_data$FULL_NAME %in% 
                                            c(griis_old$Supplied.Name, griis_old$scientificName))]
afd_data <- afd_data[! afd_data$FULL_NAME %in% griis_old$Supplied.Name,]



## Remove marine species ####
## $$ FIX: Add Fonti'sworrms scripts to repo ####
worms <- read_csv(file.path(inverts_dir, "worms/worms_afd_query_results.csv"))
names(worms)

## Find missing values in scientificname, valid_name
is.na(worms$scientificname) |> table()
is.na(worms$valid_name) |> table()

## Do valid name match scientific name?
(worms$valid_name == worms$scientificname) |> janitor::tabyl()

## List WoRMs species exclusively as marine using identifiers of habitat occupancy of each taxa
## i.e. TRUE for isMarine and NA/FALSE for the others
worms %>% select(starts_with("is")) %>% colSums(., na.rm = TRUE)

## Number of strictly marine species in WoRMs
worms %>%
  filter(isMarine == 1,
         isBrackish == 0, isTerrestrial == 0, isFreshwater == 0, isExtinct == 0) -> marine_worms 


## Check: valid_name and scientificname is different in WoRMs
marine_worms |> 
  filter(! valid_name == scientificname) |> 
  select(valid_name, scientificname)


## Exclude marine species using valid_name
afd_data |> 
  filter(FULL_NAME %in% marine_worms$valid_name) |>
  select(FULL_NAME) ->  excluded_mar_species

nrow(excluded_mar_species)

afd_data |> 
  filter(! FULL_NAME %in% marine_worms$valid_name) -> nomarine_nogriis_afd_species

## Check
nrow(nomarine_nogriis_afd_species) + nrow(excluded_mar_species) == nrow(afd_data)


## Exclude marine species using scientificname
nomarine_nogriis_afd_species |> 
  filter(FULL_NAME %in% marine_worms$scientificname) |> 
  select(FULL_NAME) -> excluded_mar_species_scientificname

nrow(excluded_mar_species_scientificname)

nomarine_nogriis_afd_species |> 
  filter(! FULL_NAME %in% marine_worms$scientificname) -> nomarine_nogriis_afd_species_snm

## Check
nrow(nomarine_nogriis_afd_species_snm) + nrow(excluded_mar_species) + nrow(excluded_mar_species_scientificname) == nrow(afd_data)


## Reporting 
message(paste("Found ", nrow(excluded_mar_species), " marine species by valid_name in WoRMS"))
message(paste("After exclusion by valid_name, a further", nrow(excluded_mar_species_scientificname), "marine species were found by scientificname in WoRMS"))
message(paste("The total number of AFD species remaining following complete exclusion is ", nrow(nomarine_nogriis_afd_species_snm)))



## Identifying duplicates ####
## $$ TO DO: Send outputs to JM for review

## List duplicates comparing all columns
nrow(afd_data[duplicated(afd_data),] )


## Create FULL_NAME + AUTHOR = COMPLETE_NAME column
afd_data %>% add_column(COMPLETE_NAME = paste0(trimws(afd_data$FULL_NAME), 
                                               ", ", trimws(afd_data$AUTHOR), 
                                               " ", trimws(afd_data$YEAR)), 
                        .after = "YEAR") -> afd_data

length(unique(afd_data$FULL_NAME))
length(unique(afd_data$COMPLETE_NAME))


# readr::write_csv(afd_data, file.path(outdir, "afd_Jan2023_clean.csv"))


## Are there duplicates in FULL_NAME & COMPLETE_NAME?
length(afd_data$FULL_NAME) == length(unique(afd_data$FULL_NAME))
length(afd_data$COMPLETE_NAME) == length(unique(afd_data$COMPLETE_NAME))


## Set as DT
afd_dt <- setDT(afd_data, key = c("FULL_NAME", "COMPLETE_NAME", "CONCEPT_GUID"))

## List of duplicates in FULL_NAME (excluding first appearance)
afd_dt$FULL_NAME[duplicated(afd_dt$FULL_NAME)]


## Duplicates in FULL_NAME (*including* first appearance)
afd_dt$FULL_NAME[duplicated(afd_dt$FULL_NAME) | duplicated(afd_dt$FULL_NAME, fromLast=TRUE)] ->
  dup_fullname

length (dup_fullname)
dup_fullname

## List of duplicates in COMPLETE_NAME (excluding first appearance)
afd_dt$COMPLETE_NAME[duplicated(afd_dt$COMPLETE_NAME)]


## Duplicates in COMPLETE_NAME (*including* first appearance)
afd_dt$COMPLETE_NAME[duplicated(afd_dt$COMPLETE_NAME) | duplicated(afd_dt$COMPLETE_NAME, fromLast=TRUE)] -> dup_completename

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
```







## HS script ####


####remove marine species using CAAB
## CAAB used for a finer sweep for local marine species-still under development?
## code from original splist_AFD_script
##to be removed from script?

Using CAAB data: https://www.cmar.csiro.au/data/caab/
  
  ```{r}
# Read in data
caab_species <- fread("databases/caab_dump_latest.csv")

# Create scientific name
caab_species |> mutate(scientific_name = ifelse(! is.na(SPECIES),
                                                paste0(GENUS," ", SPECIES),
                                                NA)) -> caab_species
# Overlap 
intersect(afd_data, caab_species) # ZERO

afd_data |> filter(VALID_NAME %in% caab_species) # ZER0 
```


## Identifying duplicates 

This section identify duplicates at the level of: 
  - row
- `VALID_NAME`
- `COMPLETE_NAME`

The number of species > the number of _unique_ species implies that there are duplicates.
JM: use `COMPLETE_NAME` for finding duplicates



```{r}
## List duplicates comparing all columns
afd_data[duplicated(afd_data),] 

## Look for duplicates in specific columns
sum(is.na(afd_data$VALID_NAME)) # Count number of NA in VALID_NAME
length(afd_data$VALID_NAME) # Total species
length(unique(afd_data$VALID_NAME)) # Total unique VALID_NAME
length(unique(afd_data$COMPLETE_NAME)) # Total unique COMPLETE_NAME
```

## Duplicates in COMPLETE_NAME (excluding first appearance)

```{r}
message(cat("Number of duplicated COMPLETE_NAME (excluding first appearance): "), 
        length(afd_data$COMPLETE_NAME[duplicated(afd_data$COMPLETE_NAME)]))
message("duplicated COMPLETE_NAME: ")
afd_data$COMPLETE_NAME[duplicated(afd_data$COMPLETE_NAME)]
```

## Duplicates in COMPLETE_NAME (*including* first appearance)

```{r}

afd_data$COMPLETE_NAME[duplicated(afd_data$COMPLETE_NAME) | duplicated(afd_data$COMPLETE_NAME, fromLast=TRUE)] -> dup_temp

message(cat("#duplicates in COMPLETE_NAME (including first appearance) : ", length(dup_temp)))
message("duplicated COMPLETE_NAME: ")
dup_temp

#remove duplicates from afd list


# readr::write_csv(temp, "output/afd_completename_repeats.csv") ##this didn't work when written like this so have changed to the code below
write.csv(dup_temp, "output/afd_completename_repeats.csv")

#write clean afd species list to .csv file
write.csv(afd_data, "output/afd_data_clean.csv")


## Various checks and tallies

Checks or tallies of species according to the number of words and particular formatting patterns in `VALID_NAME`

Note: '\' is an escape operator for various special characters

```{r}
## Counts for number of words
str_count(afd_data$VALID_NAME, pattern = "\\S+") |> janitor::tabyl()

## Counts for special characters in species names
stringr::str_count(afd_data$VALID_NAME, pattern = regex("\\\"")) |> sum() # Counting occurrences for \"
stringr::str_count(afd_data$VALID_NAME, pattern = regex("'")) |> sum() # Counting occurrences for '
stringr::str_count(afd_data$VALID_NAME, pattern = regex("\\(")) |> sum() 
stringr::str_count(afd_data$VALID_NAME, pattern = regex("\\[")) |> sum() 
```




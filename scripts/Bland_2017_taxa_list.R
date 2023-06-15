##----------------------##
## Bland_2017 taxa list ##
##----------------------##


## Script to create a list of Australian crayfish species.
## This script was used to partially generate a list of Australian crayfish species 
## (bland_taxa_list_updated.csv). This file is used in the data template script to 
## subset the Australian species from the global datasets in the Bland_2017_data_template.R script.


##-------------------------##
#### The start up basics ####
##-------------------------##


## Set working directory
getwd()
setwd(...) # set this to R project directory if needed

out_dir <- file.path(getwd(), "outputs")
dbout_dir <- file.path(out_dir, "other_dbs/Bland_2017")

## Load required package
library(tidyverse)


##--------------------------------------------------##
#### Create a list of Australian crayfish species ####
##--------------------------------------------------##

## Read in the AFD checklist cleaned for InverTraits.
## File was created from May 2023 of this script: https://github.com/payalbal/ausinvertraits.addons/blob/list-clean/scripts/AFD_checklist_clean.R
afd <- data.table::fread(file.path(out_dir, "afd_May2023_clean.csv"))

## Create species list.
bland_taxa_list <- bland_template %>%
  dplyr::filter(taxon_family %in% "Parastacidae", .preserve = FALSE) %>% # filter only rows in the family Parastacidae as all Australian crayfish belong in this family
  dplyr::select(taxon_name, taxon_family, taxname_source) %>%
  dplyr::distinct(taxon_name, .keep_all = TRUE) %>% #
  tibble::add_column(updated_taxon_name = "") %>% # add a column that will be manually added to
  tibble::add_column(notes = "") %>% # add a column that will be manually added to
  mutate(taxname_source = ifelse(taxon_name %in% afd$FULL_NAME, "AFD", taxname_source)) %>% # match species names to the AFD checklist.
  readr::write_csv(file.path(dbout_dir, "bland_taxa_list.csv")) # export the data as a csv into "outputs" folder

## We then manually checked the names that were not in the AFD checklist (taxname_source was NA).
## Of these taxa, 23 are not Australian, 7 names were misspellings of species names in AFD, 
## one species name was current but not in AFD, one name is now considered a subspecies, and 
## one is a synonym (a further one is a synonym but its name has been synonymised into three 
## different species so we were unable to give it a known name).
## The manually updated file is named bland_taxa_list_updated.csv and is used to subset the 
## Australian species in the in the Bland_2017_data_template.R script.
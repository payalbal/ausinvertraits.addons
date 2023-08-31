##--------------------##
## Plant scoring list ##
##--------------------##


## Script to create a list of plant names from the associated plant context
## in AusInverTraits and match to a plant fire susceptibility scoring data file
## derived from AusTraits.


##-------------------------##
#### The start up basics ####
##-------------------------##


## Set working directory
getwd()
setwd(...) # set this to R project directory if needed

out_dir <- file.path(getwd(), "outputs")

## Load required packages
library(tidyverse)

## Install package to match and update Australian plant species names 
#install.packages("remotes")
#remotes::install_github("traitecoevo/APCalign")

library(APCalign)

## Load the list of plant names from AusInverTraits.
p1 <- read_csv(file.path(getwd(), "data", "plant_scores/plant_names_Aug_2023.csv"), show_col_types = FALSE)

## This plant list was extracted from the database with the following code
# plant <- ausinverts$contexts %>% 
#   dplyr::filter(stringr::str_detect(context_property, "associated plant taxa")) %>% # filter out associated plant taxa context property
#   tidyr::separate_longer_delim(value, delim = ", ") %>% # separate out plant names
#   dplyr::arrange(stringr::str_squish(value)) %>% # sort plant names and remove white space
#   dplyr::distinct(value)  %>% # keep only unique plant names
#   dplyr::rename(species = value)  %>% # keep only unique plant names
#   readr::write_csv("plant_names_Aug_2023.csv") # save plant name list

## Load the plant scoring data file derived from AusTraits by Sophie Yang.
p2 <- read_csv(file.path(getwd(), "data", "plant_scores/plant_names_Aug_2023.csv"), show_col_types = FALSE)


##-------------------------------------##
####  Replace synonyms with current  ####
## names in the invertraits plant list ##
##-------------------------------------##

## retrieve the entire APC and APNI name databases and store locally
resources <- load_taxonomic_resources()

update_invertdb_names <- p1 %>% 
  pull(species) %>% 
  create_taxonomic_update_lookup(resources = resources)

update_invertdb_names %>% View

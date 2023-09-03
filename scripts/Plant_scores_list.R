##--------------------##
## Plant scoring list ##
##--------------------##


## Script to create a list of plant names from the associated plant context
## in AusInverTraits and match to a plant fire susceptibility scoring data 
## file derived from AusTraits.


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

## This plant list was extracted from the database with the following code:
# plant <- ausinverts$contexts %>% 
#   dplyr::filter(stringr::str_detect(context_property, "associated plant taxa")) %>% # filter out the associated plant taxa context property
#   tidyr::separate_longer_delim(value, delim = ", ") %>% # separate out plant names, one name per row
#   dplyr::arrange(stringr::str_squish(value)) %>% # sort plant names alphabetically and remove white space
#   dplyr::distinct(value)  %>% # keep only unique plant names
#   dplyr::rename(species = value)  %>% # rename the name variable
#   readr::write_csv("plant_names_Aug_2023.csv") # save plant name list

## Load the plant scoring data file derived from AusTraits by Sophie Yang.
p2 <- read_csv(file.path(getwd(), "data", "plant_scores/Jul_2023_plant_scores.csv"), show_col_types = FALSE)


##-------------------------------------##
####  Replace synonyms with current  ####
## names in the invertraits plant list ##
##-------------------------------------##

## Retrieve the entire APC and APNI name databases and store locally
resources <- load_taxonomic_resources()

## Query our plant names against the taxonomic resources
updated_names <- p1 %>% 
  pull(species) %>% 
  create_taxonomic_update_lookup(resources = resources)

## View the standardised names data
updated_names %>% View


## -- Manual name changes needed -- ##

# Acmena paniculatum to Syzygium paniculatum
# Weinmannia racemosa to Pterophylla racemosa (this is a weed)
# Caesalpiniaceae to Caesalpinioideae
# Mimosaceae to Caesalpinioideae
# Bombacaceae to Malvaceae
# Capparidaceae to Capparaceae
# Sterculiaceae to Malvaceae
# Eucalyptus hybrids (not sure what to do with these ones)
# Otion - not a current genus (is now Gompholobium and Aotus so not able to make a firm change)
# Stipa - not a current genus
# Bignonia - not found in Australia (I think)


# -- Notes --
# The plant family names can't be used because there are not plant scores at this level
# We need to decide when to implement code that will update plant names in the database


##-----------------------------------------##
#### Match updated names to plant scores ####
###    and create list of missing names   ###
##-----------------------------------------##

## Join the updated names to the plant scores data file

## Create a new tibble with updated names
plant_names <- updated_names %>% 
  dplyr::select(original_name, accepted_name, genus) %>% 
  dplyr::mutate(updated_name = dplyr::coalesce(accepted_name, genus))  %>% # replace NAs in the accepted_name column, which exist for the genera-only names, with names from the genus column
  dplyr::left_join(p2, by = join_by(updated_name == taxon_name))  %>% # merge the plant score data file with the updated names
  dplyr::mutate(score_GM = case_when(score_GM == "#NUM!" ~ NA, .default = as.character(score_GM))) # replace rogue values in the score column

## Create list of plant names not in plant scores data file
missing_names <- plant_names %>%
  dplyr::filter(!is.na(updated_name)) %>% # remove NAs in name column (these were family names only)
  dplyr::filter(is.na(score_GM)) %>% # keep rows with NA in the score column (i.e., those that have no score data)
  dplyr::distinct(updated_name) %>% # keep only unique plant names
  readr::write_csv(file.path(out_dir, "missing_plant_names_Sep_2023.csv")) # save plant name list

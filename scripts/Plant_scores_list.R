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
# install.packages("remotes")
# remotes::install_github("traitecoevo/APCalign")

library(APCalign)

## Load the list of plant names from AusInverTraits.
p1 <- read_csv(file.path(getwd(), "data", "plant_scores/plant_names_13Sep2023.csv"), show_col_types = FALSE)

## This plant list was extracted from the database with the following code:
# plant <- ausinverts$contexts %>% 
#   dplyr::filter(stringr::str_detect(context_property, "associated plant taxa")) %>% # filter out the associated plant taxa context property
#   tidyr::separate_longer_delim(value, delim = ", ") %>% # separate out plant names, one name per row
#   dplyr::arrange(stringr::str_squish(value)) %>% # sort plant names alphabetically and remove white space
#   dplyr::distinct(value)  %>% # keep only unique plant names
#   dplyr::rename(species = value)  %>% # rename the name variable
#   readr::write_csv("plant_names_13Sep2023.csv") # save plant name list

## Load the plant scoring data file derived from AusTraits by Sophie Yang.
p2 <- read_csv(file.path(getwd(), "data", "plant_scores/SCORE_plant_recovery_traits_updated_sep_2023.csv"), show_col_types = FALSE)


##-------------------------------------##
####  Replace synonyms with current  ####
## names in the invertraits plant list ##
##-------------------------------------##

## Retrieve the entire APC and APNI name databases and store locally
resources <- load_taxonomic_resources()

## Query our plant names against the taxonomic resources
standardised_names <- p1 %>% 
  pull(species) %>% 
  create_taxonomic_update_lookup(resources = resources)

## View the standardised names data
standardised_names %>% View

## Save the updated and standardised plant names to csv.
readr::write_csv(standardised_names, file.path(out_dir, "apc_updated_associated_plant_names_invertraits_14sep2023.csv"))

## -- Manual name changes needed -- ##

## DONE

# Acmena paniculatum to Syzygium paniculatum
# Weinmannia racemosa to Pterophylla racemosa (this is a weed)
# Caesalpiniaceae to Caesalpinioideae
# Mimosaceae to Caesalpinioideae
# Bombacaceae to Malvaceae
# Capparidaceae to Capparaceae
# Sterculiaceae to Malvaceae

## -- Names that are unknown but need to be left as is -- ##

# Eucalyptus hybrids (not sure what to do with these ones)
# Otion - not a current genus (is now Gompholobium and Aotus so not able to make a firm change)
# Stipa - not a current genus
# Bignonia - not found in Australia (I think)

## -- NOTES -- ##
# The plant family names can't be used because there are not plant scores at this level
# We need to decide when to implement code that will update plant names in the database


##-------------------------------------------------##
####  Match updated names to plant scores, make  ####
## list of missing names, create scores data file ###
##-------------------------------------------------##

## Join the updated names to the plant scores data file & create
## a data set of scores of associated plants in AusInverTraits
plant_names <- standardised_names %>% 
  dplyr::select(original_name, accepted_name, genus, update_reason) %>% # select only relevant columns
  dplyr::mutate(updated_name = dplyr::coalesce(accepted_name, genus)) %>% # replace NAs in the accepted_name column, which exist for the genera-only names, with names from the genus column
  dplyr::distinct(updated_name, .keep_all = TRUE) %>% # keep only unique plant names
  dplyr::left_join(p2, by = join_by(updated_name == taxon_name)) %>% # merge the plant score data file with the updated names
  dplyr::filter(!is.na(score_GM)) %>% # filter rows/plants with values for geometric mean score
  dplyr::select(-original_name, -accepted_name, -genus, -update_reason) %>% # remove columns not needed
  readr::write_csv(file.path(out_dir, "invertraits_plant_recovery_scores_15Sep2023.csv")) 

## Create list of plant names not in plant scores data file
missing_names <- standardised_names %>%
  dplyr::select(original_name, accepted_name, genus, update_reason) %>% # select only relevant columns
  dplyr::mutate(updated_name = dplyr::coalesce(accepted_name, genus)) %>% # replace NAs in the accepted_name column, which exist for the genera-only names, with names from the genus column
  dplyr::distinct(updated_name, .keep_all = TRUE) %>% # keep only unique plant names
  dplyr::left_join(p2, by = join_by(updated_name == taxon_name)) %>% # merge the plant score data file with the updated names
  dplyr::filter(!is.na(updated_name)) %>% # remove NAs in name column (these were family or not current genus names only)
  dplyr::filter(is.na(score_GM)) %>% # keep rows with NA in the score column (i.e., those that have no score data)
  readr::write_csv(file.path(out_dir, "missing_plant_names_Sep_2023.csv")) # save plant name list

##################################
## Other databases - Bland_2017 ##
##################################

## Script to clean and map the Bland_2017 
## crayfish dataset to IA data_template.

##---------------------##
## The start up basics ##
##---------------------##

## Load required packages
library(tidyverse)

## Load the Bland dataset
bland <- read.csv("./data/Bland_2017/Crayfish_Species_Dataset.csv", header = T) 
template <- read.csv("./data/Bland_2017/data_template.csv", header = T) 

## Check the datasets 
str(bland)
view(template)


##---------------------------------------------------------##
## Modify the dataset in preparation for the data template ##
##---------------------------------------------------------##

bland_mod <- bland %>%
  dplyr::select(-X, -Genus, -RedList, -ChelaSize, -ChelaShape, -HabitatsIUCN) %>%   # drop unnecessary columns
  dplyr::rename(
    taxon_name = Binomial,
    taxon_family = Family,
    occipital_carapace_length = BodySize,
    fecundity_per_reproductive_event = EggNumber,
    microhabitat_activity = HabitatType
  ) %>%  # change column names to match data_template column names
  dplyr::mutate(taxon_family = str_to_title(taxon_family)) %>%  # change family names to title case
  dplyr::mutate(microhabitat_shelter = microhabitat_activity) %>%  # duplicate the microhabitat_activity column and call microhabitat_shelter
  tidyr::pivot_longer(cols = occipital_carapace_length:microhabitat_shelter,
                      names_to = "trait_name",
                      values_to = "value")  # convert to long format so that all trait names and values are in two columns


##-----------------------------------------------------------##
## Merge the dataset & data template, & occupy missing cells ##
##-----------------------------------------------------------##
  
bland_template <- template %>%
  dplyr::full_join(bland_mod) %>%  # merge with empty data_template
  dplyr::filter(if_any(everything(), ~ !is.na(.))) %>%  # remove rows with all NAs (from data template)
  dplyr::mutate(taxname_source = "AFD") %>%  # add taxname_source
  dplyr::mutate(entity_type_tax = "species") %>%  # add entity_type_tax
  dplyr::mutate(life_stage_generic = "adult") %>%  # add life_stage_generic
  dplyr::mutate(life_stage_generic = if_else(
    trait_name %in% c(
      "fecundity_per_reproductive_event",
      "microhabitat_activity",
      "microhabitat_shelter"),"all",life_stage_generic)) %>%  # change life_stage_generic to "all" for certain traits
  dplyr::mutate(sex = "all") %>% # add sex
  dplyr::mutate(entity_type = "species") %>% # add entity_type
  dplyr::mutate(value_type = case_when(
    trait_name == "occipital_carapace_length" ~ "maximum",
    trait_name == "fecundity_per_reproductive_event" ~ "maximum",
    trait_name == "EOO" ~ "raw")) %>%  # specify value_type for numerical traits
  dplyr::mutate(unit_numeric = case_when(
    trait_name == "occipital_carapace_length" ~ "mm",
    trait_name == "fecundity_per_reproductive_event" ~ "offspring",
    trait_name == "EOO" ~ "km2")) %>%  # specify unit_numeric for numerical traits
  dplyr::mutate(methods = case_when(
    trait_name == "occipital_carapace_length" ~ "I collected maximum body size (mm) from species descriptions, field guides and museum specimens (references available on request). I used maximum body size as mean body size is generally not available for crayfish species. I found three measures of crayfish body size: occipital carapace length (OCL), carapace length (CL), and body length (BL). I used CL as the preferred measure of body size as it was available for most species (397 species). For species for which maximum CL was missing, I preferentially transformed OCL values, as crayfish BL is a more variable measure of crayfish body size than OCL. I corrected OCL into CL for 53 species and BL into CL for 79 species. I developed correction factors between OCL and CL, and BL and CL from a database of morphological measurements of 1,743 specimens. These measurements were obtained from species descriptions, museum plates, museum specimens and field specimens (references available on request). I used species-specific correction factors when available, if not I used genus-specific correction factors (29 species).",
    trait_name == "fecundity_per_reproductive_event" ~ "Maximum number of eggs taken from species descriptions, field guides and museum specimens.",
    trait_name == "microhabitat_activity" ~ "I followed Adamowicz & Purvis (2006) and assigned species to four habitat types: (1) streams and rivers, (2) lakes and wetlands, (3) burrows, and (4) caves. I used data from Adamowicz & Purvis (2006) for 490 species, and used IUCN assessments (IUCN 2010), field guides and species descriptions to classify the remaining species. “Strong burrowers” in the Australian scheme of Riek (1972) were also placed in “burrows”, while weak or moderate burrowers were placed in categories “streams and rivers” or “lakes and wetlands” according to other habitat information.",
    trait_name == "microhabitat_shelter" ~ "I followed Adamowicz & Purvis (2006) and assigned species to four habitat types: (1) streams and rivers, (2) lakes and wetlands, (3) burrows, and (4) caves. I used data from Adamowicz & Purvis (2006) for 490 species, and used IUCN assessments (IUCN 2010), field guides and species descriptions to classify the remaining species. “Strong burrowers” in the Australian scheme of Riek (1972) were also placed in “burrows”, while weak or moderate burrowers were placed in categories “streams and rivers” or “lakes and wetlands” according to other habitat information.",
    trait_name == "EOO" ~ "I recorded the geographical range size for each species from IUCN (2010).")) %>%  # add methods for the different traits
  dplyr::mutate(source_key = "Bland_2017") %>% # add source_key
  dplyr::mutate(source_doi = "doi: 10.1111/acv.12350") %>% # add source_doi
  dplyr::mutate(source_citation = "Bland, L. M. (2017). Global correlates of extinction risk in freshwater crayfish. Animal Conservation, 20(6), 532-542.") %>% # add source_citation
  dplyr::mutate(source_type = "article") # add source_type

## More to do to finish occupying the data template. See the TO DO list below.


##--------------------------------##
## Export the Bland_2017 data.csv ##
##--------------------------------##

write.csv(bland_template, "./outputs/other_dbs/Bland_2017/data.csv", row.names = FALSE)


##--------##
## TO DO: ##
##--------##

# (1) Move all of the environmental variables which are currently 
# in the trait_name and value columns to site_description for each 
# species and trait_name combination. In the site_description columns,
# they could be positioned "trait_name/env_var1": "value"; "trait_name/env_var2": "value" etc.
# The environmental variables are: Temp, TempSeas, Prec, PrecSeas, 
# ElevMin, Consumption, Fragment, Mercury, Pesticide, Sed, Cropland, 
# Livestock, HPD).

# (2) Change category level names of the both trait_name 
# microhabitat_activity and microhabitat_shelter to:
# value 1 to "water_lotic"
# value 2 to "water_lentic"
# value 3 to "burrow"
# value 4 to "troglofauna"
# I tried the code below, but it doesn't quite work because
# every value that doesn't match what is specified is changed to NA.
# (thus we lose all of the numerical values for the other traits).

# dplyr::mutate(value = case_when(
# trait_name == "microhabitat_activity" & value == 1 ~ "water_lotic",
# trait_name == "microhabitat_activity" & value == 2 ~ "water_lentic",
# trait_name == "microhabitat_activity" & value == 3 ~ "burrow",
# trait_name == "microhabitat_activity" & value == 4 ~ "troglofauna")) %>%  # change trait categories to match IA trait levels

## Data table approach
dt <- as.data.table(bland_template) ## convert data to data.table format; you needn't create a new object if you're runnign this step last: bland_template <- as.data.table(bland_template)
names(dt) ## to see data table column names
str(dt) ## to see types of data in each column
dt$value <- as.character(dt$value) ## change column type of character
str(dt) ## check that value column type


dt[trait_name == "microhabitat_activity" & value == 1]$value ## just a check 
dt[trait_name == "microhabitat_activity" & value == 1]$value = "water_lotic"

dt[trait_name == "microhabitat_activity" & value == 2]$value ## just a check 
dt[trait_name == "microhabitat_activity" & value == 2]$value = "water_lentic"

dt[trait_name == "microhabitat_activity" & value == 3]$value ## just a check 
dt[trait_name == "microhabitat_activity" & value == 3]$value = "burrow"


dt[trait_name == "microhabitat_activity" & value == 4]$value ## just a check 
dt[trait_name == "microhabitat_activity" & value == 4]$value = "troglofauna"


dt[trait_name == "microhabitat_activity" ]$value ## to check value column

write.csv(dt, "./outputs/other_dbs/Bland_2017/db_data.csv", row.names = FALSE)




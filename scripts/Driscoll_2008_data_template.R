##---------------##
## Driscoll_2008 ##
##---------------##

## Script to clean and map the Driscoll_2008 dataset to the InverTraits
## data_template and create a data.csv for the database.

##-------------------------##
#### The start up basics ####
##-------------------------##

## Set working directory
getwd()
setwd(...) # set this to R project directory if needed

out_dir <- file.path(getwd(), "outputs/other_dbs/Driscoll_2008")

## Load required package
library(tidyverse)

## Load the Driscoll_2008 dataset
dtas <- read_csv(file.path(getwd(), "data", "Driscoll_2008/Driscoll_2008_beetles_SW_Tasmania.csv"), show_col_types = FALSE)

## Load the data template
template <- read_csv(file.path(getwd(), "data", "Driscoll_2008/data_template.csv"), show_col_types = FALSE) 

## Load the data extracted from reading the Driscoll_2008 paper
dtas_paper <- read_csv(file.path(getwd(), "data", "Driscoll_2008/Driscoll_2008.csv"), show_col_types = FALSE) 


##-------------------------------------------------------------##
#### Modify the dataset in preparation for the data template ####
##-------------------------------------------------------------##

dtas_mod <- dtas %>%
  dplyr::rename(
    body_length = Size,
    wing_development = Flying,
    functional_role = Ecology) %>% # change column names to match data_template column names
  dplyr::mutate(taxon_family = str_to_title(taxon_family)) %>% # change values to title case
  dplyr::mutate(
    wing_development = case_when(
      wing_development == "y" ~ "winged",
      wing_development == "n" ~ "wingless",
      .default = as.character(wing_development))) %>% # modify wing_development categories to match IA trait
  dplyr::mutate(
    functional_role = case_when(
      functional_role == "carn" ~ "predator",
      functional_role == "detr" ~ "detritivore",
      functional_role == "fung" ~ "fungivore",
      functional_role == "fung?" ~ "fungivore",
      functional_role == "herb" ~ "herbivore",
      functional_role == "scav" ~ "necrophage")) %>% # modify functional_role categories to match IA trait
  dplyr::mutate(microhabitat_activity = NA) %>% # create new column for microhabitat_activity
  dplyr::mutate(
    microhabitat_activity = case_when(
      functional_role == "fungivore" ~ "fungus",
      functional_role == "herbivore" ~ "vegetation",
  .default = as.character(microhabitat_activity))) %>% # add microhabitat_activity trait value given the functional roles
  dplyr::select(taxon_name, taxon_name_original, taxname_source, taxon_family, 
                body_length, wing_development, functional_role, 
                microhabitat_activity) %>% # keep only needed columns
  dplyr::mutate(body_length = as.character(body_length))  %>% # set numeric column to character so that all trait values can be combined into one column
  tidyr::pivot_longer(cols = body_length:microhabitat_activity,
                      names_to = "trait_name",
                      values_to = "value") %>%  # convert to long format so that all trait names and values are in two columns
  tidyr::drop_na(value) # remove rows with NA in the value column


##-------------------------------------------------------------------------##
#### Merge the dataset with the IA data template, & occupy missing cells ####
##-------------------------------------------------------------------------##

dtas_template <- template %>%
  dplyr::full_join(dtas_mod) %>%  # merge the modified dataset with empty data_template
  dplyr::filter(if_any(everything(), ~ !is.na(.))) %>%  # remove rows with all NAs (from data template)
  dplyr::mutate(entity_type_tax = "species") %>%  # add entity_type_tax category for all traits
  dplyr::mutate(life_stage_generic = "adult") %>%  # add life stage for all traits
  dplyr::mutate(entity_type = "metapopulation") %>%  # add entity_type category for all traits
  dplyr::mutate(
    value_type = case_when(
      trait_name == "body_length" ~ "median", 
      .default = as.character(value_type))) %>%  # add value_type for the one numerical trait
  dplyr::mutate(
    unit_numeric = case_when(
      trait_name == "body_length" ~ "mm", 
      .default = as.character(unit_numeric))) %>%  # add unit for the one numerical trait
  dplyr::mutate(methods = "At each of three locations (North, Anne, Scotts), habitat patches from seven landscape elements 
      were sampled, including (1) Connected: Eucalyptus scrub at the edge of continuous forest; (2) Close: small patches of scrub 
      within 100 m of continuous forest, (3) Mid: small patches of scrub 100 - 420 m from continuous forest; (4) Distant: small 
      patches of scrub 420 - 780 m from continuous forest; (5) Dense: small patches of scrub with several other small patches within 
      50 m; (6) Stream: linear scrub along streams surrounded by buttongrass; (7) Matrix: buttongrass sampled 20 - 100 m from nearest 
      scrub patch. Each landscape element was replicated three times at each location, with the exceptions of North (three 
      Distant, four Mid), Anne (five Distant, two Mid), Scotts (six Distant, two Mid). The cut-off distance between Mid and Distant 
      sites was selected to ensure at least two sites in each category at each location. A total of 58 bush sites and nine buttongrass 
      sites were sampled. All discrete patches (Close, Mid, and Distant landscape elements) were small (mean size 689 m2, range 
      245 - 1211 m2). Each site was sampled using 16 pairs of 225 ml pit-fall traps (32 cups per site), spaced at 5 m intervals and 
      with 50 ml of saturated salt solution as a preservative. Plastic lids held 5 cm above the ground prevented traps from filling 
      with rain. Traps were set in December 2002 and left open for eight consecutive weeks over summer, when beetle activity is 
      highest. The collections were sorted to morphospecies then identified as far as possible. Most Staphylinidae were excluded from 
      the data set because they are difficult to distinguish morphologically, although one common, distinctive taxon Baeocera sp. A 
      was included.") %>%  # add methods for all traits
  dplyr::mutate(
    methods = case_when(
      trait_name == "body_length" ~ "Once collected, a few beetles were measured of each taxon and the median used.At each of three locations (North, Anne, Scotts), habitat patches from seven landscape elements 
      were sampled, including (1) Connected: Eucalyptus scrub at the edge of continuous forest; (2) Close: small patches of scrub 
      within 100 m of continuous forest, (3) Mid: small patches of scrub 100 - 420 m from continuous forest; (4) Distant: small 
      patches of scrub 420 - 780 m from continuous forest; (5) Dense: small patches of scrub with several other small patches within 
      50 m; (6) Stream: linear scrub along streams surrounded by buttongrass; (7) Matrix: buttongrass sampled 20 - 100 m from nearest 
      scrub patch. Each landscape element was replicated three times at each location, with the exceptions of North (three 
      Distant, four Mid), Anne (five Distant, two Mid), Scotts (six Distant, two Mid). The cut-off distance between Mid and Distant 
      sites was selected to ensure at least two sites in each category at each location. A total of 58 bush sites and nine buttongrass 
      sites were sampled. All discrete patches (Close, Mid, and Distant landscape elements) were small (mean size 689 m2, range 
      245 - 1211 m2). Each site was sampled using 16 pairs of 225 ml pit-fall traps (32 cups per site), spaced at 5 m intervals and 
      with 50 ml of saturated salt solution as a preservative. Plastic lids held 5 cm above the ground prevented traps from filling 
      with rain. Traps were set in December 2002 and left open for eight consecutive weeks over summer, when beetle activity is 
      highest. The collections were sorted to morphospecies then identified as far as possible. Most Staphylinidae were excluded from 
      the data set because they are difficult to distinguish morphologically, although one common, distinctive taxon Baeocera sp. A 
      was included.",
      trait_name == "wing_development" ~ "Once collected, beetle taxa were scored for presence or absence of functional wings.
      At each of three locations (North, Anne, Scotts), habitat patches from seven landscape elements 
      were sampled, including (1) Connected: Eucalyptus scrub at the edge of continuous forest; (2) Close: small patches of scrub 
      within 100 m of continuous forest, (3) Mid: small patches of scrub 100 - 420 m from continuous forest; (4) Distant: small 
      patches of scrub 420 - 780 m from continuous forest; (5) Dense: small patches of scrub with several other small patches within 
      50 m; (6) Stream: linear scrub along streams surrounded by buttongrass; (7) Matrix: buttongrass sampled 20 - 100 m from nearest 
      scrub patch. Each landscape element was replicated three times at each location, with the exceptions of North (three 
      Distant, four Mid), Anne (five Distant, two Mid), Scotts (six Distant, two Mid). The cut-off distance between Mid and Distant 
      sites was selected to ensure at least two sites in each category at each location. A total of 58 bush sites and nine buttongrass 
      sites were sampled. All discrete patches (Close, Mid, and Distant landscape elements) were small (mean size 689 m2, range 
      245 - 1211 m2). Each site was sampled using 16 pairs of 225 ml pit-fall traps (32 cups per site), spaced at 5 m intervals and 
      with 50 ml of saturated salt solution as a preservative. Plastic lids held 5 cm above the ground prevented traps from filling 
      with rain. Traps were set in December 2002 and left open for eight consecutive weeks over summer, when beetle activity is 
      highest. The collections were sorted to morphospecies then identified as far as possible. Most Staphylinidae were excluded from 
      the data set because they are difficult to distinguish morphologically, although one common, distinctive taxon Baeocera sp. A 
      was included.",
      .default = as.character(methods))) %>%  # add specific methods for body_length and wing_development
  dplyr::mutate(site_name = "near Lake Pedder, southwest TAS") %>%  # add site name
  dplyr::mutate(site_date_of_visit = "2002-12/2003-01") %>%  # add study date
  dplyr::mutate(site_description = "Sedgelands dominated by buttongrass Gymnoschoenus sphaerocephalus and heaths (Epacridaceae) with
                Eucalyptus scrub patches often less than a hectare that can regenerate from lignotubers after fire. The Eucalyptus 
                scrub patches have an overstorey of multi-stemmed Eucalyptus nitida usually less than 10 m tall, a dense midstorey up 
                to 3 m high of Banksia marginata and Leptospermum species, with the entangling shrub Bauera rubioides, and sword grass 
                Gahnia grandis. Eucalyptus nitida patches occur at the margins of continuous forest, as strips along streams and as 
                small patches completely surrounded by buttongrass. Annual rainfall is approximately 2500 mm. Elevation ranged from 
                300 to 400 m above sea level. Site codes were Anne, North, Scotts.") %>%  # add site description
  dplyr::mutate(source_key = "Driscoll_2008") %>%  # add source_key
  dplyr::mutate(source_doi = "doi: 10.1111/j.2007.0030-1299.16202.x") %>%  # add source_doi
  dplyr::mutate(source_citation = "Driscoll, D. A. (2008). The frequency of metapopulations, metacommunities and nestedness in a fragmented landscape. Oikos, 117(2), 297-309.") %>%  # add source_citation
  dplyr::mutate(source_type = "article") %>% # add source_type
  dplyr::bind_rows(dtas_paper) %>% # add data extracted from reading paper
  dplyr::filter(if_any(everything(), ~ !is.na(.))) %>%  # remove rows with all NAs (from dtas_paper)
  readr::write_csv(file.path(out_dir, "data.csv")) # save final dataset

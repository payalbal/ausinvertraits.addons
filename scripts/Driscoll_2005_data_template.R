##-----------------------##
## Driscoll_2005 dataset ##
##-----------------------##


## Script to clean and map the Driscoll_2005 beetles and habitat fragmentation
## dataset to the InverTraits data_template and create a data.csv for the database.


##-------------------------##
#### The start up basics ####
##-------------------------##


## Set working directory
getwd()
setwd(...) # set this to R project directory if needed

out_dir <- file.path(getwd(), "outputs/other_dbs/Driscoll_2005")

## Load required package
library(tidyverse)

## Load the Driscoll_2005 dataset
mallee <- read_csv(file.path(getwd(), "data", "Driscoll_2005/Driscoll_2005_Beetles_NSW_Mallee_Driscoll_and_Weir_2005.csv"), show_col_types = FALSE)

## Load the data template
template <- read_csv(file.path(getwd(), "data", "Driscoll_2005/data_template.csv"), show_col_types = FALSE) 

## Load the data extracted from reading the Driscoll_2005 paper
mallee_paper <- read_csv(file.path(getwd(), "data", "Driscoll_2005/Driscoll_2005.csv"), show_col_types = FALSE) 


## QUESTIONS FOR DON:

## Do you have size in mm (rather than size classes)?
## Does trophic_level apply to adults only? For foodtype descriptions, when "adult/larvae" are not mentioned, are the descriptions for adults? Or both larvae and adults?
## Does usual habitat for adults, "concealed places/nocturnal" mean "bark rock" as in other studies?

## NOTES:

##-------------------------------------------------------------##
#### Modify the dataset in preparation for the data template ####
##-------------------------------------------------------------##


## (1) Modify the dataset in preparation for the data template.

mallee_mod <- mallee %>%
  dplyr::filter(is.na(introduced) | introduced != "introduced") %>% # exclude introduced taxa
  dplyr::rename(
    taxon_name = unique_name,
    taxon_family = family,
    wing_development = flightless,
    functional_role_adult = trophic_level,
    microhabitat_activity_adult = usual_habitat_adult,
    microhabitat_activity_larva = usual_habitat_larvae) %>% # change column names to match data_template column names
  dplyr::mutate(functional_role_larva = NA)  %>% # create new functional_role columns for larva life stage
  dplyr::mutate(wing_development_male = NA)  %>% # create new functional_role columns for larva life stage
  dplyr::mutate(wing_development_female = NA)  %>% # create new functional_role columns for larva life stage
  dplyr::select(taxon_name, taxon_name_original, taxname_source, taxon_family, wing_development,
                wing_development_male, wing_development_female, functional_role_adult, functional_role_larva, adult_position,
                foodtype, microhabitat_activity_adult, microhabitat_activity_larva) %>% # keep only needed columns for now
  dplyr::mutate(
    wing_development_female = case_when(
      wing_development == "no(m)_yes(f)" ~ "wingless",
      .default = as.character(wing_development_female))) %>% # change flightless category for female beetles where specified
  dplyr::mutate(
    wing_development_male = case_when(
      wing_development == "no(m)_yes(f)" ~ "winged",
      .default = as.character(wing_development_male))) %>% # change flightless category for female beetles where specified
  dplyr::mutate(
    wing_development = case_when(
      wing_development == "yes" ~ "wingless",
      wing_development == "no" ~ "winged",
      wing_development == "no(m)_yes(f)" ~ NA)) %>% # change flightless category to "wingless", and flight category to "winged"
  dplyr::mutate(functional_role_adult = case_when(
    functional_role_adult == "adult carnivore/larva herbivore" ~ "predator",
    functional_role_adult == "carnivore" ~ "predator",
    functional_role_adult == "humus/fungus feeder" ~ "detritivore fungivore",
    functional_role_adult == "omnivore(granivore)" ~ "omnivore granivore",
    foodtype == "dead plant material/roots" ~ "detritivore herbivore",
    functional_role_adult == "scavenger" ~ "detritivore",
    .default = as.character(functional_role_adult))) %>% # specify adult functional_role trait levels given information in other columns
  dplyr::mutate(functional_role_larva = case_when(
    functional_role_adult == "adult carnivore/larva herbivore" ~ "herbivore",
    foodtype == "adult flowers/larvae humus or root feeders" ~ "detritivore herbivore",
    foodtype == "adult foliage Eucalypts/ larvae humus or grass roots" ~ "detritivore herbivore",
    foodtype == "adults flowers/foliage Eucalypts/larvae humus or roots" ~ "detritivore herbivore",
    foodtype == "adults flowers/larvae rotten wood" ~ "xylophage",
    foodtype == "adults foliage Eucalypts/larvae humus or roots" ~ "detritivore herbivore",
    foodtype == "larvae feed on termites" ~ "predator",
    foodtype == "larvae humus or root feeder" ~ "detritivore herbivore",
    foodtype == "larvae humus or roots" ~ "detritivore herbivore",
    foodtype == "larvae humus/plant root feeder" ~ "detritivore herbivore",
    foodtype == "larvae plant roots etc" ~ "herbivore",
    foodtype == "larvae prey on ants" ~ "predator",
    foodtype == "larvaeplant roots etc" ~ "herbivore",
    microhabitat_activity_larva == "in dung-provisioned burrows" ~ "coprophage",
    microhabitat_activity_larva == "in fungi" ~ "fungivore",
    .default = as.character(functional_role_larva))) %>% # specify larva functional_role trait levels given information in other columns
  dplyr::mutate(microhabitat_activity_adult = case_when(
    microhabitat_activity_adult == "active during the day, not nocturnal" ~ "bark rock",
    microhabitat_activity_adult == "arboreal/foliage Eucalypts" ~ "arboreal_canopy",
    microhabitat_activity_adult == "arboreal/live under bark" ~ "bark",
    microhabitat_activity_adult == "arboreal/lives under bark" ~ "bark",
    microhabitat_activity_adult == "arboreal/under bark" ~ "bark",
    microhabitat_activity_adult == "carcasses/skins/dung" ~ "animal_carcass",
    microhabitat_activity_adult == "concealed places/nocturnal" ~ "bark rock",
    microhabitat_activity_adult == "concealed places/nocturnal/arboreal?" ~ "bark rock arboreal_canopy",
    microhabitat_activity_adult == "fossorial" ~ "burrow",
    microhabitat_activity_adult == "fossorial/burrowing/nocturnal" ~ "burrow",
    microhabitat_activity_adult == "fossorial/nocturnal" ~ "burrow",
    microhabitat_activity_adult == "fossorial/nocturnal/open scrub/sandy soil" ~ "burrow",
    microhabitat_activity_adult == "fossorial/nocturnal/sandy soil" ~ "burrow",
    microhabitat_activity_adult == "fossorial/riparian/mud" ~ "burrow riparian",
    microhabitat_activity_adult == "ground" ~ "ground_open",
    microhabitat_activity_adult == "hygrophilus" ~ "riparian",
    microhabitat_activity_adult == "in burrows in soil" ~ "burrow",
    microhabitat_activity_adult == "in dung" ~ "dung",
    microhabitat_activity_adult == "in fungi" ~ "fungus",
    microhabitat_activity_adult == "litter" ~ "litter_ground",
    microhabitat_activity_adult == "low woodland/pastures" ~ "ground_open",
    microhabitat_activity_adult == "nocturnal" ~ "bark rock",
    microhabitat_activity_adult == "nomadic/nocturnal/ground" ~ "ground_open",
    microhabitat_activity_adult == "riparian/nocturnal" ~ "riparian",
    microhabitat_activity_adult == "subterranean and borrowing/nocturnal" ~ "burrow",
    microhabitat_activity_adult == "subterranean and burrowing" ~ "burrow",
    microhabitat_activity_adult == "subterranean/burrowing/nocturnal" ~ "burrow",
    microhabitat_activity_adult == "tall open shrubland" ~ "ground_open",
    microhabitat_activity_adult == "under bark/logs" ~ "bark under_dead_wood_ground",
    functional_role_adult == "coprophage" ~ "dung",
    functional_role_adult == "herbivore" ~ "vegetation",
    .default = as.character(microhabitat_activity_adult))) %>% # specify adult microhabitat_activity given information in other columns
  dplyr::mutate(microhabitat_activity_larva = case_when(
    microhabitat_activity_larva == "in burrows in ground" ~ "burrow",
    microhabitat_activity_larva == "in burrows in soil" ~ "burrow",
    microhabitat_activity_larva == "in dung-provisioned burrows" ~ "burrow",
    microhabitat_activity_larva == "in fungi" ~ "fungus",
    microhabitat_activity_larva == "in rotten wood" ~ "in_dead_wood_ground in_standing_wood_dead",
    microhabitat_activity_larva == "in soil" ~ "soil",
    microhabitat_activity_larva == "in soil or decaying logs" ~ "soil in_dead_wood_ground",
    microhabitat_activity_larva == "in termite mounds or logs" ~ "mound_above_ground in_dead_wood_ground",
    microhabitat_activity_larva == "live with ants" ~ "burrow",
    microhabitat_activity_larva == "live with ants ?" ~ "burrow",
    microhabitat_activity_larva == "subterranean" ~ "soil",
    microhabitat_activity_larva == "subterranean ?" ~ "soil",
    microhabitat_activity_larva == "subterranean and burrowing" ~ "burrow",
    microhabitat_activity_larva == "subterranean/burrowing" ~ "burrow",
    .default = as.character(microhabitat_activity_larva))) %>% # specify larva microhabitat_activity given information in other columns
  dplyr::rename(measurement_remarks = foodtype) %>% # rename column that has measurement remarks
  dplyr::select(taxon_name, taxon_name_original, taxname_source, taxon_family, measurement_remarks,
                wing_development, wing_development_female, wing_development_male, functional_role_larva, functional_role_adult, 
                functional_role_larva, microhabitat_activity_adult, microhabitat_activity_larva) %>% # keep and reorder only columns needed
  tidyr::pivot_longer(cols = wing_development:microhabitat_activity_larva,
                      names_to = "trait_name",
                      values_to = "value") %>%  # convert to long format so that all trait names and values are in two columns
  tidyr::drop_na(value) # remove rows with NA in the value column
  

##-------------------------------------------------------------------------##
#### Merge the dataset with the IA data template, & occupy missing cells ####
##-------------------------------------------------------------------------##

mallee_template <- template %>%
  dplyr::full_join(mallee_mod) %>%  # merge with empty data_template
  dplyr::filter(if_any(everything(), ~ !is.na(.))) %>%  # remove rows with all NAs (from data template)
  dplyr::mutate(entity_type_tax = "species") %>%  # add entity_type_tax category
  dplyr::mutate(
    life_stage_generic = case_when(
      trait_name == "functional_role_adult" ~ "adult",
      trait_name == "functional_role_larva" ~ "juvenile",
      trait_name == "microhabitat_activity_adult"~ "adult",
      trait_name == "microhabitat_activity_larva"~ "juvenile",
      trait_name == "wing_development" ~ "adult",
      trait_name == "wing_development_female" ~ "adult",
      trait_name == "wing_development_male" ~ "adult")) %>%  # add life stage for the traits
  dplyr::mutate(
    life_stage_taxon_specific = case_when(
      trait_name == "functional_role_larva" ~ "larva",
      trait_name == "microhabitat_activity_larva"~ "larva",
      .default = as.character(life_stage_taxon_specific))) %>%   # add taxon specific life stage for the relevant traits
  dplyr::mutate(
    sex = case_when(
      trait_name == "wing_development_female" ~ "female",
      trait_name == "wing_development_male"~ "male",
      .default = as.character(sex))) %>%   # add sex for the wing_development
    dplyr::mutate(
    trait_name = case_when(
      trait_name == "functional_role_adult" ~ "functional_role",
      trait_name == "functional_role_larva" ~ "functional_role",
      trait_name == "microhabitat_activity_adult"~ "microhabitat_activity",
      trait_name == "microhabitat_activity_larva"~ "microhabitat_activity",
      trait_name == "wing_development_female"~ "wing_development",
      trait_name == "wing_development_male"~ "wing_development",
      .default = as.character(trait_name))) %>% # change trait_names to match IA traits now that life stage has been specified
  dplyr::mutate(entity_type = "metapopulation") %>%  # add entity_type category
  dplyr::mutate(
    measurement_remarks = case_when(
      trait_name == "microhabitat_activity" ~ NA,
      trait_name == "wing_development" ~ NA,
      measurement_remarks == "adult flowers/larvae humus or root feeders" & life_stage_generic == "adult" ~ "feed on flowers",
      measurement_remarks == "adult flowers/larvae humus or root feeders" & life_stage_generic == "juvenile" ~ "feed on humus and roots",
      measurement_remarks == "adult foliage Eucalypts/ larvae humus or grass roots" & life_stage_generic == "adult" ~ "feed on Eucalyptus foliage",
      measurement_remarks == "adult foliage Eucalypts/ larvae humus or grass roots" & life_stage_generic == "juvenile" ~ "feed on humus and grass roots",
      measurement_remarks == "adults flowers/foliage Eucalypts/larvae humus or roots" & life_stage_generic == "adult" ~ "feed on Eucalyptus foliage and flowers",
      measurement_remarks == "adults flowers/foliage Eucalypts/larvae humus or roots" & life_stage_generic == "juvenile" ~ "feed on humus and roots",
      measurement_remarks == "adults flowers/larvae rotten wood" & life_stage_generic == "adult" ~ "feed on flowers",
      measurement_remarks == "adults flowers/larvae rotten wood" & life_stage_generic == "juvenile" ~ "feed on rotten wood",
      measurement_remarks == "adults foliage Eucalypts/larvae humus or roots" & life_stage_generic == "adult" ~ "feed on Eucalyptus foliage",
      measurement_remarks == "adults foliage Eucalypts/larvae humus or roots" & life_stage_generic == "juvenile" ~ "feed on humus and roots",
      measurement_remarks == "cow/horse/kangaroo dung" ~ "feed on cow/horse/kangaroo dung",
      measurement_remarks == "dead animals" ~ NA,
      measurement_remarks == "dead plant material" ~ NA,
      measurement_remarks == "dead plant material/roots" & life_stage_generic == "juvenile" ~ NA,
      measurement_remarks == "dead plant material/roots" & life_stage_generic == "adult" ~ "feed on plant material and roots",
      measurement_remarks == "dung" ~ NA,
      measurement_remarks == "fungal fruiting bodies" ~ "feed on fungal fruiting bodies",
      measurement_remarks == "humus/fungus" & life_stage_generic == "juvenile" ~ NA,
      measurement_remarks == "humus/fungus" & life_stage_generic == "adult" ~ "feed on humus and fungus",
      measurement_remarks == "larvae feed on termites" & life_stage_generic == "juvenile" ~ "feed on termites",
      measurement_remarks == "larvae feed on termites" & life_stage_generic == "adult" ~ NA,
      measurement_remarks == "larvae humus or root feeder" & life_stage_generic == "juvenile" ~ "feed on humus and roots",
      measurement_remarks == "larvae humus or root feeder" & life_stage_generic == "adult" ~ NA,
      measurement_remarks == "larvae humus or roots" & life_stage_generic == "juvenile" ~ "feed on humus and roots",
      measurement_remarks == "larvae humus or roots" & life_stage_generic == "adult" ~ NA,
      measurement_remarks == "larvae humus/plant root feeder" & life_stage_generic == "juvenile" ~ "feed on humus and roots",
      measurement_remarks == "larvae humus/plant root feeder" & life_stage_generic == "adult" ~ NA,
      measurement_remarks == "larvae plant roots etc" & life_stage_generic == "juvenile" ~ "feed on roots",
      measurement_remarks == "larvae plant roots etc" & life_stage_generic == "adult" ~ NA,
      measurement_remarks == "larvae prey on ants" & life_stage_generic == "juvenile" ~ "feed on ants",
      measurement_remarks == "larvae prey on ants" & life_stage_generic == "adult" ~ NA,
      measurement_remarks == "larvaeplant roots etc" & life_stage_generic == "juvenile" ~ "feed on roots",
      measurement_remarks == "larvaeplant roots etc" & life_stage_generic == "adult" ~ NA,
      measurement_remarks == "plant material/insect larvae" & life_stage_generic == "juvenile" ~ NA,
      measurement_remarks == "plant material/insect larvae" & life_stage_generic == "adult" ~ "feed on plant material and insect larvae",
      measurement_remarks == "small frogs?" & life_stage_generic == "juvenile" ~ NA,
      measurement_remarks == "small frogs?" & life_stage_generic == "adult" ~ "feed on small frogs",
      .default = as.character(measurement_remarks))) %>%  # specify measurement_remarks for functional_role and microhabitat_activity
  dplyr::mutate(methods = "We sampled 10 sites in each of the three locations. We sampled 
                each site with 16 pitfall traps (20 L, 28 cm in diameter). Each trap had 
                a separate 10 m long drift fence positioned across its centre. We spaced 
                traps at 25 m intervals and opened them for five consecutive 24 hour 
                periods in October, November, and December 1999, and in January 2000. 
                These months include late spring and summer, when animals are expected to 
                be most active. We emptied traps daily.") %>%  # add methods for all traits
  dplyr::mutate(site_name = "Pulletop, Gubbata, and Taleeban, NSW") %>%  # add site name
  dplyr::mutate(site_date_of_visit = "1999-10/2000-01") %>%  # add study date
  dplyr::mutate(site_description = "Pulletop, Gubbata, and Taleeban in south-central New 
                South Wales. Each location was 100 km2 in size and in mallee habitat. We 
                recognized six landscape elements: reserve, strip, grazed strip, road, 
                woodland, and paddock. The four landscape elements with mallee (reserve, 
                strip, grazed strip, and road) were stratified by the presence or absence 
                of spinifex (Triodia scariosa), a spiny-leaved clumping grass, in the 
                understory. Spinifex was absent from sites that were slightly lower 
                in the landscape, where the soil had higher clay content.") %>%  # add site description
  dplyr::mutate(associated_plant_taxa = case_when(
    measurement_remarks == "feed on Eucalyptus foliage" ~ "Eucalyptus",
    measurement_remarks == "feed on Eucalyptus foliage or flowers" ~ "Eucalyptus",
    .default = as.character(associated_plant_taxa))) %>% # add associated plant taxa from information in measurement_remarks
  dplyr::mutate(plant_relation_description = case_when(
    measurement_remarks == "feed on Eucalyptus foliage" ~ "The invertebrate is a folivore (herbivore) of the associated plants",
    measurement_remarks == "feed on Eucalyptus foliage or flowers" ~ "The invertebrate is a folivore (herbivore) of the associated plants",
    .default = as.character(plant_relation_description))) %>% # add associated plant relationship description from information in measurement_remarks
  dplyr::mutate(associated_fauna_taxa = case_when(
    measurement_remarks == "feed on termites" ~ "Termitoidae",
    measurement_remarks == "feed on ants" ~ "Formicidae",
    .default = as.character(associated_fauna_taxa))) %>% # add associated fauna taxa with information from "food" column
  dplyr::mutate(fauna_relation_description = case_when(
    measurement_remarks == "feed on termites" ~ "The invertebrate is a predator of the associated fauna",
    measurement_remarks == "feed on ants" ~ "The invertebrate is a predator of the associated fauna",
    .default = as.character(fauna_relation_description))) %>% # add associated fauna taxa with information from "food" column
  dplyr::mutate(source_key = "Driscoll_2005") %>%  # add source_key
  dplyr::mutate(source_doi = "doi: 10.1111/j.1523-1739.2005.00586.x") %>%  # add source_doi
  dplyr::mutate(source_citation = "Driscoll, D. A., & Weir, T. O. M. (2005). Beetle responses to habitat fragmentation depend on ecological traits, habitat condition, and remnant size. Conservation Biology, 19(1), 182-194.") %>%  # add source_citation
  dplyr::mutate(source_type = "article") %>% # add source_type
  dplyr::bind_rows(mallee_paper) %>% # add data extracted from reading paper
  dplyr::filter(if_any(everything(), ~ !is.na(.))) %>%  # remove rows with all NAs (from mallee_paper)
  readr::write_csv(file.path(out_dir, "data.csv")) # save final dataset


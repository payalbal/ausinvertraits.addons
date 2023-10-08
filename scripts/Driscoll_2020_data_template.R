##-----------------------##
## Driscoll_2020 dataset ##
##-----------------------##


## Script to clean and map the Driscoll South Australian Eyre Peninsula beetle 
## dataset to the InverTraits data_template and create a data.csv for the database.


##-------------------------##
#### The start up basics ####
##-------------------------##


## Set working directory
getwd()
setwd(...) # set this to R project directory if needed

out_dir <- file.path(getwd(), "outputs/other_dbs/Driscoll_2020")

## Load required package
library(tidyverse)

## Load the Driscoll_2020 dataset
d1 <- read_csv(file.path(getwd(), "data", "Driscoll_2020/Eyre_Peninsula_SA_beetles.csv"), show_col_types = FALSE)

## Load the data template
template <- read_csv(file.path(getwd(), "data", "Driscoll_2020/data_template.csv"), show_col_types = FALSE) 


## QUESTIONS FOR DON:
## ## How can we interpret those beetles that are flightless and have large wings?
## ## If a beetle is listed as a "scavenger" but there is no value in the food column that is "dead plant material"; are they scavenging animal matter, i.e., necrophage?

## NOTES:
## Other traits in the dataset to keep in mind: pronotum width, antennae length, total number in study


##-------------------------------------------------------------##
#### Modify the dataset in preparation for the data template ####
##-------------------------------------------------------------##


## (1) Modify the dataset in preparation for the data template.

d1_mod <- d1 %>%
  dplyr::select(species, taxon_name_original, taxname_source, family, size, bodylength_mm, 
                flightless, trophicgroup, above_on_below, trophicgroupdetail, food, 
                usual_habitat_adult, usual_habitat_larvae, introduced, Wing_Type,
                Feeding_Group, pronotum_width) %>% # select and reorder columns
  dplyr::rename(
    taxon_name = species,
    taxon_family = family,
    body_length = bodylength_mm,
    wing_development = flightless,
    functional_role_adult = trophicgroupdetail) %>% # change column names to match data_template column names
  dplyr::mutate(functional_role_larva = NA)  %>% # create new functional_role columns for larva life stage
  dplyr::select(taxon_name, taxon_name_original, taxname_source, taxon_family, body_length, 
                wing_development, Wing_Type, trophicgroup, functional_role_larva, functional_role_adult, 
                Feeding_Group, food, above_on_below, usual_habitat_adult, usual_habitat_larvae) %>% # keep only needed columns for now
  dplyr::mutate(
    wing_development = case_when(
      wing_development == "yes" ~ "wingless",
      wing_development == "no" ~ "winged")) %>% # change flightless category to "wingless", and flight category to "winged"
  dplyr::mutate(wing_development = case_when(
    wing_development == "wingless" & Wing_Type == "present (large)" ~ "rudimentary",
    .default = as.character(wing_development))) %>% # change wing_development category to rudimentary for flightless beetles with large wings
  dplyr::mutate(functional_role_adult = case_when(
    functional_role_adult == "adult carnivore/larva herbivore" ~ "predator",
    functional_role_adult == "carnivore" ~ "predator",
    functional_role_adult == "omnivore(granivore)" ~ "omnivore granivore",
    functional_role_adult == "saproxylic" ~ "xylophage",
    food == "dead plant material" ~ "detritivore",
    food == "dead plant material/roots" ~ "detritivore",
    Feeding_Group == "?pollinator" ~ "herbivore pollinator",
    Feeding_Group == "pollinator" ~ "herbivore pollinator",
    functional_role_adult == "scavenger" ~ "necrophage",
    .default = as.character(functional_role_adult))) %>% # specify adult functional_role trait levels given information in other columns
  dplyr::mutate(functional_role_larva = case_when(
    functional_role_adult == "adult carnivore/larva herbivore" ~ "herbivore",
    food == "adults flowers/foliage Eucalypts/larvae humus or roots" ~ "detritivore herbivore",
    food == "adult flowers/larvae humus or root feeders" ~ "detritivore herbivore",
    food == "adults flowers/larvae rotten wood" ~ "xylophage",
    food == "adults foliage Eucalypts/larvae humus or roots" ~ "detritivore herbivore",
    food == "larvae feed on termites" ~ "predator",
    food == "larvae humus or roots" ~ "detritivore herbivore",
    food == "larvae in dead wood" ~ "xylophage",
    food == "larvae plant roots etc" ~ "herbivore",
    food == "larvae prey on ants" ~ "predator",
    food == "sap pollen nectar, larvae eat wood" ~ "xylophage",
    .default = as.character(functional_role_larva))) %>% # specify larva functional_role trait levels given information in other columns
  dplyr::mutate(usual_habitat_adult = case_when(
    food == "under bark for WA species, maybe in grass trees and grass tussocks, http://www.archive.org/stream/proceedingsoflin46linn/proceedingsoflin46linn_djvu.txt" ~ "bark vegetation_understorey",
    usual_habitat_adult == "arboreal/lives under bark" ~ "bark",
    usual_habitat_adult == "canopy" ~ "arboreal_canopy",
    usual_habitat_adult == "carcasses/skins/dung" ~ "animal_carcass dung",
    usual_habitat_adult == "concealed places/nocturnal" ~ "bark rock",
    usual_habitat_adult == "fossorial" ~ "burrow",
    usual_habitat_adult == "fossorial/burrowing/nocturnal" ~ "burrow",
    usual_habitat_adult == "fossorial/nocturnal" ~ "burrow",
    usual_habitat_adult == "fossorial/nocturnal/sandy soil" ~ "burrow",
    usual_habitat_adult == "ground" ~ "ground_open",
    usual_habitat_adult == "hygrophilus" ~ "riparian",
    usual_habitat_adult == "in dung" ~ "dung",
    usual_habitat_adult == "litter" ~ "litter_ground",
    usual_habitat_adult == "nocturnal" ~ "bark rock",
    usual_habitat_adult == "nomadic/nocturnal/ground" ~ "ground_open",
    usual_habitat_adult == "subterranean and borrowing/nocturnal" ~ "burrow",
    usual_habitat_adult == "subterranean and burrowing" ~ "burrow",
    usual_habitat_adult == "under bark" ~ "bark",
    usual_habitat_adult == "under bark/logs" ~ "bark under_dead_wood_ground",
    usual_habitat_adult == "under bark" ~ "bark",
    food == "adult flowers/larvae humus or root feeders" ~ "vegetation",
    food == "adults flowers/foliage Eucalypts/larvae humus or roots" ~ "vegetation arboreal_trees",
    food == "adults flowers/larvae rotten wood" ~ "vegetation",
    food == "adults foliage Eucalypts/larvae humus or roots" ~ "arboreal_trees",
    food == "adults flowers/larvae rotten wood" ~ "vegetation",
    food == "flowers" ~ "vegetation",
    food == "sap pollen nectar, larvae eat wood" ~ "vegetation",
    functional_role_adult == "herbivore" ~ "vegetation",
    functional_role_adult == "herbivore pollinator" ~ "vegetation",
    functional_role_adult == "xylophage" ~ "in_dead_wood_ground in_standing_wood_dead",
    .default = as.character(usual_habitat_adult))) %>% # specify adult microhabitat_activity given information in other columns
  dplyr::mutate(usual_habitat_larvae = case_when(
    food == "larvae in dead wood" ~ "in_dead_wood_ground in_standing_wood_dead",
    usual_habitat_larvae == "in burrows in ground" ~ "burrow",
    usual_habitat_larvae == "in rotten wood" ~ "in_dead_wood_ground in_standing_wood_dead",
    usual_habitat_larvae == "in soil" ~ "soil",
    usual_habitat_larvae == "in soil or decaying logs" ~ "soil in_dead_wood_ground in_standing_wood_dead",
    usual_habitat_larvae == "in termite mounds or logs" ~ "mound_above_ground in_dead_wood_ground in_standing_wood_dead",
    usual_habitat_larvae == "live with ants ?" ~ "burrow",
    usual_habitat_larvae == "subterranean" ~ "soil",
    usual_habitat_larvae == "subterranean ?" ~ "soil",
    usual_habitat_larvae == "subterranean and burrowing" ~ "soil burrow",
    usual_habitat_larvae == "subterranean and borrowing" ~ "soil burrow",
    usual_habitat_larvae == "subterranean" ~ "soil",
    food == "sap pollen nectar, larvae eat wood" ~ "in_dead_wood_ground in_standing_wood_dead",
    .default = as.character(usual_habitat_larvae))) %>% # specify larva microhabitat_activity given information in other columns
  dplyr::mutate(associated_fauna_taxa = NA)  %>% # create column for associated_fauna_taxa
  dplyr::mutate(fauna_relation_description = NA)  %>% # create column for fauna_relation_description
  dplyr::mutate(associated_fauna_taxa = case_when(
    food == "larvae feed on termites" ~ "Termitoidae",
    food == "larvae prey on ants" ~ "Formicidae",
    food == "Scaritini" ~ "Scaritini",
    .default = as.character(associated_fauna_taxa))) %>% # add associated fauna taxa with information from "food" column
  dplyr::mutate(fauna_relation_description = case_when(
    food == "larvae feed on termites" ~ "The invertebrate is a predator of the associated fauna",
    food == "larvae prey on ants" ~ "The invertebrate is a predator of the associated fauna",
    food == "Scaritini" ~ "The invertebrate is a predator of the associated fauna",
    .default = as.character(fauna_relation_description))) %>% # add associated fauna taxa with information from "food" column
  dplyr::rename(measurement_remarks = food) %>% # rename column that has measurement remarks
  dplyr::select(taxon_name, taxon_name_original, taxname_source, taxon_family, measurement_remarks,
                associated_fauna_taxa, fauna_relation_description, body_length, wing_development, functional_role_larva,
                functional_role_adult, usual_habitat_adult, usual_habitat_larvae) %>% # keep and reorder only columns needed
  dplyr::mutate(body_length = as.character(body_length))  %>% # set numeric column to character so that all trait values can be combined into one column
  tidyr::pivot_longer(cols = body_length:usual_habitat_larvae,
                      names_to = "trait_name",
                      values_to = "value") %>%  # convert to long format so that all trait names and values are in two columns
  tidyr::drop_na(value) # remove rows with NA in the value column
  

##-------------------------------------------------------------------------##
#### Merge the dataset with the IA data template, & occupy missing cells ####
##-------------------------------------------------------------------------##

d1_template <- template %>%
  dplyr::full_join(d1_mod) %>%  # merge with empty data_template
  dplyr::filter(if_any(everything(), ~ !is.na(.))) %>%  # remove rows with all NAs (from data template)
  dplyr::mutate(entity_type_tax = "species") %>%  # add entity_type_tax category
  dplyr::mutate(
    life_stage_generic = case_when(
      trait_name == "body_length" ~ "adult",
      trait_name == "functional_role_adult" ~ "adult",
      trait_name == "functional_role_larva" ~ "juvenile",
      trait_name == "usual_habitat_adult"~ "adult",
      trait_name == "usual_habitat_larvae"~ "juvenile",
      trait_name == "wing_development" ~ "adult")) %>%  # add life stage for the traits
  dplyr::mutate(
    life_stage_taxon_specific = case_when(
      trait_name == "functional_role_larva" ~ "larva",
      trait_name == "usual_habitat_larvae"~ "larva",
      .default = as.character(life_stage_taxon_specific))) %>%   # add taxon specific life stage for the relevant traits
  dplyr::mutate(
    trait_name = case_when(
      trait_name == "functional_role_adult" ~ "functional_role",
      trait_name == "functional_role_larva" ~ "functional_role",
      trait_name == "usual_habitat_adult"~ "microhabitat_activity",
      trait_name == "usual_habitat_larvae"~ "microhabitat_activity",
      .default = as.character(trait_name))) %>% # change trait_names to match IA traits now that life stage has been specified
  dplyr::mutate(entity_type = "metapopulation") %>%  # add entity_type category
  dplyr::mutate(value_type = case_when(trait_name == "body_length" ~ "mean", .default = as.character(value_type))) %>%  # add value_type for the one numerical trait
  dplyr::mutate(unit_numeric = case_when(trait_name == "body_length" ~ "mm", .default = as.character(unit_numeric))) %>%  # add unit for the one numerical trait
  dplyr::mutate(
    measurement_remarks = case_when(
      measurement_remarks == "under bark for WA species, maybe in grass trees and grass tussocks, http://www.archive.org/stream/proceedingsoflin46linn/proceedingsoflin46linn_djvu.txt" & trait_name == "microhabitat_activity" ~ "under bark for WA species, maybe in grass trees and grass tussocks",
      trait_name == "body_length" ~ NA,
      trait_name == "microhabitat_activity" ~ NA,
      trait_name == "wing_development" ~ NA,
      measurement_remarks == "dead animals" ~ NA,
      measurement_remarks == "dead plant material" ~ NA,
      measurement_remarks == "dung" ~ NA,
      measurement_remarks == "dead plant material/roots" ~ "feed on dead plant material and plant roots",
      measurement_remarks == "dead wood" ~ "feed on dead wood",
      measurement_remarks == "flowers" ~ "feed on flowers",
      measurement_remarks == "nectar/pollen" ~ "feed on nectar and pollen",
      measurement_remarks == "plant material/insect larvae" ~ "feed on plant material and insect larvae",
      measurement_remarks == "Scaritini" ~ "prey on Scaritini",
      measurement_remarks == "adult flowers/larvae humus or root feeders" & life_stage_generic == "adult" ~ "feed on flowers",
      measurement_remarks == "adult flowers/larvae humus or root feeders" & life_stage_generic == "juvenile" ~ "feed on humus and plant roots",
      measurement_remarks == "adults flowers/foliage Eucalypts/larvae humus or roots" & life_stage_generic == "adult" ~ "feed on flowers and Eucalyptus foliage",
      measurement_remarks == "adults flowers/foliage Eucalypts/larvae humus or roots" & life_stage_generic == "juvenile" ~ "feed on humus and plant roots",
      measurement_remarks == "adults flowers/larvae rotten wood" & life_stage_generic == "adult" ~ "feed on flowers",
      measurement_remarks == "adults flowers/larvae rotten wood" & life_stage_generic == "juvenile" ~ "feed on rotten wood",
      measurement_remarks == "adults foliage Eucalypts/larvae humus or roots" & life_stage_generic == "adult" ~ "feed on Eucalyptus foliage",
      measurement_remarks == "adults foliage Eucalypts/larvae humus or roots" & life_stage_generic == "juvenile" ~ "feed on humus and plant roots",
      measurement_remarks == "larvae feed on termites" & life_stage_generic == "juvenile" ~ "feed on termites",
      measurement_remarks == "larvae feed on termites" & life_stage_generic == "adult" ~ NA,
      measurement_remarks == "larvae humus or roots" & life_stage_generic == "juvenile" ~ "feed on humus or plant roots",
      measurement_remarks == "larvae humus or roots" & life_stage_generic == "adult" ~ NA,
      measurement_remarks == "larvae plant roots etc" & life_stage_generic == "juvenile" ~ "feed on plant roots",
      measurement_remarks == "larvae plant roots etc" & life_stage_generic == "adult" ~ NA,
      measurement_remarks == "larvae prey on ants" & life_stage_generic == "juvenile" ~ "prey on ants",
      measurement_remarks == "larvae prey on ants" & life_stage_generic == "adult" ~ NA,
      measurement_remarks == "sap pollen nectar, larvae eat wood" & life_stage_generic == "juvenile" ~ "feed on wood",
      measurement_remarks == "sap pollen nectar, larvae eat wood" & life_stage_generic == "adult" ~ "feed on sap, pollen, and nectar",
      measurement_remarks == "under bark for WA species, maybe in grass trees and grass tussocks, http://www.archive.org/stream/proceedingsoflin46linn/proceedingsoflin46linn_djvu.txt"~ NA,
      .default = as.character(measurement_remarks))) %>%  # specify measurement_remarks for functional_role and microhabitat_activity
  dplyr::mutate(
    methods = case_when(
      trait_name == "body_length" ~ "We sampled beetles from 23 transect and seven grid sites across four conservation reserves. Transect sites consisted of 11 pairs of 20 litre pitfall traps, each pair connected by a 20 m drift fence. Trap pairs were spaced along the 400 m transect at 40 m intervals. Grid sites included a 5 × 10 arrangement of individual 20 litre pitfall traps, each with a 10 m drift fence, with traps spaced at 25 m intervals. The grids and transects were placed in 15 areas that were burnt by different fires in five different locations. We surveyed beetles over four consecutive summers from December 2004-February 2005 (referred to as 2004) to December 2007-February 2008 (referred to as 2007). In each summer, we conducted three six-night sampling periods approximately monthly, except in the second summer when we sampled in two periods (December and February). Sites were sampled for an average of 15.4 (SD = 4.0) nights per year and we accommodated unequal sampling in the analyses. Data from February 2006 at Pinkawillinie were excluded from year 2 due to a fire in December 2005 which changed the time since fire during the sampling year. Post-fire data from Hincks in December 2006 were excluded because inflated capture rates immediately after fire give a misleading impression of high abundance. Beetles were identified to species or morphospecies level using a photographic guide to common beetles that we prepared based on initial trapping, alongside a field-box of pinned specimens. Species that were unambiguously identified were marked with a paint spot on the ventral surface and released 5-10 m from the point of capture. Recaptured animals were excluded from the data. Individuals that could not be identified in the field were assigned a morphospecies name, and were preserved for later identification at the South Australian Museum (Eric Matthews) or CSIRO Entomology (Tom Weir, Rolf Oberprieler). Beetles < 6mm long could not be reliably collected from 20 litre pitfall traps in a time-efficient manner and were excluded from analysis, and we excluded species with fewer than five records as they were inadequate for analysis. Beetle sizes were based on measurement of one to five pinned specimens from our study.",
      trait_name == "functional_role" ~ "We sampled beetles from 23 transect and seven grid sites across four conservation reserves. Transect sites consisted of 11 pairs of 20 litre pitfall traps, each pair connected by a 20 m drift fence. Trap pairs were spaced along the 400 m transect at 40 m intervals. Grid sites included a 5 × 10 arrangement of individual 20 litre pitfall traps, each with a 10 m drift fence, with traps spaced at 25 m intervals. The grids and transects were placed in 15 areas that were burnt by different fires in five different locations. We surveyed beetles over four consecutive summers from December 2004-February 2005 (referred to as 2004) to December 2007-February 2008 (referred to as 2007). In each summer, we conducted three six-night sampling periods approximately monthly, except in the second summer when we sampled in two periods (December and February). Sites were sampled for an average of 15.4 (SD = 4.0) nights per year and we accommodated unequal sampling in the analyses. Data from February 2006 at Pinkawillinie were excluded from year 2 due to a fire in December 2005 which changed the time since fire during the sampling year. Post-fire data from Hincks in December 2006 were excluded because inflated capture rates immediately after fire give a misleading impression of high abundance. Beetles were identified to species or morphospecies level using a photographic guide to common beetles that we prepared based on initial trapping, alongside a field-box of pinned specimens. Species that were unambiguously identified were marked with a paint spot on the ventral surface and released 5-10 m from the point of capture. Recaptured animals were excluded from the data. Individuals that could not be identified in the field were assigned a morphospecies name, and were preserved for later identification at the South Australian Museum (Eric Matthews) or CSIRO Entomology (Tom Weir, Rolf Oberprieler). Beetles < 6mm long could not be reliably collected from 20 litre pitfall traps in a time-efficient manner and were excluded from analysis, and we excluded species with fewer than five records as they were inadequate for analysis. Trophic group was allocated based on expert knowledge (Tom Weir, CSIRO) of the family, or, where there were subfamily differences, the tribe level of classification was used.",
      trait_name == "microhabitat_activity" ~ "We sampled beetles from 23 transect and seven grid sites across four conservation reserves. Transect sites consisted of 11 pairs of 20 litre pitfall traps, each pair connected by a 20 m drift fence. Trap pairs were spaced along the 400 m transect at 40 m intervals. Grid sites included a 5 × 10 arrangement of individual 20 litre pitfall traps, each with a 10 m drift fence, with traps spaced at 25 m intervals. The grids and transects were placed in 15 areas that were burnt by different fires in five different locations. We surveyed beetles over four consecutive summers from December 2004-February 2005 (referred to as 2004) to December 2007-February 2008 (referred to as 2007). In each summer, we conducted three six-night sampling periods approximately monthly, except in the second summer when we sampled in two periods (December and February). Sites were sampled for an average of 15.4 (SD = 4.0) nights per year and we accommodated unequal sampling in the analyses. Data from February 2006 at Pinkawillinie were excluded from year 2 due to a fire in December 2005 which changed the time since fire during the sampling year. Post-fire data from Hincks in December 2006 were excluded because inflated capture rates immediately after fire give a misleading impression of high abundance. Beetles were identified to species or morphospecies level using a photographic guide to common beetles that we prepared based on initial trapping, alongside a field-box of pinned specimens. Species that were unambiguously identified were marked with a paint spot on the ventral surface and released 5-10 m from the point of capture. Recaptured animals were excluded from the data. Individuals that could not be identified in the field were assigned a morphospecies name, and were preserved for later identification at the South Australian Museum (Eric Matthews) or CSIRO Entomology (Tom Weir, Rolf Oberprieler). Beetles < 6mm long could not be reliably collected from 20 litre pitfall traps in a time-efficient manner and were excluded from analysis, and we excluded species with fewer than five records as they were inadequate for analysis. Microhabitat position was based on Tom Weir’s (CSIRO) expert opinion based on a lifetime studying beetles.",
      trait_name == "wing_development" ~ "We sampled beetles from 23 transect and seven grid sites across four conservation reserves. Transect sites consisted of 11 pairs of 20 litre pitfall traps, each pair connected by a 20 m drift fence. Trap pairs were spaced along the 400 m transect at 40 m intervals. Grid sites included a 5 × 10 arrangement of individual 20 litre pitfall traps, each with a 10 m drift fence, with traps spaced at 25 m intervals. The grids and transects were placed in 15 areas that were burnt by different fires in five different locations. We surveyed beetles over four consecutive summers from December 2004-February 2005 (referred to as 2004) to December 2007-February 2008 (referred to as 2007). In each summer, we conducted three six-night sampling periods approximately monthly, except in the second summer when we sampled in two periods (December and February). Sites were sampled for an average of 15.4 (SD = 4.0) nights per year and we accommodated unequal sampling in the analyses. Data from February 2006 at Pinkawillinie were excluded from year 2 due to a fire in December 2005 which changed the time since fire during the sampling year. Post-fire data from Hincks in December 2006 were excluded because inflated capture rates immediately after fire give a misleading impression of high abundance. Beetles were identified to species or morphospecies level using a photographic guide to common beetles that we prepared based on initial trapping, alongside a field-box of pinned specimens. Species that were unambiguously identified were marked with a paint spot on the ventral surface and released 5-10 m from the point of capture. Recaptured animals were excluded from the data. Individuals that could not be identified in the field were assigned a morphospecies name, and were preserved for later identification at the South Australian Museum (Eric Matthews) or CSIRO Entomology (Tom Weir, Rolf Oberprieler). Beetles < 6mm long could not be reliably collected from 20 litre pitfall traps in a time-efficient manner and were excluded from analysis, and we excluded species with fewer than five records as they were inadequate for analysis.",
      )) %>% # add methods for the traits
  dplyr::mutate(site_name = "Eyre Peninsula, South Australia") %>%  # add site name
  dplyr::mutate(site_date_of_visit = "2004-12/2008-02") %>%  # add study date
  dplyr::mutate(site_description = "The study region consists of mallee woodland dominated by multistemmed Eucalyptus species and an understorey of shrubs and spinifex (Triodia irritans). Parabolic and longitudinal sand dunes overlie a limestone-calcrete base across a relatively flat landscape. Annual rainfall is within the range 300-400 mm.") %>%  # add site description
  dplyr::mutate(site_fire_impact = "Fire-prone woodlands. Sites varied in time since fire, from 39 years ago to 0 years (sites burned during the study). The grids and transects were placed in 15 areas that were burnt by different fires in five different locations. Six transects were burnt during the study and therefore had two different times since fire in the data. Five of the grids straddled the edge of a burn for part or all of the study.") %>%  # add fire impact description
  dplyr::mutate(associated_plant_taxa = case_when(
    measurement_remarks == "feed on flowers or Eucalyptus foliage" ~ "Eucalyptus",
    measurement_remarks == "feed on Eucalyptus foliage" ~ "Eucalyptus",
    .default = as.character(associated_plant_taxa))) %>% # add associated plant taxa from information in measurement_remarks
  dplyr::mutate(plant_relation_description = case_when(
    measurement_remarks == "feed on flowers or Eucalyptus foliage" ~ "The invertebrate is a folivore (herbivore) of the associated plants",
    measurement_remarks == "feed on Eucalyptus foliage" ~ "The invertebrate is a folivore (herbivore) of the associated plants",
    .default = as.character(plant_relation_description))) %>% # add associated plant relationship description from information in measurement_remarks
  dplyr::mutate(source_key = "Driscoll_2020") %>%  # add source_key
  dplyr::mutate(source_doi = "doi: 10.1111/een.12798") %>%  # add source_doi
  dplyr::mutate(source_citation = "Driscoll, D. A., Smith, A. L., Blight, S., & Sellar, I. (2020). Interactions among body size, trophic level, and dispersal traits predict beetle detectability and occurrence responses to fire. Ecological Entomology, 45(2), 300-310.") %>%  # add source_citation
  dplyr::mutate(source_type = "article") %>% # add source_type
  readr::write_csv(file.path(out_dir, "data.csv")) # save final dataset


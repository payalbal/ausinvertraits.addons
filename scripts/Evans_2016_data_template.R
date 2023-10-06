##------------##
## Evans_2016 ##
##------------##

## Script to clean and map the Evans_2016 dataset to the InverTraits
## data_template and create a data.csv for the database.


##-------------------------##
#### The start up basics ####
##-------------------------##

## Set working directory
getwd()
setwd(...) # set this to R project directory if needed

out_dir <- file.path(getwd(), "outputs/other_dbs/Evans_2016")

## Load required package
library(tidyverse)

## Load the Evans_2016 dataset
evans <- read_csv(file.path(getwd(), "data", "Evans_2016/Evans_2016_beetles_Wog_Wog_John_Evans.csv"), show_col_types = FALSE)

## Load the data template
template <- read_csv(file.path(getwd(), "data", "Evans_2016/data_template.csv"), show_col_types = FALSE) 

## QUESTIONS FOR DON:
## Flightless 1 = yes?, 0 = no?
## Do feeding guilds refer to adult life stage?
## What is the microhabotat difference between saproxylic and xylophagous beetles? Do xylophages also feed on alive wood?
## Are body lengths ameans?


##-------------------------------------------------------------##
#### Modify the dataset in preparation for the data template ####
##-------------------------------------------------------------##

evans_mod <- evans %>%
  dplyr::rename(
    taxon_family = Family,
    body_length = Size_mm,
    wing_development = Flightless,
    functional_role = Feeding) %>% # change column names to match data_template column names
  dplyr::mutate(across(taxon_family:Tribe, str_to_title)) %>% # change values to title case
  dplyr::mutate(
    wing_development = case_when(
      wing_development == 0 ~ "winged",
      wing_development == 1 ~ "wingless",
      .default = as.character(wing_development))) %>% # modify trait categories to match IA
  dplyr::mutate(
    functional_role = case_when(
      functional_role == "MYC" ~ "fungivore",
      functional_role == "MYC?" ~ "fungivore",
      functional_role == "MYR" ~ "myrmecophile",
      functional_role == "PHY" ~ "herbivore",
      functional_role == "PHY, PRE" ~ "herbivore predator",
      functional_role == "PHY/PRE" ~ "herbivore predator",
      functional_role == "PRE" ~ "predator",
      functional_role == "PRE, PHY" ~ "herbivore predator",
      functional_role == "PRE?" ~ "predator",
      functional_role == "SAP" ~ "saproxylic",
      functional_role == "XYL" ~ "xylophage",
      functional_role == "XYL-MYC" ~ "xylophage fungivore")) %>% # modify trait categories to match IA
  dplyr::mutate(microhabitat_activity = NA) %>% # create new column for microhabitat_activity
  dplyr::mutate(
    microhabitat_activity = case_when(
      functional_role == "fungivore" ~ "fungus",
      functional_role == "myrmecophile" ~ "myrmecophile",
      functional_role == "herbivore" ~ "vegetation",
      functional_role == "herbivore predator" ~ "vegetation",
      functional_role == "saproxylic" ~ "in_dead_wood_ground in_standing_wood_dead",
      functional_role == "xylophage" ~ "in_dead_wood_ground in_standing_wood_dead in_standing_wood_alive",
      functional_role == "xylophage fungivore" ~ "in_dead_wood_ground in_standing_wood_dead in_standing_wood_alive fungus",
       .default = as.character(microhabitat_activity))) %>% # add microhabitat_activity trait value given the functional roles
  dplyr::mutate(ecological_dependency = NA) %>% # create new column for ecological_dependency
  dplyr::mutate(
    ecological_dependency = case_when(
      functional_role == "myrmecophile" ~ "specialist",
      .default = as.character(ecological_dependency))) %>% # add ecological_dependency trait value for myrmecophiles
  dplyr::mutate(
    functional_role = case_when(
      functional_role == "saproxylic" ~ "xylophage",
      functional_role == "myrmecophile" ~ NA,
      .default = as.character(functional_role))) %>% # modify function roles that aren't standard for IA
  dplyr::select(taxon_name, taxon_name_original, taxname_source, taxon_family, 
                body_length, wing_development, functional_role, 
                microhabitat_activity, ecological_dependency) %>% # keep only needed columns
  dplyr::mutate(body_length = as.character(body_length))  %>% # set numeric column to character so that all trait values can be combined into one column
  tidyr::pivot_longer(cols = body_length:ecological_dependency,
                      names_to = "trait_name",
                      values_to = "value") %>%  # convert to long format so that all trait names and values are in two columns
  tidyr::drop_na(value) # remove rows with NA in the value column


##-------------------------------------------------------------------------##
#### Merge the dataset with the IA data template, & occupy missing cells ####
##-------------------------------------------------------------------------##

evans_template <- template %>%
  dplyr::full_join(evans_mod) %>%  # merge the modified dataset with empty data_template
  dplyr::filter(if_any(everything(), ~ !is.na(.))) %>%  # remove rows with all NAs (from data template)
  dplyr::mutate(entity_type_tax = "species") %>%  # add entity_type_tax category for all traits
  dplyr::mutate(life_stage_generic = "adult") %>%  # add life stage for all traits
  dplyr::mutate(entity_type = "metapopulation") %>%  # add entity_type category for all traits
  dplyr::mutate(
    value_type = case_when(
      trait_name == "body_length" ~ "mean", 
      .default = as.character(value_type))) %>%  # add value_type for the one numerical trait
  dplyr::mutate(
    unit_numeric = case_when(
      trait_name == "body_length" ~ "mm", 
      .default = as.character(unit_numeric))) %>%  # add unit for the one numerical trait
  dplyr::mutate(
    methods = case_when(
      trait_name == "body_length" ~ "Once collected the body lengths of beetles were measured. Beetles were sampled
      along trasects using pitfall traps. At each sampling point, two pitfall traps, each consisting of a cup 90 mm 
      in diameter and 100 mm deep, a 600 mm x 50 mm high fence and a 200 mm x 200 mm roof, were placed 3 m a part in 
      a randomised direction. Pitfall traps containing pitfall fixative (94% ethanol, 5% glycol, 1% distilled water) 
      were left open for 1 week in February 2011. The two pitfall trap catches at each sampling point were pooled to 
      create one sample per sampling point. Beetles from each sample were sorted to species level. We used the Wog Wog 
      beetle reference collection stored at the Australian National Insect Collection at CSIRO Ecosystem Sciences in 
      Canberra, Australia, to help in the identification of some species. The CSIRO collection consists of morphospecies, 
      many of which are identified to species, each with a unique voucher code. We retained these codes in our data and 
      gave new codes to those morphospecies that we were not able to identify by reference to CSIRO Wog Wog collection. 
      Our resulting collection consists of species and morphospecies (all hereafter referred to as species). Our 
      experimental design consisted of three blocks of up to seven transects spanning the native Eucalyptus forest–pine 
      plantation edge. The edge between the Eucalyptus and pine forests is intersected by a dirt road of about 10 m in 
      width and (in one block) a cleared fire break of about 100 m in width. Sampling points were located along the 
      transects at 0 m (on the edge of the forest and road or firebreak), 10, 31.6, 100, 316 and 1000 m into the 
      Eucalyptus forest and 0, 10, 31.6, 100 and 316 m into the pine forest. We excluded sampling points in gullies and 
      ridge tops. As a result of this, many transects did not have the full complement of sampling points at all distances. 
      This left us with a total of 148 sampling points (84 in native Eucalyptus forest, 64 in pine forest) consisting of 
      296 pitfall traps arranged along 25 transects.",
      trait_name == "functional_role" ~ "We categorised our species into feeding guilds (predator, saproxylic, phytophagous, 
      mycetophagous, myrmecophylous, xylophagous) by referring to data used in previous work at the Wog Wog Fragmentation 
      Experiment (Margules 1992; Davies et al. 2000) for the same species in the same area. For species that we were not 
      able to match to the Wog Wog beetle collection, we assigned them to a feeding-guild according to other similar species 
      in the same genus that had already been assigned a feeding guild. Beetles were sampled along trasects using pitfall traps. 
      At each sampling point, two pitfall traps, each consisting of a cup 90 mm 
      in diameter and 100 mm deep, a 600 mm x 50 mm high fence and a 200 mm x 200 mm roof, were placed 3 m a part in 
      a randomised direction. Pitfall traps containing pitfall fixative (94% ethanol, 5% glycol, 1% distilled water) 
      were left open for 1 week in February 2011. The two pitfall trap catches at each sampling point were pooled to 
      create one sample per sampling point. Beetles from each sample were sorted to species level. We used the Wog Wog 
      beetle reference collection stored at the Australian National Insect Collection at CSIRO Ecosystem Sciences in 
      Canberra, Australia, to help in the identification of some species. The CSIRO collection consists of morphospecies, 
      many of which are identified to species, each with a unique voucher code. We retained these codes in our data and 
      gave new codes to those morphospecies that we were not able to identify by reference to CSIRO Wog Wog collection. 
      Our resulting collection consists of species and morphospecies (all hereafter referred to as species). Our 
      experimental design consisted of three blocks of up to seven transects spanning the native Eucalyptus forest–pine 
      plantation edge. The edge between the Eucalyptus and pine forests is intersected by a dirt road of about 10 m in 
      width and (in one block) a cleared fire break of about 100 m in width. Sampling points were located along the 
      transects at 0 m (on the edge of the forest and road or firebreak), 10, 31.6, 100, 316 and 1000 m into the 
      Eucalyptus forest and 0, 10, 31.6, 100 and 316 m into the pine forest. We excluded sampling points in gullies and 
      ridge tops. As a result of this, many transects did not have the full complement of sampling points at all distances. 
      This left us with a total of 148 sampling points (84 in native Eucalyptus forest, 64 in pine forest) consisting of 
      296 pitfall traps arranged along 25 transects.",
      trait_name == "wing_development" ~ "We categorised whether species were flightless or flight-capable by referring to 
      data used in previous work at the Wog Wog Fragmentation Experiment (Margules 1992; Davies et al. 2000) for the same 
      species in the same area. For species that we were not able to match to the Wog Wog beetle collection, we determined 
      whether they had functional wings according to other similar species in the same genus. Beetles were sampled
      along trasects using pitfall traps. At each sampling point, two pitfall traps, each consisting of a cup 90 mm 
      in diameter and 100 mm deep, a 600 mm x 50 mm high fence and a 200 mm x 200 mm roof, were placed 3 m a part in 
      a randomised direction. Pitfall traps containing pitfall fixative (94% ethanol, 5% glycol, 1% distilled water) 
      were left open for 1 week in February 2011. The two pitfall trap catches at each sampling point were pooled to 
      create one sample per sampling point. Beetles from each sample were sorted to species level. We used the Wog Wog 
      beetle reference collection stored at the Australian National Insect Collection at CSIRO Ecosystem Sciences in 
      Canberra, Australia, to help in the identification of some species. The CSIRO collection consists of morphospecies, 
      many of which are identified to species, each with a unique voucher code. We retained these codes in our data and 
      gave new codes to those morphospecies that we were not able to identify by reference to CSIRO Wog Wog collection. 
      Our resulting collection consists of species and morphospecies (all hereafter referred to as species). Our 
      experimental design consisted of three blocks of up to seven transects spanning the native Eucalyptus forest–pine 
      plantation edge. The edge between the Eucalyptus and pine forests is intersected by a dirt road of about 10 m in 
      width and (in one block) a cleared fire break of about 100 m in width. Sampling points were located along the 
      transects at 0 m (on the edge of the forest and road or firebreak), 10, 31.6, 100, 316 and 1000 m into the 
      Eucalyptus forest and 0, 10, 31.6, 100 and 316 m into the pine forest. We excluded sampling points in gullies and 
      ridge tops. As a result of this, many transects did not have the full complement of sampling points at all distances. 
      This left us with a total of 148 sampling points (84 in native Eucalyptus forest, 64 in pine forest) consisting of 
      296 pitfall traps arranged along 25 transects.",
      .default = as.character(methods))) %>%  # add methods for the different traits
  dplyr::mutate(site_name = "Wog Wog Habitat Fragmentation Experiment, Bondi State Forest and South East Forests National Park, NSW") %>%  # add site name
  dplyr::mutate(site_latitude = "-37.075000") %>%  # add site_latitude
  dplyr::mutate(site_longitude = "149.466667") %>%  # add site_longitude
  dplyr::mutate(site_date_of_visit = "2011-02") %>%  # add study date
  dplyr::mutate(site_description = "The area was formerly continuous native grassy to shrubby open Eucalyptus forest, 
                characterized by tussock grasses, forbs and scattered shrubs. A large area was cleared in 1987 and 
                planted with Pinus radiata for timber. At the time of data collection (2011), the pines were mature 
                trees of around 30 m height. At full maturity, the Pinus radiata forest is taller and more dense with 
                a more closed canopy than the native Eucalyptus forest. Its understory contains a dense layer of pine 
                needles with occasional shrubs, tussock grasses and sedges.") %>%  # add site description
  dplyr::mutate(associated_fauna_taxa = case_when(
    trait_name == "microhabitat_activity" & value == "myrmecophile" ~ "Formicidae",
    trait_name == "ecological_dependency" & value == "specialist" ~ "Formicidae",
    .default = as.character(associated_fauna_taxa))) %>% # add associated fauna taxa for beetles that are myrmecophiles
  dplyr::mutate(fauna_relation_description = case_when(
    trait_name == "microhabitat_activity" & value == "myrmecophile" ~ "The invertebrate is a specialist myrmecophile of the associated fauna",
    trait_name == "ecological_dependency" & value == "specialist" ~ "The invertebrate is a specialist myrmecophile of the associated fauna",
    .default = as.character(fauna_relation_description))) %>% # add associated fauna relation description for myrmecophiles
  dplyr::mutate(value = case_when(
    trait_name == "microhabitat_activity" & value == "myrmecophile" ~ "burrow",
    .default = as.character(value))) %>% # add microhabitat_activity for myrmecophiles
  dplyr::mutate(source_key = "Evans_2016") %>%  # add source_key
  dplyr::mutate(source_doi = "doi: 10.1007/s10980-016-0364-z") %>%  # add source_doi
  dplyr::mutate(source_citation = "Evans, M. J., Banks, S. C., Davies, K. F., Mcclenahan, J., Melbourne, B., & Driscoll, D. A. (2016). The use of traits to interpret responses to large scale-edge effects: a study of epigaeic beetle assemblages across a Eucalyptus forest and pine plantation edge. Landscape Ecology, 31, 1815-1831.") %>%  # add source_citation
  dplyr::mutate(source_type = "article") %>% # add source_type
  readr::write_csv(file.path(out_dir, "data.csv")) # save final dataset


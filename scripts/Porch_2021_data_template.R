##--------------------------------##
## Porch_2021 unpublished dataset ##
##--------------------------------##

## Script to clean and map the Porch & Driscoll 2020-2021 post-bushfire survey
## dataset to the InverTraits data_template and create a data.csv for the database.

## The dataset from post-fire SE Australian forests, focussing on mountain tops in  montane and alpine areas


##-------------------------##
#### The start up basics ####
##-------------------------##

## Set working directory
getwd()
setwd(...) # set this to R project directory if needed

out_dir <- file.path(getwd(), "outputs/other_dbs/Porch_2021")

## Load required package
library(tidyverse)

## Load the Porch_2021 dataset
d <- read_csv(file.path(getwd(), "data", "Porch_2021/Beetle_traits_from_post-bushfire_surveys_2020-21_Nick_Porch.csv"), show_col_types = FALSE)

## Load the data template
template <- read_csv(file.path(getwd(), "data", "Porch_2021/data_template.csv"), show_col_types = FALSE) 

## QUESTIONS FOR NICK & DON:
## Can you provide (1) simple methods for how the data was collected, (2) site name or names/region, 
## (3) site descriptions (habitat, elevation), and (4) time frame of data collection.

## # what life stage are feeding_group and habitat relevant to?
## # what habitat can we put for myrmecophile? burrow?


##-------------------------------------------------------------##
#### Modify the dataset in preparation for the data template ####
##-------------------------------------------------------------##

# Deal with Composite_Taxon (Yes)
# Deal with microhabitat for myrmecophiles - would it be burrow?

d_mod <- d %>%
  dplyr::filter(Introduced_Native == "Native") %>% # keep only native taxa
  dplyr::rename(
    taxon_family = Family,
    body_length = Size_mm,
    species_range_categories = NRE,
    wing_development = Winged,
    functional_role = Feeding_Guild,
    microhabitat_activity = Microhabitat) %>% # change column names to match data_template column names
  dplyr::mutate(across(species_range_categories:microhabitat_activity, str_to_lower)) %>% # change values to lowercase
  dplyr::mutate(
    species_range_categories = case_when(
      species_range_categories == "yes" ~ "sre",
      species_range_categories == "possible" ~ "likely_sre",
      species_range_categories == "no" ~ NA)) %>% # modify trait categories to match IA
  dplyr::mutate(
    wing_development = case_when(
      wing_development == "yes" ~ "winged",
      wing_development == "no" ~ "wingless",
      wing_development == "various" ~ NA)) %>% # modify trait categories to match IA
  dplyr::mutate(
    functional_role = case_when(
      functional_role == "detrivore" ~ "detritivore",
      functional_role == "phytophage" ~ "herbivore",
      functional_role == "various" ~ NA)) %>% # modify trait categories to match IA
  dplyr::mutate(
    microhabitat_activity = case_when(
      microhabitat_activity == "aquatic" ~ "water",
      microhabitat_activity == "dead wood" ~ "in_dead_wood_ground in_standing_wood_dead",
      microhabitat_activity == "dung/detrivore" ~ "dung litter_ground",
      microhabitat_activity == "geophilous" ~ "soil",
      microhabitat_activity == "ground/vegetation" ~ "ground_open vegetation",
      microhabitat_activity == "various" ~ NA,
      .default = as.character(microhabitat_activity))) %>% # modify trait categories to match IA
  dplyr::mutate(ecological_dependency = NA) %>% # create new column for ecological_dependency
  dplyr::mutate(
    ecological_dependency = case_when(
      microhabitat_activity == "myrmecophile" ~ "specialist",
      .default = as.character(ecological_dependency))) %>% # add ecological_dependency trait value for myrmecophiles
  dplyr::select(taxon_name, taxon_name_original, taxname_source, taxon_family, body_length, 
                species_range_categories, wing_development, functional_role, 
                microhabitat_activity, ecological_dependency) %>% # keep only needed columns for now
  dplyr::mutate(body_length = as.character(body_length))  %>% # set numeric column to character so that all trait values can be combined into one column
  tidyr::pivot_longer(cols = body_length:ecological_dependency,
                      names_to = "trait_name",
                      values_to = "value") %>%  # convert to long format so that all trait names and values are in two columns
  tidyr::drop_na(value) # remove rows with NA in the value column


##-------------------------------------------------------------------------##
#### Merge the dataset with the IA data template, & occupy missing cells ####
##-------------------------------------------------------------------------##

d_template <- template %>%
  dplyr::full_join(d_mod) %>%  # merge the modified dataset with empty data_template
  dplyr::filter(if_any(everything(), ~ !is.na(.))) %>%  # remove rows with all NAs (from data template)
  dplyr::mutate(entity_type_tax = "species") %>%  # add entity_type_tax category
  dplyr::mutate(
    life_stage_generic = case_when(
      trait_name == "body_length" ~ "adult",
      trait_name == "species_range_categories" ~ "all",
      trait_name == "wing_development" ~ "adult",
      trait_name == "functional_role" ~ "adult",
      trait_name == "microhabitat_activity" ~ "adult",
      trait_name == "ecological_dependency" ~ "adult")) %>%  # add life stage for the traits
  dplyr::mutate(entity_type = "metapopulation") %>%  # add entity_type category
  dplyr::mutate(
    value_type = case_when(
      trait_name == "body_length" ~ "mean", 
      .default = as.character(value_type))) %>%  # add value_type for the one numerical trait
  dplyr::mutate(
    unit_numeric = case_when(
      trait_name == "body_length" ~ "mm", 
      .default = as.character(unit_numeric))) %>%  # add unit for the one numerical trait
  dplyr::mutate(site_name = "southeast Australian forests") %>%  # add site name
  dplyr::mutate(site_date_of_visit = "2020/2021") %>%  # add study date
  dplyr::mutate(site_description = "Southeast forests, and mountain tops in  montane and alpine areas") %>%  # add site description
  dplyr::mutate(site_fire_impact = "Sites were burned in 2019-2020") %>%  # add fire impact description
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
  dplyr::mutate(source_key = "Porch_2021") %>%  # add source_key
  dplyr::mutate(source_citation = "Porch, N. & Driscoll, D. A. (2021). Post 2019-2020 bushfire beetle surveys in southeast Australian forests. Unpublished dataset.") %>%  # add source_citation
  dplyr::mutate(source_type = "unpublished_data") %>% # add source_type
  readr::write_csv(file.path(out_dir, "data.csv")) # save final dataset
  
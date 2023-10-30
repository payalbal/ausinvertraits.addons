##----------------------------------##
## Porch_2021 unpublished dataset ##
##----------------------------------##

## Script to clean and map the Porch and Driscoll 2020-2021 post-bushfire beetle survey
## in upland areas of southeastern Australia dataset to the InverTraits data_template 
## and create a data.csv for the database.


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


##-------------------------------------------------------------##
#### Modify the dataset in preparation for the data template ####
##-------------------------------------------------------------##

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
      functional_role == "various" ~ NA,
      microhabitat_activity == "dung" ~ "coprophage detritivore",
      microhabitat_activity == "dung/detrivore" ~ "coprophage detritivore")) %>% # modify trait categories to match IA
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
    entity_type = case_when(
      trait_name == "body_length" ~ "individual", 
      .default = as.character(entity_type))) %>%  # add entity_type for body_length
  dplyr::mutate(
    value_type = case_when(
      trait_name == "body_length" ~ "raw", 
      .default = as.character(value_type))) %>%  # add value_type for the one numerical trait
  dplyr::mutate(
    unit_numeric = case_when(
      trait_name == "body_length" ~ "mm", 
      .default = as.character(unit_numeric))) %>%  # add unit for the one numerical trait
  dplyr::mutate(
    methods = case_when(
      trait_name == "body_length" ~ "Once collected, body length was measured on a reference individual from a taxon. We conducted beetle pitfall surveys at the 40 selected locations, including a mixture of 68 burned and unburned sites that were paired where possible. These surveys were conducted in the summer of 2020-21, approximately one year after the 2019-20 megafires. At each site (n = 68), we established nine pitfall arrays, including three replicates of three classes of micro-refuge (logs, rocks, and wet areas). Prior to deploying the pitfall traps, we conducted on-site searches to find micro-refuges that were most likely to have facilitated beetle survivorship during the fires. For example, the largest rocks, oldest logs, or wettest areas. We used falcon tubes (25 mm diameter and 100 mm length) as beetle pitfall traps, and each tube contained 15 ml of 50% propylene glycol solution as a preservative. These were dug into the ground so that they sat flush with the soil surface. We placed a bamboo stick in each pitfall trap, as a climbing pole to allow vertebrate bycatch to escape. Transparent plastics lids supported by wooden skewers were used as covers, to protect pitfall traps from pollution and precipitation. We placed the pitfall traps in clusters of three, 75 cm apart in a triangular arrangement, at 5 m intervals along a 20 m array (15 traps total) representing a particular micro-refuge. Each trio was placed in an area most suitable for sampling the targeted micro-refuge. For example, the distance between clusters was occasionally increased or decreased, depending on the spacing of logs in the environment. The location of each cluster was recorded using a GPS and marked with pink flagging tape. We left the pitfall traps in the ground for approximately one month. On retrieval each trio was pooled into a single sample, so that there were five samples for each array. We sealed the tubes with a cap and took them to the laboratory, where sorting and identification of captured species could occur. In the lab, we sorted all pitfall samples using binocular microscopes, and beetles were removed and stored in EtOH. Each beetle specimen was then identified to morphospecies by Nicholas Porch and placed within known genera. Where possible, morphospecies were identified to species. Each species or morphospecies was photographed at least once using a Leica M205-C steromicroscope and pinned/pointed to create a reference collection; non-reference material was stored in labelled vials. Multiple photographs were taken of common morphospecies, or variable taxa", 
      trait_name == "species_range_categories" ~ "Once collected, beetle taxa were classified as a narrow range endemic (or short range endemic) if their known distribution was less than 10,000 km2. We conducted beetle pitfall surveys at the 40 selected locations, including a mixture of 68 burned and unburned sites that were paired where possible. These surveys were conducted in the summer of 2020-21, approximately one year after the 2019-20 megafires. At each site (n = 68), we established nine pitfall arrays, including three replicates of three classes of micro-refuge (logs, rocks, and wet areas). Prior to deploying the pitfall traps, we conducted on-site searches to find micro-refuges that were most likely to have facilitated beetle survivorship during the fires. For example, the largest rocks, oldest logs, or wettest areas. We used falcon tubes (25 mm diameter and 100 mm length) as beetle pitfall traps, and each tube contained 15 ml of 50% propylene glycol solution as a preservative. These were dug into the ground so that they sat flush with the soil surface. We placed a bamboo stick in each pitfall trap, as a climbing pole to allow vertebrate bycatch to escape. Transparent plastics lids supported by wooden skewers were used as covers, to protect pitfall traps from pollution and precipitation. We placed the pitfall traps in clusters of three, 75 cm apart in a triangular arrangement, at 5 m intervals along a 20 m array (15 traps total) representing a particular micro-refuge. Each trio was placed in an area most suitable for sampling the targeted micro-refuge. For example, the distance between clusters was occasionally increased or decreased, depending on the spacing of logs in the environment. The location of each cluster was recorded using a GPS and marked with pink flagging tape. We left the pitfall traps in the ground for approximately one month. On retrieval each trio was pooled into a single sample, so that there were five samples for each array. We sealed the tubes with a cap and took them to the laboratory, where sorting and identification of captured species could occur. In the lab, we sorted all pitfall samples using binocular microscopes, and beetles were removed and stored in EtOH. Each beetle specimen was then identified to morphospecies by Nicholas Porch and placed within known genera. Where possible, morphospecies were identified to species. Each species or morphospecies was photographed at least once using a Leica M205-C steromicroscope and pinned/pointed to create a reference collection; non-reference material was stored in labelled vials. Multiple photographs were taken of common morphospecies, or variable taxa", 
      trait_name == "wing_development" ~ "Once collected, beetle taxa were classified as having wings or not based on a reference specimen. We conducted beetle pitfall surveys at the 40 selected locations, including a mixture of 68 burned and unburned sites that were paired where possible. These surveys were conducted in the summer of 2020-21, approximately one year after the 2019-20 megafires. At each site (n = 68), we established nine pitfall arrays, including three replicates of three classes of micro-refuge (logs, rocks, and wet areas). Prior to deploying the pitfall traps, we conducted on-site searches to find micro-refuges that were most likely to have facilitated beetle survivorship during the fires. For example, the largest rocks, oldest logs, or wettest areas. We used falcon tubes (25 mm diameter and 100 mm length) as beetle pitfall traps, and each tube contained 15 ml of 50% propylene glycol solution as a preservative. These were dug into the ground so that they sat flush with the soil surface. We placed a bamboo stick in each pitfall trap, as a climbing pole to allow vertebrate bycatch to escape. Transparent plastics lids supported by wooden skewers were used as covers, to protect pitfall traps from pollution and precipitation. We placed the pitfall traps in clusters of three, 75 cm apart in a triangular arrangement, at 5 m intervals along a 20 m array (15 traps total) representing a particular micro-refuge. Each trio was placed in an area most suitable for sampling the targeted micro-refuge. For example, the distance between clusters was occasionally increased or decreased, depending on the spacing of logs in the environment. The location of each cluster was recorded using a GPS and marked with pink flagging tape. We left the pitfall traps in the ground for approximately one month. On retrieval each trio was pooled into a single sample, so that there were five samples for each array. We sealed the tubes with a cap and took them to the laboratory, where sorting and identification of captured species could occur. In the lab, we sorted all pitfall samples using binocular microscopes, and beetles were removed and stored in EtOH. Each beetle specimen was then identified to morphospecies by Nicholas Porch and placed within known genera. Where possible, morphospecies were identified to species. Each species or morphospecies was photographed at least once using a Leica M205-C steromicroscope and pinned/pointed to create a reference collection; non-reference material was stored in labelled vials. Multiple photographs were taken of common morphospecies, or variable taxa", 
      trait_name == "functional_role" ~ "Once collected, beetle taxa were assigned to feeding group by an expert or according to related taxa. We conducted beetle pitfall surveys at the 40 selected locations, including a mixture of 68 burned and unburned sites that were paired where possible. These surveys were conducted in the summer of 2020-21, approximately one year after the 2019-20 megafires. At each site (n = 68), we established nine pitfall arrays, including three replicates of three classes of micro-refuge (logs, rocks, and wet areas). Prior to deploying the pitfall traps, we conducted on-site searches to find micro-refuges that were most likely to have facilitated beetle survivorship during the fires. For example, the largest rocks, oldest logs, or wettest areas. We used falcon tubes (25 mm diameter and 100 mm length) as beetle pitfall traps, and each tube contained 15 ml of 50% propylene glycol solution as a preservative. These were dug into the ground so that they sat flush with the soil surface. We placed a bamboo stick in each pitfall trap, as a climbing pole to allow vertebrate bycatch to escape. Transparent plastics lids supported by wooden skewers were used as covers, to protect pitfall traps from pollution and precipitation. We placed the pitfall traps in clusters of three, 75 cm apart in a triangular arrangement, at 5 m intervals along a 20 m array (15 traps total) representing a particular micro-refuge. Each trio was placed in an area most suitable for sampling the targeted micro-refuge. For example, the distance between clusters was occasionally increased or decreased, depending on the spacing of logs in the environment. The location of each cluster was recorded using a GPS and marked with pink flagging tape. We left the pitfall traps in the ground for approximately one month. On retrieval each trio was pooled into a single sample, so that there were five samples for each array. We sealed the tubes with a cap and took them to the laboratory, where sorting and identification of captured species could occur. In the lab, we sorted all pitfall samples using binocular microscopes, and beetles were removed and stored in EtOH. Each beetle specimen was then identified to morphospecies by Nicholas Porch and placed within known genera. Where possible, morphospecies were identified to species. Each species or morphospecies was photographed at least once using a Leica M205-C steromicroscope and pinned/pointed to create a reference collection; non-reference material was stored in labelled vials. Multiple photographs were taken of common morphospecies, or variable taxa", 
      trait_name == "microhabitat_activity" ~ "Once collected, beetle taxa were assigned to microhabitat by an expert or according to related taxa. We conducted beetle pitfall surveys at the 40 selected locations, including a mixture of 68 burned and unburned sites that were paired where possible. These surveys were conducted in the summer of 2020-21, approximately one year after the 2019-20 megafires. At each site (n = 68), we established nine pitfall arrays, including three replicates of three classes of micro-refuge (logs, rocks, and wet areas). Prior to deploying the pitfall traps, we conducted on-site searches to find micro-refuges that were most likely to have facilitated beetle survivorship during the fires. For example, the largest rocks, oldest logs, or wettest areas. We used falcon tubes (25 mm diameter and 100 mm length) as beetle pitfall traps, and each tube contained 15 ml of 50% propylene glycol solution as a preservative. These were dug into the ground so that they sat flush with the soil surface. We placed a bamboo stick in each pitfall trap, as a climbing pole to allow vertebrate bycatch to escape. Transparent plastics lids supported by wooden skewers were used as covers, to protect pitfall traps from pollution and precipitation. We placed the pitfall traps in clusters of three, 75 cm apart in a triangular arrangement, at 5 m intervals along a 20 m array (15 traps total) representing a particular micro-refuge. Each trio was placed in an area most suitable for sampling the targeted micro-refuge. For example, the distance between clusters was occasionally increased or decreased, depending on the spacing of logs in the environment. The location of each cluster was recorded using a GPS and marked with pink flagging tape. We left the pitfall traps in the ground for approximately one month. On retrieval each trio was pooled into a single sample, so that there were five samples for each array. We sealed the tubes with a cap and took them to the laboratory, where sorting and identification of captured species could occur. In the lab, we sorted all pitfall samples using binocular microscopes, and beetles were removed and stored in EtOH. Each beetle specimen was then identified to morphospecies by Nicholas Porch and placed within known genera. Where possible, morphospecies were identified to species. Each species or morphospecies was photographed at least once using a Leica M205-C steromicroscope and pinned/pointed to create a reference collection; non-reference material was stored in labelled vials. Multiple photographs were taken of common morphospecies, or variable taxa", 
      trait_name == "ecological_dependency" ~ "Once collected, beetle taxa were assigned an ecological dependency where relevant by an expert. We conducted beetle pitfall surveys at the 40 selected locations, including a mixture of 68 burned and unburned sites that were paired where possible. These surveys were conducted in the summer of 2020-21, approximately one year after the 2019-20 megafires. At each site (n = 68), we established nine pitfall arrays, including three replicates of three classes of micro-refuge (logs, rocks, and wet areas). Prior to deploying the pitfall traps, we conducted on-site searches to find micro-refuges that were most likely to have facilitated beetle survivorship during the fires. For example, the largest rocks, oldest logs, or wettest areas. We used falcon tubes (25 mm diameter and 100 mm length) as beetle pitfall traps, and each tube contained 15 ml of 50% propylene glycol solution as a preservative. These were dug into the ground so that they sat flush with the soil surface. We placed a bamboo stick in each pitfall trap, as a climbing pole to allow vertebrate bycatch to escape. Transparent plastics lids supported by wooden skewers were used as covers, to protect pitfall traps from pollution and precipitation. We placed the pitfall traps in clusters of three, 75 cm apart in a triangular arrangement, at 5 m intervals along a 20 m array (15 traps total) representing a particular micro-refuge. Each trio was placed in an area most suitable for sampling the targeted micro-refuge. For example, the distance between clusters was occasionally increased or decreased, depending on the spacing of logs in the environment. The location of each cluster was recorded using a GPS and marked with pink flagging tape. We left the pitfall traps in the ground for approximately one month. On retrieval each trio was pooled into a single sample, so that there were five samples for each array. We sealed the tubes with a cap and took them to the laboratory, where sorting and identification of captured species could occur. In the lab, we sorted all pitfall samples using binocular microscopes, and beetles were removed and stored in EtOH. Each beetle specimen was then identified to morphospecies by Nicholas Porch and placed within known genera. Where possible, morphospecies were identified to species. Each species or morphospecies was photographed at least once using a Leica M205-C steromicroscope and pinned/pointed to create a reference collection; non-reference material was stored in labelled vials. Multiple photographs were taken of common morphospecies, or variable taxa"
      )) %>%  # add methods for the traits
  dplyr::mutate(site_name = "southeast Australian upland areas in NSW, VIC, ACT") %>%  # add site name
  dplyr::mutate(site_date_of_visit = "2020-12/2021-02") %>%  # add study date
  dplyr::mutate(site_description = "Our study was conducted at 40 montane and subalpine locations in Victoria, NSW, and the ACT. Locations were then classified as either escarpment or subalpine based on their proximity to the coast and vegetation type. Escarpment sites marked the first higher elevation point inland from the coast and included wet sclerophyll and warm temperate rainforest habitat. Subalpine sites were further inland, generally higher in elevation and had subalpine woodland vegetation, often dominated by Snow Gums (Eucalyptus pauciflora).") %>%  # add site description
  dplyr::mutate(site_fire_impact = "Sites were burned or unburned in the 2019-20 megafires") %>%  # add fire impact description
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
  dplyr::mutate(source_citation = "Porch, N. & Driscoll, D. A. (2021). Impact of the 2019-20 megafires on narrow-range endemic beetles in south-eastern Australia. Unpublished data.") %>%  # add source_citation
  dplyr::mutate(source_type = "unpublished_data") %>% # add source_type
  readr::write_csv(file.path(out_dir, "data.csv")) # save final dataset
  
##################################
## Other databases - Bland_2017 ##
##################################


## Script to clean and map the Bland_2017 raw crayfish 
## datasets provided by Bland (2) and the open access dataset (1)
## that accompanies the paper to the IA data_template.


##---------------------##
## The start up basics ##
##---------------------##

## Load required package
library(tidyverse)


## Load the Bland datasets
b1 <- read.csv("scripts/Allometry_06062014.csv", header = T) 
b2 <- read.csv("scripts/Life_History_Traits_06062014.csv", header = T) 
b3 <- read.csv("scripts/Crayfish_Species_Dataset.csv", header = T) 

## Load the file containing Australian crayfish genera and family name
b_names <- read.csv("scripts/aus_crayfish_genera.csv", header = T) 

## Load the data template
template <- read.csv("scripts/template.csv", header = T) 


##-----------------------##
## The allometry dataset ##
##-----------------------##

## (1) Modify the dataset in preparation for the data template.

b1_mod <- b1 %>%
  dplyr::select(-tax_id, -Specimen, -c(CL:AbdoWidth), -Mean_of_N_individuals) %>% # drop unnecessary columns
  dplyr::rename(
    taxon_name = Binomial,
    value = OCL,
    sex = Sex,
    secondary_citation = Reference) %>% # change column names to match data_template column names
  tidyr::separate_wider_delim(
    taxon_name,
    delim = " ",
    names = c("genus", "epithet"),
    cols_remove = FALSE) %>% # create new columns with genus and specific epithet, so that the Australian crayfish family name can be matched below
  dplyr::left_join(b_names, by = "genus") %>% # merge the crayfish name dataset for taxon family names
  dplyr::mutate(Heterochely = replace_na(Heterochely, "None")) %>%  # add category for specimens without heterochely so that rows can be excluded below
  dplyr::filter(Heterochely != "Small") %>%  # exclude rows with measurements for the small chelae as they are repeat rows of individuals
  dplyr::mutate(sex = str_to_lower(sex)) %>%  # change sex categories to lowercase case
  dplyr::mutate(measurement_remarks1 = secondary_citation) %>%  # duplicate citation column (so that notes about museum specimens can be separated from citations below)
  dplyr::mutate(
    measurement_remarks1 = case_when(
      Data_Type == "Plate" ~  NA,
      Data_Type == "Table" ~  NA,
      Data_Type == "Table mean" ~  NA,
      .default = as.character(measurement_remarks1))) %>%  # keep only notes related to 'Museum' data types in this temporary measurement_remarks column (to be collapsed later)
  dplyr::mutate(
    secondary_citation = case_when(
      Data_Type == "Museum" ~  NA,
      .default = as.character(secondary_citation))) %>%  # remove notes related to museum specimens from the secondary citation column (so it is only citations)
  dplyr::mutate(
    Data_Type = case_when(
      Data_Type == "Plate" ~ "measurement taken from plate",
      Data_Type == "Museum" ~ "measurement made on museum specimen",
      Data_Type == "Table" ~ "measurement taken from table",
      Data_Type == "Table mean" ~ "measurement taken from table mean")) %>%  # describe where data comes from
  dplyr::mutate(
    Types = case_when(
      Types == "Allotype" ~ "measurement made on allotype",
      Types == "Holotype" ~ "measurement made on holotype",
      Types == "Paratype" ~ "measurement made on paratype",
      Types == "Paratypes" ~ "measurement made on paratypes",
      .default = as.character(measurement_remarks1))) %>%  # describe what 'types' the measurements came from
  tidyr::unite(
    "measurement_remarks",
    sep = ", ",
    c("Data_Type", "Types", "measurement_remarks1"),
    na.rm = TRUE,
    remove = FALSE) %>%  # combine information about measurement details into measurement_remarks
  dplyr::select(-genus, -epithet, -Data_Type, -Types, -Heterochely, -measurement_remarks1)  # drop unnecessary columns


## (2) Merge the allometry dataset & IA data template, & occupy missing cells

b1_template <- template %>%
  dplyr::full_join(b1_mod) %>%  # merge with empty data_template
  dplyr::filter(if_any(everything(), ~ !is.na(.))) %>%  # remove rows with all NAs (from data template)
  dplyr::mutate(taxname_source = "AFD") %>%  # add taxname_source (note that this is only correct for the Australian species)
  dplyr::mutate(entity_type_tax = "species") %>%  # add entity_type_tax category
  dplyr::mutate(life_stage_generic = "adult") %>%  # add life_stage_generic category
  dplyr::mutate(trait_name = "occipital_carapace_length") %>%  # add trait_name
  dplyr::mutate(entity_type = "individual") %>%  # add entity_type category
  dplyr::mutate(value_type = "raw") %>%  # add value_type category
  dplyr::mutate(unit_numeric = "mm") %>%  # add unit_numeric
  tidyr::drop_na(value) %>%  # remove rows with NA in the value column
  dplyr::mutate(value = as.character(value))  %>%  # make the value column a character
  dplyr::mutate(replicates = 1) %>%  # add replicates
  dplyr::mutate(methods = "Measurements were obtained from species descriptions, museum plates, museum specimens, and field specimens. Morphological measurements were obtained at 0.1 mm precision.") %>% # add methods
  dplyr::mutate(source_key = "Bland_2017") %>%  # add source_key
  dplyr::mutate(source_doi = "doi: 10.1111/acv.12350") %>%  # add source_doi
  dplyr::mutate(source_citation = "Bland, L. M. (2017). Global correlates of extinction risk in freshwater crayfish. Animal Conservation, 20(6), 532-542.") %>%  # add source_citation
  dplyr::mutate(source_type = "article")  # add source_type


##---------------------------------##
## The life history traits dataset ##
##---------------------------------##

## (1) Modify the dataset in preparation for the data template.

## (1a) First make two data frames for two complicated variables (female size at maturity and male minimum form 1 size)

## Female size at maturity data (to be merged later on)

b2_size_mat <- b2 %>%
  dplyr::select(Friendly_name, Size_maturity_females, Size_Mat_Type, Ref_Size_mat) %>%  # select needed columns
  dplyr::rename(
    taxon_name = Friendly_name,
    value = Size_maturity_females,
    trait_name = Size_Mat_Type,
    secondary_citation = Ref_Size_mat) %>%  # rename columns
  tidyr::separate_wider_delim(
    taxon_name,
    delim = " ",
    names = c("genus", "epithet"),
    cols_remove = FALSE) %>%  # create new columns with genus and specific epithet, so that the Australian crayfish family name can be matched below
  dplyr::left_join(b_names, by = "genus") %>%  # merge the crayfish name dataset for taxon family names for the Australian species
  dplyr::filter(trait_name %in% c("OCL", "TL")) %>%  # keep only rows with occipital_carapace_length or body_length measurements
  dplyr::mutate(
    trait_name = case_when(
      trait_name == "OCL" ~ "occipital_carapace_length_mat",
      trait_name == "TL" ~ "body_length_mat")) %>%  # rename size at maturity categories with unique trait names so that details can be added later
  dplyr::select(taxon_name, taxon_family, trait_name, value, secondary_citation)  # select only needed columns


## Male form 1 size data (to be merged later on)

b2_size_form <- b2 %>%
  dplyr::select(Friendly_name, Min_size_form_I_males, Type_size_form_I, Ref_form_I_males) %>%   # select needed columns
  dplyr::rename(
    taxon_name = Friendly_name,
    value = Min_size_form_I_males,
    trait_name = Type_size_form_I,
    secondary_citation = Ref_form_I_males)  %>%  # rename columns
  tidyr::separate_wider_delim(
    taxon_name,
    delim = " ",
    names = c("genus", "epithet"),
    cols_remove = FALSE) %>% # create new columns with genus and specific epithet, so that the Australian crayfish family name can be matched below
  dplyr::left_join(b_names, by = "genus") %>% # merge the crayfish name dataset for taxon family names for the Australian species
  dplyr::filter(trait_name %in% c("OCL", "TL")) %>%  # keep only rows with occipital_carapace_length or body_length measurements
  dplyr::mutate(
    trait_name = case_when(
      trait_name == "OCL" ~ "occipital_carapace_length_form",
      trait_name == "TL" ~ "body_length_form")) %>%  # rename form I size categories with unique trait names so that details can be added later
  dplyr::select(taxon_name, taxon_family, trait_name, value, secondary_citation)  # select only needed columns


## (1b) Create a dataset with the secondary_citations for each trait (to be merged with the trait values later).

b2_mod_refs <- b2 %>%
  dplyr::select(Friendly_name, Ref_OCL, Ref_BL, Ref_EggS, Ref_egg_number, Ref_Lifespan, Ref_Age) %>%   # select needed columns
  dplyr::rename(taxon_name = Friendly_name) %>%  # rename taxon name column
  tidyr::pivot_longer(cols = Ref_OCL:Ref_Age,
                      names_to = "trait_name2",
                      values_to = "secondary_citation") %>%  # convert to long format so that all trait names and values are in two columns
  dplyr::mutate(
    trait_name = case_when(
      trait_name2 == "Ref_OCL" ~ "occipital_carapace_length",
      trait_name2 == "Ref_BL" ~ "body_length",
      trait_name2 == "Ref_EggS" ~ "Egg_size",
      trait_name2 == "Ref_egg_number" ~ "fecundity_per_reproductive_event",
      trait_name2 == "Ref_Lifespan" ~ "life_span",
      trait_name2 == "Ref_Age" ~ "time_to_maturity")) # add a trait_name column that names the traits that citations are associated with (so that this dataset can be merged later)

## (1c) Create a dataset with the trait names and values, and merge with the secondary citation, and size (maturity and form I) data sets.

b2_mod <- b2 %>%
  dplyr::rename(
    taxon_name = Friendly_name,
    occipital_carapace_length = Max_OCL,
    body_length = Max_BL,
    fecundity_per_reproductive_event = MaxEggNumber,
    life_span = Lifespan,
    time_to_maturity = Age_at_maturity) %>%  # change column names to match data_template column names
  dplyr::mutate(life_span = life_span * 365) %>%  # convert life span in years to days
  dplyr::mutate(time_to_maturity = time_to_maturity * 365) %>%  # convert time to maturity in years to days
  tidyr::separate_wider_delim(
    taxon_name,
    delim = " ",
    names = c("genus", "epithet"),
    cols_remove = FALSE) %>% # create new columns with genus and specific epithet, so that the Australian crayfish family name can be matched below
  dplyr::left_join(b_names, by = "genus") %>% # merge the crayfish name dataset for taxon family names for the Australian species
  dplyr::select(
    taxon_name,
    taxon_family,
    occipital_carapace_length,
    body_length,
    Egg_size,
    fecundity_per_reproductive_event,
    life_span,
    time_to_maturity) %>%   # select only needed columns
  tidyr::pivot_longer(
    cols = occipital_carapace_length:time_to_maturity,
    names_to = "trait_name",
    values_to = "value") %>% # convert to long format so that all trait names and values are in two columns
  dplyr::left_join(b2_mod_refs, by = c("taxon_name", "trait_name")) %>% # merge the dataset with secondary citations
  dplyr::select(-trait_name2) %>%
  tidyr::drop_na(value)  %>% # remove rows with NA in the value column
  dplyr::bind_rows(b2_size_mat, b2_size_form) # combine the female size at maturity and male form I size data


## (2) Merge the life history dataset & IA data template, & occupy missing cells

b2_template <- template %>%
  dplyr::full_join(b2_mod) %>%  # merge with empty data_template
  dplyr::filter(if_any(everything(), ~ !is.na(.))) %>%  # remove rows with all NAs (from data template)
  dplyr::mutate(taxname_source = "AFD") %>%  # add taxname_source (note that this is only correct for the Australian species)
  dplyr::mutate(entity_type_tax = "species") %>%  # add entity_type_tax category
  dplyr::mutate(
    life_stage_generic = case_when(
      trait_name == "body_length" ~ "adult",
      trait_name == "body_length_form" ~ "adult",
      trait_name == "body_length_mat" ~ "adult",
      trait_name == "Egg_size" ~ "egg",
      trait_name == "fecundity_per_reproductive_event" ~ "all",
      trait_name == "life_span" ~ "all",
      trait_name == "occipital_carapace_length" ~ "adult",
      trait_name == "occipital_carapace_length_form" ~ "adult",
      trait_name == "occipital_carapace_length_mat" ~ "adult",
      trait_name == "time_to_maturity" ~ "all")) %>%   # add life stage for each trait
  dplyr::mutate(
    life_stage_taxon_specific = case_when(
      trait_name == "occipital_carapace_length_form" ~ "form 1",
      trait_name == "body_length_form" ~ "form 1")) %>%   # add form 1 life stage for the appropriate traits
  dplyr::mutate(
    sex = case_when(
      trait_name == "occipital_carapace_length_form" ~ "male",
      trait_name == "body_length_form" ~ "male",
      trait_name == "occipital_carapace_length_mat" ~ "female",
      trait_name == "body_length_mat" ~ "female",
      trait_name == "fecundity_per_reproductive_event" ~ "all",
      .default = as.character(sex))) %>%   # add sex for the appropriate traits
  dplyr::mutate(entity_type = "species") %>% # add entity_type
  dplyr::mutate(value_type = case_when(
    trait_name == "body_length" ~ "maximum",
    trait_name == "body_length_form" ~ "minimum",
    trait_name == "body_length_mat" ~ "mean",
    trait_name == "Egg_size" ~ "maximum",
    trait_name == "fecundity_per_reproductive_event" ~ "maximum",
    trait_name == "life_span" ~ "mean",
    trait_name == "occipital_carapace_length" ~ "maximum",
    trait_name == "occipital_carapace_length_form" ~ "minimum",
    trait_name == "occipital_carapace_length_mat" ~ "mean",
    trait_name == "time_to_maturity" ~ "mean")) %>%  # specify value_type for numerical traits
  dplyr::mutate(measurement_remarks = case_when(
    trait_name == "body_length_mat" ~ "Female size at maturity.",
    trait_name == "occipital_carapace_length_mat" ~ "Female size at maturity.")) %>%  # provide details of measurements
  dplyr::mutate(
    trait_name = case_when(
      trait_name == "body_length_form" ~ "body_length",
      trait_name == "body_length_mat" ~ "body_length",
      trait_name == "Egg_size" ~ "body_length",
      trait_name == "occipital_carapace_length_form" ~ "occipital_carapace_length",
      trait_name == "occipital_carapace_length_mat" ~ "occipital_carapace_length",
      .default = as.character(trait_name))) %>%  # rename traits
  dplyr::mutate(unit_numeric = case_when(
    trait_name == "body_length" ~ "mm",
    trait_name == "fecundity_per_reproductive_event" ~ "offspring",
    trait_name == "life_span" ~ "days",
    trait_name == "occipital_carapace_length" ~ "mm",
    trait_name == "time_to_maturity" ~ "days")) %>%  # specify units for numerical traits
  dplyr::mutate(value = as.character(value))  %>%  # make the value column a character
  dplyr::mutate(methods = case_when(
    trait_name == "body_length" ~ "Body lengths were collected from species descriptions and field guides. Maximum size was used as mean size is generally not available for crayfish species.",
    trait_name == "fecundity_per_reproductive_event" ~ "Maximum number of eggs taken from species descriptions, field guides, and museum specimens.",
    trait_name == "life_span" ~ "Life span was taken from species descriptions and field guides.",
    trait_name == "occipital_carapace_length" ~ "Occipital carapace lengths were collected from species descriptions and field guides. Maximum size was used as mean size is generally not available for crayfish species.",
    trait_name == "time_to_maturity" ~ "Age at maturity was taken from species descriptions and field guides."))  %>%  # specify methods for each trait.
  dplyr::mutate(source_key = "Bland_2017") %>%  # add source_key
  dplyr::mutate(source_doi = "doi: 10.1111/acv.12350") %>%  # add source_doi
  dplyr::mutate(source_citation = "Bland, L. M. (2017). Global correlates of extinction risk in freshwater crayfish. Animal Conservation, 20(6), 532-542.") %>% # add source_citation
  dplyr::mutate(source_type = "article")  # add source_type


##-------------------------##
## The open access dataset ##
##-------------------------##


## (1) Modify the dataset in preparation for the data template.

b3_mod <- b3 %>%
  dplyr::select(Binomial, Family, HabitatType, EOO) %>%   # select needed columns
  dplyr::rename(
    taxon_name = Binomial,
    taxon_family = Family,
    microhabitat_activity = HabitatType) %>%  # change column names to match data_template names
  dplyr::mutate(taxon_family = str_to_title(taxon_family)) %>%  # change family names to title case
  dplyr::mutate(microhabitat_shelter = microhabitat_activity) %>%  # duplicate the microhabitat_activity column and call microhabitat_shelter
  tidyr::pivot_longer(cols = microhabitat_activity:microhabitat_shelter,
                      names_to = "trait_name",
                      values_to = "value") %>%  # convert to long format so that all trait names and values are in two columns
  tidyr::drop_na(value) # remove rows with NA in the value column


## (2) Merge the open access dataset & IA data template, & occupy missing cells

b3_template <- template %>%
  dplyr::full_join(b3_mod) %>%  # merge with empty data_template
  dplyr::filter(if_any(everything(), ~ !is.na(.))) %>%  # remove rows with all NAs (from data template)
  dplyr::mutate(taxname_source = "AFD") %>%  # add taxname_source
  dplyr::mutate(entity_type_tax = "species") %>%  # add entity_type_tax
  dplyr::mutate(life_stage_generic = "all") %>%  # add life_stage_generic
  dplyr::mutate(sex = "all") %>% # add sex category
  dplyr::mutate(entity_type = "species") %>% # add entity_type
  dplyr::mutate(value_type = case_when(
    trait_name == "EOO" ~ "raw")) %>%  # specify value_type for numerical traits
  dplyr::mutate(unit_numeric = case_when(
    trait_name == "EOO" ~ "km2")) %>%  # specify unit_numeric for numerical traits
  dplyr::mutate(value = case_when(
    trait_name == "microhabitat_activity" & value == 1 ~ "water_lotic",
    trait_name == "microhabitat_activity" & value == 2 ~ "water_lentic",
    trait_name == "microhabitat_activity" & value == 3 ~ "burrow",
    trait_name == "microhabitat_activity" & value == 4 ~ "troglofauna",
    .default = as.character(value))) %>%  # change trait categories to match IA trait levels  
  dplyr::mutate(value = case_when(
    trait_name == "microhabitat_shelter" & value == 1 ~ "water_lotic",
    trait_name == "microhabitat_shelter" & value == 2 ~ "water_lentic",
    trait_name == "microhabitat_shelter" & value == 3 ~ "burrow",
    trait_name == "microhabitat_shelter" & value == 4 ~ "troglofauna",
    .default = as.character(value))) %>%  # change trait categories to match IA trait levels  
  dplyr::mutate(methods = case_when(
    trait_name == "microhabitat_activity" ~ "Species were assigned to four habitat types: (1) streams and rivers, (2) lakes and wetlands, (3) burrows, and (4) caves. Data was taken from Adamowicz & Purvis (2006) to classify habitat type for 490 species, and IUCN assessments (IUCN 2010), field guides, and species descriptions for the remaining species. “Strong burrowers” in the Australian scheme of Riek (1972) were also placed in “burrows”, while weak or moderate burrowers were placed in categories “streams and rivers” or “lakes and wetlands” according to other habitat information.",
    trait_name == "microhabitat_shelter" ~ "Species were assigned to four habitat types: (1) streams and rivers, (2) lakes and wetlands, (3) burrows, and (4) caves. Data was taken from Adamowicz & Purvis (2006) to classify habitat type for 490 species, and IUCN assessments (IUCN 2010), field guides, and species descriptions for the remaining species. “Strong burrowers” in the Australian scheme of Riek (1972) were also placed in “burrows”, while weak or moderate burrowers were placed in categories “streams and rivers” or “lakes and wetlands” according to other habitat information.",
    trait_name == "EOO" ~ "Geographical range size for each species was taken from IUCN (2010) Red List of threatened species.")) %>%  # add methods for the different traits
  dplyr::mutate(source_key = "Bland_2017") %>% # add source_key
  dplyr::mutate(source_doi = "doi: 10.1111/acv.12350") %>% # add source_doi
  dplyr::mutate(source_citation = "Bland, L. M. (2017). Global correlates of extinction risk in freshwater crayfish. Animal Conservation, 20(6), 532-542.") %>% # add source_citation
  dplyr::mutate(source_type = "article")  %>% # add source_type
  dplyr::mutate(secondary_citation = case_when(
    trait_name == "microhabitat_activity" ~ "Adamowicz, S. J., & Purvis, A. (2006). Macroevolution and extinction risk patterns in freshwater crayfish. Freshwater Crayfish, 15, 1-23; Riek, E. F. (1972). The phylogeny of the Parastacidae (Crustacea: Astacoidea), and description of a new genus of Australian freshwater crayfishes. Australian Journal of Zoology, 20(4), 369-389.",
    trait_name == "microhabitat_shelter" ~ "Adamowicz, S. J., & Purvis, A. (2006). Macroevolution and extinction risk patterns in freshwater crayfish. Freshwater Crayfish, 15, 1-23; Riek, E. F. (1972). The phylogeny of the Parastacidae (Crustacea: Astacoidea), and description of a new genus of Australian freshwater crayfishes. Australian Journal of Zoology, 20(4), 369-389.",
    trait_name == "EOO" ~ "IUCN. (2010). IUCN Red List of threatened species. Available at: http://www.iucnredlist.org/")) # add secondary citations for the methods description


##-------------------------------##
## Combine the datasets & export ##
##-------------------------------##


bland_template <- b1_template %>%
  dplyr::bind_rows(b2_template, b3_template) # combine the three datasets mapped to the data template

write.csv(bland_template, "scripts/data.csv", row.names = FALSE) # export the data as a csv


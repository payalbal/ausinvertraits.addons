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

out_dir <- file.path(getwd(), "outputs")
dbout_dir <- file.path(out_dir, "other_dbs/Driscoll_2020")

## Load required package
library(tidyverse)

## Load the Driscoll_2020 dataset
d1 <- read_csv(file.path(getwd(), "Driscoll_2020", "data/Eyre_Peninsula_SA_beetles.csv"), show_col_types = FALSE)

## Load the data template
template <- read_csv(file.path(getwd(), "Driscoll_2020", "data/data_template.csv"), show_col_types = FALSE) 


########
# Questions for Don:
# what is the column 'size' (values 1, 2, 3)? There is also a column bodylength_mm
########


##---------------------------------------##
#### The Eyre Peninsula beetle dataset ####
##---------------------------------------##

## Drop introduced species
## Include total in study in measurement_remarks
## size = 1 & bodylenth_mm = NA == bodylenth_mm ??
## flightless, life_stage adult ( yes: dispersal syndrome == active_flight; no: dispersal syndrome == terrestrial_movement; )



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
    functional_role = trophicgroup,
    secondary_citation = Reference) %>% # change column names to match data_template column names
  dplyr::mutate(body_length = CL + AbdoLength) %>% # create new body length variable from the sum of carapace and abdomen length
  tidyr::separate_wider_delim(
    taxon_name,
    delim = " ",
    names = c("genus", "epithet"),
    cols_remove = FALSE) %>% # create new columns with genus and specific epithet, so that the Australian crayfish family name can be matched below
  dplyr::left_join(b_names, by = "genus") %>% # merge the crayfish name dataset so that the family name is added for Australian species
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
      .default = as.character(secondary_citation))) %>%  # remove notes related to museum specimens from the secondary citation column (so this column is only citations)
  dplyr::mutate(
    Data_Type = case_when(
      Data_Type == "Plate" ~ "Measurement taken from plate.",
      Data_Type == "Museum" ~ "Measurement made on museum specimen.",
      Data_Type == "Table" ~ "Measurement taken from table.",
      Data_Type == "Table mean" ~ "measurement taken from table mean.")) %>%  # describe where the data comes from
  dplyr::mutate(
    Types = case_when(
      Types == "Allotype" ~ "Measurement made on allotype.",
      Types == "Holotype" ~ "Measurement made on holotype.",
      Types == "Paratype" ~ "Measurement made on paratype.",
      Types == "Paratypes" ~ "Measurement made on paratypes.")) %>%  # describe what 'types' the measurements came from
  tidyr::unite(
    "measurement_remarks",
    sep = " ",
    c("Data_Type", "Types", "measurement_remarks1"),
    na.rm = TRUE,
    remove = FALSE) %>%  # combine information about measurement details into measurement_remarks
  dplyr::select(taxon_name, taxon_family, sex, measurement_remarks, secondary_citation, occipital_carapace_length, body_length)  %>%   # keep only necessary columns
  tidyr::pivot_longer(cols = occipital_carapace_length:body_length,
                      names_to = "trait_name",
                      values_to = "value")  # convert to long format so that the trait names and values are in two columns
  

## (2) Merge the allometry dataset with the IA data template, & occupy missing cells

b1_template <- template %>%
  dplyr::full_join(b1_mod) %>%  # merge with empty data_template
  dplyr::filter(if_any(everything(), ~ !is.na(.))) %>%  # remove rows with all NAs (from data template)
  dplyr::mutate(entity_type_tax = "species") %>%  # add entity_type_tax category
  dplyr::mutate(life_stage_generic = "adult") %>%  # add life_stage_generic category
  dplyr::mutate(entity_type = "individual") %>%  # add entity_type category
  dplyr::mutate(value_type = "raw") %>%  # add value_type category
  dplyr::mutate(unit_numeric = "mm") %>%  # add unit_numeric
  tidyr::drop_na(value) %>%  # remove rows with NA in the value column
  dplyr::mutate(value = as.character(value))  %>%  # make the value column a character so that both categorical and numerical values can occur in this column
  dplyr::mutate(replicates = 1) %>%  # add replicates (1 because these are measurements on individuals)
  dplyr::mutate(methods = "Measurements were obtained from species descriptions, museum plates, museum specimens, or field specimens.") %>% # add methods
  dplyr::mutate(source_key = "Bland_2017") %>%  # add source_key
  dplyr::mutate(source_doi = "doi: 10.1111/acv.12350") %>%  # add source_doi
  dplyr::mutate(source_citation = "Bland, L. M. (2017). Global correlates of extinction risk in freshwater crayfish. Animal Conservation, 20(6), 532-542.") %>%  # add source_citation
  dplyr::mutate(source_type = "article")  # add source_type


##-------------------------------------##
#### The life history traits dataset ####
##-------------------------------------##

## (1) Modify the dataset in preparation for the data template.

## (1a) First make two data frames for two complicated variables (female size at maturity and male minimum form 1 size)

## Female size at maturity data (to be merged later on).

b2_size_mat <- b2 %>%
  dplyr::select(Friendly_name, Size_maturity_females, Size_Mat_Type, Ref_Size_mat) %>%  # select needed columns
  dplyr::rename(
    taxon_name = Friendly_name,
    value = Size_maturity_females,
    trait_name = Size_Mat_Type,
    secondary_citation = Ref_Size_mat) %>%  # rename columns to match data template
  dplyr::filter(trait_name %in% c("OCL", "TL")) %>%  # keep only rows with occipital_carapace_length or body_length measurements
  dplyr::mutate(
    trait_name = case_when(
      trait_name == "OCL" ~ "occipital_carapace_length_mat",
      trait_name == "TL" ~ "body_length_mat")) %>%  # rename size at maturity categories with unique trait names so that details can be added later
  dplyr::select(taxon_name, trait_name, value, secondary_citation)  # select only needed columns


## Male form 1 size data (to be merged later on).

b2_size_form <- b2 %>%
  dplyr::select(Friendly_name, Min_size_form_I_males, Type_size_form_I, Ref_form_I_males) %>%   # select needed columns
  dplyr::rename(
    taxon_name = Friendly_name,
    value = Min_size_form_I_males,
    trait_name = Type_size_form_I,
    secondary_citation = Ref_form_I_males)  %>%  # rename columns to match data template
  dplyr::filter(trait_name %in% c("OCL", "TL")) %>%  # keep only rows with occipital_carapace_length or body_length measurements
  dplyr::mutate(
    trait_name = case_when(
      trait_name == "OCL" ~ "occipital_carapace_length_form",
      trait_name == "TL" ~ "body_length_form")) %>%  # rename form I size categories with unique trait names so that details can be added later
  dplyr::select(taxon_name, trait_name, value, secondary_citation)  # select only needed columns


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
  dplyr::select(
    taxon_name,
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
  tidyr::drop_na(value) %>% # remove rows with NA in the value column
  dplyr::bind_rows(b2_size_mat, b2_size_form) %>% # combine the female size at maturity and male form I size data
  tidyr::separate_wider_delim(
    taxon_name,
    delim = " ",
    names = c("genus", "epithet"),
    cols_remove = FALSE) %>% # create new columns with genus and specific epithet, so that the Australian crayfish family name can be matched below
  dplyr::left_join(b_names, by = "genus") %>% # merge the crayfish name dataset to get family names for the Australian species
  dplyr::select(taxon_name, taxon_family, trait_name, value, secondary_citation) # select only needed columns
  

## (2) Merge the life history dataset with the IA data template, & occupy missing cells.

b2_template <- template %>%
  dplyr::full_join(b2_mod) %>%  # merge with empty data_template
  dplyr::filter(if_any(everything(), ~ !is.na(.))) %>%  # remove rows with all NAs (from data template)
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
  dplyr::mutate(entity_type = "species") %>% # add entity_type for all rows
  dplyr::mutate(
    value_type = case_when(
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
  dplyr::mutate(
    measurement_remarks = case_when(
      trait_name == "body_length_mat" ~ "Female size at maturity.",
      trait_name == "occipital_carapace_length_mat" ~ "Female size at maturity.",
      trait_name == "body_length" ~ "Body lengths were collected from species descriptions and field guides. Maximum size was used as mean size is generally not available for crayfish species.",
      trait_name == "occipital_carapace_length" ~ "Occipital carapace lengths were collected from species descriptions and field guides. Maximum size was used as mean size is generally not available for crayfish species.")) %>%  # provide details of measurements
  dplyr::mutate(
    trait_name = case_when(
      trait_name == "body_length_form" ~ "body_length",
      trait_name == "body_length_mat" ~ "body_length",
      trait_name == "Egg_size" ~ "body_length",
      trait_name == "occipital_carapace_length_form" ~ "occipital_carapace_length",
      trait_name == "occipital_carapace_length_mat" ~ "occipital_carapace_length",
      .default = as.character(trait_name))) %>%  # rename traits
  dplyr::mutate(
    unit_numeric = case_when(
      trait_name == "body_length" ~ "mm",
      trait_name == "fecundity_per_reproductive_event" ~ "offspring",
      trait_name == "life_span" ~ "days",
      trait_name == "occipital_carapace_length" ~ "mm",
      trait_name == "time_to_maturity" ~ "days")) %>%  # specify units for numerical traits
  dplyr::mutate(value = as.character(value))  %>%  # make the value column a character
  dplyr::mutate(
    methods = case_when(
      trait_name == "body_length" ~ "Measurements were obtained from species descriptions, museum plates, museum specimens, or field specimens.",
      trait_name == "fecundity_per_reproductive_event" ~ "Maximum number of eggs taken from species descriptions, field guides, and museum specimens.",
      trait_name == "life_span" ~ "Life span was taken from species descriptions and field guides.",
      trait_name == "occipital_carapace_length" ~ "Measurements were obtained from species descriptions, museum plates, museum specimens, or field specimens.",
      trait_name == "time_to_maturity" ~ "Age at maturity was taken from species descriptions and field guides."))  %>%  # specify methods for each trait.
  dplyr::mutate(source_key = "Bland_2017") %>%  # add source_key
  dplyr::mutate(source_doi = "doi: 10.1111/acv.12350") %>%  # add source_doi
  dplyr::mutate(source_citation = "Bland, L. M. (2017). Global correlates of extinction risk in freshwater crayfish. Animal Conservation, 20(6), 532-542.") %>% # add source_citation
  dplyr::mutate(source_type = "article") %>%  # add source_type
  tibble::add_column(measurement_remarks2 = NA) %>% # create second temporary measurement_remarks column to capture information from the source_citation column that are not citations
  dplyr::mutate(
    measurement_remarks2 = case_when(
      secondary_citation == "Australia fisheries" ~ "Taken from Australia fisheries.",
      secondary_citation == "Australian Museum P11920" ~ "Taken from Australian Museum specimen P11920.",
      secondary_citation == "Australian Museum P11968" ~ "Taken from Australian Museum specimen P11968.",
      secondary_citation == "Australian Museum P34019" ~ "Taken from Australian Museum specimen P34019.",
      secondary_citation == "Australian Museum P34039" ~ "Taken from Australian Museum specimen P34039.",
      secondary_citation == "Australian Museum P34045" ~ "Taken from Australian Museum specimen P34045.",
      secondary_citation == "Australian Museum P34075" ~ "Taken from Australian Museum specimen P34075.",
      secondary_citation == "Australian Museum P84271" ~ "Taken from Australian Museum specimen P84271.",
      secondary_citation == "Clarke & Ashcroft" ~ "Taken from Clarke & Ashcroft (citation cannot be traced).",
      secondary_citation == "Flora and Fauna Guarantee Act 1988" ~ "Taken from Flora and Fauna Guarantee Act 1988.",
      secondary_citation == "http://www.arkive.org/giant-freshwater-crayfish/astacopsis-gouldi/" ~ "Taken from http://www.arkive.org/giant-freshwater-crayfish/astacopsis-gouldi/",
      secondary_citation == "Nat Hist" ~ "Taken from Natural History Museum (London) specimen.",
      secondary_citation == "NHM" ~ "Taken from Natural History Museum (London) specimen.",
      secondary_citation == "NHM 1927.4.29.6 Tyers river Gippsland Australia" ~ "Taken from Natural History Museum (London) specimen 1927.4.29.6 Tyers river Gippsland Australia.")) %>% # capture information from source_citation column that are not citations
  tidyr::unite(
    "measurement_remarks",
    sep = " ",
    c("measurement_remarks", "measurement_remarks2"),
    na.rm = TRUE,
    remove = FALSE) %>%  # unite information about measurement details into measurement_remarks
  dplyr::mutate(secondary_citation = case_when(
    secondary_citation %in% c("Australia fisheries", "Australian Museum P11920",
      "Australian Museum P11968", "Australian Museum P34019", "Australian Museum P34039",
      "Australian Museum P34045", "Australian Museum P34075", "Australian Museum P84271",
      "Based on OCL (OCL)", "Clarke & Ashcroft", "Flora and Fauna Guarantee Act 1988",
      "http://www.arkive.org/giant-freshwater-crayfish/astacopsis-gouldi/", "Nat Hist",
      "NHM", "NHM 1927.4.29.6 Tyers river Gippsland Australia") ~ NA,
    .default = as.character(secondary_citation))) %>% # remove information from the source_citation column that are not citations
  dplyr::select(-measurement_remarks2) # remove temporary measurement_remarks column

 
##-----------------------------##
#### The open access dataset ####
##-----------------------------##

## (1) Modify the dataset in preparation for the data template.

b3_mod <- b3 %>%
  dplyr::select(Binomial, Family, HabitatType, EOO:HPD) %>%   # select needed columns
  dplyr::rename(
    taxon_name = Binomial,
    taxon_family = Family,
    microhabitat_activity = HabitatType) %>%  # change column names to match data_template names
  dplyr::mutate(taxon_family = str_to_title(taxon_family)) %>%  # change family names to title case
  dplyr::mutate(Temp = paste("Mean values of extrinsic factors across species ranges were extracted. Mean annual temperature (degrees C):", Temp, sep = " ")) %>%
  dplyr::mutate(TempSeas = paste("Mean temperature seasonality (standard deviation):", TempSeas, sep = " ")) %>%
  dplyr::mutate(Prec = paste("Mean annual precipitation (mm):", Prec, sep = " ")) %>%
  dplyr::mutate(PrecSeas = paste("Mean precipitation seasonality (coefficient of variation):", PrecSeas, sep = " ")) %>% 
  dplyr::mutate(ElevMin = paste("Minimum elevation (m):", ElevMin, sep = " ")) %>% 
  dplyr::mutate(Consumption = paste("Mean water consumption (CDF-standardized water consumption through irrigation, thermoelectric, and manufacturing industries divided by contemporary discharge):", Consumption, sep = " ")) %>% 
  dplyr::mutate(Fragment = paste("Mean river fragmentation (CDF-standardized proportion of each drainage basin that is accessible from a given grid cell):", Fragment, sep = " ")) %>% 
  dplyr::mutate(Mercury = paste("Mean mercury deposition (CDF-standardized difference between present-day and pre-industrial Hg deposition):", Mercury, sep = " ")) %>% 
  dplyr::mutate(Pesticide = paste("Mean pesticide loading (CDF-standardized country-based pesticide application to croplands):", Pesticide, sep = " ")) %>% 
  dplyr::mutate(Sed = paste("Mean sediment loading (CDF-standardized total suspended solids):", Sed, sep = " ")) %>% 
  dplyr::mutate(Cropland = paste("Mean cropland coverage (CDF-standardized fraction of land devoted to growing crops):", Cropland, sep = " ")) %>% 
  dplyr::mutate(Livestock = paste("Livestock density:", Livestock, sep = " ")) %>% 
  dplyr::mutate(HPD = paste("Mean human population density (People per km2, year 2000):", HPD, sep = " ")) %>% 
  tidyr::unite(
    "site_description",
    sep = ", ",
    c("Temp", "TempSeas", "Prec", "PrecSeas", "ElevMin", "Consumption", "Fragment", "Mercury", "Pesticide", "Sed", "Cropland", "Livestock", "HPD"),
    na.rm = TRUE,
    remove = FALSE) %>%  # combine information on environmental and threat variables into site_description column
  dplyr::select(taxon_name, taxon_family, site_description, microhabitat_activity, EOO)  %>%   # keep only necessary columns
  dplyr::mutate(microhabitat_shelter = microhabitat_activity) %>%  # duplicate the microhabitat_activity column and call microhabitat_shelter
  tidyr::pivot_longer(cols = microhabitat_activity:microhabitat_shelter,
                      names_to = "trait_name",
                      values_to = "value") %>%  # convert to long format so that all trait names and values are in two columns
  tidyr::drop_na(value) # remove rows with NA in the value column


## (2) Merge the open access dataset with the IA data template, & occupy missing cells.

b3_template <- template %>%
  dplyr::full_join(b3_mod) %>%  # merge with empty data_template
  dplyr::filter(if_any(everything(), ~ !is.na(.))) %>%  # remove rows with all NAs (from data template)
  dplyr::mutate(entity_type_tax = "species") %>%  # add entity_type_tax for all rows
  dplyr::mutate(life_stage_generic = "all") %>%  # add life_stage_generic for all rows
  dplyr::mutate(sex = "all") %>% # add sex category for all rows
  dplyr::mutate(entity_type = "species") %>% # add entity_type for all rows
  dplyr::mutate(value_type = case_when(
    trait_name == "EOO" ~ "raw")) %>%  # specify value_type for numerical trait
  dplyr::mutate(unit_numeric = case_when(
    trait_name == "EOO" ~ "km2")) %>%  # specify unit_numeric for numerical trait
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
  dplyr::mutate(site_description = case_when(
    trait_name == "microhabitat_activity" ~ NA,
    trait_name == "microhabitat_shelter" ~ NA,
    .default = as.character(site_description))) %>%  # retain site_description only for EOO
  dplyr::mutate(source_doi = "doi: 10.1111/acv.12350") %>% # add source_doi
  dplyr::mutate(source_citation = "Bland, L. M. (2017). Global correlates of extinction risk in freshwater crayfish. Animal Conservation, 20(6), 532-542.") %>% # add source_citation
  dplyr::mutate(source_type = "article")  %>% # add source_type
  dplyr::mutate(secondary_citation = case_when(
    trait_name == "microhabitat_activity" ~ "Adamowicz, S. J., & Purvis, A. (2006). Macroevolution and extinction risk patterns in freshwater crayfish. Freshwater Crayfish, 15, 1-23; Riek, E. F. (1972). The phylogeny of the Parastacidae (Crustacea: Astacoidea), and description of a new genus of Australian freshwater crayfishes. Australian Journal of Zoology, 20(4), 369-389.",
    trait_name == "microhabitat_shelter" ~ "Adamowicz, S. J., & Purvis, A. (2006). Macroevolution and extinction risk patterns in freshwater crayfish. Freshwater Crayfish, 15, 1-23; Riek, E. F. (1972). The phylogeny of the Parastacidae (Crustacea: Astacoidea), and description of a new genus of Australian freshwater crayfishes. Australian Journal of Zoology, 20(4), 369-389.",
    trait_name == "EOO" ~ "IUCN. (2010). IUCN Red List of threatened species. Available at: http://www.iucnredlist.org/")) # add secondary citations for the methods description


##--------------------------------------------------------------------##
#### Combine the datasets, subset only Australian species, & export ####
##--------------------------------------------------------------------##

## Read in the file with a list of Australian taxa and corrections between the Bland_2017 and AFD taxon names.
## bland_taxa_list_updated.csv was created in the associated script Bland_2017_taxa_list.R and then manually modified.

aus_names <- read_csv(file.path(getwd(), "data", "Bland_2017/bland_taxa_list_updated.csv"), show_col_types = FALSE)

## Combine the three datasets, subset Australian species, correct
bland_template <- b1_template %>%
  dplyr::bind_rows(b2_template, b3_template) %>%
  dplyr::full_join(aus_names, by = "taxon_name") %>% 
  dplyr::select(-taxname_source.x, -taxon_family.y) %>% # remove repeat columns from merging (retain taxname_source from the aus_names dataset, retain taxon_family from bland_template)
  dplyr::rename(
    taxon_family = taxon_family.x,
    taxname_source = taxname_source.y) %>%  # rename columns to remove x and y
  dplyr::mutate(entity_type_tax = case_when(
    notes == "subspecies" ~ "subspecies",
    .default = as.character(entity_type_tax))) %>% # specify subspecies for entity_type_tax
  dplyr::filter(taxon_family %in% "Parastacidae", .preserve = FALSE) %>%  # include only taxa in the Parastacidae
  dplyr::filter(!is.na(taxname_source)) %>%  # exclude non-Australian species in the Parastacidae
  dplyr::mutate(taxon_name_original = case_when(
    taxon_name == "Cherax albidus" ~ "Cherax albidus",
    taxon_name == "Geocharax gracilis" ~ "Geocharax gracilis",
    taxon_name == "Cherax preisii" ~ "Cherax preisii",
    taxon_name == "Engaeus hemicerratulus" ~ "Engaeus hemicerratulus",
    taxon_name == "Euastacus australiensis" ~ "Euastacus australiensis",
    taxon_name == "Euastacus balanesis" ~ "Euastacus balanesis",
    taxon_name == "Euastacus bidawalis" ~ "Euastacus bidawalis",
    taxon_name == "Euastacus wiowuru" ~ "Euastacus wiowuru",
    taxon_name == "Euastacus yarreansis" ~ "Euastacus yarreansis")) %>% # add original name to taxon_name_original in cases where names are different to current AFD name
  dplyr::mutate(taxname_issues_description = case_when(
    taxon_name == "Cherax preisii" ~ "taxon_name_original name misspelled in source, Cherax preissii misspelled as Cherax preisii",
    taxon_name == "Engaeus hemicerratulus" ~ "taxon_name_original name misspelled in source, Engaeus hemicirratulus misspelled as Engaeus hemicerratulus",
    taxon_name == "Euastacus australiensis" ~ "taxon_name_original name misspelled in source, Euastacus australasiensis misspelled as Euastacus australiensis",
    taxon_name == "Euastacus balanesis" ~ "taxon_name_original name misspelled in source, Euastacus balanensis misspelled as Euastacus balanesis",
    taxon_name == "Euastacus bidawalis" ~ "taxon_name_original name misspelled in source, Euastacus bidawalus misspelled as Euastacus bidawalis",
    taxon_name == "Euastacus serratus" ~ "taxon_name_original cannot be traced, name has been synonymised into three different species so current name is unknown",
    taxon_name == "Euastacus wiowuru" ~ "taxon_name_original name misspelled in source, Euastacus woiwuru misspelled as Euastacus wiowuru",
    taxon_name == "Euastacus yarreansis" ~ "taxon_name_original name misspelled in source, Euastacus yarraensis misspelled as Euastacus yarreansis")) %>% # describe when names were misspelled
  dplyr::mutate(taxon_name = case_when(
    taxon_name == "Cherax albidus" ~ "Cherax destructor albidus",
    taxon_name == "Geocharax gracilis" ~ "Geocharax tasmanicus",
    taxon_name == "Cherax preisii" ~ "Cherax preissii",
    taxon_name == "Engaeus hemicerratulus" ~ "Engaeus hemicirratulus",
    taxon_name == "Euastacus australiensis" ~ "Euastacus australasiensis",
    taxon_name == "Euastacus balanesis" ~ "Euastacus balanensis",
    taxon_name == "Euastacus bidawalis" ~ "Euastacus bidawalus",
    taxon_name == "Euastacus wiowuru" ~ "Euastacus woiwuru",
    taxon_name == "Euastacus yarreansis" ~ "Euastacus yarraensis",
    .default = as.character(taxon_name))) %>% # correct names due subspecies, synonyms, and misspellings
  dplyr::select(-updated_taxon_name, -notes) %>%
  dplyr::relocate(taxname_source, .after = taxon_name_original) %>%
  readr::write_csv(file.path(dbout_dir, "data.csv")) # save final dataset


place_118147 <- read_csv("~/austraits files/iNat_Yosemite/data/Australian_species_downloads/place_118147.csv")
place_10293 <- read_csv("~/austraits files/iNat_Yosemite/data/Australian_species_downloads/place_10293.csv")
place_10287 <- read_csv("~/austraits files/iNat_Yosemite/data/Australian_species_downloads/place_10287.csv")
place_7616 <- read_csv("~/austraits files/iNat_Yosemite/data/Australian_species_downloads/place_7616.csv")
place_7333 <- read_csv("~/austraits files/iNat_Yosemite/data/Australian_species_downloads/place_7333.csv")
place_6744 <- read_csv("~/austraits files/iNat_Yosemite/data/Australian_species_downloads/place_6744.csv")
taxa <- read_csv("~/austraits files/iNat_Yosemite/data/inaturalist-taxonomy.dwca/taxa.csv")

all_AU_species <- place_118147 %>%
  bind_rows(place_10293, place_10287, place_7616, place_7333, place_6744) %>%
  select(-`Rank...1`) %>%
  rename(scientificName = Name, commonName = `Common name`, taxonRank = `Rank...5`) %>%
  group_by(ID) %>%
  summarise(
    scientificName = first(scientificName),
    commonName = first(commonName),
    taxonRank = first(taxonRank),
    Observations = sum(Observations)
    ) %>%
  ungroup() %>%
  distinct() %>%
  arrange(-Observations) %>%
  write_csv("~/austraits files/iNat_Yosemite/data/Australian_species_downloads/all_AU_species.csv")

invert_phyla <- taxa %>%
  filter(is.na(class),!is.na(phylum), kingdom == "Animalia", taxonRank == "phylum", phylum != "Chordata") %>% distinct(phylum, taxonID) %>%
  mutate(taxonID = str_replace(taxonID, "https://www.inaturalist.org/taxa/",""))

iNat_inverts_in_Australia <- taxa %>%
  filter(phylum %in% invert_phyla$phylum) %>% 
  filter(scientificName %in% all_AU_species$scientificName)

invert_phyla <- iNat_inverts_in_Australia %>% 
  dplyr::distinct(phylum)

AU_phyla <- taxa %>% 
  filter(!is.na(phylum), is.na(class), is.na(order), is.na(family), is.na(genus), is.na(specificEpithet), is.na(infraspecificEpithet)) %>%
  filter(scientificName %in% invert_phyla$phylum)

invert_classes <- iNat_inverts_in_Australia %>% 
  dplyr::distinct(class)

AU_classes <- taxa %>% 
  filter(!is.na(class), is.na(order), is.na(family), is.na(genus), is.na(specificEpithet), is.na(infraspecificEpithet)) %>%
  filter(class %in% invert_classes$class)

invert_orders <- iNat_inverts_in_Australia %>% 
  dplyr::distinct(order)

AU_orders <- taxa %>% 
  filter(!is.na(order), is.na(family), is.na(genus), is.na(specificEpithet), is.na(infraspecificEpithet)) %>%
  filter(order %in% invert_orders$order)

invert_families <- iNat_inverts_in_Australia %>% 
  dplyr::distinct(family)

AU_families <- taxa %>% 
  filter(!is.na(family), is.na(genus), is.na(specificEpithet), is.na(infraspecificEpithet)) %>%
  filter(family %in% invert_families$family)

invert_genera <- iNat_inverts_in_Australia %>% 
  dplyr::distinct(genus)

AU_genera <- taxa %>% 
  filter(!is.na(genus), is.na(specificEpithet), is.na(infraspecificEpithet)) %>%
  filter(genus %in% invert_genera$genus)

iNat_inverts_in_Australia <- iNat_inverts_in_Australia %>% 
  bind_rows(AU_genera, AU_families, AU_orders, AU_classes, AU_phyla) %>%
  distinct(scientificName, .keep_all = TRUE)

iNat_inverts_in_Australia %>%
  write_csv("~/GitHub/ausinvertraits.addons/data/iNat_inverts_in_Australia.csv")

#playing
# invert_phyla %>%
#   mutate(taxonID = paste0(taxonID, collapse = ",")) %>%
#   distinct(taxonID)

# All phyla

# 47115,47120,47491,47534,47549,48051,48824,51280,51508,51836,52319,54960,63142,68104,68235,122158,124337,126917,151826,151827,151828,151829,151830,151831,151832,151833,151836,151837,151838,211191,774624,884506

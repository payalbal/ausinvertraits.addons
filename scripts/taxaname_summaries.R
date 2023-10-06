## AFD gaps



## ----------------------------------------------- ## 
## Set working environment ####
## ----------------------------------------------- ##
# .rs.restartR()
rm(list = ls())
gc()
# system("ps")
# system("pkill -f R")

## Packages
x <- c('data.table')
lapply(x, require, character.only = TRUE)
rm(x)


## Paths
# inverts_dir <- "/Volumes/6300-PAYALB/uom_data/ausinvertraits.addons_data"
# inverts_dir <- "/tempdata/research-cifs/6300-payalb/uom_data/ausinvertraits.addons_data"
output_dir <- "./outputs"




## ----------------------------------------- ##
## Cleaned AFD checklist summary ####
## ----------------------------------------- ##

afd <- fread(file.path(output_dir, "afd_May2023_clean.csv"))
afd <- setDT(afd, key = c("FULL_NAME", "COMPLETE_NAME", "CONCEPT_GUID"))
names(afd)

## By order
length(unique(afd$ORDER))
afd.orders <- afd[, .N, ORDER]
message(cat("Number of unique orders in cleaned AFD: "),
        nrow(afd.orders))
write.csv(afd.orders, file = file.path(output_dir, "afd_by_order.csv"), row.names = FALSE)

count1 <- afd.orders[which(afd.orders$N <= 1)]
message(cat("Number of orders with 1 listed species in cleaned AFD: "),
        nrow(count1))
write.csv(count1, file = file.path(output_dir, "order_count0_1.csv"), row.names = FALSE)

count2 <- afd.orders[which(afd.orders$N > 1 & afd.orders$N <= 5)]
message(cat("Number of orders with 1-5 listed species in cleaned AFD: "),
        nrow(count2))
write.csv(count2, file = file.path(output_dir, "order_count2_5.csv"), row.names = FALSE)

count3 <- afd.orders[which(afd.orders$N <= 10) , ]
message(cat("Number of orders with less than or equal to 10 listed species in cleaned AFD: "),
        nrow(count3))
write.csv(count3, file = file.path(output_dir, "order_count10.csv"), row.names = FALSE)


## By family
length(unique(afd$FAMILY))
afd.family <- afd[, .N, FAMILY]
message(cat("Number of unique families in cleaned AFD: "),
        nrow(afd.family))
write.csv(afd.family, file = file.path(output_dir, "afd_by_family.csv"), row.names = FALSE)


count1 <- afd.family[which(afd.family$N <= 1)]
message(cat("Number of families with 1 listed species in cleaned AFD: "),
        nrow(count1))
write.csv(count1, file = file.path(output_dir, "family_count0_1.csv"), row.names = FALSE)

count2 <- afd.family[which(afd.family$N > 1 & afd.family$N <= 5)]
message(cat("Number of families with 1-5 listed species in cleaned AFD: "),
        nrow(count2))
write.csv(count2, file = file.path(output_dir, "family_count2_5.csv"), row.names = FALSE)

count3 <- afd.family[which(afd.family$N <= 10) , ]
message(cat("Number of families with less than or equal to 10 listed species in cleaned AFD: "),
        nrow(count3))
write.csv(count3, file = file.path(output_dir, "family_count10.csv"), row.names = FALSE)




## ----------------------------------------- ##
## AusInverTraits database summary ####
## ----------------------------------------- ##
ausinverts <- readRDS("/Users/payalb/Dropbox/Projects/IA_InverTraits/Rprojects/ausinvertraits.build/outputs/ausinvertraits.rds")

ausinverts$taxa
length(unique(ausinverts$traits$taxon_name))

db <- setDT(ausinverts$traits, key = "taxon_name")
db[taxon_name == "unresolved"]

message(cat("Number of unresolved species in db: "),
        nrow(db[taxon_name == "unresolved"]))




## ----------------------------------------- ##
## AusInverTraits datatable summary ####
## ----------------------------------------- ##
data_dir = "/Users/payalb/Dropbox/Projects/IA_InverTraits/Rprojects/ausinvertraits.build/data"

## Read in raw data files ####
csvfiles <- list.files(data_dir, pattern = "data.csv",
                       full.names = TRUE, all.files = TRUE, recursive = TRUE)

# csvfiles <- readRDS("./outputs/testfiles.rds")
message(cat("Number of input raw data files: "),
        length(csvfiles))

## Merge csv files ####
out <- do.call("bind_rows", c(lapply(csvfiles, read_csv, col_types = cols( .default = col_character()))))

# ## Or using dplyr
# out <- csvfiles %>% 
# lapply(read_csv,  col_types = cols( .default = col_character())) %>% 
#   bind_rows 

## >> Convert to data table
trait.data <- as.data.table(out)

## >> Remove attributes
attributes(trait.data)$spec <- NULL
str(trait.data)

## >> Remove rows with NAs in all columns
trait.data[rowSums(is.na(trait.data)) == ncol(trait.data), ]
nrow(trait.data) - nrow(trait.data[rowSums(is.na(trait.data)) == ncol(trait.data), ])

trait.data <- trait.data[rowSums(is.na(trait.data)) != ncol(trait.data), ]

## Change column data types ####
traitnames <- names(trait.data)
trait_type <- c("character",	"character", "character", "character",	"character", "character",	"character", "character", "character",	"character",	"character", "character", "character",	"character", "numeric",	"character", "numeric", "numeric", "character", "character", "numeric", "numeric", "IDate", "character",	"character",	"character", "character", "character",	"character", "character", "character", "character",	"character", "character", "character", "character", "character", "character")

dt_dummy <- as.data.table(cbind(traitnames, trait_type))

## >> Numeric columns
trait.data[, dt_dummy[trait_type == "numeric"]$traitnames := lapply(.SD, as.numeric), .SDcols = dt_dummy[trait_type == "numeric"]$traitnames]

## >> $$  FOR LATER - Date column ####
trait.data[, dt_dummy[trait_type == "IDate"]$traitnames := lapply(.SD, as.IDate), , .SDcols = dt_dummy[trait_type == "IDate"]$traitnames]

# x <- lubridate::as_date(trait.data$site_date_of_visit)

## Data exploration + fix issues ####
dim(trait.data)

## >> Taxa stats
message(cat("Number of unique species (taxon_name): "),
        length(unique(trait.data$taxon_name)))

message(cat("Number of unique families (taxon_family): "),
        length(unique(trait.data$taxon_family)))

message(cat("List of unique families (taxon_family): "))
unique(trait.data$taxon_family)

write.csv(unique(trait.data$taxon_family),
          file = file.path(output_dir, "ausinverts_families.csv"), 
          row.names = FALSE)


## >> NA in tax_family
unique(trait.data[is.na(taxon_family)]$source_key) 
## "Hodkinson_1974" "Hollis_2004": records for taxrank higher than family
unique(trait.data[is.na(taxon_family)]$entity_type_tax)
unique(trait.data[is.na(taxon_family)]$entity_type)

## $$ >> >> TO FIX: NA in taxon_name ####
nrow(trait.data[is.na(taxon_name)])

## >> >> Morphospecies identified as 'NA'
nrow(trait.data[is.na(taxon_name) & is.na(taxname_source)])
length(unique(trait.data[is.na(taxon_name) & is.na(taxname_source)]$taxon_name_original))
trait.data[is.na(taxon_name) & is.na(taxname_source)]$taxon_name_original

x <- trait.data[is.na(taxon_name) & is.na(taxname_source)][, .(taxon_name_original,taxname_issues_description)]
x <- unique(x[,.(taxon_name_original,taxname_issues_description)])

message(cat("Number of morphospecies identified as NA in db: "),
        nrow(x))

write.csv(x, file = file.path(output_dir, "taxname_original_NA.csv"),
          row.names = FALSE)

unique(x$taxname_issues_description)

write.csv(unique(x$taxname_issues_description), 
          file = file.path(output_dir, "taxname_original_NA_reasons.csv"),
          row.names = FALSE)

## >> >> Name not found in checklists recorded as 'not_found'
nrow(trait.data[is.na(taxon_name) & taxname_source == "not_found"])
trait.data[is.na(taxon_name) & taxname_source == "not_found"]$taxon_name_original

x <- trait.data[is.na(taxon_name) & taxname_source == "not_found"][, .(taxon_name_original,taxname_issues_description)]
x <- unique(x[,.(taxon_name_original,taxname_issues_description)])

message(cat("Number of species not found in any checklist: "),
        nrow(x))

write.csv(x, file = file.path(output_dir, "taxname_original_notfound.csv"),
          row.names = FALSE)

unique(x$taxname_issues_description)
write.csv(unique(x$taxname_issues_description), 
          file = file.path(output_dir, "taxname_original_notfound_reasons.csv"),
          row.names = FALSE)








counts <- ala_dat[,.N,by = scientificName]
nrow(counts) == length(ala_species)
write.csv(counts, file = file.path(output_dir, "datacounts_ALAspecies.csv"), row.names = FALSE)


## because we have additional species in ALA compared to AFD checklist
## see names3.csv and names2.csv

## List for ALA mapping (Oct 2020)
countMTE5 <- counts[which(counts$N >= 5)]
countMTE5 <- countMTE5[order(N)]
range(countMTE5$N)
write.csv(countMTE5, file = file.path(output_dir, "countMTE5.csv"), row.names = FALSE)




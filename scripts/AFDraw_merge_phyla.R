# ---
# Merge raw AFD data
# author: Payal Bal
# notes: 
#   Data obtained from AFD, Jan 2023 (contact: Matthew Lockett)
#   See AFD_invert_phyla_JM_Dec2022.xlsx for selected invertebrate phyla/sub-phyla
# ---


## Set working environment ####
rm(list = ls())
gc()
# system("ps")
# system("pkill -f R")

## Paths
inverts_dir <- "/Volumes/6300-PAYALB/uom_data/inverts_data"
outdir <- file.path(invT_dir, "outputs")



## Load data and subset to relevant Phyla/Sub-phyla ####
afd <- data.table::fread(file.path(inverts_dir, "ANIMALIA Jan 2023.csv"))
sort(unique(afd$PHYLUM))

temp <- afd[afd$SUBPHYLUM %in% c(toupper("Cephalochordata"), toupper("Tunicata"), toupper("Urochordata"))]

dim(afd)
dim(afd[afd$PHYLUM != "CHORDATA"])
dim(afd[afd$PHYLUM == "CHORDATA"])

afd <- afd[afd$PHYLUM != "CHORDATA"]; dim(afd)
afd <- rbind(afd,temp); dim(afd)

## Checks
sum(is.na(afd$PHYLUM))
sum(is.na(afd$FULL_NAME))


## Save merged data ####
setorder(afd, PHYLUM)
write.csv(afd, file = file.path(outdir, "afd_Jan2023.csv"), 
          row.names = FALSE)

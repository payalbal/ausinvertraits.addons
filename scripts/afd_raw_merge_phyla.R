## AFD checklist update: Jan 2023 
## See email chain: query: AFD species list download (contact: Matthew Lockett, AFD)

invT_dir <- "/Volumes/6300-PAYALB/uom_data/inverts_data"
output_dir <- file.path(invT_dir, "outputs")

afd <- data.table::fread("/Volumes/6300-PAYALB/uom_data/inverts_data/AFD/ANIMALIA Jan 2023.csv")
sort(unique(afd$PHYLUM))

temp <- afd[afd$SUBPHYLUM %in% c(toupper("Cephalochordata"), toupper("Tunicata"), toupper("Urochordata"))]

dim(afd)
dim(afd[afd$PHYLUM != "CHORDATA"])
dim(afd[afd$PHYLUM == "CHORDATA"])

afd <- afd[afd$PHYLUM != "CHORDATA"]; dim(afd)
afd <- rbind(afd,temp); dim(afd)

sum(is.na(afd$PHYLUM))
sum(is.na(afd$FULL_NAME))

setorder(afd, PHYLUM)
write.csv(afd, file = file.path(output_dir, "afd_Jan2023.csv"), 
          row.names = FALSE)

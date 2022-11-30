

## Set working environment ####
rm(list = ls())
gc()
# system("ps")
# system("pkill -f R")

x <- c('data.table', 'dplyr', 
       'galah', 'ALA4R',
       'httr', 'assertthat',
       'RCurl',
       'parallel', "future", "future.apply")
lapply(x, require, character.only = TRUE)
rm(x)

galah_config(atlas = "Australia")
galah_config(email = "bal.payal@gmail.com")

## Server paths
invert_dir = "/tempdata/research-cifs/6300-payalb/uom_data/inverts_data"
ala_dir <- dir <-  file.path(invert_dir, "data_ALA")
if(!dir.exists(dir)) {dir.create(dir)} else {print("directory already exists")}

output_dir <- dir <- file.path(inverts_dir, "outputs")
if(!dir.exists(dir)) {dir.create(dir)} else {print("directory already exists")}

## Functions
source("/tempdata/workdir/aus-ppms_data/scripts/download_ALAtax_galah.R")
'%!in%' <- function(x,y)!('%in%'(x,y))


  # ## Load AFD taxonomic checklist ####
  # afd_taxonomy <- fread("/data/research-cifs/6300-payalb/uom_data/nesp_bugs_data/outputs/afd_species_clean.csv")
  # afd_taxon <- unique(afd_taxonomy$PHYLUM)
  # length(afd_taxon)
  # 
  # ## Get ALA counts (by phylum) using ALA4R ####
  # nodatalog <- file.path(output_dir, "ALA_invert_phylum_nodata_log.txt")
  # writeLines(c("species0"), nodatalog)
  # 
  # taxon.counts <- lapply(afd_taxon,
  #                        function(x){
  #                          tmp <- tryCatch(expr = get_ala_taxondata(x,
  #                                                                   get_counts_only = TRUE,
  #                                                                   specimens_only = TRUE,
  #                                                                   dst = ala_dir),
  #                                          error = function(e){ 
  #                                            print(paste("\nNot run: no records for", x))
  #                                            
  #                                            cat(paste(x, "\n"),
  #                                                file = nodatalog, 
  #                                                append = T)
  #                                          })
  #                        })
  # 
  # taxon.counts <- as.data.frame(taxon.counts)
  # colnames(taxon.counts) <- afd_taxon
  # write.csv(taxon.counts, file.path(output_dir, "ALA_invert_phylum_counts.csv"))


## Load taxon list ####
taxlist <- fread(file.path(invert_dir, "traits_taxa.csv"))
taxlist[, c("date_added", "date_edited", "plant_host_species") := NULL]

## Replace empty cells ("") with NAs
taxlist <- taxlist[, lapply(.SD, function(x) replace(x, which(x==""), NA))]

## Create taxa list for download
temp <- c()
taxnames <- rev(names(taxlist)[-1])

for(i in 1:nrow(taxlist)){
  for(taxname in taxnames){
    if(is.na(taxlist[i,..taxname])) next
    temp <- c(temp, taxlist[i,..taxname])
  }
}

splist <- data.table()
splist$taxclass <- names(temp)
splist$taxname <- as.vector(unlist(temp))
rm(temp)

write.csv(splist, file.path(output_dir, "taxlist_ALAdownload.csv"), 
          row.names = FALSE)

## Get ALA counts using galah calls
f <- galah_filter(profile = "ALA")
                  # , year >= 1950 & year <= 2022)
y <- lapply(seq_along(splist$taxname), function(x){galah_call() |> galah_identify(splist$taxname[x]) |> atlas_counts(filter = f, type = "record")})

## Problems with Homonymns: In biology, a homonym is a name for a taxon that is identical in spelling to another such name, that belongs to a different taxon

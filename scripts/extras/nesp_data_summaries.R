## NESP data summaries and species lists


## Set working environment ####
## _______________________________________________

rm(list = ls())
gc()
# system("ps")
# system("pkill -f R")

x <- c("data.table", "rje", "stringr", 
       "sp", "raster")
lapply(x, require, character.only = TRUE)
rm(x)


## Server paths
bugs_dir = "/tempdata/research-cifs/uom_data/nesp_bugs_data"
iadir = "/tempdata/research-cifs/uom_data/IA_firetraits_data"
output_dir = file.path(iadir, "outputs")

## Data summary ####
data <- fread(file.path(bugs_dir, "outputs", "data_ALAnonALA_wgs84_corrected.csv"))

## Year range
range(data$year, na.rm = TRUE)
x <- data[, .N, year]
setorder(x, year)
write.csv(x, file = file.path(output_dir, "nespdata_byyear.csv"), 
          row.names = FALSE)

## Pre 1990 species
datsub <- data[year < 1990]; dim(dat_sub)
message(cat("Number of species with pre-1990 records:", 
            length(unique(datsub$spfile))))

datsub2 <- data[year >= 1990]; dim(datsub2)
sub2_splist <- datsub2$spfile

message(cat("Number of species with ONLY pre-1990 records:", 
            sum(!unique(datsub$spfile) %in% unique(datsub2$spfile))))

temp <- datsub[, .N, spfile]


## >> Add column to indicate if species has >=1990 records
temp$data_from_1990 <- rep(NA, nrow(temp))
x <- unique(datsub$spfile) [unique(datsub$spfile) %in% unique(datsub2$spfile)]
temp[spfile %in% x]$data_from_1990 = 1
temp[is.na(data_from_1990)]$data_from_1990 = 0
setorder(temp, data_from_1990)

## >> Add number of records
temp$N_post1990 <- numeric()
y <- datsub2[, .N, spfile]

  ## Check all species are equal
  all(temp[data_from_1990 == TRUE]$spfile == y[spfile %in% temp[data_from_1990 == TRUE]$spfile]$spfile)

temp[data_from_1990 == TRUE]$N_post1990 = y[spfile %in% temp[data_from_1990 == TRUE]$spfile]$N
names(temp)[2] <- "N_pre_1990"
temp <- temp[, c(1,3,2,4)]
write.csv(temp, file = file.path(output_dir, "nespdata_pre1990_species.csv"), 
          row.names = FALSE)



## Species list based on number of occurrence points
counts <- data[, .N, spfile]

count1 <- counts[which(counts$N == 1 | counts$N == 2)]
message(cat("Number of species with 1 or 2 records in NESP data: "),
        nrow(count1))
write.csv(count1, file.path(output_dir, "spcounts_1or2.csv"), 
          row.names = FALSE)

countLTE20 <- counts[which(counts$N > 2 & counts$N < 20) , ]
message(cat("Number of species with more than 2 and less than 20 records in NESP data: "),
        nrow(countLTE20))
write.csv(countLTE20, file.path(output_dir, "spcounts_3to19.csv"), 
          row.names = FALSE)

countMT20 <- counts[which(counts$N >= 20)]
message(cat("Number of species with more than or equal to 20 records in NESP data: "),
        nrow(countMT20))
write.csv(countMT20, file.path(output_dir, "spcounts_20ormore.csv"), 
          row.names = FALSE)


plot(density(counts$N))
plot(density(counts[N > 20 & N < 200, N]))



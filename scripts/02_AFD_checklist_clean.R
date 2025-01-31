## AFD cleaning
## Authors: Payal Bal, Elizabeth Wenk




## ----------------------------------------------- ## 
## Set working environment ####
## ----------------------------------------------- ##
# .rs.restartR()
rm(list = ls())
gc()
# system("ps")
# system("pkill -f R")

## Packages
x <- c('dplyr', 'janitor', 'stringr', 'tidyverse', 'stringr', 
       'data.table', 'usethis', 'devtools')
lapply(x, require, character.only = TRUE)
rm(x)

## Functions 
source(file.path(getwd(),"scripts/remove_improper_names_v2.R"))

## Paths
inverts_dir <- "/Volumes/6300-PAYALB/uom_data/ausinvertraits.addons_data"
# inverts_dir <- "/tempdata/research-cifs/6300-payalb/uom_data/ausinvertraits.addons_data"
outdir <- file.path(inverts_dir, "outputs")





## ----------------------------------------------- ##
## Load Australian Faunal Directory taxonomy ####
## ----------------------------------------------- ##
afd_data <- fread(file.path(outdir, "afd_Jan2023.csv"))

names(afd_data)
sum(is.na(afd_data$FULL_NAME))
length(unique(afd_data$FULL_NAME))
nrow(afd_data)
nrow(afd_data) - length(unique(afd_data$FULL_NAME)) == sum(duplicated(afd_data$FULL_NAME))





## ----------------------------------------------- ##
## Remove improper names ####
## ----------------------------------------------- ##
species_record <- remove_improper_names(name_vector = afd_data$FULL_NAME,
                                        allow.subgenus = TRUE,
                                        allow.higher.taxa = TRUE) 

## Check if there are any NAs in updated list
is.na(species_record$updated_list) |> table()


## Check number of words in species names
sp_words <- sapply(strsplit(as.character(species_record$updated_list), " "), length)
table(sp_words)
species_record$updated_list[sp_words == 4]


## Filter records based on updated list
afd_data <- afd_data[which(afd_data$FULL_NAME %in% species_record$updated_list),]


## Remove double and single quotes from names
grep("\"",afd_data$FULL_NAME, value = TRUE)
writeLines(grep("\"",afd_data$FULL_NAME, value = TRUE)[1])
x <- grep("\"",afd_data$FULL_NAME, value = TRUE)
y <- gsub("\"","",x)
afd_data[FULL_NAME %in% x]$FULL_NAME <- y
grep("\"",afd_data$FULL_NAME, value = TRUE)

grep("\'",afd_data$FULL_NAME, value = TRUE)
x <- grep("\'",afd_data$FULL_NAME, value = TRUE)
y <- gsub("\'","",x)
afd_data[FULL_NAME %in% x]$FULL_NAME <- y
grep("\'",afd_data$FULL_NAME, value = TRUE)

setDT(afd_data, key = 'FULL_NAME')
length(unique(afd_data$FULL_NAME)); nrow(afd_data)





## ----------------------------------------------- ##
## Identify duplicates ####
## ----------------------------------------------- ##

## List duplicates comparing all columns
nrow(afd_data[duplicated(afd_data),] )


## Duplicates in FULL_NAME (excluding first appearance)
afd_data$FULL_NAME[duplicated(afd_data$FULL_NAME)]

## Duplicates in FULL_NAME (*including* first appearance)
afd_data$FULL_NAME[duplicated(afd_data$FULL_NAME) | duplicated(afd_data$FULL_NAME, fromLast=TRUE)]

temp <- afd_data[duplicated(afd_data$FULL_NAME) | duplicated(afd_data$FULL_NAME, fromLast=TRUE)]
nrow(temp)
fwrite(temp, file.path(outdir, "afd_fullname_repeats.csv"))


## Duplicates in COMPLETE_NAME (excluding first appearance)
afd_data$COMPLETE_NAME[duplicated(afd_data$COMPLETE_NAME)]

## Duplicates in COMPLETE_NAME (*including* first appearance)
afd_data$COMPLETE_NAME[duplicated(afd_data$COMPLETE_NAME) | duplicated(afd_data$COMPLETE_NAME, fromLast=TRUE)]

temp <- afd_data[duplicated(afd_data$COMPLETE_NAME) | duplicated(afd_data$COMPLETE_NAME, fromLast=TRUE)]
nrow(temp)
fwrite(temp, file.path(outdir, "afd_completename_repeats.csv"))


## Create FULL_NAME + AUTHOR = COMPLETE_NAME column
afd_data %>% add_column(COMPLETE_NAME = paste0(trimws(afd_data$FULL_NAME), 
                                               ", ", trimws(afd_data$AUTHOR), 
                                               " ", trimws(afd_data$YEAR)), 
                        .after = "YEAR") -> afd_data

length(unique(afd_data$FULL_NAME))
length(unique(afd_data$COMPLETE_NAME))

## Set keys
afd_data <- setDT(afd_data, key = c("FULL_NAME", "COMPLETE_NAME", "CONCEPT_GUID"))


## Are there duplicates in FULL_NAME & COMPLETE_NAME?
length(afd_data$FULL_NAME) != length(unique(afd_data$FULL_NAME))
nrow(afd_data[duplicated(FULL_NAME),] )

length(afd_data$COMPLETE_NAME) != length(unique(afd_data$COMPLETE_NAME))
nrow(afd_data[duplicated(COMPLETE_NAME),] )


## >> Resolve duplicates manually ####
## NOTE: Duplicates will NOT be removed at this stage. 
...





## ----------------------------------------------- ##
## Data checks (not exhaustive) ####
## ----------------------------------------------- ##
species_record <- remove_improper_names(afd_data$FULL_NAME,
                                        allow.subgenus = TRUE,
                                        allow.higher.taxa = TRUE) 

## Counts for number of words
sp_words <- sapply(strsplit(as.character(afd_data$FULL_NAME), " "), length)
table(sp_words)
# ## stringr version
# str_count(afd_data$FULL_NAME, pattern = "\\S+") |> janitor::tabyl() 


## Counts for special characters in species names
grep("\"",afd_data$FULL_NAME, value = TRUE) ## occurrences for "
grep("\'",afd_data$FULL_NAME, value = TRUE) ## occurrences for '
grep("\\(",afd_data$FULL_NAME, value = TRUE) ## occurrences for ()
grep("\\[",afd_data$FULL_NAME, value = TRUE) ## occurrences for ()

# ## stringr version
# stringr::str_count(afd_data$FULL_NAME, pattern = regex("\\\"")) |> sum() # Counting occurrences for \"
# stringr::str_count(afd_data$FULL_NAME, pattern = regex("'")) |> sum() # Counting occurrences for '
# stringr::str_count(afd_data$FULL_NAME, pattern = regex("\\(")) |> sum() 
# stringr::str_count(afd_data$FULL_NAME, pattern = regex("\\[")) |> sum() 





## ----------------------------------------------- ##
## Create rows for higher order taxa ####
## ----------------------------------------------- ##
names(afd_data)
taxclasses <- names(afd_data)[c(1,3:19, 22)]



for(t in taxclasses){
  
}




## ----------------------------------------------- ##
## Save cleaned data ####
## ----------------------------------------------- ##
fwrite(afd_data, file.path(outdir, paste0("AFD_cleaned_", Sys.Date())))

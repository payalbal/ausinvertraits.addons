#' Remove improper names using stringr
#'
#' @author Payal Bal, Fonti Kar
#' 
#' @param name_vector vector of species names
#' @param allow.subgenus logical; if TRUE retains subgenera and variety
#' @param allow.higher.taxa logical, if TRUE species with single word names are retained as they are likely within species suffix
#' @details This function 1)  removes trailing white spaces, 2) flags and removes species with specific taxonomic modifiers, 
#' text, symbol, formatting patterns in their name (improper species), 3) flags, and removes if so specified, subspecies and variety, 4) flags and removes species that one have one word in their name (incomplete species) 
#' 
#' @return list containing accepted species names, improper names, subspecies and variety names and incomplete names



remove_improper_names <- function(name_vector,
                                  allow.subgenus,
                                  allow.higher.taxa){
  
  
  
  message("Cleaning checklist for improper species names...")
  
  
  
  ## Check if name_vector is in expected format ####
  if (!is.character(name_vector)){
    stop("name_vector is not a character vector")
  }
  
  
  
  ## Clean trailing and double spaces ####
  name_vector <- stringr::str_squish(name_vector)
  
  
  
  ## Tax modifiers for improper species ####
  ## >> Note: As text, symbol, formatting patterns that we want to flag/remove from final list
  regex_pattern <- c("Unplaced", 
                     "\\?",
                     "\\sex\\s",
                     "sp\\.",
                     "Sp\\.",
                     "spp\\.",
                     "aff\\.",
                     "cf\\.", 
                     "indet\\.", 
                     "indet\\s", 
                     "\\ssp$",
                     "aff\\s",
                     "\\ssp\\s",
                     "cf\\s", 
                     "\\sor\\s", 
                     "\\[", 
                     # "sensu",
                     "species$",
                     "sens\\.", 
                     "-$", 
                     "\\s-\\s", 
                     "/", 
                     "s\\.", 
                     "etc\\.",
                     "\\sx$", 
                     "Unidentifi",
                     "spec\\.", 
                     "affin\\.", 
                     "species\\s",
                     "taxon", 
                     "spec\\.nov\\.",
                     "cf\\,", 
                     "\\sand\\s",
                     "\\swith\\s",
                     "\\*",
                     "\\:",
                     "\\\\0",
                     "[:digit:]",
                     "\\\\|[^[:print:]]")
  
  
  
  ## Improper species names as identified by taxa modifiers or particular text patterns ####
  improper_sp <- stringr::str_subset(string = name_vector, 
                                     pattern = stringr::regex(
                                       paste(regex_pattern, collapse = "|")),
                                     negate = FALSE) # to get non-matching names
  proper_sp <- stringr::str_subset(string = name_vector, 
                                   pattern = stringr::regex(
                                     paste(regex_pattern, collapse = "|")),
                                   negate = TRUE) # to get matching names
  
  
  
  # ## sp. names  - not removed ####
  # regex_pattern <- c("sp\\.",
  #                    "Sp\\.",
  #                    "spp\\.")
  # sp_sp <- stringr::str_subset(string = proper_sp,
  #                              pattern = stringr::regex(
  #                                paste(regex_pattern, collapse = "|")),
  #                              negate = FALSE) # to get non-matching names
  
  
  
  ## Sub-genus & variety names ####
  subgen_pattern <- c("\\(*\\)", "var\\.") ## text within () and names with var.
  sub_gen <- stringr::str_subset(string = proper_sp, 
                                 pattern = stringr::regex(
                                   paste(subgen_pattern, collapse = "|")),
                                 negate = FALSE) # FALSE to get non-matching names
  
  if(!allow.subgenus){ ## remove sub-genus and var. if allow.subgenus = FALSE
    proper_sp <- stringr::str_subset(string = proper_sp, 
                                     pattern = stringr::regex(
                                       paste(subgen_pattern, collapse = "|")),
                                     negate = TRUE) # to get matching names
  }
  
  
  
  ## Incomplete names ####
  ## >> names with less <= one word; likely genus level or higher taxa without species suffix
  incomplete_idx <- stringr::str_count(string = proper_sp, pattern = "\\w+") <= 1
  incomplete_sp <- proper_sp[incomplete_idx]
  if (!allow.higher.taxa){ ## remove incomplete names if allow.higher.taxa = FALSE
    proper_sp <- proper_sp[!incomplete_idx] # Note: [-incomplete_idx] does not work
  }
  
  
  
  ## Sub-species names - not removed ####
  subsp_idx <- stringr::str_count(string = proper_sp, pattern = "\\w+") >= 3
  sub_sp <- proper_sp[subsp_idx]
  sub_sp <- sub_sp[!sub_sp %in% sub_gen] # remove subgenus names
  # sub_sp <- sub_sp[!sub_sp %in% sp_sp] # remove sp. names
  
  
  
  ## Outputs ####
  if(allow.subgenus & allow.higher.taxa){
    message("allow.subgenus = TRUE & allow.higher.taxa = TRUE")
    spout <- improper_sp
    spout <- length(spout)
    spin <- length(proper_sp)
    
  } else {
    
    if(allow.subgenus & !allow.higher.taxa){
      message("allow.subgenus = TRUE & allow.higher.taxa = FALSE")
      spout <- c(improper_sp, incomplete_sp)
      spout <- length(spout)
      spin <- length(proper_sp)
      
      
    } else {
      
      if(!allow.subgenus & allow.higher.taxa){ 
        message("allow.subgenus = FALSE & allow.higher.taxa = TRUE")
        spout <- c(improper_sp, sub_gen)
        spout <- length(spout)
        spin <- length(proper_sp)
        
      } else {
        
        if(!allow.subgenus & !allow.higher.taxa){
          message("allow.subgenus = FALSE & allow.higher.taxa = FALSE")
          spout <- c(improper_sp, incomplete_sp, sub_gen)
          spout <- length(spout)
          spin <- length(proper_sp)
          
        }
      }
    }
  }
  
  
  
  ## Output messages ####
  message("************************************************************")
  cat("\n")
  
  message(cat("Number of input species names: "),
          length(name_vector))
  message(cat("Number of species names returned: "),
          length(proper_sp))
  
  message(cat("Number of species names removed: "),
          spout)
  message(cat("Proportion of species names removed: "),
          round(spout/length(name_vector), 4))
  
  cat("\n")
  message(cat("Improper species names found: "),
          length(improper_sp))
  # message(cat("sp./Sp./spp. names found: "),
  #         length(sp_sp))
  message(cat("Incomplete species names found: "),
          length(incomplete_sp))
  message(cat("Sub-genus & varieties found: "),
          length(sub_gen))
  message(cat("Sub-species found: "),
          length(sub_sp))
  
  cat("\n")
  message(cat("Duplicates in returned list: "),
          sum(duplicated(proper_sp)))
  
  cat("\n")
  message("************************************************************")
  
  
  
  ## Return output ####
  return(list(updated_list = proper_sp,
              improper_species = improper_sp,
              # sp_names = sp_sp,
              subgenus = sub_gen,
              subspecies = sub_sp,
              incomplete_species = incomplete_sp))
}
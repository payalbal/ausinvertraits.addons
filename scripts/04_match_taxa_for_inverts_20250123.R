

# setting up taxa list using variant of code from APCalign function align_taxa
taxa_raw <-
  dplyr::tibble(
    original_name = character(0L),
    cleaned_name = character(0L),
    aligned_name = character(0L),
    taxonomic_dataset = character(0L),
    identifier = character(0L),
    known = logical(0L),
    checked = logical(0L)
  ) %>%
  bind_rows(ausinvertraits$traits %>% distinct(taxon_name, dataset_id) %>% rename(original_name = taxon_name, identifier = dataset_id))
  
taxa <- list()

taxa[["tocheck"]] <-
    dplyr::tibble(
      original_name = NA_character_,
      cleaned_name = NA_character_,
      stripped_name = NA_character_,
      stripped_name2 = NA_character_,
      trinomial = NA_character_,
      binomial = NA_character_,
      word_one = NA_character_,
      words_one_two = NA_character_,
      ignore_bracketed_words = NA_character_,
      aligned_name = NA_character_,
      aligned_reason = NA_character_,
      fuzzy_match_word_one = NA_character_,
      fuzzy_match_word_one_synonym = NA_character_,
      fuzzy_match_family = NA_character_,
      fuzzy_match_family_synonym = NA_character_,
      fuzzy_match_binomial = NA_character_,
      fuzzy_match_binomial_synonym = NA_character_,
      fuzzy_match_trinomial = NA_character_,
      fuzzy_match_trinomial_synonym = NA_character_,
      fuzzy_match_ignore_bracketed_words = NA_character_,
      fuzzy_match_ignore_bracketed_words_synonym = NA_character_,
      fuzzy_match_cleaned = NA_character_,
      fuzzy_match_cleaned_synonym = NA_character_,
      fuzzy_match_cleaned_imprecise = NA_character_,
      fuzzy_match_cleaned_synonym_imprecise = NA_character_,
      taxonomic_dataset = NA_character_,
      taxonomic_status = NA_character_,
      taxon_rank = NA_character_,
      alignment_code = NA_character_,
    ) %>%
dplyr::bind_rows(taxa_raw) %>% 
  dplyr::mutate(
    checked = FALSE,
    known = FALSE,
    identifier = identifier
  ) %>%
  # take unique values of original name by identifier combinations
  # so each name only processed once (or multiple times if unique identifiers)
  dplyr::filter(!duplicated(paste0(original_name, identifier))) %>%
  dplyr::filter(original_name %>% standardise_names() != "")


#' @title Match taxonomic names to names in a taxonomic dataset
#' 
#' @description
#' This function attempts to match input strings to Australia's specified taxonomic datasets. It attempts:
#' 1. perfect matches and fuzzy matches
#' 2. matches to infraspecies, species, genus, and higher rank names
#' 3. matches to the entire input string and subsets there-of
#' 4. searches for string patterns that suggest a specific taxon rank
#' 
#' @details
#' - It cycles through more than 20 different string patterns, sequentially
#'  searching for additional match patterns.
#' - It identifies string patterns in input names that suggest a name can only be
#'  aligned to a genus (hybrids that are not accepted names; graded species;
#'  taxa not identified to species).
#' - It prioritises matches that do not require fuzzy matching (i.e. synonyms,
#'  orthographic variants) over those that do.
#' - Taxonomic datasets are sorted, so names align to the top priority taxonomic dataset if a name is present in multiple lists.
#' 
#' @param taxa The list of taxa requiring checking
#
#' @param resources The list(s) of accepted names to check against,
#'  loaded through the function `load_taxonomic_resources()`
#' @param fuzzy_abs_dist The number of characters allowed to be different
#'  for a fuzzy match.
#' @param fuzzy_rel_dist The proportion of characters allowed to be different
#'  for a fuzzy match. 
#' @param fuzzy_matches Fuzzy matches are turned on as a default. The relative
#'  and absolute distances allowed for fuzzy matches to species and
#'  infraspecific taxon names are defined by the parameters 
#' `fuzzy_abs_dist` and `fuzzy_rel_dist`
#' @param identifier A dataset, location or other identifier,
#'  which defaults to NA.
#'
#' @noRd
match_taxa <- function(
    taxa, 
    resources,
    fuzzy_abs_dist = 3, 
    fuzzy_rel_dist = 0.2, 
    fuzzy_matches = TRUE, 
    identifier = NA_character_
) {
  
  update_na_with <- function(current, new) {
    ifelse(is.na(current), new, current)
  }
  
  
  ## A function that specifies particular fuzzy matching conditions (for the
  ## function fuzzy_match) when matching is being done at the taxon rank of genus or higher.
  if (fuzzy_matches == TRUE) {
    fuzzy_match_word_one <- function(x, y) {
      purrr::map_chr(x, ~ fuzzy_match(.x, y, 2, 0.35, n_allowed = 1))
    }
  } else {  
    fuzzy_match_word_one <- function(x, y) {
      purrr::map_chr(x, ~ fuzzy_match(.x, y, 0, 0.0, n_allowed = 1))
    }    
  }
  
  ## override all fuzzy matching parameters with absolute and 
  ## relative distances of 0 if fuzzy matching is turned off
  if (fuzzy_matches == FALSE) {
    fuzzy_abs_dist <- 0
    fuzzy_rel_dist <- 0
  }
  
  ## Repeatedly used identifier strings are created. 
  ## These identifier strings are added to the aligned names of taxa that do
  ## not match to a species or infra-specific level name.
  taxa$tocheck <- taxa$tocheck %>%
    dplyr::mutate(
      identifier_string = ifelse(is.na(identifier), NA_character_, paste0(" [", identifier, "]")),
      identifier_string2 = ifelse(is.na(identifier), NA_character_, paste0("; ", identifier)),
      aligned_name_tmp = NA_character_
    )
  
  ## In the tocheck dataframe, add columns with manipulated versions of the string to match
  ## Various stripped versions of the string to match, versions with 1, 2 and 3 words (word_one, binomial, trinomial), 
  ## and fuzzy-matched word_one are propagated.
  taxa$tocheck <- taxa$tocheck %>%
    dplyr::mutate(
      cleaned_name = cleaned_name %>%
        update_na_with(standardise_names(original_name)),
      stripped_name = stripped_name %>%
        update_na_with(strip_names(cleaned_name)),
      stripped_name2 = stripped_name2 %>%
        update_na_with(strip_names_extra(stripped_name)),
      ignore_bracketed_words = stringr::str_remove(original_name, " \\(.*\\)"),
      trinomial = word(ignore_bracketed_words, start = 1, end = 3),
      binomial = word(ignore_bracketed_words, start = 1, end = 2),
      word_one = extract_genus(original_name),
      words_one_two = word(stripped_name2, start = 1, end = 2)
    )
  
  ## Taxa that have been checked are moved from `taxa$tocheck` to `taxa$checked`
  ## These lines of code are repeated after each matching cycle to
  ## progressively move taxa from `tocheck` to `checked`
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # START MATCHES
  # match_01a: Exact scientific name matches (accepted/valid names)
  # Taxon names that are an accepted scientific name, with authorship.
  
  i <-
    taxa$tocheck$original_name %in% resources$species$accepted$scientific_name
  
  ii <-
    match(
      taxa$tocheck[i,]$original_name,
      resources$species$accepted$scientific_name
    )
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    dplyr::mutate(
      taxonomic_dataset = resources$species$accepted$taxonomic_dataset[ii],
      taxon_rank = resources$species$accepted$taxon_rank[ii],
      taxonomic_status = resources$species$accepted$taxonomic_status[ii],
      aligned_name = resources$species$accepted$canonical_name[ii],
      aligned_reason = paste0(
        "Exact match of taxon name to an accepted/valid scientific name (including authorship) in ", 
        resources$species$accepted$taxonomic_dataset[ii], " (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      alignment_code = "match_01a_accepted_scientific_name_with_authorship"
    )
  
  taxa <- redistribute(taxa)
  
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_01b: Exact scientific name matches (synonyms)
  # Taxon names that are exact matches to a synonymmous scientific name, with authorship.
  
  i <-
    taxa$tocheck$original_name %in% resources$species$synonym$scientific_name
  
  ii <-
    match(
      taxa$tocheck[i,]$original_name,
      resources$species$synonym$scientific_name
    )
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    dplyr::mutate(
      taxonomic_dataset = resources$species$synonym$taxonomic_dataset[ii],
      taxon_rank = resources$species$synonym$taxon_rank[ii],
      taxonomic_status = resources$species$synonym$taxonomic_status[ii],
      aligned_name = resources$species$synonym$canonical_name[ii],
      aligned_reason = paste0(
        "Exact match of taxon name to a synonymous scientific name (including authorship) in ", 
        resources$species$accepted$taxonomic_dataset[ii], " (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      alignment_code = "match_01b_synonym_scientific_name_with_authorship"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_01c: Exact canonical matches (accepted/valid names)
  # Taxon names that are exact matches to canonical names, once filler words and punctuation are removed.
  i <-
    taxa$tocheck$cleaned_name %in% resources$species$accepted$canonical_name
  
  ii <-
    match(
      taxa$tocheck[i,]$cleaned_name,
      resources$species$accepted$canonical_name
    )
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    dplyr::mutate(
      taxonomic_dataset = resources$species$accepted$taxonomic_dataset[ii],
      taxon_rank = resources$species$accepted$taxon_rank[ii],
      taxonomic_status = resources$species$accepted$taxonomic_status[ii],
      aligned_name = resources$species$accepted$canonical_name[ii],
      aligned_reason = paste0(
        "Exact match of taxon name to an accepted/valid canonical name in ", 
        resources$species$accepted$taxonomic_dataset[ii], 
        " once punctuation and filler words are removed (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      alignment_code = "match_01c_accepted_canonical_name"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_01d: Exact canonical matches (synonyms)
  # Taxon names that are exact matches to a synonymous canonical name once filler words and punctuation are removed.
  i <-
    taxa$tocheck$cleaned_name %in% resources$species$synonym$canonical_name
  
  ii <-
    match(
      taxa$tocheck[i,]$cleaned_name,
      resources$species$synonym$canonical_name
    )
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    dplyr::mutate(
      taxonomic_dataset = resources$species$synonym$taxonomic_dataset[ii],
      taxon_rank = resources$species$synonym$taxon_rank[ii],
      taxonomic_status = resources$species$synonym$taxonomic_status[ii],
      aligned_name = resources$species$synonym$canonical_name[ii],
      aligned_reason = paste0(
        "Exact match of taxon name to a synonymous canonical name in ", 
        resources$species$accepted$taxonomic_dataset[ii], 
        " once punctuation and filler words are removed (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      alignment_code = "match_01d_synonym_canonical_name"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_02a: Exact matches, higher level taxa
  # Exact matches to higher level taxa for names where the final "word" is `sp` or `spp`
  # Aligned name includes identifier to indicate `genus sp.`, `family sp.`, etc refers to a specific species (or infra-specific taxon), 
  # associated with a specific dataset/location.
  
  for (ranks in taxon_ranks_to_check) {
    
    i <-
      stringr::str_detect(taxa$tocheck$cleaned_name, "[:space:]sp\\.$") &
      taxa$tocheck$word_one %in% resources[[ranks]]$canonical_name #&
      word(taxa$tocheck$cleaned_name, 2) %in% c("sp.") 
      ##TO DO - previous line needs to be tweaked to accommodate hybrids & genus (subgenus) syntax. 
      ## Should actually be full string minus genus as declared by "extract_genus"
    
    ii <-
      match(
        taxa$tocheck[i,]$word_one,
        resources[[ranks]]$canonical_name
      )
    
    taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
      dplyr::mutate(
        taxonomic_dataset = resources[[ranks]]$taxonomic_dataset[ii],
        taxonomic_status = resources[[ranks]]$taxonomic_status[ii],
        taxon_rank = ranks,
        aligned_name_tmp = paste0(resources[[ranks]]$word_one[ii], " sp."),
        aligned_name = ifelse(is.na(identifier_string),
                              aligned_name_tmp,
                              paste0(aligned_name_tmp, identifier_string)
        ),
        aligned_reason = paste0(
          "Exact match of taxon name ending with `sp.` to a(n) ", taxonomic_status, " ", taxon_rank, " in ",
          taxonomic_dataset,
          " (",
          Sys.Date(),
          ")"
        ),
        checked = TRUE,
        known = TRUE,
        alignment_code = "match_02a_exact_higher_level_accepted_or_synonym"
      )
    
    taxa <- redistribute(taxa)
    
    if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  }

  # match_02b: Fuzzy matches, higher level taxa
  # Fuzzy matches to higher ranked taxon names where the final "word" is `sp` or `spp`
  # Aligned name includes identifier to indicate `name sp.` refers to a specific species (or other taxon), associated with a specific dataset/location.
  
  for (ranks in taxon_ranks_to_check) {  
    taxa$tocheck <- taxa$tocheck %>%
      dplyr::mutate(
        fuzzy_match_word_one =
          fuzzy_match_word_one(word_one, resources[[ranks]]$canonical_name)
      )
  
    i <-
      stringr::str_detect(taxa$tocheck$cleaned_name, "[:space:]sp\\.$") &
      taxa$tocheck$fuzzy_match_word_one %in% resources[[ranks]]$canonical_name &
      word(taxa$tocheck$cleaned_name, 2) %in% c("sp.")
    
    ii <-
      match(
        taxa$tocheck[i,]$fuzzy_match_word_one,
        resources[[ranks]]$canonical_name
      )
    
    taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
      dplyr::mutate(
        taxonomic_dataset = resources[[ranks]]$taxonomic_dataset[ii],
        taxonomic_status = resources[[ranks]]$taxonomic_status[ii],
        taxon_rank = ranks,           
        aligned_name_tmp = 
          paste0(resources[[ranks]]$canonical_name[ii], " sp."),
        aligned_name = ifelse(is.na(identifier_string),
                              aligned_name_tmp,
                              paste0(aligned_name_tmp, identifier_string)
        ),
        aligned_reason = paste0(
          "Fuzzy match of taxon name ending with `sp.` to to a(n) ", taxonomic_status, " ", taxon_rank, " in ",
          taxonomic_dataset,
          " (",
          Sys.Date(),
          ")"
        ),
        known = TRUE,
        checked = TRUE,
        alignment_code = "match_02b_fuzzy_higher_rank_accepted"
      )
    
    taxa <- redistribute(taxa)
    #if (nrow(taxa$tocheck) == 0)
    #  return(taxa)
    
  }
  
  ## removed matches 3, 4 for simplicity, since I don't think they would be triggered by any names (for specific weird informal names)
  
  # match_05a: Fuzzy canonical matches (accepted/valid names)
  # Fuzzy match of taxon name to an accepted/valid canonical name, once filler words and punctuation are removed.
  for (i in seq_len(nrow(taxa$tocheck))) {    
    taxa$tocheck$fuzzy_match_cleaned[i] <-
      fuzzy_match(
        txt = taxa$tocheck$stripped_name[i],
        accepted_list = resources$species$accepted$stripped_canonical,
        max_distance_abs = fuzzy_abs_dist,
        max_distance_rel = fuzzy_rel_dist,
        n_allowed = 1
      )
  }
  
  i <-
    taxa$tocheck$fuzzy_match_cleaned %in% resources$species$accepted$stripped_canonical
  
  ii <-
    match(
      taxa$tocheck[i,]$fuzzy_match_cleaned,
      resources$species$accepted$stripped_canonical
    )
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    dplyr::mutate(
      taxonomic_dataset = resources$species$accepted$taxonomic_dataset[ii],
      taxon_rank = resources$species$accepted$taxon_rank[ii],
      taxonomic_status = resources$species$accepted$taxonomic_status[ii],
      aligned_name = resources$species$accepted$canonical_name[ii],
      aligned_reason = paste0(
        "Fuzzy match of taxon name to an accepted canonical name in ", taxonomic_dataset, 
        " once punctuation and filler words are removed (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      alignment_code = "match_05a_fuzzy_accepted_canonical_name"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_05b: Fuzzy canonical matches (synonyms)
  # Fuzzy match of taxon name to a synonymous canonical name, once filler words and punctuation are removed.
  for (i in seq_len(nrow(taxa$tocheck))) {    
    taxa$tocheck$fuzzy_match_cleaned_synonym[i] <-
      fuzzy_match(
        txt = taxa$tocheck$stripped_name[i],
        accepted_list = resources$species$synonym$stripped_canonical,
        max_distance_abs = fuzzy_abs_dist,
        max_distance_rel = fuzzy_rel_dist,
        n_allowed = 1
      )
  }
  
  i <-
    taxa$tocheck$fuzzy_match_cleaned_synonym %in% resources$species$synonym$stripped_canonical
  
  ii <-
    match(
      taxa$tocheck[i,]$fuzzy_match_cleaned_synonym,
      resources$species$synonym$stripped_canonical
    )
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    dplyr::mutate(
      taxonomic_dataset = resources$species$synonym$taxonomic_dataset[ii],
      taxon_rank = resources$species$synonym$taxon_rank[ii],
      taxonomic_status = resources$species$synonym$taxonomic_status[ii],
      aligned_name = resources$species$synonym$canonical_name[ii],
      aligned_reason = paste0(
        "Fuzzy match of taxon name to a synonymous canonical name in ", taxonomic_dataset, 
        " once punctuation and filler words are removed (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      alignment_code = "match_05b_fuzzy_synonym_canonical_name"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  
  # match_09a: Exact trinomial matches (accepted/valid names)
  # Exact match of first three words of taxon name ("trinomial") to an accepted/valid canonical name.
  # The purpose of matching only the first three words only to accepted/valid names is that
  # sometimes the submitted taxon name is a valid trinomial + notes and 
  # such names will only be aligned by matches considering only the first three words of the stripped name.
  # This match also does a good job aligning and correcting syntax of phrase names (for plants).
  i <-
    taxa$tocheck$trinomial %in% resources$species$accepted$trinomial
  
  ii <-
    match(
      taxa$tocheck[i,]$trinomial,
      resources$species$accepted$trinomial
    )
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    dplyr::mutate(
      taxonomic_dataset = resources$species$accepted$taxonomic_dataset[ii],
      taxon_rank = resources$species$accepted$taxon_rank[ii],
      taxonomic_status = resources$species$accepted$taxonomic_status[ii],
      aligned_name = resources$species$accepted$canonical_name[ii],
      aligned_reason = paste0(
        "Exact match of the first three words of the taxon name to an accepted/valid canonical name in ",
        taxonomic_dataset, " (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      alignment_code = "match_09a_trinomial_exact_accepted"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_09b: Exact trinomial matches (synonyms)
  # Exact match of first three words of taxon name ("trinomial") to a synonymous canonical name.
  # The purpose of matching only the first three words only to names is that
  # sometimes the submitted taxon name is a valid trinomial + notes and 
  # such names will only be aligned by matches considering only the first three words of the stripped name.
  # This match also does a good job aligning and correcting syntax of phrase names.
  i <-
    taxa$tocheck$trinomial %in% resources$species$synonym$trinomial
  
  ii <-
    match(
      taxa$tocheck[i,]$trinomial,
      resources$species$synonym$trinomial
    )
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    dplyr::mutate(
      taxonomic_dataset = resources$species$synonym$taxonomic_dataset[ii],
      taxon_rank = resources$species$synonym$taxon_rank[ii],
      taxonomic_status = resources$species$synonym$taxonomic_status[ii],
      aligned_name = resources$species$synonym$canonical_name[ii],
      aligned_reason = paste0(
        "Exact match of the first three words of the taxon name to a synonymous canonical name in ",
        taxonomic_dataset, " (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      alignment_code = "match_09b_trinomial_exact_synonym"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_09c: Fuzzy trinomial matches (accepted/valid names)
  # Fuzzy match of first three words of taxon name ("trinomial") to an accepted/valid canonical name.
  # The purpose of matching only the first three words only to accepted/valid names is that
  # sometimes the submitted taxon name is a valid trinomial + notes and 
  # such names will only be aligned by matches considering only the first three words of the stripped name.
  # This match also does a good job aligning and correcting syntax of phrase names
  
  for (i in seq_len(nrow(taxa$tocheck))) {
    if (!is.na(taxa$tocheck$trinomial[i])) {
      taxa$tocheck$fuzzy_match_trinomial[i] <-
        fuzzy_match(
          txt = taxa$tocheck$trinomial[i],
          accepted_list = resources$species$accepted$trinomial,
          max_distance_abs = fuzzy_abs_dist,
          max_distance_rel = fuzzy_rel_dist,
          n_allowed = 1
        )
    }
  }

  i <-
    taxa$tocheck$fuzzy_match_trinomial %in% resources$species$accepted$trinomial

  ii <-
    match(
      taxa$tocheck[i,]$fuzzy_match_trinomial,
      resources$species$accepted$trinomial
    )

  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    dplyr::mutate(
      taxonomic_dataset = resources$species$accepted$taxonomic_dataset[ii],
      taxon_rank = resources$species$accepted$taxon_rank[ii],
      taxonomic_status = resources$species$accepted$taxonomic_status[ii],
      aligned_name = resources$species$accepted$canonical_name[ii],
      aligned_reason = paste0(
        "Fuzzy match of the first three words of the taxon name to an accepted/valid canonical name in",
        taxonomic_dataset, " (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      alignment_code = "match_09c_trinomial_fuzzy_accepted"
    )

  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)

  # match_09d: Fuzzy trinomial matches (synonyms)
  # Fuzzy match of first three words of taxon name ("trinomial") to a synonymous canonical name.
  # The purpose of matching only the first three words only to names is that
  # sometimes the submitted taxon name is a valid trinomial + notes and
  # such names will only be aligned by matches considering only the first three words of the stripped name.
  # This match also does a good job aligning and correcting syntax of phrase names
  for (i in seq_len(nrow(taxa$tocheck))) {
    if (!is.na(taxa$tocheck$trinomial[i])) {
      taxa$tocheck$fuzzy_match_trinomial_synonym[i] <-
        fuzzy_match(
          txt = taxa$tocheck$trinomial[i],
          accepted_list = resources$species$synonym$trinomial,
          max_distance_abs = fuzzy_abs_dist,
          max_distance_rel = fuzzy_rel_dist,
          n_allowed = 1
        )
    }
  }

  i <-
    taxa$tocheck$fuzzy_match_trinomial_synonym %in% resources$species$synonym$trinomial

  ii <-
    match(
      taxa$tocheck[i,]$fuzzy_match_trinomial_synonym,
      resources$species$synonym$trinomial
    )

  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    dplyr::mutate(
      taxonomic_dataset = resources$species$synonym$taxonomic_dataset[ii],
      taxon_rank = resources$species$synonym$taxon_rank[ii],
      taxonomic_status = resources$species$synonym$taxonomic_status[ii],
      aligned_name = resources$species$synonym$canonical_name[ii],
      aligned_reason = paste0(
        "Fuzzy match of the first three words of the taxon name to a synonymous canonical name in ",
        taxonomic_dataset, " (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      alignment_code = "match_09d_trinomial_fuzzy_synonym"
    )

  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  
  
  # match_11a: exact matches ignoring bracketed words (accepted/valid)
  # The subgenus is sometimes included in brackets following the genus name and matches
  # should be sought both with and without the bracketed terms since syntax is inconsistent.
  
  i <-
    taxa$tocheck$ignore_bracketed_words %in% resources$species$accepted$ignore_bracketed_words
  
  ii <-
    match(
      taxa$tocheck[i,]$ignore_bracketed_words,
      resources$species$accepted$ignore_bracketed_words
    )
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    dplyr::mutate(
      taxonomic_dataset = resources$species$accepted$taxonomic_dataset[ii],
      taxon_rank = resources$species$accepted$taxon_rank[ii],
      taxonomic_status = resources$species$accepted$taxonomic_status[ii],
      aligned_name = resources$species$accepted$canonical_name[ii],
      aligned_reason = paste0(
        "Exact match of the taxon name with bracketed words removed to an accepted/valid canonical name in ",
        taxonomic_dataset, " (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      alignment_code = "match_11a_no_brackets_accepted"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_11b: exact matches ignoring bracketed words (synonyms)
  # The subgenus is sometimes included in brackets following the genus name and matches
  # should be sought both with and without the bracketed terms since syntax is inconsistent.
  
  i <-
    taxa$tocheck$ignore_bracketed_words %in% resources$species$synonym$canonical_name
  
  ii <-
    match(
      taxa$tocheck[i,]$ignore_bracketed_words,
      resources$species$synonym$ignore_bracketed_words
    )
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    dplyr::mutate(
      taxonomic_dataset = resources$species$synonym$taxonomic_dataset[ii],
      taxon_rank = resources$species$synonym$taxon_rank[ii],
      taxonomic_status = resources$species$synonym$taxonomic_status[ii],
      aligned_name = resources$species$synonym$canonical_name[ii],
      aligned_reason = paste0(
        "Exact match of the taxon name with bracketed words removed to a synonymous canonical name in ",
        taxonomic_dataset, " (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      alignment_code = "match_11b_no_brackets_synonyms"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_11c: fuzzy matches ignoring bracketed words (accepted/valid names)
  
  for (i in seq_len(nrow(taxa$tocheck))) {
    if (!is.na(taxa$tocheck$ignore_bracketed_words[i]) &
        is.na(taxa$tocheck$fuzzy_match_ignore_bracketed_words[i])) {
      taxa$tocheck$fuzzy_match_ignore_bracketed_words[i] <-
        fuzzy_match(
          txt = taxa$tocheck$ignore_bracketed_words[i],
          accepted_list = resources$species$accepted$ignore_bracketed_words,
          max_distance_abs = fuzzy_abs_dist,
          max_distance_rel = fuzzy_rel_dist,
          n_allowed = 1,
          epithet_letters = 2
        )
    }
  }
  
  i <-
    taxa$tocheck$fuzzy_match_ignore_bracketed_words %in% resources$species$accepted$ignore_bracketed_words
  
  ii <-
    match(
      taxa$tocheck[i,]$fuzzy_match_ignore_bracketed_words,
      resources$species$accepted$ignore_bracketed_words
    )
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    dplyr::mutate(
      taxonomic_dataset = resources$species$accepted$taxonomic_dataset[ii],
      taxon_rank = resources$species$accepted$taxon_rank[ii],
      taxonomic_status = resources$species$accepted$taxonomic_status[ii],
      aligned_name = resources$species$accepted$canonical_name[ii],
      aligned_reason = paste0(
        "Fuzzy match of the taxon name with bracketed words removed to an accepted/valid canonical name in ",
        taxonomic_dataset, " (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      alignment_code = "match_11c_no_brackets_fuzzy_accepted"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_11d: fuzzy matches ignoring bracketed words (synonyms)
  
  for (i in seq_len(nrow(taxa$tocheck))) {
    if (!is.na(taxa$tocheck$ignore_bracketed_words[i]) &
        is.na(taxa$tocheck$fuzzy_match_ignore_bracketed_words_synonym[i])) {
      taxa$tocheck$fuzzy_match_ignore_bracketed_words_synonym[i] <-
        fuzzy_match(
          txt = taxa$tocheck$ignore_bracketed_words[i],
          accepted_list = resources$species$synonym$ignore_bracketed_words,
          max_distance_abs = fuzzy_abs_dist,
          max_distance_rel = fuzzy_rel_dist,
          n_allowed = 1,
          epithet_letters = 2
        )
    }
  }
  
  i <-
    taxa$tocheck$fuzzy_match_ignore_bracketed_words_synonym %in% resources$species$synonym$ignore_bracketed_words
  
  ii <-
    match(
      taxa$tocheck[i,]$fuzzy_match_ignore_bracketed_words_synonym,
      resources$species$synonym$ignore_bracketed_words
    )
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    dplyr::mutate(
      taxonomic_dataset = resources$species$synonym$taxonomic_dataset[ii],
      taxon_rank = resources$species$synonym$taxon_rank[ii],
      taxonomic_status = resources$species$synonym$taxonomic_status[ii],
      aligned_name = resources$species$synonym$canonical_name[ii],
      aligned_reason = paste0(
        "Fuzzy match of the taxon name with bracketed words removed to a synonymous canonical name in ",
        taxonomic_dataset, " (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      alignment_code = "match_11d_no_brackets_fuzzy_synonym"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_10a: Exact binomial matches (accepted/valid names)
  # Exact match of first two words of taxon name ("binomial") to an accepted/valid canonical name.
  # The purpose of matching only the first two words only to accepted/valid names is that
  # sometimes the submitted taxon name is a valid binomial + notes 
  # or a valid binomial + invalid infraspecific epithet.
  # Such names will only be aligned by matches considering only the first two words of the stripped name.
  # This match also does a good job aligning and correcting syntax of phrase names.
  
  i <-
    taxa$tocheck$binomial %in% resources$species$accepted$binomial
  
  ii <-
    match(
      taxa$tocheck[i,]$binomial,
      resources$species$accepted$binomial
    )
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    dplyr::mutate(
      taxonomic_dataset = resources$species$accepted$taxonomic_dataset[ii],
      taxon_rank = resources$species$accepted$taxon_rank[ii],
      taxonomic_status = resources$species$accepted$taxonomic_status[ii],
      aligned_name_tmp = paste0(resources$species$accepted$canonical_name[ii], " [", cleaned_name),
      aligned_name = ifelse(is.na(identifier_string2),
                            paste0(aligned_name_tmp, "]"),
                            paste0(aligned_name_tmp, identifier_string2, "]")),
      aligned_reason = paste0(
        "Exact match of the first two words of the taxon name to an accepted/valid canonical name in ",
        taxonomic_dataset, " (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      alignment_code = "match_10a_binomial_exact_accepted"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
  
  # match_10b: Exact binomial matches (synonyms)
  # Exact match of first two words of taxon name ("binomial") to a synonymous canonical name.
  # The purpose of matching only the first two words only to names is that
  # sometimes the submitted taxon name is a valid binomial + notes 
  # or a valid binomial + invalid infraspecific epithet.
  # Such names will only be aligned by matches considering only the first two words of the stripped name.
  # This match also does a good job aligning and correcting syntax of phrase names.
  
  i <-
    taxa$tocheck$binomial %in% resources$species$synonym$binomial &
    stringr::str_detect(taxa$tocheck$ignore_bracketed_words, " ")
  
  ii <-
    match(
      taxa$tocheck[i,]$binomial,
      resources$species$synonym$binomial
    )
  
  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    dplyr::mutate(
      taxonomic_dataset = resources$species$synonym$taxonomic_dataset[ii],
      taxon_rank = resources$species$synonym$taxon_rank[ii],
      taxonomic_status = resources$species$synonym$taxonomic_status[ii],
      aligned_name_tmp = paste0(resources$species$synonym$canonical_name[ii], " [", cleaned_name),
      aligned_name = ifelse(is.na(identifier_string2),
                            paste0(aligned_name_tmp, "]"),
                            paste0(aligned_name_tmp, identifier_string2, "]")),
      aligned_reason = paste0(
        "Exact match of the first two words of the taxon name to a synonymous canonical name in ",
        taxonomic_dataset, " (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      alignment_code = "match_10b_binomial_exact_synonym"
    )
  
  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)

    
  # match_10c: Fuzzy binomial matches (accepted/valid names)
  # Fuzzy match of first two words of taxon name ("binomial") to an accepted/valid canonical name.
  # The purpose of matching only the first two words only to accepted/valid names is that
  # sometimes the submitted taxon name is a valid binomial + notes 
  # or a valid binomial + invalid infraspecific epithet.
  # Such names will only be aligned by matches considering only the first two words of the stripped name.
  # This match also does a good job aligning and correcting syntax of phrase names.
  for (i in seq_len(nrow(taxa$tocheck))) {
    if (!is.na(taxa$tocheck$binomial[i]) &
        is.na(taxa$tocheck$fuzzy_match_binomial[i])) {
      taxa$tocheck$fuzzy_match_binomial[i] <-
        fuzzy_match(
          txt = taxa$tocheck$binomial[i],
          accepted_list = resources$species$accepted$binomial,
          max_distance_abs = fuzzy_abs_dist,
          max_distance_rel = fuzzy_rel_dist,
          n_allowed = 1,
          epithet_letters = 2
        )
    }
  }

  i <-
    taxa$tocheck$fuzzy_match_binomial %in% resources$species$accepted$binomial &
    stringr::str_detect(taxa$tocheck$ignore_bracketed_words, " ")

  ii <-
    match(
      taxa$tocheck[i,]$fuzzy_match_binomial,
      resources$species$accepted$binomial
    )

  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    dplyr::mutate(
      taxonomic_dataset = resources$species$accepted$taxonomic_dataset[ii],
      taxon_rank = resources$species$accepted$taxon_rank[ii],
      taxonomic_status = resources$species$accepted$taxonomic_status[ii],
      aligned_name_tmp = paste0(resources$species$accepted$canonical_name[ii], " [", cleaned_name),
      aligned_name = ifelse(is.na(identifier_string2),
                            paste0(aligned_name_tmp, "]"),
                            paste0(aligned_name_tmp, identifier_string2, "]")),
      aligned_reason = paste0(
        "Fuzzy match of the first two words of the taxon name to a synonyms canonical name in ",
        taxonomic_dataset, " (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      alignment_code = "match_10c_binomial_fuzzy_accepted"
    )

  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)
   
  # match_10d: Fuzzy binomial matches (synonyms)
  # Fuzzy match of first two words of taxon name ("binomial") to a synonymous canonical name.
  # The purpose of matching only the first two words only to names is that
  # sometimes the submitted taxon name is a valid binomial + notes
  # or a valid binomial + invalid infraspecific epithet.
  # Such names will only be aligned by matches considering only the first two words of the stripped name.
  # This match also does a good job aligning and correcting syntax of phrase names (in plants).
  for (i in seq_len(nrow(taxa$tocheck))) {
    if (!is.na(taxa$tocheck$binomial[i]) &
        is.na(taxa$tocheck$fuzzy_match_binomial_synonym[i])) {
      taxa$tocheck$fuzzy_match_binomial_synonym[i] <-
        fuzzy_match(
          txt = taxa$tocheck$binomial[i],
          accepted_list = resources$species$synonym$binomial,
          max_distance_abs = fuzzy_abs_dist,
          max_distance_rel = fuzzy_rel_dist,
          n_allowed = 1,
          epithet_letters = 2
        )
    }
  }

  i <-
    taxa$tocheck$fuzzy_match_binomial_synonym %in% resources$species$synonym$binomial &
    stringr::str_detect(taxa$tocheck$ignore_bracketed_words, " ")

  ii <-
    match(
      taxa$tocheck[i,]$fuzzy_match_binomial_synonym,
      resources$species$synonym$binomial
    )

  taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
    dplyr::mutate(
      taxonomic_dataset = resources$species$synonym$taxonomic_dataset[ii],
      taxon_rank = resources$species$synonym$taxon_rank[ii],
      taxonomic_status = resources$species$synonym$taxonomic_status[ii],
      aligned_name_tmp = paste0(resources$species$synonym$canonical_name[ii], " [", cleaned_name),
      aligned_name = ifelse(is.na(identifier_string2),
                            paste0(aligned_name_tmp, "]"),
                            paste0(aligned_name_tmp, identifier_string2, "]")),
      aligned_reason = paste0(
        "Fuzzy match of the first two words of the taxon name to a synonymous canonical name in ",
        taxonomic_dataset, " (",
        Sys.Date(),
        ")"
      ),
      known = TRUE,
      checked = TRUE,
      alignment_code = "match_10d_binomial_fuzzy_synonym"
    )

  taxa <- redistribute(taxa)
  if (nrow(taxa$tocheck) == 0)
    return(taxa)

  # match_12a: higher-level alignment of 2 words
  # Toward the end of the alignment function, see if first two words of unmatched taxa is a 
  # higher order taxon name in one of the taxonomic references.
  # This is required as subgenus names are formatted as `genus (subgenus)`
  # The 'taxon name' is then reformatted  as `taxon name sp.` with the original name in square brackets.
  for (rank in taxon_ranks_to_check) {  
    
    i <-
      taxa$tocheck$words_one_two %in% resources[[rank]]$words_one_two
    
    ii <-
      match(
        taxa$tocheck[i,]$words_one_two,
        resources[[rank]]$words_one_two
      )
    
    taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
      dplyr::mutate(
        taxonomic_dataset = resources[[rank]]$taxonomic_dataset[ii],
        taxon_rank = rank,
        taxonomic_status = resources[[rank]]$taxonomic_status[ii],
        aligned_name_tmp = paste0(resources[[rank]]$canonical_name[ii], " sp. [", cleaned_name),
        aligned_name = ifelse(is.na(identifier_string2),
                              paste0(aligned_name_tmp, "]"),
                              paste0(aligned_name_tmp, identifier_string2, "]")
        ),
        aligned_reason = paste0(
          "Exact match of the first two words of the taxon name to a ", taxon_rank, " in ", taxonomic_dataset, " (",
          Sys.Date(),
          ")"
        ),
        known = TRUE,
        checked = TRUE,
        alignment_code = "match_12a_higher_rank_taxon_exact_accepted-subgenus"
      )
    
    taxa <- redistribute(taxa)
    if (nrow(taxa$tocheck) == 0)
      return(taxa)
  }
  
  # match_12b: higher-level alignment
  # Toward the end of the alignment function, see if first word of unmatched taxa is a 
  # higher order taxon name in one of the taxonomic references.
  # The 'taxon name' is then reformatted  as `taxon name sp.` with the original name in square brackets.
  for (rank in taxon_ranks_to_check) {  
    
   i <-
    taxa$tocheck$word_one %in% resources[[rank]]$word_one
  
    ii <-
      match(
        taxa$tocheck[i,]$word_one,
        resources[[rank]]$word_one
      )
    
    taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
      dplyr::mutate(
        taxonomic_dataset = resources[[rank]]$taxonomic_dataset[ii],
        taxon_rank = rank,
        taxonomic_status = resources[[rank]]$taxonomic_status[ii],
        aligned_name_tmp = paste0(resources[[rank]]$word_one[ii], " sp. [", cleaned_name),
        aligned_name = ifelse(is.na(identifier_string2),
                              paste0(aligned_name_tmp, "]"),
                              paste0(aligned_name_tmp, identifier_string2, "]")
        ),
        aligned_reason = paste0(
          "Exact match of the first word of the taxon name to a ", taxon_rank, " in ", taxonomic_dataset, " (",
          Sys.Date(),
          ")"
        ),
        known = TRUE,
        checked = TRUE,
        alignment_code = "match_12b_higher_rank_taxon_exact_accepted"
      )
    
    taxa <- redistribute(taxa)
    if (nrow(taxa$tocheck) == 0)
      return(taxa)
  }
  
  
  # match_12f: higher-level fuzzy alignment
  # The final alignment step is to see if a fuzzy match can be made for the first word of unmatched taxa to an  
  # higher order taxon name in one of the taxonomic references.
  # The 'taxon name' is then reformatted  as `taxon name sp.` with the original name in square brackets.
  for (rank in taxon_ranks_to_check) {  
    i <-
      taxa$tocheck$fuzzy_match_word_one %in% resources[[rank]]$word_one
    
    ii <-
      match(
        taxa$tocheck[i,]$word_one,
        resources[[rank]]$word_one
      )
    
    taxa$tocheck[i,] <- taxa$tocheck[i,] %>%
      dplyr::mutate(
        taxonomic_dataset = resources[[rank]]$taxonomic_dataset[ii],
        taxon_rank = rank,
        taxonomic_status = resources[[rank]]$taxonomic_status[ii],
        aligned_name_tmp = paste0(fuzzy_match_word_one, " sp. [", cleaned_name),
        aligned_name = ifelse(is.na(identifier_string2),
                              paste0(aligned_name_tmp, "]"),
                              paste0(aligned_name_tmp, identifier_string2, "]")
        ),
        aligned_reason = paste0(
          "Fuzzy match of the first word of the taxon name to a ", taxon_rank, " in ", taxonomic_dataset, " (",
          Sys.Date(),
          ")"
        ),
        known = TRUE,
        checked = TRUE,
        alignment_code = "match_12f_higher_rank_taxon_fuzzy_accepted"
      )
    
    taxa <- redistribute(taxa)
    if (nrow(taxa$tocheck) == 0)
      return(taxa)
  }
    
 
  
  taxa$tocheck <- taxa$tocheck %>% dplyr::select(-identifier_string, -identifier_string2, -aligned_name_tmp)
  taxa$checked <- taxa$checked %>% dplyr::select(-identifier_string, -identifier_string2, -aligned_name_tmp)
  
  return(taxa)
}


read_csv("~/GitHub/ausinvertraits.addons/data_extra/aligned_taxa_JRM.csv") -> Jess_edits_1
read_csv("~/GitHub/ausinvertraits.addons/data_extra/unmatched_taxa_JRM.csv") -> Jess_edits_2

Jess_edits_1 %>% bind_rows(Jess_edits_2) %>% select(original_name, aligned_name_Jan = aligned_name, taxon_rank_manual_1, 
                                                    taxon_rank_Jan = taxon_rank, manual_edit, reason_for_manual_edit, identifier) -> Jan_alignments

bind_rows(aligned_names2) %>% select(original_name, cleaned_name, stripped_name, aligned_name, aligned_reason, taxon_rank, alignment_code, identifier) %>%
  full_join(Jan_alignments) %>% select(original_name, aligned_name_Jan, taxon_rank_manual_1, taxon_rank_Jan, aligned_name, manual_edit, reason_for_manual_edit, everything()) -> compare_alignments

#compare_alignments %>% filter(aligned_name != aligned_name_Jan) %>% View()

#compare_alignments %>% filter(aligned_name != aligned_name_Jan) %>% write_csv("~/GitHub/ausinvertraits.addons/data_extra/manual_choices.csv")

read_csv("~/GitHub/ausinvertraits.addons/data_extra/manual_choices.csv") -> manual_choices

compare_alignments %>% left_join(manual_choices) -> compare_alignments

compare_alignments %>% 
  mutate(
    replace = ifelse(aligned_name == aligned_name_Jan & !is.na(manual_edit), manual_edit, NA), # cases where Jess overrides algorithm (names checked)
    replace = ifelse(column_to_use == "aligned", aligned_name, replace), # errors in algorithm, manual fixes
    replace = ifelse(column_to_use == "old_aligned", aligned_name_Jan, replace), # errors in algorithm, manual fixes
    replace = ifelse(column_to_use == "manual", manual_edit, replace), # errors in algorithm, manual fixes
    replace = ifelse(aligned_name == aligned_name_Jan & is.na(manual_edit), aligned_name, replace), # aligned name same both Jan & March variants of algorithm, Jess happy with outcome
    replace = ifelse(!is.na(manual_edit) & is.na(replace), manual_edit, replace) # no match in Jan, using Jess' suggestion
  ) -> part_way

#part_way %>% filter(is.na(replace)) %>% write_csv("~/GitHub/ausinvertraits.addons/data_extra/more_manual_edits.csv", na ="")

read_csv("~/GitHub/ausinvertraits.addons/data_extra/more_manual_edits.csv") -> more_manual_edits

part_way %>%
  left_join(more_manual_edits %>% select(original_name, identifier, manual_edit2, taxon_rank_manual)) %>%
  mutate(
    replace = ifelse(is.na(replace), manual_edit2, replace),
    taxonomic_resolution = ifelse(aligned_name == aligned_name_Jan & !is.na(manual_edit), manual_edit, NA),
    taxonomic_resolution = ifelse(column_to_use == "aligned", taxon_rank, taxonomic_resolution),
    taxonomic_resolution = ifelse(column_to_use == "old_aligned", taxon_rank_Jan, taxonomic_resolution),
    taxonomic_resolution = ifelse(column_to_use == "manual", taxon_rank_Jan, taxonomic_resolution),
    taxonomic_resolution = ifelse(aligned_name == aligned_name_Jan & is.na(manual_edit), taxon_rank, taxonomic_resolution),
    taxonomic_resolution = ifelse(!is.na(manual_edit) & is.na(taxonomic_resolution), taxon_rank_manual_1, taxonomic_resolution),
    taxonomic_resolution = ifelse(is.na(taxonomic_resolution), taxon_rank_manual, taxonomic_resolution)
  ) %>% write_csv("~/GitHub/ausinvertraits.addons/data_extra/final_manual_edits_1.csv", na="")


replacements_to_add <- read_csv("~/GitHub/ausinvertraits.addons/data_extra/final_manual_edits.csv") %>% 
  select(find, replace, taxonomic_resolution, reason, dataset_id = identifier) %>% 
  filter(find != replace)

replacements_to_add_by_dataset <- split(replacements_to_add, replacements_to_add$dataset_id)

for (i in seq_along(1:length(replacements_to_add_by_dataset))) {
  replacements_to_add_by_dataset[[i]] <- replacements_to_add_by_dataset[[i]] %>% select(find, replace, taxonomic_resolution, reason)
  traits.build::metadata_add_taxonomic_changes_list(names(replacements_to_add_by_dataset)[[i]], replacements_to_add_by_dataset[[i]])
}

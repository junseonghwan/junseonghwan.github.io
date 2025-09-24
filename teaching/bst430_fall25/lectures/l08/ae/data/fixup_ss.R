ss = read_csv('sample_sheet.csv') %>% 
  mutate(folder_name = str_match(molecule_h5, '/(Sample_[^/]+)/')[,2]) %>% 
  select(library_id, folder_name, treatment, sample_ID, tissue_source, dataset) %>%
  filter(folder_name != "Sample_300_0171_PBL_BT_5_2")
write_csv(ss, 'sample_sheet2.csv')

str_c(ss$sample_ID, collapse = '; ')

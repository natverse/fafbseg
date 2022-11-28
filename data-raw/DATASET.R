## code to prepare `DATASET` dataset goes here
da2ids=c("720575940639337461", "720575940628259407", "720575940611849187",
"720575940622364184", "720575940624106442", "720575940622734835",
"720575940622762995", "720575940626937617", "720575940622311704",
"720575940610052266", "720575940638719104")

aids=da2ids
aids=union(aids, flywire_partner_summary2(da2ids, partners = 'out', version=447, add_cell_types = F, threshold = 35, by.roi = T, summarise = F)$post_pt_root_id %>% as.character())
aids=union(aids, flywire_partner_summary2(da2ids, partners = 'out', version=447, add_cell_types = F, threshold = 35, by.roi = T, summarise = T)$post_pt_root_id %>% as.character())
aids=union(aids, flywire_partner_summary2(da2ids, partners = 'out', version=447, add_cell_types = F, threshold = 35, by.roi = F, summarise = T)$post_pt_root_id %>% as.character())
aids=union(aids, flywire_partner_summary2(da2ids, partners = 'out', version=447, add_cell_types = F, threshold = 35, by.roi = F, summarise = F)$post_pt_root_id %>% as.character())
aids=union(aids, flywire_partner_summary2(da2ids, partners = 'in', version=447, add_cell_types = F, threshold = 15, by.roi = T, summarise = F)$post_pt_root_id %>% as.character())
aids=union(aids, flywire_partner_summary2(da2ids, partners = 'in', version=447, add_cell_types = F, threshold = 35, by.roi = T, summarise = T)$post_pt_root_id %>% as.character())
aids=union(aids, flywire_partner_summary2(da2ids, partners = 'in', version=447, add_cell_types = F, threshold = 15, by.roi = T, summarise = F)$pre_pt_root_id %>% as.character())
aids=union(aids, flywire_partner_summary2(da2ids, partners = 'in', version=447, add_cell_types = F, threshold = 35, by.roi = T, summarise = T)$pre_pt_root_id %>% as.character())
aids=union(aids, lywire_partner_summary2(da2ids, partners = 'in', version=447, add_cell_types = F, threshold = 35, by.roi = F, summarise = T)$pre_pt_root_id %>% as.character())
aids=union(aids, flywire_partner_summary2(da2ids, partners = 'in', version=447, add_cell_types = F, threshold = 35, by.roi = F, summarise = T)$pre_pt_root_id %>% as.character())
aids=union(aids, flywire_partner_summary2(da2ids, partners = 'in', version=447, add_cell_types = F, threshold = 15, by.roi = F, summarise = F)$pre_pt_root_id %>% as.character())

aids64=flywire_ids(aids, integer64 = T)

syn447 <- flywire_connectome_data(version=447)

syn447sel <- syn447 %>%
  filter(pre_pt_root_id %in% aids64 & post_pt_root_id %in% aids64) %>%
  collect()

arrow::write_feather(syn447sel, file.path(system.file('tests/testthat/testdata/', package = 'fafbseg'), 'fcd/447/syn_proof_analysis_filtered_447.feather'), compression = 'lz4')


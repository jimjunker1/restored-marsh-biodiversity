here::i_am("code/03_analyze-beta-partition.R")

# Load all the community matrices

# Due to bug in source() function. 01_load-data.R script must be run by hand because of differential encoding and special characters treatment in`fix_latlong()`

marsh_attr = data.frame(marsh_id = c("LHA","LHB","LHC","WPH01","WPH02","PS07"),
                        group_id = c("restored","restored","natural","natural","natural","natural")
) %>% dplyr::mutate(marsh_id = factor(marsh_id, levels = c("LHA","LHB","LHC","WPH01","WPH02","PS07")))

# split the communities and estimate
# column names for selecting the community data
remove_clean_names <- list(
  "marsh_id",
  "plot_id",
  "plot_size_m2",
  "lat_dd",
  "long_dd",
  "date",
  "technique",
  "gear_area_m2",
  "mesh_size_mm",
  "effort_min",
  "effort_hr",
  "Sample Description"
) %>% unlist()

# separate each community
# tweak minnow community to combine days together
minnow2018mod_matfull <- minnow2018_matfull %>%
  group_by(marsh_id, plot_id) %>%
  dplyr::summarise(across(-any_of(remove_clean_names), ~sum(.x, na.rm= TRUE)))

comm_list <- ls()[grepl("matfull", ls())] %>%
  .[!grepl("minnow2018_matfull", .)] %>%
  sapply(., function(x) eval(as.name(x)), USE.NAMES = TRUE) %>%
  purrr::map(~ .x %>% junkR::named_group_split(marsh_id)) %>%
  rlist::list.subset(., !grepl("CLASS|PHYLUM", names(.)))
# tweak minnow community to combine days into 

site_diversity_part <-
  purrr::map(comm_list, ~ .x %>%
    purrr::map(~ .x %>%
      column_to_rownames("plot_id") %>%
      dplyr::select(-any_of(remove_clean_names)) %>%
      dplyr::mutate(across(where(is.numeric), ~ case_when(
        .x == 0 ~ 0,
        .x != 0 ~ 1
      ))) %>%
      betapart::beta.multi(index.family = "sor")))

site_diversity_dis <-
  purrr::map(comm_list, ~ .x %>%
    purrr::map(~ .x %>%
      column_to_rownames("plot_id") %>%
      dplyr::select(-any_of(remove_clean_names)) %>%
      betapart:::beta.multi.abund(index.family = "bray")))

plot_diversity_part <-
  purrr::map(comm_list, ~ .x %>%
               purrr::map(~ .x %>%
                            column_to_rownames("plot_id") %>%
                            dplyr::select(-any_of(remove_clean_names)) %>%
                            dplyr::mutate(across(where(is.numeric), ~ case_when(
                              .x == 0 ~ 0,
                              .x != 0 ~ 1
                            ))) %>%
                            betapart::beta.pair(index.family = "sor")))

plot_diversity_dis <-
  purrr::map(comm_list, ~ .x %>%
               purrr::map(~ .x %>%
                            column_to_rownames("plot_id") %>%
                            dplyr::select(-any_of(remove_clean_names)) %>%
                            betapart:::beta.pair.abund(index.family = "bray")))


# preliminary figures
diversity_relpart_df = 
  purrr::map(plot_diversity_part, ~.x %>%
  # within each community type
  purrr::map(~.x %>%
               #within each site
               purrr::map(~.x %>%
                            as.matrix %>%
                            .[upper.tri(.)] %>%
                            unlist)) %>%
    # name the marsh id
    bind_rows(.id = "marsh_id")) %>%
  # set the community ids and bind data set
  bind_rows(.id = 'community_id') %>%
  dplyr::mutate(beta.sim_rel = beta.sim/beta.sor,
                beta.sne_rel = beta.sne/beta.sor,
                beta.sor_rel = beta.sor/beta.sor) %>%
  dplyr::select(-beta.sor,-beta.sim,-beta.sne) %>%
  na.omit %>%
  pivot_longer(-community_id:-marsh_id, names_to = 'partition', values_to = 'value')

diversity_relabun_df = 
  purrr::map(plot_diversity_dis, ~.x %>%
               purrr::map(~.x %>%
                            purrr::map(~.x %>%
                                         as.matrix %>%
                                         .[upper.tri(.)] %>%
                                         unlist)) %>%
               bind_rows(.id = 'marsh_id')) %>%
  bind_rows(.id = 'community_id') %>%
  dplyr::mutate(beta.bal_rel = beta.bray.bal/beta.bray,
                beta.gra_rel = beta.bray.gra/beta.bray,
                beta.bray_rel = beta.bray/beta.bray) %>%
  dplyr::select(-beta.bray, -beta.bray.bal, -beta.bray.gra) %>%
  na.omit %>%
  pivot_longer(-community_id:-marsh_id, names_to = 'partition', values_to = 'value')
 
###### SPARE(D) code ########
#
# site_lists = comm_list %>%
#   purrr::map(~.x %>%
#                junkR::named_group_split(marsh_id) %>%
#                purrr::map(~.x %>% dplyr::select(plot_id) %>%
#                unlist %>% unique %>%
#                combn(.,2) %>% t))
# comm_list[[9]]%>%
#                         furrr::future_map(~.x %>%
#                                      column_to_rownames('plot_id') %>%
#                                      dplyr::select(-any_of(remove_clean_names)) %>%
#                                      dplyr::mutate(across(where(is.numeric), ~case_when(.x == 0 ~ 0,
#                                                                                  .x != 0 ~ 1))) %>%
#                                      betapart::beta.pair(index.family = 'sor'))

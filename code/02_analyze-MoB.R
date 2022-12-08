here::i_am("code/02_analyze-MoB.R")

# Load all the community matrices
# Due to bug in source() function. 01_load-data.R script must be run by hand because of differential encoding and special characters treatment in`fix_latlong()`

# Load packages
cl <- makeCluster(detectCores() - 1)

#Load functions
source("code/plot_mobr_out2.R")
# source("code/plot_abu2.R")
# clusterEvalQ(cl, library(mobr))
# clusterExport(cl, '')

# Preparing data
n_perm_MoB <- 1000
n_perm_PERMANOVA <- 1000

#Parameters
main_titles <- c("Plants", "Surface microbes", "Below-surface microbes",
                 "On-marsh nekton", "Minnow", "Off-marsh nekton", "Macroinfauna",
                 "Spiders")

# store results
mob_stats <- list()
delta_stats <- list()
perm_res <- list()
perm_pairwise_res <- list()
bray_cent_dist <- list()
plot_effect_grad <- list ()
plot_SAD <- list ()

# analyses
set.seed(42)
start <- Sys.time()
for (i in 1:length(comm)) {
  temp_comm <- comm[[i]]
  temp_attr <- plot_attr[[i]]
  mob_in <- make_mob_in(temp_comm, temp_attr,
                        coord_names = c("long_dd", "lat_dd"), latlong = TRUE
  )
  file_name <- paste0(
    "analyses_output/",
    taxa_names[i], ".pdf"
  )
  
  pdf(file_name, width = 14, height = 7)
  
  # Species abundance distribution
  par(mfrow = c(1, 2))
  plot_abu(mob_in, "marsh_id", type = "rad", pooled = TRUE, log = "x")
  
  plot_abu(mob_in, "marsh_id", type = "sad", pooled = TRUE, log = "x")
  
  # Two scale analysis
  mob_stats[[i]] <- get_mob_stats(mob_in,
                                  group_var = "marsh_id", ref_level = NULL,
                                  n_perm = n_perm_MoB
  )
  plot(mob_stats[[i]], "S")
  plot(mob_stats[[i]], "N")
  plot(mob_stats[[i]], "S_n")
  plot(mob_stats[[i]], "S_PIE")
  
  
  # Multi scale analysis
  delta_stats[[i]] <- get_delta_stats(mob_in, "marsh_id",
                                      ref_level = NULL,
                                      log_scale = TRUE, n_perm = n_perm_MoB,
                                      overall_p = TRUE, type = "discrete"
  )
  plot(delta_stats[[i]], stat = "b1", scale_by = NULL, log2 = "x", "S ~ effort")
  plot_effect_grad [[i]] <- plot_mobr_out2(delta_stats[[i]], stat = "b1", scale_by = NULL, 
                                           log2 = "x", "effect ~ grad",
                                           main_title= main_titles[i],
                                           x_label = "Marsh")
  plot_effect_grad [[i]]
  plot(delta_stats[[i]], stat = "b1", scale_by = NULL, log2 = "x", "stat ~ effort")
  
  # Composition differences
  perm_res[[i]] <- adonis(temp_comm ~ temp_attr$marsh_id,
                          parallel = cl, permutations = n_perm_PERMANOVA
  )
  dist_comm <- temp_comm[apply(temp_comm, 1, sum) > 0,]
  dist_attr <- temp_attr[apply(temp_comm, 1, sum) > 0,]
  perm_pairwise_res[[i]] <- pairwise.adonis(dist_comm, dist_attr$marsh_id,
                                            perm = n_perm_PERMANOVA
  )
  
  dist_temp <- vegdist(dist_comm, method = "bray")
  bray_cent_dist[[i]] <- dist_multi_centroids(dist_temp, dist_attr$marsh_id)
  
  clus_ward <- hclust(bray_cent_dist[[i]], method = "ward.D")
  par(mfrow = c(1, 2))
  plot(clus_ward, cex = 0.6)
  textplot(capture.output(perm_pairwise_res[[i]][, c(1, 5, 6, 7)]),
           valign = "center",
           halign = "center",
           mar = c(0, 0, 0, 0), cex = 0.6
  )
  
  dev.off()
}
print(Sys.time() - start)
rm(start)
stopCluster(cl)

save(mob_stats,
     delta_stats,
     perm_res,
     perm_pairwise_res,
     bray_cent_dist,
     taxa_names,
     plot_effect_grad,
     file = here::here("analyses_output/output_analyses.RData")
)



## Run the delta stats analysis within and among created marshes
## This code is currently commented out as it takes a bit of time and computation to run. The objects are pre-made and loaded below.
## If you want/need to rerun the code, you can here.

load ('sub-projects/restored-marsh-diversity/analyses_output/output_analyses.RData')
# pairwise_mobr = function(marshes = NULL, comm = NULL, attr = NULL, comm_name = NULL, string_concat = NULL,...){
#   rows = grep(paste(unlist(marshes), collapse = "|"), attr$marsh_id)
#   comm = comm[rows,] %>% .[,colSums(.) >0]
#   attr = attr[rows,]
#   
#   
#   set.seed(42)
#   
#   mob_in <- make_mob_in(comm, attr,
#                         coord_names = c("long_dd", "lat_dd"), latlong = TRUE
#   )
#   delta_stats <- get_delta_stats(mob_in, "marsh_id", inds = 10,
#                                       ref_level = NULL,
#                                       log_scale = TRUE, n_perm = n_perm_MoB,
#                                       overall_p = TRUE, type = "discrete"
#   )
#   
#   name = paste0(paste(unlist(marshes), collapse = "_"),"_",comm_name, string_concat,"_DeltaStats")
#   path = paste0("sub-projects/restored-marsh-diversity/analyses_output/",name,".rds")
#   saveRDS(delta_stats, file = here::here(path))
#   return(assign(name, delta_stats))
#   
# }
# 
# 
# createdMarshes = c("LHA","LHB")
# # within created marshes
# withinCreated = t(combn(createdMarshes, m = 2)) %>% apply(.,1,function(x) as.list(unlist(x)))
# 
# naturalMarshes = c("LHC","WPH01","WPH02","PS07")
# 
# withinNatural = t(combn(naturalMarshes, m =2)) %>% apply(.,1,function(x) as.list(unlist(x))) 
# # within natural marshes
# naturalList = purrr::map(withinNatural, ~.x %>% unlist %>% rep(.,8) %>% split(., ceiling(seq_along(.)/2)))
# 
# acrossMarshes = t(combn(c(createdMarshes, naturalMarshes), m = 2)) %>% apply(.,1,function(x) as.list(unlist(x)))
# # remove the duplicates to minimize redundancy
# dupCreated = which(purrr::map2(acrossMarshes, withinCreated, ~length(intersect(.x,.y))) == 2) %>% unlist
# dupNatural = purrr::map(withinNatural, ~purrr::map2(acrossMarshes, list(.), ~length(intersect(.y,.x))==2)) %>%
#   purrr::map(~which(.x == TRUE)) %>% unlist
# 
# acrossMarshes = acrossMarshes[-c(dupCreated,dupNatural)]
# 
# # debugonce(pairwise_mobr)                         
# withinNaturalList = purrr::map(naturalList, ~purrr::pmap(list(.x, comm, plot_attr, taxa_names), ~pairwise_mobr(..1,..2,..3, comm_name = ..4, string_concat = "Natural")))
# # saveRDS(withinPairwiseList, here::here("sub-projects/restored-marsh-diversity/analyses_output/withinNaturalList.rds"))
# 
# createdLists = purrr::map(withinCreated, ~.x %>% unlist %>% rep(.,8) %>% split(., ceiling(seq_along(.)/2))) %>% flatten
# 
# withinCreatedList = furrr::future_pmap(list(createdLists, comm, plot_attr, taxa_names), ~pairwise_mobr(..1,..2,..3, comm_name = ..4, string_concat = "Created"))
# 
# # across created and natural marshes
# amongList = purrr::map(acrossMarshes, ~.x %>% unlist %>% rep(.,8) %>% split(., ceiling(seq_along(.)/2)))
# 
# amongMarshesList = purrr::map(amongList, ~purrr::pmap(list(.x, comm, plot_attr,taxa_names), ~pairwise_mobr(..1,..2,..3, comm_name = ..4, string_concat = "Among")))#,

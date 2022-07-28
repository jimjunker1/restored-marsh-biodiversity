here::i_am("code/08_make-effects-figures.R")

# make final figures manuscript
# load in analysis data and assign names
load(here::here("analyses_output/output_analyses.RData"))
# load(here::here("sub-projects/restored-marsh-diversity/analyses_output/withinAmongAnalyses.rds"))
library(tidyverse)
library(grid)
library(gtable)
library(ggthemes)
# library(ggdendro)
library(dendextend)
# taxa grouping orders: Plants, Microbes0-2, Microbes8-10, Infauna, Spiders, Pond minnow, Off-marsh
taxa_names <- c("plants", "microbes_0.2", "microbes_8.10", "pond_minnow", "minnow", "trawl", "infauna", "spiders")
comm = setNames(comm, taxa_names)

taxa_order = c("microbes_0.2", "microbes_8.10", "plants", "infauna", "spiders", "pond_minnow",
               "minnow", "trawl")

marsh_attr = data.frame(marsh_id = c("LHA","LHB","LHC","WPH01","WPH02","PS07"),
                        group_id = c("restored","restored","natural","natural","natural","natural")
) %>% dplyr::mutate(marsh_id = factor(marsh_id, levels = c("LHA","LHB","LHC","WPH01","WPH02","PS07")))

plot_attr = setNames(plot_attr, taxa_names)
plot_attr = purrr::map(plot_attr, ~.x %>% left_join(marsh_attr))

commMobIn = purrr::map2(comm,plot_attr, ~mobr::make_mob_in(.x, .y, coord_names = c('long_dd','lat_dd'),
                                                           latlong = TRUE))
delta_stats = setNames(delta_stats, nm = taxa_names)

comm = rlist::list.subset(comm, taxa_order); plot_attr = rlist::list.subset(plot_attr, taxa_order)
commMobIn = rlist::list.subset(commMobIn, taxa_order)
delta_stats = rlist::list.subset(delta_stats, taxa_order)

## set plotting aesthetics

site_order = c("LHA","LHB","LHC","WPH01","WPH02","PS07")

color_scheme = c("#01665e","#5ab4ac", "#ffffb2","#fed98e","#fe9929","#cc4c02")
color_scheme2 = c("#01665e","#5ab4ac","#ffffcc","#a1dab4","#41b6c4","#225ea8")
shape_scheme = c(22,22,21,21,21,21)
theme_set(theme_minimal())

## Effectsplots ####

marsh_attr = data.frame(marsh_id = c("LHA","LHB","LHC","WPH01","WPH02","PS07"),
                        group_id = c("restored","restored","natural","natural","natural","natural")
) %>% dplyr::mutate(marsh_id = factor(marsh_id, levels = c("LHA","LHB","LHC","WPH01","WPH02","PS07")))

plantPlotAttr = plot_attr[[1]] %>% left_join(marsh_attr)
plantMob = mobr::make_mob_in(comm = comm[[1]], plantPlotAttr, coord_names = c("long_dd", "lat_dd"), latlong = TRUE)

plantDelta = mobr::get_delta_stats(plantMob, 
                                   "group_id",
                                   ref_level = NULL,
                                   log_scale = TRUE, 
                                   n_perm = 100,
                                   overall_p = TRUE,
                                   type = "discrete")

# marsh effects

plot_attr = purrr::map(plot_attr, ~.x %>% left_join(marsh_attr))

commMobIn = purrr::map2(comm, plot_attr, ~mobr::make_mob_in(comm = .x, plot_attr = .y, 
                                                            coord_names = c("long_dd","lat_dd"),
                                                            latlong = TRUE))

commDeltaStats = purrr::map(commMobIn, ~mobr::get_delta_stats(.x,
                                                              "group_id",
                                                              ref_level = "natural",
                                                              log_scale = TRUE,
                                                              n_perm = 1000,
                                                              overall_p = TRUE,
                                                              type = "discrete"))
# plot(commDeltaStats[[1]])
plot_deltas = function(mob_out = NULL, plot_annotation = NULL,...){
  
  effDf = mob_out %>%
    pluck("mod_df") %>%
    dplyr::filter(index == "b1")
  
  agg_plot = effDf %>%
    dplyr::filter(test == 'agg') %>%
    ggplot()+
    geom_ribbon(aes(x = effort, ymin = low_value, ymax = high_value), 
                fill = 'grey' , alpha = 0.5) +
    geom_line(aes(x = effort, y = value), color = 'blue', size = 1.1)+
    geom_hline(aes(yintercept = 0), linetype = 'dashed', color = 'black')+
    annotate('text', label = plot_annotation, family = 'serif', x = 0, y = Inf,
             hjust = 0, vjust = 1, size = 4)+     theme_tufte()+
    theme(axis.title = element_blank())
  
  NSAD_ymax = effDf %>% dplyr::filter(test != 'agg') %>% select(value, high_value) %>% unlist %>% max
  NSAD_ymin = effDf %>% dplyr::filter(test != 'agg') %>% select(value, low_value) %>% unlist %>% min
  
  N_plot = effDf %>%
    dplyr::filter(test == 'N') %>%
    ggplot()+
    geom_ribbon(aes(x = effort, ymin = low_value, ymax = high_value), 
                fill = 'grey' , alpha = 0.5) +
    geom_line(aes(x = effort, y = value), color = 'blue', size = 1.1)+
    geom_hline(aes(yintercept = 0), linetype = 'dashed', color = 'black')+
    scale_y_continuous(limits = c(NSAD_ymin, NSAD_ymax))+
    theme_tufte()+
    theme(axis.title = element_blank())
  
  SAD_plot = effDf %>%
    dplyr::filter(test == 'SAD') %>%
    ggplot()+
    geom_ribbon(aes(x = effort, ymin = low_value, ymax = high_value), 
                fill = 'grey' , alpha = 0.5) +
    geom_line(aes(x = effort, y = value), color = 'blue', size = 1.1)+
    geom_hline(aes(yintercept = 0), linetype = 'dashed', color = 'black')+
    scale_y_continuous(limits = c(NSAD_ymin, NSAD_ymax))+
    theme_tufte()+
    theme(axis.title = element_blank())
  
  gridExtra::grid.arrange(agg_plot, N_plot, SAD_plot, ncol = 3)
}

plot_deltas(commDeltaStats[[1]], "plants")

group_names = c("Plants", "Microbes (0-2 cm)", "Microbes (8-10 cm)","On-Marsh\n minnow", "Full minnow","Trawl", "Infauna", "Spiders")

effectsPlots = purrr::map2(commDeltaStats, group_names, ~plot_deltas(.x, .y))

Agg <- grid::grobTree(#rectGrob(x=unit(0.5, "npc"), y=unit(1,"npc"),
  # gp=gpar(fill="lightgrey", col = NA), vjust = 1, 
  # width = unit(0.27, "npc"), height = unit(1, "npc")),
  textGrob('Aggregation', x=unit(0.5, "npc"), #y=unit(0.99,"npc"),
           gp = gpar(fontfamily = 'serif', cex = 1.1),
           vjust =0))
N <- grid::grobTree(#rectGrob(x=unit(0.5, "npc"), y=unit(1,"npc"),
  # gp=gpar(fill="lightgrey", col = NA), vjust = 1, 
  # width = unit(0.28, "npc"), height = unit(1, "npc")),
  textGrob('Density', x=unit(0.5, "npc"), #y=unit(0.99,"npc"),
           gp = gpar(fontfamily = 'serif', cex = 1.1),
           vjust =0))
SAD <- grid::grobTree(#rectGrob(x=unit(0.5, "npc"), y=unit(1,"npc"),
  # gp=gpar(fill="lightgrey", col = NA), vjust = 1, 
  # width = unit(0.21, "npc"), height = unit(1, "npc")),
  textGrob('SAD', x=unit(0.5, "npc"), #y=unit(0.99,"npc"),
           gp = gpar(fontfamily = 'serif', cex = 1.1),
           vjust =0))

png(here::here("figures/effects.png"), width = 6, height = 10, units = "in", res = 480 )
fullPlot = gridExtra::grid.arrange(Agg, N, SAD, 
                                   effectsPlots[[1]], 
                                   effectsPlots[[2]], 
                                   effectsPlots[[3]],
                                   effectsPlots[[4]], 
                                   effectsPlots[[6]],
                                   effectsPlots[[7]], 
                                   effectsPlots[[8]],
                                   layout_matrix = rbind(c(1,2,3),
                                                         rep(4,3),
                                                         rep(4,3),
                                                         rep(4,3),
                                                         rep(4,3),
                                                         rep(5,3),
                                                         rep(5,3),
                                                         rep(5,3),
                                                         rep(5,3),
                                                         rep(6,3),
                                                         rep(6,3),
                                                         rep(6,3),
                                                         rep(6,3),
                                                         rep(7,3),
                                                         rep(7,3),
                                                         rep(7,3),
                                                         rep(7,3),
                                                         rep(8,3),
                                                         rep(8,3),
                                                         rep(8,3),
                                                         rep(8,3),
                                                         rep(8,3),
                                                         rep(9,3),
                                                         rep(9,3),
                                                         rep(9,3),
                                                         rep(9,3),
                                                         rep(10,3),
                                                         rep(10,3),
                                                         rep(10,3),
                                                         rep(10,3)),
                                   ncol = 3,
                                   left = grid::textGrob(label = expression("Slope ( "*italic(beta)*"1 )"),
                                                         gp = gpar(fontfamily = 'serif'), rot = 90),
                                   bottom = grid::textGrob(label = "Effort (plot/individual)",
                                                           gp = gpar(fontfamily = 'serif')))
dev.off()


### Effect size figure
extract_effects = function(delta, ...){
  # type = gsub(".*(Created|Natural|Among).*", "\\1", names(delta))
  
  df = delta %>%
    # flatten %>%
    pluck("mod_df") %>%
    dplyr::filter(index == "b1") %>%
    dplyr::mutate(value = abs(value))
  
  aggMaxEffort = df %>%
    dplyr::filter(test == "agg") %>%
    dplyr::select(effort) %>% 
    dplyr::mutate(diff = abs(effort-(max(effort)/2))) %>%
    slice_min(diff) %>% dplyr::select(effort) %>% unlist %>%
    max
  
  nEfforts = df %>%
    dplyr::filter(test == 'N') %>%
    dplyr::select(effort) %>% 
    dplyr::mutate(diff = abs(effort-(max(effort)/2))) %>%
    slice_min(diff) %>% dplyr::select(effort) %>% unlist %>%
    max %>%
    c(df %>% dplyr::filter(test == "N") %>%
        dplyr::select(effort) %>% unlist %>% min,.)
  
  dfOut = df %>%
    dplyr::filter(test == "agg" & effort %in% c(1,aggMaxEffort) |
                    test %in% c("N","SAD") & effort %in% nEfforts) %>%
    # dplyr::mutate(type = type) %>%
    group_by(test) %>% 
    dplyr::mutate(effGroup = case_when(effort == min(effort) ~ "lower",
                                       TRUE ~ "upper")) %>%
    ungroup
  
  return(dfOut)
}

extract_full_effects = function(delta, nm = NULL, ...){
  effDf = delta %>% pluck("mod_df") %>% 
    dplyr::filter(index == 'b1') %>%
    dplyr::select(test, effort, value) %>%
    dplyr::mutate(siteComp = gsub("(\\w{3,4}_\\w{3,4})\\s.*","\\1", nm),
                  type = gsub(".*\\s(\\w+)$","\\1", nm))
  return(effDf)
}

plot_deltas = function(deltaDf = NULL, plot_annotation = NULL,...){
  agg_plot = deltaDf %>%
    dplyr::filter(test == 'agg') %>%
    dplyr::mutate(value = abs(value)) %>%
    ggplot()+
    geom_smooth(aes(x = effort, y = value, group = siteComp, color = type),
                size = 0.9, alpha = 0.7, se = FALSE, span = 0.7)+
    geom_hline(aes(yintercept = 0), linetype = 'dashed', size = 0.8)+
    scale_y_continuous(name = expression(""~Delta~italic(S)), limits = c(0,NA),
                       expand = c(0.01,0.01))+
    scale_x_continuous(trans = 'log2', labels = scales::label_number_auto(),
                       expand = c(0.01,0.01))+
    scale_color_manual(values = c(color_scheme[5],"grey",color_scheme[1]))+
    geom_rangeframe(sides = 'lb')+
    annotate('text', label = plot_annotation, x = 1, y = Inf, hjust = 0,
             vjust = 1, family = 'serif', size = 4)+
    theme(legend.position = "none",
          axis.title = element_blank())
  
  N_plot = deltaDf %>%
    dplyr::filter(test == 'N') %>%
    dplyr::mutate(value = abs(value)) %>%
    ggplot()+
    geom_smooth(aes(x = effort, y = value, group = siteComp, color = type),
                size = 0.9, alpha = 0.7, se = FALSE, span = 0.7)+
    geom_hline(aes(yintercept = 0), linetype = 'dashed', size = 0.8)+
    scale_y_continuous(name = expression(""~Delta~italic(S)), limits = c(0,NA),
                       expand = c(0.01,0.01))+
    scale_x_continuous(trans = 'log2', labels = scales::label_number_auto(),
                       expand = c(0.01,0.01))+
    scale_color_manual(values = c(color_scheme[5],"grey",color_scheme[1]))+
    geom_rangeframe(sides = 'lb')+
    theme(legend.position = "none",
          axis.title = element_blank())
  
  SAD_plot = deltaDf %>%
    dplyr::filter(test == 'SAD') %>%
    dplyr::mutate(value = abs(value)) %>%
    ggplot()+
    geom_smooth(aes(x = effort, y = value, group = siteComp, color = type),
                size = 0.9, alpha = 0.7, se = FALSE, span = 0.7)+
    geom_hline(aes(yintercept = 0), linetype = 'dashed', size = 0.8)+
    scale_y_continuous(name = expression(""~Delta~italic(S)), limits = c(0,NA),
                       expand = c(0.01,0.01))+
    scale_x_continuous(trans = 'log2', labels = scales::label_number_auto(),
                       expand = c(0.01,0.01))+
    scale_color_manual(values = c(color_scheme[5],"grey",color_scheme[1]))+
    geom_rangeframe(sides = 'lb')+
    theme(legend.position = "none",
          axis.title = element_blank())
  
  
  gridExtra::grid.arrange(agg_plot, N_plot, SAD_plot, ncol = 3)
  
}

### Plants ----

plantFiles = list.files(here::here("analyses_output"), "*.rds",
                        full.names = TRUE) %>%
  .[grepl("plants", ., ignore.case = TRUE)] 

plantNames = plantFiles %>%
  lapply(., function(x){
    x_str = unlist(strsplit(x,"/"))
    x_split = x_str[length(x_str)]
    site_nms = sapply(strsplit(x_split,"_"),"[",1:2) %>% paste(., collapse = "_")
    comparison_type = gsub(".*(Created|Natural|Among).*", "\\1",x_split)
    paste(site_nms, comparison_type)
  })

plantDeltas = plantFiles %>%
  lapply(., readRDS) %>%
  setNames(.,plantNames)

plantDeltaDf = purrr::map2(plantDeltas, plantNames, ~extract_full_effects(.x,.y)) %>% bind_rows %>%
  dplyr::mutate(type = factor(type, levels = c("Natural", "Among", "Created")))

# #### microbes 0-2

microbe02Files = list.files(here::here("analyses_output"), "*.rds",
                            full.names = TRUE) %>%
  .[grepl("microbes_0.2", ., ignore.case = TRUE)] 

microbe02Names = microbe02Files %>%
  lapply(., function(x){
    x_str = unlist(strsplit(x,"/"))
    x_split = x_str[length(x_str)]
    site_nms = sapply(strsplit(x_split,"_"),"[",1:2) %>% paste(., collapse = "_")
    comparison_type = gsub(".*(Created|Natural|Among).*", "\\1",x_split)
    paste(site_nms, comparison_type)
  })

microbe02Deltas = microbe02Files %>%
  lapply(., readRDS) %>%
  setNames(.,microbe02Names)

microbe02DeltaDf = purrr::map2(microbe02Deltas, microbe02Names, ~extract_full_effects(.x,.y)) %>% bind_rows %>%
  dplyr::mutate(value = abs(value),
                type = factor(type, levels = c("Natural", "Among", "Created")))



# #### microbes 8-10

microbe810Files = list.files(here::here("analyses_output"), "*.rds",
                             full.names = TRUE) %>%
  .[grepl("microbes_8.10", ., ignore.case = TRUE)] 

microbe810Names = microbe810Files %>%
  lapply(., function(x){
    x_str = unlist(strsplit(x,"/"))
    x_split = x_str[length(x_str)]
    site_nms = sapply(strsplit(x_split,"_"),"[",1:2) %>% paste(., collapse = "_")
    comparison_type = gsub(".*(Created|Natural|Among).*", "\\1",x_split)
    paste(site_nms, comparison_type)
  })

microbe810Deltas = microbe810Files %>%
  lapply(., readRDS) %>%
  setNames(.,microbe810Names)


microbe810DeltaDf = purrr::map2(microbe810Deltas, microbe810Names, ~extract_full_effects(.x,.y)) %>% bind_rows %>%
  dplyr::mutate(value = abs(value),
                type = factor(type, levels =c("Natural", "Among", "Created")))


# #### pond minnow

onMarshMinnowFiles = list.files(here::here("analyses_output"), "*.rds",
                                full.names = TRUE) %>%
  .[grepl("pond_minnow", ., ignore.case = TRUE)] 

onMarshMinnowNames = onMarshMinnowFiles %>%
  lapply(., function(x){
    x_str = unlist(strsplit(x,"/"))
    x_split = x_str[length(x_str)]
    site_nms = sapply(strsplit(x_split,"_"),"[",1:2) %>% paste(., collapse = "_")
    comparison_type = gsub(".*(Created|Natural|Among).*", "\\1",x_split)
    paste(site_nms, comparison_type)
  })

onMarshMinnowDeltas = onMarshMinnowFiles %>%
  lapply(., readRDS) %>%
  setNames(.,onMarshMinnowNames)


onMarshMinnowDeltaDf = purrr::map2(onMarshMinnowDeltas,onMarshMinnowNames, ~extract_full_effects(.x,.y)) %>% bind_rows %>%
  dplyr::mutate(value = abs(value),
                type = factor(type, levels = c("Natural", "Among", "Created")))


# #### trawl

trawlFiles = list.files(here::here("analyses_output"), "*.rds",
                        full.names = TRUE) %>%
  .[grepl("trawl", ., ignore.case = TRUE)] 

trawlNames = trawlFiles %>%
  lapply(., function(x){
    x_str = unlist(strsplit(x,"/"))
    x_split = x_str[length(x_str)]
    site_nms = sapply(strsplit(x_split,"_"),"[",1:2) %>% paste(., collapse = "_")
    comparison_type = gsub(".*(Created|Natural|Among).*", "\\1",x_split)
    paste(site_nms, comparison_type)
  })

trawlDeltas = trawlFiles %>%
  lapply(., readRDS) %>%
  setNames(.,trawlNames)

trawlDeltaDf = purrr::map2(trawlDeltas,trawlNames, ~extract_full_effects(.x,.y)) %>% bind_rows %>%
  dplyr::mutate(value = abs(value),
                type = factor(type, levels = c("Natural", "Among", "Created")))


# #### infauna

infaunaFiles = list.files(here::here("analyses_output"), "*.rds",
                          full.names = TRUE) %>%
  .[grepl("infauna", ., ignore.case = TRUE)] 

infaunaNames = infaunaFiles %>%
  lapply(., function(x){
    x_str = unlist(strsplit(x,"/"))
    x_split = x_str[length(x_str)]
    site_nms = sapply(strsplit(x_split,"_"),"[",1:2) %>% paste(., collapse = "_")
    comparison_type = gsub(".*(Created|Natural|Among).*", "\\1",x_split)
    paste(site_nms, comparison_type)
  })

infaunaDeltas = infaunaFiles %>%
  lapply(., readRDS) %>%
  setNames(.,infaunaNames)

infaunaDeltaDf = purrr::map2(infaunaDeltas,infaunaNames, ~extract_full_effects(.x,.y)) %>% bind_rows %>%
  dplyr::mutate(value = abs(value),
                type = factor(type, levels = c("Natural", "Among", "Created")))


# #### spiders

spidersFiles = list.files(here::here("analyses_output"), "*.rds",
                          full.names = TRUE) %>%
  .[grepl("spiders", ., ignore.case = TRUE)] 

spidersNames = spidersFiles %>%
  lapply(., function(x){
    x_str = unlist(strsplit(x,"/"))
    x_split = x_str[length(x_str)]
    site_nms = sapply(strsplit(x_split,"_"),"[",1:2) %>% paste(., collapse = "_")
    comparison_type = gsub(".*(Created|Natural|Among).*", "\\1",x_split)
    paste(site_nms, comparison_type)
  })

spidersDeltas = spidersFiles %>%
  lapply(., readRDS) %>%
  setNames(.,spidersNames)


spidersDeltaDf = purrr::map2(spidersDeltas, spidersNames, ~extract_full_effects(.x,.y)) %>% bind_rows %>%
  dplyr::mutate(value = abs(value),
                type = factor(type, levels = c("Natural", "Among", "Created")))


commDeltaStats = list(plantDeltaDf,
                      microbe02DeltaDf,
                      microbe810DeltaDf,
                      spidersDeltaDf,
                      infaunaDeltaDf,
                      onMarshMinnowDeltaDf,
                      trawlDeltaDf)

group_names = c("Plants", "Microbes (0-2 cm)", "Microbes (8-10 cm)", "Spiders", "Infauna","On-Marsh minnow", "Trawl")

effectsPlots = purrr::map2(commDeltaStats, group_names, ~plot_deltas(.x, .y))

plot_legend = cowplot::as_gtable(cowplot::get_legend(plantDeltaDf %>%
                                                       dplyr::filter(test == 'agg') %>%
                                                       dplyr::mutate(value = abs(value)) %>%
                                                       ggplot()+
                                                       geom_smooth(aes(x = effort, y = value, group = siteComp, color = type),
                                                                   size = 0.9, alpha = 0.7, se = FALSE, span = 0.7)+
                                                       scale_color_manual(values = c(color_scheme[1],"grey",color_scheme[5]))+
                                                       guides(color = guide_legend(nrow = 1), byRow = TRUE)+
                                                       theme(legend.title = element_blank(),
                                                             legend.text = element_text(family ='serif', size = 12))))


Agg <- grid::grobTree(textGrob('Aggregation', x=unit(0.5, "npc"), y=unit(0.01,"npc"),
                               gp = gpar(fontfamily = 'serif', cex = 1.1),
                               vjust =0))
N <- grid::grobTree(textGrob('Density', x=unit(0.5, "npc"), y=unit(0.01,"npc"),
                             gp = gpar(fontfamily = 'serif', cex = 1.1),
                             vjust =0))
SAD <- grid::grobTree(textGrob('SAD', x=unit(0.5, "npc"), y=unit(0.01,"npc"),
                               gp = gpar(fontfamily = 'serif', cex = 1.1),
                               vjust =0))

png(here::here("figures/effects.png"), width = 6, height = 10, units = "in", res = 480 )
fullPlot = gridExtra::grid.arrange(plot_legend,
                                   Agg, N, SAD, 
                                   effectsPlots[[1]], 
                                   effectsPlots[[2]], 
                                   effectsPlots[[3]],
                                   effectsPlots[[4]], 
                                   effectsPlots[[5]],
                                   effectsPlots[[6]], 
                                   effectsPlots[[7]],
                                   layout_matrix = rbind(c(1,1,1),
                                                         c(2,3,4),
                                                         rep(5,3),
                                                         rep(5,3),
                                                         rep(5,3),
                                                         rep(5,3),
                                                         rep(6,3),
                                                         rep(6,3),
                                                         rep(6,3),
                                                         rep(6,3),
                                                         rep(7,3),
                                                         rep(7,3),
                                                         rep(7,3),
                                                         rep(7,3),
                                                         rep(8,3),
                                                         rep(8,3),
                                                         rep(8,3),
                                                         rep(8,3),
                                                         rep(9,3),
                                                         rep(9,3),
                                                         rep(9,3),
                                                         rep(9,3),
                                                         rep(10,3),
                                                         rep(10,3),
                                                         rep(10,3),
                                                         rep(10,3),
                                                         rep(11,3),
                                                         rep(11,3),
                                                         rep(11,3),
                                                         rep(11,3)),
                                   ncol = 3,
                                   left = grid::textGrob(label = expression(""~Delta~italic(S)),
                                                         gp = gpar(fontfamily = 'serif'), rot = 90),
                                   bottom = grid::textGrob(label = "Effort (plot/individual)",
                                                           gp = gpar(fontfamily = 'serif')))
dev.off()

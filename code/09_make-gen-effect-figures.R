
here::i_am("code/09_make-gen-effect-figures.R")

# make final figures manuscript
# load in analysis data and assign names
load(here::here("analyses_output/output_analyses.RData"))
# load(here::here("sub-projects/restored-marsh-diversity/analyses_output/withinAmongAnalyses.rds"))
library(tidyverse)
library(grid)
library(gtable)
library(ggthemes)

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
delta_stats = rlist::list.subset(delta_stats, taxa_order)

comm = rlist::list.subset(comm, taxa_order); plot_attr = rlist::list.subset(plot_attr, taxa_order)
commMobIn = rlist::list.subset(commMobIn, taxa_order)

## set plotting aesthetics
site_order = c("LHA","LHB","LHC","WPH01","WPH02","PS07")

color_scheme = c("#01665e","#5ab4ac", "#ffffb2","#fed98e","#fe9929","#cc4c02")
color_scheme2 = c("#01665e","#5ab4ac","#ffffcc","#a1dab4","#41b6c4","#225ea8")
shape_scheme = c(22,22,21,21,21,21)
theme_set(theme_minimal())

group_names = c( "Surface\nMicrobes", "Below-Surface\nMicrobes","Plants", "Macroinfauna", "Spiders", 
                 "On-marsh\nnekton", "Full minnow","Off-marsh\nnekton")

# for all of the groups plot the three effects plots side-by-side

plot_gen_effects = function(delta_out = NULL, plot_annotation = NULL, axes_titles = FALSE,
                            plot_legend = FALSE, stat = 'b1',...){
  sDf = delta_out %>% pluck('mod_df') #%>% dplyr::filter(index == stat)
  
  agg_plot = sDf %>%
    dplyr::filter(test == "agg") %>%
    ggplot() +
    # geom_ribbon(aes(x = effort, ymin = low_value, ymax = high_value), alpha = 0.2)+
    geom_line(aes(x = effort, y = value, color = index),  size = 1.5)+
    geom_hline(aes(yintercept = 0), color = 'black', linetype = "dashed", size = 1.2)+
    theme(legend.position = c(0,1))
  
  if(!is.null(plot_annotation)){
    agg_plot = agg_plot +
      annotate('text', label = plot_annotation, x = 1, y = Inf, family = 'serif', size = 4,
               hjust = 0, vjust = 1, lineheight = 0.8)
  }
  
  
  
  N_plot = sDf %>%
    dplyr::filter( test == 'N') %>%
    ggplot()+
    # geom_ribbon(aes(x = effort, ymin = low_value, ymax = high_value), alpha = 0.2)+
    geom_line(aes(x = effort, y = value, color = index), size = 1.5)+
    geom_hline(aes(yintercept = 0), color = 'black', linetype = "dashed", size = 1.2)+
    theme(legend.position = 'none')
  
  SAD_plot = sDf %>%
    dplyr::filter( test == 'SAD') %>%
    ggplot()+
    # geom_ribbon(aes(x = effort, ymin = low_value, ymax = high_value), alpha = 0.2)+
    geom_line(aes(x = effort, y = value, color = index), size = 1.5)+
    geom_hline(aes(yintercept = 0), color = 'black', linetype = "dashed", size = 1.2)+
    theme(legend.position = 'none')  
  
  if(!axes_titles){
    agg_plot = agg_plot + theme(axis.title = element_blank())
    N_plot = N_plot + theme(axis.title = element_blank())
    SAD_plot = SAD_plot + theme(axis.title = element_blank())
    
  }
  
  agg_plot = ggplotGrob(agg_plot);N_plot = ggplotGrob(N_plot);SAD_plot = ggplotGrob(SAD_plot)
  
  gridExtra::grid.arrange(grobs = list(agg_plot, N_plot, SAD_plot), ncol = 3)
}

debugonce(plot_gen_effects)

genEffPlots = purrr::map2(delta_stats, group_names, ~plot_gen_effects(delta_out = .x, 
                                                       plot_annotation = .y,
                                                       plot_legend = TRUE
                                                       ))


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

png(here::here("figures/gen_effects.png"), width = 5, height = 10, units = "in", res = 480 )
fullPlot = gridExtra::grid.arrange(Agg, N, SAD, 
                                   genEffPlots[[1]],
                                   genEffPlots[[2]],
                                   genEffPlots[[3]],
                                   genEffPlots[[4]], 
                                   genEffPlots[[5]],
                                   genEffPlots[[6]], 
                                   genEffPlots[[8]],
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
                                                         rep(9,3),
                                                         rep(9,3),
                                                         rep(9,3),
                                                         rep(9,3),
                                                         rep(11,3),
                                                         rep(11,3),
                                                         rep(11,3),
                                                         rep(11,3)),
                                   ncol = 3,
                                   left = grid::textGrob(label = expression(''~Delta*S),
                                                         gp = gpar(fontfamily = 'serif'), rot = 90),
                                   bottom = grid::textGrob(label = expression("Effort (plot/individual)"),
                                                           gp = gpar(fontfamily = 'serif'))
)
dev.off()

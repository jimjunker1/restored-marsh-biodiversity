
# Make the Diversity plots #
here::i_am("code/06_make-rarefaction-figures.R")
load(here::here("analyses_output/output_analyses.RData"))

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

comm = rlist::list.subset(comm, taxa_order); plot_attr = rlist::list.subset(plot_attr, taxa_order)
commMobIn = rlist::list.subset(commMobIn, taxa_order)
delta_stats = rlist::list.subset(delta_stats, taxa_order)
mob_stats = setNames(mob_stats, nm = taxa_names)
mob_stats = rlist::list.subset(mob_stats, taxa_order)
## set plotting aesthetics

site_order = c("LHA","LHB","LHC","WPH1","WPH2","PS7")

color_scheme = c("#01665e","#5ab4ac", "#cfcf8a","#fed98e","#fe9929","#cc4c02")
color_scheme2 = c("#01665e","#5ab4ac","#ffffcc","#a1dab4","#41b6c4","#225ea8")
shape_scheme = c(22,22,21,21,21,21)
theme_set(theme_minimal())

group_names = c( "Surface\nMicrobes", "Below-Surface\nMicrobes","Plants", "Macroinfauna", "Spiders", 
                 "On-marsh\nnekton", "Full minnow","Off-marsh\nnekton")

# Plot code ####

### Figure 2 ----
#### Rarefaction curves

rare_plots = function(x, plot_legend=TRUE, axis_titles = FALSE, plot_annotation = NULL, plot_title = FALSE, ...){
  # agg = "sSBR", N = "nsSBR", SAD = "IBR"
  ind_dens = delta_stats %>%
    pluck(x) %>%
    pluck('density_stat') %>%
    pluck('ind_dens')
  
  df = delta_stats %>%
    pluck(x) %>%
    pluck('S_df') %>%
    dplyr::mutate(group =factor(group, levels = site_order)) #%>%
  # dplyr::mutate(effort = case_when((sample == "indiv" & test == "N") ~ round(effort/ind_dens),
  # TRUE ~ effort)) 
  
  upper_y = 10^(plyr::round_any(log10(max(df$S,na.rm = TRUE)), 0.1, f = ceiling))
  upper_x = 10^(plyr::round_any(log10(max(df$effort, na.rm = TRUE)), 0.1, f = ceiling))
  plot_x = df %>% dplyr::filter(sample == 'plot') %>% dplyr::select(effort) %>% max
  
  AGG_plot = df %>%
    dplyr::filter(test == 'agg') %>%
    ggplot(aes(x = effort, y = S, group = group, color = group, linetype =group))+
    geom_line(size = 1.1)+
    scale_x_continuous(trans = 'log2',
                       name = "Effort", limits = c(1, plot_x),
                       breaks = c(1,2,4,8,16,32,64))+
    scale_y_continuous(name = expression("Species richness ("~italic("S")~")"),
                       labels = scales::label_number_auto(),
                       limits = c(NA,upper_y))+
    annotate('text', label = plot_annotation, family = 'serif', x = 1, y = Inf,
             hjust = 0, vjust = 1, size = 4, lineheight = .8)+
    scale_color_manual(values = color_scheme)+
    scale_linetype_manual(values = c("dotted", "dotted", "solid", "solid", "solid", "solid"))+
    theme(legend.position =  'none',
          axis.title = element_blank(),
          # axis.text.y = element_blank(),
          panel.grid = element_blank(),
          #plot.margin = margin(t = 1, r = 0.01, b = 1, l = 0.01),
          axis.text.x = element_text(family= 'serif', size=8, colour="black"),
          axis.text.y = element_text(family= 'serif', size=8, colour="black"))
  
  if(plot_title){
    sSBR <- grid::grobTree(rectGrob(x=unit(0.5, "npc"), y=unit(1,"npc"),
                                    gp=gpar(fill="lightgrey", col = NA), vjust = 1, 
                                    width = unit(0.27, "npc"), height = unit(0.15, "npc")),
                           textGrob('sSBR', x=unit(0.5, "npc"), y=unit(0.99,"npc"),
                                    gp = gpar(fontfamily = 'serif', cex = 1.1),
                                    vjust =1))
    AGG_plot = AGG_plot + annotation_custom(sSBR)
  }
  
  N_plot = df %>%
    dplyr::filter(test == 'N') %>%
    ggplot(aes(x = effort, y = S, group = group, color = group, linetype =group))+
    geom_line(size = 1.1)+
    scale_x_continuous(trans = 'log2',
                       name = "Effort", limits = c(1, upper_x),
                       labels = c("1","10", "100", "1000", "1e4", "1e5"),
                       breaks = c(1,10, 100, 1e3, 1e4, 1e5))+
    scale_y_continuous(name = expression("Species richness ("~italic("S")~")"),
                       labels = scales::label_number_auto(),
                       limits = c(NA,upper_y), position = "right")+
    scale_color_manual(values = color_scheme)+
    scale_linetype_manual(values = c("dotted", "dotted", "solid", "solid", "solid", "solid"))+
    theme(legend.position =  'none',
          axis.title = element_blank(),
          axis.text.y = element_blank(),
          panel.grid = element_blank(),
          #plot.margin = margin(t = 1, r = 0.01, b = 1, l = 0.01),
          axis.text.x = element_text(family= 'serif', size=8, colour="black"))
  if(plot_title){
    nsSBR <- grid::grobTree(rectGrob(x=unit(0.5, "npc"), y=unit(1,"npc"),
                                     gp=gpar(fill="lightgrey", col = NA), vjust = 1, 
                                     width = unit(0.28, "npc"), height = unit(0.15, "npc")),
                            textGrob('nsSBR', x=unit(0.5, "npc"), y=unit(0.99,"npc"),
                                     gp = gpar(fontfamily = 'serif', cex = 1.1),
                                     vjust =1))
    N_plot = N_plot + annotation_custom(nsSBR)+coord_cartesian(clip = 'off')
  }
  
  SAD_plot = df %>%
    dplyr::filter(test == 'SAD') %>%
    ggplot(aes(x = effort, y = S, group = group, color = group, linetype =group))+
    geom_line(size = 1.1)+
    scale_x_continuous(trans = 'log2',
                       name = "Effort", limits = c(1, upper_x),
                       labels = c("1","10", "100", "1000", "1e4", "1e5"),
                       breaks = c(1,10, 100, 1e3, 1e4, 1e5))+
    scale_y_continuous(name = expression("Species richness ("~italic("S")~")"),
                       labels = scales::label_number_auto(),
                       limits = c(NA,upper_y))+
    scale_color_manual(values = color_scheme)+
    scale_linetype_manual(values = c("dotted", "dotted", "solid", "solid", "solid", "solid"))+
    # guides(color = guide_legend(byrow = TRUE, nrow = 1))+
    theme(legend.position = "none",
          axis.title = element_blank(),
          axis.text.y = element_blank(),
          panel.grid = element_blank(),
          #plot.margin = margin(t = 1, r = 0.01, b = 1, l = 0.01),
          axis.text.x = element_text(family= 'serif', size=8, colour="black"))#
          
  if(plot_title){
    IBR <- grid::grobTree(rectGrob(x=unit(0.5, "npc"), y=unit(1,"npc"),
                                   gp=gpar(fill="lightgrey", col = NA), vjust = 1, 
                                   width = unit(0.21, "npc"), height = unit(0.15, "npc")),
                          textGrob('IBR', x=unit(0.5, "npc"), y=unit(0.99,"npc"),
                                   gp = gpar(fontfamily = 'serif', cex = 1.1),
                                   vjust =1))
    SAD_plot = SAD_plot + annotation_custom(IBR)
  }
  
  # AGG_plot = ggplotGrob(AGG_plot); N_plot = ggplotGrob(N_plot); SAD_plot = ggplotGrob(SAD_plot)
  
  
  
  # g = cbind(AGG_plot, N_plot, SAD_plot, size = "max")
  
  if(axis_titles & plot_annotation != "Spiders"){
    AGG_plot = ggplotGrob(AGG_plot); N_plot = ggplotGrob(N_plot); SAD_plot = ggplotGrob(SAD_plot)
    
    full_plot = gridExtra::arrangeGrob(grobs = list(AGG_plot, N_plot, SAD_plot), nrow = 1, 
                                       left = textGrob(label = expression('Taxa richness ('~italic("S")~")"),
                                                       gp = gpar(fontfamily = 'serif'), rot = 90),
                                       bottom = textGrob(label = "Sampling effort (plot/individual)",
                                                         gp = gpar(fontfamily = 'serif')))
  } else if(axis_titles == FALSE & plot_annotation != "Spiders"){
    AGG_plot = ggplotGrob(AGG_plot); N_plot = ggplotGrob(N_plot); SAD_plot = ggplotGrob(SAD_plot)
    
    full_plot = gridExtra::arrangeGrob(grobs = list(AGG_plot, N_plot, SAD_plot), nrow = 1)
  } else if(plot_annotation == "Spiders"){
    AGG_plot = AGG_plot +
      scale_color_manual(values = rep('white',6))+
      theme(axis.text = element_text(color = 'white'))
    
    AGG_plot = ggplotGrob(AGG_plot); N_plot = ggplotGrob(N_plot); SAD_plot = ggplotGrob(SAD_plot)
    
    full_plot = gridExtra::arrangeGrob(grobs = list(AGG_plot, N_plot, SAD_plot), nrow = 1)
    
  }
  
  return(full_plot)
}

# debugonce(rare_plots)
# gridExtra::grid.arrange(rare_plots(names(delta_stats[6]), plot_legend = TRUE, plot_titles = FALSE, plot_annotation = "A"))

group_names = c( "Surface\nMicrobes", "Below-Surface\nMicrobes","Plants", "Macroinfauna", "Spiders", 
                 "On-marsh\nnekton", "Full minnow","Off-marsh\nnekton")

legend_pos = c(F, F, F, F, F, F, F, F)
plot_title = c(F, F, F, F, F, F, F, F)
plotList = purrr::pmap(list(names(delta_stats),legend_pos, group_names, plot_title),
                       ~rare_plots(..1, plot_legend = ..2, 
                                   axis_titles = FALSE,
                                   plot_annotation = ..3,
                                   
                                   plot_title = ..4))

plot_legend = 
  cowplot::get_legend(delta_stats[[1]] %>%
                        pluck("S_df") %>%
                        dplyr::filter(test == 'agg') %>%
                        dplyr::mutate(group =factor(group, levels = site_order)) %>%
                        ggplot(aes(x = effort, y = S, group = group, 
                                   color = group, linetype =group))+
                        geom_line(size = 1.1)+
                        guides(color = guide_legend(byrow = TRUE, nrow = 1))+
                        scale_color_manual(values = color_scheme)+
                        scale_linetype_manual(
                          values = c("dotted", "dotted", "solid",
                                     "solid", "solid", "solid"))+
                        theme(legend.title = element_blank(),
                              legend.text = element_text(family = 'serif'),
                              legend.margin= margin(c(t = 1,r = 5,b = 1,l = 5)) ))

sSBR <- grid::grobTree(#rectGrob(x=unit(0.5, "npc"), y=unit(1,"npc"),
  # gp=gpar(fill="lightgrey", col = NA), vjust = 1, 
  # width = unit(0.27, "npc"), height = unit(1, "npc")),
  textGrob('sSBR', x=unit(0.5, "npc"), #y=unit(0.99,"npc"),
           gp = gpar(fontfamily = 'serif', cex = 1.1),
           vjust =0))
nsSBR <- grid::grobTree(#rectGrob(x=unit(0.5, "npc"), y=unit(1,"npc"),
  # gp=gpar(fill="lightgrey", col = NA), vjust = 1, 
  # width = unit(0.28, "npc"), height = unit(1, "npc")),
  textGrob('nsSBR', x=unit(0.5, "npc"), #y=unit(0.99,"npc"),
           gp = gpar(fontfamily = 'serif', cex = 1.1),
           vjust =0))
IBR <- grid::grobTree(#rectGrob(x=unit(0.5, "npc"), y=unit(1,"npc"),
  # gp=gpar(fill="lightgrey", col = NA), vjust = 1, 
  # width = unit(0.21, "npc"), height = unit(1, "npc")),
  textGrob('IBR', x=unit(0.5, "npc"), #y=unit(0.99,"npc"),
           gp = gpar(fontfamily = 'serif', cex = 1.1),
           vjust =0))

# gridExtra::grid.arrange(sSBR, nsSBR, IBR, plotList[[5]],
#              ncol = 3, layout_matrix = rbind(c(1,2,3),
#                                              c(4,4,4),
#                                              c(4,4,4),
#                                              c(4,4,4),
#                                              c(4,4,4),
#                                              c(4,4,4),
#                                              c(4,4,4),
#                                              c(4,4,4),
#                                              c(4,4,4),
#                                              c(4,4,4),
#                                              c(4,4,4),
#                                              c(4,4,4),
#                                              c(4,4,4),
#                                              c(4,4,4),
#                                              c(4,4,4),c(4,4,4)))

# removed full minnow community; plotList[[5]]
png(here::here("figures/rarefactions.png"), width = 6, height = 10, units = "in", res = 480 )
fullPlot = gridExtra::grid.arrange(plot_legend,sSBR,nsSBR, IBR, 
                                   plotList[[1]], plotList[[2]], 
                                   plotList[[3]], plotList[[4]],
                                   plotList[[5]], plotList[[6]], 
                                   plotList[[8]],
                                   layout_matrix = rbind(c(NA,1,NA),
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
                                                         rep(8,3),rep(8,3),
                                                         rep(8,3),
                                                         rep(8,3),
                                                         rep(9,3),
                                                         rep(9,3),
                                                         rep(9,3),rep(9,3),
                                                         rep(10,3),rep(10,3),
                                                         rep(10,3),
                                                         rep(10,3),
                                                         rep(11,3),rep(11,3),
                                                         rep(11,3),
                                                         rep(11,3)),
                                   ncol = 3,
                                   left = grid::textGrob(label = expression('Taxa richness ('~italic("S")~")"),
                                                         gp = gpar(fontfamily = 'serif'), rot = 90),
                                   bottom = grid::textGrob(label = "Sampling effort (plot/individual)",
                                                           gp = gpar(fontfamily = 'serif')))
dev.off()


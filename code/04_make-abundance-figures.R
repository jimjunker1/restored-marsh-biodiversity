# Load all the community matrices

# Due to bug in source() function. 01_load-data.R script must be run by hand because of differential encoding and special characters treatment in`fix_latlong()`
source(here::here("code/03_analyze-beta-partition.R"))
here::i_am("code/04_make-abundance-figures.R")

# make final figures manuscript
# load in analysis data and assign names
load(here::here("analyses_output/output_analyses.RData"))
# load(here::here("sub-projects/restored-marsh-diversity/analyses_output/withinAmongAnalyses.rds"))
library(tidyverse)
library(grid)
library(gtable)
library(ggthemes)
library(dendextend)
library (scales)
library (cowplot)
library (gridExtra)


### New code 
main_titles <- c("Plants", "Surface microbes", "Below-surface microbes",
                 "On-marsh nekton", "Minnow", "Off-marsh nekton", "Macroinfauna",
                 "Spiders")
color_scheme = c("#01665e","#5ab4ac", "#ffffb2","#fed98e","#fe9929","#cc4c02")

res <- list ()
for (i in 1:length(main_titles)){
  res [[i]]<- data.frame (Ab = comm[[i]] %>% apply(., 1, sum),
                          Marsh = plot_attr [[i]]$marsh_id,
                          Assemblage = main_titles [[i]])
}

df <- do.call ("rbind", res)
lv <- c("Surface microbes", "Below-surface microbes",
"Plants", "Macroinfauna", "Spiders", "On-marsh nekton",
"Off-marsh nekton")
df <- df %>%
  filter (Assemblage != "Minnow") %>%
  mutate (Assemblage = factor (Assemblage, 
                               levels = lv),
          Marsh = factor(Marsh,
                         levels = c("LHA","LHB","LHC","WPH1","WPH2","PS7"))) 

gg_plots <- list()
breaks = list (surf_micr = c(20000,40000, 80000, 160000),
               Bsurf_micr = c(70000, 110000, 140000, 170000),
               Plants = c(200, 1000, 5000, 11000),
               Macroinfauna = c(0, 10, 50, 200),
               Spiders = c(0, 4, 20, 100),
               OnMarshNek = c(0, 10, 200, 2000),
               OffMarshNek = c(0,10, 200, 2600))

colour_text = c(rep("black",2), rep("gray40",4))
font_text = c(rep("bold.italic",2), rep("bold",4))
#scientific_notation <- c(TRUE,TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)
for (i in 1:length(lv)){
  gg_plots [[i]] <- df %>%
    filter (Assemblage == lv[i]) %>%
    ggplot(., aes (x=Marsh,y=Ab))+
    theme(axis.title = element_blank(),
          axis.text.y =element_text(colour="black", angle = 90),
          axis.text.x =element_text(colour=colour_text, face=font_text),
          panel.background = element_rect(fill = NA),
          panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                          colour = "gray95"))+
    geom_boxplot(fill= color_scheme)+
    scale_y_continuous(trans='log1p',
                       breaks = breaks[[i]],
                       limits = c(min(breaks[[i]]),
                                  max(breaks[[i]])))+ #,labels = function(x) format(x, scientific = scientific_notation[i])
    labs (y="Abundance", x="Marsh")+
    ggtitle(lv[i])
}

P <- plot_grid (gg_plots [[1]],gg_plots [[2]],gg_plots [[3]],
         gg_plots [[4]], gg_plots [[5]], gg_plots [[6]],
         gg_plots [[7]])
y.grob <- textGrob("Taxa abundance", 
                   gp=gpar(col="black", fontsize=16), rot=90)
x.grob <- textGrob("Marsh", 
                   gp=gpar(col="black", fontsize=16))

tiff(file=here::here("figures/abundances2.tiff"),
     width=3000,height=3000,units = "px",
     compression="lzw",res=300)
grid.arrange(arrangeGrob(P, left = y.grob, bottom= x.grob))
dev.off()

# 
# # taxa grouping orders: Plants, Microbes0-2, Microbes8-10, Infauna, Spiders, Pond minnow, Off-marsh
# taxa_names <- c("plants", "microbes_0.2", "microbes_8.10", "pond_minnow", "minnow", "trawl", "infauna", "spiders")
# comm = setNames(comm, taxa_names)
# 
# taxa_order = c("microbes_0.2", "microbes_8.10", "plants", "infauna", "spiders", "pond_minnow",
#                "minnow", "trawl")
# 
# marsh_attr = data.frame(marsh_id = c("LHA","LHB","LHC","WPH01","WPH02","PS07"),
#                         group_id = c("restored","restored","natural","natural","natural","natural")
# ) %>% dplyr::mutate(marsh_id = factor(marsh_id, levels = c("LHA","LHB","LHC","WPH01","WPH02","PS07")))
# 
# plot_attr = setNames(plot_attr, taxa_names)
# plot_attr = purrr::map(plot_attr, ~.x %>% left_join(marsh_attr))
# 
# commMobIn = purrr::map2(comm,plot_attr, ~mobr::make_mob_in(.x, .y, coord_names = c('long_dd','lat_dd'),
#                                                             latlong = TRUE))
# delta_stats = setNames(delta_stats, nm = taxa_names)
# 
# comm = rlist::list.subset(comm, taxa_order); plot_attr = rlist::list.subset(plot_attr, taxa_order)
# commMobIn = rlist::list.subset(commMobIn, taxa_order)
# delta_stats = rlist::list.subset(delta_stats, taxa_order)
# 
# ## set plotting aesthetics
# site_order = c("LHA","LHB","LHC","WPH01","WPH02","PS07")
# 
# color_scheme = c("#01665e","#5ab4ac", "#ffffb2","#fed98e","#fe9929","#cc4c02")
# color_scheme2 = c("#01665e","#5ab4ac","#ffffcc","#a1dab4","#41b6c4","#225ea8")
# shape_scheme = c(22,22,21,21,21,21)
# theme_set(theme_minimal())
# 
# ### Figure 2 ----
# #### Abundance boxplots
# 
# plot_abun = function(mob_in = NULL, plot_annotation = NULL, plot_legend = TRUE, plot_titles = FALSE,
#                      plot_text = TRUE, x_annotation = NULL,...){
#   Ndf = mob_in %>%
#     pluck("comm") %>%
#     apply(., 1, sum) %>%
#     bind_cols(mob_in %>% pluck("env"))%>%
#     setNames(., nm = c("N","marsh_id", "group_id")) %>%
#     dplyr::mutate(marsh_id =factor(marsh_id, levels = site_order))
# 
# 
#   N_plot = Ndf %>%
#     group_by(marsh_id) %>%
#     dplyr::summarise(lower = quantile(N,0.25, na.rm = TRUE),
#                      upper = quantile(N, 0.75, na.rm = TRUE),
#                      N = median(N, na.rm = TRUE)) %>%
#     ggplot()+
#     geom_errorbar(aes(x = marsh_id, ymin = log10(lower), ymax = log10(upper)), width = 0, size = 0.8)+
#     geom_point(aes(x = marsh_id, y = log10(N), fill = marsh_id), shape = 21, size = 3)+
#     scale_fill_manual(values = color_scheme)+
#     annotate('text', label = plot_annotation, family = 'serif', x = x_annotation[1], y = Inf,
#              hjust = x_annotation[2], vjust = 1, size = 4,lineheight = .8)+
#     scale_y_continuous(breaks = log10(c(1,5,10,50,100,500,1000,5000,10000,50000,100000,500000)),
#                        labels = 10^(log10(c(1,5,10,50,100,500,1000,5000,10000,50000,100000,500000))))+
#     scale_x_discrete(expand = c(0.05,0.01))+
#     guides(fill = guide_legend(byrow = TRUE, nrow = 1))+
#     geom_rangeframe(aes(x = marsh_id, y = log10(N)), sides = "lb", inherit.aes = FALSE)+
#     coord_cartesian(clip = 'off')+
#     theme_tufte()+
#     theme(legend.title = element_blank())
# 
#   if(!plot_legend){
#     N_plot = N_plot + theme(legend.position = 'none')
#   }
#   if(!plot_titles){
#     N_plot = N_plot + theme(axis.title = element_blank())
#   }
#   if(!plot_text){
#     N_plot = N_plot + theme(axis.text.x = element_blank())
#   }
#     return(ggplotGrob(N_plot))
# }
# # debugonce(plot_abun)
# 
# group_names = c( "Surface\nMicrobes", "Below-Surface\nMicrobes","Plants", "Macroinfauna", "Spiders",
#                 "On-marsh\nnekton", "Full minnow","Off-marsh\nnekton")
# legend_pos = c(F, F, F, F, F, F, F, F)
# plot_title = c(F, F, F, F, F, F, F, F)
# plot_text = c(F, F, F, F, F, F, F, T)
# annotation_xpos = list(c(Inf,1), c(Inf,1),c(Inf,1), c(1,0), c(Inf,1), c(Inf,1), c(Inf,1), c(Inf,1) )
# 
# 
# 
# nPlotsList = purrr::pmap(list(commMobIn, legend_pos, group_names, plot_title, plot_text, annotation_xpos), ~plot_abun(..1, plot_legend = ..2,
#                                                                           plot_annotation = ..3, plot_titles = ..4,
#                                                                           plot_text = ..5, x_annotation = ..6))
# legend = cowplot::get_legend(plot_abun(commMobIn[[8]], plot_legend = TRUE, plot_annotation = "plants"))
# 
# maxWidth = grid::unit.pmax(nPlotsList[[1]]$widths[2:5], nPlotsList[[2]]$widths[2:5],
#                            nPlotsList[[3]]$widths[2:5], nPlotsList[[4]]$widths[2:5],
#                            nPlotsList[[5]]$widths[2:5], nPlotsList[[6]]$widths[2:5],
#                            nPlotsList[[7]]$widths[2:5], nPlotsList[[8]]$widths[2:5])
# 
# 
# nPlotsList[[1]]$widths[2:5]<-nPlotsList[[2]]$widths[2:5]<-nPlotsList[[3]]$widths[2:5]<-nPlotsList[[4]]$widths[2:5]<-nPlotsList[[5]]$widths[2:5]<-nPlotsList[[6]]$widths[2:5]<-nPlotsList[[7]]$widths[2:5]<-nPlotsList[[8]]$widths[2:5] <- as.list(maxWidth)
# 
# png("./sub-projects/restored-marsh-diversity/figures/abundances.png", width = 4.5, height = 10, units = "in", res = 480 )
# fullPlot = gridExtra::grid.arrange(nPlotsList[[1]],
#                                    nPlotsList[[2]],
#                                    nPlotsList[[3]],
#                                    nPlotsList[[4]],
#                                    nPlotsList[[5]],
#                                    nPlotsList[[6]],
#                                    nPlotsList[[8]],
#                                    layout_matrix = rbind(1,
#                                                          1,
#                                                          1,
#                                                          1,
#                                                         2,
#                                                         2,
#                                                         2,
#                                                         2,
#                                                         3,
#                                                         3,
#                                                         3,
#                                                         3,
#                                                         4,
#                                                         4,
#                                                         4,
#                                                         4,
#                                                         5,
#                                                         5,
#                                                         5,
#                                                         5,
#                                                         6,
#                                                         6,
#                                                         6,
#                                                         6,
#                                                         7,
#                                                         7,
#                                                         7,
#                                                         7),
# 
#                                    ncol = 1,
#                                    left = grid::textGrob(label = expression('Species abundance'),
#                                                          gp = gpar(fontfamily = 'serif'), rot = 90)
# )
# dev.off()

#### Spare(d) code #####
# rare_plots = function(x, plot_legend=TRUE, plot_titles = FALSE, plot_annotation = NULL, ...){
#   df = delta_stats %>%
#     pluck(x) %>%
#     pluck('S_df') %>%
#     dplyr::mutate(group =factor(group, levels = site_order))
#   
#   upper_y = 10^(plyr::round_any(log10(max(df$S,na.rm = TRUE)), 0.1, f = ceiling))
#   upper_x = 10^(plyr::round_any(log10(max(df$effort, na.rm = TRUE)), 0.1, f = ceiling))
#   plot_x = df %>% dplyr::filter(sample == 'plot') %>% dplyr::select(effort) %>% max
#   
#   SAD_plot = df %>%
#     dplyr::filter(test == 'SAD') %>%
#     ggplot(aes(x = effort, y = S, group = group, color = group, linetype =group))+
#     geom_line(size = 1.1)+
#     scale_x_continuous(name = "Effort", limits = c(0.1, upper_x), 
#                        breaks = c(0,1,10, 100, 1000, 10000))+
#     coord_trans(x = 'log2') +
#     scale_y_continuous(name = expression("Species richness ("~italic("S")~")"),
#                        limits = c(0,upper_y))+
#     scale_color_manual(values = color_scheme)+
#     scale_linetype_manual(values = c("dotted", "dotted", "solid", "solid", "solid", "solid"))+
#     theme(legend.position =  'none',
#           axis.title = element_blank(),
#           axis.text.y = element_blank())
#   
#   AGG_plot = df %>%
#     dplyr::filter(test == 'agg') %>%
#     ggplot(aes(x = effort, y = S, group = group, color = group, linetype =group))+
#     geom_line(size = 1.1)+
#     scale_x_continuous(name = "Effort", limits = c(1, plot_x),
#                        breaks = c(1,2,4,8,12,20,32))+
#     coord_trans(x = 'log2') +
#     scale_y_continuous(name = expression("Species richness ("~italic("S")~")"),
#                        limits = c(0,upper_y))+
#     guides(color = guide_legend(byrow = TRUE, nrow = 1))+
#     scale_color_manual(values = color_scheme)+
#     scale_linetype_manual(values = c("dotted", "dotted", "solid", "solid", "solid", "solid"))+
#     theme(legend.title = element_blank(),
#           legend.text = element_text(family = 'serif'),
#           axis.title = element_blank(),
#           # axis.text.y = element_blank(),
#           legend.position = c(0,1),
#           legend.justification = c(0,1))
#   
#   if(!plot_legend){
#     AGG_plot = AGG_plot + theme(legend.position = 'none')
#   }
#   
#   N_plot = df %>%
#     dplyr::filter(test == 'N') %>%
#     ggplot(aes(x = effort, y = S, group = group, color = group, linetype =group))+
#     geom_line(size = 1.1)+
#     scale_x_continuous(name = "Effort", limits = c(0.1, upper_x),
#                        breaks = c(1,10, 100, 1000, 10000,NA))+
#     coord_trans(x = 'log2') +
#     scale_y_continuous(name = expression("Species richness ("~italic("S")~")"),
#                        limits = c(0,upper_y), position = "right")+
#     scale_color_manual(values = color_scheme)+
#     scale_linetype_manual(values = c("dotted", "dotted", "solid", "solid", "solid", "solid"))+
#     annotate('text', label = plot_annotation, family = 'serif', x = Inf, y = Inf,
#              hjust = 1, vjust = 1, size = 4)+
#     theme(legend.position =  'none',
#           axis.title = element_blank(),
#           axis.text.y = element_blank())
#   
#   if(plot_titles){
#     full_plot = gridExtra::arrangeGrob(grobs = list(AGG_plot, SAD_plot, N_plot), nrow = 1, 
#                                        left = textGrob(label = expression('Species richness ('~italic("S")~")"),
#                                                        gp = gpar(fontfamily = 'serif'), rot = 90),
#                                        bottom = textGrob(label = "Effort (plot/individual)",
#                                                          gp = gpar(fontfamily = 'serif')))
#   } else if(plot_titles == FALSE){
#     full_plot = gridExtra::arrangeGrob(grobs = list(AGG_plot, SAD_plot, N_plot), nrow = 1)
#   }
#   
#   return(full_plot)
# }
# 
# diversities_plot = function(x = NULL, plot_titles = FALSE, plot_legend = FALSE, plot_annotation = NULL,
#                             inset_ymax = NULL, ...){
#   b_df = mob_diversities %>%
#     pluck(x) %>%
#     dplyr::filter(index == 'beta_S')
#   beta_ymax = max(b_df$value, na.rm = TRUE)+(0.2*max(b_df$value, na.rm=TRUE))
#   beta_plot = ggplotGrob(b_df %>%
#                            ggplot(aes(x = group, y = value))+
#                            geom_boxplot(aes(fill = group))+
#                            scale_y_continuous(name = expression(''~beta*"-diversity"),
#                                               limits = c(0,beta_ymax), expand = c(0.01,0.01),
#                                               breaks = c(0,1,2,4,8,14,22)) +
#                            scale_fill_manual(values = color_scheme)+
#                            geom_rangeframe(sides = "lb")+
#                            theme(legend.position = 'none',
#                                  panel.grid = element_blank(),
#                                  plot.background = element_rect(color = "black"),
#                                  axis.title.x = element_blank(),
#                                  axis.text.x = element_blank())#element_text(angle = 45, vjust = 1, hjust = 1))
#   )
#   
#   
#   # g = grobTree(beta_plot, width= unit(0.01,"in"),height=unit(0.01,"in"),
#   # x=1, y=0,just = c("right", "bottom"))
#   ag_df = mob_summaries %>%
#     pluck(x)
#   ag_ymax = max(ag_df$gamma_S_median, na.rm = TRUE)+(0.6*max(ag_df$gamma_S_median, na.rm = TRUE))
#   if(min(ag_df$gamma_S_median, na.rm = TRUE)-(0.6*min(ag_df$gamma_S_median, na.rm = TRUE)) >= 2){
#     ag_ymin = min(ag_df$gamma_S_median, na.rm = TRUE)-(0.6*min(ag_df$gamma_S_median, na.rm = TRUE))
#   } else{ ag_ymin = 0}
#   jitter_height = abs(ag_ymax - ag_ymin)*0.01
#   
#   ag_xmax = max(ag_df$S_quant75, na.rm = TRUE)+(0.5*max(ag_df$S_quant75, na.rm = TRUE))
#   if(min(ag_df$S_quant25, na.rm = TRUE)-(0.5*min(ag_df$S_quant25, na.rm = TRUE)) >= 2){
#     ag_xmin =min(ag_df$S_quant25, na.rm = TRUE)-(0.5*min(ag_df$S_quant25, na.rm = TRUE))
#   } else{ag_xmin = 0}
#   
#   if(grep("microbes", plot_annotation, ignore.case = TRUE)){
#     ag_ymax = max(ag_df$gamma_S_median, na.rm = TRUE)+(0.2*max(ag_df$gamma_S_median, na.rm = TRUE))
#     if(min(ag_df$gamma_S_median, na.rm = TRUE)-(0.2*min(ag_df$gamma_S_median, na.rm = TRUE)) >= 2){
#       ag_ymin = min(ag_df$gamma_S_median, na.rm = TRUE)-(0.2*min(ag_df$gamma_S_median, na.rm = TRUE))
#     } else{ ag_ymin = 0}
#     jitter_height = abs(ag_ymax - ag_ymin)*0.01
#     
#     
#     
#     ag_xmax = max(ag_df$S_quant75, na.rm = TRUE)+(0.2*max(ag_df$S_quant75, na.rm = TRUE))
#     if(min(ag_df$S_quant25, na.rm = TRUE)-(0.2*min(ag_df$S_quant25, na.rm = TRUE)) >= 2){
#       ag_xmin =min(ag_df$S_quant25, na.rm = TRUE)-(0.2*min(ag_df$S_quant25, na.rm = TRUE))
#     } else{ag_xmin = 0}
#   }
#   
#   ag_df %>%   
#     dplyr::mutate(group = factor(group, levels = site_order)) %>%
#     ggplot(aes(x = S_median, y = gamma_S_median))+
#     geom_abline(alpha = 0.8, color = 'grey', size = 1.2)+
#     geom_errorbarh(aes(color = group, xmin = S_quant25, xmax = S_quant75, linetype = group), height = 0, size = 1.5)+
#     geom_point(aes(fill = group, shape = group),size = 2.2)+
#     scale_y_continuous(name = expression(""~gamma*"-diversity"),
#                        limits = c(ag_ymin,ag_ymax), expand = c(0.01,0.01))+
#     scale_x_continuous(name = expression(""~alpha*"-diversity"),
#                        limits = c(NA,ag_xmax), expand = c(0.01,0.01))+
#     scale_fill_manual(values = color_scheme)+
#     scale_color_manual(values = color_scheme)+
#     scale_shape_manual(values = shape_scheme)+
#     scale_linetype_manual(
#       values = c("dotted", "dotted", "solid",
#                  "solid", "solid", "solid"))+
#     guides(color = guide_legend(nrow = 2),
#            fill = guide_legend(nrow = 2))+
#     geom_rangeframe(sides = "lb")+
#     annotate('text', label = plot_annotation, family = 'serif', size = 4,
#              x = Inf, y = Inf, hjust = 1, vjust = 1, lineheight = 0.8)+
#     theme(#panel.border = element_rect(color = 'black', size = 0.5, linetype = 'solid'),
#       panel.grid = element_blank(),
#       legend.title = element_blank(),
#       legend.position = c(1,1),
#       legend.justification = c(1,1)) -> ag_plot
#   
#   if(plot_titles == FALSE){
#     ag_plot = ag_plot + theme(axis.title = element_blank())
#   }
#   if(plot_legend == FALSE){
#     ag_plot = ag_plot + theme(legend.position = 'none')
#   }
#   
#   full_plot = 
#     cowplot::ggdraw()+
#     cowplot::draw_plot(ag_plot) +
#     cowplot::draw_plot(beta_plot, x = 0.99, y = 0.1, width = .4, height = inset_ymax,
#                        hjust = 1, vjust = 0)
#   return(ggplotGrob(full_plot))
# }
# 
# 
# # quick plot of the relative partitions
# # tiff(here::here("sub-projects/restored-marsh-diversity/figures/diversit_relpart_plot.tiff"), res = 300, height = 8.4,width=11, unit='in', compression = 'lzw')
# oldGroupNames = unique(diversity_relabun_df$community_id)
# oldGroupsToKeep = oldGroupNames[c(1,3,6,8,9,11)]
# newGroupToKeep = c("Infauna", "Microbes (0-2 cm)", "Microbes (8-10 cm)", "Minnow", "Plants", "Trawl")
# groupKeyVal = setNames(newGroupToKeep, nm = oldGroupsToKeep)

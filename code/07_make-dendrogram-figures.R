# Dendrograms
here::i_am("code/07_make-dendrogram-figures.R")

load(here::here("analyses_output/output_analyses.RData"))
# load(here::here("sub-projects/restored-marsh-diversity/analyses_output/withinAmongAnalyses.rds"))
library(tidyverse)
library(grid)
library(gtable)
library(ggthemes)
library(ggdendro)
library(dendextend)
library (cowplot)
library(ggtext)
library(gridExtra)

#Parameters
main_titles <- c("Plants", "Surface microbes", "Below-surface microbes",
                 "On-marsh nekton", "Minnow", "Off-marsh nekton", "Macroinfauna",
                 "Spiders")

ggs <- list ()
for (i in 1:length(bray_cent_dist)){
  ddata_x <- bray_cent_dist[[i]] %>% hclust(method = "ward.D") %>%
    as.dendrogram () %>%
    dendro_data ()
  
  labs <- label(ddata_x)
  ddata_x$labels$marsh_type <- ifelse ( labs$label == "LHA" | labs$label == "LHB", "Created", "Natural") %>%
    factor()
  
  ggs [[i]] <- ggplot(segment(ddata_x)) +
    geom_segment(aes(x=x, y=y, xend=xend, yend=yend)) +
    geom_richtext(data=label(ddata_x),
              aes(label=label, x=x, y=0.05, colour=marsh_type,
                  fontface="bold"),
              size = 5, angle = 90, label.size=1.2)+#, 
    ylim(-0.05,0.73) +
    theme (legend.position="none",
           panel.background = element_rect(fill = "white",
                                           colour = "white"),
           axis.title = element_blank(),
           axis.text.x = element_blank(),
           axis.text.y = element_text(size=12),
           axis.ticks.x = element_blank(),
           plot.title = element_text(size = 16))+
    ggtitle (main_titles[i])
}

tiff(file=here::here("figures/clusters2.tiff"),width=4200,
     height=3000,units = "px",
     compression="lzw",res=300)
P <- plot_grid (ggs[[2]],
           ggs[[3]],
           ggs[[1]],
           ggs[[7]],
           ggs[[8]],
           ggs[[4]],
           ggs[[6]],
           nrow=3, ncol=3,
           label_size = 16)
y.grob <- textGrob("Bray-Curtis distance", 
                   gp=gpar(col="black", fontsize=16), rot=90) #fontface="bold",
grid.arrange(arrangeGrob(P, left = y.grob))

dev.off()

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
#                                                            latlong = TRUE))
# delta_stats = setNames(delta_stats, nm = taxa_names)
# 
# comm = rlist::list.subset(comm, taxa_order); plot_attr = rlist::list.subset(plot_attr, taxa_order)
# commMobIn = rlist::list.subset(commMobIn, taxa_order)
# delta_stats = rlist::list.subset(delta_stats, taxa_order)
# 
# ## set plotting aesthetics
# 
# site_order = c("LHA","LHB","LHC","WPH01","WPH02","PS07")
# 
# color_scheme = c("#01665e","#5ab4ac", "#ffffb2","#fed98e","#fe9929","#cc4c02")
# color_scheme2 = c("#01665e","#5ab4ac","#ffffcc","#a1dab4","#41b6c4","#225ea8")
# shape_scheme = c(22,22,21,21,21,21)
# theme_set(theme_minimal())
# 
# group_names = c( "Surface\nMicrobes", "Below-Surface\nMicrobes","Plants", "Macroinfauna", "Spiders", 
#                  "On-marsh\nnekton", "Full minnow","Off-marsh\nnekton")
# 
# # Plot code #####
# #### Distance dendograms -----
# 
# bray_cent_dist = setNames(bray_cent_dist, nm= taxa_names) %>% rlist::list.subset(., taxa_order)
# 
# bcCluster = purrr::map(bray_cent_dist, ~hclust(.x, method = 'ward.D'))
# 
# plot_dendro = function(clust, grpName = NULL, color = FALSE,...){
#   #set plotting attributes
#   site_order = c("LHA","LHB","LHC","WPH01","WPH02","PS07")
#   label_scheme = c("LHA\n(created)","LHB\n(created)","LHC\n(reference)","WPH01\n(reference)",
#                    "WPH02\n(reference)","PS07\n(reference)")
#   style_scheme = c("bold.italic","bold.italic", "plain","plain","plain","plain")
#   
#   labelKey = setNames(label_scheme, site_order)
#   styleKey = setNames(style_scheme, site_order)
#   
#   if(color){
#     color_scheme = c("#01665e","#5ab4ac", "#ffffb2","#fed98e","#fe9929","#cc4c02")
#     colorKey = setNames(color_scheme, site_order)
#   }
#   
#   x = clust
#   # labels(x) <- labelKey[labels(clust)]
#   
#   styles = styleKey[labels(clust)]
#   
#   x %>% ggdendrogram(size = 5) +
#     scale_y_continuous(name = 'Bray-Curtis distance', limits = c(0,0.8), breaks = c(0,0.25,0.5,0.75), expand = c(0,0.1)) +
#     annotate('text', label = grpName, family = 'serif', x = 1, y = Inf, hjust = 0,
#              vjust= 1,size = 2.5, lineheight = 0.8)+
#     theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 7, face = styles),
#           axis.text.y = element_text(size = 6),
#           axis.text = element_text(family = 'serif'),
#           plot.margin = margin(t = 0.01, r = 0.1, b = 0.01, l = 0.1))
#   
#   
#   
# }
# # debugonce(plot_dendro)
# dendroPlots = purrr::map2(bcCluster,group_names, ~plot_dendro(clust = .x, grpName = .y))
# 
# # dendroPlots[[1]]
# 
# 
# png("./sub-projects/restored-marsh-diversity/figures/clusters.png", width = 6, height = 4, units = "in", res = 480 )
# fullPlot = gridExtra::grid.arrange(dendroPlots[[1]], 
#                                    dendroPlots[[2]], 
#                                    dendroPlots[[3]],
#                                    dendroPlots[[4]], 
#                                    dendroPlots[[5]],
#                                    dendroPlots[[6]], 
#                                    dendroPlots[[8]],
#                                    layout_matrix = rbind(c(1,2,3),
#                                                          c(4,5,6),
#                                                          c(7,NA,NA)),
#                                    ncol = 3,
#                                    left = grid::textGrob(label = expression("Bray-Curtis distance"),
#                                                          gp = gpar(fontfamily = 'serif', size = 6), rot = 90))
# dev.off()

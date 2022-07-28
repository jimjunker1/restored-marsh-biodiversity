here::i_am("code/11_make_SAD_plots.R")
require (ggplot2)
require (cowplot)

marshes <- c("LHA","LHB","LHC","WPH1","WPH2","PS7")
assemblages_number <- c(2,3,1,7,8,4,6)
assemblages <- c("Surface microbes", "Below-surface microbes", "Plants",
                 "Macroinfauna", "Spiders", "On-marsh nekton", "Off-marsh nekton" )

ggs <- list()
dfs <- list()

ct <- 1
for (k in assemblages_number){
  for (i in 1:length(marshes)){
    comm_sub <- comm[[k]]
    plot_attr_sub <- plot_attr [[k]]
    
    comm_sub <- comm_sub [which (plot_attr_sub$marsh_id == marshes[i]), ]
    
    Total <- colSums(comm_sub) %>% sum ()
    
    dfs [[i]] <- data.frame (Rel_proportion = (comm_sub %>% colSums() / Total) %>% sort(decreasing = TRUE),
                             Rank = 1:ncol (comm_sub),
                             Marsh = marshes[i],
                             Marsh_type = marsh_type[i])
  }
  df <- do.call ("rbind", dfs)
  
  ggs [[ct]] <- ggplot (df, aes (x=Rank, y=Rel_proportion))+
    geom_line(aes (colour= Marsh, linetype = Marsh_type),
              size=1.5) +
    theme(
      axis.title = element_text(size=14, colour = "black"),
      axis.text = element_text(size =12, colour = "black"),
      panel.background = element_rect(fill = "white",
                                          colour = "black",
                                          size = 0.5, linetype = "solid"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(size = 0.5, linetype = "solid",
                                   colour = "black"),
          legend.position="bottom",
          legend.key=element_blank(),
          legend.key.size = unit(2, 'cm'),
          legend.key.height = unit(0.8, 'cm'),
          legend.key.width = unit(1.3, 'cm'),
          legend.text = element_text(size=12),
          legend.title = element_text(size=14)) +
    guides(linetype = FALSE) +
    ylim (0,1) +
    scale_colour_manual (values = c("#01665e","#5ab4ac", "#cfcf8a","#fed98e","#fe9929","#cc4c02")) +
    scale_linetype_manual (values = c(3,1)) +
    scale_x_continuous(trans='log2') +
    labs (y= "Relative abundance",
          x= "Taxa rank")+
    ggtitle(assemblages[ct])
    
  ct <- ct+1
}

legend <- get_legend(ggs[[1]])#+theme(legend.box.margin = margin(0, 0, 0, 12))

tiff(file=here::here("figures/SAD.tiff"),width=3000,
     height=4500,units = "px",
     compression="lzw",res=300)
plot_grid(
  ggs[[1]] + theme(legend.position="none"),
  ggs[[2]] + theme(legend.position="none"),
  ggs[[3]] + theme(legend.position="none"),
  ggs[[4]] + theme(legend.position="none"),
  ggs[[5]] + theme(legend.position="none"),
  ggs[[6]] + theme(legend.position="none"),
  ggs[[7]] + theme(legend.position="none"),
  legend,
  ncol=2)
dev.off()

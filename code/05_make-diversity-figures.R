# Make the Diversity plots #
here::i_am("code/05_make-diversity-figures.R")
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

marsh_attr = data.frame(marsh_id = c("LHA","LHB","LHC","WPH1","WPH2","PS7"),
                        group_id = c("restored","restored","natural","natural","natural","natural")
) %>% dplyr::mutate(marsh_id = factor(marsh_id, levels = c("LHA","LHB","LHC","WPH1","WPH2","PS7")))

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

#color_scheme = c("#01665e","#5ab4ac", "#ffffb2","#fed98e","#fe9929","#cc4c02")
color_scheme = c("#01665e","#5ab4ac", "#cfcf8a","#fed98e","#fe9929","#cc4c02")
color_scheme2 = c("#01665e","#5ab4ac","#ffffcc","#a1dab4","#41b6c4","#225ea8")
shape_scheme = c(22,22,21,21,21,21)
theme_set(theme_minimal())

group_names = c( "Surface\nMicrobes", "Below-Surface\nMicrobes","Plants", "Macroinfauna", "Spiders", 
                 "On-marsh\nnekton", "Full minnow","Off-marsh\nnekton")

# Plot code ####

## Sn diversities ####
# Figure 2
mob_alphabeta = mob_stats %>%
  purrr::map(~.x %>% 
               pluck('samples_stats') %>% 
               data.frame %>% 
               dplyr::filter(index %in% c('S_n','beta_S_n')) %>%
               dplyr::mutate(index = recode_factor(index, !!!c(S_n = 'S', beta_S_n ='beta_S'))) %>%
               dplyr::mutate(across(where(is.factor), droplevels))%>%
               dplyr::mutate(group =factor(group, levels = site_order))) 

mob_gamma = mob_stats %>%
  purrr::map(~.x %>%
               pluck('groups_stats') %>%
               data.frame %>%
               dplyr::filter(index == 'S_n') %>% 
               dplyr::mutate(index = recode_factor(index, S_n = 'gamma_S')) %>%
               dplyr::mutate(across(where(is.factor), droplevels)) %>%
               dplyr::mutate(group =factor(group, levels = site_order)))

mob_diversities = purrr::map2(mob_alphabeta, mob_gamma, ~bind_rows(.x,.y))

mob_summaries = mob_diversities%>%
  purrr::map(~.x %>%
               group_by(group, index) %>%
               dplyr::summarise(across(value, list(mean = ~mean(.x, na.rm = TRUE),
                                                   median = ~median(.x, na.rm = TRUE),
                                                   quant2.5 = ~quantile(.x, 0.025, na.rm = TRUE),
                                                   quant25 = ~quantile(.x, 0.25, na.rm = TRUE),
                                                   quant75 = ~quantile(.x, 0.75, na.rm = TRUE),
                                                   quant97.5 = ~quantile(.x, 0.975, na.rm = TRUE),
                                                   min = ~min(.x, na.rm= TRUE),
                                                   max = ~max(.x, na.rm = TRUE)),
                                       .names = "{.fn}")) %>%
               pivot_longer(-group:-index, names_to = 'stat', values_to = 'value') %>%
               tidyr::unite("stat", index,stat) %>%
               pivot_wider(names_from = stat, values_from = value) %>%
               dplyr::mutate(group =factor(group, levels = site_order)))
diversityPlots = list()
###
# "Surface\nMicrobes"
b_df = mob_diversities[[1]] %>%
  dplyr::filter(index == 'beta_S')
beta_ymax = max(b_df$value, na.rm = TRUE)+(0.2*max(b_df$value, na.rm=TRUE))
beta_plot = ggplotGrob(b_df %>%
                         ggplot(aes(x = group, y = value))+
                         geom_boxplot(aes(fill = group))+
                         scale_y_continuous(name = expression(''~beta*"-diversity"),
                                            limits = c(0,beta_ymax), expand = c(0.01,0.01),
                                            breaks = c(0,1,2,4,8,14,22)) +
                         scale_fill_manual(values = color_scheme)+
                         #geom_rangeframe(sides = "lb")+
                         theme(legend.position = 'none',
                               panel.grid = element_blank(),
                               axis.title.x = element_blank(),
                               axis.text.x = element_blank(),
                               panel.background = element_rect(fill = 'transparent', color = NA),
                               plot.background = element_rect(fill = 'white', color = 'black'),
                               axis.line = element_line(size = 0.5, colour = "black", linetype=1),
                               axis.text = element_text(colour = "black"))#element_text(angle = 45, vjust = 1, hjust = 1))
);grid.draw(beta_plot)

ag_df = mob_summaries[[1]]
ag_ymax = max(ag_df$gamma_S_median, na.rm = TRUE)+(0.1*max(ag_df$gamma_S_median, na.rm = TRUE))
if(min(ag_df$gamma_S_median, na.rm = TRUE)-(0.1*min(ag_df$gamma_S_median, na.rm = TRUE)) >= 2){
  ag_ymin = min(ag_df$gamma_S_median, na.rm = TRUE)-(0.1*min(ag_df$gamma_S_median, na.rm = TRUE))
} else{ ag_ymin = 0}
jitter_height = abs(ag_ymax - ag_ymin)*0.01

ag_xmax = max(ag_df$S_quant75, na.rm = TRUE)+(0.3*max(ag_df$S_quant75, na.rm = TRUE))
if(min(ag_df$S_quant25, na.rm = TRUE)-(0.3*min(ag_df$S_quant25, na.rm = TRUE)) >= 2){
  ag_xmin =min(ag_df$S_quant25, na.rm = TRUE)-(0.3*min(ag_df$S_quant25, na.rm = TRUE))
} else{ag_xmin = 0}

ag_df %>%   
  dplyr::mutate(group = factor(group, levels = site_order)) %>%
  ggplot(aes(x = S_median, y = gamma_S_median))+
  #geom_abline(alpha = 0.8, color = 'grey', size = 1.2)+
  geom_errorbarh(aes(color = group, xmin = S_quant25, xmax = S_quant75, linetype = group), height = 0, size = 1.5)+
  geom_point(aes(fill = group, shape = group),size = 2.2)+
  scale_y_continuous(name = expression(""~gamma*"-diversity"),
                     limits = c(ag_ymin,ag_ymax), expand = c(0.01,0.01))+ # #min (ag_df$S_min), max (ag_df$gamma_S_max)
  scale_x_continuous(name = expression(""~alpha*"-diversity"),
                     limits = c(NA,ag_xmax), expand = c(0.01,0.01))+ #
  scale_fill_manual(values = color_scheme)+
  scale_color_manual(values = color_scheme)+
  scale_shape_manual(values = shape_scheme)+
  scale_linetype_manual(
    values = c("dotted", "dotted", "solid",
               "solid", "solid", "solid"))+
  guides(color = guide_legend(nrow = 2),
         fill = guide_legend(nrow = 2))+
  #geom_rangeframe(sides = "lb")+
  annotate('text', label = group_names[1], family = 'serif', size = 4,
           x = Inf, y = Inf, hjust = 1, vjust = 1, lineheight = 0.8)+
  theme(legend.position = 'none',
        panel.grid = element_blank(),
        legend.title = element_blank(),
        axis.title = element_blank(),
        axis.line = element_line(size = 0.5, colour = "black", linetype=1),
        axis.text = element_text(colour = "black")) -> ag_plot

full_plot = 
  cowplot::ggdraw()+
  cowplot::draw_plot(ag_plot) +
  cowplot::draw_plot(beta_plot, x = 0.99, y = 0.115, width = .4, height = 0.5,
                     hjust = 1, vjust = 0);full_plot
diversityPlots[[1]] <- full_plot

###
# "Below-Surface\nMicrobes"
b_df = mob_diversities[[2]] %>%
  dplyr::filter(index == 'beta_S')
beta_ymax = max(b_df$value, na.rm = TRUE)+(0.2*max(b_df$value, na.rm=TRUE))
beta_plot = ggplotGrob(b_df %>%
                         ggplot(aes(x = group, y = value))+
                         geom_boxplot(aes(fill = group))+
                         scale_y_continuous(name = expression(''~beta*"-diversity"),
                                            limits = c(0,beta_ymax), expand = c(0.01,0.01),
                                            breaks = c(0,1,2,4,8,14,22)) +
                         scale_fill_manual(values = color_scheme)+
                         #geom_rangeframe(sides = "lb")+
                         theme(legend.position = 'none',
                               panel.grid = element_blank(),
                               axis.title.x = element_blank(),
                               axis.text.x = element_blank(),
                               panel.background = element_rect(fill = 'transparent', color = NA),
                               plot.background = element_rect(fill = 'white', color = 'black'),
                               axis.line = element_line(size = 0.5, colour = "black", linetype=1),
                               axis.text = element_text(colour = "black"))#element_text(angle = 45, vjust = 1, hjust = 1))
);grid.draw(beta_plot)

ag_df = mob_summaries[[2]]
ag_ymax = max(ag_df$gamma_S_median, na.rm = TRUE)+(0.1*max(ag_df$gamma_S_median, na.rm = TRUE))
if(min(ag_df$gamma_S_median, na.rm = TRUE)-(0.1*min(ag_df$gamma_S_median, na.rm = TRUE)) >= 2){
  ag_ymin = min(ag_df$gamma_S_median, na.rm = TRUE)-(0.1*min(ag_df$gamma_S_median, na.rm = TRUE))
} else{ ag_ymin = 0}
jitter_height = abs(ag_ymax - ag_ymin)*0.01

ag_xmax = max(ag_df$S_quant75, na.rm = TRUE)+(0.3*max(ag_df$S_quant75, na.rm = TRUE))
if(min(ag_df$S_quant25, na.rm = TRUE)-(0.3*min(ag_df$S_quant25, na.rm = TRUE)) >= 2){
  ag_xmin =min(ag_df$S_quant25, na.rm = TRUE)-(0.3*min(ag_df$S_quant25, na.rm = TRUE))
} else{ag_xmin = 0}

ag_df %>%   
  dplyr::mutate(group = factor(group, levels = site_order)) %>%
  ggplot(aes(x = S_median, y = gamma_S_median))+
  #geom_abline(alpha = 0.8, color = 'grey', size = 1.2)+
  geom_errorbarh(aes(color = group, xmin = S_quant25, xmax = S_quant75, linetype = group), height = 0, size = 1.5)+
  geom_point(aes(fill = group, shape = group),size = 2.2)+
  scale_y_continuous(name = expression(""~gamma*"-diversity"),
                     limits = c(ag_ymin,ag_ymax), expand = c(0.01,0.01))+
  scale_x_continuous(name = expression(""~alpha*"-diversity"),
                     limits = c(NA,ag_xmax), expand = c(0.01,0.01))+
  scale_fill_manual(values = color_scheme)+
  scale_color_manual(values = color_scheme)+
  scale_shape_manual(values = shape_scheme)+
  scale_linetype_manual(
    values = c("dotted", "dotted", "solid",
               "solid", "solid", "solid"))+
  guides(color = guide_legend(nrow = 2),
         fill = guide_legend(nrow = 2))+
  #geom_rangeframe(sides = "lb")+
  annotate('text', label = group_names[2], family = 'serif', size = 4,
           x = Inf, y = Inf, hjust = 1, vjust = 1, lineheight = 0.8)+
  theme(legend.position = 'none',
        panel.grid = element_blank(),
        legend.title = element_blank(),
        axis.title = element_blank(),
        axis.line = element_line(size = 0.5, colour = "black", linetype=1),
        axis.text = element_text(colour = "black")) -> ag_plot;ag_plot

full_plot = 
  cowplot::ggdraw()+
  cowplot::draw_plot(ag_plot) +
  cowplot::draw_plot(beta_plot, x = 0.99, y = 0.115, width = .4, height = 0.5,
                     hjust = 1, vjust = 0);full_plot
diversityPlots[[2]] <- full_plot
###
# Plants
b_df = mob_diversities[[3]] %>%
  dplyr::filter(index == 'beta_S')
beta_ymax = max(b_df$value, na.rm = TRUE)+(0.2*max(b_df$value, na.rm=TRUE))
beta_plot = ggplotGrob(b_df %>%
                         ggplot(aes(x = group, y = value))+
                         geom_boxplot(aes(fill = group))+
                         scale_y_continuous(name = expression(''~beta*"-diversity"),
                                            limits = c(0,beta_ymax), expand = c(0.01,0.01),
                                            breaks = c(0,1,2,4,8,14,22)) +
                         scale_fill_manual(values = color_scheme)+
                         #geom_rangeframe(sides = "lb")+
                         theme(legend.position = 'none',
                               panel.grid = element_blank(),
                               axis.title.x = element_blank(),
                               axis.text.x = element_blank(),
                               panel.background = element_rect(fill = 'transparent', color = NA),
                               plot.background = element_rect(fill = 'white', color = 'black'),
                               axis.line = element_line(size = 0.5, colour = "black", linetype=1),
                               axis.text = element_text(colour = "black"))#element_text(angle = 45, vjust = 1, hjust = 1))
);grid.draw(beta_plot)

ag_df = mob_summaries[[3]]
ag_ymax = max(ag_df$gamma_S_median, na.rm = TRUE)+(0.1*max(ag_df$gamma_S_median, na.rm = TRUE))
if(min(ag_df$gamma_S_median, na.rm = TRUE)-(0.1*min(ag_df$gamma_S_median, na.rm = TRUE)) >= 2){
  ag_ymin = min(ag_df$gamma_S_median, na.rm = TRUE)-(0.1*min(ag_df$gamma_S_median, na.rm = TRUE))
} else{ ag_ymin = 0}
jitter_height = abs(ag_ymax - ag_ymin)*0.01

ag_xmax = max(ag_df$S_quant75, na.rm = TRUE)+(0.3*max(ag_df$S_quant75, na.rm = TRUE))
if(min(ag_df$S_quant25, na.rm = TRUE)-(0.3*min(ag_df$S_quant25, na.rm = TRUE)) >= 2){
  ag_xmin =min(ag_df$S_quant25, na.rm = TRUE)-(0.3*min(ag_df$S_quant25, na.rm = TRUE))
} else{ag_xmin = 0}

ag_df %>%   
  dplyr::mutate(group = factor(group, levels = site_order)) %>%
  ggplot(aes(x = S_median, y = gamma_S_median))+
  #geom_abline(alpha = 0.8, color = 'grey', size = 1.2)+
  geom_errorbarh(aes(color = group, xmin = S_quant25, xmax = S_quant75, linetype = group), height = 0, size = 1.5)+
  geom_point(aes(fill = group, shape = group),size = 2.2)+
  scale_y_continuous(name = expression(""~gamma*"-diversity"),
                     limits = c(ag_ymin,ag_ymax), expand = c(0.01,0.01))+#
  scale_x_continuous(name = expression(""~alpha*"-diversity"),
                     limits = c(NA,ag_xmax), expand = c(0.01,0.01))+ #
  scale_fill_manual(values = color_scheme)+
  scale_color_manual(values = color_scheme)+
  scale_shape_manual(values = shape_scheme)+
  scale_linetype_manual(
    values = c("dotted", "dotted", "solid",
               "solid", "solid", "solid"))+
  guides(color = guide_legend(nrow = 2),
         fill = guide_legend(nrow = 2))+
  #geom_rangeframe(sides = "lb")+
  annotate('text', label = group_names[3], family = 'serif', size = 4,
           x = Inf, y = Inf, hjust = 1, vjust = 1, lineheight = 0.8)+
  theme(legend.position = 'none',
        panel.grid = element_blank(),
        legend.title = element_blank(),
        axis.title = element_blank(),
        axis.line = element_line(size = 0.5, colour = "black", linetype=1),
        axis.text = element_text(colour = "black")) -> ag_plot;ag_plot

full_plot = 
  cowplot::ggdraw()+
  cowplot::draw_plot(ag_plot) +
  cowplot::draw_plot(beta_plot, x = 0.99, y = 0.115, width = .4, height = 0.5,
                     hjust = 1, vjust = 0);full_plot
diversityPlots[[3]] <- full_plot
###
# "Macroinfauna"
b_df = mob_diversities[[4]] %>%
  dplyr::filter(index == 'beta_S')
beta_ymax = max(b_df$value, na.rm = TRUE)+(0.2*max(b_df$value, na.rm=TRUE))
beta_plot = ggplotGrob(b_df %>%
                         ggplot(aes(x = group, y = value))+
                         geom_boxplot(aes(fill = group))+
                         scale_y_continuous(name = expression(''~beta*"-diversity"),
                                            limits = c(0,beta_ymax), expand = c(0.01,0.01),
                                            breaks = c(0,1,2,4,8,14,22)) +
                         scale_fill_manual(values = color_scheme)+
                         #geom_rangeframe(sides = "lb")+
                         theme(legend.position = 'none',
                               panel.grid = element_blank(),
                               axis.title.x = element_blank(),
                               axis.text.x = element_blank(),
                               panel.background = element_rect(fill = 'transparent', color = NA),
                               plot.background = element_rect(fill = 'white', color = 'black'),
                               axis.line = element_line(size = 0.5, colour = "black", linetype=1),
                               axis.text = element_text(colour = "black"))#element_text(angle = 45, vjust = 1, hjust = 1))
);grid.draw(beta_plot)

ag_df = mob_summaries[[4]]
ag_ymax = max(ag_df$gamma_S_median, na.rm = TRUE)+(0.1*max(ag_df$gamma_S_median, na.rm = TRUE))
if(min(ag_df$gamma_S_median, na.rm = TRUE)-(0.1*min(ag_df$gamma_S_median, na.rm = TRUE)) >= 2){
  ag_ymin = min(ag_df$gamma_S_median, na.rm = TRUE)-(0.1*min(ag_df$gamma_S_median, na.rm = TRUE))
} else{ ag_ymin = 0}
jitter_height = abs(ag_ymax - ag_ymin)*0.01

ag_xmax = max(ag_df$S_quant75, na.rm = TRUE)+(0.5*max(ag_df$S_quant75, na.rm = TRUE))
if(min(ag_df$S_quant25, na.rm = TRUE)-(0.3*min(ag_df$S_quant25, na.rm = TRUE)) >= 2){
  ag_xmin =min(ag_df$S_quant25, na.rm = TRUE)-(0.3*min(ag_df$S_quant25, na.rm = TRUE))
} else{ag_xmin = 0}

ag_df %>%   
  dplyr::mutate(group = factor(group, levels = site_order)) %>%
  ggplot(aes(x = S_median, y = gamma_S_median))+
  #geom_abline(alpha = 0.8, color = 'grey', size = 1.2)+
  geom_errorbarh(aes(color = group, xmin = S_quant25, xmax = S_quant75, linetype = group), height = 0, size = 1.5)+
  geom_point(aes(fill = group, shape = group),size = 2.2)+
  scale_y_continuous(name = expression(""~gamma*"-diversity"),
                     limits = c(ag_ymin,ag_ymax), expand = c(0.01,0.01))+ #
  scale_x_continuous(name = expression(""~alpha*"-diversity"),
                     limits = c(NA,ag_xmax), expand = c(0.01,0.01))+ # NA,ag_xmax
  scale_fill_manual(values = color_scheme)+
  scale_color_manual(values = color_scheme)+
  scale_shape_manual(values = shape_scheme)+
  scale_linetype_manual(
    values = c("dotted", "dotted", "solid",
               "solid", "solid", "solid"))+
  guides(color = guide_legend(nrow = 2),
         fill = guide_legend(nrow = 2))+
  #geom_rangeframe(sides = "lb")+
  annotate('text', label = group_names[4], family = 'serif', size = 4,
           x = Inf, y = Inf, hjust = 1, vjust = 1, lineheight = 0.8)+
  theme(legend.position = 'none',
        panel.grid = element_blank(),
        legend.title = element_blank(),
        axis.title = element_blank(),
        axis.line = element_line(size = 0.5, colour = "black", linetype=1),
        axis.text = element_text(colour = "black")) -> ag_plot;ag_plot

full_plot = 
  cowplot::ggdraw()+
  cowplot::draw_plot(ag_plot) +
  cowplot::draw_plot(beta_plot, x = 0.99, y = 0.115, width = .4, height = 0.5,
                     hjust = 1, vjust = 0);full_plot
diversityPlots[[4]] <- full_plot
###
# "Spiders"
b_df = mob_diversities[[5]] %>%
  dplyr::filter(index == 'beta_S')
beta_ymax = max(b_df$value, na.rm = TRUE)+(0.2*max(b_df$value, na.rm=TRUE))
beta_plot = ggplotGrob(b_df %>%
                         ggplot(aes(x = group, y = value))+
                         geom_boxplot(aes(fill = group))+
                         scale_y_continuous(name = expression(''~beta*"-diversity"),
                                            limits = c(0,beta_ymax), expand = c(0.01,0.01),
                                            breaks = c(0,1,2,4,8,14,22)) +
                         scale_fill_manual(values = color_scheme)+
                         #geom_rangeframe(sides = "lb")+
                         theme(legend.position = 'none',
                               panel.grid = element_blank(),
                               axis.title.x = element_blank(),
                               axis.text.x = element_blank(),
                               panel.background = element_rect(fill = 'transparent', color = NA),
                               plot.background = element_rect(fill = 'white', color = 'black'),
                               axis.line = element_line(size = 0.5, colour = "black", linetype=1),
                               axis.text = element_text(colour = "black"))#element_text(angle = 45, vjust = 1, hjust = 1))
);grid.draw(beta_plot)

ag_df = mob_summaries[[5]]
ag_ymax = max(ag_df$gamma_S_median, na.rm = TRUE)+(0.1*max(ag_df$gamma_S_median, na.rm = TRUE))
if(min(ag_df$gamma_S_median, na.rm = TRUE)-(0.1*min(ag_df$gamma_S_median, na.rm = TRUE)) >= 2){
  ag_ymin = min(ag_df$gamma_S_median, na.rm = TRUE)-(0.1*min(ag_df$gamma_S_median, na.rm = TRUE))
} else{ ag_ymin = 0}
jitter_height = abs(ag_ymax - ag_ymin)*0.01

ag_xmax = max(ag_df$S_quant75, na.rm = TRUE)+(0.5*max(ag_df$S_quant75, na.rm = TRUE))
if(min(ag_df$S_quant25, na.rm = TRUE)-(0.3*min(ag_df$S_quant25, na.rm = TRUE)) >= 2){
  ag_xmin =min(ag_df$S_quant25, na.rm = TRUE)-(0.3*min(ag_df$S_quant25, na.rm = TRUE))
} else{ag_xmin = 0}

ag_df %>%   
  dplyr::mutate(group = factor(group, levels = site_order)) %>%
  ggplot(aes(x = S_median, y = gamma_S_median))+
  #geom_abline(alpha = 0.8, color = 'grey', size = 1.2)+
  geom_errorbarh(aes(color = group, xmin = S_quant25, xmax = S_quant75, linetype = group), height = 0, size = 1.5)+
  geom_point(aes(fill = group, shape = group),size = 2.2)+
  scale_y_continuous(name = expression(""~gamma*"-diversity"),
                     limits = c(ag_ymin,ag_ymax), expand = c(0.01,0.01))+ #ag_ymin,ag_ymax
  scale_x_continuous(name = expression(""~alpha*"-diversity"),
                     limits = c(NA,ag_xmax), expand = c(0.01,0.01))+ #NA,ag_xmax
  scale_fill_manual(values = color_scheme)+
  scale_color_manual(values = color_scheme)+
  scale_shape_manual(values = shape_scheme)+
  scale_linetype_manual(
    values = c("dotted", "dotted", "solid",
               "solid", "solid", "solid"))+
  guides(color = guide_legend(nrow = 2),
         fill = guide_legend(nrow = 2))+
  #geom_rangeframe(sides = "lb")+
  annotate('text', label = group_names[5], family = 'serif', size = 4,
           x = Inf, y = Inf, hjust = 1, vjust = 1, lineheight = 0.8)+
  theme(legend.position = 'none',
        panel.grid = element_blank(),
        legend.title = element_blank(),
        axis.title = element_blank(),
        axis.line = element_line(size = 0.5, colour = "black", linetype=1),
        axis.text = element_text(colour = "black")) -> ag_plot;ag_plot

full_plot = 
  cowplot::ggdraw()+
  cowplot::draw_plot(ag_plot) +
  cowplot::draw_plot(beta_plot, x = 0.99, y = 0.115, width = .4, height = 0.5,
                     hjust = 1, vjust = 0);full_plot
diversityPlots[[5]] <- full_plot
###
# On-marsh nekton
b_df = mob_diversities[[6]] %>%
  dplyr::filter(index == 'beta_S')
beta_ymax = max(b_df$value, na.rm = TRUE)+(0.2*max(b_df$value, na.rm=TRUE))
beta_plot = ggplotGrob(b_df %>%
                         ggplot(aes(x = group, y = value))+
                         geom_boxplot(aes(fill = group))+
                         scale_y_continuous(name = expression(''~beta*"-diversity"),
                                            limits = c(0,beta_ymax), expand = c(0.01,0.01),
                                            breaks = c(0,1,2,4,8,14,22)) +
                         scale_fill_manual(values = color_scheme)+
                         #geom_rangeframe(sides = "lb")+
                         theme(legend.position = 'none',
                               panel.grid = element_blank(),
                               axis.title.x = element_blank(),
                               axis.text.x = element_blank(),
                               panel.background = element_rect(fill = 'transparent', color = NA),
                               plot.background = element_rect(fill = 'white', color = 'black'),
                               axis.line = element_line(size = 0.5, colour = "black", linetype=1),
                               axis.text = element_text(colour = "black"))#element_text(angle = 45, vjust = 1, hjust = 1))
);grid.draw(beta_plot)

ag_df = mob_summaries[[6]]
ag_ymax = max(ag_df$gamma_S_median, na.rm = TRUE)+(0.1*max(ag_df$gamma_S_median, na.rm = TRUE))
if(min(ag_df$gamma_S_median, na.rm = TRUE)-(0.1*min(ag_df$gamma_S_median, na.rm = TRUE)) >= 2){
  ag_ymin = min(ag_df$gamma_S_median, na.rm = TRUE)-(0.1*min(ag_df$gamma_S_median, na.rm = TRUE))
} else{ ag_ymin = 0}
jitter_height = abs(ag_ymax - ag_ymin)*0.01

ag_xmax = max(ag_df$S_quant75, na.rm = TRUE)+(0.8*max(ag_df$S_quant75, na.rm = TRUE))
if(min(ag_df$S_quant25, na.rm = TRUE)-(0.3*min(ag_df$S_quant25, na.rm = TRUE)) >= 2){
  ag_xmin =min(ag_df$S_quant25, na.rm = TRUE)-(0.3*min(ag_df$S_quant25, na.rm = TRUE))
} else{ag_xmin = 0}

ag_df %>%   
  dplyr::mutate(group = factor(group, levels = site_order)) %>%
  ggplot(aes(x = S_median, y = gamma_S_median))+
  #geom_abline(alpha = 0.8, color = 'grey', size = 1.2)+
  geom_errorbarh(aes(color = group, xmin = S_quant25, xmax = S_quant75, linetype = group), height = 0, size = 1.5)+
  geom_point(aes(fill = group, shape = group),size = 2.2)+
  scale_y_continuous(name = expression(""~gamma*"-diversity"),
                     limits = c(ag_ymin,ag_ymax), expand = c(0.01,0.01))+ #
  scale_x_continuous(name = expression(""~alpha*"-diversity"),
                     limits = c(NA,ag_xmax), expand = c(0.01,0.01))+ #
  scale_fill_manual(values = color_scheme)+
  scale_color_manual(values = color_scheme)+
  scale_shape_manual(values = shape_scheme)+
  scale_linetype_manual(
    values = c("dotted", "dotted", "solid",
               "solid", "solid", "solid"))+
  guides(color = guide_legend(nrow = 2),
         fill = guide_legend(nrow = 2))+
  #geom_rangeframe(sides = "lb")+
  annotate('text', label = group_names[6], family = 'serif', size = 4,
           x = Inf, y = Inf, hjust = 1, vjust = 1, lineheight = 0.8)+
  theme(legend.position = 'none',
        panel.grid = element_blank(),
        legend.title = element_blank(),
        axis.title = element_blank(),
        axis.line = element_line(size = 0.5, colour = "black", linetype=1),
        axis.text = element_text(colour = "black")) -> ag_plot;ag_plot

full_plot = 
  cowplot::ggdraw()+
  cowplot::draw_plot(ag_plot) +
  cowplot::draw_plot(beta_plot, x = 0.99, y = 0.115, width = .4, height = 0.5,
                     hjust = 1, vjust = 0);full_plot
diversityPlots[[6]] <- full_plot
###
# Off-marsh nekton
b_df = mob_diversities[[8]] %>%
  dplyr::filter(index == 'beta_S')
beta_ymax = max(b_df$value, na.rm = TRUE)+(0.2*max(b_df$value, na.rm=TRUE))
beta_plot = ggplotGrob(b_df %>%
                         ggplot(aes(x = group, y = value))+
                         geom_boxplot(aes(fill = group))+
                         scale_y_continuous(name = expression(''~beta*"-diversity"),
                                            limits = c(0,beta_ymax), expand = c(0.01,0.01),
                                            breaks = c(0,1,2,4,8,14,22)) +
                         scale_fill_manual(values = color_scheme)+
                         #geom_rangeframe(sides = "lb")+
                         theme(legend.position = 'none',
                               panel.grid = element_blank(),
                               axis.title.x = element_blank(),
                               axis.text.x = element_blank(),
                               panel.background = element_rect(fill = 'transparent', color = NA),
                               plot.background = element_rect(fill = 'white', color = 'black'),
                               axis.line = element_line(size = 0.5, colour = "black", linetype=1),
                               axis.text = element_text(colour = "black"))#element_text(angle = 45, vjust = 1, hjust = 1))
);grid.draw(beta_plot)

ag_df = mob_summaries[[8]]
ag_ymax = max(ag_df$gamma_S_median, na.rm = TRUE)+(0.1*max(ag_df$gamma_S_median, na.rm = TRUE))
if(min(ag_df$gamma_S_median, na.rm = TRUE)-(0.1*min(ag_df$gamma_S_median, na.rm = TRUE)) >= 2){
  ag_ymin = min(ag_df$gamma_S_median, na.rm = TRUE)-(0.1*min(ag_df$gamma_S_median, na.rm = TRUE))
} else{ ag_ymin = 0}
jitter_height = abs(ag_ymax - ag_ymin)*0.01

ag_xmax = max(ag_df$S_quant75, na.rm = TRUE)+(0.3*max(ag_df$S_quant75, na.rm = TRUE))
if(min(ag_df$S_quant25, na.rm = TRUE)-(0.3*min(ag_df$S_quant25, na.rm = TRUE)) >= 2){
  ag_xmin =min(ag_df$S_quant25, na.rm = TRUE)-(0.3*min(ag_df$S_quant25, na.rm = TRUE))
} else{ag_xmin = 0}

ag_df %>%   
  dplyr::mutate(group = factor(group, levels = site_order)) %>%
  ggplot(aes(x = S_median, y = gamma_S_median))+
  #geom_abline(alpha = 0.8, color = 'grey', size = 1.2)+
  geom_errorbarh(aes(color = group, xmin = S_quant25, xmax = S_quant75, linetype = group), height = 0, size = 1.5)+
  geom_point(aes(fill = group, shape = group),size = 2.2)+
  scale_y_continuous(name = expression(""~gamma*"-diversity"),
                     limits = c(ag_ymin,ag_ymax), expand = c(0.01,0.01))+
  scale_x_continuous(name = expression(""~alpha*"-diversity"),
                     limits = c(NA,ag_xmax), expand = c(0.01,0.01))+
  scale_fill_manual(values = color_scheme)+
  scale_color_manual(values = color_scheme)+
  scale_shape_manual(values = shape_scheme)+
  scale_linetype_manual(
    values = c("dotted", "dotted", "solid",
               "solid", "solid", "solid"))+
  guides(color = guide_legend(nrow = 2),
         fill = guide_legend(nrow = 2))+
  #geom_rangeframe(sides = "lb")+
  annotate('text', label = group_names[8], family = 'serif', size = 4,
           x = Inf, y = Inf, hjust = 1, vjust = 1, lineheight = 0.8)+
  theme(legend.position = 'none',
        panel.grid = element_blank(),
        legend.title = element_blank(),
        axis.title = element_blank(),
        axis.line = element_line(size = 0.5, colour = "black", linetype=1),
        axis.text = element_text(colour = "black")) -> ag_plot;ag_plot

full_plot = 
  cowplot::ggdraw()+
  cowplot::draw_plot(ag_plot) +
  cowplot::draw_plot(beta_plot, x = 0.99, y = 0.115, width = .4, height = 0.5,
                     hjust = 1, vjust = 0);full_plot
diversityPlots[[8]] <- full_plot
###

plot_legend = 
  cowplot::get_legend(mob_summaries[[1]] %>%
                        # pluck(x) %>%
                        dplyr::mutate(group =factor(group, levels = site_order)) %>%
                        ggplot(aes(x = S_median, y = gamma_S_median))+
                        geom_point(aes(fill = group, shape = group),size = 3.3)+
                        scale_fill_manual(values = color_scheme)+
                        scale_color_manual(values = color_scheme)+
                        scale_shape_manual(values = shape_scheme)+
                        guides(color = guide_legend(nrow = 2),
                               fill = guide_legend(nrow = 2))+
                        scale_linetype_manual(
                          values = c("dotted", "dotted", "solid",
                                     "solid", "solid", "solid"))+
                        theme(legend.title = element_blank(),
                              legend.text = element_text(family = 'serif', size = 14),
                              legend.margin= margin(c(t = 1,r = 5,b = 1,l = 5))))

plot_legend$grobs <- lapply(plot_legend$grobs, function(z) modifyList(z, list(x = unit(0.5, 'npc'),
                                                                              y = unit(1, 'npc'),
                                                                              vjust = 0)))


# removed full minnow diversityPlots[[5]]
png(here::here("figures/Sn_diversities.png"), width = 8, height = 10, units = "in", res = 480 )
full_plot = gridExtra::grid.arrange(diversityPlots[[1]], diversityPlots[[2]],
                                    diversityPlots[[3]], diversityPlots[[4]],
                                    diversityPlots[[5]], diversityPlots[[6]],
                                    diversityPlots[[8]],
                                    plot_legend,
                                    ncol = 2,
                                    left = textGrob(label = expression(""~gamma*"-diversity ("~italic(S[N])~")"),
                                                    gp = gpar(fontfamily = 'serif'), rot = 90),
                                    bottom = textGrob(label = expression(""~alpha*"-diversity ("~italic(S[N])~")"),
                                                      gp = gpar(fontfamily = 'serif')))
dev.off()


## S_diversities#####
mob_stats = setNames(mob_stats, nm = taxa_names)
mob_stats = rlist::list.subset(mob_stats, taxa_order)

mob_alphabeta = mob_stats %>%
  purrr::map(~.x %>% 
               pluck('samples_stats') %>% 
               data.frame %>% 
               dplyr::filter(index %in% c('S','beta_S')) %>%
               dplyr::mutate(across(where(is.factor), droplevels))%>%
               dplyr::mutate(group =factor(group, levels = site_order))) #%>%
# bind_rows(.id = 'taxa_group')

mob_gamma = mob_stats %>%
  purrr::map(~.x %>%
               pluck('groups_stats') %>%
               data.frame %>%
               dplyr::filter(index == 'S') %>% #& effort == min(effort, na.rm = TRUE)) %>%
               dplyr::mutate(index = recode_factor(index, S = 'gamma_S')) %>%
               dplyr::mutate(across(where(is.factor), droplevels)) %>%
               dplyr::mutate(group =factor(group, levels = site_order)))

mob_diversities = purrr::map2(mob_alphabeta, mob_gamma, ~bind_rows(.x,.y))

mob_summaries = mob_diversities %>%
  purrr::map(~.x %>%
               group_by(group, index) %>%
               dplyr::summarise(across(value, list(mean = ~mean(.x, na.rm = TRUE),
                                                   median = ~median(.x, na.rm = TRUE),
                                                   quant2.5 = ~quantile(.x, 0.025, na.rm = TRUE),
                                                   quant25 = ~quantile(.x, 0.25, na.rm = TRUE),
                                                   quant75 = ~quantile(.x, 0.75, na.rm = TRUE),
                                                   quant97.5 = ~quantile(.x, 0.975, na.rm = TRUE),
                                                   min = ~min(.x, na.rm= TRUE),
                                                   max = ~max(.x, na.rm = TRUE)),
                                       .names = "{.fn}")) %>%
               pivot_longer(-group:-index, names_to = 'stat', values_to = 'value') %>%
               tidyr::unite("stat", index,stat) %>%
               pivot_wider(names_from = stat, values_from = value) %>%
               dplyr::mutate(group =factor(group, levels = site_order)))

diversityPlots = list()

# "Surface\nMicrobes"
b_df = mob_diversities[[1]] %>%
  dplyr::filter(index == 'beta_S')
beta_ymax = max(b_df$value, na.rm = TRUE)+(0.2*max(b_df$value, na.rm=TRUE))
beta_plot = ggplotGrob(b_df %>%
                         ggplot(aes(x = group, y = value))+
                         geom_boxplot(aes(fill = group))+
                         scale_y_continuous(name = expression(''~beta*"-diversity"),
                                            limits = c(0,beta_ymax), expand = c(0.01,0.01),
                                            breaks = c(0,2,4,8,14,22)) +
                         scale_fill_manual(values = color_scheme)+
                         #geom_rangeframe(sides = "lb")+
                         theme(legend.position = 'none',
                               panel.grid = element_blank(),
                               axis.title.x = element_blank(),
                               axis.text.x = element_blank(),
                               panel.background = element_rect(fill = 'transparent', color = NA),
                               plot.background = element_rect(fill = 'white', color = 'black'),
                               axis.line = element_line(size = 0.5, colour = "black", linetype=1),
                               axis.text = element_text(colour = "black"))#element_text(angle = 45, vjust = 1, hjust = 1))
);grid.draw(beta_plot)

ag_df = mob_summaries[[1]]
ag_ymax = max(ag_df$gamma_S_median, na.rm = TRUE)+(0.1*max(ag_df$gamma_S_median, na.rm = TRUE))
if(min(ag_df$gamma_S_median, na.rm = TRUE)-(0.1*min(ag_df$gamma_S_median, na.rm = TRUE)) >= 2){
  ag_ymin = min(ag_df$gamma_S_median, na.rm = TRUE)-(0.1*min(ag_df$gamma_S_median, na.rm = TRUE))
} else{ ag_ymin = 0}
jitter_height = abs(ag_ymax - ag_ymin)*0.01

ag_xmax = max(ag_df$S_quant75, na.rm = TRUE)+(0.3*max(ag_df$S_quant75, na.rm = TRUE))
if(min(ag_df$S_quant25, na.rm = TRUE)-(0.3*min(ag_df$S_quant25, na.rm = TRUE)) >= 2){
  ag_xmin =min(ag_df$S_quant25, na.rm = TRUE)-(0.3*min(ag_df$S_quant25, na.rm = TRUE))
} else{ag_xmin = 0}

ag_df %>%   
  dplyr::mutate(group = factor(group, levels = site_order)) %>%
  ggplot(aes(x = S_median, y = gamma_S_median))+
  #geom_abline(alpha = 0.8, color = 'grey', size = 1.2)+
  geom_errorbarh(aes(color = group, xmin = S_quant25, xmax = S_quant75, linetype = group), height = 0, size = 1.5)+
  geom_point(aes(fill = group, shape = group),size = 2.2)+
  scale_y_continuous(name = expression(""~gamma*"-diversity"),
                     limits = c(ag_ymin,ag_ymax), expand = c(0.01,0.01))+
  scale_x_continuous(name = expression(""~alpha*"-diversity"),
                     limits = c(NA,ag_xmax), expand = c(0.01,0.01))+
  scale_fill_manual(values = color_scheme)+
  scale_color_manual(values = color_scheme)+
  scale_shape_manual(values = shape_scheme)+
  scale_linetype_manual(
    values = c("dotted", "dotted", "solid",
               "solid", "solid", "solid"))+
  guides(color = guide_legend(nrow = 2),
         fill = guide_legend(nrow = 2))+
  #geom_rangeframe(sides = "lb")+
  annotate('text', label = group_names[1], family = 'serif', size = 4,
           x = Inf, y = Inf, hjust = 1, vjust = 1, lineheight = 0.8)+
  theme(legend.position = 'none',
        panel.grid = element_blank(),
        legend.title = element_blank(),
        axis.title = element_blank(),
        axis.line = element_line(size = 0.5, colour = "black", linetype=1),
        axis.text = element_text(colour = "black")) -> ag_plot

full_plot = 
  cowplot::ggdraw()+
  cowplot::draw_plot(ag_plot) +
  cowplot::draw_plot(beta_plot, x = 0.99, y = 0.115, width = .4, height = 0.5,
                     hjust = 1, vjust = 0);full_plot
diversityPlots[[1]] <- full_plot

###
# "Below-Surface\nMicrobes"
b_df = mob_diversities[[2]] %>%
  dplyr::filter(index == 'beta_S')
beta_ymax = max(b_df$value, na.rm = TRUE)+(0.2*max(b_df$value, na.rm=TRUE))
beta_plot = ggplotGrob(b_df %>%
                         ggplot(aes(x = group, y = value))+
                         geom_boxplot(aes(fill = group))+
                         scale_y_continuous(name = expression(''~beta*"-diversity"),
                                            limits = c(0,beta_ymax), expand = c(0.01,0.01),
                                            breaks = c(0,2,4,8,14,22)) +
                         scale_fill_manual(values = color_scheme)+
                         #geom_rangeframe(sides = "lb")+
                         theme(legend.position = 'none',
                               panel.grid = element_blank(),
                               axis.title.x = element_blank(),
                               axis.text.x = element_blank(),
                               panel.background = element_rect(fill = 'transparent', color = NA),
                               plot.background = element_rect(fill = 'white', color = 'black'),
                               axis.line = element_line(size = 0.5, colour = "black", linetype=1),
                               axis.text = element_text(colour = "black"))#element_text(angle = 45, vjust = 1, hjust = 1))
);grid.draw(beta_plot)

ag_df = mob_summaries[[2]]
ag_ymax = max(ag_df$gamma_S_median, na.rm = TRUE)+(0.1*max(ag_df$gamma_S_median, na.rm = TRUE))
if(min(ag_df$gamma_S_median, na.rm = TRUE)-(0.1*min(ag_df$gamma_S_median, na.rm = TRUE)) >= 2){
  ag_ymin = min(ag_df$gamma_S_median, na.rm = TRUE)-(0.1*min(ag_df$gamma_S_median, na.rm = TRUE))
} else{ ag_ymin = 0}
jitter_height = abs(ag_ymax - ag_ymin)*0.01

ag_xmax = max(ag_df$S_quant75, na.rm = TRUE)+(0.2*max(ag_df$S_quant75, na.rm = TRUE))
if(min(ag_df$S_quant25, na.rm = TRUE)-(0.3*min(ag_df$S_quant25, na.rm = TRUE)) >= 2){
  ag_xmin =min(ag_df$S_quant25, na.rm = TRUE)-(0.3*min(ag_df$S_quant25, na.rm = TRUE))
} else{ag_xmin = 0}

ag_df %>%   
  dplyr::mutate(group = factor(group, levels = site_order)) %>%
  ggplot(aes(x = S_median, y = gamma_S_median))+
  #geom_abline(alpha = 0.8, color = 'grey', size = 1.2)+
  geom_errorbarh(aes(color = group, xmin = S_quant25, xmax = S_quant75, linetype = group), height = 0, size = 1.5)+
  geom_point(aes(fill = group, shape = group),size = 2.2)+
  scale_y_continuous(name = expression(""~gamma*"-diversity"),
                     limits = c(ag_ymin,ag_ymax), expand = c(0.01,0.01))+#
  scale_x_continuous(name = expression(""~alpha*"-diversity"),
                     limits = c(NA,500), expand = c(0.01,0.01))+#ag_xmax
  scale_fill_manual(values = color_scheme)+
  scale_color_manual(values = color_scheme)+
  scale_shape_manual(values = shape_scheme)+
  scale_linetype_manual(
    values = c("dotted", "dotted", "solid",
               "solid", "solid", "solid"))+
  guides(color = guide_legend(nrow = 2),
         fill = guide_legend(nrow = 2))+
  #geom_rangeframe(sides = "lb")+
  annotate('text', label = group_names[2], family = 'serif', size = 4,
           x = Inf, y = Inf, hjust = 1, vjust = 1, lineheight = 0.8)+
  theme(legend.position = 'none',
        panel.grid = element_blank(),
        legend.title = element_blank(),
        axis.title = element_blank(),
        axis.line = element_line(size = 0.5, colour = "black", linetype=1),
        axis.text = element_text(colour = "black")) -> ag_plot;ag_plot

full_plot = 
  cowplot::ggdraw()+
  cowplot::draw_plot(ag_plot) +
  cowplot::draw_plot(beta_plot, x = 0.99, y = 0.115, width = .4, height = 0.5,
                     hjust = 1, vjust = 0);full_plot
diversityPlots[[2]] <- full_plot
###
# Plants
b_df = mob_diversities[[3]] %>%
  dplyr::filter(index == 'beta_S')
beta_ymax = max(b_df$value, na.rm = TRUE)+(0.2*max(b_df$value, na.rm=TRUE))
beta_plot = ggplotGrob(b_df %>%
                         ggplot(aes(x = group, y = value))+
                         geom_boxplot(aes(fill = group))+
                         scale_y_continuous(name = expression(''~beta*"-diversity"),
                                            limits = c(0,beta_ymax), expand = c(0.01,0.01),
                                            breaks = c(0,2,4,8,14,22)) +
                         scale_fill_manual(values = color_scheme)+
                         #geom_rangeframe(sides = "lb")+
                         theme(legend.position = 'none',
                               panel.grid = element_blank(),
                               axis.title.x = element_blank(),
                               axis.text.x = element_blank(),
                               panel.background = element_rect(fill = 'transparent', color = NA),
                               plot.background = element_rect(fill = 'white', color = 'black'),
                               axis.line = element_line(size = 0.5, colour = "black", linetype=1),
                               axis.text = element_text(colour = "black"))#element_text(angle = 45, vjust = 1, hjust = 1))
);grid.draw(beta_plot)

ag_df = mob_summaries[[3]]
ag_ymax = max(ag_df$gamma_S_median, na.rm = TRUE)+(0.1*max(ag_df$gamma_S_median, na.rm = TRUE))
if(min(ag_df$gamma_S_median, na.rm = TRUE)-(0.1*min(ag_df$gamma_S_median, na.rm = TRUE)) >= 2){
  ag_ymin = min(ag_df$gamma_S_median, na.rm = TRUE)-(0.1*min(ag_df$gamma_S_median, na.rm = TRUE))
} else{ ag_ymin = 0}
jitter_height = abs(ag_ymax - ag_ymin)*0.01

ag_xmax = max(ag_df$S_quant75, na.rm = TRUE)+(0.3*max(ag_df$S_quant75, na.rm = TRUE))
if(min(ag_df$S_quant25, na.rm = TRUE)-(0.3*min(ag_df$S_quant25, na.rm = TRUE)) >= 2){
  ag_xmin =min(ag_df$S_quant25, na.rm = TRUE)-(0.3*min(ag_df$S_quant25, na.rm = TRUE))
} else{ag_xmin = 0}

ag_df %>%   
  dplyr::mutate(group = factor(group, levels = site_order)) %>%
  ggplot(aes(x = S_median, y = gamma_S_median))+
  #geom_abline(alpha = 0.8, color = 'grey', size = 1.2)+
  geom_errorbarh(aes(color = group, xmin = S_quant25, xmax = S_quant75, linetype = group), height = 0, size = 1.5)+
  geom_point(aes(fill = group, shape = group),size = 2.2)+
  scale_y_continuous(name = expression(""~gamma*"-diversity"),
                     limits = c(ag_ymin,ag_ymax), expand = c(0.01,0.01))+
  scale_x_continuous(name = expression(""~alpha*"-diversity"),
                     limits = c(NA,ag_xmax), expand = c(0.01,0.01))+
  scale_fill_manual(values = color_scheme)+
  scale_color_manual(values = color_scheme)+
  scale_shape_manual(values = shape_scheme)+
  scale_linetype_manual(
    values = c("dotted", "dotted", "solid",
               "solid", "solid", "solid"))+
  guides(color = guide_legend(nrow = 2),
         fill = guide_legend(nrow = 2))+
  #geom_rangeframe(sides = "lb")+
  annotate('text', label = group_names[3], family = 'serif', size = 4,
           x = Inf, y = Inf, hjust = 1, vjust = 1, lineheight = 0.8)+
  theme(legend.position = 'none',
        panel.grid = element_blank(),
        legend.title = element_blank(),
        axis.title = element_blank(),
        axis.line = element_line(size = 0.5, colour = "black", linetype=1),
        axis.text = element_text(colour = "black")) -> ag_plot;ag_plot

full_plot = 
  cowplot::ggdraw()+
  cowplot::draw_plot(ag_plot) +
  cowplot::draw_plot(beta_plot, x = 0.99, y = 0.115, width = .4, height = 0.5,
                     hjust = 1, vjust = 0);full_plot
diversityPlots[[3]] <- full_plot
###
# "Macroinfauna"
b_df = mob_diversities[[4]] %>%
  dplyr::filter(index == 'beta_S')
beta_ymax = max(b_df$value, na.rm = TRUE)+(0.2*max(b_df$value, na.rm=TRUE))
beta_plot = ggplotGrob(b_df %>%
                         ggplot(aes(x = group, y = value))+
                         geom_boxplot(aes(fill = group))+
                         scale_y_continuous(name = expression(''~beta*"-diversity"),
                                            limits = c(0,beta_ymax), expand = c(0.01,0.01),
                                            breaks = c(0,2,4,8,14,22)) +
                         scale_fill_manual(values = color_scheme)+
                         #geom_rangeframe(sides = "lb")+
                         theme(legend.position = 'none',
                               panel.grid = element_blank(),
                               axis.title.x = element_blank(),
                               axis.text.x = element_blank(),
                               panel.background = element_rect(fill = 'transparent', color = NA),
                               plot.background = element_rect(fill = 'white', color = 'black'),
                               axis.line = element_line(size = 0.5, colour = "black", linetype=1),
                               axis.text = element_text(colour = "black"))#element_text(angle = 45, vjust = 1, hjust = 1))
);grid.draw(beta_plot)

ag_df = mob_summaries[[4]]
ag_ymax = max(ag_df$gamma_S_median, na.rm = TRUE)+(0.1*max(ag_df$gamma_S_median, na.rm = TRUE))
if(min(ag_df$gamma_S_median, na.rm = TRUE)-(0.1*min(ag_df$gamma_S_median, na.rm = TRUE)) >= 2){
  ag_ymin = min(ag_df$gamma_S_median, na.rm = TRUE)-(0.1*min(ag_df$gamma_S_median, na.rm = TRUE))
} else{ ag_ymin = 0}
jitter_height = abs(ag_ymax - ag_ymin)*0.01

ag_xmax = max(ag_df$S_quant75, na.rm = TRUE)+(0.7*max(ag_df$S_quant75, na.rm = TRUE))
if(min(ag_df$S_quant25, na.rm = TRUE)-(0.3*min(ag_df$S_quant25, na.rm = TRUE)) >= 2){
  ag_xmin =min(ag_df$S_quant25, na.rm = TRUE)-(0.3*min(ag_df$S_quant25, na.rm = TRUE))
} else{ag_xmin = 0}

ag_df %>%   
  dplyr::mutate(group = factor(group, levels = site_order)) %>%
  ggplot(aes(x = S_median, y = gamma_S_median))+
  #geom_abline(alpha = 0.8, color = 'grey', size = 1.2)+
  geom_errorbarh(aes(color = group, xmin = S_quant25, xmax = S_quant75, linetype = group), height = 0, size = 1.5)+
  geom_point(aes(fill = group, shape = group),size = 2.2)+
  scale_y_continuous(name = expression(""~gamma*"-diversity"),
                     limits = c(ag_ymin,ag_ymax), expand = c(0.01,0.01))+
  scale_x_continuous(name = expression(""~alpha*"-diversity"),
                     limits = c(NA,11), expand = c(0.01,0.01))+#ag_xmax
  scale_fill_manual(values = color_scheme)+
  scale_color_manual(values = color_scheme)+
  scale_shape_manual(values = shape_scheme)+
  scale_linetype_manual(
    values = c("dotted", "dotted", "solid",
               "solid", "solid", "solid"))+
  guides(color = guide_legend(nrow = 2),
         fill = guide_legend(nrow = 2))+
  #geom_rangeframe(sides = "lb")+
  annotate('text', label = group_names[4], family = 'serif', size = 4,
           x = Inf, y = Inf, hjust = 1, vjust = 1, lineheight = 0.8)+
  theme(legend.position = 'none',
        panel.grid = element_blank(),
        legend.title = element_blank(),
        axis.title = element_blank(),
        axis.line = element_line(size = 0.5, colour = "black", linetype=1),
        axis.text = element_text(colour = "black")) -> ag_plot;ag_plot

full_plot = 
  cowplot::ggdraw()+
  cowplot::draw_plot(ag_plot) +
  cowplot::draw_plot(beta_plot, x = 0.99, y = 0.115, width = .4, height = 0.5,
                     hjust = 1, vjust = 0);full_plot
diversityPlots[[4]] <- full_plot
###
# "Spiders"
b_df = mob_diversities[[5]] %>%
  dplyr::filter(index == 'beta_S')
beta_ymax = max(b_df$value, na.rm = TRUE)+(0.2*max(b_df$value, na.rm=TRUE))
beta_plot = ggplotGrob(b_df %>%
                         ggplot(aes(x = group, y = value))+
                         geom_boxplot(aes(fill = group))+
                         scale_y_continuous(name = expression(''~beta*"-diversity"),
                                            limits = c(0,beta_ymax), expand = c(0.01,0.01),
                                            breaks = c(0,2,4,8,14,22)) +
                         scale_fill_manual(values = color_scheme)+
                         #geom_rangeframe(sides = "lb")+
                         theme(legend.position = 'none',
                               panel.grid = element_blank(),
                               axis.title.x = element_blank(),
                               axis.text.x = element_blank(),
                               panel.background = element_rect(fill = 'transparent', color = NA),
                               plot.background = element_rect(fill = 'white', color ='black'),
                               axis.line = element_line(size = 0.5, colour = "black", linetype=1),
                               axis.text = element_text(colour = "black"))#element_text(angle = 45, vjust = 1, hjust = 1))
);grid.draw(beta_plot)

ag_df = mob_summaries[[5]]
ag_ymax = max(ag_df$gamma_S_median, na.rm = TRUE)+(0.1*max(ag_df$gamma_S_median, na.rm = TRUE))
if(min(ag_df$gamma_S_median, na.rm = TRUE)-(0.1*min(ag_df$gamma_S_median, na.rm = TRUE)) >= 2){
  ag_ymin = min(ag_df$gamma_S_median, na.rm = TRUE)-(0.1*min(ag_df$gamma_S_median, na.rm = TRUE))
} else{ ag_ymin = 0}
jitter_height = abs(ag_ymax - ag_ymin)*0.01

ag_xmax = max(ag_df$S_quant75, na.rm = TRUE)+(0.5*max(ag_df$S_quant75, na.rm = TRUE))
if(min(ag_df$S_quant25, na.rm = TRUE)-(0.3*min(ag_df$S_quant25, na.rm = TRUE)) >= 2){
  ag_xmin =min(ag_df$S_quant25, na.rm = TRUE)-(0.3*min(ag_df$S_quant25, na.rm = TRUE))
} else{ag_xmin = 0}

ag_df %>%   
  dplyr::mutate(group = factor(group, levels = site_order)) %>%
  ggplot(aes(x = S_median, y = gamma_S_median))+
  #geom_abline(alpha = 0.8, color = 'grey', size = 1.2)+
  geom_errorbarh(aes(color = group, xmin = S_quant25, xmax = S_quant75, linetype = group), height = 0, size = 1.5)+
  geom_point(aes(fill = group, shape = group),size = 2.2)+
  scale_y_continuous(name = expression(""~gamma*"-diversity"),
                     limits = c(ag_ymin,ag_ymax), expand = c(0.01,0.01))+
  scale_x_continuous(name = expression(""~alpha*"-diversity"),
                     limits = c(NA,14), expand = c(0.01,0.01))+#ag_xmax
  scale_fill_manual(values = color_scheme)+
  scale_color_manual(values = color_scheme)+
  scale_shape_manual(values = shape_scheme)+
  scale_linetype_manual(
    values = c("dotted", "dotted", "solid",
               "solid", "solid", "solid"))+
  guides(color = guide_legend(nrow = 2),
         fill = guide_legend(nrow = 2))+
  #geom_rangeframe(sides = "lb")+
  annotate('text', label = group_names[5], family = 'serif', size = 4,
           x = Inf, y = Inf, hjust = 1, vjust = 1, lineheight = 0.8)+
  theme(legend.position = 'none',
        panel.grid = element_blank(),
        legend.title = element_blank(),
        axis.title = element_blank(),
        axis.line = element_line(size = 0.5, colour = "black", linetype=1),
        axis.text = element_text(colour = "black")) -> ag_plot;ag_plot

full_plot = 
  cowplot::ggdraw()+
  cowplot::draw_plot(ag_plot) +
  cowplot::draw_plot(beta_plot, x = 0.99, y = 0.115, width = .4, height = 0.5,
                     hjust = 1, vjust = 0);full_plot
diversityPlots[[5]] <- full_plot
###
# On-marsh nekton
b_df = mob_diversities[[6]] %>%
  dplyr::filter(index == 'beta_S')
beta_ymax = max(b_df$value, na.rm = TRUE)+(0.2*max(b_df$value, na.rm=TRUE))
beta_plot = ggplotGrob(b_df %>%
                         ggplot(aes(x = group, y = value))+
                         geom_boxplot(aes(fill = group))+
                         scale_y_continuous(name = expression(''~beta*"-diversity"),
                                            limits = c(0,beta_ymax), expand = c(0.01,0.01),
                                            breaks = c(0,2,4,8,14,22)) +
                         scale_fill_manual(values = color_scheme)+
                         #geom_rangeframe(sides = "lb")+
                         theme(legend.position = 'none',
                               panel.grid = element_blank(),
                               axis.title.x = element_blank(),
                               axis.text.x = element_blank(),
                               panel.background = element_rect(fill = 'transparent', color = NA),
                               plot.background = element_rect(fill = 'white', color = 'black'),
                               axis.line = element_line(size = 0.5, colour = "black", linetype=1),
                               axis.text = element_text(colour = "black"))#element_text(angle = 45, vjust = 1, hjust = 1))
);grid.draw(beta_plot)

ag_df = mob_summaries[[6]]
ag_ymax = max(ag_df$gamma_S_median, na.rm = TRUE)+(0.1*max(ag_df$gamma_S_median, na.rm = TRUE))
if(min(ag_df$gamma_S_median, na.rm = TRUE)-(0.1*min(ag_df$gamma_S_median, na.rm = TRUE)) >= 2){
  ag_ymin = min(ag_df$gamma_S_median, na.rm = TRUE)-(0.1*min(ag_df$gamma_S_median, na.rm = TRUE))
} else{ ag_ymin = 0}
jitter_height = abs(ag_ymax - ag_ymin)*0.01

ag_xmax = max(ag_df$S_quant75, na.rm = TRUE)+(0.4*max(ag_df$S_quant75, na.rm = TRUE))
if(min(ag_df$S_quant25, na.rm = TRUE)-(0.3*min(ag_df$S_quant25, na.rm = TRUE)) >= 2){
  ag_xmin =min(ag_df$S_quant25, na.rm = TRUE)-(0.3*min(ag_df$S_quant25, na.rm = TRUE))
} else{ag_xmin = 0}

ag_df %>%   
  dplyr::mutate(group = factor(group, levels = site_order)) %>%
  ggplot(aes(x = S_median, y = gamma_S_median))+
  #geom_abline(alpha = 0.8, color = 'grey', size = 1.2)+
  geom_errorbarh(aes(color = group, xmin = S_quant25, xmax = S_quant75, linetype = group), height = 0, size = 1.5)+
  geom_point(aes(fill = group, shape = group),size = 2.2)+
  scale_y_continuous(name = expression(""~gamma*"-diversity"),
                     limits = c(ag_ymin,ag_ymax), expand = c(0.01,0.01))+
  scale_x_continuous(name = expression(""~alpha*"-diversity"),
                     limits = c(NA,10), expand = c(0.01,0.01))+#ag_xmax
  scale_fill_manual(values = color_scheme)+
  scale_color_manual(values = color_scheme)+
  scale_shape_manual(values = shape_scheme)+
  scale_linetype_manual(
    values = c("dotted", "dotted", "solid",
               "solid", "solid", "solid"))+
  guides(color = guide_legend(nrow = 2),
         fill = guide_legend(nrow = 2))+
  #geom_rangeframe(sides = "lb")+
  annotate('text', label = group_names[6], family = 'serif', size = 4,
           x = Inf, y = Inf, hjust = 1, vjust = 1, lineheight = 0.8)+
  theme(legend.position = 'none',
        panel.grid = element_blank(),
        legend.title = element_blank(),
        axis.title = element_blank(),
        axis.line = element_line(size = 0.5, colour = "black", linetype=1),
        axis.text = element_text(colour = "black")) -> ag_plot;ag_plot

full_plot = 
  cowplot::ggdraw()+
  cowplot::draw_plot(ag_plot) +
  cowplot::draw_plot(beta_plot, x = 0.99, y = 0.115, width = .4, height = 0.5,
                     hjust = 1, vjust = 0);full_plot
diversityPlots[[6]] <- full_plot
###
# Off-marsh nekton
b_df = mob_diversities[[8]] %>%
  dplyr::filter(index == 'beta_S')
beta_ymax = max(b_df$value, na.rm = TRUE)+(0.2*max(b_df$value, na.rm=TRUE))
beta_plot = ggplotGrob(b_df %>%
                         ggplot(aes(x = group, y = value))+
                         geom_boxplot(aes(fill = group))+
                         scale_y_continuous(name = expression(''~beta*"-diversity"),
                                            limits = c(0,beta_ymax), expand = c(0.01,0.01),
                                            breaks = c(0,2,4,8,14,22)) +
                         scale_fill_manual(values = color_scheme)+
                         #geom_rangeframe(sides = "lb")+
                         theme(legend.position = 'none',
                               panel.grid = element_blank(),
                               axis.title.x = element_blank(),
                               axis.text.x = element_blank(),
                               panel.background = element_rect(fill = 'transparent', color = NA),
                               plot.background = element_rect(fill = 'white', color = 'black'),
                               axis.line = element_line(size = 0.5, colour = "black", linetype=1),
                               axis.text = element_text(colour = "black"))#element_text(angle = 45, vjust = 1, hjust = 1))
);grid.draw(beta_plot)

ag_df = mob_summaries[[8]]
ag_ymax = max(ag_df$gamma_S_median, na.rm = TRUE)+(0.1*max(ag_df$gamma_S_median, na.rm = TRUE))
if(min(ag_df$gamma_S_median, na.rm = TRUE)-(0.2*min(ag_df$gamma_S_median, na.rm = TRUE)) >= 2){
  ag_ymin = min(ag_df$gamma_S_median, na.rm = TRUE)-(0.2*min(ag_df$gamma_S_median, na.rm = TRUE))
} else{ ag_ymin = 0}
jitter_height = abs(ag_ymax - ag_ymin)*0.01

ag_xmax = max(ag_df$S_quant75, na.rm = TRUE)+(0.9*max(ag_df$S_quant75, na.rm = TRUE))
if(min(ag_df$S_quant25, na.rm = TRUE)-(0.3*min(ag_df$S_quant25, na.rm = TRUE)) >= 2){
  ag_xmin =min(ag_df$S_quant25, na.rm = TRUE)-(0.3*min(ag_df$S_quant25, na.rm = TRUE))
} else{ag_xmin = 0}

ag_df %>%   
  dplyr::mutate(group = factor(group, levels = site_order)) %>%
  ggplot(aes(x = S_median, y = gamma_S_median))+
  #geom_abline(alpha = 0.8, color = 'grey', size = 1.2)+
  geom_errorbarh(aes(color = group, xmin = S_quant25, xmax = S_quant75, linetype = group), height = 0, size = 1.5)+
  geom_point(aes(fill = group, shape = group),size = 2.2)+
  scale_y_continuous(name = expression(""~gamma*"-diversity"),
                     limits = c(ag_ymin,ag_ymax), expand = c(0.01,0.01))+
  scale_x_continuous(name = expression(""~alpha*"-diversity"),
                     limits = c(NA,13), expand = c(0.01,0.01))+#ag_xmax
  scale_fill_manual(values = color_scheme)+
  scale_color_manual(values = color_scheme)+
  scale_shape_manual(values = shape_scheme)+
  scale_linetype_manual(
    values = c("dotted", "dotted", "solid",
               "solid", "solid", "solid"))+
  guides(color = guide_legend(nrow = 2),
         fill = guide_legend(nrow = 2))+
  #geom_rangeframe(sides = "lb")+
  annotate('text', label = group_names[8], family = 'serif', size = 4,
           x = Inf, y = Inf, hjust = 1, vjust = 1, lineheight = 0.8)+
  theme(legend.position = 'none',
        panel.grid = element_blank(),
        legend.title = element_blank(),
        axis.title = element_blank(),
        axis.line = element_line(size = 0.5, colour = "black", linetype=1),
        axis.text = element_text(colour = "black")) -> ag_plot;ag_plot

full_plot = 
  cowplot::ggdraw()+
  cowplot::draw_plot(ag_plot) +
  cowplot::draw_plot(beta_plot, x = 0.99, y = 0.115, width = .4, height = 0.5,
                     hjust = 1, vjust = 0);full_plot
diversityPlots[[8]] <- full_plot
###

plot_legend = 
  cowplot::get_legend(mob_summaries[[1]] %>%
                        # pluck(x) %>%
                        dplyr::mutate(group =factor(group, levels = site_order)) %>%
                        ggplot(aes(x = S_median, y = gamma_S_median))+
                        geom_point(aes(fill = group, shape = group),size = 3.3)+
                        scale_fill_manual(values = color_scheme)+
                        scale_color_manual(values = color_scheme)+
                        scale_shape_manual(values = shape_scheme)+
                        guides(color = guide_legend(nrow = 2),
                               fill = guide_legend(nrow = 2))+
                        scale_linetype_manual(
                          values = c("dotted", "dotted", "solid",
                                     "solid", "solid", "solid"))+
                        theme(legend.title = element_blank(),
                              legend.text = element_text(family = 'serif', size = 14),
                              legend.margin= margin(c(t = 1,r = 5,b = 1,l = 5))))

plot_legend$grobs <- lapply(plot_legend$grobs, function(z) modifyList(z, list(x = unit(0.5, 'npc'),
                                                                              y = unit(1, 'npc'),
                                                                              vjust = 0)))

png(here::here("figures/s_diversities.png"), width = 8, height = 10, units = "in", res = 480 )
full_plot = gridExtra::grid.arrange(diversityPlots[[1]], diversityPlots[[2]],
                                    diversityPlots[[3]], diversityPlots[[4]],
                                    diversityPlots[[5]], diversityPlots[[6]],
                                    diversityPlots[[8]],
                                    plot_legend,
                                    ncol = 2,
                                    left = textGrob(label = expression(""~gamma*"-diversity"),
                                                    gp = gpar(fontfamily = 'serif'), rot = 90),
                                    bottom = textGrob(label = expression(""~alpha*"-diversity"),
                                                      gp = gpar(fontfamily = 'serif')))
dev.off()

## S_PIE #####

mob_alphabeta = mob_stats %>%
  purrr::map(~.x %>% 
               pluck('samples_stats') %>% 
               data.frame %>% 
               dplyr::filter(index %in% c('S_PIE','beta_S_PIE')) %>%
               dplyr::mutate(index = recode_factor(index, !!!c(S_PIE = 'S', beta_S_PIE ='beta_S'))) %>%
               dplyr::mutate(across(where(is.factor), droplevels))%>%
               dplyr::mutate(group =factor(group, levels = site_order))) #%>%
# bind_rows(.id = 'taxa_group')

mob_gamma = mob_stats %>%
  purrr::map(~.x %>%
               pluck('groups_stats') %>%
               data.frame %>%
               dplyr::filter(index == 'S_PIE') %>% #& effort == min(effort, na.rm = TRUE)) %>%
               dplyr::mutate(index = recode_factor(index, S_PIE = 'gamma_S')) %>%
               dplyr::mutate(across(where(is.factor), droplevels)) %>%
               dplyr::mutate(group =factor(group, levels = site_order)))

mob_diversities = purrr::map2(mob_alphabeta, mob_gamma, ~bind_rows(.x,.y))

mob_summaries = mob_diversities%>%
  purrr::map(~.x %>%
               group_by(group, index) %>%
               dplyr::summarise(across(value, list(mean = ~mean(.x, na.rm = TRUE),
                                                   median = ~median(.x, na.rm = TRUE),
                                                   quant2.5 = ~quantile(.x, 0.025, na.rm = TRUE),
                                                   quant25 = ~quantile(.x, 0.25, na.rm = TRUE),
                                                   quant75 = ~quantile(.x, 0.75, na.rm = TRUE),
                                                   quant97.5 = ~quantile(.x, 0.975, na.rm = TRUE),
                                                   min = ~min(.x, na.rm= TRUE),
                                                   max = ~max(.x, na.rm = TRUE)),
                                       .names = "{.fn}")) %>%
               pivot_longer(-group:-index, names_to = 'stat', values_to = 'value') %>%
               tidyr::unite("stat", index,stat) %>%
               pivot_wider(names_from = stat, values_from = value) %>%
               dplyr::mutate(group =factor(group, levels = site_order)))

diversityPlots = list()

# "Surface\nMicrobes"
b_df = mob_diversities[[1]] %>%
  dplyr::filter(index == 'beta_S')
beta_ymax = max(b_df$value, na.rm = TRUE)+(0.2*max(b_df$value, na.rm=TRUE))
beta_plot = ggplotGrob(b_df %>%
                         ggplot(aes(x = group, y = value))+
                         geom_boxplot(aes(fill = group))+
                         scale_y_continuous(name = expression(''~beta*"-diversity"),
                                            limits = c(0,beta_ymax), expand = c(0.01,0.01),
                                            breaks = c(0,1,2,4,8,14,22)) +
                         scale_fill_manual(values = color_scheme)+
                         #geom_rangeframe(sides = "lb")+
                         theme(legend.position = 'none',
                               panel.grid = element_blank(),
                               axis.title.x = element_blank(),
                               axis.text.x = element_blank(),
                               panel.background = element_rect(fill = 'transparent', color = NA),
                               plot.background = element_rect(fill = 'white', color = 'black'),
                               axis.line = element_line(size = 0.5, colour = "black", linetype=1),
                               axis.text = element_text(colour = "black"))#element_text(angle = 45, vjust = 1, hjust = 1))
);grid.draw(beta_plot)

ag_df = mob_summaries[[1]]
ag_ymax = max(ag_df$gamma_S_median, na.rm = TRUE)+(0.1*max(ag_df$gamma_S_median, na.rm = TRUE))
if(min(ag_df$gamma_S_median, na.rm = TRUE)-(0.1*min(ag_df$gamma_S_median, na.rm = TRUE)) >= 2){
  ag_ymin = min(ag_df$gamma_S_median, na.rm = TRUE)-(0.1*min(ag_df$gamma_S_median, na.rm = TRUE))
} else{ ag_ymin = 0}
jitter_height = abs(ag_ymax - ag_ymin)*0.01

ag_xmax = max(ag_df$S_quant75, na.rm = TRUE)+(0.25*max(ag_df$S_quant75, na.rm = TRUE))
if(min(ag_df$S_quant25, na.rm = TRUE)-(0.3*min(ag_df$S_quant25, na.rm = TRUE)) >= 2){
  ag_xmin =min(ag_df$S_quant25, na.rm = TRUE)-(0.3*min(ag_df$S_quant25, na.rm = TRUE))
} else{ag_xmin = 0}

ag_ymax =50
ag_df %>%   
  dplyr::mutate(group = factor(group, levels = site_order)) %>%
  ggplot(aes(x = S_median, y = gamma_S_median))+
  #geom_abline(alpha = 0.8, color = 'grey', size = 1.2)+
  geom_errorbarh(aes(color = group, xmin = S_quant25, xmax = S_quant75, linetype = group), height = 0, size = 1.5)+
  geom_point(aes(fill = group, shape = group),size = 2.2)+
  scale_y_continuous(name = expression(""~gamma*"-diversity"),
                     limits = c(ag_ymin,ag_ymax), expand = c(0.01,0.01))+
  scale_x_continuous(name = expression(""~alpha*"-diversity"),
                     limits = c(NA,46), expand = c(0.01,0.01))+#ag_xmax
  scale_fill_manual(values = color_scheme)+
  scale_color_manual(values = color_scheme)+
  scale_shape_manual(values = shape_scheme)+
  scale_linetype_manual(
    values = c("dotted", "dotted", "solid",
               "solid", "solid", "solid"))+
  guides(color = guide_legend(nrow = 2),
         fill = guide_legend(nrow = 2))+
  #geom_rangeframe(sides = "lb")+
  annotate('text', label = group_names[1], family = 'serif', size = 4,
           x = Inf, y = Inf, hjust = 1, vjust = 1, lineheight = 0.8)+
  theme(legend.position = 'none',
        panel.grid = element_blank(),
        legend.title = element_blank(),
        axis.title = element_blank(),
        axis.line = element_line(size = 0.5, colour = "black", linetype=1),
        axis.text = element_text(colour = "black")) -> ag_plot

full_plot = 
  cowplot::ggdraw()+
  cowplot::draw_plot(ag_plot) +
  cowplot::draw_plot(beta_plot, x = 0.99, y = 0.115, width = .4, height = 0.5,
                     hjust = 1, vjust = 0);full_plot
diversityPlots[[1]] <- full_plot

###
# "Below-Surface\nMicrobes"
b_df = mob_diversities[[2]] %>%
  dplyr::filter(index == 'beta_S')
beta_ymax = max(b_df$value, na.rm = TRUE)+(0.2*max(b_df$value, na.rm=TRUE))
beta_plot = ggplotGrob(b_df %>%
                         ggplot(aes(x = group, y = value))+
                         geom_boxplot(aes(fill = group))+
                         scale_y_continuous(name = expression(''~beta*"-diversity"),
                                            limits = c(0,beta_ymax), expand = c(0.01,0.01),
                                            breaks = c(0,1,2,4,8,14,22)) +
                         scale_fill_manual(values = color_scheme)+
                         #geom_rangeframe(sides = "lb")+
                         theme(legend.position = 'none',
                               panel.grid = element_blank(),
                               axis.title.x = element_blank(),
                               axis.text.x = element_blank(),
                               panel.background = element_rect(fill = 'transparent', color = NA),
                               plot.background = element_rect(fill = 'white', color = 'black'),
                               axis.line = element_line(size = 0.5, colour = "black", linetype=1),
                               axis.text = element_text(colour = "black"))#element_text(angle = 45, vjust = 1, hjust = 1))
);grid.draw(beta_plot)

ag_df = mob_summaries[[2]]
ag_ymax = max(ag_df$gamma_S_median, na.rm = TRUE)+(0.1*max(ag_df$gamma_S_median, na.rm = TRUE))
if(min(ag_df$gamma_S_median, na.rm = TRUE)-(0.1*min(ag_df$gamma_S_median, na.rm = TRUE)) >= 2){
  ag_ymin = min(ag_df$gamma_S_median, na.rm = TRUE)-(0.1*min(ag_df$gamma_S_median, na.rm = TRUE))
} else{ ag_ymin = 0}
jitter_height = abs(ag_ymax - ag_ymin)*0.01

ag_xmax = max(ag_df$S_quant75, na.rm = TRUE)+(0.2*max(ag_df$S_quant75, na.rm = TRUE))
if(min(ag_df$S_quant25, na.rm = TRUE)-(0.3*min(ag_df$S_quant25, na.rm = TRUE)) >= 2){
  ag_xmin =min(ag_df$S_quant25, na.rm = TRUE)-(0.3*min(ag_df$S_quant25, na.rm = TRUE))
} else{ag_xmin = 0}

ag_df %>%   
  dplyr::mutate(group = factor(group, levels = site_order)) %>%
  ggplot(aes(x = S_median, y = gamma_S_median))+
  #geom_abline(alpha = 0.8, color = 'grey', size = 1.2)+
  geom_errorbarh(aes(color = group, xmin = S_quant25, xmax = S_quant75, linetype = group), height = 0, size = 1.5)+
  geom_point(aes(fill = group, shape = group),size = 2.2)+
  scale_y_continuous(name = expression(""~gamma*"-diversity"),
                     limits = c(ag_ymin,ag_ymax), expand = c(0.01,0.01))+
  scale_x_continuous(name = expression(""~alpha*"-diversity"),
                     limits = c(NA,38), expand = c(0.01,0.01))+#ag_xmax
  scale_fill_manual(values = color_scheme)+
  scale_color_manual(values = color_scheme)+
  scale_shape_manual(values = shape_scheme)+
  scale_linetype_manual(
    values = c("dotted", "dotted", "solid",
               "solid", "solid", "solid"))+
  guides(color = guide_legend(nrow = 2),
         fill = guide_legend(nrow = 2))+
  #geom_rangeframe(sides = "lb")+
  annotate('text', label = group_names[2], family = 'serif', size = 4,
           x = Inf, y = Inf, hjust = 1, vjust = 1, lineheight = 0.8)+
  theme(legend.position = 'none',
        panel.grid = element_blank(),
        legend.title = element_blank(),
        axis.title = element_blank(),
        axis.line = element_line(size = 0.5, colour = "black", linetype=1),
        axis.text = element_text(colour = "black")) -> ag_plot;ag_plot

full_plot = 
  cowplot::ggdraw()+
  cowplot::draw_plot(ag_plot) +
  cowplot::draw_plot(beta_plot, x = 0.99, y = 0.115, width = .4, height = 0.5,
                     hjust = 1, vjust = 0);full_plot
diversityPlots[[2]] <- full_plot
###
# Plants
b_df = mob_diversities[[3]] %>%
  dplyr::filter(index == 'beta_S')
beta_ymax = max(b_df$value, na.rm = TRUE)+(0.2*max(b_df$value, na.rm=TRUE))
beta_plot = ggplotGrob(b_df %>%
                         ggplot(aes(x = group, y = value))+
                         geom_boxplot(aes(fill = group))+
                         scale_y_continuous(name = expression(''~beta*"-diversity"),
                                            limits = c(0,beta_ymax), expand = c(0.01,0.01),
                                            breaks = c(0,1,2,4,8,14,22)) +
                         scale_fill_manual(values = color_scheme)+
                         #geom_rangeframe(sides = "lb")+
                         theme(legend.position = 'none',
                               panel.grid = element_blank(),
                               axis.title.x = element_blank(),
                               axis.text.x = element_blank(),
                               panel.background = element_rect(fill = 'transparent', color = NA),
                               plot.background = element_rect(fill = 'white', color = 'black'),
                               axis.line = element_line(size = 0.5, colour = "black", linetype=1),
                               axis.text = element_text(colour = "black"))#element_text(angle = 45, vjust = 1, hjust = 1))
);grid.draw(beta_plot)

ag_df = mob_summaries[[3]]
ag_ymax = max(ag_df$gamma_S_median, na.rm = TRUE)+(0.1*max(ag_df$gamma_S_median, na.rm = TRUE))
if(min(ag_df$gamma_S_median, na.rm = TRUE)-(0.1*min(ag_df$gamma_S_median, na.rm = TRUE)) >= 2){
  ag_ymin = min(ag_df$gamma_S_median, na.rm = TRUE)-(0.1*min(ag_df$gamma_S_median, na.rm = TRUE))
} else{ ag_ymin = 0}
jitter_height = abs(ag_ymax - ag_ymin)*0.01

ag_xmax = max(ag_df$S_quant75, na.rm = TRUE)+(0.3*max(ag_df$S_quant75, na.rm = TRUE))
if(min(ag_df$S_quant25, na.rm = TRUE)-(0.3*min(ag_df$S_quant25, na.rm = TRUE)) >= 2){
  ag_xmin =min(ag_df$S_quant25, na.rm = TRUE)-(0.3*min(ag_df$S_quant25, na.rm = TRUE))
} else{ag_xmin = 0}

ag_df %>%   
  dplyr::mutate(group = factor(group, levels = site_order)) %>%
  ggplot(aes(x = S_median, y = gamma_S_median))+
  #geom_abline(alpha = 0.8, color = 'grey', size = 1.2)+
  geom_errorbarh(aes(color = group, xmin = S_quant25, xmax = S_quant75, linetype = group), height = 0, size = 1.5)+
  geom_point(aes(fill = group, shape = group),size = 2.2)+
  scale_y_continuous(name = expression(""~gamma*"-diversity"),
                     limits = c(ag_ymin,ag_ymax), expand = c(0.01,0.01))+
  scale_x_continuous(name = expression(""~alpha*"-diversity"),
                     limits = c(NA,ag_xmax), expand = c(0.01,0.01))+
  scale_fill_manual(values = color_scheme)+
  scale_color_manual(values = color_scheme)+
  scale_shape_manual(values = shape_scheme)+
  scale_linetype_manual(
    values = c("dotted", "dotted", "solid",
               "solid", "solid", "solid"))+
  guides(color = guide_legend(nrow = 2),
         fill = guide_legend(nrow = 2))+
  #geom_rangeframe(sides = "lb")+
  annotate('text', label = group_names[3], family = 'serif', size = 4,
           x = Inf, y = Inf, hjust = 1, vjust = 1, lineheight = 0.8)+
  theme(legend.position = 'none',
        panel.grid = element_blank(),
        legend.title = element_blank(),
        axis.title = element_blank(),
        axis.line = element_line(size = 0.5, colour = "black", linetype=1),
        axis.text = element_text(colour = "black")) -> ag_plot;ag_plot

full_plot = 
  cowplot::ggdraw()+
  cowplot::draw_plot(ag_plot) +
  cowplot::draw_plot(beta_plot, x = 0.99, y = 0.115, width = .4, height = 0.5,
                     hjust = 1, vjust = 0);full_plot
diversityPlots[[3]] <- full_plot
###
# "Macroinfauna"
b_df = mob_diversities[[4]] %>%
  dplyr::filter(index == 'beta_S')
beta_ymax = max(b_df$value, na.rm = TRUE)+(0.2*max(b_df$value, na.rm=TRUE))
beta_plot = ggplotGrob(b_df %>% dplyr::select(-effort) %>% na.omit %>%
                         ggplot(aes(x = group, y = value))+
                         geom_boxplot(aes(fill = group))+
                         scale_y_continuous(name = expression(''~beta*"-diversity"),
                                            limits = c(0,beta_ymax), expand = c(0.01,0.01),
                                            breaks = c(0,1,2,4,8,14,22)) +
                         scale_fill_manual(values = color_scheme)+
                         #geom_rangeframe(sides = "lb")+
                         theme(legend.position = 'none',
                               panel.grid = element_blank(),
                               axis.title.x = element_blank(),
                               axis.text.x = element_blank(),
                               panel.background = element_rect(fill = 'transparent', color = NA),
                               plot.background = element_rect(fill = 'white', color = 'black'),
                               axis.line = element_line(size = 0.5, colour = "black", linetype=1),
                               axis.text = element_text(colour = "black"))#element_text(angle = 45, vjust = 1, hjust = 1))
);grid.draw(beta_plot)

ag_df = mob_summaries[[4]]
ag_ymax = max(ag_df$gamma_S_median, na.rm = TRUE)+(0.1*max(ag_df$gamma_S_median, na.rm = TRUE))
if(min(ag_df$gamma_S_median, na.rm = TRUE)-(0.1*min(ag_df$gamma_S_median, na.rm = TRUE)) >= 2){
  ag_ymin = min(ag_df$gamma_S_median, na.rm = TRUE)-(0.1*min(ag_df$gamma_S_median, na.rm = TRUE))
} else{ ag_ymin = 0}
jitter_height = abs(ag_ymax - ag_ymin)*0.01

ag_xmax = max(ag_df$S_quant75, na.rm = TRUE)+(0.2*max(ag_df$S_quant75, na.rm = TRUE))
if(min(ag_df$S_quant25, na.rm = TRUE)-(0.3*min(ag_df$S_quant25, na.rm = TRUE)) >= 2){
  ag_xmin =min(ag_df$S_quant25, na.rm = TRUE)-(0.3*min(ag_df$S_quant25, na.rm = TRUE))
} else{ag_xmin = 0}

ag_df %>%   
  dplyr::mutate(group = factor(group, levels = site_order)) %>%
  ggplot(aes(x = S_median, y = gamma_S_median))+
  #geom_abline(alpha = 0.8, color = 'grey', size = 1.2)+
  geom_errorbarh(aes(color = group, xmin = S_quant25, xmax = S_quant75, linetype = group), height = 0, size = 1.5)+
  geom_point(aes(fill = group, shape = group),size = 2.2)+
  scale_y_continuous(name = expression(""~gamma*"-diversity"),
                     limits = c(ag_ymin,ag_ymax), expand = c(0.01,0.01))+
  scale_x_continuous(name = expression(""~alpha*"-diversity"),
                     limits = c(NA,ag_xmax), expand = c(0.01,0.01))+
  scale_fill_manual(values = color_scheme)+
  scale_color_manual(values = color_scheme)+
  scale_shape_manual(values = shape_scheme)+
  scale_linetype_manual(
    values = c("dotted", "dotted", "solid",
               "solid", "solid", "solid"))+
  guides(color = guide_legend(nrow = 2),
         fill = guide_legend(nrow = 2))+
  #geom_rangeframe(sides = "lb")+
  annotate('text', label = group_names[4], family = 'serif', size = 4,
           x = Inf, y = Inf, hjust = 1, vjust = 1, lineheight = 0.8)+
  theme(legend.position = 'none',
        panel.grid = element_blank(),
        legend.title = element_blank(),
        axis.title = element_blank(),
        axis.line = element_line(size = 0.5, colour = "black", linetype=1),
        axis.text = element_text(colour = "black")) -> ag_plot;ag_plot

full_plot = 
  cowplot::ggdraw()+
  cowplot::draw_plot(ag_plot) +
  cowplot::draw_plot(beta_plot, x = 0.99, y = 0.115, width = .4, height = 0.5,
                     hjust = 1, vjust = 0);full_plot
diversityPlots[[4]] <- full_plot
###
# "Spiders"
b_df = mob_diversities[[5]] %>%
  dplyr::filter(index == 'beta_S')
beta_ymax = max(b_df$value, na.rm = TRUE)+(0.2*max(b_df$value, na.rm=TRUE))
beta_plot = ggplotGrob(b_df %>%
                         ggplot(aes(x = group, y = value))+
                         geom_boxplot(aes(fill = group))+
                         scale_y_continuous(name = expression(''~beta*"-diversity"),
                                            limits = c(0,beta_ymax), expand = c(0.01,0.01),
                                            breaks = c(0,1,2,4,8,14,22)) +
                         scale_fill_manual(values = color_scheme)+
                         #geom_rangeframe(sides = "lb")+
                         theme(legend.position = 'none',
                               panel.grid = element_blank(),
                               axis.title.x = element_blank(),
                               axis.text.x = element_blank(),
                               panel.background = element_rect(fill = 'transparent', color = NA),
                               plot.background = element_rect(fill = 'white', color = 'black'),
                               axis.line = element_line(size = 0.5, colour = "black", linetype=1),
                               axis.text = element_text(colour = "black"))#element_text(angle = 45, vjust = 1, hjust = 1))
);grid.draw(beta_plot)

ag_df = mob_summaries[[5]]
ag_ymax = max(ag_df$gamma_S_median, na.rm = TRUE)+(0.1*max(ag_df$gamma_S_median, na.rm = TRUE))
if(min(ag_df$gamma_S_median, na.rm = TRUE)-(0.1*min(ag_df$gamma_S_median, na.rm = TRUE)) >= 2){
  ag_ymin = min(ag_df$gamma_S_median, na.rm = TRUE)-(0.1*min(ag_df$gamma_S_median, na.rm = TRUE))
} else{ ag_ymin = 0}
jitter_height = abs(ag_ymax - ag_ymin)*0.01

ag_xmax = max(ag_df$S_quant75, na.rm = TRUE)+(0.2*max(ag_df$S_quant75, na.rm = TRUE))
if(min(ag_df$S_quant25, na.rm = TRUE)-(0.3*min(ag_df$S_quant25, na.rm = TRUE)) >= 2){
  ag_xmin =min(ag_df$S_quant25, na.rm = TRUE)-(0.3*min(ag_df$S_quant25, na.rm = TRUE))
} else{ag_xmin = 0}

ag_df %>%   
  dplyr::mutate(group = factor(group, levels = site_order)) %>%
  ggplot(aes(x = S_median, y = gamma_S_median))+
  #geom_abline(alpha = 0.8, color = 'grey', size = 1.2)+
  geom_errorbarh(aes(color = group, xmin = S_quant25, xmax = S_quant75, linetype = group), height = 0, size = 1.5)+
  geom_point(aes(fill = group, shape = group),size = 2.2)+
  scale_y_continuous(name = expression(""~gamma*"-diversity"),
                     limits = c(ag_ymin,ag_ymax), expand = c(0.01,0.01))+
  scale_x_continuous(name = expression(""~alpha*"-diversity"),
                     limits = c(NA,14), expand = c(0.01,0.01))+#ag_xmax
  scale_fill_manual(values = color_scheme)+
  scale_color_manual(values = color_scheme)+
  scale_shape_manual(values = shape_scheme)+
  scale_linetype_manual(
    values = c("dotted", "dotted", "solid",
               "solid", "solid", "solid"))+
  guides(color = guide_legend(nrow = 2),
         fill = guide_legend(nrow = 2))+
  #geom_rangeframe(sides = "lb")+
  annotate('text', label = group_names[5], family = 'serif', size = 4,
           x = Inf, y = Inf, hjust = 1, vjust = 1, lineheight = 0.8)+
  theme(legend.position = 'none',
        panel.grid = element_blank(),
        legend.title = element_blank(),
        axis.title = element_blank(),
        axis.line = element_line(size = 0.5, colour = "black", linetype=1),
        axis.text = element_text(colour = "black")) -> ag_plot;ag_plot

full_plot = 
  cowplot::ggdraw()+
  cowplot::draw_plot(ag_plot) +
  cowplot::draw_plot(beta_plot, x = 0.99, y = 0.115, width = .4, height = 0.5,
                     hjust = 1, vjust = 0);full_plot
diversityPlots[[5]] <- full_plot
###
# On-marsh nekton
b_df = mob_diversities[[6]] %>%
  dplyr::filter(index == 'beta_S')
beta_ymax = max(b_df$value, na.rm = TRUE)+(0.2*max(b_df$value, na.rm=TRUE))
beta_plot = ggplotGrob(b_df %>%
                         ggplot(aes(x = group, y = value))+
                         geom_boxplot(aes(fill = group))+
                         scale_y_continuous(name = expression(''~beta*"-diversity"),
                                            limits = c(0,beta_ymax), expand = c(0.01,0.01),
                                            breaks = c(0,1,2,4,8,14,22)) +
                         scale_fill_manual(values = color_scheme)+
                         #geom_rangeframe(sides = "lb")+
                         theme(legend.position = 'none',
                               panel.grid = element_blank(),
                               axis.title.x = element_blank(),
                               axis.text.x = element_blank(),
                               panel.background = element_rect(fill = 'transparent', color = NA),
                               plot.background = element_rect(fill = 'white', color = 'black'),
                               axis.line = element_line(size = 0.5, colour = "black", linetype=1),
                               axis.text = element_text(colour = "black"))#element_text(angle = 45, vjust = 1, hjust = 1))
);grid.draw(beta_plot)

ag_df = mob_summaries[[6]]
ag_ymax = max(ag_df$gamma_S_median, na.rm = TRUE)+(0.1*max(ag_df$gamma_S_median, na.rm = TRUE))
if(min(ag_df$gamma_S_median, na.rm = TRUE)-(0.1*min(ag_df$gamma_S_median, na.rm = TRUE)) >= 2){
  ag_ymin = min(ag_df$gamma_S_median, na.rm = TRUE)-(0.1*min(ag_df$gamma_S_median, na.rm = TRUE))
} else{ ag_ymin = 0}
jitter_height = abs(ag_ymax - ag_ymin)*0.01

ag_xmax = max(ag_df$S_quant75, na.rm = TRUE)+(0.2*max(ag_df$S_quant75, na.rm = TRUE))
if(min(ag_df$S_quant25, na.rm = TRUE)-(0.3*min(ag_df$S_quant25, na.rm = TRUE)) >= 2){
  ag_xmin =min(ag_df$S_quant25, na.rm = TRUE)-(0.3*min(ag_df$S_quant25, na.rm = TRUE))
} else{ag_xmin = 0}

ag_df %>%   
  dplyr::mutate(group = factor(group, levels = site_order)) %>%
  ggplot(aes(x = S_median, y = gamma_S_median))+
  #geom_abline(alpha = 0.8, color = 'grey', size = 1.2)+
  geom_errorbarh(aes(color = group, xmin = S_quant25, xmax = S_quant75, linetype = group), height = 0, size = 1.5)+
  geom_point(aes(fill = group, shape = group),size = 2.2)+
  scale_y_continuous(name = expression(""~gamma*"-diversity"),
                     limits = c(ag_ymin,ag_ymax), expand = c(0.01,0.01))+
  scale_x_continuous(name = expression(""~alpha*"-diversity"),
                     limits = c(NA,ag_xmax), expand = c(0.01,0.01))+
  scale_fill_manual(values = color_scheme)+
  scale_color_manual(values = color_scheme)+
  scale_shape_manual(values = shape_scheme)+
  scale_linetype_manual(
    values = c("dotted", "dotted", "solid",
               "solid", "solid", "solid"))+
  guides(color = guide_legend(nrow = 2),
         fill = guide_legend(nrow = 2))+
  #geom_rangeframe(sides = "lb")+
  annotate('text', label = group_names[6], family = 'serif', size = 4,
           x = Inf, y = Inf, hjust = 1, vjust = 1, lineheight = 0.8)+
  theme(legend.position = 'none',
        panel.grid = element_blank(),
        legend.title = element_blank(),
        axis.title = element_blank(),
        axis.line = element_line(size = 0.5, colour = "black", linetype=1),
        axis.text = element_text(colour = "black")) -> ag_plot;ag_plot

full_plot = 
  cowplot::ggdraw()+
  cowplot::draw_plot(ag_plot) +
  cowplot::draw_plot(beta_plot, x = 0.99, y = 0.115, width = .4, height = 0.5,
                     hjust = 1, vjust = 0);full_plot
diversityPlots[[6]] <- full_plot
###
# Off-marsh nekton
b_df = mob_diversities[[8]] %>%
  dplyr::filter(index == 'beta_S')
beta_ymax = max(b_df$value, na.rm = TRUE)+(0.2*max(b_df$value, na.rm=TRUE))
beta_plot = ggplotGrob(b_df %>% dplyr::select(-effort) %>% na.omit %>%
                         ggplot(aes(x = group, y = value))+
                         geom_boxplot(aes(fill = group))+
                         scale_y_continuous(name = expression(''~beta*"-diversity"),
                                            limits = c(0,beta_ymax), expand = c(0.01,0.01),
                                            breaks = c(0,1,2,4,8,14,22)) +
                         scale_fill_manual(values = color_scheme)+
                         #geom_rangeframe(sides = "lb")+
                         theme(legend.position = 'none',
                               panel.grid = element_blank(),
                               axis.title.x = element_blank(),
                               axis.text.x = element_blank(),
                               panel.background = element_rect(fill = 'transparent', color = NA),
                               plot.background = element_rect(fill = 'white', color = 'black'),
                               axis.line = element_line(size = 0.5, colour = "black", linetype=1),
                               axis.text = element_text(colour = "black"))#element_text(angle = 45, vjust = 1, hjust = 1))
);grid.draw(beta_plot)

ag_df = mob_summaries[[8]]
ag_ymax = max(ag_df$gamma_S_median, na.rm = TRUE)+(0.1*max(ag_df$gamma_S_median, na.rm = TRUE))
if(min(ag_df$gamma_S_median, na.rm = TRUE)-(0.1*min(ag_df$gamma_S_median, na.rm = TRUE)) >= 2){
  ag_ymin = min(ag_df$gamma_S_median, na.rm = TRUE)-(0.1*min(ag_df$gamma_S_median, na.rm = TRUE))
} else{ ag_ymin = 0}
jitter_height = abs(ag_ymax - ag_ymin)*0.01

ag_xmax = max(ag_df$S_quant75, na.rm = TRUE)+(0.13*max(ag_df$S_quant75, na.rm = TRUE))
if(min(ag_df$S_quant25, na.rm = TRUE)-(0.3*min(ag_df$S_quant25, na.rm = TRUE)) >= 2){
  ag_xmin =min(ag_df$S_quant25, na.rm = TRUE)-(0.3*min(ag_df$S_quant25, na.rm = TRUE))
} else{ag_xmin = 0}

ag_df %>%   
  dplyr::mutate(group = factor(group, levels = site_order)) %>%
  ggplot(aes(x = S_median, y = gamma_S_median))+
  #geom_abline(alpha = 0.8, color = 'grey', size = 1.2)+
  geom_errorbarh(aes(color = group, xmin = S_quant25, xmax = S_quant75, linetype = group), height = 0, size = 1.5)+
  geom_point(aes(fill = group, shape = group),size = 2.2)+
  scale_y_continuous(name = expression(""~gamma*"-diversity"),
                     limits = c(ag_ymin,ag_ymax), expand = c(0.01,0.01))+
  scale_x_continuous(name = expression(""~alpha*"-diversity"),
                     limits = c(NA,5), expand = c(0.01,0.01))+#ag_xmax
  scale_fill_manual(values = color_scheme)+
  scale_color_manual(values = color_scheme)+
  scale_shape_manual(values = shape_scheme)+
  scale_linetype_manual(
    values = c("dotted", "dotted", "solid",
               "solid", "solid", "solid"))+
  guides(color = guide_legend(nrow = 2),
         fill = guide_legend(nrow = 2))+
  #geom_rangeframe(sides = "lb")+
  annotate('text', label = group_names[8], family = 'serif', size = 4,
           x = Inf, y = Inf, hjust = 1, vjust = 1, lineheight = 0.8)+
  theme(legend.position = 'none',
        panel.grid = element_blank(),
        legend.title = element_blank(),
        axis.title = element_blank(),
        axis.line = element_line(size = 0.5, colour = "black", linetype=1),
        axis.text = element_text(colour = "black")) -> ag_plot;ag_plot

full_plot = 
  cowplot::ggdraw()+
  cowplot::draw_plot(ag_plot) +
  cowplot::draw_plot(beta_plot, x = 0.99, y = 0.115, width = .4, height = 0.5,
                     hjust = 1, vjust = 0);full_plot
diversityPlots[[8]] <- full_plot
###

plot_legend = 
  cowplot::get_legend(mob_summaries[[1]] %>%
                        # pluck(x) %>%
                        dplyr::mutate(group =factor(group, levels = site_order)) %>%
                        ggplot(aes(x = S_median, y = gamma_S_median))+
                        geom_point(aes(fill = group, shape = group),size = 3.3)+
                        scale_fill_manual(values = color_scheme)+
                        scale_color_manual(values = color_scheme)+
                        scale_shape_manual(values = shape_scheme)+
                        guides(color = guide_legend(nrow = 2),
                               fill = guide_legend(nrow = 2))+
                        scale_linetype_manual(
                          values = c("dotted", "dotted", "solid",
                                     "solid", "solid", "solid"))+
                        theme(legend.title = element_blank(),
                              legend.text = element_text(family = 'serif', size = 14),
                              legend.margin= margin(c(t = 1,r = 5,b = 1,l = 5))))

plot_legend$grobs <- lapply(plot_legend$grobs, function(z) modifyList(z, list(x = unit(0.5, 'npc'),
                                                                              y = unit(1, 'npc'),
                                                                              vjust = 0)))



png(here::here("figures/sPIE_diversities.png"), width = 8, height = 10, units = "in", res = 480 )
full_plot = gridExtra::grid.arrange(diversityPlots[[1]], diversityPlots[[2]],
                                    diversityPlots[[3]], diversityPlots[[4]],
                                    diversityPlots[[5]], diversityPlots[[6]],
                                    diversityPlots[[8]],
                                    plot_legend,
                                    ncol = 2,
                                    left = textGrob(label = expression(""~gamma*"-diversity ("~italic(S[PIE])~")"),
                                                    gp = gpar(fontfamily = 'serif'), rot = 90),
                                    bottom = textGrob(label = expression(""~alpha*"-diversity ("~italic(S[PIE])~")"),
                                                      gp = gpar(fontfamily = 'serif')))
dev.off()

## Spare(d) code ####
#### Diversities (Gamma, Alpha, Beta)
# name working lists
# mob_stats = setNames(mob_stats, nm = taxa_names)
# mob_stats = rlist::list.subset(mob_stats, taxa_order)
# 
# mob_alphabeta = mob_stats %>%
#   purrr::map(~.x %>% 
#                pluck('samples_stats') %>% 
#                data.frame %>% 
#                dplyr::filter(index %in% c('S','beta_S')) %>%
#                dplyr::mutate(across(where(is.factor), droplevels))%>%
#                dplyr::mutate(group =factor(group, levels = site_order))) #%>%
# # bind_rows(.id = 'taxa_group')
# 
# mob_gamma = mob_stats %>%
#   purrr::map(~.x %>%
#                pluck('groups_stats') %>%
#                data.frame %>%
#                dplyr::filter(index == 'S') %>% #& effort == min(effort, na.rm = TRUE)) %>%
#                dplyr::mutate(index = recode_factor(index, S = 'gamma_S')) %>%
#                dplyr::mutate(across(where(is.factor), droplevels)) %>%
#                dplyr::mutate(group =factor(group, levels = site_order)))
# 
# mob_diversities = purrr::map2(mob_alphabeta, mob_gamma, ~bind_rows(.x,.y))
# 
# mob_summaries = mob_diversities%>%
#   purrr::map(~.x %>%
#                group_by(group, index) %>%
#                dplyr::summarise(across(value, list(mean = ~mean(.x, na.rm = TRUE),
#                                                    median = ~median(.x, na.rm = TRUE),
#                                                    quant2.5 = ~quantile(.x, 0.025, na.rm = TRUE),
#                                                    quant25 = ~quantile(.x, 0.25, na.rm = TRUE),
#                                                    quant75 = ~quantile(.x, 0.75, na.rm = TRUE),
#                                                    quant97.5 = ~quantile(.x, 0.975, na.rm = TRUE),
#                                                    min = ~min(.x, na.rm= TRUE),
#                                                    max = ~max(.x, na.rm = TRUE)),
#                                        .names = "{.fn}")) %>%
#                pivot_longer(-group:-index, names_to = 'stat', values_to = 'value') %>%
#                tidyr::unite("stat", index,stat) %>%
#                pivot_wider(names_from = stat, values_from = value) %>%
#                dplyr::mutate(group =factor(group, levels = site_order)))
# 
# # View(mob_summaries[[4]])
# 
# diversities_plot = function(x = NULL, plot_titles = FALSE, plot_legend = FALSE, plot_annotation = NULL,
#                             inset_ymax = NULL, ...){
#   b_df = mob_diversities %>%
#     pluck(x) %>%
#     dplyr::filter(index == 'beta_S')
#   beta_ymax = max(b_df$value, na.rm = TRUE)+(0.1*max(b_df$value, na.rm=TRUE))
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
#   ag_ymax = max(ag_df$gamma_S_median, na.rm = TRUE)+(0.1*max(ag_df$gamma_S_median, na.rm = TRUE))
#   if(min(ag_df$gamma_S_median, na.rm = TRUE)-(0.2*min(ag_df$gamma_S_median, na.rm = TRUE)) >= 2){
#     ag_ymin = min(ag_df$gamma_S_median, na.rm = TRUE)-(0.2*min(ag_df$gamma_S_median, na.rm = TRUE))
#   } else{ ag_ymin = 0}
#   jitter_height = abs(ag_ymax - ag_ymin)*0.01
#   
#   
#   
#   ag_xmax = max(ag_df$S_quant75, na.rm = TRUE)+(0.2*max(ag_df$S_quant75, na.rm = TRUE))
#   if(min(ag_df$S_quant25, na.rm = TRUE)-(0.2*min(ag_df$S_quant25, na.rm = TRUE)) >= 2){
#     ag_xmin =min(ag_df$S_quant25, na.rm = TRUE)-(0.2*min(ag_df$S_quant25, na.rm = TRUE))
#   } else{ag_xmin = 0}
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
# # debugonce(diversities_plot)
# 
# group_names = c( "Surface\nMicrobes", "Below-Surface\nMicrobes","Plants", "Macroinfauna", "Spiders", 
#                  "On-marsh\nnekton", "Full minnow","Off-marsh\nnekton")
# 
# legend_pos = c(F, F, F, F, F, F, F, F)
# ymax = c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5,0.5, 0.5)
# 
# 
# diversityPlots = purrr::pmap(list(names(mob_summaries), legend_pos, group_names, ymax),
#                              ~diversities_plot(..1, plot_titles = FALSE, plot_legend = ..2,
#                                                plot_annotation = ..3, inset_ymax = ..4))
# 
# # dev.new();grid.draw(diversityPlots[[1]])
# 
# plot_legend = 
#   cowplot::get_legend(mob_summaries[[1]] %>%
#                         # pluck(x) %>%
#                         dplyr::mutate(group =factor(group, levels = site_order)) %>%
#                         ggplot(aes(x = S_median, y = gamma_S_median))+
#                         geom_point(aes(fill = group, shape = group),size = 2.2)+
#                         scale_fill_manual(values = color_scheme)+
#                         scale_color_manual(values = color_scheme)+
#                         scale_shape_manual(values = shape_scheme)+
#                         guides(color = guide_legend(nrow = 2),
#                                fill = guide_legend(nrow = 2))+
#                         scale_linetype_manual(
#                           values = c("dotted", "dotted", "solid",
#                                      "solid", "solid", "solid"))+
#                         theme(legend.title = element_blank(),
#                               legend.text = element_text(family = 'serif'),
#                               legend.margin= margin(c(t = 1,r = 5,b = 1,l = 5))))
# 
# plot_legend$grobs <- lapply(plot_legend$grobs, function(z) modifyList(z, list(x = unit(0.5, 'npc'),
#                                                                               y = unit(1, 'npc'),
#                                                                               vjust = 0)))
# 
# 
# # removed full minnow diversityPlots[[5]]
# png("./sub-projects/restored-marsh-diversity/figures/s_diversities.png", width = 8, height = 10, units = "in", res = 480 )
# full_plot = gridExtra::grid.arrange(diversityPlots[[1]], diversityPlots[[2]],
#                                     diversityPlots[[3]], diversityPlots[[4]],
#                                     diversityPlots[[5]], diversityPlots[[6]],
#                                     diversityPlots[[8]],
#                                     plot_legend,
#                                     ncol = 2,
#                                     left = textGrob(label = expression(""~gamma*"-diversity"),
#                                                     gp = gpar(fontfamily = 'serif'), rot = 90),
#                                     bottom = textGrob(label = expression(""~alpha*"-diversity"),
#                                                       gp = gpar(fontfamily = 'serif')))
# dev.off()
# 
# ## S_PIE
# 
# mob_alphabeta = mob_stats %>%
#   purrr::map(~.x %>% 
#                pluck('samples_stats') %>% 
#                data.frame %>% 
#                dplyr::filter(index %in% c('S_PIE','beta_S_PIE')) %>%
#                dplyr::mutate(index = recode_factor(index, !!!c(S_PIE = 'S', beta_S_PIE ='beta_S'))) %>%
#                dplyr::mutate(across(where(is.factor), droplevels))%>%
#                dplyr::mutate(group =factor(group, levels = site_order))) #%>%
# # bind_rows(.id = 'taxa_group')
# 
# mob_gamma = mob_stats %>%
#   purrr::map(~.x %>%
#                pluck('groups_stats') %>%
#                data.frame %>%
#                dplyr::filter(index == 'S_PIE') %>% #& effort == min(effort, na.rm = TRUE)) %>%
#                dplyr::mutate(index = recode_factor(index, S_PIE = 'gamma_S')) %>%
#                dplyr::mutate(across(where(is.factor), droplevels)) %>%
#                dplyr::mutate(group =factor(group, levels = site_order)))
# 
# mob_diversities = purrr::map2(mob_alphabeta, mob_gamma, ~bind_rows(.x,.y))
# 
# mob_summaries = mob_diversities%>%
#   purrr::map(~.x %>%
#                group_by(group, index) %>%
#                dplyr::summarise(across(value, list(mean = ~mean(.x, na.rm = TRUE),
#                                                    median = ~median(.x, na.rm = TRUE),
#                                                    quant2.5 = ~quantile(.x, 0.025, na.rm = TRUE),
#                                                    quant25 = ~quantile(.x, 0.25, na.rm = TRUE),
#                                                    quant75 = ~quantile(.x, 0.75, na.rm = TRUE),
#                                                    quant97.5 = ~quantile(.x, 0.975, na.rm = TRUE),
#                                                    min = ~min(.x, na.rm= TRUE),
#                                                    max = ~max(.x, na.rm = TRUE)),
#                                        .names = "{.fn}")) %>%
#                pivot_longer(-group:-index, names_to = 'stat', values_to = 'value') %>%
#                tidyr::unite("stat", index,stat) %>%
#                pivot_wider(names_from = stat, values_from = value) %>%
#                dplyr::mutate(group =factor(group, levels = site_order)))
# 
# 
# group_names = c( "Surface\nMicrobes", "Below-Surface\nMicrobes","Plants", "Macroinfauna", "Spiders", 
#                  "On-marsh\nnekton", "Full minnow","Off-marsh\nnekton")
# # group_names = c("Plants", "Microbes (0-2 cm)", "Microbes (8-10 cm)","On-Marsh minnow", "Full minnow","Trawl")
# legend_pos = c(F, F, F, F, F, F, F, F)
# ymax = c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5,0.5, 0.5)
# 
# # debugonce(diversities_plot)
# 
# diversityPlots = purrr::pmap(list(names(mob_summaries), legend_pos, group_names, ymax),
#                              ~diversities_plot(..1, plot_titles = FALSE, plot_legend = ..2,
#                                                plot_annotation = ..3, inset_ymax = ..4))
# plot_legend = 
#   cowplot::get_legend(mob_summaries[[1]] %>%
#                         # pluck(x) %>%
#                         dplyr::mutate(group =factor(group, levels = site_order)) %>%
#                         ggplot(aes(x = S_median, y = gamma_S_median))+
#                         geom_point(aes(fill = group, shape = group),size = 2.2)+
#                         scale_fill_manual(values = color_scheme)+
#                         scale_color_manual(values = color_scheme)+
#                         scale_shape_manual(values = shape_scheme)+
#                         guides(color = guide_legend(nrow = 2),
#                                fill = guide_legend(nrow = 2))+
#                         scale_linetype_manual(
#                           values = c("dotted", "dotted", "solid",
#                                      "solid", "solid", "solid"))+
#                         theme(legend.title = element_blank(),
#                               legend.text = element_text(family = 'serif'),
#                               legend.margin= margin(c(t = 1,r = 5,b = 1,l = 5))))
# 
# plot_legend$grobs <- lapply(plot_legend$grobs, function(z) modifyList(z, list(x = unit(0.5, 'npc'),
#                                                                               y = unit(1, 'npc'),
#                                                                               vjust = 0)))
# 
# 
# # removed full minnow diversityPlots[[5]]
# png("./sub-projects/restored-marsh-diversity/figures/sPIE_diversities.png", width = 8, height = 10, units = "in", res = 480 )
# full_plot = gridExtra::grid.arrange(diversityPlots[[1]], diversityPlots[[2]],
#                                     diversityPlots[[3]], diversityPlots[[4]],
#                                     diversityPlots[[5]], diversityPlots[[6]],
#                                     diversityPlots[[8]],
#                                     plot_legend,
#                                     ncol = 2,
#                                     left = textGrob(label = expression(""~gamma*"-diversity ("~italic(S[PIE])~")"),
#                                                     gp = gpar(fontfamily = 'serif'), rot = 90),
#                                     bottom = textGrob(label = expression(""~alpha*"-diversity ("~italic(S[PIE])~")"),
#                                                       gp = gpar(fontfamily = 'serif')))
# dev.off()


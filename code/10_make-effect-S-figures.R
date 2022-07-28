here::i_am('code/10_make-effect-S-figures.R')
require (cowplot)
require (ggplot2)

load(here::here('analyses_output/output_analyses.RData'))

tiff(file=here::here("figures/effect_S.tiff"),width=5000,
     height=5000,units = "px",
     compression="lzw",res=300)
plot_grid (plot_effect_grad[[2]],
           plot_effect_grad[[3]],
           plot_effect_grad[[1]],
           plot_effect_grad[[7]],
           plot_effect_grad[[4]],
           plot_effect_grad[[6]],
           nrow=3, ncol=2,
           label_size = 16)
dev.off()

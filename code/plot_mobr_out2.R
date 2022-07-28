plot_mobr_out2 <- function (x, stat = "b1", log2 = "", scale_by = NULL, 
          display = c("S ~ effort", "effect ~ grad", "stat ~ effort"), 
          eff_sub_effort = TRUE, eff_log_base = 2, eff_disp_pts = TRUE, 
          eff_disp_smooth = FALSE, main_title, x_label, ...) 
{
  process_names <- list(
    'agg'="Aggregation",
    'N'="Density",
    'SAD'="SAD"
  )
  process_labeller <- function(variable,value){
    return(process_names[value])
  }
  
  
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  if (x$type == "discrete") {
    if (stat != "b1") 
      warning("The only statistic that has a reasonable interpretation for a discrete explanatory variable is the difference in the group means from the reference group (i.e., set stat = \"b1\")")
  }
  if (!is.null(scale_by)) {
    if (scale_by == "indiv") {
      x$S_df = mutate(x$S_df, effort = ifelse(sample == 
                                                "plot", round(.data$effort * x$density_stat$ind_dens), 
                                              .data$effort))
      x$mod_df = mutate(x$mod_df, effort = ifelse(sample == 
                                                    "plot", round(.data$effort * x$density_stat$ind_dens), 
                                                  .data$effort))
    }
    if (scale_by == "plot") {
      x$S_df = mutate(x$S_df, effort = ifelse(sample == 
                                                "indiv", round(.data$effort/x$density_stat$ind_dens), 
                                              .data$effort))
      x$mod_df = mutate(x$mod_df, effort = ifelse(sample == 
                                                    "indiv", round(.data$effort/x$density_stat$ind_dens), 
                                                  .data$effort))
    }
  }
  p_list = vector("list", 4)
  if ("S ~ grad" %in% display) {
    facet_labs = c(agg = "sSBR", N = "nsSBR", 
                   SAD = "IBR")
    p_list[[1]] = ggplot(x$S_df, aes(.data$env, .data$S)) + 
      geom_smooth(aes(group = .data$effort, color = .data$effort), 
                  method = "lm", se = FALSE) + labs(x = x$env_var) + 
      facet_wrap(. ~ test, scales = "free", labeller = as_labeller(facet_labs))
  }
  if ("S ~ effort" %in% display) {
    facet_labs = c(agg = "sSBR", N = "nsSBR", 
                   SAD = "IBR")
    p_list[[2]] = ggplot(x$S_df, aes(.data$effort, .data$S)) + 
      geom_line(aes(group = .data$group, color = .data$env)) + 
      facet_wrap(. ~ test, scales = "free", labeller = as_labeller(facet_labs)) + 
      labs(y = expression("richness (" * italic(S) * 
                            ")"), color = x$env_var)
  }
  if ("effect ~ grad" %in% display) {
    efforts = sort(unique(x$S_df$effort))
    if (is.logical(eff_sub_effort)) {
      if (eff_sub_effort) {
        effort_r = floor(log(range(efforts), eff_log_base))
        effort_2 = eff_log_base^(effort_r[1]:effort_r[2])
        effort_2 = effort_2[effort_2 > 1]
        eff_d = as.matrix(stats::dist(c(efforts, effort_2)))
        eff_d = eff_d[-((length(efforts) + 1):ncol(eff_d)), 
                      -(1:length(efforts))]
        min_index = apply(eff_d, 2, function(x) which(x == 
                                                        min(x))[1])
        sub_effort = efforts[min_index]
        message(paste("Effect size shown at the following efforts:", 
                      paste(sub_effort, collapse = ", ")))
      }
      else sub_effort = efforts
    }
    else if (!is.null(eff_sub_effort)) 
      sub_effort = eff_sub_effort
    if (x$type == "continuous") 
      x$S_df = x$S_df %>% group_by(.data$test, .data$effort) %>% 
        mutate(low_effect = predict(loess(low_effect ~ 
                                            .data$env), .data$env)) %>% mutate(high_effect = predict(loess(high_effect ~ 
                                                                                                             .data$env), .data$env))
    p_list[[3]] = ggplot(subset(x$S_df, x$S_df$effort %in% 
                                  sub_effort), aes(.data$env, .data$effect)) + geom_hline(yintercept = 0, 
                                                                                          linetype = "dashed") + labs(x = x$env_var) + 
      facet_wrap(. ~ test, scales = "free_y",
                 labeller=process_labeller) + labs(y = expression("effect (" * 
                                                                      italic(S) * ")"),
                                                   x = x_label,
                                                   colour = "Effort") + scale_fill_manual(name = element_blank(), 
                                                                                                            values = c(null = "grey40")) + scale_colour_gradient2(trans = scales::log2_trans(), 
                                                                                                                                                                  low = rgb(248, 203, 173, maxColorValue = 255), mid = rgb(237, 
                                                                                                                                                                                                                           127, 52, maxColorValue = 255), high = rgb(165, 
                                                                                                                                                                                                                                                                     0, 33, maxColorValue = 255), midpoint = 4)+
      theme (axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=11),
             axis.text.y = element_text(size=11),
             axis.title = element_text(size=14),
             strip.text.x = element_text(size = 12),
             legend.title = element_text(size=13),
             legend.text = element_text(size=10),
             panel.background = element_rect(fill = "white",
                                             colour = "black",
                                             size = 0.5, linetype = "solid"),
             panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                             colour = "gray90"), 
             panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                             colour = "gray90"))+
      ggtitle(main_title)
    
    if (eff_disp_pts) 
      p_list[[3]] = p_list[[3]] + geom_point(aes(group = .data$effort, 
                                                 color = .data$effort),
                                             size=2)
    if (eff_disp_smooth) 
      p_list[[3]] = p_list[[3]] + geom_smooth(aes(group = .data$effort, 
                                                  color = .data$effort), method = lm, se = FALSE)
  }
  if ("stat ~ effort" %in% display) {
    if (stat == "b0") 
      ylab = expression("intercept (" * italic(beta)[0] * 
                          ")")
    if (stat == "b1") 
      ylab = expression("slope (" * italic(beta)[1] * 
                          ")")
    if (stat == "r2") 
      ylab = expression(italic(R^2))
    if (stat == "r") 
      ylab = expression(italic(r))
    if (stat == "f") 
      ylab = expression(italic(F))
    p_list[[4]] = ggplot(subset(x$mod_df, x$mod_df$index == 
                                  stat), aes(.data$effort, .data$value)) + geom_ribbon(aes(ymin = .data$low_value, 
                                                                                           ymax = .data$high_value, fill = "null"), alpha = 0.25) + 
      geom_line(aes(group = .data$index, color = "observed")) + 
      geom_hline(yintercept = 0, linetype = "dashed") + 
      facet_wrap(. ~ test, scales = "free_x") + labs(y = ylab) + 
      scale_color_manual(name = element_blank(), values = c(observed = "red")) + 
      scale_fill_manual(name = element_blank(), values = c(null = "grey40"))
  }
  if (!is.null(scale_by)) {
    scale_by = ifelse(scale_by == "indiv", "# of individuals", 
                      "# of plots")
    if (!is.null(p_list[[1]])) 
      p_list[[1]] = p_list[[1]] + labs(color = scale_by)
    if (!is.null(p_list[[3]])) 
      p_list[[3]] = p_list[[3]] + labs(color = scale_by)
    if (!is.null(p_list[[2]])) 
      p_list[[2]] = p_list[[2]] + labs(x = scale_by)
    if (!is.null(p_list[[4]])) 
      p_list[[4]] = p_list[[4]] + labs(x = scale_by)
  }
  if (grepl("x", log2)) {
    if (!is.null(p_list[[2]])) 
      p_list[[2]] = p_list[[2]] + scale_x_continuous(trans = "log2")
    if (!is.null(p_list[[4]])) 
      p_list[[4]] = p_list[[4]] + scale_x_continuous(trans = "log2")
  }
  if (grepl("y", log2)) {
    if (!is.null(p_list[[2]])) 
      p_list[[2]] = p_list[[2]] + scale_y_continuous(trans = "log2")
  }
  p_list = Filter(Negate(is.null), p_list)
  egg::ggarrange(plots = p_list)
}

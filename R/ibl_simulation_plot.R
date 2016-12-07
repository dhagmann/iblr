#' Plots the proportion of choices for each option.
#'
#' \code{ibl_simulation_plot} takes as input the output from
#' \code{ibl_simulation}. It then plots the proportion of choices for each
#' option over time.
#'
#' @param simulation The output from \code{ibl_simulation}, i.e. a matrix in
#' which each row corresponds to a trial, each column to a simulation or
#' participant, and the cell (x,y) to the option chosen in trial x by
#' participant y.
#'
#' @return Outputs a ggplot object.

ibl_simulation_plot <- function(simulation, names = NA) {
  props <- cbind(1:nrow(simulation), sapply(1:max(simulation),
                                            function(i) rowMeans(simulation == i))) %>%
    data.frame() %>%
    rename(trial = X1) %>%
    gather(option, proportion, -trial)

  ggplot(props, aes(x = trial, y = proportion, color = option)) +
    geom_line() +
    geom_hline(yintercept = 1/max(simulation), alpha=0.25, lty=2) +
    theme_bw() +
    xlab("Trial") +
    ylab("Proportion of Choices") +
    ylim(0,1) +
    theme(panel.grid = element_blank()) +
    scale_color_discrete(name = "Choices",
                         breaks = unique(props$option),
                         labels = ifelse(is.na(names), sapply(1:max(simulation),
                                         function(i) paste("Option", i)),
                                           names))
}

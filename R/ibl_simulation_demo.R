#Options: outcomes and probabilities
ops1 <- c(250, 1)  
ops2 <- c(500,0,100, 0.7,0.2,0.1)

## Collect all gambles in a list
gg <- grep("^ops", ls(), value=TRUE)
gambles <- lapply(1:length(gg), function(g)  matrix(get(gg[g]),ncol=2))

iterations <- 1000
prevalues <- 550
ntrials <- 50
noise <- 0.25
decay <- 0.50
foregone <- F

mmm <- ibl_simulation(gambles,ntrials,prevalues)

ibl_simulation_plot(mmm, noptions = 2, plot_option = 2)

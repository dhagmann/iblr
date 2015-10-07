ibl_simulation <- function(gambles,
                           ntrials,
                           prevalues,
                           noise = 0.25,
                           decay = 0.5,
                           foregone = F,
                           cores = 1,
                           iterations = 1000) {
    library(parallel)
    
    noptions <- length(gambles)
    simulation_results <- mclapply(1:iterations, mc.cores=cores, FUN=function(x) ibl_new(noptions,prevalues,ntrials,noise,decay,gambles,foregone))
    simulation_results <- do.call(cbind,simulation_results)
}
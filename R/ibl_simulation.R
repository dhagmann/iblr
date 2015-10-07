#' Simulates the IBL Model
#' @param gambles A list of gambles containing payoffs in column 1 and associated probabilities in column 2
#' @param ntrials The number of trials in the experiment
#' @param prevalues The number of simulated draws before the start of the experiment
#' @param noise The noise parameter of IBL
#' @param decay The decoy parameter of IBL
#' @param foregone Logical. Whether foregone outcomes are observed by the decision maker
#' @param parallel Logical. Should the model run using parallel processing?
#' @param cores Number of cores. If left blank, uses half of all available cores (fastest)
#' @param iterations The number of iterations for the simulation
#' 
#' @return A matrix with the number of rows equal to \code{ntrials} and the number of columns equal to \code{iterations}.
#' 
#' @examples Simple example needed. (After fixing how gambles are handled?)

ibl_simulation <- function(gambles,
                           ntrials,
                           prevalues,
                           noise = 0.25,
                           decay = 0.5,
                           foregone = F,
                           parallel = T,
                           cores = NULL,
                           iterations = 1000) {
    
    noptions <- length(gambles)
    
    if(parallel) {
        # Set the number of cores to be used
        if(is.null(cores)) {
            cl <- makeCluster(detectCores()/2,  # There may be a better way to do this?
                              methods = F)      # If something breaks, remove this line. 
                                                # Code runs faster with it.
        } else {
            cl <- makeCluster(cores,
                              methods = F)      # If something breaks, remove this line.
        }
        
        registerDoParallel(cl)
        simulation_results <- foreach(i=1:iterations,
                                      .combine = cbind,
                                      .export = "ibl_new") %dopar%  # .export may not be needed if function is in package
            ibl_new(noptions,prevalues,ntrials,noise,decay,gambles,foregone)
        stopCluster(cl)
    } else {
        simulation_results <- foreach(i=1:iterations,
                                      .combine = cbind,
                                      .export = "ibl_new") %do%  # .export may not be needed if function is in package
            ibl_new(noptions,prevalues,ntrials,noise,decay,gambles,foregone)
    }
    
    return(simulation_results)
}
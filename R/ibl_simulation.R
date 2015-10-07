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
        library(doParallel) # Move this into recommended DEPENDENCIES once we create a package
        
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
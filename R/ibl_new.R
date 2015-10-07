ibl_new <- function(noptions, prevalues, ntrials, noise, decay, gambles, foregone){ #arguments of the function
    
    tau <- noise * sqrt(2) #definition of tau 
    Decision <- matrix(NA,nrow=ntrials,ncol=1) #Initiate final decision matrix
    options <- lapply(1:noptions, function(k){ #List to store instances/trial number/outcomes
        x <- list(0)
        names(x) <- as.character(prevalues)
        x
    })
    
    for(t in 1:ntrials){ #Start loop
        #Basic ibl loop
        blend <- sapply(options, function(p){
            activation <- lapply(p, function(x){
                rndc <- runif(1)
                nois <- noise * log((1-rndc)/rndc)
                y <- nois + (log(sum((t - x)^-decay)))
            })
            probl <- exp(unlist(activation) / tau) / sum(exp(unlist(activation) / tau))
            blendValue <- sum(as.numeric(names(probl)) * probl)
        })
        
        #Ibl decision
        ibl_first <- which(blend==max(blend))
        if(length(ibl_first)>1) ibl_choice <- sample(ibl_first,1) else ibl_choice <- ibl_first
        
        #store decision
        Decision[t] <- ibl_choice
        
        # sample payoffs and store trial number
        if(foregone){
            for(op in 1:noptions){
                payoff <- gambles[[op]][,1][sample(length(gambles[[op]][,1]), size=1, prob=gambles[[op]][,2])]
                options[[op]][[as.character(payoff)]] <- c(options[[op]][[as.character(payoff)]],t)
            }
        } else{
            payoff <- gambles[[ibl_choice]][,1][sample(length(gambles[[ibl_choice]][,1]), size=1, prob=gambles[[ibl_choice]][,2])]
            options[[ibl_choice]][[as.character(payoff)]] <- c(options[[ibl_choice]][[as.character(payoff)]],t)
        }
        
    }
    return(Decision)
}
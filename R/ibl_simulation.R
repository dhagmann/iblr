library(parallel) # loads package parallel: if not already installed, install.packages("package.name")

#Options: outcomes and probabilities
ops1 <- c(250, 1)  
ops2 <- c(500,0,100, 0.7,0.2,0.1)

## Collect all gambles in a list
gg <- grep("^ops", ls(), value=TRUE)
gambles <- lapply(1:length(gg), function(g)  matrix(get(gg[g]),ncol=2))
noptions <- length(gambles) ## Number of options/decks in your simulation

## Parameters for Simulation
iterations <- 1000
prevalues <- 550
ntrials <- 50
noise <- 0.25
decay <- 0.50
foregone <- TRUE

# IBL Function ------------------------------------------------------------

IBL_new <- function(noptions, prevalues, ntrials, noise, decay, gambles, foregone){ #arguments of the function
  
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

#If you have linux/mac you can parallelize the simulation and use the function mclapply
#otherwise use the lapply function


mmm <- mclapply(1:iterations, mc.cores=4, FUN=function(x) IBL_new(noptions,prevalues,ntrials,noise,decay,gambles,foregone))
#mmm <- lapply(1:iterations, FUN=function(x) IBL_new(noptions,prevalues,ntrials,noise,decay,gambles))
mmm2 <- do.call(cbind,mmm)
props <- t(apply(mmm2,1, function(x) prop.table(tabulate(x,noptions))))
plot(props[,2],type="l",ylim=c(0,1),pch=16,col="blue")
abline(h=0.5)


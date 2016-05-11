#' Iterates through the IBL Model
#'
#' \code{ibl_new} is called by \code{ibl_simulation} and simulates one
#' participant going through the experiment.
#'
#' @param noptions The number of options available.
#' @inheritParams ibl_simulation
#'
#' @return A column vector representing the participant's choice in each trial.
#'   The length of the vector is equal to the number of trials.

ibl_new <- function(noptions, prevalues, ntrials, noise, decay, gambles, foregone){

    tau <- noise * sqrt(2) #definition of tau
    Decision <- matrix(NA,nrow=ntrials,ncol=1) #Initiate final decision matrix
    options <- lapply(1:noptions, function(k){ #List to store instances/trial number/outcomes
        x <- list(0)
        names(x) <- as.character(prevalues)
        return(x)
    })

    for(t in 1:ntrials){ #Start loop

        Decision[t] <- ibl_loop(options, tau, noise, decay, t, x) %>%  # Basic ibl loop
          ibl_choice()  # Store decision

        # Sample payoffs and store trial number
        if(foregone){
            for(op in 1:noptions){
                payoff <- choice_outcome(gambles, op)
                options[[op]][[as.character(payoff)]] <- c(options[[op]][[as.character(payoff)]],t)
            }
        } else{
            payoff <- choice_outcome(gambles, Decision[t])
            options[[Decision[t]]][[as.character(payoff)]] <- c(options[[Decision[t]]][[as.character(payoff)]],t)
        }

    }
    return(Decision)
}

ibl_loop <- function(options, tau, noise, decay, t, x) {
  #Basic ibl loop
  sapply(options, function(p){
    activation <- lapply(p, function(x){
      rndc <- runif(1)
      nois <- noise * log((1-rndc)/rndc)
      y <- nois + (log(sum((t - x)^-decay)))
    })
    probl <- exp(unlist(activation) / tau) / sum(exp(unlist(activation) / tau))
    blendValue <- sum(as.numeric(names(probl)) * probl)
  })
}

ibl_choice <- function(blend) {
  #Ibl decision
  blend %>%
    rank(ties.method="random") %>%
    which.max()
}

choice_outcome <- function(gambles, op) {
  payoffs <- gambles[[op]][,1]
  probabilities <- gambles[[op]][,2]
  choice <- sample(length(payoffs), size = 1, prob = probabilities)
  outcome <- gambles[[op]][,1][choice]
  return(outcome)
}

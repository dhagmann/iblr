# iblr
This R library implements the Instance Based Learning Theory model proposed by Gonzalez, Lerch, and Lebiere (2003).

It contains two primary functions:  
* **ibl_simulation** simulates data based on parameters about  
* **ibl_fitting** estimates the best-fitting model parameters for experimental data.

## Installation
Install the latest version with:

```R
# install.packages("devtools")
# get a token from https://github.com/settings/tokens
devtools::install_github("dhagmann/iblr", auth_token = "abc")
```

## Getting Started
To run a simulation, first define the model parameters, then run the simulation function.
```R
# Options: outcomes and probabilities
ops1 <- c(250, 
            1)  
ops2 <- c(500,   0, 100, 
          0.7, 0.2,  0.1)

# Collect all gambles in a list
gg <- grep("^ops", ls(), value=TRUE)

gambles <- lapply(1:length(gg), function(g)  matrix(get(gg[g]),ncol=2))
ntrials <- 50
prevalues <- 550

# Run simulation
sim <- ibl_simulation(gambles,ntrials,prevalues)

# Plot proportion of times Option 2 is chosen
ibl_simulation_plot(sim, noptions = length(gambles), plot_option = 2)
```

You can recover the initial parameters (with some noise) using the fitting function.
```R

```

## References

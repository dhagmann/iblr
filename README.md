# iblr
This R library implements the Instance-Based Learning model for experience-based decision-making (see Gonzalez & Dutt, 2011; Lejarraga, Dutt, & Gonzalez, 2012)

It contains two primary functions:  
* **ibl_simulation** simulates data based on parameters about  
* **ibl_fitting** estimates the best-fitting model parameters for experimental data.

## Installation
Install the latest version with:

```R
# install.packages("devtools")
devtools::install_github("dhagmann/iblr")
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

# Plot proportion of times each of the two options is chosen
ibl_simulation_plot(sim)
```

You can recover the initial parameters (with some noise) using the fitting function.
```R

```

## References

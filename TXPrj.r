# Define a new library directory
new_lib_dir <- "C:/Users/bkf510/OneDrive - University of Texas at San Antonio/Desktop/R/library"

# Set the new library directory as the default
.libPaths(c(new_lib_dir, .libPaths()))

install.packages("car")
library(car)
library(haven)
library(dplyr)
library(stringr)
library(tidyverse)
library(ggplot2)
library(glue)
library(tibble)
library(ggpubr)
library(cowplot)
library(writexl)
library(tidyr)
#demographic library
library(bayesTFR)
library(bayesLife)
library(bayesPop)




#procedure
setwd("C:/Users/bkf510/OneDrive - University of Texas at San Antonio/Desktop/R/Population projection/Country")
getwd()
#TFR
sim.dir.tfr <- file.path(getwd(), "TFRprojections")
run.tfr.mcmc(iter = 1000, nr.chains = 1, thin = 1, output.dir = sim.dir.tfr, seed = 1)
run.tfr3.mcmc(sim.dir = sim.dir.tfr, iter = 1000, nr.chains = 1, thin = 1, seed = 1)
tfr.predict(sim.dir = sim.dir.tfr, nr.traj = 100, burnin = 500, burnin3 = 500, seed = 1)
#life expectancy
sim.dir.e0 <- file.path(getwd(), "e0_projections")
run.e0.mcmc(sex = "Female", iter = 1000, nr.chains = 1, thin = 1,  output.dir = sim.dir.e0, seed = 1)
# Generate probabilistic population projections
sim.dir.pop <- file.path(getwd(), "pop_projections")
pop.pred <- pop.predict(output.dir = sim.dir.pop, inputs = list(tfr.sim.dir = sim.dir.tfr, e0F.sim.dir = sim.dir.e0, e0M.sim.dir = "joint_"), keep.vital.events = TRUE, verbose = TRUE)
pop.pred <- get.pop.prediction(sim.dir.pop)

# Get the locations where R packages are installed
package_locations <- .libPaths()
print(package_locations)

#population prediction example
pop.pred <- get.pop.prediction(sim.dir.pop)
country <- 840
summary(pop.pred, country)
#plot trajectories
pop.trajectories.plot(pop.pred, country = country, sum.over.ages = TRUE)
pop.trajectories.plot(pop.pred, country = country, sex = "male", age = 1:5, sum.over.ages = TRUE) #age refer this which(pop.pred$ages < 45) or pop.pred$ages[1:3]
pop.byage.plot(pop.pred, country = country, year = 2100)
pop.byage.plot(pop.pred, country = country, year = 2060, pi = 80, nr.traj = 50)
pop.byage.plot(pop.pred, country = country, year = 1960, add = TRUE, col = "blue", show.legend = FALSE)
pop.cohorts.plot(pop.pred, country = country)
cohort.data <- cohorts(pop.pred, country = country)
head(names(cohort.data))
pop.cohorts.plot(pop.pred, cohort.data = cohort.data, cohorts = c(1980, 2000, 2020))
#population pyramid
pop.pyramid(pop.pred, country, year = c(2100, 2015), age = 1:23)
pop.trajectories.pyramid(pop.pred, country, year = c(2100, 2025, 1950),age = 1:23, pi = 95, nr.traj = 0, proportion = TRUE)
pop.pyramidAll(pop.pred, year = list(c(2100, 2015), c(2050, 2015)), age = 1:23, output.dir = "mypyramids")

#state specific approach
data <- read.table(file.path(find.package("bayesPop"), "ex-data", "popestimates_WAKing.txt"), header = TRUE, row.names = 1)
head(data)

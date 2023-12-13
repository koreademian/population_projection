# Define a new library directory
new_lib_dir <-"C:/Users/bkf510/OneDrive - University of Texas at San Antonio/Desktop/apps/R/library"

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
library(bayesMig)
library(bayesPop)

#procedure for before 2019 research
setwd("C:/Users/bkf510/OneDrive - University of Texas at San Antonio/Desktop/apps/R/Population projection/Country")
getwd()
#TFR
sim.dir.tfr <- file.path(getwd(), "TFRprojections")
run.tfr.mcmc(iter = 1000, nr.chains = 1, thin = 1, output.dir = sim.dir.tfr, seed = 1)
#run.tfr3.mcmc(sim.dir = sim.dir.tfr, iter = 1000, nr.chains = 1, thin = 1, seed = 1)
tfr.predict(sim.dir = sim.dir.tfr, nr.traj = 100, burnin = 500, burnin3 = 500, seed = 1)
#life expectancy
sim.dir.e0 <- file.path(getwd(), "e0_projections")
run.e0.mcmc(sex = "Female", iter = 1000, nr.chains = 1, thin = 1,  output.dir = sim.dir.e0, seed = 1)
#migration
sim.dir.mig <- file.path(getwd(), "mig_projections")
run.mig.mcmc(nr.chains = 4, iter = 10000, thin = 10, output.dir = sim.dir.mig

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

##procedure from 2023 research for subnational projection
#View the county tfr data
my.subtfr.file<-file.path(find.package("bayesTFR"),'extdata','TXcounty_tfr_annual.txt')
subtfr<-read.delim(my.subtfr.file, check.names=FALSE)
head(subtfr)

#Directory with national projections
nat.dir<-file.path(find.package("bayesTFR"),"ex-data", "bayesTFR.output")

#Subnational projections for United States
subnat.dir<-file.path("C:/Users/bkf510/OneDrive - University of Texas at San Antonio/Desktop/apps/R/Population projection/Country", "TFRprojections")
preds<-tfr.predict.subnat(840, my.tfr.file=my.subtfr.file, sim.dir=nat.dir, output.dir=subnat.dir, start.year=2022, annual=TRUE)
names(preds)
get.countries.table(preds[["840"]])
summary(preds[["840"]], 'Texas')

#plot subnational and national TFR
nat.pred<-get.tfr.prediction(nat.dir)

tfr.trajectories.plot(preds[["840"]],453, pi=95, half.child.variant=FALSE)
tfr.trajectories.plot(nat.pred,840, pi=95, half.child.variant=FALSE,add=TRUE)

#retrieve trajectories
trajs.travis<-get.tfr.trajectories(preds[["840"]],453)
summary(t(trajs.travis))

#cleanup
unlink(subnat.dir)


#View the county life expectancy data
my.sube0.file<-file.path(find.package("bayesLife"), 'extdata','countye0_annual.txt')
sube0<-read.delim(my.sube0.file, check.names=FALSE)
head(sube0)

#Directory with national projections
nat.dir<-file.path(find.package("bayesLife"),"ex-data", "bayesLife.output")

#Subnational projections for United States
#including the joint female-male gap model
subnat.dir<-file.path("C:/Users/bkf510/OneDrive - University of Texas at San Antonio/Desktop/apps/R/Population projection/Country", "e0_projections")
preds<-e0.predict.subnat(840, my.e0.file=my.sube0.file, sim.dir=nat.dir, output.dir=subnat.dir, start.year=2022, annual=TRUE)
names(preds)
get.countries.table(preds[["840"]])
summary(preds[["840"]], 'Texas')
e0.trajectories.plot(preds[["840"]], 'Bexar County')

#plot subnational and national e0 in oneplot
nat.pred<-get.e0.prediction(nat.dir)
e0.trajectories.plot(preds[["840"]],29, pi=95, show.legend=TRUE)
e0.trajectories.plot(nat.pred,840, pi=95, add=TRUE, col=rep("darkgreen",5), nr.traj=0, show.legend=TRUE, lty=1, bty='n')

#add male projection to USA
#using (wrongly) female data only for demonstration
predUSA<-e0.jmale.predict.subnat(preds[["840"]], my.e0.file=my.sube0.file)

#retrieve male prediction object
predUSAMale<-get.rege0.prediction(subnat.dir,840, joint.male=TRUE)

#the smae works using
predUSAMale<-get.e0.jmale.prediction(predUSA)
summary(predUSAMale)

#Retrieve female and male trajectories
trajsF.Bexar<-get.e0.trajectories(predUSA, "Bexar County")
summary(trajsF.Bexar)
trajsF.Bexar.table<-e0.trajectories.table(predUSA, "Bexar County")

trajsM.Bexar<-get.e0.trajectories(predUSAMale, "Bexar County")
summary(trajsM.Bexar)
trajsM.Bexar.table<-e0.trajectories.table(predUSAMale, "Bexar County")

trajsF.Bexar.table
trajsM.Bexar.table

#summary of differences
summary(t(trajsF.Bexar-trajsM.Bexar))

#cleanup
unlink(subnat.dir)

#migration
#simulation for TX County
us.mig.file<-file.path(find.package("bayesMig"), "extdata", "TX_county_mig.txt")
sim.dir<-file.path("C:/Users/bkf510/OneDrive - University of Texas at San Antonio/Desktop/apps/R/Population projection/Country", "mig_projections")
m<-run.mig.mcmc(nr.chains=3, iter=100, thin=1, my.mig.file=us.mig.file, annual=TRUE, output.dir=sim.dir, present.year=2022, replace.output=TRUE)
summary(m)
summary(m, "Bexar County")

mig.partraces.plot(m)
mig.partraces.cs.plot("Bexar County", m)

#prediction
pred<-mig.predict(sim.dir=sim.dir, burnin=5, end.year=2100)

mig.trajectories.plot(pred, "Bexar County", pi=80, ylim=c(-0.02,0.02))
mig.trajectories.plot(pred, "Bexar County")
summary(pred,"Bexar County")

#View locations included in the simulation
get.countries.table(pred)

unlink(sim.dir, recursive=TRUE)

#subnational projections for USA
#Use subnational probabilistic TFR simulation
my.subtfr.file<-file.path(find.package("bayesTFR"), 'extdata', 'TXcounty_tfr_annual.txt')
tfr.nat.dir<-file.path(find.package("bayesTFR"), 'ex-data', 'bayesTFR.output')
tfr.reg.dir<-tempfile()
#tfr.reg.dir<-file.path("C:/Users/bkf510/OneDrive - University of Texas at San Antonio/Desktop/apps/R/Population projection/Country", "TFRprojections")
tfr.preds<-tfr.predict.subnat(840, my.tfr.file=my.subtfr.file, sim.dir=tfr.nat.dir, output.dir=tfr.reg.dir, start.year=2022, annual=TRUE)

#use subnational probabilistic e0
my.sube0.file<-file.path(find.package("bayesLife"), 'extdata','countye0_annual.txt')
e0.nat.dir<-file.path(find.package("bayesLife"),"ex-data", "bayesLife.output")
e0.reg.dir<-tempfile()
e0.preds<-e0.predict.subnat(840, my.e0.file=my.sube0.file, sim.dir=e0.nat.dir, output.dir=e0.reg.dir, start.year=2022, predict.jmale=TRUE,my.e0M.file=my.sube0.file,annual=TRUE)

#Population projections
data.dir<-file.path(find.package("bayesPop"), "extdata")
sim.dir<-tempfile()
pred<-pop.predict.subnat(output.dir=sim.dir,
locations=file.path(data.dir,"USAlocations.txt"),
inputs=list(popM=file.path(data.dir, ""),
            popF=file.path(data.dir,""),
            patterns=file.path(data.dir,"USApatterns.txt"),
            tfr.sim.dir=file.path(tfr.reg.dir, "subnat", "c840"), #C:\Users\bkf510\AppData\Local\Temp\Rtmp2FLlIF\file2f44e204fed
            e0.sim.dir=file.path(e0.reg.dir, "subnat_ar1", "c840"), #C:\Users\bkf510\AppData\Local\Temp\Rtmp2FLlIF\file2f457cf2777
            e0M.sim.dir="joint_"
            ),
        verbose=TRUE)
)
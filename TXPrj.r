# Define a new library directory
new_lib_dir <-"C:/Users/bkf510/OneDrive - University of Texas at San Antonio/Desktop/apps/R/library"

# Set the new library directory as the default
.libPaths(c(new_lib_dir, .libPaths()))

install.packages("wpp2022")
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
library(lubridate)
library(wpp2022)
#library(wpp2019)
#library(MortCast)

#from UW github
#library(devtools)
#options(timeout = 600)
#install_github("PPgp/wpp2022", force=TRUE)

#procedure for before 2019 research
setwd("Y:/Projections/Research/Bayesian_Model/simulation")
getwd()

#Overall Flow
#TFR run by mcmc for simulating the total fertility rates of all countries of world (phaseII)
#sim.dir.tfr <- file.path(getwd(), "TFRprojections")
#run.tfr.mcmc(iter = 50000, nr.chains = 5, thin = 10, output.dir = sim.dir.tfr, seed = 1)
#run.tfr3.mcmc(sim.dir = sim.dir.tfr, iter = 50000, nr.chains = 5, thin = 10, seed = 1, replace.output=TRUE)
#tfr.predict(sim.dir = sim.dir.tfr, nr.traj = NULL, burnin = 1000, burnin3 = 1000, seed = 1)
#life expectancy
#storing e0_prediction trajectories of each nation
#sim.dir.e0 <- file.path(getwd(), "e0_projections")
#run.e0.mcmc(sex = "Female", iter = 50000, nr.chains = 5, thin = 10,  output.dir = sim.dir.e0, seed = 1)
#e0.predict(sim.dir=sim.dir.e0, nr.traj = NULL, burnin = 1000, seed = 1)
#migration
#storing mig_prediction trajectories of each nation
#sim.dir.mig <- file.path(getwd(), "mig_projections")
#run.mig.mcmc(nr.chains = 5, iter = 50000, thin = 10, output.dir = sim.dir.mig)
#mig.predict(sim.dir=sim.dir.mig, nr.traj = NULL, burnin = 1000, seed = 1)

# Generate probabilistic population projections
#sim.dir.pop <- file.path(getwd(), "pop_projections")
#pop.pred <- pop.predict(output.dir = sim.dir.pop, inputs = list(tfr.sim.dir = sim.dir.tfr, e0F.sim.dir = sim.dir.e0, e0M.sim.dir = "joint_"), keep.vital.events = TRUE, verbose = TRUE)
#pop.pred <- get.pop.prediction(sim.dir.pop)

# Get the locations where R packages are installed
#package_locations <- .libPaths()
#print(package_locations)

#Subnational TFR with Texas County level
##procedure from 2023 research for subnational projection
#View the county tfr data
#my.subtfr.file<-file.path(find.package("bayesTFR"),'extdata','TXcounty_tfr_annual.txt')
#subtfr<-read.delim(my.subtfr.file, check.names=FALSE)
#head(subtfr)

#Directory with national projections
#nat.dir<-file.path("Y:/Projections/Research/Bayesian_Model/simulation", "TFRprojections")

#Subnational projections for United States
#subnat.dir<-file.path("Y:/Projections/Research/Bayesian_Model/simulation/TFRprojections")
#preds<-tfr.predict.subnat(840, my.tfr.file=my.subtfr.file, sim.dir=nat.dir, output.dir=subnat.dir, start.year=2022, annual=TRUE)
#names(preds)
#get.countries.table(preds[["840"]])
#summary(preds[["840"]], 'Bexar County')

#plot subnational and national TFR
#nat.pred<-get.tfr.prediction(nat.dir)

#tfr.trajectories.plot(preds[["840"]],29, pi=95, half.child.variant=FALSE)

#retrieve trajectories
#trajs.bexar<-get.tfr.trajectories(preds[["840"]],29)
#summary(t(trajs.bexar))

#cleanup
#unlink(subnat.dir)


#View the county life expectancy data
#my.sube0.file<-file.path(find.package("bayesLife"), 'extdata','countye0_annual.txt')
#sube0<-read.delim(my.sube0.file, check.names=FALSE)
#head(sube0)

#Directory with national projections
#nat.dir<-file.path("Y:/Projections/Research/Bayesian_Model/simulation", "e0_projections")

#Subnational projections for United States
#including the joint female-male gap model
#subnat.dir<-file.path("Y:/Projections/Research/Bayesian_Model/simulation/e0_projections")
#preds<-e0.predict.subnat(840, my.e0.file=my.sube0.file, sim.dir=nat.dir, output.dir=subnat.dir, start.year=2022, annual=TRUE)
#names(preds)
#get.countries.table(preds[["840"]])
#summary(preds[["840"]], 'Bexar County')

#plot subnational and national e0 in oneplot
#nat.pred<-get.e0.prediction(nat.dir)
#e0.trajectories.plot(preds[["840"]],29, pi=95, show.legend=TRUE)
#e0.trajectories.plot(nat.pred,840, pi=95, add=TRUE, col=rep("darkgreen",5), nr.traj=0, show.legend=TRUE, lty=1, bty='n')

#add male projection to USA
#using (wrongly) female data only for demonstration
#predUSA<-e0.jmale.predict.subnat(preds[["840"]], my.e0.file=my.sube0.file)

#retrieve male prediction object
#predUSAMale<-get.rege0.prediction(subnat.dir,840, joint.male=TRUE)

#the smae works using
#predUSAMale<-get.e0.jmale.prediction(predUSA)
#summary(predUSAMale)

#Retrieve female and male trajectories
#trajsF.Bexar<-get.e0.trajectories(predUSA, "Bexar County")
#summary(trajsF.Bexar)
#trajsF.Bexar.table<-e0.trajectories.table(predUSA, "Bexar County")

#trajsM.Bexar<-get.e0.trajectories(predUSAMale, "Bexar County")
#summary(trajsM.Bexar)
#trajsM.Bexar.table<-e0.trajectories.table(predUSAMale, "Bexar County")

#trajsF.Bexar.table
#trajsM.Bexar.table

#summary of differences
#summary(t(trajsF.Bexar-trajsM.Bexar))

#cleanup
#unlink(subnat.dir)

#migration
#simulation for TX County
#us.mig.file<-file.path("Y:/Projections/Research/Bayesian_Model/Overview/TXmig_rates_2010_2022.txt")
sim.dir.mig<-file.path("Y:/Projections/Research/Bayesian_Model/simulation/mig_projections")
#run.mig.mcmc(nr.chains=5, iter=50000, thin=10, my.mig.file=us.mig.file, annual=TRUE,output.dir=sim.dir.mig, present.year=2020, replace.output=TRUE)
#summary(m, "Bexar County")
#mig.partraces.plot(m)
#mig.partraces.cs.plot("Bexar County", m)

#prediction
#pred<-mig.predict(sim.dir=sim.dir.mig, nr.traj = 50000, burnin = 1000, seed = 1, end.year=2060, replace.output=TRUE)

mig.sub.preds<-get.mig.prediction(sim.dir=sim.dir.mig)
#mig.trajectories.plot(mig.sub.preds, "Bexar County", pi=80, ylim=c(-0.02,0.02))
mig.trajectories.plot(mig.sub.preds, 29)
#mig.trajectories.table(mig.sub.preds, 29)

mig.sub.plots<-file.path("Y:/Projections/Research/Bayesian_Model/simulation/mig_projections/subnat/plots")
#mig.sub.tables<-file.path("Y:/Projections/Research/Bayesian_Model/simulation/mig_projections/subnat")
#mig.trajectories.plot.all(mig.sub.preds, output.dir=mig.sub.plots)
#mig.write.projection.summary(mig.sub.preds, output.dir=mig.sub.tables)

#summary(pred,"Bexar County")
#summary(pred)
#View locations included in the simulation
#get.countries.table(mig.sub.preds)

#unlink(sim.dir, recursive=TRUE)

#Use subnational probabilistic TFR simulation
#my.subtfr.file<-file.path("Y:/Projections/Research/Bayesian_Model/Overview/TXcounty_tfr_annual.txt") 
#tfr.nat.dir<-file.path("Y:/Projections/Research/Bayesian_Model/simulation/TFRprojections")
tfr.sub.dir<-file.path("Y:/Projections/Research/Bayesian_Model/simulation/TFRprojections/")
#tfr.preds<-tfr.predict.subnat(840, my.tfr.file=my.subtfr.file, sim.dir=tfr.nat.dir, output.dir=tfr.sub.dir, start.year=2022, annual=TRUE)
tfr.preds<-get.regtfr.prediction(tfr.sub.dir)
tfr.trajectories.plot(tfr.preds[["840"]],29, pi=95, half.child.variant=FALSE)
#tfr.sub.plots<-file.path("Y:/Projections/Research/Bayesian_Model/simulation/TFRprojections/subnat/plots")
#tfr.sub.tables<-file.path("Y:/Projections/Research/Bayesian_Model/simulation/TFRprojections/subnat/c840")
#tfr.trajectories.plot.all(tfr.preds[["840"]], output.dir = tfr.sub.plots, output.type = "png", main = NULL, verbose = FALSE)
#write.projection.summary(dir=tfr.sub.tables)

#use subnational probabilistic e0
#my.sube0.file<-file.path("Y:/Projections/Research/Bayesian_Model/Overview/countye0_annual.txt") 
#e0.nat.dir<-file.path("Y:/Projections/Research/Bayesian_Model/simulation/e0_projections")
e0.sub.dir<-file.path("Y:/Projections/Research/Bayesian_Model/simulation/e0_projections")
#e0.preds<-e0.predict.subnat(840, my.e0.file=my.sube0.file, sim.dir=e0.nat.dir, output.dir=e0.sub.dir, start.year=2022, predict.jmale=TRUE,my.e0M.file=my.sube0.file,annual=TRUE)
e0.preds<-get.rege0.prediction(e0.sub.dir, country=NULL, method="ar1", joint.male=FALSE)
e0.trajectories.plot(e0.preds[["840"]],29, pi=95, show.legend=TRUE)
#head(tfr.preds)
#summary(e0.preds)
e0.sub.plots<-file.path("Y:/Projections/Research/Bayesian_Model/simulation/e0_projections/subnat/plots")
#e0.sub.tables<-file.path("Y:/Projections/Research/Bayesian_Model/simulation/e0_projections/subnat/c840")
#e0.trajectories.plot.all(e0.preds[["840"]], output.dir = e0.sub.plots, output.type = "png", main = NULL, verbose = FALSE)
#write.projection.summary(dir=e0.sub.tables) 

#create asmig 'migrationM'
library(data.table)
asmig<-read.table(file="Y:/Projections/Research/Bayesian_Model/Overview/TXmig_rates_asmr.txt", header=TRUE, sep="\t")
colnames(asmig)[colnames(asmig) == "X2022"] <- "2022"
migration.totals2age(asmig, ages=NULL, annual=FALSE, time.periods="2022", schedule=NULL, scale=1, method="auto", 
                sex="M", id.col="country_code", mig.is.rate=TRUE)
#Population projections 5*5
data.dir<-file.path("Y:/Projections/Research/Bayesian_Model/Overview")
sim.dir<-file.path("Y:/Projections/Research/Bayesian_Model/simulation/bayesPrediction")
mig.sim.dir<-file.path("Y:/Projections/Research/Bayesian_Model/simulation/mig_projections")
pred<-pop.predict.subnat(output.dir=sim.dir, default.country=840,end.year=2060, start.year=2010, present.year=2020,
    locations=file.path(data.dir,"USAlocations.txt"),
    inputs=list(popM=file.path(data.dir, "USApopM.txt"),
            popF=file.path(data.dir,"USApopF.txt"),
            #migF=file.path(data.dir,"migF.txt"),
            #migM=file.path(data.dir,"migM.txt"),
            #mig=file.path(data.dir,"mig.txt"),
           #patterns=file.path(data.dir,"USApatterns.txt"),
            tfr.sim.dir=file.path(tfr.sub.dir, "subnat", "c840"), #C:/Users/bkf510/AppData/Local/Temp/Rtmp2FLlIF/file2f44e204fed
            e0F.sim.dir=file.path(e0.sub.dir, "subnat_ar1", "c840"), #C:/Users/bkf510/AppData/Local/Temp/Rtmp2FLlIF/file2f457cf2777
            e0M.sim.dir="joint_",
            #migtrj=file.path(mig.sim.dir, "predictions", "projection_summary_user_friendly.csv"), #use total migration by county
            GQpopM=file.path(data.dir, "USApopGQM.txt"), #use GQcounts by county for males
            GQpopF=file.path(data.dir, "USApopGQF.txt") #use GQcounts by county for females
            ),
    verbose=TRUE) #, replace.output=TRUE
outdir<-file.path("Y:/Projections/Research/Bayesian_Model/simulation/bayesPrediction/plot")
#outdir<-file.path("Y:/Projections/Research/Bayesian_Model/simulation/bayesPrediction/table")
pred<-get.pop.prediction(sim.dir=sim.dir)

pop.trajectories.plot(pred,29, sum.over.ages=TRUE)
#pop.trajectories.plotAll(pred,year=2060, sum.over.ages=TRUE, output.dir=outdir)

pop.byage.plot(pred,expression = "G507_{}", nr.traj = 50, year = 2050)
age<-pop.byage.table(pred, expression = "B29{}", pi=c(80,95),  year = 2030)
pop.pyramid(pred, 29, year=2060)
write.pop.projection.summary(pred, what=c(),output.dir = outdir)
#get.countries.table(pred)

#wirte various measures
#outdir<-file.path("Y:/Projections/Research/Bayesian_Model/simulation/bayesPrediction/output")
#write.pop.projection.summary(pred, what=c("pop", "popsexage", "popsex"),output.dir=outdir)
#write.pop.projection.summary(pred, expression="G29_f", output.dir=outdir, include.observed=TRUE, digits=2)
#unlink(outdir, recursive=TRUE)

#write migration projections
mig.sim.dir<-file.path("Y:/Projections/Research/Bayesian_Model/simulation/mig_projections")
county_mig<-get.mig.prediction(sim.dir=mig.sim.dir)
mig.trajectories.plot(county_mig, 85, pi=95)
mig.trajectories.plot(county_mig, "Bexar County")
mig.trajectories.plot.all(county_mig, output.dir=mig.sim.dir )
mig.write.projection.summary(county_mig, output.dir=mig.sim.dir)
summary(mig,"Bexar County")
summary(mig)
age.specific.migration(county_mig, output.dir=outdir)
asmig <- age.specific.migration()
head(asmig$male)
head(asmig$female)
outdir<-file.path("Y:/Projections/Research/Bayesian_Model/simulation/bayesPrediction/output")
write.pop.projection.summary(pred, what=NULL, expression="G29_M" , output.dir=outdir)
write.pop.trajectories(pred, expression="G29_f{1:19}" , output.file="pop_BEXAR_trj.csv", byage=TRUE)

df<-read.csv("Y:/Projections/Research/Bayesian_Model/simulation/mig_projections/predictions/projection_summary_user_friendly.csv")
asmig<-migration.totals2age(df, ages.to.zero=TRUE, annual = TRUE, scale = 1, method = "rc", time.periods=NULL, sex = "M",  id.col = "country_code")
age.specific.migration(county_mig, years = seq(2010, 2100, by = 5), countries = NULL, smooth = TRUE, rescale = TRUE, ages.to.zero = TRUE,
write.to.disk = FALSE, directory = getwd(), file.prefix = "migration", depratio = wpp.year == 2019, verbose = TRUE)
head(asmr)

get.trajectory.indices(pred, 29, what=c("TFR", "migM", ",migF"))
extract.trajectories.eq(pred, country = 29, expression = "GXXX_F[1:19]",
quant = 0.5, values = NULL, nr.traj = 1)


head(df)
rcastro.schedule(annual=FALSE)

migAll<-get.pop.ex("GXXX_M[1:20]", pred)
head(migALL)

write.pop.projection.summary(pred, expression="GXXX[1:19]", file.suffix="amf", output.dir=outdir, include.observed=TRUE, digits=2)

get.trajectory.indices(pred, 29, what=c("migM", "migF"))


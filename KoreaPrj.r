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
library(wpp2019)
#library(wpp2019)
#library(MortCast)

#from UW github
#library(devtools)
#options(timeout = 600)
#install_github("PPgp/wpp2022", force=TRUE)

#procedure for 2022 research
setwd("C:/Users/bkf510/OneDrive - University of Texas at San Antonio/Desktop/Bayesian_backup")
getwd()

#Overall Flow
#TFR run by mcmc for simulating the total fertility rates of all countries of world (phaseIII)
sim.dir.tfr <- file.path(getwd(), "TFRprojections")
run.tfr.mcmc(iter = 1000, nr.chains = 5, thin = 10, output.dir = sim.dir.tfr, wpp.year=2019, start.year=1980,  seed = 1, replace.output=TRUE)
run.tfr3.mcmc(sim.dir = sim.dir.tfr, iter = 1000, nr.chains = 5, thin = 10, seed = 1, wpp.year=2019, replace.output=TRUE)
tfr.predict(sim.dir = sim.dir.tfr, nr.traj = NULL, thin=NULL, burnin3 = 100, seed = 1, replace.output=TRUE)
tfr.dir<-file.path("C:/Users/bkf510/OneDrive - University of Texas at San Antonio/Desktop/Bayesian_backup/TFRprojections")
pred<-get.tfr.prediction(sim.dir=tfr.dir)
summary(pred, 410)
tfr.trajectories.plot(pred, 410)
#life expectancy
#storing e0_prediction trajectories of each nation
sim.dir.e0 <- file.path(getwd(), "e0_projections")
run.e0.mcmc(sex = c("Female", "Male"), iter = 1000, nr.chains = 5, thin = 10, wpp.year=2019, output.dir = sim.dir.e0, seed = 1, replace.output=TRUE)
e0.predict(sim.dir=sim.dir.e0, nr.traj = NULL, burnin = 100, seed = 1)
e0.dir<-file.path("C:/Users/bkf510/OneDrive - University of Texas at San Antonio/Desktop/Bayesian_backup/e0_projections")
pred<-get.e0.prediction(sim.dir=e0.dir)
summary(pred,410)
e0.trajectories.plot(pred,410)
#migration
#storing mig_prediction trajectories of each nation
sim.dir.mig <- file.path(getwd(), "mig_projections")
run.mig.mcmc(nr.chains = 5, iter = 1000, thin = 10, output.dir = sim.dir.mig, replace.output=TRUE)
mig.predict(sim.dir=sim.dir.mig, nr.traj = NULL, burnin = 100, seed = 1)
mig.dir<-file.path("C:/Users/bkf510/OneDrive - University of Texas at San Antonio/Desktop/Bayesian_backup/mig_projections")
pred<-get.mig.prediction(sim.dir=mig.dir)
summary(pred,410)
mig.trajectories.plot(pred,410)
# Generate probabilistic population projections
sim.dir.pop <- file.path(getwd(), "pop_projections")
pop.pred <- pop.predict(output.dir = sim.dir.pop, inputs = list(tfr.sim.dir = sim.dir.tfr, e0F.sim.dir = sim.dir.e0, e0M.sim.dir = "joint_", mig.sim.dir=sim.dir.mig),
     wpp.year=2019, start.year=1980, replace.output=TRUE, keep.vital.events = TRUE, verbose = TRUE)
pop.pred <- get.pop.prediction(sim.dir=sim.dir.pop)
str(pop.pred)
summary(pop.pred)
pop.trajectories.plot(pop.pred,410)


# Get the locations where R packages are installed
#package_locations <- .libPaths()
#print(package_locations)

#Subnational TFR with SouthKorea provincial level
##procedure from 2023 research for subnational projection
#View the county tfr data
my.subtfr.file<-file.path("C:/Users/bkf510/OneDrive - University of Texas at San Antonio/Desktop/Bayesian_backup/subnational/tfr_subnational_Korea.txt")
subtfr<-read.delim(my.subtfr.file, check.names=FALSE)
head(subtfr)

#Directory with national projections
nat.dir<-file.path("C:/Users/bkf510/OneDrive - University of Texas at San Antonio/Desktop/Bayesian_backup/TFRprojections")

#Subnational projections for Korea
subnat.dir<-file.path("C:/Users/bkf510/OneDrive - University of Texas at San Antonio/Desktop/Bayesian_backup/TFRprojections")
preds<-tfr.predict.subnat(410, my.tfr.file=my.subtfr.file, sim.dir=nat.dir, output.dir=subnat.dir, start.year=2020, annual=TRUE) #annual=TRUE
names(preds)
get.countries.table(preds[["410"]])
summary(preds[["410"]], 'Seoul')
tfr.trajectories.plot.all(preds[["410"]], output.dir = file.path(subnat.dir, 'subnat_plot'), output.type = "png", main = NULL, verbose = FALSE)
tfr.trajectories.table(preds[["410"]], 11,  pi = c(80, 95),half.child.variant = TRUE, adjusted = TRUE)
#plot subnational and national TFR
#nat.pred<-get.tfr.prediction(nat.dir)

#tfr.trajectories.plot(preds[["840"]],29, pi=95, half.child.variant=FALSE)

#retrieve trajectories
#trajs.bexar<-get.tfr.trajectories(preds[["840"]],29)
#summary(t(trajs.bexar))

#cleanup
unlink(subnat.dir)


#View the county life expectancy data
my.sube0.file<-file.path("C:/Users/bkf510/OneDrive - University of Texas at San Antonio/Desktop/Bayesian_backup/subnational/e0_subnational_Korea.txt")
sube0<-read.delim(my.sube0.file, check.names=FALSE)
head(sube0)

#Directory with national projections
nat.dir<-file.path("C:/Users/bkf510/OneDrive - University of Texas at San Antonio/Desktop/Bayesian_backup/e0_projections")

#Subnational projections for United States
#including the joint female-male gap model
subnat.dir<-file.path("C:/Users/bkf510/OneDrive - University of Texas at San Antonio/Desktop/Bayesian_backup/e0_projections")
preds<-e0.predict.subnat(410, my.e0.file=my.sube0.file, sim.dir=nat.dir, output.dir=subnat.dir, start.year=2020, annual=TRUE)
names(preds)
get.countries.table(preds[["410"]])
summary(preds[["410"]], 'Seoul')

#add male projection to Korea
#using (wrongly) female data only for demonstration
predKorea<-e0.jmale.predict.subnat(preds[["410"]], my.e0.file=my.sube0.file)

#retrieve male prediction object
predKoreaMale<-get.rege0.prediction(subnat.dir,410, joint.male=TRUE)

#the same works using
predKoreaMale<-get.e0.jmale.prediction(predKorea)
summary(predKoreaMale)

#plot subnational and national e0 in oneplot
nat.pred<-get.e0.prediction(nat.dir)
#e0.trajectories.plot(preds[["840"]],29, pi=95, show.legend=TRUE)
#e0.trajectories.plot(nat.pred,840, pi=95, add=TRUE, col=rep("darkgreen",5), nr.traj=0, show.legend=TRUE, lty=1, bty='n')
e0.trajectories.plot.all(preds[["410"]], output.dir = file.path(subnat.dir, 'subnat_plot'), output.type = "png", main = NULL, verbose = FALSE)
e0.trajectories.table(preds[["410"]],11)
#Retrieve female and male trajectories
trajsF.Seoul<-get.e0.trajectories(predKorea, "Seoul")
summary(trajsF.Seoul)
trajsF.Seoul.table<-e0.trajectories.table(predKorea, "Seoul")

trajsM.Seoul<-get.e0.trajectories(predKoreaMale, "Seoul")
summary(trajsM.Seoul)
#trajsM.Bexar.table<-e0.trajectories.table(predUSAMale, "Bexar County")

#trajsF.Bexar.table
#trajsM.Bexar.table

#summary of differences
summary(t(trajsF.Seoul-trajsM.Seoul))

#cleanup
unlink(subnat.dir)

#migration
#simulation for Korea areas
Korea.mig.file<-file.path("C:/Users/bkf510/OneDrive - University of Texas at San Antonio/Desktop/Bayesian_backup/subnational/migrates_subnational_Korea.txt")
sim.dir.mig<-file.path("C:/Users/bkf510/OneDrive - University of Texas at San Antonio/Desktop/Bayesian_backup/mig_projections/subnat")
#run.mig.mcmc(nr.chains=5, iter=1000, thin=10, my.mig.file=Korea.mig.file, annual=TRUE,output.dir=sim.dir.mig, present.year=2020, replace.output=TRUE)
#summary(m, "Seoul")
#mig.partraces.plot(m)
#mig.partraces.cs.plot("Seoul", m)

#prediction
#pred<-mig.predict(sim.dir=sim.dir.mig, nr.traj = 1000, burnin = 100, seed = 1, end.year=2100, replace.output=TRUE)

mig.sub.preds<-get.mig.prediction(sim.dir=sim.dir.mig)

mig.sub.plots<-file.path("C:/Users/bkf510/OneDrive - University of Texas at San Antonio/Desktop/Bayesian_backup/mig_projections/subnat/plots")
mig.sub.tables<-file.path("C:/Users/bkf510/OneDrive - University of Texas at San Antonio/Desktop/Bayesian_backup/mig_projections/subnat/tables")
mig.write.projection.summary(mig.sub.preds, output.dir=mig.sub.tables)
mig.trajectories.plot.all(mig.sub.preds, output.dir = mig.sub.plots, output.type = "png", verbose = FALSE)

summary(mig.sub.preds,"Seoul")
#summary(pred)
#View locations included in the simulation
#get.countries.table(mig.sub.preds)

unlink(sim.dir.mig)

#Use subnational probabilistic TFR simulation
#my.subtfr.file<-file.path("Y:/Projections/Research/Bayesian_Model/Overview/TXcounty_tfr_annual.txt") 
#tfr.nat.dir<-file.path("Y:/Projections/Research/Bayesian_Model/simulation/TFRprojections")
#tfr.sub.dir<-file.path("Y:/Projections/Research/Bayesian_Model/simulation/TFRprojections/")
#tfr.preds<-tfr.predict.subnat(840, my.tfr.file=my.subtfr.file, sim.dir=tfr.nat.dir, output.dir=tfr.sub.dir, start.year=2022, annual=TRUE)
#tfr.preds<-get.regtfr.prediction(tfr.sub.dir)
#tfr.trajectories.plot(tfr.preds[["840"]],29, pi=95, half.child.variant=FALSE)
#tfr.sub.plots<-file.path("Y:/Projections/Research/Bayesian_Model/simulation/TFRprojections/subnat/plots")
#tfr.sub.tables<-file.path("Y:/Projections/Research/Bayesian_Model/simulation/TFRprojections/subnat/c840")
#tfr.trajectories.plot.all(tfr.preds[["840"]], output.dir = tfr.sub.plots, output.type = "png", main = NULL, verbose = FALSE)
#write.projection.summary(dir=tfr.sub.tables)

#use subnational probabilistic e0
#my.sube0.file<-file.path("Y:/Projections/Research/Bayesian_Model/Overview/countye0_annual.txt") 
#e0.nat.dir<-file.path("Y:/Projections/Research/Bayesian_Model/simulation/e0_projections")
#e0.sub.dir<-file.path("Y:/Projections/Research/Bayesian_Model/simulation/e0_projections")
#e0.preds<-e0.predict.subnat(840, my.e0.file=my.sube0.file, sim.dir=e0.nat.dir, output.dir=e0.sub.dir, start.year=2022, predict.jmale=TRUE,my.e0M.file=my.sube0.file,annual=TRUE)
#e0.preds<-get.rege0.prediction(e0.sub.dir, country=NULL, method="ar1", joint.male=FALSE)
#e0.trajectories.plot(e0.preds[["840"]],29, pi=95, show.legend=TRUE)
#head(tfr.preds)
#summary(e0.preds)
#e0.sub.plots<-file.path("Y:/Projections/Research/Bayesian_Model/simulation/e0_projections/subnat/plots")
#e0.sub.tables<-file.path("Y:/Projections/Research/Bayesian_Model/simulation/e0_projections/subnat/c840")
#e0.trajectories.plot.all(e0.preds[["840"]], output.dir = e0.sub.plots, output.type = "png", main = NULL, verbose = FALSE)
#write.projection.summary(dir=e0.sub.tables) 

#create asmig 'migrationM'
#library(data.table)
#asmig<-read.table(file="Y:/Projections/Research/Bayesian_Model/Overview/TXmig_rates_asmr.txt", header=TRUE, sep="/t")
#colnames(asmig)[colnames(asmig) == "X2022"] <- "2022"
#migration.totals2age(asmig, ages=NULL, annual=FALSE, time.periods="2022", schedule=NULL, scale=1, method="auto", 
#                sex="M", id.col="country_code", mig.is.rate=TRUE)
#Population projections 5*5
data.dir<-file.path("C:/Users/bkf510/OneDrive - University of Texas at San Antonio/Desktop/Bayesian_backup/subnational")
sim.dir.pop<-file.path("C:/Users/bkf510/OneDrive - University of Texas at San Antonio/Desktop/Bayesian_backup/simulation/subkorea")
tfr.sub.dir<-file.path("C:/Users/bkf510/OneDrive - University of Texas at San Antonio/Desktop/Bayesian_backup/TFRprojections/subnat")
e0.sub.dir<-file.path("C:/Users/bkf510/OneDrive - University of Texas at San Antonio/Desktop/Bayesian_backup/e0_projections/subnat_ar1")
mig.sim.dir<-file.path("C:/Users/bkf510/OneDrive - University of Texas at San Antonio/Desktop/Bayesian_backup/mig_projections/subnat")
pred<-pop.predict.subnat(output.dir=sim.dir.pop, default.country=410,end.year=2100, start.year=1980, present.year=2020, 
    locations=file.path(data.dir,"KRlocations.txt"),
    inputs=list(popM=file.path(data.dir, "KRpopM.txt"),
            popF=file.path(data.dir,"KRpopF.txt"),
            #patterns=file.path(data.dir,"KRpatterns.txt"),
            tfr.sim.dir=file.path(tfr.sub.dir, "c410"),
            e0F.sim.dir=file.path(e0.sub.dir, "c410"),
            e0M.sim.dir="joint_"
            ),
    verbose=TRUE, replace.output=TRUE)
out.plot.dir<-file.path(sim.dir.pop, "plot")
out.table.dir<-file.path(sim.dir.pop, "table")
sub.pop<-file.path("C:/Users/bkf510/OneDrive - University of Texas at San Antonio/Desktop/Bayesian_backup/simulation/subkorea")


head(pred)
str(pred)
class(pred)
sub_pred<-get.pop.prediction(sim.dir.pop)
summary(pred)

write.pop.projection.summary(pred, what=c(),output.dir = out.table.dir)
pop.pyramid(pred, 11, year=2060)
pop.pyramid(pred, 11, year=2100)

# Save the data frame to a CSV file
write.csv(pred, file = "C:/Users/bkf510/OneDrive - University of Texas at San Antonio/Desktop/Bayesian_backup/simulation/subkorea/summary_SKsub_pred.csv", row.names = FALSE)


write.csv(summary_df, file = "C:/Users/bkf510/OneDrive - University of Texas at San Antonio/Desktop/Bayesian_backup/simulation/bayesprediction/table/incheon.csv", row.names = FALSE)


pop.byage.plot(pred,expression = "P11_{}", nr.traj = 50, year = 2100)
age<-pop.byage.table(pred, expression = "B29{}", pi=c(80,95),  year = 2030)
pop.pyramid(pred, 11, year=2060)

#get.countries.table(pred)

sim.dir.pop <- file.path(getwd(), "pop_projections")
pop.pred <- pop.predict(output.dir = sim.dir.pop, inputs = list(tfr.sim.dir = sim.dir.tfr, e0F.sim.dir = sim.dir.e0, e0M.sim.dir = "joint_", mig.sim.dir=sim.dir.mig),
     wpp.year=2019, start.year=1980, replace.output=TRUE, keep.vital.events = TRUE, verbose = TRUE)
pop.pred <- get.pop.prediction(sim.dir=sim.dir.pop)
str(pop.pred)
summary(pop.pred, 410)
pop.trajectories.plot(pop.pred,410)


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


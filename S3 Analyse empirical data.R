
##########################################################
### This file supplements the paper "Nilsen, EB & Strand, O. (2017). Integrating data from several sources for increased 
### insight into demographic processes: Simulation studies and proof of concept for hierarchical change in ratio models. PlosOne".
### The code is used to run the jags models (S1 Model code.R) using real data from two reindeer areas (Sn?hetta & Kundsh?) in Norway.  
############################################

rm(list=ls())
###################################################################################
require(R2jags)
source("S2 Simulate data.R")

#####################
### SIMULATING SOME DATA - TO BE USED AS INITIAL VALUES HERE; 

Simulated_data <- SimPop(PHI3=0.95, mean_F=0.9, p1=0.5, p2=0.5, bias1=1)                                 # Run script to simmulate data 


#####################
### READING AND PREPARING DATA FROM SN?HETTA AND
### KNUTSH?

d <- read.csv("data/S4 ReindeerData.csv")
ds <- subset(d, Area=="Snohetta")
dk <- subset(d, Area=="Knutsho")




#########################
# Bundle data
# Sn?hetta data; 
bugs.data.Sno <- list(SU = ds[1:23,3],
                      J = ds[1:23,4],
                      H0f = ds[1:23,6],
                      H0m = ds[1:23,5],
                      H1f = ds[1:23,7],
                      H1m = ds[1:23,8],
                      Hadf = ds[1:23,9],
                      Hadm = ds[1:23,10],
                      T = 23,
                      y=ds[2:24, 2],
                      C0=ds[1:23,11],
                      Cm=ds[1:23,13],
                      Cf=ds[1:23,12])

# Knutsh? data; 
bugs.data.Knu <- list(SU = dk[1:23,3],
                      J = dk[1:23,4],
                      H0f = dk[1:23,6],
                      H0m = dk[1:23,5],
                      H1f = dk[1:23,7],
                      H1m = dk[1:23,8],
                      Hadf = dk[1:23,9],
                      Hadm = dk[1:23,10],
                      T = 23,
                      y=dk[2:24, 2],
                      C0=dk[1:23,11],
                      Cm=dk[1:23,13],
                      Cf=dk[1:23,12])


###########################################################################################################
### PREPARING JAGS MODEL RUNS; 

# MCMC settings
niter <- 300000
nthin <- 3
nburn <- 200000
nchains <- 3


# Initial values
inits <- function() { list( phi3 = runif(1, 0.9, 0.99), 
                            mean.f = runif(1, 2, 3), 
                            mean.phi1 = runif(1, 2, 3), 
                            sd_phi1 = runif(1, 0, 1),
                            sd_f = runif(1, 0, 1),
                            p2 = runif(23, 0.6, 0.9),
                            p1 = runif(23, 0.6, 0.9),
                            N0f = Simulated_data$N[1, 2:24]*7, 
                            N0m = Simulated_data$N[2, 2:24]*7, 
                            N1f = Simulated_data$N[3, 2:24]*7,
                            N1m = Simulated_data$N[4, 2:24]*7,
                            Nadf = Simulated_data$N[5, 2:24]*7,
                            Nadm = Simulated_data$N[6, 2:24]*7) }  



###########################################################################################################
# RUNNING MODELS
# M1: WITH COUNT DATA

Sno_M1 <- jags(data=bugs.data.Sno, inits=inits, 
                    parameters.to.save=c("mean.phi1", "phi3", "phi1", "f", "p1", "p2", "Ntot", "Xtot", "Sjuv", "fert", "sd_f", "sd_phi1"), 
                    model.file="jagsMod_M1.bug",n.chain=nchains, n.iter=niter, 
                    n.burnin=nburn, DIC=TRUE, working.directory=NULL, 
                    jags.seed = 123, refresh = niter/50, progress.bar = "text", digits=5)

Knu_M1 <- jags(data=bugs.data.Knu, inits=inits, 
               parameters.to.save=c("mean.phi1", "phi3", "phi1", "f", "p1", "p2", "Ntot", "Xtot", "Sjuv", "fert", "sd_f", "sd_phi1"), 
               model.file="jagsMod_M1.bug",n.chain=nchains, n.iter=niter, 
               n.burnin=nburn, DIC=TRUE, working.directory=NULL, 
               jags.seed = 123, refresh = niter/50, progress.bar = "text", digits=5)


# M2: WITHOUT COUNT DATA

Sno_M2 <- jags(data=bugs.data.Sno[-10], inits=inits, 
               parameters.to.save=c("mean.phi1", "phi3", "phi1", "f", "p1", "p2", "Ntot", "Xtot", "Sjuv", "fert", "sd_f", "sd_phi1"), 
               model.file="jagsMod_M2.bug",n.chain=nchains, n.iter=niter, 
               n.burnin=nburn, DIC=TRUE, working.directory=NULL, 
               jags.seed = 123, refresh = niter/50, progress.bar = "text", digits=5)

Knu_M2 <- jags(data=bugs.data.Knu[-10], inits=inits, 
               parameters.to.save=c("mean.phi1", "phi3", "phi1", "f", "p1", "p2", "Ntot", "Xtot", "Sjuv", "fert", "sd_f", "sd_phi1"), 
               model.file="jagsMod_M2.bug",n.chain=nchains, n.iter=niter, 
               n.burnin=nburn, DIC=TRUE, working.directory=NULL, 
               jags.seed = 123, refresh = niter/50, progress.bar = "text", digits=5)


####################################
#### not run - saving model results to .Rdata

# save(Sno_M1, Sno_M2, Knu_M1, Knu_M2, file="ModelOutput_real.Rdata")
 


###########################################################
##### SUMMARY TABLE - DEMOGRAPHIC RATES
##### Numbers reported in results section of paper

## Sn?hetta; 
Sn?hetta <- rbind(round(f_s1,2),round(f_s2,2),round(phi1_s1,2),round(phi1_s2,2),round(phi3_s1,2),round(phi3_s2,2))
rownames(Sn?hetta) <- c("f_M1", "f_M2", "Sjuv_M1", "Sjuv_M1", "Phi3_M1", "Phi3_M2")

# Knutsh? 
Knutsh? <- rbind(round(f_k1,2),round(f_k2,2),round(phi1_k1,2),round(phi1_k2,2),round(phi3_k1,2),round(phi3_k2,2))
rownames(Knutsh?) <- c("f_M1", "f_M2", "Sjuv_M1", "Sjuv_M1", "Phi3_M1", "Phi3_M2")
 


###########################################################################################################################
#### This file supplements the paper "Nilsen, EB & Strand, O. (2017). Integrating data from several sources for increased 
#### insight into demographic processes: Simulation studies and proof of concept for hierarchical change in ratio models. PlosOne".
#### It is used to assess the robustness of the results, by making repeated estimates of population parameters (using the 
#### hierarchical change in ratio model) using simulated data sets. Simulated data are made using code in the file "S2 Simulate data.R"
####  


#### FIRST - ASSUMING NO UNMODELLED BIAS IN THE DATA SETS
rm(list=ls)

library(R2jags)
source("S2 Simulate data.R")

Sens_results_no_bias <- matrix(ncol=17, nrow=0)    

  for(j in 1:200){
    temp_phi1 <- sample(seq(0.85, 0.95, 0.01), 1)
    temp_f <- sample(seq(0.6, 0.95, 0.01), 1)
    temp_phi3 <- sample(seq(0.88, 0.95, 0.01), 1)
    mult <- sample(seq(1,5, 1), 1)
    
    
    Simulated_data <- SimPop(m=mult, p1=0.5, p2=0.5, bias1=1, bias2=1, bias3=1, PHI3=temp_phi3, mean_phi1=temp_phi1, mean_F=temp_f)
    
    
    bugs.data <- list(SU = Simulated_data$SU[1,12:30],
                      J = Simulated_data$J[1,12:30],
                      H0f = Simulated_data$H[1,12:30],
                      H0m = Simulated_data$H[2,12:30],
                      H1f = Simulated_data$H[3,12:30],
                      H1m = Simulated_data$H[4,12:30],
                      Hadf = Simulated_data$H[5,12:30],
                      Hadm = Simulated_data$H[6,12:30],
                      T = 19,
                      C0=Simulated_data$C0[1,12:30],
                      Cm=Simulated_data$Cm[1,12:30],
                      Cf=Simulated_data$Cf[1,12:30])
    
    inits <- function() { list( phi3 = runif(1, 0.8, 0.99), 
                                phi1 = runif(1, 0.75, 0.95), 
                                f = runif(1, 0.5, 0.95),
                                p2 = runif(1, 0.5, 0.9),
                                p1 = runif(1, 0.4, 0.9),
                                N0f = Simulated_data$N[1, 12:30]*2, 
                                N0m = Simulated_data$N[2, 12:30]*2, 
                                N1f = Simulated_data$N[3, 12:30]*2,
                                N1m = Simulated_data$N[4, 12:30]*2,
                                Nadf = Simulated_data$N[5, 12:30]*2,
                                Nadm = Simulated_data$N[6, 12:30]*2) } 
    # MCMC settings
    niter <- 25000
    nthin <- 3
    nburn <- 15000
    nchains <- 3
      
    
    # Call jags from R 
    
    sim_res <- jags(data=bugs.data, inits=inits, parameters.to.save=c("phi1", "phi3", "f", "p1", "p2", "Ntot"), 
                    model.file="jagsMod_No_Counts.bug",n.chain=nchains, n.iter=niter, 
                    n.burnin=nburn, DIC=TRUE, working.directory=NULL, 
                    jags.seed = 123, refresh = niter/50, progress.bar = "text", digits=5)
    
    ## Saving in/output
    
    mod_phi3 <- sim_res$BUGSout$mean$phi3
    phi3_bias <- (sim_res$BUGSout$mean$phi3-temp_phi3)/temp_phi3
    mod_phi3_sd <- sim_res$BUGSout$sd$phi3
    
    mod_phi1 <- sim_res$BUGSout$mean$phi1
    phi1_bias <- (sim_res$BUGSout$mean$phi1-temp_phi1)/temp_phi1
    mod_phi1_sd <- sim_res$BUGSout$sd$phi1
    
    mod_f <- sim_res$BUGSout$mean$f
    f_bias <- (sim_res$BUGSout$mean$f-temp_f)/temp_f
    mod_f_sd <- sim_res$BUGSout$sd$f
    
    r_mod <- exp(summary(lm(log(sim_res$BUGSout$mean$Ntot)~seq(1:19)))$coef[2])
    r_real <- exp(summary(lm(log(Simulated_data$N_tot[12:30])~seq(1:19)))$coef[2])
    r_bias <- (r_mod-r_real)/r_real
    
    N_bias <- mean((sim_res$BUGSout$mean$Ntot-Simulated_data$N_tot[12:30])/Simulated_data$N_tot[12:30])
    N_last <- Simulated_data$N_tot[30]
    
    temp <- cbind(temp_phi3, mod_phi3, phi3_bias, mod_phi3_sd, temp_phi1, mod_phi1, phi1_bias, mod_phi1_sd,
                  temp_f, mod_f, f_bias, mod_f_sd, r_real, r_mod, r_bias, N_bias, N_last)
    
    Sens_results_no_bias <- rbind(Sens_results_no_bias, temp)
    
  }

Sens_results_no_bias <- as.data.frame(Sens_results_no_bias)

##############################################################################################################
##### SENSITIVITY ANALYSIS - ASSESSING BIAS IN "MINIMUM COUNT" DATA, BY MAKING REPEATED ESTIMATES OF 
#### POPULATION PARAMETERS FROM SIMULATED DATA.
#####  

library(R2jags)
source("Simulated_Data.R")


Sens_results_obsTot <- matrix(ncol=20, nrow=0)    

l <-  seq(0.5, 1, 0.05) # p.obs

for(i in l){
  for(j in 1:15){
    temp_phi1 <- sample(seq(0.85, 0.95, 0.01), 1)
    temp_f <- sample(seq(0.6, 0.95, 0.01), 1)
    temp_phi3 <- sample(seq(0.88, 0.95, 0.01), 1)
    mult <- sample(seq(1,5, 1), 1)
    
    
    
    Simulated_data <- SimPop(m=mult, p1=0.5, p2=0.5, bias1=1, bias2=1, bias3=1, PHI3=temp_phi3, mean_phi1=temp_phi1, 
                              mean_F=temp_f, error.dist="bin", p.obs=i)
    #Simulated_data <- SimPop6(p1=0.5, p2=0.5, bias1=1, PHI3=0.9, mean_phi1=0.9, mean_F=0.75)
    
    
    bugs.data <- list(SU = Simulated_data$SU[1,12:30],
                      J = Simulated_data$J[1,12:30],
                      H0f = Simulated_data$H[1,12:30],
                      H0m = Simulated_data$H[2,12:30],
                      H1f = Simulated_data$H[3,12:30],
                      H1m = Simulated_data$H[4,12:30],
                      Hadf = Simulated_data$H[5,12:30],
                      Hadm = Simulated_data$H[6,12:30],
                      T = 19,
                      y=round(Simulated_data$N_obs[12:30]),
                      C0=Simulated_data$C0[1,12:30],
                      Cm=Simulated_data$Cm[1,12:30],
                      Cf=Simulated_data$Cf[1,12:30])
    
    inits <- function() { list( phi3 = runif(1, 0.8, 0.99), 
                                phi1 = runif(1, 0.75, 0.95), 
                                f = runif(1, 0.5, 0.95),
                                p2 = runif(1, 0.5, 0.9),
                                p1 = runif(1, 0.4, 0.9),
                                N0f = Simulated_data$N[1, 12:30]*2, 
                                N0m = Simulated_data$N[2, 12:30]*2, 
                                N1f = Simulated_data$N[3, 12:30]*2,
                                N1m = Simulated_data$N[4, 12:30]*2,
                                Nadf = Simulated_data$N[5, 12:30]*2,
                                Nadm = Simulated_data$N[6, 12:30]*2) } 
    # MCMC settings
    niter <- 25000
    nthin <- 3
    nburn <- 15000
    nchains <- 3
    
    
    
    # Call jags from R 
    
    sim_res <- jags(data=bugs.data, inits=inits, parameters.to.save=c("phi1", "phi3", "f", "p1", "p2", "Ntot"), 
                    model.file="jagsMod_Ntot_poisson.bug",n.chain=nchains, n.iter=niter, 
                    n.burnin=nburn, DIC=TRUE, working.directory=NULL, 
                    jags.seed = 123, refresh = niter/50, progress.bar = "text", digits=5)
    
    ## Saving in/output
    
    mod_phi3 <- sim_res$BUGSout$mean$phi3
    phi3_bias <- (sim_res$BUGSout$mean$phi3-temp_phi3)/temp_phi3
    mod_phi3_sd <- sim_res$BUGSout$sd$phi3
    
    mod_phi1 <- sim_res$BUGSout$mean$phi1
    phi1_bias <- (sim_res$BUGSout$mean$phi1-temp_phi1)/temp_phi1
    mod_phi1_sd <- sim_res$BUGSout$sd$phi1
    
    mod_f <- sim_res$BUGSout$mean$f
    f_bias <- (sim_res$BUGSout$mean$f-temp_f)/temp_f
    mod_f_sd <- sim_res$BUGSout$sd$f
    
    r_mod <- exp(summary(lm(log(sim_res$BUGSout$mean$Ntot)~seq(1:19)))$coef[2])
    r_real <- exp(summary(lm(log(Simulated_data$N_tot[12:30])~seq(1:19)))$coef[2])
    r_bias <- (r_mod-r_real)/r_real
    
    N_bias <- mean((sim_res$BUGSout$mean$Ntot-Simulated_data$N_tot[12:30])/Simulated_data$N_tot[12:30])
    N_last <- Simulated_data$N_tot[30]
    p_bias <- ((i*0.5) - 0.5)/0.5
    
    temp <- cbind(i, mult, temp_phi3, mod_phi3, phi3_bias, mod_phi3_sd, temp_phi1, mod_phi1, phi1_bias, mod_phi1_sd,
                  temp_f, mod_f, f_bias, mod_f_sd, r_real, r_mod, r_bias, N_bias, N_last, p_bias)
    
    Sens_results_obsTot <- rbind(Sens_results_obsTot, temp)
    
  }
}


sens_results_obsTot <- as.data.frame(Sens_results_obsTot)

######################################################################################################################
##### SENSITIVITY ANALYSIS ASSESSING EFFECTS OF BIAS IN POPULATION STRUCTURE SURVEYS (p1 and p2). BASED ON REPEATED ESTIMATES OF 
##### POPULATION PARAMETERS FROM SIMULATED DATA.
##### 

library(R2jags)
source("Simulated_Data.R")
Sens_results_b123 <- matrix(ncol=22, nrow=0)    

bias_mat <- data.frame(b1=c(seq(0.7, 1.3, 0.05), rep(1,13), seq(0.7, 1.3, 0.05), rep(1,13)), 
                          b2=c(rep(1,13), seq(0.7, 1.3, 0.05), seq(0.7, 1.3, 0.05), rep(1,13)),
                          b3=c(rep(1,13), rep(1,13), rep(1,13), seq(0.7, 1.3, 0.05)),
                          cat=rep(1:4, each=13))

  
  for(i in 1:dim(bias_mat[1])){
    for(j in 1:5){
      temp_phi1 <- sample(seq(0.85, 0.95, 0.01), 1)
      temp_f <- sample(seq(0.6, 0.95, 0.01), 1)
      temp_phi3 <- sample(seq(0.88, 0.95, 0.01), 1)
      mult <- sample(seq(1,5, 1), 1)
      
      
  
  Simulated_data <- SimPop(m=mult, p1=0.5, p2=0.5, bias1=bias_mat$b1[i], bias2=bias_mat$b2[i], bias3=bias_mat$b3[i], PHI3=temp_phi3, mean_phi1=temp_phi1, mean_F=temp_f)
  

  bugs.data <- list(SU = Simulated_data$SU[1,12:30],
                     J = Simulated_data$J[1,12:30],
                     H0f = Simulated_data$H[1,12:30],
                     H0m = Simulated_data$H[2,12:30],
                     H1f = Simulated_data$H[3,12:30],
                     H1m = Simulated_data$H[4,12:30],
                     Hadf = Simulated_data$H[5,12:30],
                     Hadm = Simulated_data$H[6,12:30],
                     T = 19,
                     C0=Simulated_data$C0[1,12:30],
                     Cm=Simulated_data$Cm[1,12:30],
                     Cf=Simulated_data$Cf[1,12:30])

  inits <- function() { list( phi3 = runif(1, 0.8, 0.99), 
                             phi1 = runif(1, 0.75, 0.95), 
                             f = runif(1, 0.5, 0.95),
                             p2 = runif(1, 0.5, 0.9),
                             p1 = runif(1, 0.4, 0.9),
                             N0f = Simulated_data$N[1, 12:30]*2, 
                             N0m = Simulated_data$N[2, 12:30]*2, 
                             N1f = Simulated_data$N[3, 12:30]*2,
                             N1m = Simulated_data$N[4, 12:30]*2,
                             Nadf = Simulated_data$N[5, 12:30]*2,
                             Nadm = Simulated_data$N[6, 12:30]*2) } 
# MCMC settings
niter <- 25000
nthin <- 3
nburn <- 15000
nchains <- 3



# Call jags from R 
sim_res <- jags(data=bugs.data, inits=inits, parameters.to.save=c("phi1", "phi3", "f", "p1", "p2", "Ntot"), 
               model.file="jagsMod_No_Counts.bug",n.chain=nchains, n.iter=niter, 
               n.burnin=nburn, DIC=TRUE, working.directory=NULL, 
               jags.seed = 123, refresh = niter/50, progress.bar = "text", digits=5)

## Saving in/output

mod_phi3 <- sim_res$BUGSout$mean$phi3
phi3_bias <- (sim_res$BUGSout$mean$phi3-temp_phi3)/temp_phi3
mod_phi3_sd <- sim_res$BUGSout$sd$phi3

mod_phi1 <- sim_res$BUGSout$mean$phi1
phi1_bias <- (sim_res$BUGSout$mean$phi1-temp_phi1)/temp_phi1
mod_phi1_sd <- sim_res$BUGSout$sd$phi1

mod_f <- sim_res$BUGSout$mean$f
f_bias <- (sim_res$BUGSout$mean$f-temp_f)/temp_f
mod_f_sd <- sim_res$BUGSout$sd$f

r_mod <- exp(summary(lm(log(sim_res$BUGSout$mean$Ntot)~seq(1:19)))$coef[2])
r_real <- exp(summary(lm(log(Simulated_data$N_tot[12:30])~seq(1:19)))$coef[2])
r_bias <- (r_mod-r_real)/r_real

N_bias <- mean((sim_res$BUGSout$mean$Ntot-Simulated_data$N_tot[12:30])/Simulated_data$N_tot[12:30])
N_last <- Simulated_data$N_tot[30]

temp <- cbind(bias_mat$b1[i], bias_mat$b2[i], bias_mat$b3[i], bias_mat$cat[i], mult, temp_phi3, mod_phi3, phi3_bias, 
			  mod_phi3_sd, temp_phi1, mod_phi1, phi1_bias, mod_phi1_sd,
              temp_f, mod_f, f_bias, mod_f_sd, r_real, r_mod, r_bias, N_bias, N_last)

Sens_results_b123 <- rbind(Sens_results_b123, temp)
}
}

Sens_results_b123 <- as.data.frame(Sens_results_b123)
Sens_results_b123 <- Sens_results_b123 %>% rename(b1=V1, b2=V2, b3=V3, cat=V4)





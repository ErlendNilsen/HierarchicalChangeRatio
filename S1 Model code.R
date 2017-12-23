
#################################################################
### THIS SCRIPTFILE CONTAINS JAGS CODE FOR THE DIFFERNT VERSIONS OF THE INTEGRATED HIERARCHICAL CHANGE-IN-RATIO
### MODEL PRESENTED IN "Nilsen, EB & Strand, O. (2017). Integrating data from several sources for increased 
### insight into demographic processes: Simulation studies and proof of concept for hierarchical change in ratio models. PlosOne" 
############################################  

### M1: USED FOR THE EMPIRICAL ANALYSIS. ASSUME THAT UNBIASED "TOTAL COUNT DATA" 
### IS AVAILABLE. DEMOGRAPHIC RATE VARIAITON MODELLED USING LOGISTIC MIXED EFFECTS MODELS
###
### M2: USED FOR THE EMPIRICAL ANALYSIS. NO "TOTAL COUNT DATA" IS USED FOR THE MODELLING. 
### DEMOGRAPHIC RATE VARIAITON MODELLED USING LOGISTIC MIXED EFFECTS MODELS
###
### jagsMod_No_Counts: A SIMPLIFIED VERSION OF THE MODEL, USED IN SIMULATIONS. 
### ASSUMING CONSTANT DEMOGRAPHIC RATES. NO "TOTAL COUNT DATA" USED IN THE MODEL.
### 
### jagsMod_Ntot_poisson: A SIMPLIFIED VERSION OF THE MODEL, USED IN SIMULATIONS. 
### ASSUMING CONSTANT DEMOGRAPHIC RATES. "TOTAL COUNT DATA" USED IN THE MODEL.

###################################################################
###
### JAGS-model 'M1'

sink("jagsMod_M1.bug")
cat("
    model {
    
    
    ############################################################
    # Define the priors for the parameters
    ############################################################ 
    
    # Initial pre-harvest population sizes
    N0f[1] ~ dnorm(500, 0.0001)I(0,)     # calves
    N0m[1] ~ dnorm(500, 0.0001)I(0,)     # calves
    N1f[1]~ dnorm(300, 0.0001)I(0,)      # yearling females
    N1m[1]~ dnorm(300, 0.0001)I(0,)      # yearling males
    Nadf[1]~dnorm(500, 0.0001)I(0,)      # adult females
    Nadm[1]~dnorm(500, 0.0001)I(0,)      # adult females
    
    # Survival probabilites and productivity-estimates
    
    
    
    for(t in 1:T){
    p1[t]~ dunif(0.1,1)
    }
    
    for(t in 1:T){
    p2[t]~ dunif(0.1,1)
    }
    
    ## Vital rates
    phi3 ~ dunif(0, 1)      
    mean.f ~ dunif(-5,5)
    mean.phi1 ~ dunif(-5,5)
    
    tau_f <- 1/(sd_f*sd_f)
    sd_f ~ dunif(0,2)
    
    tau_phi1 <- 1/(sd_phi1*sd_phi1)
    sd_phi1 ~ dunif(0,2)
    
    
    ##############################
    # DERIVED PARAMETERS
    #############################
    
    fert <- exp(mean.f)/(1+exp(mean.f))
    Sjuv <- exp(mean.phi1)/(1+exp(mean.phi1))  
    
    
    #############################
    # SYSTEM PROCESS
    #############################
    
    
    for (t in 1:(T-1)){  
    
    logit(f[t]) <- mean.f+eps_f[t]
    eps_f[t]~dnorm(0, tau_f)
    
    logit(phi1[t]) <- mean.phi1+eps_phi1[t]
    eps_phi1[t]~dnorm(0, tau_phi1)
    
    
    N0f[t+1] ~dbin(phi1[t]*f[t]/2, max(50, Nadf[t+1]))
    N0m[t+1] ~dbin(phi1[t]*f[t]/2, max(50, Nadf[t+1]))
    
    N1f[t+1] ~ dbin(phi3, max(50, round(N0f[t]-H0f[t])))
    N1m[t+1] ~ dbin(phi3, max(50, round(N0m[t]-H0m[t])))
    
    Nadf[t+1] ~ dbin(phi3, max(50, round(N1f[t]+Nadf[t]-H1f[t]-Hadf[t])))
    Nadm[t+1] ~ dbin(phi3, max(50, round(N1m[t]+Nadm[t]-H1m[t]-Hadm[t])))
    
    }
    
    for (t in 1:T){
    Ntot[t] <- round(N0f[t] + N0m[t] + N1f[t] + N1m[t] + Nadf[t] + Nadm[t])      # summing up population vector to population size	
    } 
    
    for (t in 1:T){
    Xtot[t] <- round(N0f[t]+N0m[t]+N1f[t]+N1m[t]+Nadf[t]+Nadm[t]-H0f[t]-H0m[t]-H1f[t]-H1m[t]-Hadf[t]-Hadm[t])    	# summing up population vector to population size	
    }
    
    #################################################################
    # OBSERVATION PROCESS: COUNT DATA
    #################################################################
    
    for(t in 1:T){
    y[t]~dpois(Xtot[t])
    }
    
    ############################################################
    # OBSERVATION PROCESS: STRUCTURE SURVEY DATA
    ############################################################
    
    for (t in 1:(T-1)){
    
    J[t] ~ dbin(p1[t], round((N0f[t]+N0m[t])/phi1[t]))
    SU[t] ~ dbin(p1[t], round(N1m[t]+N1f[t]+Nadf[t]))
    
    C0[t] ~ dbin(p2[t], round(N0f[t]+N0m[t]-H0f[t]-H0m[t])) 
    Cf[t] ~ dbin(p2[t], round(N1f[t]+Nadf[t]-H1f[t]-Hadf[t]))
    Cm[t] ~ dbin(p2[t], round(N1m[t]+Nadm[t]-H1m[t]-Hadm[t]))
    }
    
    
    ############################################################
    ### Ending model; 
    ###########################################################
    
    } # End Model
    ",fill = TRUE)
sink()



################################################################
###
### JAGS-model 'M2'

sink("jagsMod_M2.bug")
cat("
    model {
    
    
    ############################################################
    # Define the priors for the parameters
    ############################################################ 
    
    # Initial pre-harvest population sizes
    N0f[1] ~ dnorm(500, 0.0001)I(0,)     # calves
    N0m[1] ~ dnorm(500, 0.0001)I(0,)     # calves
    N1f[1]~ dnorm(300, 0.0001)I(0,)      # yearling females
    N1m[1]~ dnorm(300, 0.0001)I(0,)      # yearling males
    Nadf[1]~dnorm(500, 0.0001)I(0,)      # adult females
    Nadm[1]~dnorm(500, 0.0001)I(0,)      # adult females
 
    
    for(t in 1:T){
    p1[t]~ dunif(0.1,1)
    }
    
    for(t in 1:T){
    p2[t]~ dunif(0.1,1)
    }
    
    ## Vital rates
    phi3 ~ dunif(0, 1)      
    mean.f ~ dunif(-5,5)
    mean.phi1 ~ dunif(-5,5)

    tau_f <- 1/(sd_f*sd_f)
    sd_f ~ dunif(0,2)

    tau_phi1 <- 1/(sd_phi1*sd_phi1)
    sd_phi1 ~ dunif(0,2)

       
    ##############################
    # DERIVED PARAMETERS
    #############################
    
    fert <- exp(mean.f)/(1+exp(mean.f))
    Sjuv <- exp(mean.phi1)/(1+exp(mean.phi1))  
    
    
    #############################
    # SYSTEM PROCESS
    #############################
    
    
    for (t in 1:(T-1)){  
    
    logit(f[t]) <- mean.f+eps_f[t]
    eps_f[t]~dnorm(0, tau_f)
    
    logit(phi1[t]) <- mean.phi1+eps_phi1[t]
    eps_phi1[t]~dnorm(0, tau_phi1)

    
    N0f[t+1] ~dbin(phi1[t]*f[t]/2, max(50, Nadf[t+1]))
    N0m[t+1] ~dbin(phi1[t]*f[t]/2, max(50, Nadf[t+1]))
    
    N1f[t+1] ~ dbin(phi3, max(50, round(N0f[t]-H0f[t])))
    N1m[t+1] ~ dbin(phi3, max(50, round(N0m[t]-H0m[t])))
    
    Nadf[t+1] ~ dbin(phi3, max(50, round(N1f[t]+Nadf[t]-H1f[t]-Hadf[t])))
    Nadm[t+1] ~ dbin(phi3, max(50, round(N1m[t]+Nadm[t]-H1m[t]-Hadm[t])))
    
    }
    
    for (t in 1:T){
    Ntot[t] <- round(N0f[t] + N0m[t] + N1f[t] + N1m[t] + Nadf[t] + Nadm[t])  		# summing up population vector to population size	
    } 
    
    for (t in 1:T){
    Xtot[t] <- round(N0f[t]+N0m[t]+N1f[t]+N1m[t]+Nadf[t]+Nadm[t]-H0f[t]-H0m[t]-H1f[t]-H1m[t]-Hadf[t]-Hadm[t])    	# summing up population vector to population size	
    }
    
    #################################################################
    # OBSERVATION PROCESS: COUNT DATA
    #################################################################
    
    
    ############################################################
    # OBSERVATION PROCESS: STRUCTURE SURVEY DATA
    ############################################################
    
    for (t in 1:(T-1)){
    
    J[t] ~ dbin(p1[t], round((N0f[t]+N0m[t])/phi1[t]))
    SU[t] ~ dbin(p1[t], round(N1m[t]+N1f[t]+Nadf[t]))
    
    C0[t] ~ dbin(p2[t], round(N0f[t]+N0m[t]-H0f[t]-H0m[t])) 
    Cf[t] ~ dbin(p2[t], round(N1f[t]+Nadf[t]-H1f[t]-Hadf[t]))
    Cm[t] ~ dbin(p2[t], round(N1m[t]+Nadm[t]-H1m[t]-Hadm[t]))
    }
    
    
    ############################################################
    ### Ending model; 
    ############################################################
    
    } # End Model
    ",fill = TRUE)
sink()

#################################################################
###
### JAGS-model 'jagsMod_No_Counts'

sink("jagsMod_No_Counts.bug")
cat("
    model {
    
    
    ############################################################
    # 1. Define the priors for the parameters
    ############################################################ 
    
    # Initial pre-harvest population sizes
    N0f[1] ~ dnorm(500, 0.0001)I(0,)     # calves
    N0m[1] ~ dnorm(500, 0.0001)I(0,)     # calves
    N1f[1]~ dnorm(300, 0.0001)I(0,)      # yearling females
    N1m[1]~ dnorm(300, 0.0001)I(0,)      # yearling males
    Nadf[1]~dnorm(500, 0.0001)I(0,)      # adult females
    Nadm[1]~dnorm(500, 0.0001)I(0,)      # adult females
    
    # Survival probabilites and productivity-estimates
    
    phi3 ~ dunif(0, 1)				
    phi1 ~ dunif(0,1)
    f ~ dunif(0, 1)			
    p1~ dunif(0,1)
    p2~ dunif(0,1)
    
    
    
    
    #############################
    # 3.1.1 System process
    #############################
    
    
    for (t in 1:(T-1)){  
    
    
    
    N0f[t+1] ~dbin(phi1*f/2, max(20, Nadf[t+1]))
    N0m[t+1] ~dbin(phi1*f/2, max(20, Nadf[t+1]))
    
    N1f[t+1] ~ dbin(phi3, max(20, round(N0f[t]-H0f[t])))
    N1m[t+1] ~ dbin(phi3, max(20, round(N0m[t]-H0m[t])))
    
    Nadf[t+1] ~ dbin(phi3, max(20, round(N1f[t]+Nadf[t]-H1f[t]-Hadf[t])))
    Nadm[t+1] ~ dbin(phi3, max(20, round(N1m[t]+Nadm[t]-H1m[t]-Hadm[t])))
    
    }
    
    for (t in 1:T){
    Ntot[t] <- round(N0f[t] + N0m[t] + N1f[t] + N1m[t] + Nadf[t] + Nadm[t])  		# summing up population vector to population size	
    } 
    
    #################################################################
    # OBSERVATION PROCESS: COUNT DATA
    #################################################################
    
   
    ############################################################
    # 3.2. Likelihood for reproductive data: binomial obs. prob model
    ############################################################
    
    for (t in 1:T){
    
    J[t] ~ dbin(p1, round((N0f[t]+N0m[t])/phi1))
    SU[t] ~ dbin(p1, round(N1m[t]+N1f[t]+Nadf[t]))
    
    C0[t] ~ dbin(p2, round(N0f[t]+N0m[t]-H0f[t]-H0m[t])) 
    Cf[t] ~ dbin(p2, round(N1f[t]+Nadf[t]-H1f[t]-Hadf[t]))
    Cm[t] ~ dbin(p2, round(N1m[t]+Nadm[t]-H1m[t]-Hadm[t]))
    }
    
    
    ############################################################
    ### Ending model; 
    ###########################################################
    
    } # End Model
    ",fill = TRUE)
sink()


#################################################################
###
### JAGS-model 'jagsMod_Ntot_poisson'

sink("jagsMod_Ntot_poisson.bug")
cat("
    model {
    
    
    ############################################################
    # 1. Define the priors for the parameters
    ############################################################ 
    
    # Initial pre-harvest population sizes
    N0f[1] ~ dnorm(500, 0.0001)I(0,)     # calves
    N0m[1] ~ dnorm(500, 0.0001)I(0,)     # calves
    N1f[1]~ dnorm(300, 0.0001)I(0,)      # yearling females
    N1m[1]~ dnorm(300, 0.0001)I(0,)      # yearling males
    Nadf[1]~dnorm(500, 0.0001)I(0,)      # adult females
    Nadm[1]~dnorm(500, 0.0001)I(0,)	    # adult females
    
    # Survival probabilites and productivity-estimates
    
    phi3 ~ dunif(0, 1)				
    phi1 ~ dunif(0,1)
    f ~ dunif(0, 1)			
    p1~ dunif(0,1)
    p2~ dunif(0,1)
    
    
    
    
    #############################
    # SYSTEM PROCESS
    #############################
    
    
    for (t in 1:(T-1)){  
    
    
    
    N0f[t+1] ~dbin(phi1*f/2, max(50, Nadf[t+1]))
    N0m[t+1] ~dbin(phi1*f/2, max(50, Nadf[t+1]))
    
    N1f[t+1] ~ dbin(phi3, max(50, round(N0f[t]-H0f[t])))
    N1m[t+1] ~ dbin(phi3, max(50, round(N0m[t]-H0m[t])))
    
    Nadf[t+1] ~ dbin(phi3, max(50, round(N1f[t]+Nadf[t]-H1f[t]-Hadf[t])))
    Nadm[t+1] ~ dbin(phi3, max(50, round(N1m[t]+Nadm[t]-H1m[t]-Hadm[t])))
    
    }
    
    for (t in 1:T){
    Ntot[t] <- round(N0f[t] + N0m[t] + N1f[t] + N1m[t] + Nadf[t] + Nadm[t])  		# summing up population vector to population size	
    } 

    #################################################################
    # OBSERVATION PROCESS: COUNT DATA
    #################################################################

    for(t in 1:T){
    y[t]~dpois(Ntot[t])
                 }
    
    ############################################################
    # 3.2. Likelihood for reproductive data: binomial obs. prob model
    ############################################################
    
    for (t in 1:T){
    
    J[t] ~ dbin(p1, round((N0f[t]+N0m[t])/phi1))
    SU[t] ~ dbin(p1, round(N1m[t]+N1f[t]+Nadf[t]))
    
    C0[t] ~ dbin(p2, round(N0f[t]+N0m[t]-H0f[t]-H0m[t])) 
    Cf[t] ~ dbin(p2, round(N1f[t]+Nadf[t]-H1f[t]-Hadf[t]))
    Cm[t] ~ dbin(p2, round(N1m[t]+Nadm[t]-H1m[t]-Hadm[t]))
    }
    
    
    ############################################################
    ### Ending model; 
    ###########################################################
    
    } # End Model
    ",fill = TRUE)
sink()





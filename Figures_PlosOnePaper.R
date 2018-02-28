

library(tidyverse)


################################################################################
################################################################################
### FIGURE 2 IN MANUSCRIPT; 


MM <- Sens_results_no_bias %>% select(f_bias, phi1_bias, phi3_bias, r_bias, N_bias) %>%
  reshape(timevar="B_param", 
          varying=c("f_bias", "phi1_bias", "phi3_bias", "r_bias", "N_bias"),
          times=c("f_bias", "phi1_bias", "phi3_bias", "r_bias", "N_bias"),
          v.names = "B_value",
          direction="long")
## Boxplot; 


par(bty="l")
boxplot(B_value~B_param, data=MM, ylim=c(-0.1, 0.25), notch=T, boxwex=0.3, col="orange2", ylab="Bias", border="grey10", xaxt="n")
axis(side=1, at=c(1,2,3,4,5), labels=expression("f", "N", Phi[1], Phi[3], "r"), cex=1.1)
abline(h=0, lty=3) 

################################################################################
###### PLOTTING - FIGURE 3 IN MANUSCRIPT; 


par(mfrow=c(5,1), cex=0.75)

par(mar=c(2,4,1,1))
plot(jitter(sens_results_obsTot$i), sens_results_obsTot$f_bias, pch=1, col="orange2", 
     ylab="Bias in f", xlab="", ylim=c(-0.4, 0.4))
abline(h=0, lty=3)

par(mar=c(2,4,1,1))
plot(jitter(sens_results_obsTot$i), sens_results_obsTot$phi1_bias, pch=1, col="orange2", 
     ylab=expression("Bias in"~phi[1]), xlab="", ylim=c(-0.4, 0.4))
abline(h=0, lty=3)

par(mar=c(2,4,1,1))
plot(jitter(sens_results_obsTot$i), sens_results_obsTot$phi3_bias, pch=1, col="orange2", 
     ylab=expression("Bias in"~phi[2]), xlab="", ylim=c(-0.4, 0.4))
abline(h=0, lty=3)

par(mar=c(2,4,1,1))
plot(jitter(sens_results_obsTot$i), sens_results_obsTot$r_bias, pch=1, col="orange2", 
     ylab="Bias in r", xlab="", ylim=c(-0.4, 0.4))
abline(h=0, lty=3)

par(mar=c(2,4,1,1))
plot(jitter(sens_results_obsTot$i), sens_results_obsTot$N_bias, pch=1, col="orange2", 
     ylab="Bias in N", xlab="", ylim=c(-0.4, 0.4))
abline(h=0, lty=3)

################################################################################
################################################################################
#### PLOTTING FIGURE 4 IN MANUSCRIPT 
#### Assuming bias in p1 and/or p2

Sens_results_b123 <- Sens_results_b123 %>% rename(b1=V1, b2=V2, b3=V3, cat=V4)


Res_p1 <- filter(Sens_results_b123, cat==1)        # Females in pre-harvest
Res_p2 <- filter(Sens_results_b123, cat==2)        # Females in post-harvest
Res_p1_p2 <- filter(Sens_results_b123, cat==3)     # Females in post- and pre-harvest
Res_p2b <- filter(Sens_results_b123, cat==4)       # Males in post-harvest


##### Plotting without polygon (bias+-5%)

par(mfrow=c(5,4), oma=c(1,3,2,1))

## F-bias
par(mar=c(1,2,2,0))
plot(jitter(Res_p1$b1), Res_p1$f_bias, pch=16, col="orange2", ylab="", xlab="", ylim=c(-0.4, 0.4), yaxt="n")
axis(side=2, at=c(-0.4, -0.2, 0.0, 0.2, 0.4))
abline(v=0, lty=3)
abline(h=0, lty=3)

par(mar=c(1,2,2,0))
plot(jitter(Res_p2$b2), Res_p2$f_bias, pch=16, col="orange2", ylab="", xlab="", ylim=c(-0.4, 0.4), yaxt="n")
axis(side=2, at=c(-0.4, -0.2, 0.0, 0.2, 0.4), labels=F)
abline(v=0, lty=3)
abline(h=0, lty=3)

par(mar=c(1,2,2,0))
plot(jitter(Res_p1_p2$b1), Res_p1_p2$f_bias, pch=16, col="orange2", ylab="", xlab="", ylim=c(-0.4, 0.4), yaxt="n")
axis(side=2, at=c(-0.4, -0.2, 0.0, 0.2, 0.4), labels=F)
abline(v=0, lty=3)
abline(h=0, lty=3)

par(mar=c(1,2,2,0))
plot(jitter(Res_p2b$b3), Res_p2b$f_bias, pch=16, col="orange2", ylab="", xlab="", ylim=c(-0.4, 0.4), yaxt="n")
axis(side=2, at=c(-0.4, -0.2, 0.0, 0.2, 0.4), labels=F)
abline(v=0, lty=3)
abline(h=0, lty=3)
mtext("Bias in f", outer=T, side=2, adj=0.92, cex=1, line=1)
mtext(c("I", "II", "III", "IV"), outer=T, side=3, adj=c(0.14, 0.39, 0.64, 0.9), cex=1.2, line=0)



## phi1-bias
par(mar=c(1,2,2,0))
plot(jitter(Res_p1$b1), Res_p1$phi1_bias, pch=1, col="orange2", ylab="", xlab="", ylim=c(-0.4, 0.4), yaxt="n")
axis(side=2, at=c(-0.4, -0.2, 0.0, 0.2, 0.4))
abline(v=0, lty=3)
abline(h=0, lty=3)

par(mar=c(1,2,2,0))
plot(jitter(Res_p2$b2), Res_p2$phi1_bias, pch=1, col="orange2", ylab="", xlab="", ylim=c(-0.4, 0.4), yaxt="n")
axis(side=2, at=c(-0.4, -0.2, 0.0, 0.2, 0.4), labels=F)
abline(v=0, lty=3)
abline(h=0, lty=3)

par(mar=c(1,2,2,0))
plot(jitter(Res_p1_p2$b1), Res_p1_p2$phi1_bias, pch=1, col="orange2", ylab="", xlab="", ylim=c(-0.4, 0.4), yaxt="n")
axis(side=2, at=c(-0.4, -0.2, 0.0, 0.2, 0.4), labels=F)
abline(v=0, lty=3)
abline(h=0, lty=3)

par(mar=c(1,2,2,0))
plot(jitter(Res_p2b$b3), Res_p2b$phi1_bias, pch=1, col="orange2", ylab="", xlab="", ylim=c(-0.4, 0.4), yaxt="n")
axis(side=2, at=c(-0.4, -0.2, 0.0, 0.2, 0.4), labels=F)
abline(v=0, lty=3)
abline(h=0, lty=3)
mtext(text=expression(Bias~"in"~phi[1]), outer=T, side=2, adj=0.72, cex=1, line=1)



## phi2-bias
par(mar=c(1,2,2,0))
plot(jitter(Res_p1$b1), Res_p1$phi3_bias, pch=1, col="orange2", ylab="", xlab="", ylim=c(-0.4, 0.4), yaxt="n")
axis(side=2, at=c(-0.4, -0.2, 0.0, 0.2, 0.4))
abline(v=0, lty=3)
abline(h=0, lty=3)

par(mar=c(1,2,2,0))
plot(jitter(Res_p2$b2), Res_p2$phi3_bias, pch=1, col="orange2", ylab="", xlab="", ylim=c(-0.4, 0.4), yaxt="n")
axis(side=2, at=c(-0.4, -0.2, 0.0, 0.2, 0.4), labels=F)
abline(v=0, lty=3)
abline(h=0, lty=3)

par(mar=c(1,2,2,0))
plot(jitter(Res_p1_p2$b1), Res_p1_p2$phi3_bias, pch=1, col="orange2", ylab="", xlab="", ylim=c(-0.4, 0.4), yaxt="n")
axis(side=2, at=c(-0.4, -0.2, 0.0, 0.2, 0.4), labels=F)
abline(v=0, lty=3)
abline(h=0, lty=3)

par(mar=c(1,2,2,0))
plot(jitter(Res_p2b$b3), Res_p2b$phi3_bias, pch=1, col="orange2", ylab="", xlab="", ylim=c(-0.4, 0.4), yaxt="n")
axis(side=2, at=c(-0.4, -0.2, 0.0, 0.2, 0.4), labels=F)
abline(v=0, lty=3)
abline(h=0, lty=3)
mtext(text=expression(Bias~"in"~phi[2]), outer=T, side=2, adj=0.50, cex=1, line=1)


## r-bias
par(mar=c(1,2,2,0))
plot(jitter(Res_p1$b1), Res_p1$r_bias, pch=1, col="orange2", ylab="", xlab="", ylim=c(-0.4, 0.4), yaxt="n")
axis(side=2, at=c(-0.4, -0.2, 0.0, 0.2, 0.4))
abline(v=0, lty=3)
abline(h=0, lty=3)

par(mar=c(1,2,2,0))
plot(jitter(Res_p2$b2), Res_p2$r_bias, pch=1, col="orange2", ylab="", xlab="", ylim=c(-0.4, 0.4), yaxt="n")
axis(side=2, at=c(-0.4, -0.2, 0.0, 0.2, 0.4), labels=F)
abline(v=0, lty=3)
abline(h=0, lty=3)

par(mar=c(1,2,2,0))
plot(jitter(Res_p1_p2$b1), Res_p1_p2$r_bias, pch=1, col="orange2", ylab="", xlab="", ylim=c(-0.4, 0.4), yaxt="n")
axis(side=2, at=c(-0.4, -0.2, 0.0, 0.2, 0.4), labels=F)
abline(v=0, lty=3)
abline(h=0, lty=3)

par(mar=c(1,2,2,0))
plot(jitter(Res_p2b$b3), Res_p2b$r_bias, pch=1, col="orange2", ylab="", xlab="", ylim=c(-0.4, 0.4), yaxt="n")
axis(side=2, at=c(-0.4, -0.2, 0.0, 0.2, 0.4), labels=F)
abline(v=0, lty=3)
abline(h=0, lty=3)
mtext("Bias in r", outer=T, side=2, adj=0.28, cex=1, line=1)


## N-bias
par(mar=c(1,2,2,0))
plot(jitter(Res_p1$b1), Res_p1$N_bias, pch=1, col="orange2", ylab="", xlab="", ylim=c(-0.4, 0.4), yaxt="n")
axis(side=2, at=c(-0.4, -0.2, 0.0, 0.2, 0.4))
abline(v=0, lty=3)
abline(h=0, lty=3)

par(mar=c(1,2,2,0))
plot(jitter(Res_p2$b2), Res_p2$N_bias, pch=1, col="orange2", ylab="", xlab="", ylim=c(-0.4, 0.4), yaxt="n")
axis(side=2, at=c(-0.4, -0.2, 0.0, 0.2, 0.4), labels=F)
abline(v=0, lty=3)
abline(h=0, lty=3)

par(mar=c(1,2,2,0))
plot(jitter(Res_p1_p2$b1), Res_p1_p2$N_bias, pch=1, col="orange2", ylab="", xlab="", ylim=c(-0.4, 0.4), yaxt="n")
axis(side=2, at=c(-0.4, -0.2, 0.0, 0.2, 0.4), labels=F)
abline(v=0, lty=3)
abline(h=0, lty=3)

par(mar=c(1,2,2,0))
plot(jitter(Res_p2b$b3), Res_p2b$N_bias, pch=1, col="orange2", ylab="", xlab="", ylim=c(-0.4, 0.4), yaxt="n")
axis(side=2, at=c(-0.4, -0.2, 0.0, 0.2, 0.4), labels=F)
abline(v=0, lty=3)
abline(h=0, lty=3)
mtext("Bias in N", outer=T, side=2, adj=0.05, cex=1, line=1)

############################################################################
############################################################################
###### PLOTTING - FIGURE 5 IN MANUSCRIPT; 

par(mfrow=c(2,3))

firstYear=1992
lastYear=2012
nyears <- seq(firstYear, lastYear)

##########
# SN?HETTA - M1

# Plotting time series of X
mean <- upper <- lower <- numeric()
for(i in 1:length(nyears)){
  upper[i] <- quantile(Sno_M1$BUGSout$sims.list$Xtot[,i], 0.975)
  lower[i] <- quantile(Sno_M1$BUGSout$sims.list$Xtot[,i], 0.025)
  mean[i] <- mean(Sno_M1$BUGSout$sims.list$Xtot[,i])
}

par(bty="l", mar=c(4,4,3,1), cex=0.8)
plot(nyears,mean, type="l", col="dark red", lwd=1, lty=1, ylim=c(0,3500), ylab="Population size (X[t])", 
     xlab="Year", xlim=c(1990, 2015))
lines(nyears,mean, type="p", col="dark red", pch=16)
arrows(x0=nyears, x1=nyears, y0=lower, y1=upper, angle=0)
arrows(x0=nyears, x1=nyears, y1=lower, y0=upper, angle=0)

points(x=seq(1992,2012), y=ds$N[2:22], pch=16, col="dark orange")
mtext("SN?HETTA", outer=F, adj=0.01, line=1)

# Plotting time series of f

firstYear=1992
lastYear=2012
nyears <- seq(firstYear, lastYear)

mean <- upper <- lower <- numeric()
for(i in 1:length(nyears)){
  upper[i] <- quantile(Sno_M1$BUGSout$sims.list$f[,i], 0.975)
  lower[i] <- quantile(Sno_M1$BUGSout$sims.list$f[,i], 0.025)
  mean[i] <- mean(Sno_M1$BUGSout$sims.list$f[,i])
}

par(bty="l", mar=c(4,4,3,1), cex=0.8)
plot(nyears,mean, type="l", col="dark red", lwd=1, lty=1, ylim=c(0,1), ylab="Fecundity (f)", 
     xlim=c(1990, 2015), xlab="Year")
lines(nyears,mean, type="p", col="dark red", pch=16)
arrows(x0=nyears, x1=nyears, y0=lower, y1=upper, angle=0)
arrows(x0=nyears, x1=nyears, y1=lower, y0=upper, angle=0)
abline(h=mean(Sno_M1$BUGSout$sims.list$fert), lty=3, col="dark grey", lwd=2)


# Plotting time series of phi1

mean <- upper <- lower <- numeric()
for(i in 1:length(nyears)){
  upper[i] <- quantile(Sno_M1$BUGSout$sims.list$phi1[,i], 0.975)
  lower[i] <- quantile(Sno_M1$BUGSout$sims.list$phi1[,i], 0.025)
  mean[i] <- mean(Sno_M1$BUGSout$sims.list$phi1[,i])
}

par(bty="l", mar=c(4,4,3,1), cex=0.8)
plot(nyears,mean, type="l", col="dark red", lwd=1, lty=1, ylim=c(0,1), ylab="Juvenile survival", 
     xlab="Year", xlim=c(1990, 2015))
lines(nyears,mean, type="p", col="dark red", pch=16)
arrows(x0=nyears, x1=nyears, y0=lower, y1=upper, angle=0)
arrows(x0=nyears, x1=nyears, y1=lower, y0=upper, angle=0)
abline(h=mean(Sno_M1$BUGSout$sims.list$Sjuv), lty=3, col="dark grey", lwd=2)


##########
# KNUTSH? - M1

# Plotting time series of X
mean <- upper <- lower <- numeric()
for(i in 1:length(nyears)){
  upper[i] <- quantile(Knu_M1$BUGSout$sims.list$Xtot[,i], 0.975)
  lower[i] <- quantile(Knu_M1$BUGSout$sims.list$Xtot[,i], 0.025)
  mean[i] <- mean(Knu_M1$BUGSout$sims.list$Xtot[,i])
}

par(bty="l", mar=c(4,4,3,1), cex=0.8)
plot(nyears,mean, type="l", col="dark red", lwd=1, lty=1, ylim=c(0,3500), ylab="Population size (X[t])", 
     xlab="Year", xlim=c(1990, 2015))
lines(nyears,mean, type="p", col="dark red", pch=16)
arrows(x0=nyears, x1=nyears, y0=lower, y1=upper, angle=0)
arrows(x0=nyears, x1=nyears, y1=lower, y0=upper, angle=0)

points(x=seq(1992,2012), y=dk$N[2:22], pch=16, col="dark orange")
mtext("KNUTSH?", outer=F, adj=0.01, line=1)

# Plotting time series of f

firstYear=1992
lastYear=2012
nyears <- seq(firstYear, lastYear)

mean <- upper <- lower <- numeric()
for(i in 1:length(nyears)){
  upper[i] <- quantile(Knu_M1$BUGSout$sims.list$f[,i], 0.975)
  lower[i] <- quantile(Knu_M1$BUGSout$sims.list$f[,i], 0.025)
  mean[i] <- mean(Knu_M1$BUGSout$sims.list$f[,i])
}

par(bty="l", mar=c(4,4,3,1), cex=0.8)
plot(nyears,mean, type="l", col="dark red", lwd=1, lty=1, ylim=c(0,1), ylab="Fecundity (f)", 
     xlim=c(1990, 2015), xlab="Year")
lines(nyears,mean, type="p", col="dark red", pch=16)
arrows(x0=nyears, x1=nyears, y0=lower, y1=upper, angle=0)
arrows(x0=nyears, x1=nyears, y1=lower, y0=upper, angle=0)
abline(h=mean(Sno_M1$BUGSout$sims.list$fert), lty=3, col="dark grey", lwd=2)


# Plotting time series of phi1

mean <- upper <- lower <- numeric()
for(i in 1:length(nyears)){
  upper[i] <- quantile(Knu_M1$BUGSout$sims.list$phi1[,i], 0.975)
  lower[i] <- quantile(Knu_M1$BUGSout$sims.list$phi1[,i], 0.025)
  mean[i] <- mean(Knu_M1$BUGSout$sims.list$phi1[,i])
}

par(bty="l", mar=c(4,4,3,1), cex=0.8)
plot(nyears,mean, type="l", col="dark red", lwd=1, lty=1, ylim=c(0,1), ylab="Juvenile survival", 
     xlab="Year", xlim=c(1990, 2015))
lines(nyears,mean, type="p", col="dark red", pch=16)
arrows(x0=nyears, x1=nyears, y0=lower, y1=upper, angle=0)
arrows(x0=nyears, x1=nyears, y1=lower, y0=upper, angle=0)
abline(h=mean(Knu_M1$BUGSout$sims.list$Sjuv), lty=3, col="dark grey", lwd=2)

######################################################################################################
######################################################################################################
#### PLOTTING - FIGURE 6 IN MANUSCRIPT 
nedre=0.5

par(bty="l", xaxt="n")

### PHI3
phi3_s1 <- quantile(Sno_M1$BUGSout$sims.list$phi3, probs=c(0.025, 0.5, 0.975))
phi3_s2 <- quantile(Sno_M2$BUGSout$sims.list$phi3, probs=c(0.025, 0.5, 0.975))
phi3_k1 <- quantile(Knu_M1$BUGSout$sims.list$phi3, probs=c(0.025, 0.5, 0.975))
phi3_k2 <- quantile(Knu_M2$BUGSout$sims.list$phi3, probs=c(0.025, 0.5, 0.975))

plot(c(1,2,5,6), c(phi3_s1[2], phi3_s2[2], phi3_k1[2], phi3_k2 [2]), pch=16, 
     ylim=c(nedre,1), xlim=c(0, 30), xlab="", ylab="Probability", 
     col=c("dark red", "dark blue", "orange2", "grey10" ))
arrows(x0=c(1,2,5,6), x1=c(1,2,5,6), y0=c(phi3_s1[3], phi3_s2[3], phi3_k1[3], phi3_k2 [3]), 
       y1=c(phi3_s1[1], phi3_s2[1], phi3_k1[1], phi3_k2 [1]), angle=90, length=0.04, code=3, 
       col=c("dark red", "dark blue", "orange2", "grey10" ))

mtext(side=3, expression(phi[3]), adj=0.14, cex=0.8)
mtext(side=3, expression(phi[1]), adj=0.45, cex=0.8)
mtext(side=3, "f", adj=0.76, cex=0.8)

### PHI1
phi1_s1 <- quantile(Sno_M1$BUGSout$sims.list$Sjuv, probs=c(0.025, 0.5, 0.975))
phi1_s2 <- quantile(Sno_M2$BUGSout$sims.list$Sjuv, probs=c(0.025, 0.5, 0.975))
phi1_k1 <- quantile(Knu_M1$BUGSout$sims.list$Sjuv, probs=c(0.025, 0.5, 0.975))
phi1_k2 <- quantile(Knu_M2$BUGSout$sims.list$Sjuv, probs=c(0.025, 0.5, 0.975))

points(c(11, 12, 15, 16), c(phi1_s1[2], phi1_s2[2], phi1_k1[2], phi1_k2 [2]), pch=16, col=c("dark red", "dark blue", "orange2", "grey10" ))
arrows(x0=c(11, 12, 15, 16), x1=c(11, 12, 15, 16), y0=c(phi1_s1[3], phi1_s2[3], phi1_k1[3], phi1_k2 [3]), 
       y1=c(phi1_s1[1], phi1_s2[1], phi1_k1[1], phi1_k2 [1]), angle=90, length=0.04, code=3, 
       col=c("dark red", "dark blue", "orange2", "grey10" ))

### f
f_s1 <- quantile(Sno_M1$BUGSout$sims.list$fert, probs=c(0.025, 0.5, 0.975))
f_s2 <- quantile(Sno_M2$BUGSout$sims.list$fert, probs=c(0.025, 0.5, 0.975))
f_k1 <- quantile(Knu_M1$BUGSout$sims.list$fert, probs=c(0.025, 0.5, 0.975))
f_k2 <- quantile(Knu_M2$BUGSout$sims.list$fert, probs=c(0.025, 0.5, 0.975))

points(c(21, 22, 25, 26), c(f_s1[2], f_s2[2], f_k1[2], f_k2[2]), pch=16, col=c("dark red", "dark blue", "orange2", "grey10" ))
arrows(x0=c(21, 22, 25, 26), x1=c(21, 22, 25, 26), y0=c(f_s1[1], f_s2[1], f_k1[1], f_k2[1]), 
       y1=c(f_s1[3], f_s2[3], f_k1[3], f_k2[3]), angle=90, length=0.04, code=3, 
       col=c("dark red", "dark blue", "orange2", "grey10" ))

legend(seg.len=4, "bottomleft", c("Sn?hetta_M1", "Sn?hetta_M2", "Knutsh?_M1", "Knutsh?_M2"), col=c("dark red", "dark blue", "orange2", "grey10" ), lty=1, pch=16,  bty="n", cex=0.8)
lines(c(3.5, 3.5), c(0.8, 1), lty=3)
lines(c(13.5, 13.5), c(0.8, 1), lty=3)
lines(c(23.5, 23.5), c(0.5, 1), lty=3)


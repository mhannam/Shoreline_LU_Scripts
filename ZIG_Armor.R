# We model SAV area as a gamma dist variable, composed of the product of the potential habitat
# and the expected proportion occupied. We model that proportion with a logit-linked linear model
#
data{
  #z[i]
  #ones[i]
}
model{
  #for the ones trick:
    c <- 100000
    
  for (i in 1:NSites) {
    # Presence
    w[i] <- ilogit(mu_SE_PA[SubEst[i]] + B_BH_PA * BH[i] + B_RR_PA * RR[i] +
                     B_BH_Sal_PA[Sal[i]]*BH[i] + B_RR_Sal_PA[Sal[i]]*RR[i])
    
    # Abundance
    logGamma[i]    <- log(dgamma(SAV[i], lambdaObs[i], rObs[i]))
    
    lambdaObs[i]   <- SAV_hat[i]^2 / var_y_hat    
    rObs[i]        <- SAV_hat[i] / var_y_hat
    
    SAV_hat[i] <- Prop_adj[i]*Hab[i] 
    Prop_adj[i] <- min(max(.1,Prop[i]), .9)
    Prop[i] <- ilogit(mu_SE[SubEst[i]] + B_BH * BH[i] + B_RR * RR[i]+
                        B_BH_Sal[Sal[i]]*BH[i] + B_RR_Sal[Sal[i]]*RR[i])
    
    # define the total likelihood, where the likelihood is (1 - w) if y = 0 (z = 0) or
    # the likelihood is w * gammalik if y >= 0.0001 (z = 1). So if z = 1, then the first bit must be
    # z[i] is 0 if y = 0, and 1 if y > 0
    
    logLik[i] <- (1 - z[i]) * log(1 - w[i]) + z[i] * (log(w[i]) + logGamma[i]) 
    Lik[i]    <- exp(logLik[i])
    
    
    # Use the ones trick, p[i] must fall btw 0 and 1, so divide by a very large number (C)
    # this scaling doesn't affect the outcome b/c MCMC works on the relative scale
    # The likelihood of a 1 drawn from a bernoulli dist with p, is p
    p[i]    <- Lik[i] / c 
    ones[i] ~ dbern(p[i])
  }
  maxLik = max(Lik[])
    
  for (j in 1:NSubEst) {
    #Prop_SE ~ dbeta(Prop_RS[RivSys[j]])
    Prop_SE[j] <- ilogit(mu_SE[j]) # a derived proportion for
    mu_SE[j]    ~ dnorm(mu_CB, tau_SE)#yhat_SE[j], tau_SE)
    #yhat_SE[j] <- mu_RS[SubRivSys[j]]# + B_Sal[SubSal[j]] + 
     # B_SubBH * SubBH[j] + B_SubRR * SubRR[j]# + Landcover + PctArmor 
    
    mu_SE_PA[j] ~ dnorm(mu_CB_PA, tau_SE_PA)#yhat_SE_PA[j], tau_SE_PA)
    #yhat_SE_PA[j] <- mu_RS_PA[SubRivSys[j]] #+ B_Sal_PA[SubSal[j]] +
     # B_SubBH_PA * SubBH[j] + B_SubRR_PA * SubRR[j]# + Landcover + PctArmor 
  }
  
  # for (k in 1:NRivSys) {
  #   #Prop_RS[k] ~ dbeta(Prop) 
  #   Prop_RS[k] <- ilogit(mu_RS[k])
  #   mu_RS[k]    ~ dnorm(yhat_RS[k], tau_RS) # <- yhat_RS[k] #
  #   yhat_RS[k] <- mu_CB # + landcover?
  #   
  #   mu_RS_PA[k]    ~ dnorm(yhat_RS_PA[k], tau_RS_PA)
  #   yhat_RS_PA[k] <- mu_CB_PA
  # }
  
  var_y_hat <- sig_St^-2
  sig_St    ~ dunif(0,100)
  tau_RS <- sig_RS^-2
  sig_RS  ~ dunif(0,100)
  tau_SE <- sig_SE^-2
  sig_SE  ~ dunif(0,100)
  
  tau_RS_PA <- sig_RS_PA^-2
  sig_RS_PA  ~ dunif(0,100)
  tau_SE_PA <- sig_SE_PA^-2
  sig_SE_PA  ~ dunif(0,100)
  
  B_BH ~ dnorm(0, .001)
  B_RR ~ dnorm(0, .001)
  B_SubBH ~ dnorm(0, .001)
  B_SubRR ~ dnorm(0, .001)
  
  B_BH_PA ~ dnorm(0, .001)
  B_RR_PA ~ dnorm(0, .001)
  B_SubBH_PA ~ dnorm(0, .001)
  B_SubRR_PA ~ dnorm(0, .001)
  
  for(i in 2:NSal){
    B_Sal[i]  ~ dnorm(0, .001)
    B_BH_Sal[i] ~ dnorm(0, .001)
    B_RR_Sal[i] ~ dnorm(0, .001)
    
    B_Sal_PA[i]  ~ dnorm(0, .001)
    B_BH_Sal_PA[i] ~ dnorm(0, .001)
    B_RR_Sal_PA[i] ~ dnorm(0, .001)
  }
  
  B_Sal[1] <- -mean(B_Sal[2:NSal]) 
  B_BH_Sal[1] <- 0#-mean(B_BH_Sal[2:NSal]) 
  B_RR_Sal[1] <- 0#-mean(B_RR_Sal[2:NSal]) 
  #B_Sal[1] <- 0
  
  B_Sal_PA[1] <- -mean(B_Sal_PA[2:NSal]) 
  B_BH_Sal_PA[1] <- 0#-mean(B_BH_Sal[2:NSal]) 
  B_RR_Sal_PA[1] <- 0#-mean(B_RR_Sal[2:NSal]) 
  
  mu_CB   ~ dnorm(0, .001)
  mu_CB_PA ~ dnorm(0, .001)
}
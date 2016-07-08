# We model SAV area as a gamma dist variable, composed of the product of the potential habitat
# and the expected proportion occupied. We model that proportion with a logit-linked linear model
#
data{
  #z[i]
  #ones[i]
}
model{
  #for the ones trick:
  c <- 10000
  
  for (i in 1:NSites) {
    # Presence
    #w[i] <- w_1[i]#min(max(.1,w_1[i]),.9)
    w[i] <- ilogit(alpha_PA + mu_SE_PA[SubEst[i]] + B_BH_PA * BH[i] + B_RR_PA * RR[i] +
                     B_BH_Sal_PA[Sal[i]]*BH[i] + B_RR_Sal_PA[Sal[i]]*RR[i])
    
    
    # Abundance
    logdlnorm[i]   <- log(dlnorm(SAV[i],SAV_hat[i], tau_y_hat))
    #logGamma[i]    <- log(dgamma(SAV[i], lambdaObs[i], rObs[i]))
    
    #lambdaObs[i]   <- SAV_hat[i]^2 / var_y_hat    
    #rObs[i]        <- SAV_hat[i] / var_y_hat
    
    #SAV_hat[i] <- Prop[i]*Hab[i] 
    #Prop_adj[i] <- Prop[i]# min(max(.1,Prop[i]), .9)
    SAV_hat[i] <- log(Hab[i])+alpha + mu_SE[SubEst[i]] + B_BH * BH[i] + B_RR * RR[i] +
                        B_BH_Sal[Sal[i]]*BH[i] + B_RR_Sal[Sal[i]]*RR[i]
    
    # define the total likelihood, where the likelihood is (1 - w) if y = 0 (z = 0) or
    # the likelihood is w * gammalik if y >= 0.0001 (z = 1). So if z = 1, then the first bit must be
    # z[i] is 0 if y = 0, and 1 if y > 0
    
    logLik[i] <- (1 - z[i]) * log(1 - w[i]) + z[i] * (log(w[i]) + logdlnorm[i]) 
    Lik[i]    <- exp(logLik[i])
    
    
    # Use the ones trick, p[i] must fall btw 0 and 1, so divide by a very large number (C)
    # this scaling doesn't affect the outcome b/c MCMC works on the relative scale
    # The likelihood of a 1 drawn from a bernoulli dist with p, is p
    p[i]    <- Lik[i] / c 
    #p_adj[i] <- min(max(.00001,p[i]), .99999)
    ones[i] ~ dbern(p[i])
  }
  maxp = max(p[])
  minp = min(p[])
  
  for (j in 1:NSubEst) {
    #Prop_SE ~ dbeta(Prop_RS[RivSys[j]])
    Prop_SE[j] <- ilogit(mu_SE[j]) # a derived proportion for
    mu_SE[j]    ~ dnorm(yhat_SE[j], tau_SE)
    yhat_SE[j] <-  B_Sal[SubSal[j]] #+ # mu_RS[SubRivSys[j]]
    #B_SubBH * SubBH[j] + B_SubRR * SubRR[j]# + Landcover + PctArmor 
    
    mu_SE_PA[j] ~ dnorm(yhat_SE_PA[j], tau_SE_PA)
    yhat_SE_PA[j] <-  B_Sal_PA[SubSal[j]]# + #mu_RS_PA[SubRivSys[j]] +
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
  
  
  alpha ~ dnorm(0, .001)
  alpha_PA ~ dnorm(0, .001)
  
  tau_y_hat <- sig_St^-2
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
    B_BH_Sal[i] ~ dnorm(0, .001)
    B_RR_Sal[i] ~ dnorm(0, .001)
    
    B_BH_Sal_PA[i] ~ dnorm(0, .001)
    B_RR_Sal_PA[i] ~ dnorm(0, .001)
  }
  
  for (i in 1:NSal){
    B_Sal_raw[i]     ~ dnorm(0, .001)
    B_Sal[i]        <- B_Sal_raw[i] - mean(B_Sal_raw[])
    B_Sal_PA_raw[i]  ~ dnorm(0, .001)
    B_Sal_PA[i]     <- B_Sal_PA_raw[i] - mean(B_Sal_PA_raw[])
  }
  
  #B_Sal[1] <- 0#-mean(B_Sal[2:NSal]) 
  B_BH_Sal[1] <- 0#-mean(B_BH_Sal[2:NSal]) 
  B_RR_Sal[1] <- 0#-mean(B_RR_Sal[2:NSal]) 
  #B_Sal[1] <- 0
  
  #B_Sal_PA[1] <- 0#-mean(B_Sal_PA[2:NSal]) 
  B_BH_Sal_PA[1] <- 0#-mean(B_BH_Sal[2:NSal]) 
  B_RR_Sal_PA[1] <- 0#-mean(B_RR_Sal[2:NSal]) 
  
  mu_CB   ~ dnorm(0, .001)
  mu_CB_PA ~ dnorm(0, .001)
  
  
  #Derived Quantities:
  
  BH_MH <- B_BH + B_BH_Sal[1]
  BH_OH <- B_BH + B_BH_Sal[2]
  BH_PH <- B_BH + B_BH_Sal[3]
  #BH_TF <- Beta_BH + Beta_BS[4]
  
  RR_MH <- B_RR + B_RR_Sal[1]
  RR_OH <- B_RR + B_RR_Sal[2]
  RR_PH <- B_RR + B_RR_Sal[3]
  #RR_TF <- Beta_RR + Beta_RS[4]
}
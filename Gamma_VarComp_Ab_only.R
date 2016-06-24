# We model SAV area as a gamma dist variable, composed of the product of the potential habitat
# and the expected proportion occupied. We model that proportion with a logit-linked linear model
#

model{
  for (i in 1:NSites) {
    SAV[i]      ~ dgamma(lambdaObs[i],rObs[i])#dgamma(alpha[j], beta[j])
    
    lambdaObs[i]   <- SAV_hat[i]^2 / var_y_hat
    rObs[i]        <- SAV_hat[i] / var_y_hat
    SAV_hat[i]  <- Prop[i]*Hab[i] # 
    Prop[i]     <- ilogit(mu_SE[SubEst[i]])
  }
  
  for (j in 1:NSubEst) {
    #Prop_SE ~ dbeta(Prop_RS[RivSys[j]])
    Prop_SE[j] <- ilogit(mu_SE[j]) # a derived proportion for
    mu_SE[j]    ~ dnorm(yhat_SE[j], tau_SE)
    yhat_SE[j] <- mu_RS[SubRivSys[j]]+ B_Sal[SubSal[j]] #+ SALINZONE + Landcover + PctArmor 
  }
  
  for (k in 1:NRivSys) {
    #Prop_RS[k] ~ dbeta(Prop) 
    Prop_RS[k] <- ilogit(mu_RS[k])
    mu_RS[k]    ~ dnorm(yhat_RS[k], tau_RS)
    #mu_RS[k] <- mu_RS_raw[k]-mean(mu_RS_raw[])
    yhat_RS[k] <- mu_CB # + landcover?
  }
  
  # to get variance components on the prediction scale
  for (n in 1:100){
    newmu_RS[n]    ~ dnorm(mu_CB, tau_RS)
    newProp_RS[n] <- ilogit(newmu_RS[n])
    newmu_SE[n]    ~ dnorm(mu_CB, tau_SE)
    newProp_SE[n] <- ilogit(newmu_SE[n])
  }
  
  sd_newProp_RS  <- sd(newProp_RS[])
  var_newProp_RS <- sd_newProp_RS^2
  sd_newProp_SE  <- sd(newProp_SE[])
  var_newProp_SE <- sd_newProp_SE^2
  sd_Sal <- sd(B_Sal[])
  
  var_y_hat <- sig_St^-2
  sig_St    ~ dunif(0,10000)
  tau_RS <- sig_RS^-2
  sig_RS  ~ dunif(0,100)
  sig2_RS <- ilogit(sig_RS)
  tau_SE <- sig_SE^-2
  sig_SE  ~ dunif(0,100)
  #tau_Sal <- sig_Sal^-2
  #sig_Sal  ~ dunif(0,100)
  
  for(i in 2:NSal){
    B_Sal[i] ~ dnorm(0, .0001)
    #B_Sal_raw[i]  ~ dnorm(mu_Sal, .0001)
    #B_Sal[i] <- B_Sal_raw[i]-mean(B_Sal_raw[2:4])
    #B_Sal[i] ~ dnorm(0, tau_Sal)
  }
  B_Sal[1] <- -sum(B_Sal[2:NSal]) #0
  
  mu_CB     ~ dnorm(0, .0001)
  mu_Sal    ~ dnorm(0,.0001)
}
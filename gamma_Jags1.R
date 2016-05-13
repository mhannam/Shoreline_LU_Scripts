# We model SAV area as a gamma dist variable, composed of the product of the potential habitat
# and the expected proportion occupied. We model that proportion with a logit-linked linear model
#

model{
  for (i in 1:NSites) {
    SAV[i]      ~ dgamma(lambdaObs[i],rObs[i])#dgamma(alpha[j], beta[j])
    
    lambdaObs[i]   <- SAV_hat[i]^2 / var_y_hat
    rObs[i]        <- SAV_hat[i] / var_y_hat

    
    SAV_hat[i]  <- Prop[i]*Hab[i] # or exp(mu[SubEst[i]])*Hab[i] # or ilogit(mu[SubEst[i]])*Hab[i]
    #Prop[i]    <- Prop_SE[SubEst[i]]
    #Prop[i] <- ilogit(Subest + RivSys)
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
  
  var_y_hat <- sig_St^-2
  sig_St    ~ dunif(0,10000)
  tau_RS <- sig_RS^-2
  sig_RS  ~ dunif(0,100)
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
  B_Sal[1] <- -mean(B_Sal[2:NSal]) #0
  
  mu_CB     ~ dnorm(0, .0001)
  mu_Sal    ~ dnorm(0,.0001)
}
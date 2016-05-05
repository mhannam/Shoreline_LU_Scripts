model{
  for (i in 1:NSites) {
    SAV[i]      ~ dgamma(SAV_hat[i])#dgamma(alpha[j], beta[j])
    SAV_hat[i] <- Prop[i]*Hab[i] # or exp(mu[SubEst[i]])*Hab[i] # or ilogit(mu[SubEst[i]])*Hab[i]
    
    Prop[i]    <- Prop_SE[SubEst[i]]
    Prop[i] <- ilogit(Subest + RivSys)
    Prop[i] <- ilogit(mu_SE[Subest[i]])
  }
  
  for (j in 1:NSubEst) {
    #Prop_SE ~ dbeta(Prop_RS[RivSys[j]])
    Prop_SE[j] ~ ilogit(mu_SE[i]) # a derived proportion for
    mu_SE[j] ~ dnorm(yhat_SE[j])
    yhat_SE[j] <- mu_RS[RivSys[j]] #+ SALINZONE + Landcover + PctArmor 
  }
  
  for (k in 1:NRivSys) {
    #Prop_RS[k] ~ dbeta(Prop) 
    Prop_RS[k] ~ ilogit(mu_RS[k])
    mu_RS[k]   ~ dnorm(yhat_RS[k])
    yhat_RS[k] <- mu_CB # + landcover?
  }
}
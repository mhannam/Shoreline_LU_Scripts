model{
  for (i in 1:NSites) {
    SAV[i]      ~ dgamma(SAV_hat[i])#dgamma(alpha[j], beta[j])
    SAV_hat[i] <- Prop[i]*Hab[i] # or exp(mu[SubEst[i]])*Hab[i] # or ilogit(mu[SubEst[i]])*Hab[i]
    
    Prop[i]    <- Prop_SE[SubEst[i]]
  }
  
  for (j in 1:NSubEst) {
    Prop_SE ~ dbeta()
    
  }
  
  for (k in 1:NRivSys) {
    Prop_RS[k] ~ dbeta() 
  }
}
# I don't think this model will say much. The proportion is getting passed down each hierachy 
# as a definition, not as a distribution, so I don't think that we are 'sampling' the next
# level up. But I guess we should run a simulation either way.
model{
	for (i in 1:NSites) {
		SAV[i]      ~ dpois(SAV_hat[i])#dgamma(alpha[j], beta[j])
		SAV_hat[i] <- Prop[i]*Hab[i]
		
		Prop[i]    <- Prop_SE[SubEst[i]]
	}
	
	for (j in 1:NSubEst) {
	  SAV_SE[j]      ~ dpois(SAV_SE_hat[j])
	  SAV_SE_hat[j] <- Prop_SE[j]*Hab_SE[j]
	  Prop_SE[j]    <- Prop_RS[RivSys[j]]
	}
	
	for (k in 1:NRivSys) {
		SAV_RS[k] ~ dpois(SAV_RS_hat[k])
		SAV_RS_hat[k] <- Prop_RS[k] * Hab_RS[k]
	  Prop_RS[k] 
	}
}
alpha[j] ~ dnorm(mu_RivSys[k]) #dgamma(alphaS[k], betaS[k])



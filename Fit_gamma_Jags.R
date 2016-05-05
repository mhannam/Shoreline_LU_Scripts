source('Sh_LU_Data_Prep.R')
library(runjags)
library(dplyr)

#Model Data Prep -------------------------------

#Sample the data
SAV_Sh.samp = SAV_Sh %>% group_by(SubEst) %>% sample_frac(.01)#sample_n(50)
SAV_Sh.samp$SubEst = factor(SAV_Sh.samp$SubEst)

#Create a Subest Summary Table
Subest.Samp = SAV_Sh.samp %>% group_by(SubEst) %>% 
  summarise(RivSys = first(BIGNAME),
            RivSysQC = last(BIGNAME),
            Sal = first(SALINZONE))


mod.dat.samp = 
  with(SAV_Sh.samp,
       list(
         NSites = length(SAV),
         SAV    = SAV,
         Hab    = Hab,
         NSubEst = length(levels(SubEst)),
         SubEst  = as.numeric(SubEst),
         NRivSys = length(levels(BIGNAME)),
         SiteRivSys = as.numeric(BIGNAME),
         Nsal   = length(levels(SALINZONE)),
         Sal    = as.numeric(SALINZONE)
))
mod.dat.samp$SubRivSys = as.numeric(Subest.Samp$RivSys)
mod.dat.samp$SubSal    = as.numeric(Subest.Samp$Sal)

# make zero-truncated data for model development
mod.dat.samp.ztrunc = 
  with(filter(SAV_Sh.samp, Hab>0),
       list(
         NSites = length(sav98_13m2),
         SAV    = sav98_13m2,
         Hab    = pothabm2,
         NSubEst = length(levels(SubEst)),
         SubEst  = as.numeric(SubEst),
         NRivSys = length(levels(BIGNAME)),
         RivSys = as.numeric(BIGNAME),
         NSal   = length(levels(SALINZONE)),
         Sal    = as.numeric(SALINZONE)
       ))
mod.dat.samp.ztrunc$SubRivSys = as.numeric(Subest.Samp$RivSys)
mod.dat.samp.ztrunc$SubSal    = as.numeric(Subest.Samp$Sal)

my.pars = c('sig_RS', 'sig_SE', 'sig_St', 'B_Sal', 'mu_CB')
VarComp.mod = ('gamma_Jags1.R')
VarComp = run.jags(VarComp.mod, data = mod.dat.samp.ztrunc,
                   monitor = my.pars, n.chains = 6, burnin = 10000, sample = 1000, thin=5,
                   method = 'rjparallel')
VarComp

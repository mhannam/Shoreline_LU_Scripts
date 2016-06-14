source('Sh_LU_Data_Prep.R')
library(runjags)
library(dplyr)
library(lme4)

varcomp.lmer = lmer(SAV/10000~SALINZONE+(1|HUC_8/SubEst), data = SAV_Sh.samp)

my.inits = list(list('sig_SE' = 1.7, 'sig_RS' = .36, 'sig_St'= 2.7,
                     'B_Sal[1]'= .35, 'B_Sal[2]' = 1.2, 'B_Sal[3]' = .86,'B_Sal[4]' = 5.3),
                list('sig_SE' = .7, 'sig_RS' = 1.36, 'sig_St'= 1.7,
                     'B_Sal[1]'= 2.35, 'B_Sal[2]' = 3.2, 'B_Sal[3]' = 2.86,'B_Sal[4]' = 7.3),
                list('sig_SE' = 2.7, 'sig_RS' = 3.36, 'sig_St'= 5.7,
                     'B_Sal[1]'= -1.35, 'B_Sal[2]' = -1.2, 'B_Sal[3]' = -1.86,'B_Sal[4]' = -3.3)
)

#Model Data Prep -------------------------------

#Sample the data
SAV_Sh.samp = SAV_Sh1 %>% group_by(SubEst) %>% sample_frac(.2) %>%
  filter(Hab>0) #sample_n(50) 
SAV_Sh.samp$SubEst = factor(SAV_Sh.samp$SubEst)

SAV_Sh.samp = filter(SAV_Sh.samp, Hab>0)

#Create a Subest Summary Table
Subest.Samp = SAV_Sh.samp %>% group_by(SubEst) %>% 
  summarise(HUC_8 = first(HUC_8),
            HUC_8QC = last(HUC_8),
            Sal = first(SALINZONE),
            SubBH = first(BulkheadPerc),
            SubRR = first(RiprapPerc))


mod.dat.samp = 
  with(SAV_Sh.samp,
       list(
         NSites = length(SAV),
         ones   = rep(1, length(SAV)),
         z      = ifelse(SAV==0,0,1),
         SAV    = SAV/10000,
         Hab    = Hab/10000,
         NSubEst = length(levels(SubEst)),
         SubEst  = as.numeric(SubEst),
         NRivSys = length(levels(HUC_8)),
         SiteRivSys = as.numeric(HUC_8),
         NSal   = length(levels(SALINZONE)),
         Sal    = as.numeric(SALINZONE),
         BH     = as.numeric(Structure=="Bulkhead"),
         RR     = as.numeric(Structure=="Riprap")
))

mod.dat.samp$SubRivSys = as.numeric(Subest.Samp$HUC_8)
mod.dat.samp$SubSal    = as.numeric(Subest.Samp$Sal)
mod.dat.samp$SubBH = Subest.Samp$SubBH
mod.dat.samp$SubRR = Subest.Samp$SubRR


# make zero-truncated data for model development
mod.dat.samp.ztrunc = 
  with(filter(SAV_Sh.samp, Hab>0),
       list(
         NSites = length(sav98_13m2),
         ones   = rep(1, length(SAV)),
         z      = ifelse(SAV==0,0,1),
         SAV    = sav98_13m2/10000,
         Hab    = pothabm2/10000,
         NSubEst = length(levels(SubEst)),
         SubEst  = as.numeric(SubEst),
         NRivSys = length(levels(HUC_8)),
         RivSys = as.numeric(HUC_8),
         NSal   = length(levels(SALINZONE)),
         Sal    = as.numeric(SALINZONE),
         BH     = as.numeric(Structure=="Bulkhead"),
         RR     = as.numeric(Structure=="Riprap")
       ))
mod.dat.samp.ztrunc$SubRivSys = as.numeric(Subest.Samp$HUC_8)
mod.dat.samp.ztrunc$SubSal    = as.numeric(Subest.Samp$Sal)
mod.dat.samp.ztrunc$SubBH     = Subest.Samp$SubBH
mod.dat.samp.ztrunc$SubRR     = Subest.Samp$SubRR



my.pars = c('sig_RS', 'sig_SE', 'sig_St', 'B_Sal', 'mu_CB', 'sig_Sal', 
            'B_BH', 'B_RR','B_BH_Sal','B_RR_Sal','B_SubBH', 'B_SubRR', 'Prop_RS',
            'sd_newProp_RS','var_newProp_RS','sd_newProp_SE','Lik[1]','maxp','maxLik')

VarComp.mod = ('gamma_VarComp_Ab_only.R')
VarComp.mod = ('gamma_Jags_armor.R')

VarComp = run.jags(VarComp.mod, data = mod.dat.samp.ztrunc,
                   monitor = my.pars, n.chains = 3, burnin = 10000, sample = 5000,
                   thin=1,modules = 'glm on',inits = my.inits,
                   method = 'rjparallel')#, mutate = list('sd', vars = "B_Sal[]"))
VarComp



ZIG.mod = 'ZIG_Armor.R'
my.inits = list(list('mu_CB' = 1,'mu_CB_PA'=1,
                     'sig_SE' = 1.7, 'sig_RS' = .36, 'sig_St'= 2.7, 
                     'sig_SE_PA' = 1.7, 'sig_RS_PA' = .36, 'sig_St_PA'= 2.7, 
                     'B_Sal[1]'= .35, 'B_Sal[2]' = 1.2, 'B_Sal[3]' = .86,'B_Sal[4]' = 5.3,
                     'B_Sal_PA[1]'= .35, 'B_Sal_PA[2]' = 1.2, 'B_Sal_PA[3]' = .86,'B_Sal_PA[4]' = 5.3),
                list('mu_CB' = 1,'mu_CB_PA'=1,
                     'sig_SE' = .7, 'sig_RS' = 1.36, 'sig_St'= 1.7,
                     'sig_SE_PA' = 1.7, 'sig_RS_PA' = .36, 'sig_St_PA'= 2.7, 
                     'B_Sal[1]'= 2.35, 'B_Sal[2]' = 3.2, 'B_Sal[3]' = 2.86,'B_Sal[4]' = 7.3,
                     'B_Sal_PA[1]'= .35, 'B_Sal_PA[2]' = 1.2, 'B_Sal_PA[3]' = .86,'B_Sal_PA[4]' = 5.3),
                list('mu_CB' = 1,'mu_CB_PA'=1,
                     'sig_SE' = 2.7, 'sig_RS' = 3.36, 'sig_St'= 5.7,
                     'sig_SE_PA' = 1.7, 'sig_RS_PA' = .36, 'sig_St_PA'= 2.7, 
                     'B_Sal[1]'= -1.35, 'B_Sal[2]' = -1.2, 'B_Sal[3]' = -1.86,'B_Sal[4]' = -3.3,
                     'B_Sal_PA[1]'= .35, 'B_Sal_PA[2]' = 1.2, 'B_Sal_PA[3]' = .86,'B_Sal_PA[4]' = 5.3)
)
ZIG = run.jags(ZIG.mod, data = mod.dat.samp.ztrunc, inits = my.inits,
               monitor = my.pars, n.chains = 3, burnin = 1000, sample = 1000,
               thin = 1, modules = 'glm on',
               method = 'rjparallel')

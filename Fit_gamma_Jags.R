source('Sh_LU_Data_Prep.R')
library(runjags)
library(dplyr)
library(lme4)

varcomp.lmer = lmer(SAV/10000~SALINZONE+(1|HUC_8/SubEst), data = SAV_Sh.samp)
armor.lmer = lmer(OccHab250~Structure*SALINZONE + (1|SubEst), 
                   data = SAV_Sh.samp)#, na.action = na.fail)#, family = poisson(link='log'))

my.inits = list(list('sig_SE' = 1.7, 'sig_RS' = .36, 'sig_St'= 2.7,
                     'B_Sal[1]'= .35, 'B_Sal[2]' = 1.2, 'B_Sal[3]' = .86,'B_Sal[4]' = 5.3),
                list('sig_SE' = .7, 'sig_RS' = 1.36, 'sig_St'= 1.7,
                     'B_Sal[1]'= 2.35, 'B_Sal[2]' = 3.2, 'B_Sal[3]' = 2.86,'B_Sal[4]' = 7.3),
                list('sig_SE' = 2.7, 'sig_RS' = 3.36, 'sig_St'= 5.7,
                     'B_Sal[1]'= -1.35, 'B_Sal[2]' = -1.2, 'B_Sal[3]' = -1.86,'B_Sal[4]' = -3.3)
)

#Model Data Prep -------------------------------

#Sample the data
SAV_Sh.samp = SAV_Sh1 %>% group_by(SubEst) %>% sample_frac(.5) #%>%
  #filter(Hab>0) #sample_n(50) 
SAV_Sh.samp$SubEst = factor(SAV_Sh.samp$SubEst)

#SAV_Sh.samp = filter(SAV_Sh.samp, Hab>0)

#Create a Subest Summary Table
Subest.Samp = SAV_Sh.samp %>% group_by(SubEst) %>% 
  summarise(HUC_8 = first(HUC_8),
            HUC_8QC = last(HUC_8),
            Sal = first(SALINZONE),
            SubBH = first(BulkheadPerc),
            SubRR = first(RiprapPerc),
            SubShMarsh = sum(Perc_Marsh*Length_m)/sum(Length_m))


mod.dat.samp = 
  with(SAV_Sh.samp,
       list(
         NSites = length(SAV),
         ones   = rep(1, length(SAV)),
         z      = ifelse(SAV>.0001,1,0),
         SAV    = SAV_250/10000,
         SAVpres= sav98_13m2/10000,
         Hab    = Pot_250/10000, #Hab/10000,
         NSubEst = length(levels(SubEst)),
         SubEst  = as.numeric(SubEst),
         NRivSys = length(levels(HUC_8)),
         SiteRivSys = as.numeric(HUC_8),
         NSal   = length(levels(SALINZONE)),
         Sal    = as.numeric(SALINZONE),
         Str    = as.numeric(Structure),
         BH     = as.numeric(Structure=="Bulkhead"),
         RR     = as.numeric(Structure=="Riprap"),
         Perc_Marsh = Perc_Marsh
))

mod.dat.samp$SubRivSys = as.numeric(Subest.Samp$HUC_8)
mod.dat.samp$SubSal    = as.numeric(Subest.Samp$Sal)
mod.dat.samp$SubBH = Subest.Samp$SubBH
mod.dat.samp$SubRR = Subest.Samp$SubRR


# make zero-truncated data for model development
mod.dat.samp.ztrunc = 
  with(filter(SAV_Sh.samp, Hab>0),
       list(
         NSites = length(SAV_250),
         ones   = rep(1, length(SAV)),
         z      = ifelse(SAV==0,0,1),
         SAV    = SAV_250/10000,
         Hab    = Hab_250/10000,
         NSubEst = length(levels(SubEst)),
         SubEst  = as.numeric(SubEst),
         NRivSys = length(levels(HUC_8)),
         RivSys = as.numeric(HUC_8),
         NSal   = length(levels(SALINZONE)),
         Sal    = as.numeric(SALINZONE),
         Str    = as.numeric(Structure),
         BH     = as.numeric(Structure=="Bulkhead"),
         RR     = as.numeric(Structure=="Riprap"),
         Perc_Marsh = Perc_Marsh
       ))
mod.dat.samp.ztrunc$SubRivSys = as.numeric(Subest.Samp$HUC_8)
mod.dat.samp.ztrunc$SubSal    = as.numeric(Subest.Samp$Sal)
mod.dat.samp.ztrunc$SubBH     = Subest.Samp$SubBH
mod.dat.samp.ztrunc$SubRR     = Subest.Samp$SubRR



my.pars = c('sig_RS', 'sig_SE', 'sig_St', 'B_Sal', 'mu_CB', 'sig_Sal', 
            'B_BH', 'B_RR','B_BH_Sal','B_RR_Sal','B_SubBH', 'B_SubRR', 'Prop_RS',
            'sd_newProp_RS','var_newProp_RS','sd_newProp_SE','Lik[1]','maxp','maxLik','minp')

VarComp.mod = ('gamma_VarComp_Ab_only.R')
VarComp.mod = ('gamma_Jags_armor.R')
VarComp.mod = ('gamm_Jags_armor_alt.R')

VarComp = run.jags(VarComp.mod, data = mod.dat.samp.ztrunc,
                   monitor = my.pars, n.chains = 3, burnin = 10000, sample = 5000,
                   thin=1,modules = 'glm on',inits = my.inits,
                   method = 'rjparallel')#, mutate = list('sd', vars = "B_Sal[]"))
VarComp


my.pars = c('sig_RS', 'sig_SE', 'sig_St','sig_RS_PA', 'sig_SE_PA', 'sig_St_PA',
            'B_Sal', 'mu_CB', 'sig_Sal', 'B_Sal_PA', 'mu_CB_PA', 'sig_Sal_PA',
            'B_BH', 'B_RR','B_BH_Sal','B_RR_Sal','B_SubBH', 'B_SubRR', 'Prop_RS',
            'B_BH_PA', 'B_RR_PA', 'B_BH_Sal_PA', 'B_RR_Sal_PA', 'B_Sub_BH_PA', 'B_SubRR_PA', 
            'sd_newProp_RS','var_newProp_RS','sd_newProp_SE', 'sd_Sal',
            'sd_Sal_PA')#,'DIC')#,'minLik','maxp','maxLik','minp',
            #'maxG', 'minG', 'minw', 'maxw')
ZIG.mod = 'ZIG_Armor.R'
ZIGVC.mod = 'ZIG_Varcomp.R'
my.inits = list(list('mu_CB' = 1,'mu_CB_PA'=1,
                     'sig_SE' = 1.7, 'sig_RS' = .36, 'sig_St'= 8.17, 
                     'sig_SE_PA' = 2.5, 'sig_RS_PA' = .36, 'sig_St_PA'= 2.7, 
                     'B_Sal_raw'= c(.35, 2, .8),# 1.4),
                     'B_Sal_PA_raw'= c(.35, 1.2,  .86)), # 1.4)),
                list('mu_CB' = 1,'mu_CB_PA'=1,
                     'sig_SE' = .7, 'sig_RS' = 1.36, 'sig_St'= 7.6,
                     'sig_SE_PA' = 2, 'sig_RS_PA' = .36, 'sig_St_PA'= 2.7, 
                     'B_Sal_raw'= c(2.35,3.2,  2.86),# 2.7),
                     'B_Sal_PA_raw'= c(.35, 3.2, .86)),# 3.3)),
                list('mu_CB' = 1,'mu_CB_PA'=1,
                     'sig_SE' = 2.7, 'sig_RS' = 3.36, 'sig_St'= 8.7,
                     'sig_SE_PA' = 3, 'sig_RS_PA' = .36, 'sig_St_PA'= 2.7, 
                     'B_Sal_raw'= c(-1.35, 1.0,  -1.86),# 0),
                     'B_Sal_PA_raw'= c(.35,  1.0,  .86))#, 0))
)

ZIGArm = run.jags(ZIG.mod, data = mod.dat.samp, inits = my.inits,
               monitor = my.pars, n.chains = 3, burnin = 1000, sample = 1000,
               thin = 1, modules = 'glm on',
               method = 'rjparallel')
ZIGVC = run.jags(ZIGVC.mod, data = mod.dat.samp, inits = my.inits,
               monitor = my.pars, n.chains = 3, burnin = 2000, sample = 1000,
               thin = 1, modules = 'glm on',
               method = 'rjparallel')


# Fit with old data --------------------------------------------------------------
my.pars = c('sig_RS', 'sig_SE', 'sig_St','sig_RS_PA', 'sig_SE_PA', 'sig_St_PA',
            'B_Sal', 'mu_CB', 'sig_Sal', 'B_Sal_PA', 'mu_CB_PA', 'sig_Sal_PA',
            'B_BH', 'B_RR','B_BH_Sal','B_RR_Sal','B_SubBH', 'B_SubRR', 'Prop_RS',
            'B_BH_PA', 'B_RR_PA', 'B_BH_Sal_PA', 'B_RR_Sal_PA', 'B_Sub_BH_PA', 'B_SubRR_PA', 
            'sd_newProp_RS','var_newProp_RS','sd_newProp_SE', 'sd_Sal',
            'sd_Sal_PA','B_Mar', 'B_Mar_PA')#,'minLik','maxp','maxLik','minp',
#'maxG', 'minG', 'minw', 'maxw')
ZIG.mod = 'ZIG_Armor.R'
#ZIGVC.mod = 'ZIG_Varcomp.R'
my.inits.old = list(list('mu_CB' = 1, 'mu_CB_PA'=1,
                     'sig_SE' = 1.7, 'sig_RS' = .36, 'sig_St'= 8.17, 
                     'sig_SE_PA' = 2.5, 'sig_RS_PA' = .36, 'sig_St_PA'= 2.7, 
                     'B_BH' = 0, 'B_RR' = 1,
                     'B_Sal_raw'    = c(.35, .2, .8),
                     'B_Sal_PA_raw' = c(.35, 1.2, .86)),
                list('mu_CB' = 1, 'mu_CB_PA'=1,
                     'sig_SE' = .7, 'sig_RS' = 1.36, 'sig_St'= 7.6,
                     'sig_SE_PA' = 2, 'sig_RS_PA' = .36, 'sig_St_PA'= 2.7,  
                     'B_BH' = 1, 'B_RR' = -1,
                     'B_Sal_raw'    = c(2.35,3.2,2.8),
                     'B_Sal_PA_raw' = c(.35, 3.2, .86)),
                list('mu_CB' = 1, 'mu_CB_PA'=1,
                     'sig_SE' = 2.7, 'sig_RS' = 3.36, 'sig_St'= 8.7,
                     'sig_SE_PA' = 3, 'sig_RS_PA' = .36, 'sig_St_PA'= 2.7,  
                     'B_BH' = -1, 'B_RR' = 0,
                     'B_Sal_raw'    = c(-1.35, 1.0, -1.8),
                     'B_Sal_PA_raw' = c(.35, 1.0, .86))
)

ZIGOld = run.jags(ZIG.mod, data = old.dat.samp, inits = my.inits.old,
                  monitor = my.pars, n.chains = 3, burnin = 1000, sample = 1000,
                  thin = 1, modules = 'glm on',
                  method = 'rjparallel')

ZILN.mod = 'ZILN_Armor.R'
ZILN = run.jags(ZILN.mod, data = mod.dat.samp, inits = my.inits, monitor = my.pars, n.chains = 3,
                burnin = 1000, sample = 1000, modules = 'glm on', method = 'rjparallel')
ZILN.old = run.jags(ZILN.mod, data = old.dat.samp, inits = my.inits, monitor = my.pars, n.chains = 3,
                burnin = 1000, sample = 1000, modules = 'glm on', method = 'rjparallel')

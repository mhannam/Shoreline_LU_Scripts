source(Community_Data_Prep)

Old_SAV = read.csv('L:\\Hannam\\SAV\\Data\\Shoreline_LU\\Shoreline_Landuse_Comm_Prepped.csv')


#Model Data Prep -------------------------------

#Sample the data
Old_SAV.samp = Old_SAV %>% group_by(River) %>% sample_frac(.5) %>%
  filter(PTHab_Area>0) #sample_n(50) 
Old_SAV.samp$SubEst = factor(Old_SAV.samp$River)

#SAV_Sh.samp = filter(SAV_Sh.samp, Hab>0)

#Create a Subest Summary Table
Old_Subest.Samp = Old_SAV.samp %>% group_by(SubEst) %>% 
  summarise(Sal = first(Salinity))


old.dat.samp = 
  with(Old_SAV.samp,
       list(
         NSites = length(SAV_Area),
         ones   = rep(1, length(SAV_Area)),
         z      = ifelse(SAV_Area>.0001,1,0),
         SAV    = SAV_Area*100, #from km^2 to hectare
         Hab    = PTHab_Area*100,
         NSubEst = length(levels(SubEst)),
         SubEst  = as.numeric(SubEst),
         NSal   = length(levels(Salinity)),
         Sal    = as.numeric(SALINZONE),
         #Str    = as.numeric(Structure),
         BH     = Bulkhead,
         RR     = Riprap
       ))

# mod.dat.samp$SubRivSys = as.numeric(Subest.Samp$HUC_8)
 old.dat.samp$SubSal    = as.numeric(Old_Subest.Samp$Sal)
# mod.dat.samp$SubBH = Subest.Samp$SubBH
# mod.dat.samp$SubRR = Subest.Samp$SubRR
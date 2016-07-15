library(tidyr)
library(dplyr)
#file.choose()

# # Files on Mac ---------------------------
# VA_SAV_StrFile = "/Users/mikehannam/Dropbox (Smithsonian)/SERC/SAV/Data/VAeucasstru_spJ_SummarizeWit_sav98_13.csv"
# VA_Sh_LUFile = "/Users/mikehannam/Dropbox (Smithsonian)/SERC/SAV/Data/VAlubcFEATURE2.csv"
# 
# MD_SAV_StrFile = "/Users/mikehannam/Dropbox (Smithsonian)/SERC/SAV/Data/MDeucasstru_spJ_SummarizeWit_sav98_13.csv"
# MD_Sh_LUFile = "/Users/mikehannam/Dropbox (Smithsonian)/SERC/SAV/Data/MDlubcFEATURE2.csv"
# 
# SubEstFile =  "/Users/mikehannam/Dropbox (Smithsonian)/SERC/SAV/Data/Subest_Metrics.csv"
# SubEstHucFile =  "/Users/mikehannam/Dropbox (Smithsonian)/SERC/SAV/Data/SubEst_Huc.txt"

# Files on PC -----------------------------
Chris_File     = "L:/Hannam/SAV/Shoreline_LU/sstru_Grouped_50_125_250mBuf_lubc_SubEst.txt"

VA_SAV_StrFile = "L:/Williams/GIS/ForMike/OLD_DELETE/VAeucasstru_spJ_SummarizeWit_sav98_13.csv"
VA_Sh_LUFile   = "L:\\Hannam\\SAV\\Data\\Shoreline_LU\\Special_Issue_Data\\VAlubcFEATURE2.csv"

MD_SAV_StrFile = "L:\\Williams\\GIS\\ForMike\\OLD_DELETE\\MDeucasstru_spJ_SummarizeWit_sav98_13.csv"
MD_Sh_LUFile   = "L:\\Hannam\\SAV\\Data\\Shoreline_LU\\Special_Issue_Data\\MDlubcFEATURE2.csv"

SubEstFile     = "L:\\Hannam\\SAV\\Data\\Shoreline_LU\\Subest_Metrics.csv"
SubEstHucFile  = "L:\\Hannam\\SAV\\Data\\Shoreline_LU\\Special_Issue_Data\\SubEst_Huc.txt"
SubEstSAVFile  = "L:\\Hannam\\SAV\\Data\\NOAA_CSCOR\\SubEstSAV_Abundance_Summary_FINAL.csv"

#VA_SAV_250file = "L:\\Williams\\GIS\\ForMike\\SAVTabulateArea\\OLD_DELETE\\eucasstruVA_b9813sub250va_lte250.csv"
VA_SAV_500file = "L:\\Williams\\GIS\\ForMike\\SAVTabulateArea\\ReTestMerge9813\\SAVSUMS_eucasstruvaJ_beds9813_1m.txt"
VA_SAV_250file = "L:\\Williams\\GIS\\ForMike\\SAVTabulateArea\\ReTestMerge9813\\SAVSUMS2_eucasstruvaJ_va1m_sav250.txt"
VA_SAV_100file = 
VA_PtHab_250file   = "L:\\Williams\\GIS\\ForMike\\SAVTabulateArea\\ReTestMerge9813\\SAVSUMS_eucasstruvaJ_va1m_pot250.txt"
VA_file = "L:/Hannam/SAV/Data/Shoreline_LU/Special_Issue_Data/VA_SAV_Str_LU.txt"
  
#MD_SAV_250file = 
MD_SAV_500file = "L:\\Williams\\GIS\\ForMike\\SAVTabulateArea\\ReTestMerge9813\\SAVSUMS_eucasstrumdJ_beds9813_1m.txt"
MD_SAV_250file = "L:\\Williams\\GIS\\ForMike\\SAVTabulateArea\\ReTestMerge9813\\SAVSUMS_eucasstrumdJ_md1m_beds250.txt"
MD_SAV_100file = 
MD_PtHab_250file =   "L:\\Williams\\GIS\\ForMike\\SAVTabulateArea\\ReTestMerge9813\\SAVSUMS_eucasstrumdJ_md1m_pot250.txt"
MD_file = "L:/Hannam/SAV/Data/Shoreline_LU/Special_Issue_Data/MD_SAV_Str_LU.csv"

#Load Chris's data -----------------------------------------------------------
CP_SAV <- read.csv(Chris_File)
CP_SAV2 <- filter(CP_SAV, GROUPED %in% c('Bulkhead', 'Natural','Riprap'), 
                  PTHAB_AREA > 0,
                  struGlngth>.075,
                  #SUBEST_ID !='', 
                  SALINITY %in% c('MH', 'PH', 'OH'))
CP_SAV2$OccHab = with(CP_SAV2, SAV_AREA/max(PTHAB_AREA,SAV_AREA)) # create occ hab and deal with SAV > Occhab
CP_SAV2$GROUPED = factor(CP_SAV2$GROUPED, levels(CP_SAV2$GROUPED)[c(6,7,2)]) # reorder factor levels

#Load and Prep Maryland Data ------------------------------------------------- 
MD_SAV_Str <- read.csv(MD_SAV_StrFile)
MD_Sh_LU   <- read.csv(MD_Sh_LUFile)
MD_SAV_250 <- read.csv(MD_SAV_250file)
MD_SAV_500 <- read.csv(MD_SAV_500file)
MD_Pot_250 <- read.csv(MD_PtHab_250file)

#Join files for SAV within 250m and within 500m
MD_SAV0 = full_join(
  select(MD_SAV_500, SAV_500 = VALUE_1, sstruID = SSTRUID),
  select(MD_SAV_250, SAV_250 = VALUE_1, sstruID = SSTRUID),
  'sstruID')
MD_SAV = full_join(MD_SAV0,
  select(MD_Pot_250, Hab_250 = VALUE_1, sstruID = SSTRUID))

#Join those to the SAV_Str file
#MD_SAV_Str1 <- left_join(MD_SAV, MD_SAV_Str, 'sstruID') # This drops potential habitat = 0 (b/c 0 is missing in original data)
MD_SAV_Str1 <- full_join(MD_SAV, MD_SAV_Str, 'sstruID')  # This keeps potential habitat = 0

#MD_SAV_Str1[is.na(MD_SAV_Str1$SAV_500), 'SAV_500'] = 0
#MD_SAV_Str1[is.na(MD_SAV_Str1$SAV_250), 'SAV_250'] = 0

New_MD_Levels = c("Agriculture", "Bare", "Blank", "Comm_Ind_Other", "Marsh", "Forest", "Grass", "Comm_Ind_Other",
                  "Comm_Ind_Other","Comm_Ind_Other", "Residential", "Scrub_Shrub", "Forest")
length(New_MD_Levels) - length(levels(MD_Sh_LU$FEATURE2)) #SHOULD BE 0

levels(MD_Sh_LU$FEATURE2) = New_MD_Levels

MD_Sh_LU_Wide = spread(MD_Sh_LU, key = FEATURE2, value = PercentLength, fill=0) %>%
  group_by(Join_ID) %>%
  summarise(Length_m = sum(SUM_Length_METERS),
            Perc_Ag = sum(Agriculture),
            Perc_Comm_Ind = sum(Comm_Ind_Other),
            Perc_Marsh = sum(Marsh),
            Perc_Res = sum(Residential),
            Perc_Forest = sum(Forest),
            Perc_Grass = sum(Grass), 
            Perc_ScrubSh = sum(Scrub_Shrub),
            Perc_Blank = sum(Blank),
            Perc_Bare  = sum(Bare)
            )

MD_SAV_Sh = left_join(MD_SAV_Str1, MD_Sh_LU_Wide, by = "Join_ID") %>%
  select(Structure = STRUCTURE, County = county, #sav89_13m2, sav98_13m2, 
         Hab_500 = pothabm2, #savPCTphab, 
         SubEst = SUBESTID, Length_m,
         Perc_Ag:Perc_ScrubSh,
         SAV_250, SAV_500, Hab_250,sstruID) %>%
  mutate(State = 'MD', State_sstruID = paste(State, sstruID,sep="_"))

# Use newer table that is pre-joined: -----------------------------------
MD_SAV_new = read.csv(MD_file)

MD_SAV_Sh2 = MD_SAV_new %>% select(
  Structure = STRUCTURE,
  Hab_500 = poth500,
  Hab_250 = poth250,
  Hab_100 = poth100,
  SubEst = SUBESTID,
  Length_m = SUM_Length_METERS,
  Perc_Ag:Perc_ScrubSh, 
  SAV_500 = sav500, SAV_250 = sav250, SAV_100 = sav100, ST_STRUID
)

VA_SAV_new = read.csv(VA_file)

VA_SAV_Sh2 = VA_SAV_new %>% select(
  Structure = STRUCTURE,
  Hab_500 = poth500,
  Hab_250 = poth250,
  Hab_100 = poth100,
  SubEst = SUBESTID,
  Length_m = SUM_Length_METERS,
  Perc_Ag:Perc_ScrubSh, 
  SAV_500 = sav500, SAV_250 = sav250, SAV_100 = sav100, ST_STRUID
)

#Load and Prep Virginia Data ------------------------------------------------- 
VA_SAV_Str <- read.csv(VA_SAV_StrFile)
VA_Sh_LU   <- read.csv(VA_Sh_LUFile)
VA_SAV_250 <- read.csv(VA_SAV_250file)
VA_SAV_500 <- read.csv(VA_SAV_500file)
VA_Pot_250 <- read.csv(VA_PtHab_250file)

#Join files for SAV within 250m and SAV from 250-500m
VA_SAV0 = full_join(
  select(VA_SAV_500, SAV_500=VALUE_1, sstruID = SSTRUID),
  select(VA_SAV_250, SAV_250=VALUE_1, sstruID = SSTRUID),
  'sstruID')
VA_SAV = full_join(VA_SAV0,
                   select(VA_Pot_250, Hab_250 = VALUE_1, sstruID = SSTRUID))

#Join those to the SAV_Str file
#VA_SAV_Str1 <- left_join(VA_SAV, VA_SAV_Str, 'sstruID')
VA_SAV_Str1 <- full_join(VA_SAV, VA_SAV_Str, 'sstruID')
#VA_SAV_Str1[is.na(VA_SAV_Str1$SAV_500), 'SAV_500']=0
#VA_SAV_Str1[is.na(VA_SAV_Str1$SAV_250), 'SAV_250']=0


#Reclassify
# (Forest, forested, timbered) -> Forest
# (Commercial, Military, Industrial, Paved) -> Comm_Ind_Other
# (Residential, Family Dwelling, Multi Familiy Dwelling) -> Residential
New_VA_Levels = c('Blank', 'Agriculture','Bare', 'Comm_Ind_Other','Marsh','Marsh', 'Residential',
                              'Forest','Forest','Grass', 'Comm_Ind_Other','Marsh', 'Comm_Ind_Other', 'Residential',
                              'Comm_Ind_Other', 'Residential', 'Scrub_Shrub', 'Forest')
levels(VA_Sh_LU$feature2) <- New_VA_Levels
levels(VA_Sh_LU$feature2)


VA_Sh_LU_Wide = spread(VA_Sh_LU, key = feature2, value = PercentLength, fill = 0) %>%
  group_by(Join_ID) %>%
  summarise(Length_m      = sum(SUM_Length_METERS),
            Perc_Ag       = sum(Agriculture),
            Perc_Comm_Ind = sum(Comm_Ind_Other),
            Perc_Marsh    = sum(Marsh),
            Perc_Res      = sum(Residential),
            Perc_Forest   = sum(Forest),
            Perc_Grass    = sum(Grass), 
            Perc_ScrubSh  = sum(Scrub_Shrub),
            Perc_Blank = sum(Blank),
            Perc_Bare  = sum(Bare)
            )
    #This step creates 257 NAs in Length_m, presumably the whole LU file has NAs here
VA_SAV_Sh = left_join(VA_SAV_Str1, VA_Sh_LU_Wide, by= "Join_ID" ) %>%
  select(Structure = STRUCTURE, 
         County = COUNTY, #sav89_13m2, sav98_13m2, 
         Hab_500 = pothabm2, #savPCTphab, 
         SubEst = SUBESTID, 
         Length_m, Perc_Ag:Perc_ScrubSh, SAV_250, SAV_500, Hab_250,sstruID) %>%
  mutate(State = 'VA', State_sstruID = paste(State, sstruID,sep="_"))



summary(VA_SAV_Sh)
summary(MD_SAV_Sh)

#Combine----------------------------------------
SAV_Sh0 = rbind(VA_SAV_Sh, MD_SAV_Sh)
SAV_Sh02 = rbind(VA_SAV_Sh2, MD_SAV_Sh2)

#TO ANALYZE ONLY MD DATA: ################-------------------------------
#SAV_Sh0 = MD_SAV_Sh

#GIS gave NA to 0 SAV segments, add fields where these are 0, but keep NAs:
SAV_Sh0 = SAV_Sh02 %>% mutate(SAV = SAV_250,
                              OccHab100 = SAV_100/Hab_100,
                             OccHab500 = SAV_500/Hab_500,
                             OccHab250 = SAV_250/Hab_250)#savPCTphab)

SAV_Sh0[is.na(SAV_Sh0$SAV),c('SAV')] = 0.00001 #This stands in for zero, but is filtered out by the model
SAV_Sh0[is.na(SAV_Sh0$SAV_500),c('SAV_500')] = 0.00001 #This stands in for zero, but is filtered out by the model
SAV_Sh0[is.na(SAV_Sh0$Hab_500),'Hab_500'] = 0
SAV_Sh0[is.na(SAV_Sh0$Hab_250),'Hab_250'] = 0
SAV_Sh0[is.na(SAV_Sh0$OccHab500),'OccHab500'] = 0
SAV_Sh0[is.na(SAV_Sh0$OccHab250),'OccHab250'] = 0

#Reclass SStru-----------------------------------------------
New.Struc = c('Bulkhead', 'Other','Other','Marina','Marina','Marina','Wharf',
              'Other','Natural','Riprap','Other', 'Wharf','Other')
levels(SAV_Sh0$Structure) = New.Struc
SAV_Sh0$Structure = factor(SAV_Sh0$Structure)

#Add subestuary metrics--------------------------------------------------------------------
SubEst = read.csv(SubEstFile, skip = 1)
SubEstHuc = read.csv(SubEstHucFile)
SubEstSAV = read.csv(SubEstSAVFile)

#Get just HUCs from SubEstHuc
SubEstHuc_Sel = SubEstHuc %>% 
  select(SubEst = SUBEST_ID, HUC_8)

#Combine HUCs with some of SubEst metrics
SubEst_Sel = 
SubEst %>% select(SubEst = CODE, NAME, BIGNAME, SALINZONE, LANDCAT, 
                  RiprapPerc, BulkheadPerc, FOREST06, WETLAND06,
                  IMPERVI11pct,DEV06, CROP06, MarshPhragPerc,MarshNoPhragPerc)

SubEst_Sel = left_join(SubEst_Sel, SubEstHuc_Sel )

#Join SubEst to SAV
SAV_Sh = left_join(SAV_Sh0, SubEst_Sel)#, by = c("SubEst"="CODE"))
SAV_Sh$SubEst = factor(SAV_Sh$SubEst)

#Drop NA subestuary records about 175 of these
SAV_Sh = SAV_Sh[SAV_Sh$SubEst!='',]
SAV_Sh$SubEst = factor(SAV_Sh$SubEst)

# Add other HUC scales
SAV_Sh = mutate(SAV_Sh, 
                HUC_6 = factor(substr(HUC_8,1,6)),
                HUC_4 = factor(substr(HUC_8,1,4)))
SAV_Sh$HUC_8 = factor(SAV_Sh$HUC_8)

# Expand potential habitat to include all sav area
# SAV_Sh$Pot_250a = ifelse(SAV_Sh$Pot_250>SAV_Sh$SAV_250, 
#                          SAV_Sh$Pot_250, SAV_Sh$SAV_250)

### Filter Dataset -----------------------------------------
#Examine shoreline segment length, and select a reasonable subset of lengths--------------
hist(log(SAV_Sh$Length_m))

quantile(SAV_Sh$Length_m, na.rm=TRUE, probs = seq(0,1,.1))
quantile(SAV_Sh$Length_m, na.rm=TRUE, probs = seq(0,1,.25))

#SAV_Sh1 = filter(SAV_Sh, Length_m < 450, Length_m > 25)
#SAV_Sh1 = filter(SAV_Sh, Length_m < 125, Length_m > 75) #Range used by Patrick et al

SAV_Sh1 = filter(SAV_Sh, Structure %in% c('Bulkhead', 'Riprap','Natural'),
                 Length_m < 125, Length_m > 75, #Hab_250>0,
                 SALINZONE %in% c('OH', 'MH', 'PH'))#,
                 #!is.na(Pot_250a))
SAV_Sh1$Structure = factor(SAV_Sh1$Structure, levels(SAV_Sh1$Structure)[c(5,6,1)])
SAV_Sh1$SALINZONE = factor(SAV_Sh1$SALINZONE)

plot(SAV_250~Hab_250, data = SAV_Sh1)
plot(SAV_250~SAV_500, data = SAV_Sh1)
plot(SAV_500~Hab_500, data = SAV_Sh1)
#SAV_Sh1[SAV_Sh1$SAV_250==max(SAV_Sh1$SAV_250),]

SAV_Subest = SAV_Sh %>% group_by(SubEst) %>% summarise(
  SAV_250 = sum(SAV), 
  SAV_500 = sum(SAV_500),
  Hab_250 = sum(Hab_250),
  Hab_500 = sum(Hab_500),
  Perc_BH = sum(Length_m*as.numeric(Structure=='Bulkhead'))/sum(Length_m),
  Perc_RR = sum(Length_m*as.numeric(Structure=='Riprap'))/sum(Length_m),
  Sal     = first(SALINZONE),
  Perc_BH2 = max(BulkheadPerc)
)
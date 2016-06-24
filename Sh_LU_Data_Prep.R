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
VA_SAV_StrFile = "L:\\Hannam\\SAV\\Data\\Shoreline_LU\\Special_Issue_Data\\VAeucasstru_spJ_SummarizeWit_sav98_13.csv"
VA_Sh_LUFile   = "L:\\Hannam\\SAV\\Data\\Shoreline_LU\\Special_Issue_Data\\VAlubcFEATURE2.csv"

MD_SAV_StrFile = "L:\\Hannam\\SAV\\Data\\Shoreline_LU\\Special_Issue_Data\\MDeucasstru_spJ_SummarizeWit_sav98_13.csv"
MD_Sh_LUFile   = "L:\\Hannam\\SAV\\Data\\Shoreline_LU\\Special_Issue_Data\\MDlubcFEATURE2.csv"

SubEstFile     = "L:\\Hannam\\SAV\\Data\\Shoreline_LU\\Subest_Metrics.csv"
SubEstHucFile  = "L:\\Hannam\\SAV\\Data\\Shoreline_LU\\Special_Issue_Data\\SubEst_Huc.txt"
SubEstSAVFile  = "L:\\Hannam\\SAV\\Data\\NOAA_CSCOR\\SubEstSAV_Abundance_Summary_FINAL.csv"

VA_SAV_250file = "L:\\Hannam\\SAV\\Data\\Shoreline_LU\\Special_Issue_Data\\eucasstruVA_b9813sub250_lte250.csv"
VA_SAV_500file = "L:\\Hannam\\SAV\\Data\\Shoreline_LU\\Special_Issue_Data\\eucasstruVA_b9813sub250p_250to500m.csv"

MD_SAV_250file = "L:\\Hannam\\SAV\\Data\\Shoreline_LU\\Special_Issue_Data\\eucasstruMD_b9813sub250_lte250.csv"
MD_SAV_500file = "L:\\Hannam\\SAV\\Data\\Shoreline_LU\\Special_Issue_Data\\eucasstruMD_b9813sub250p_250to500m.csv"

#Load and Prep Maryland Data ------------------------------------------------- 
MD_SAV_Str <- read.csv(MD_SAV_StrFile)
MD_Sh_LU   <- read.csv(MD_Sh_LUFile)
MD_SAV_250 <- read.csv(MD_SAV_250file)
MD_SAV_500 <- read.csv(MD_SAV_500file)

#Join files for SAV within 250m and SAV from 250-500m
MD_SAV = full_join(
  select(MD_SAV_500, SAV_500=VALUE_1, sstruID = SSTRUID),
  select(MD_SAV_250, SAV_250=VALUE_1, sstruID = SSTRUID),
  'sstruID')

#Join those to the SAV_Str file
MD_SAV_Str1 <- left_join(MD_SAV_Str, VA_SAV, 'sstruID')
MD_SAV_Str1[is.na(MD_SAV_Str1$SAV_500), 'SAV_500']=0
MD_SAV_Str1[is.na(MD_SAV_Str1$SAV_250), 'SAV_250']=0

New_MD_Levels = c("Agriculture", "Bare", "Blank", "Comm_Ind_Other", "Marsh", "Forest", "Grass", "Comm_Ind_Other",
                  "Comm_Ind_Other","Comm_Ind_Other", "Residential", "Scrub_Shrub", "Forest")
length(New_MD_Levels)-length(levels(MD_Sh_LU$FEATURE2)) #SHOULD BE 0

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
            Perc_ScrubSh = sum(Scrub_Shrub)
            )

MD_SAV_Sh = left_join(MD_SAV_Str1, MD_Sh_LU_Wide, by = "Join_ID") %>%
  select(Structure = STRUCTURE, County = county, sav89_13m2, sav98_13m2, pothabm2, savPCTphab, 
         SubEst = SUBESTID, Length_m,
         Perc_Ag:Perc_ScrubSh,
         SAV_250, SAV_500)


#Load and Prep Virginia Data ------------------------------------------------- 
VA_SAV_Str <- read.csv(VA_SAV_StrFile)
VA_Sh_LU   <- read.csv(VA_Sh_LUFile)
VA_SAV_250 <- read.csv(VA_SAV_250file)
VA_SAV_500 <- read.csv(VA_SAV_500file)

#Join files for SAV within 250m and SAV from 250-500m
VA_SAV = full_join(
  select(VA_SAV_500, SAV_500=VALUE_1, sstruID = SSTRUID),
  select(VA_SAV_250, SAV_250=VALUE_1, sstruID = SSTRUID),
  'sstruID')


#Join those to the SAV_Str file
VA_SAV_Str1 <- left_join(VA_SAV_Str, VA_SAV, 'sstruID')
VA_SAV_Str1[is.na(VA_SAV_Str1$SAV_500), 'SAV_500']=0
VA_SAV_Str1[is.na(VA_SAV_Str1$SAV_250), 'SAV_250']=0
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
  summarise(Length_m = sum(SUM_Length_METERS),
            Perc_Ag = sum(Agriculture),
            Perc_Comm_Ind = sum(Comm_Ind_Other),
            Perc_Marsh = sum(Marsh),
            Perc_Res = sum(Residential),
            Perc_Forest = sum(Forest),
            Perc_Grass = sum(Grass), 
            Perc_ScrubSh = sum(Scrub_Shrub)
  )
    #This step creates 257 NAs in Length_m, presumably the whole LU file has NAs here
VA_SAV_Sh = left_join(VA_SAV_Str1, VA_Sh_LU_Wide, by= "Join_ID" ) %>%
  select(Structure = STRUCTURE, County = COUNTY, sav89_13m2, sav98_13m2, pothabm2, 
         savPCTphab, SubEst = SUBESTID, Length_m, Perc_Ag:Perc_ScrubSh,
         SAV_250, SAV_500)



summary(VA_SAV_Sh)
summary(MD_SAV_Sh)

#Combine----------------------------------------
SAV_Sh = rbind(VA_SAV_Sh, MD_SAV_Sh)

#GIS gave NA to 0 SAV segments, add fields where these are 0, but keep NAs:
SAV_Sh = SAV_Sh %>% mutate(SAV = sav89_13m2, Hab = pothabm2, OccHab = savPCTphab)

SAV_Sh[is.na(SAV_Sh$SAV),c('SAV')] = 0.00001 #This stands in for zero, but is filtered out by the model
SAV_Sh[is.na(SAV_Sh$Hab),'Hab'] = 0
SAV_Sh[is.na(SAV_Sh$OccHab),'OccHab'] = 0

#Reclass SStru-----------------------------------------------
New.Struc = c('Bulkhead', 'Other','Other','Marina','Marina','Marina','Wharf',
              'Other','Natural','Riprap','Other', 'Wharf','Other')
levels(SAV_Sh$Structure) = New.Struc
SAV_Sh$Structure = factor(SAV_Sh$Structure)

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
SAV_Sh = left_join(SAV_Sh, SubEst_Sel)#, by = c("SubEst"="CODE"))
SAV_Sh$SubEst = factor(SAV_Sh$SubEst)

#Drop NA subestuary records
SAV_Sh = SAV_Sh[SAV_Sh$SubEst!='',]
SAV_Sh$SubEst = factor(SAV_Sh$SubEst)

# Add other HUC scales
SAV_Sh = mutate(SAV_Sh, 
                HUC_6 = factor(substr(HUC_8,1,6)),
                HUC_4 = factor(substr(HUC_8,1,4)))
SAV_Sh$HUC_8 = factor(SAV_Sh$HUC_8)

### Filter Dataset -----------------------------------------
#Examine shoreline segment length, and select a reasonable subset of lengths--------------
hist(log(SAV_Sh$Length_m))

quantile(SAV_Sh$Length_m, na.rm=TRUE, probs = seq(0,1,.1))

#SAV_Sh1 = filter(SAV_Sh, Length_m < 450, Length_m > 25)
SAV_Sh1 = filter(SAV_Sh, Length_m < 125, Length_m > 75) #Range used by Patrick et al

SAV_Sh1 = filter(SAV_Sh, Structure %in% c('Bulkhead', 'Riprap','Natural'),
                 Length_m < 125, Length_m > 75)
SAV_Sh1$Structure = factor(SAV_Sh1$Structure)

plot(I(SAV_250)~SAV, data = SAV_Sh1)
plot(I(SAV_250+SAV_500)~SAV, data = SAV_Sh1)

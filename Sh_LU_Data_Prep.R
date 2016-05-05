library(tidyr)
library(dplyr)
#file.choose()

#Load and Prep Maryland Data ------------------------------------------------- 
MD_SAV_Str <- read.csv("L:\\Hannam\\SAV\\Data\\Shoreline_LU\\Special_Issue_Data\\MDeucasstru_spJ_SummarizeWit_sav98_13.csv")
MD_Sh_LU   <- read.csv("L:\\Hannam\\SAV\\Data\\Shoreline_LU\\Special_Issue_Data\\MDlubcFEATURE2.csv")

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

MD_SAV_Sh = left_join(MD_SAV_Str, MD_Sh_LU_Wide, by = "Join_ID") %>%
  select(Structure = STRUCTURE, County = county, sav89_13m2, sav98_13m2, pothabm2, savPCTphab, 
         SubEst = SUBESTID, Length_m,
         Perc_Ag:Perc_ScrubSh)


#Load and Prep Virginia Data ------------------------------------------------- 
VA_SAV_Str <- read.csv("L:\\Hannam\\SAV\\Data\\Shoreline_LU\\Special_Issue_Data\\VAeucasstru_spJ_SummarizeWit_sav98_13.csv")
VA_Sh_LU   <- read.csv("L:\\Hannam\\SAV\\Data\\Shoreline_LU\\Special_Issue_Data\\VAlubcFEATURE2.csv")

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
    
VA_SAV_Sh = left_join(VA_SAV_Str, VA_Sh_LU_Wide, by= "Join_ID" ) %>%
  select(Structure = STRUCTURE, County = COUNTY, sav89_13m2, sav98_13m2, pothabm2, 
         savPCTphab, SubEst = SUBESTID, Length_m, Perc_Ag:Perc_ScrubSh)

summary(VA_SAV_Sh)
summary(MD_SAV_Sh)

#Combine----------------------------------------
SAV_Sh = rbind(VA_SAV_Sh, MD_SAV_Sh)

#GIS gave NA to 0 SAV segments, add fields where these are 0, but keep NAs:
SAV_Sh = SAV_Sh %>% mutate(SAV = sav89_13m2, Hab = pothabm2, OccHab = savPCTphab)

SAV_Sh[is.na(SAV_Sh$SAV),c('SAV', 'Hab')]=0
SAV_Sh[is.na(SAV_Sh$OccHab),'OccHab']=0

#Add subestuary metrics--------------------------------------------------------------------
SubEst = read.csv("L:\\Hannam\\SAV\\Data\\Shoreline_LU\\Subest_Metrics.csv", skip = 1)

summary(SubEst)
SubEst_Sel = 
SubEst %>% select(CODE, NAME, BIGNAME, SALINZONE, LANDCAT, RiprapPerc, BulkheadPerc,
                 FOREST06, WETLAND06,IMPERVI11pct,DEV06, CROP06, MarshPhragPerc,MarshNoPhragPerc)

SAV_Sh = left_join(SAV_Sh, SubEst_Sel, by = c("SubEst"="CODE"))
SAV_Sh$SubEst = factor(SAV_Sh$SubEst)

#Drop NA subestuary records
SAV_Sh = SAV_Sh[SAV_Sh$SubEst!='',]
SAV_Sh$SubEst = factor(SAV_Sh$SubEst)

#Examine shoreline segment length, and select a reasonable subset of lengths--------------
hist(log(SAV_Sh$Length_m))

quantile(SAV_Sh$Length_m, na.rm=TRUE, probs = seq(0,1,.1))

SAV_Sh1 = filter(SAV_Sh, Length_m < 450, Length_m > 25)


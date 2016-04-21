library(tidyr)
library(dplyr)
file.choose()

#Load and Prep Maryland Data ------------------------------------------------- 
MD_SAV_Str <- read.csv("L:\\Hannam\\SAV\\Data\\Shoreline_LU\\Special_Issue_Data\\MDeucasstru_spJ_Summary.csv")
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
  select(Structure = STRUCTURE, County = county, sav89_13m2, pothabm2, savPCTphab, 
         SubEst = SUBESTID, Length_m,
         Perc_Ag:Perc_ScrubSh)


#Load and Prep Virginia Data ------------------------------------------------- 
VA_SAV_Str <- read.csv("L:\\Hannam\\SAV\\Data\\Shoreline_LU\\Special_Issue_Data\\VAeucasstru_spJ_Summary.csv")
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
    
VA_SAV_Sh = left_join(VA_SAV_Str, VA_Sh_LU_Wide, by ) %>%
  select(Structure = STRUCTURE, County = COUNTY, sav89_13m2, pothabm2, savPCTphab, SubEst = SUBESTID, Length_m,
         Perc_Ag:Perc_ScrubSh)

library(dplyr)
library(ggplot2)

source("Sh_LU_Data_Prep.R")
summary(SAV_Sh)



#Examine shoreline segment length, and select a reasonable subset of lengths
hist(log(SAV_Sh$Length_m))

quantile(SAV_Sh$Length_m, na.rm=TRUE, probs = seq(0,1,.1))

SAV_Sh1 = filter(SAV_Sh, Length_m < 450, Length_m > 25)


ggplot(SAV_Sh1, aes(x = Structure, y = Perc_Comm_Ind)) +
         geom_boxplot()
          geom_point()

          
SubEst_Compare = SAV_Sh %>% group_by(SubEst) %>%
  summarise(SAV_sum = sum(SAV/1000^2)) %>%
  left_join(SubEst, by = c('SubEst'='CODE'))

plot(SAV5yrTot~SAV_sum, data = SubEst_Compare)

ggplot(SubEst_Compare, aes(x = SubEst, y = I(SAV_sum/SAV5yrTot)))+
  geom_point()+geom_hline(yintercept=1)+coord_flip()


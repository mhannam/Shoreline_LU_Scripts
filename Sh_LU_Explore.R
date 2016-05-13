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
  left_join(SubEst, by = c('SubEst'='CODE')) %>%
  mutate(SubEst_SAV = SAV5yrTotA*AREAkm2/100)

plot(SubEst_SAV~SAV_sum, data = SubEst_Compare)
abline(a=0, b=1)
identify(SubEst_Compare$SAV_sum, SubEst_Compare$SubEst_SAV, labels = SubEst_Compare$SubEst, plot=TRUE)
#identify(plot(SubEst_SAV~SAV_sum, data = SubEst_Compare),label = SubEst_Compare$SubEst, plot=TRUE)

ggplot(SubEst_Compare, aes(x = SubEst, y = I(SAV_sum-SubEst_SAV)))+
  geom_point()+geom_hline(yintercept=0)+coord_flip()#+facet_wrap(~SALINZONE)

ggplot(SubEst_Compare, aes(x = SubEst_SAV, y = SAV_sum))+
  geom_point()+geom_abline(intercept=0,slope=1)

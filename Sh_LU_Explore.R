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

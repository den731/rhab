library(car)
library(ggplot)
####### Import Data #################
MCSUMresponseAverage <- read_csv("MCSUMresponseAverage.csv", col_names = TRUE)
LOGTransformed <- read_csv("LOGTransformed.csv", col_names = TRUE)
UltimateFactor <- read_csv("UltimateFactor.csv", col_names = TRUE)
#########################################
plot(LOGTransformed$OP~LOGTransformed$doy)
plot(LOGTransformed$phyco~LOGTransformed$doy)
plot(LOGTransformed$temp3~LOGTransformed$doy)
plot(LOGTransformed$turb~LOGTransformed$doy)

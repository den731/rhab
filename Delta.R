library(dplyr)
# install.packages("corrplot")
library(corrplot)
library(tidyverse)
DELTA <- UltimateFactor %>%
  rowwise() %>%
  mutate(Delta=ELISA-SUM) %>%
  summarise_all(funs(mean(., na.rm=TRUE)))


table <- DELTA %>%
  select(19:33, Delta) 
 
corrtable <- round(cor(table,  use="complete.obs"),2)
setEPS()
postscript(file="congener_matrix.eps")
corrplot.mixed(corrtable, 
               
               diag = "l",
               lower = "number",
               lower.col = "black",
               
               tl.pos = "lt",
               tl.col="black", 
               tl.srt=45, 
               tl.cex=0.6,
               number.font =1.25,
               insig = "blank",
               addgrid.col = "grey",
               mar=c(0,0,1,0),
               number.cex= 0.571)
dev.off()
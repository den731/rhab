################# SUBSET PACKAGES ####################
library(leaps)
# library(tidyverse)
# library(dplyr)
# Import data ##########################
MCSUMresponseAverage <- read_csv("MCSUMresponseAverage.csv", col_names = TRUE)
LOGTransformed <- read_csv("LOGTransformed.csv", col_names = TRUE)
UltimateFactor <- read_csv("UltimateFactor.csv", col_names = TRUE)

################# PLOTS ###############################

# Sum as response variable
Fullsubset <- regsubsets(SUM ~ .,
                         nbest = 5,
                         really.big = T,
                         method="exhaustive",
                         nvmax =3,
                         data=MCSUMresponseAverage)
# 16sRNA as response variable
zFull2 <- regsubsets(X16SRNA ~.,
                     nbest=5,
                     nvmax=3,
                     really.big = T,
                     MCSUMresponseAverage, 
                     method = "exhaustive")

ReducedSUM <- regsubsets(SUM ~ turb + 
                          WtShArea + 
                          OP +
                          phyco,
                         nvmax = 3,
                         nbest=5,
                        data=MCSUMresponseAverage)

ReducedX16 <- regsubsets(X16SRNA ~ chloro +
                           turb +
                           hobotemp +
                           LkWshRatio +
                           precip30 +
                           Forest,
                         nvmax = 3,
                         nbest = 5,
                         data=MCSUMresponseAverage)

############### Show plots ###########################

# SUM plot
plot(Fullsubset)
# 16sRNA plot
plot(zFull2)

# reduced SUM plot
setEPS()
postscript(file="subset2.eps")
plot(ReducedSUM)
dev.off()
# reduced 16srna plot
setEPS()
postscript(file="subset3.eps")
plot(ReducedX16)
dev.off()
####################### [Thesis Subset.eps] #########
# SUM as response variable
setEPS()
postscript(file="Subset.eps")
plot(Fullsubset)
dev.off()

#########################[Thesis Subset.eps]########




####################### [Thesis Subset1.eps] #########

#16sRNA as response
setEPS()
postscript(file="Subset1.eps")
plot(zFull2)
dev.off()


####################### [Thesis Subset1.eps] #########




################ Extraneous ####################

## Somewhat nicer, sometimes...
zb <- summary(Fullsubset)
bic <- zb$bic
matrix <- zb$which 
rownames(matrix) <- bic
library(reshape2)
meltedz <- melt(matrix)
melted <- meltedz %>% filter(Var1 < -4.4790) 
ggplot(melted, aes(x= Var2, y = Var1, fill =  value)) +
  geom_tile() +
  scale_fill_manual(values = c("white", "black")) +
  theme(legend.position = "none",axis.text.x=element_text(size=5,angle = 45, hjust =1)) +
  scale_y_reverse() +
  ylab("BIC") +
  xlab("Predictor")
ggsave("subset.eps", device = "eps")


# another for 16srna
zb <- summary(zFull2)
bic <- zb$bic
matrix <- zb$which 
rownames(matrix) <- bic
library(reshape2)
meltedz <- melt(matrix)
melted <- meltedz %>% filter(Var1 < -23.5334) 
ggplot(melted, aes(x= Var2, y = Var1, fill =  value)) +
  geom_tile() +
  scale_fill_manual(values = c("white", "black")) +
  theme(legend.position = "none",axis.text.x=element_text(size=5,angle = 45, hjust =1)) +
  scale_y_reverse() +
  ylab("BIC") +
  xlab("Predictor")
ggsave("subset2.eps", device="eps")





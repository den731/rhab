library(car)
library(lme4)
library(leaps)

#### Import Data #################
# Averaged then Log transformed. Excluded ELISA, N=29
MCSUMresponseAverage <- read.csv("MCSUMresponseAverage.csv", header = TRUE)
# Log Transformed data N=116
LOGTransformed <- read.csv("LOGTransformed.csv", header = TRUE)
# Raw data N=116
UltimateFactor <- read.csv("UltimateFactor.csv", header = TRUE)


#################### DR. RAFFEL 11/05/18 ##################








































































#####################################
###########SUBSET####################
#####################################

# MCSUMresponeseaverage does  not contain ELISA results
Fullsubset <- regsubsets(SUM ~ .,
                         nbest = 5,
                         really.big = T,
                         method="exhaustive",
                         nvmax =3,
                         data=MCSUMresponseAverage)
plot(Fullsubset)

# ^^from full subset,
# Drop mcye, 16s rRNA

# Top 3 MCYE, Max_Depth, LkWshRatio, Water, precip3
# MCYE was chosen most

sub1 <- regsubsets(SUM ~ .,
                   nbest = 5,
                   really.big = T,
                   method = "exhaustive",
                   force.out = c("MCYE"),
                   nvmax = 3,
                   data=MCSUMresponseAverage)
plot(sub1)
# ^^
# Top 3 Max_Depth, Barren, LkWshRatio, Water, TKN
# Max_Depth and Barren chosen most
# Lets Remove Max_Depth

sub2 <- regsubsets(SUM ~ .,
                   nbest = 5,
                   really.big = T,
                   method = "exhaustive",
                   force.out = c("MCYE", "Max_Depth"),
                   nvmax = 3,
                   data=MCSUMresponseAverage)
plot(sub2)
# Top 3 OP, wtemp, conduc and pH.
# pH and OP was included the most.

sub3 <- regsubsets(SUM ~ .,
                   nbest = 5,
                   really.big = T,
                   method = "exhaustive",
                   force.out = c("MCYE", "Max_Depth", "OP", "pH"),
                   nvmax = 3,
                   data=MCSUMresponseAverage)
plot(sub3)
# Top 3 TKN,turb, chloro and phyco, precip3 and precip30
# Lets remove chlor and phyco
sub4 <- regsubsets(SUM ~ .,
                   nbest = 5,
                   really.big = T,
                   method = "exhaustive",
                   force.out = c("MCYE", "Max_Depth", "OP", "pH", "chloro", "phyco"),
                   nvmax = 3,
                   data=MCSUMresponseAverage)

plot(sub4)

sub5 <- regsubsets(SUM ~ .,
                   nbest = 5,
                   really.big = T,
                   method = "exhaustive",
                   force.out = c("MCYE", "Max_Depth", "OP","turb"),
                   nvmax = 3,
                   data=MCSUMresponseAverage)
plot(sub5)
sub6 <- regsubsets(SUM ~ .,
                   nbest = 5,
                   really.big = T,
                   method = "exhaustive",
                   force.out = c("MCYE", "Max_Depth",  "OP", "turb", "phyco", "chloro"),
                   nvmax = 3,
                   data=MCSUMresponseAverage)
plot(sub6, scale=c("Cp"))

# OP associated with Land use?
sub6 <- regsubsets(OP ~ Developed + Forest + Agriculture + Barren + Shrubs + Herbaceous + Wetlands,
                   nbest = 5,
                   method = "exhaustive",
                   nvmax = 3,
                   data=MCSUMresponseAverage)

plot(sub6)
sub7 <- regsubsets(OP ~ Forest + Agriculture + Barren + Shrubs + Herbaceous + Wetlands,
                   nbest = 5,
                   method = "exhaustive",
                   nvmax = 3,
                   data=MCSUMresponseAverage)
plot(sub7)
sub7 <- regsubsets(OP ~ Forest + Agriculture + Barren + Shrubs + Wetlands,
                   nbest = 5,
                   method = "exhaustive",
                   nvmax = 3,
                   data=MCSUMresponseAverage)
plot(sub7)
#################
model <- lm(SUM~TKN,
            data=MCSUMresponseAverage)
Anova(model)
plot(MCSUMresponseAverage$SUM~MCSUMresponseAverage$TKN)
abline(model)
# Not signifigant, but positive correlation (p=0.10)
model <- lm(NO3~Developed,
            data=MCSUMresponseAverage)
Anova(model)

model <- lmer(OP ~ Developed +
                (1|LK_CODE),
              data=LOGTransformed)
Anova(model, test.statistic = "F")
summary(model)
# No

model <- lm(OP ~ Developed,
            data=MCSUMresponseAverage)
Anova(model)
plot(model)
model <- lm(SUM~precip3,
            data=MCSUMresponseAverage)
plot(MCSUMresponseAverage$SUM~MCSUMresponseAverage$precip3)
abline(model)
# Nope

model <- lm(SUM~OP,
            data=MCSUMresponseAverage)
Anova(model)
summary(model)
plot(MCSUMresponseAverage$SUM~MCSUMresponseAverage$OP)
abline(model)
# Outlier on row 26 and row 4
Omitted <- MCSUMresponseAverage %>%
  slice(c(-4,-26))

model <- lm(SUM~OP,
            data=Omitted)
plot(Omitted$SUM~Omitted$OP)
abline(model)
Anova(model)

Omitted1 <- Omitted %>%
  slice(c(-24))

model <- lm(SUM~OP,
            data=Omitted1)
plot(Omitted1$SUM~Omitted1$OP)
abline(model)
Anova(model)

# Ok, so orthophosphate with all outliers removed, it still comes signifigant. 
# Does watershed area explain the errors?
model <- lm(SUM~OP,
            data=MCSUMresponseAverage)

plot(residuals(model)~MCSUMresponseAverage$WtShArea)
abline(lm(residuals(model)~MCSUMresponseAverage$WtShArea))
Anova(lm(residuals(model)~MCSUMresponseAverage$WtShArea))


# Does lake area and lake area to watershed ratio have a role?

model <- lm(SUM~Lk)

model <- lmer(SUM~OP + 
                (1|LK_CODE),
              data=LOGTransformed)

Anova(model,test.statistic="F")
# p=0.07

# Does 16s rRNA good at predicting MC?
model <- lm(SUM ~ X16SRNA,
            data=MCSUMresponseAverage)
Anova(model)

# Or mcye?
model <- lm(SUM ~ MCYE,
            data=MCSUMresponseAverage)
Anova(model)
plot(model)

plot(MCSUMresponseAverage$SUM~MCSUMresponseAverage$MCYE)
abline(model)
################


# Hypothesis ##################################################
# SUM~Developed ####################################

Model <- lm(SUM~Developed, 
            data=MCSUMresponseAverage)
Anova(Model, test.statistic = "F")
summary(Model)
plot(Model)
acf(MCSUMresponseAverage$turb, type = "partial", na.action = na.pass)
# Residuals are ok...Not signifigant though

Model <- lmer(SUM~Developed +
                (1|LK_CODE),
              data=LOGTransformed)
summary(Model)
Anova(Model, test.statistic ="F")
coef(Model)
# Not signifigant

# Is TN:TP associated with SUM?

model <- lm(SUM ~ TNTP,
            data=MCSUMresponseAverage)
Anova(model)
# No

model <- lm(SUM ~ TN + TP + TN*TP,
            data=MCSUMresponseAverage)
anova(model)
# No



# Is Developed land use associated with nutrients?


# Phyco ############################################

Model <- lm(SUM~phyco,
          data = MCSUMresponseAverage)
Anova(Model, test.statistic = "F")
plot(Model)
# Driven by outlier...
# Remove outlier and retest
Omitted <- MCSUMresponseAverage %>% 
  slice(-4) # Remove row 4
Model <- lm(SUM~phyco,
            data = Omitted)
Anova(Model, test.statistic = "F")
plot(Model)
# Its ok

# Stepwise ####################################
# SUM as response
## From subset analysis, full model is
# turb, wtsharea,op,phyco
# Subset analysis finds sum ~ turb + wtsharea to be the best. 
# Lets see if backward regression finds the same 

fullmodel <- lm(SUM ~ turb +
                  WtShArea +
                  OP +
                  phyco,
                data=MCSUMresponseAverage)
Anova(fullmodel, test.statistic = "F")
plot(residuals(fullmodel)~MCSUMresponseAverage$precip5)
abline(residuals(fullmodel)~MCSUMresponseAverage$precip5)
# Remove OP
Model1 <- lm(SUM ~ turb +
                  WtShArea +
                  phyco,
                data=MCSUMresponseAverage)
Anova(Model1, test.statistic = "F")

# Remove phyco
Model2 <- lm(SUM ~ turb +
                 WtShArea,
               data=MCSUMresponseAverage)
Anova(Model2, test.statistic = "F")
summary(Model2)
# Test them as single predictors

Model3 <- lm(SUM ~ turb,
               data=MCSUMresponseAverage)


plot(residuals(Model3)~MCSUMresponseAverage$WtShArea,
     xlab="log10(Watershed Area)",
     ylab="Residuals of log10(MC) ~ log10(turbidity)" )
abline(lm(residuals(Model3)~MCSUMresponseAverage$WtShArea))

Anova(Model3, test.statistic = "F")

Model4 <- lm(SUM ~ WtShArea,
               data=MCSUMresponseAverage)

Anova(Model4, test.statistic = "F")
summary(Model4)

plot(residuals(Model4)~MCSUMresponseAverage$turb)
abline(lm(residuals(Model4)~MCSUMresponseAverage$turb))

plot(residuals(Model2))
plot(Model2)
abline(Model2)
# Is adding wt area decrease rss?
anova(Model3,Model2)
anova(Model3,Model2,test="Chisq")

model <- lmer(SUM ~ turb +
                WtShArea +
                (1|LK_CODE),
              data=LOGTransformed)
summary(model)
Anova(model, test.statistic = "F")

model2 <- lmer(SUM ~ turb +
                (1|LK_CODE),
              data=LOGTransformed)
summary(model)
Anova(model,  test.statistic = "F")

anova(model2,model)
model <- lmer(SUM ~ WtShArea +
                (1|LK_CODE),
              data=LOGTransformed)
summary(model)
Anova(model,  test.statistic = "F")

model <- lm(SUM ~ WtShArea,
            data=MCSUMresponseAverage)

summary(model)


plot(Model2)

AIC(fullmodel,Model1,Model2,Model3,Model4)
stargazer(fullmodel,Model1,Model2,Model3, keep.stat = c("ll"))

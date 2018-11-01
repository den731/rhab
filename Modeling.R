library(car)
library(lme4)
library(dplyr) # This lets you use the pipe function: %>% 
# For creating statistical tables, I use stargazer
# Which allows latex tables

# install.packages("stargazer")
# library(stargazer)

# install.packages("MuMIn") 

# Import Data ###################################################
MCSUMresponseAverage <- read.csv("MCSUMresponseAverage.csv", header = TRUE)
LOGTransformed <- read.csv("LOGTransformed.csv", header = TRUE)
UltimateFactor <- read.csv("UltimateFactor.csv", header = TRUE)

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

BIC(fullmodel,Model1,Model2,Model3,Model4)
stargazer(fullmodel,Model1,Model2,Model3, keep.stat = c("ll"))

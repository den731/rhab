library(car)
library(lme4)

# For creating statistical tables, I use stargazer
# Which allows latex tables

# install.packages("stargazer")
library(stargazer)

# install.packages("MuMIn") 

# Hypothesis ##################################################
# SUM~Developed ####################################
Model <- lm(SUM~Developed, 
            data=MCSUMresponseAverage)
Anova(Model, test.statistic = "F")
summary(Model)
plot(Model)
# Residuals are ok...Not signifigant though

Model <- lmer(SUM~Developed +
                (1|LK_CODE),
              data=LOGTransformed)
summary(Model)
Anova(Model, test.statistic ="F")
coef(Model)

Model1  <- lmer(SUM~OP + phyco + (1|LK_CODE),
		data=LOGTransformed)

summary(Model1)
Anova(Model1, test.statistic = "F")

# Forest ##########################################
# Average values of temperature for each month
UltimateFactor %>% group_by(Month) %>% summarise(mean=median(hobotemp, na.rm=T))

LOGTransformed %>% group_by(Month) %>% summarise(mean=mean(hobotemp, na.rm=T))

# Phyco ############################################

Model <- lm(SUM~phyco,
          data = MCSUMresponseAverage)
Anova(Model, test.statistic = "F")
plot(Model)


# Stepwise ####################################
# SUM as response
## From subset analysis, full model is
# turb, wtsharea,op,phyco

fullmodel <- lm(SUM ~ turb +
                  WtShArea +
                  OP +
                  phyco,
                data=MCSUMresponseAverage)
Anova(fullmodel, test.statistic = "F")

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



Model3 <- lm(SUM ~ turb,
               data=MCSUMresponseAverage)

Anova(Model3, test.statistic = "F")

Model4 <- lm(SUM ~ phyco,
               data=MCSUMresponseAverage)

Anova(Model4, test.statistic = "F")

anova(Model3,Model1)
anova(Model3,Model2)

plot(Model2)

BIC(fullmodel,Model1,Model2,Model3,Model4)
install.packages("stargazer")
library(stargazer)
stargazer(fullmodel,Model1,Model2,Model3, keep.stat = c("ll"))

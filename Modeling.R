library(car)
library(lme4)
# install.packages("MuMIn") 

###################################################
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

UltimateFactor %>% group_by(Month) %>% summarise(mean=median(hobotemp, na.rm=T))

LOGTransformed %>% group_by(Month) %>% summarise(mean=mean(hobotemp, na.rm=T))

# Phyco ############################################

Model <- lm(SUM~phyco,
          data = MCSUMresponseAverage)
Anova(Model, test.statistic = "F")
plot(Model)

## Import ##
MCSUMresponseAverage <- read.csv("MCSUMresponseAverage.csv", header = TRUE)
LOGTransformed <- read.csv("LOGTransformed.csv", header = TRUE)
UltimateFactor <- read.csv("UltimateFactor.csv", header = TRUE)

#### SUM~Turb graph

## Get coeffient
cf <- round(coef(lm(SUM~turb, data=MCSUMresponseAverage)), 2) 
## Get equation
eq <- paste0("log10(MC) = ", cf[2], 
             "log10(Turbidity) ", cf[1])
plot(MCSUMresponseAverage$SUM~MCSUMresponseAverage$turb, xlab="log10(Turbidity)", ylab = "log10(MC SUM)")
abline(lm(MCSUMresponseAverage$SUM~MCSUMresponseAverage$turb))
mtext(eq, 3, line=-2)
summary(lm(MCSUMresponseAverage$SUM~MCSUMresponseAverage$turb))



# SUM~mcye

## Get coeffient
cf <- round(coef(lm(SUM~MCYE, data=MCSUMresponseAverage)), 2) 
## Get equation
eq <- paste0("log10(MC) = ", cf[2], 
             "log10(mcyE) ", cf[1])
plot(MCSUMresponseAverage$SUM~MCSUMresponseAverage$MCYE, xlab="log10(mcyE)", ylab = "log10(MC SUM)")
abline(lm(MCSUMresponseAverage$SUM~MCSUMresponseAverage$MCYE))
mtext(eq, 3, line=-2)
summary(lm(MCSUMresponseAverage$SUM~MCSUMresponseAverage$MCYE))






#### SUM~MCYE

## Get coeffient
cf <- round(coef(lm(SUM~MCYE, data=MCSUMresponseAverage)), 2) 
## Get equation
eq <- paste0("log(MC Sum) = ", 
             abs(cf[2]), " log(mcyE) ", cf[1])
plot(MCSUMresponseAverage$SUM~MCSUMresponseAverage$MCYE, xlab="log(mcyE) ", ylab = "Log(MC Sum)")
abline(lm(MCSUMresponseAverage$SUM~MCSUMresponseAverage$MCYE))
mtext(eq, 3, line=-2)




#### SUM~OP
## Get coeffient
cf <- round(coef(lm(SUM~OP, data=MCSUMresponseAverage)), 2) 
## Get equation
eq <- paste0("log10(MC Sum) = ", 
             abs(cf[2]), " log10(OP) +", cf[1])
plot(MCSUMresponseAverage$SUM~MCSUMresponseAverage$OP, xlab="log(ortho-P)", ylab = "log(MC Sum)")
abline(lm(MCSUMresponseAverage$SUM~MCSUMresponseAverage$OP))
mtext(eq, 3, line=-2)

omitted <- MCSUMresponseAverage %>%
  slice(c(-25,-4,-26))

## Get coeffient
cf <- round(coef(lm(SUM~OP, data=omitted)), 2) 
## Get equation
eq <- paste0("log(MC Sum) = ", 
             abs(cf[2]), " log(ortho-P) +", cf[1])

plot(omitted$SUM~omitted$OP, 
     xlab="log(ortho-P)",
     ylab = "log(SUM)")
abline(lm(omitted$SUM~omitted$OP))
mtext(eq, 3, line=-2)


# Developed.eps




# Forest.eps
setEPS()
postscript(file="forest.eps")
#par(mfrow=c(1,2))
plot(MCSUMresponseAverage$X16SRNA~MCSUMresponseAverage$Forest, ylab = "log10(16s rRNA+45)", xlab = "% Forest Land-use")
abline(lm(X16SRNA~Forest, data=MCSUMresponseAverage))
#title("A", adj = 0)
#plot(MCSUMresponseAverage$SUM~MCSUMresponseAverage$MCYE, xlab = "log10(mcyE) gene copies", ylab = "log10(MC) ")
#abline(lm(SUM~MCYE, data=MCSUMresponseAverage))
#title("B", adj = 0)
dev.off()

### Agriculture.eps

require(lme4)
setEPS()
postscript(file="Agriculture.eps")
plot(MCSUMresponseAverage$X16SRNA~MCSUMresponseAverage$Agriculture, xlab = "Percentage of Agriculture Land-use", ylab = "Log10(16S rRNA)")
abline(lm(X16SRNA~Agriculture, data=MCSUMresponseAverage))
dev.off()



### plot2.eps

setEPS()
postscript(file="plot2.eps")
par(mfrow=c(1,2))
plot(MCSUMresponseAverage$SUM~MCSUMresponseAverage$turb, xlab = "log10(Turbidity)", ylab = "log10(MC)")
abline(lm(SUM~turb, data=MCSUMresponseAverage))
title("A", adj = 0)
plot(MCSUMresponseAverage$SUM~MCSUMresponseAverage$WtShArea, xlab = "log10(Watershed Area)" , ylab = " ")
abline(lm(SUM~WtShArea, data=MCSUMresponseAverage))
title("B", adj = 0)
dev.off()



### plot1.eps

setEPS()
postscript(file="plot1.eps", width = 8, height = 8)
par(mfrow=c(1,2))
plot(MCSUMresponseAverage$OP~MCSUMresponseAverage$turb, ylab = "log10(OP)", xlab = "log10(turb)")
abline(lm(OP~turb, data=MCSUMresponseAverage))
title("A", adj = 0)
plot(Omit$OP~Omit$turb, xlab = "log10(turb) ", ylab = "log10(OP)")
abline(lm(OP~turb, data=Omit))
title("B", adj = 0)
dev.off()

# Ommited outliers 4 and 26
Omit <- MCSUMresponseAverage %>% slice(c(-26,-4))

Model <- (lm(OP~turb, data=Omit))
summary(Model)
Anova(Model, test.statistics="F")

# residual.eps
Model3 <- lm(SUM ~ turb,
               data=MCSUMresponseAverage)

setEPS()
postscript(file="residual.eps")
plot(residuals(Model3)~MCSUMresponseAverage$WtShArea,
     xlab="log10(Watershed Area)",
     ylab="Residuals of log10(MC) ~ log10(turbidity)" )
abline(lm(residuals(Model3)~MCSUMresponseAverage$WtShArea))
dev.off()
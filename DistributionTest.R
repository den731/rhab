

########### Packages and variables ##############

require(fitdistrplus)
X16SRNA <- LOGTransformed$X16SRNA %>% na.omit() %>% as.numeric()
SUM <- LOGTransformed$SUM %>% na.omit() %>% as.numeric()


######## PLOTS  #############

#Plot distribution of 16srna before and after log transformation
par(mfrow=c(1,2)) 
plot(density(UltimateFactor$X16SRNA, na.rm = T), xlab = "16s rRNA Gene cp/mL of Sample",
     main = "A")
plot(density(LOGTransformed$X16SRNA, na.rm = T), xlab = "log10(X16SRNA+MDL)",
     main = "B")


# Plot distribution of mass spec results before and after transformation
par(mfrow=c(1,2)) 
plot(density(UltimateFactor$SUM, na.rm = T), xlab = "SUM",
     main = "A")
plot(density(LOGTransformed$SUM, na.rm = T), xlab = "log10(SUM+MDL)",
     main = "B")

# Fit diagnostic of a normal distribution onto mass spec results
fit.norm <- fitdist(SUM, "norm", method = "mle")
plot(fit.norm)


# Detach package, otherwise having it loaded will break other packages!
detach("package:fitdistrplus")

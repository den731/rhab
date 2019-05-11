



####time1.eps

# day of year test
setEPS()
postscript(file="time1.eps")
plot(LOGTransformed$doy,LOGTransformed$SUM, xlab = "Day of the year", ylab = "log(MC+0.03)")
Model1<-lm(SUM~doy+I(doy^2),data=LOGTransformed)
Model2<-lm(SUM~doy,data=LOGTransformed)
curve(coef(Model1)[1]+coef(Model1)[2]*x+coef(Model1)[3]*x^2,add=TRUE)
text(190, 0.5, "July")
text(220, 0.5, "August")
text(250, 0.5, "September")
text(280, 0.5, "October")
dev.off()



#### time2.eps

# day of year test
setEPS()
postscript(file="time2.eps")
plot(LOGTransformed$doy,LOGTransformed$X16SRNA, xlab = "Day of the year", ylab = "log(X16SRNA+45)")
Model1<-lm(X16SRNA~doy+I(doy^2),data=LOGTransformed)
Model2<-lm(X16SRNA~doy,data=LOGTransformed)
curve(coef(Model1)[1]+coef(Model1)[2]*x+coef(Model1)[3]*x^2,add=TRUE)
text(190, 6, "July")
text(220, 6.6, "August")
text(260, 6, "September")
text(280, 6.6, "October")
dev.off()



#### [time3.eps]

setEPS()
postscript(file="time3.eps", width = 8, height = 10)
par(mfcol=c(2,1) , mai=c(0.8,0.9,0.3,0.3))
plot(LOGTransformed$doy,LOGTransformed$SUM,  xlab = " ", xaxt='n', ylab = "log10(MC+0.03)")
Model1<-lm(SUM~doy+I(doy^2),data=LOGTransformed)
Model2<-lm(SUM~doy,data=LOGTransformed)
curve(coef(Model1)[1]+coef(Model1)[2]*x+coef(Model1)[3]*x^2,add=TRUE)
text(190, 0.5, "July")
text(220, 0.5, "August")
text(250, 0.5, "September")
text(277, 0.7, "October")
title("A", adj = 0)
plot(LOGTransformed$doy,LOGTransformed$X16SRNA, xlab = "Day of the year", ylab = "log(X16SRNA+45)")
Model1<-lm(X16SRNA~doy+I(doy^2),data=LOGTransformed)
Model2<-lm(X16SRNA~doy,data=LOGTransformed)
curve(coef(Model1)[1]+coef(Model1)[2]*x+coef(Model1)[3]*x^2,add=TRUE)
text(190, 6, "July")
text(220, 6.6, "August")
text(260, 6, "September")
text(277, 6.6, "October")
title("B", adj = 0)
dev.off()


### roughdraft of Temprature 
plot(LOGTransformed$doy,LOGTransformed$wtemp, xlab = "Day of the year", ylab = "Water Temprature")
Model1<-lm(wtemp~doy+I(doy^2),data=LOGTransformed)
Model2<-lm(wtemp~doy,data=LOGTransformed)
curve(coef(Model1)[1]+coef(Model1)[2]*x+coef(Model1)[3]*x^2,add=TRUE)
text(190, 6, "July")
text(220, 6.6, "August")
text(260, 6, "September")
text(280, 6.6, "October")



### [time4.eps] 
setEPS()
postscript(file="time4.eps", width = 8, height = 10)
month1 <- LOGTransformed$Month
sum1  <- LOGTransformed$SUM
dna <- LOGTransformed$X16SRNA
par(mfcol=c(2,1), mai=c(0.8,0.9,0.3) )
boxplot(sum1~month1, xlab = " ", xaxt='n', ylab="log10(MC+0.03) ")
title("A", adj = 0)
boxplot(dna~month1, xlab = "Month" , ylab="log(X16SRNA+45)", names=c("July" ,"August" ,"September" , "October"))
title("B", adj=0)
dev.off()





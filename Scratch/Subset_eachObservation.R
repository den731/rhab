## Subset Modeling on Each Observation

### MC Sum as Response Variable

#### Subset Analysis


\newpage

```{r, fig.height=5, fig.cap="Subset Regression: Full Model Ranked by BIC"}

#The mastersheet is Log transformed and stored as "LOGTransformed". 
#This has all the variables, so we select only what we are interested.
# Just get all variables of interest. The dplyr package is used for data manipulations
MCSUMresponse <- LOGTransformed  %>% 
  select(-1:-7, -16:-28, -30:-47, -56, -57, -75:-83)
#--------------------------------------------

# Run regsubset 
regsubset.all <- regsubsets(SUM ~ ., 
                            MCSUMresponse, 
                            nbest=100,
                            nvmax =4,  
                            method = "exhaustive",
                            really.big = TRUE)
#-------------------------------------------
plot(regsubset.all) # Plot the regression subset. By default, the variables are scored by BIC
```







```{r, fig.cap="Subset Regression: Full Model Ranked by Adjusted R2"}
plot(regsubset.all, scale = "adjr2")
```


From both plots,  *mcyE*, turbidity, phyco, moderate, medium, and high impervious surface areas are frequently chosen. Orthophosphate, ammonia, mussel mass and mussel numbers is also included in the last few iterations.


\newpage

#### Step-wise Regression

We will start with the model including *mcyE*, turbidity, phyco, moderate, medium, high, orthophosphate, ammonia, and the mussel data. The first regression model with all chosen variables are shown.  Then one variable will be omitted if p > 0.05 based on type II test.


First Model-
  
  ```{r}
zmod1 <- lm(SUM~
              MCYE +
              turb +
              phyco +
              Medium_impervious+ 
              Moderate_impervious +
              High_impervious +
              OP +
              NH3 +
              LogMusselMass +
              LogMusselNum,
            data=MCSUMresponse)
z011 <- Anova(zmod1)
z011
```


```{r,include=FALSE}
zmod2 <- lm(SUM~
              MCYE +
              turb +
              phyco +
              Moderate_impervious +
              High_impervious +
              OP +
              NH3 +
              LogMusselMass +
              LogMusselNum,
            data=MCSUMresponse)
z022 <- Anova(zmod2)
z022
```

```{r,include=FALSE}
zmod3 <- lm(SUM~
              MCYE +
              turb +
              phyco +
              Moderate_impervious +
              OP +
              NH3 +
              LogMusselMass +
              LogMusselNum,
            data=MCSUMresponse)
z033 <- Anova(zmod3)
z033
```

```{r,include=FALSE}
zmod4 <- lm(SUM~
              MCYE +
              turb +
              phyco +
              Moderate_impervious +
              OP +
              LogMusselMass +
              LogMusselNum,
            data=MCSUMresponse)
z044 <- Anova(zmod4)
z044
```


```{r,include=FALSE}
zmod4 <- lm(SUM~
              MCYE +
              turb +
              Moderate_impervious +
              OP +
              LogMusselMass +
              LogMusselNum,
            data=MCSUMresponse)
z044 <- Anova(zmod4)
z044
```



```{r,include=FALSE}
zmod5 <- lm(SUM~
              MCYE +
              turb +
              Moderate_impervious +
              OP +
              LogMusselMass,
            data=MCSUMresponse)
z055 <- Anova(zmod5)
z055
```



The last four variables-
  
  ```{r}
zmod6 <- lm(SUM~
              MCYE +
              turb +
              Moderate_impervious +
              OP,
            data=MCSUMresponse)
z066 <- Anova(zmod6)
z066
```

\newpage

```{r}
zTOP <- lm(SUM~
             MCYE +
             turb +
             Moderate_impervious,
           data=MCSUMresponse)


zMCYE <- lm(SUM~
              MCYE ,
            data=MCSUMresponse)

zTURB <- lm(SUM~
              turb,
            data=MCSUMresponse)

zMOD <- lm(SUM~
             Moderate_impervious,
           data=MCSUMresponse)
anova(zTOP)
anova(zMCYE)
anova(zTURB)
anova(zMOD)


```

The final three variables are *mcyE*, turbidity, and moderate impervious surfaces. Each variable by itself has a  significant relationship with MC Sum. Lets observe each scatterplot and best fit line. Also the residuals will be observed.


\newpage

#### Scatterplots and Residuals


```{r}
## Get coeffient
cf <- round(coef(lm(SUM~MCYE, data=MCSUMresponse)), 2) 
## Get equation
eq <- paste0("MC SUM = ", 
             ifelse(sign(cf[2])==1, " + ", " - "), abs(cf[2]), " mcyE ", cf[1])
plot(MCSUMresponse$SUM ~ MCSUMresponse$MCYE, xlab="mcyE", ylab="MC Sum")
abline(lm(SUM~MCYE, data=MCSUMresponse))
mtext(eq, 3, line=-2)

model1<- lm(SUM~turb+Moderate_impervious, data=MCSUMresponse)
```

```{r}
zsingle1 <- lm(SUM~MCYE, data=MCSUMresponse, na.action=na.omit)
layout(matrix(c(1,2,3,4),2,2))
plot(zsingle1)
```


\newpage

```{r}
## Get coeffient
cf <- round(coef(lm(SUM~turb, data=MCSUMresponse)), 2) 
## Get equation
eq <- paste0("MC SUM = ", 
             ifelse(sign(cf[2])==1, " + ", " - "), abs(cf[2]), " Turbidity ", cf[1])
plot(MCSUMresponse$SUM ~ MCSUMresponse$turb, xlab="Turbidity", ylab="MC Sum")
abline(lm(SUM~turb, data=MCSUMresponse))
mtext(eq, 3, line=-2)
```

```{r}
zsingle2 <- lm(SUM~turb, data=MCSUMresponse, na.action=na.omit)
layout(matrix(c(1,2,3,4),2,2))
plot(zsingle2)

```

\newpage

```{r}
## Get coeffient
cf <- round(coef(lm(SUM~Medium_impervious, data=MCSUMresponse)), 2) 
## Get equation
eq <- paste0("MC SUM = ", 
             ifelse(sign(cf[2])==1, " + ", " - "), abs(cf[2]), " Area of Imp. Surfaces ", cf[1])
plot(MCSUMresponse$SUM ~ MCSUMresponse$Medium_impervious, xlab="Area of Medium Impervious Surface", ylab="MC Sum")
abline(lm(SUM~Medium_impervious, data=MCSUMresponse))
mtext(eq, 3, line=-2)
```



```{r}
zsingle3 <- lm(SUM~Medium_impervious, data=MCSUMresponse, na.action=na.omit)
layout(matrix(c(1,2,3,4),2,2))
plot(zsingle2)

```

\newpage


### MC Sum from SPATTs as Response Variable

#### Subset analysis

```{r}

SPATTSresponse <- LOGTransformed  %>% 
  select(-1:-7, -16:-29, -30:-45,-47, -56, -57, -75:-83)

regsubsets.spatts <- regsubsets(S_SUM ~ ., 
                                SPATTSresponse, 
                                nbest=100,
                                nvmax =4,  
                                method = "exhaustive",
                                really.big = TRUE)
plot(regsubsets.spatts)
```

Orthophosphate, nitrate+nitrite, ammonia, *mcyE*, barren, turbidity, herbaceous and Total_impervious impervious surfaces.


#### Step-wise Regression

```{r}
zsmod1 <- lm(S_SUM ~
               OP +
               NOX +
               NH3 +
               MCYE +
               Barren +
               turb +
               Herbaceous +
               Total_impervious,
             data = SPATTSresponse)
Anova(zsmod1)
```

```{r,include=FALSE}
zsmod2 <- lm(S_SUM ~
               OP +
               NH3 +
               MCYE +
               Barren +
               turb +
               Herbaceous +
               Total_impervious,
             data = SPATTSresponse)
Anova(zsmod2)
```

```{r,include=FALSE}
zsmod2 <- lm(S_SUM ~
               OP +
               MCYE +
               Barren +
               turb +
               Herbaceous +
               Total_impervious,
             data = SPATTSresponse)
Anova(zsmod2)
```

```{r,include=FALSE}
zsmod2 <- lm(S_SUM ~
               MCYE +
               Barren +
               turb +
               Herbaceous +
               Total_impervious,
             data = SPATTSresponse)
Anova(zsmod2)
```

```{r,include=FALSE}
zsmod2 <- lm(S_SUM ~
               MCYE +
               turb +
               Herbaceous +
               Total_impervious,
             data = SPATTSresponse)
Anova(zsmod2)
```


```{r}
zsmod2 <- lm(S_SUM ~
               MCYE +
               Herbaceous +
               Total_impervious,
             data = SPATTSresponse)
Anova(zsmod2)
```

\newpage


#### Scatterplots and Residuals

```{r}
## Get coeffient
cf <- round(coef(lm(S_SUM~MCYE, data=SPATTSresponse)), 2) 
## Get equation
eq <- paste0("MC SUM from SPATTs = ", 
             ifelse(sign(cf[2])==1, " + ", " - "), abs(cf[2]), " mcyE + ", cf[1])
plot(SPATTSresponse$S_SUM ~ SPATTSresponse$MCYE, xlab="mcyE", ylab="MC Sum")
abline(lm(S_SUM~MCYE, data=SPATTSresponse))
mtext(eq, 1, line=-2)
```
```{r}
model1 <- lm(S_SUM~MCYE, data=SPATTSresponse, na.action=na.omit)
layout(matrix(c(1,2,3,4),2,2))
plot(model1)
```





\newpage

```{r}
cf <- round(coef(lm(S_SUM~Herbaceous, data=SPATTSresponse)), 2) 
eq <- paste0("MC SUM from SPATTs = ", 
             ifelse(sign(cf[2])==1, " + ", " - "), abs(cf[2]), " Herbaceous + ", cf[1])
plot(SPATTSresponse$S_SUM ~ SPATTSresponse$Herbaceous, xlab="Herbaceous", ylab="MC Sum")
abline(lm(S_SUM~Herbaceous, data=SPATTSresponse))
mtext(eq, 1, line=-2)
```


```{r}
model1 <- lm(S_SUM~Herbaceous, data=SPATTSresponse, na.action=na.omit)
layout(matrix(c(1,2,3,4),2,2))
plot(model1)
```

\newpage

```{r}
cf <- round(coef(lm(S_SUM~Total_impervious, data=SPATTSresponse)), 2) 
eq <- paste0("MC SUM from SPATTs = ", 
             ifelse(sign(cf[2])==1, " + ", " - "), abs(cf[2]), " Impervious Surfaces» + ", cf[1])
plot(SPATTSresponse$S_SUM ~ SPATTSresponse$Total_impervious, xlab="Impervious Surfaces", ylab="MC Sum")
abline(lm(S_SUM~Total_impervious, data=SPATTSresponse))
mtext(eq, 1, line=-2)
```


```{r}
model1 <- lm(S_SUM~Total_impervious, data=SPATTSresponse, na.action=na.omit)
layout(matrix(c(1,2,3,4),2,2))
plot(model1)
```


\newpage

### mcyE gene as Response

#### Subset Analysis


```{r}
zgene <- MCSUMresponse %>% select(-SUM)
zaFull <- regsubsets(MCYE ~ .,
                     nvmax=6, 
                     nbest=3, 
                     
                     really.big=TRUE,
                     zgene, 
                     method = "exhaustive")
plot(zaFull)



```

TP, 16s rRNA, wtemp, High_impervious, phyco, Forest, herbaceous, wetlands




\newpage

#### Step-wise Regression

```{r}
zymod1 <- lm(MCYE~
               TP +
               X16SRNA +
               wtemp +
               High_impervious +
               phyco +
               Forest +
               Herbaceous +
               Wetlands,
             data=MCSUMresponse)
zy11 <- Anova(zymod1)
zy11
```



```{r,include=FALSE}
zymod2 <- lm(MCYE~
               TP +
               X16SRNA +
               wtemp +
               High_impervious +
               phyco +
               Forest +
               Wetlands,
             data=MCSUMresponse)
zy12 <- Anova(zymod2)
zy12
```


```{r,include=FALSE}
zymod3 <- lm(MCYE~
               TP +
               X16SRNA +
               wtemp +
               High_impervious +
               phyco +
               Wetlands,
             data=MCSUMresponse)
zy13 <- Anova(zymod3)
zy13
```

```{r,include=FALSE}
zymod4 <- lm(MCYE~
               TP +
               X16SRNA +
               wtemp +
               High_impervious +
               Wetlands,
             data=MCSUMresponse)
zy14 <- Anova(zymod4)
zy14
```

```{r,include=FALSE}
zymod5 <- lm(MCYE~
               TP +
               X16SRNA +
               wtemp +
               High_impervious,
             data=MCSUMresponse)
zy15 <- Anova(zymod5)
zy15
```

```{r}
zymod6 <- lm(MCYE~
               TP +
               X16SRNA +
               wtemp,
             data=MCSUMresponse)
zy16 <- Anova(zymod6)
zy16
```

```{r}
zymod7 <- lm(MCYE~
               X16SRNA +
               wtemp,
             data=MCSUMresponse)
zy17 <- Anova(zymod7)
zy17
```



```{r}
zymod8 <- lm(MCYE~
               wtemp,
             data=MCSUMresponse)
zy18 <- Anova(zymod8)
zy18
```

\newpage
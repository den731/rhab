## Tables Grabsamples

library(stargazer)


third <- UltimateFactor %>%
  select(Nodul:ELISA)
stargazer(as.data.frame(third), 
          header=FALSE, 
          covariate.labels = 
            c("Nodularin",
              "{[D-Asp3]}MC-RR",
              "MC-RR", "MC-YR",
              "MC-HtyR", 
              "MC-LR", 
              "{[D-Asp3]}MC-LR",
              "MC-HilR",
              "MC-WR", 
              "MC-LA", 
              "MC-LY",
              "MC-LW", 
              "MC-LF", 
              "MC Sum from LC MS/MS ",
              "MC from ELISA"),
          notes = "Values are expressed as($\\mu$g of MC*${L^{-1}}$)", 
          notes.align = "r")




fourth <- MonthOrdered %>%
  select(39:53)
stargazer(as.data.frame(fourth),
          header=FALSE,
          digits = 0, 
          covariate.labels = 
            c("{[D-Asp3]}MC-RR", 
              "MC-RR", "Nodularin",
              "MC-YR", "MC-HtyR",
              "MC-LR",
              "{[D-Asp3]}MC-LR", 
              "MC-HilR", 
              "MC-WR", 
              "MC-LA",
              "MC-LY",
              "MC-LW", 
              "MC-LF", 
              "MC Sum from LC MS/MS", 
              "MC from ELISA"),
          notes = "Values are expressed as (ng of MC / gram of resin)", 
          notes.align = "r")



##  TABLE Nutrients




stargazer(
  as.data.frame(first.half), 
          header=FALSE, 
          covariate.labels = c(" Orthophosphate (mg P/L)",
                               "Nitrate+Nitrite (mg N/L)",
                               "Ammonia (mg N/L) ", 
                               "Total Phosphorus (mg P/L)",
                               "Total Kjeldahl Nitrogen (mg N/L)", 
                               "Total Nitrogen")
          )



## TABLE QPCR 


second <- UltimateFactor %>%
  select("X16SRNA", MCYE:SxtA)
stargazer(as.data.frame(second), header = FALSE,  digits = 0, covariate.labels = c("16S rRNA", "mcyE", "cyrA", "sxtA"), notes = "* Values are expressed as Gene copies/$\\mu$L", notes.align = "r" )





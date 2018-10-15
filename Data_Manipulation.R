





##################################################################################
##################################################################################
##################################################################################
# Libarys for data manipulation, import functions
library(tidyverse) # This loads dplyr which is primarily needed here.
# dplyr also helps with calculation. 
# Very easy to understand the syntax in my opinion
# Optionally instead of loading tidyverse, you can load dplyr
# library(dplyr)
library(lubridate)



# Import data, mastersheet excel file
zimported <- read.csv("MasterSheet.csv")

# NA are -999 so convert to NULL or NA
zimported[zimported==-999] <- NA 

# Drop id collumn, convert dates to something R understands
# Calculate total nitrogen and tn:tp ratio 
Master_Sheet <- zimported %>% 
  select(-id) %>% # Dropping ID collumn 
  mutate(Dates = ymd(zimported$Date)) %>% 
  mutate(TN = NO3 + TKN) %>%
  mutate(TNTP = TN/TP)

#######################

# Import precipitation data and lake stations for each lake
# Imported from QGIS output
zghcnd <- read.csv("GHCND.csv")
zzghcnd <- read.csv("zzghcnd.csv")

# Join precip data to lake by station
zjoined <- zghcnd %>% 
  full_join(zzghcnd, by = c("STATION" = "station")) %>% 
  drop_na(lk_code)

# Filter out collumns and add dates and months
zNOAA <- zjoined %>% 
  select(date = DATE, prcp = PRCP, LK_CODE = lk_code, tobs = TOBS) %>%
  mutate(Dates = ymd(date)) %>%
  select(Dates, prcp, LK_CODE, tobs) %>%
  mutate(Months.rain = (month(Dates, label=TRUE, abbr = FALSE)))


#######################


# Ecoregion Import
ecoregion <- read.csv(file="ecoregion.csv")

# Lake information. Takes lat and long, county and land use from qgis/arcgis calculation
Lake_info_comprehensive <- read.csv(file="Lake_info_comprehensive.csv")

# Imports hobo data, they are averaged in excel already.
# Averaged each month for every lake. 
zhobo <- read.csv(file="Hobo_MONTHLY.csv")

# Calculate land use percentages from Lake info sheet since
# they are in acres
info <- Lake_info_comprehensive %>% 
  rowwise() %>% 
  mutate(sum = sum(Water,Developed,Barren,Forest,Shrubs,
                   Herbaceous,Agriculture,Wetlands)) %>% 
  mutate_at(vars(Water:Wetlands), funs(./sum)) 

# Data Manipulation

# Order months by month instead of alphabetical. 
# Also calculate total Sum from SPATTS 
# Added a time interval for temporal joins

MonthOrdered <- as.data.frame(Master_Sheet) %>% 
  mutate(month = factor(month.name[Month], levels = month.name)) %>% 
  arrange(month) %>% 
  mutate_at(vars(
    S_D_Asp3_RR, S_MC_RR, S_Nodul,
    S_MC_YR, S_MC_HtyR, S_MC_LR,
    S_D_Asp3_RR, S_MC_HilR, S_MC_WR,
    S_MC_LA, S_MC_LY, S_MC_LW,
    S_MC_LF), funs(.*45/90)) %>% # Edited, fixed to per day
  # Converted spatts into nanogram per gram of resin
  rowwise() %>% 
  mutate(S_SUM = 
           sum(S_D_Asp3_RR, S_MC_RR, S_Nodul, 
               S_MC_YR, S_MC_HtyR, S_MC_LR,
               S_D_Asp3_RR, S_MC_HilR, S_MC_WR,
               S_MC_LA, S_MC_LY, S_MC_LW,
               S_MC_LF)) %>%
  mutate(fiveday = ymd(Dates - ddays(5)),
         threeday = ymd(Dates - ddays(3)),
         sevenday = ymd(Dates - ddays(7)) 
         ,thirtyday = ymd(Dates - ddays(30))) 

MonthOrdered[MonthOrdered==Inf] <- 0





#Precipitation Data Join
zRain <- full_join(MonthOrdered, 
                   zNOAA, 
                   by = c("LK_CODE" = "LK_CODE"))

#Filter rain data to match date ranges.
zNOAA3 <- zRain %>% rowwise() %>%
  filter(Dates.y >= as.Date(threeday) & 
           Dates.y <= as.Date(Dates.x))
zNOAA5 <- zRain %>% rowwise() %>% 
  filter(Dates.y >= as.Date(fiveday) &
           Dates.y <= as.Date(Dates.x))
zNOAA7 <- zRain %>% rowwise() %>% 
  filter(Dates.y >= as.Date(sevenday) &
           Dates.y <= as.Date(Dates.x))
zNOAA30 <- zRain %>% rowwise() %>% 
  filter(Dates.y >= as.Date(thirtyday) &
           Dates.y <= as.Date(Dates.x))

#Average by Day of sampling varied by lagged temprature dates. 
zzNOAA3 <- zNOAA3 %>% 
  group_by(LK_CODE, Mo) %>% 
  summarise(precip3 = mean(prcp, na.rm = TRUE),
            temp3 = mean(tobs, na.rm =TRUE))

zzNOAA5 <- zNOAA5 %>% 
  group_by(LK_CODE, Mo) %>% 
  summarise(precip5 = mean(prcp, na.rm = TRUE), 
            temp5 = mean(tobs, na.rm =TRUE))

zzNOAA7 <- zNOAA7 %>%
  group_by(LK_CODE, Mo) %>% 
  summarise(precip7 = mean(prcp, na.rm = TRUE), 
            temp7 = mean(tobs, na.rm =TRUE))

zzNOAA30 <- zNOAA30 %>% 
  group_by(LK_CODE, Mo) %>% 
  summarise(precip30 = mean(prcp, na.rm = TRUE),
            temp30 = mean(tobs, na.rm =TRUE))

# Surveyor information

zzsurveyor <- read.csv(file="surveyor.csv") 

#Ultimate Join of all tables
zUltimateSheet <- MonthOrdered %>%
  full_join(info,
            by = c("LK_CODE" = "LK_CODE")) %>%
  full_join(ecoregion, 
            by = c("LK_CODE" = "LK_CODE")) %>%
  full_join(zzNOAA3,
            by = c("LK_CODE" = "LK_CODE", "Mo" = "Mo")) %>%
  full_join(zzNOAA5,
            by = c("LK_CODE" = "LK_CODE", "Mo" = "Mo")) %>%
  full_join(zzNOAA7,
            by = c("LK_CODE" = "LK_CODE", "Mo" = "Mo")) %>%
  full_join(zzNOAA30, 
            by = c("LK_CODE" = "LK_CODE", "Mo" = "Mo")) %>%
  full_join(zhobo, 
            by = c("LK_CODE" = "LK_CODE", "Month" = "Month")) %>%
  full_join(zzsurveyor, 
            by = c("LK_CODE" = "LK_CODE")) %>%
  rename(hobotemp = TEMP, 
         hobolight = LIGH, 
         ecoregion = US_L3NAME) %>%
  mutate(LK_CODE = factor(LK_CODE),
         year = as.factor(year),
         network = factor(Network, levels = c("1", "2", "3", "4"),
                          exclude = TRUE),
         open = as.factor(Open_clos)) %>%
  rowwise() %>%
  # Omit all great lake sites. Lake Superior
  filter_all(all_vars(!grepl('SUP',.))) %>% 
  # Omit Lake Erie
  filter_all(all_vars(!grepl('ERI',.))) %>% 
  # Omit Lake St. Clair
  filter_all(all_vars(!grepl('STC',.))) %>% 
  # Omit Cullen Park 
  filter_all(all_vars(!grepl('CUL',.))) %>% 
  select(-county, -REACHCODE, -sum) 



# Convert all characters as factor
UltimateFactor <- as.data.frame(zUltimateSheet) %>% 
  mutate_if(is.character, as.factor) %>%
# Surveyors such as Ryan, Jason, Ben and Szlag team
# are converted to dummy variables as factor levels
  mutate(surveyor = factor(surveyor, levels = c(1,2,3,4))) 


# Modeling Log Transformed dataset. 
# Each variable is added by reportable detection limit
#Log transformed all data
LOGTransformed <- UltimateFactor %>% 
  mutate_at(vars(OP),
            funs(log10(.+0.003))) %>%
  mutate_at(vars(NO3), 
            funs(log10(.+0.04))) %>%
  mutate_at(vars(NH3),
            funs(log10(. + 0.006))) %>%
  mutate_at(vars(TP), 
            funs(log10(. + 0.002))) %>%
  mutate_at(vars(TKN),
            funs(log10(. + 0.07))) %>%
  mutate_at(vars(TN), 
            funs(log10(. + 0.116))) %>%
  mutate_at(vars(DO),
            funs(log10(. + 0.01))) %>%
  mutate_at(vars(turb, conduc),
            funs(log10(. + 0.01))) %>%
  mutate_at(vars(ELISA), 
            funs(log10(. + 0.15))) %>%
  mutate_at(vars(LkArea), 
            funs(log10(. + 1))) %>%
  mutate_at(vars(WtShArea), 
            funs(log10(. + 1))) %>%
  mutate_at(vars(phyco,chloro), 
            funs(log10(. + 0.01))) %>%
  mutate_at(vars(X16SRNA, CYANA, MCYE, CyrA, SxtA), 
            funs(log10(. + 20))) %>% 
  # qpcr results log transformed
  mutate_at(vars(Nodul:SUM), 
            funs(log10(. + 0.03))) %>% 
  # Mass Spec
  mutate_at(vars(Anatoxin:S_ELISA), 
            funs(log10(. + 1))) %>% 
  # SPATTS
  mutate_at(vars(precip3, precip5, precip7, 
                 precip30, hobolight), 
            funs(log10(.+1))) %>%
  # Drop variables not to be included in modeling
  select(-CYANA,-SxtA,-Sensored,-PPIA,-Sensored_1,
         -S_EL_CENS,-Dates, -month,
         -fiveday:-Network,-Open_clos,
         -Zebra,-network,
         -open, -Anatoxin, -Cylindro,-CyrA)


#Averaged by each lake, then log transformed
MCSUMresponseAverage <- UltimateFactor  %>%
  select(-CYANA,-SxtA,-Sensored,-PPIA,-Sensored_1,
         -S_EL_CENS,-Dates, -month,
         -fiveday:-Network,-Open_clos,
         -Zebra,-network,
         -open, -Anatoxin, -Cylindro,-CyrA) %>%
  group_by(LK_CODE) %>%
  summarise_all(funs(mean(., na.rm=TRUE))) %>%
  mutate_at(vars(OP), funs(log10(.+0.003))) %>%
  mutate_at(vars(NO3), funs(log10(.+0.04))) %>%
  mutate_at(vars(NH3), funs(log10(. + 0.006))) %>%
  mutate_at(vars(TP), funs(log10(. + 0.002))) %>%
  mutate_at(vars(TKN), funs(log10(. + 0.07))) %>%
  mutate_at(vars(TN), funs(log10(. + 0.116))) %>%
  mutate_at(vars(DO), funs(log10(. + 0.01))) %>%
  mutate_at(vars(turb, conduc), funs(log10(. + 0.01))) %>%
  mutate_at(vars(ELISA), funs(log10(. + 0.15))) %>%
  mutate_at(vars(LkArea), funs(log10(. + 1))) %>%
  mutate_at(vars(WtShArea), funs(log10(. + 1))) %>%
  mutate_at(vars(phyco,chloro), funs(log10(. + 0.01))) %>%
  mutate_at(vars(X16SRNA, MCYE), funs(log10(. + 20))) %>%
  mutate_at(vars(Nodul:SUM), funs(log10(. + 0.03))) %>% 
  #Mass Spec
  mutate_at(vars(S_D_Asp3_RR:S_SUM), funs(log10(. + 1))) %>% 
  # SPATTS
  mutate_at(vars(precip3, precip5, precip7,
                 precip30, hobolight), funs(log10(.+1))) %>%
 select(-LK_CODE:-doy, -Nodul:-MC_LF, 
         -ecoregion, -surveyor, -ELISA,
         -S_D_Asp3_RR:-S_ELISA)

########################################################################
########################################################################
########################################################################


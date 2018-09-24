# Import hobo logger data. 
# Each hobo loggers were compiled and averaged by each month

zHoboList <- list.files(path = "2017_HOBO/",
                        pattern = "*.csv",
                        full.names = T)
zhobomatrix <- data.frame(X1 = zHoboList) %>%
  mutate(filename_wo_ext = gsub(".csv","", X1))



yBEA <- read.csv(file="2017_HOBO/BEA.csv") 
zBEA <- yBEA %>%
  mutate(Date=vars(select(2)))

zBEL <- read.csv(file="2017_HOBO/BEL.csv")
zBEA <- read.csv(file="2017_HOBO/BEA.csv")
zBRI <- read.csv(file="2017_HOBO/BRI.csv") %>% mutate(Date=vars(select(2)),
                                                      Temp=vars(3),
                                                      Intensity=vars(4))

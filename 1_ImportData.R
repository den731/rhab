# This script is to aggregate the master sheet data, the lake info, weather data and the algal data


# INSTALL THESE LIBRARIES

install.packages("dyplyr")
install.packages("lubridate")


# Load the libraries

library(dplyr)
library(lubridate)


# Import the csv files

xmastersheet <- read.csv("MasterSheet.csv")

# Covert -999 values in mastersheet to N/A's

xmastersheet[xmastersheet==-999] <- NA

##### RE-Analysis post-1st review
# The Free Clinics monitoring data
## Visualizing data
require("ggplot2")      # Powerful and aesthetic plotting system for R
require("gridExtra")    # Arrange multiple ggplots in same figure (multi-panels)
require("scales")       #
require("RColorBrewer") # creates nice color schemes
require("corrplot")     # A graphical display of a correlation matrix between all combinations of variables
## Statistical analysis
require("stats")        # Lots of stats stuff
## Data management
require("plyr")         # Allows you t split data structure into groups (pollutant type, location, etc.) and apply function on each group
require("dplyr")
require("zoo")          # Helps streamline data if you have irregular time series
require("reshape2")     # Convert data with "wide" columns to "long" columns
require("lubridate")    # Date and time data made easy! See reference PDF in Google Drive
require("data.table")
require("TTR")
#require("xlsx")        # creates errors # Reads and writes to xlsx file
require("purrr")
require("tidyr")
require("fBasics")
require("pls")
## Mapping tools
require("stringi")
require("ggmap")        # Plotting of maps same as you would with ggplot2
require("maptools")     # Read, write, and handle Shapefiles in R
require("mapdata")      # Supplement to maps package

## Read data file
# Data file has previous manipulations
TFC_RWH.1 <- read.csv("./Working/TFC_RWH.DEL.csv")
## View to confirm proper read
#View(TFC_RWH.1)

## rename columns
colnames(TFC_RWH.1) <- c("date.time", 
                         "rainfall", 
                         "intensity",
                         "Air.temp", 
                         "Bottom.temp", 
                         "Bottom.depth", 
                         "Middle.temp", 
                         "Middle.depth", 
                         "Top.temp", 
                         "Top.depth", 
                         "event")
# Confirm
# View(TFC_RWH.1)

## Set date time fomat
TFC_RWH.1$date.time <- mdy_hm(TFC_RWH.1$date.time, tz = "est")
# Confirm class
#class(TFC_RWH.1[,1])

## Need to convert units to metric
TFC_RWH.m <- mutate(TFC_RWH.1, rainfall = (rainfall * 25.4), 
                      intensity = (intensity * 25.4), 
                      Air.temp = (Air.temp - 32)/1.8, 
                      Bottom.temp = (Bottom.temp - 32)/1.8, 
                      Bottom.depth = (Bottom.depth * 30.48), 
                      Middle.temp = (Middle.temp - 32)/1.8, 
                      Middle.depth = (Middle.depth * 30.48), 
                      Top.temp = (Top.temp - 32)/1.8, 
                      Top.depth = (Top.depth * 30.48))
#View(TFC_RWH.m)

## Split into list of events
RWHevents <- split(TFC_RWH.m, TFC_RWH.m$event) 
# Returns a list of events 
# View(RWHevents)

## Calculates mean of Duration & Rainfall Accumulaiton 
# Returns a data frame of values same length as list
RWHsum <- RWHevents %>%
  map_df(function(df) {summarise(df, Date = min(date.time), 
                                 Duration = ((max(date.time)-min(date.time))/3600),
                                 Accumulation = sum(rainfall, na.rm = TRUE),
                                 max.intensity = max(intensity, na.rm = TRUE),
                                 medbottemp = median(Bottom.temp, na.rm = TRUE), 
                                 maxbottemp = max(Bottom.temp, na.rm = TRUE),
                                 avebotdepth = mean(Bottom.depth, na.rm = TRUE),
                                 medmidtemp = median(Middle.temp, na.rm = TRUE), 
                                 maxmidtemp = max(Middle.temp, na.rm = TRUE),
                                 avemiddepth = mean(Middle.depth, na.rm = TRUE),
                                 medtoptemp = median(Top.temp, na.rm = TRUE), 
                                 maxtoptemp = max(Top.temp, na.rm = TRUE),
                                 avetopdepth = mean(Top.depth, na.rm = TRUE))})
# View(RWHsum)

## Mutate to provide additional hydrology analsis
RWH_event_analysis <- (RWHsum) %>%
  subset(Accumulation >= 5.0) 
#View(RWH_event_analysis)

# ## Summarise rainfall info
# Rainfall_event.summary <- (Rainsum[-1, ]) %>%
#   select(Duration,
#          Accumulation,
#          max.intensity5) %>%
#   summarise_all(funs(median, min, max), na.rm = TRUE)
# #View(Rainfall_event.summary)














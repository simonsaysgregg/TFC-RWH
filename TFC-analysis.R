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
TFC_RWH.1.2 <- mutate(TFC_RWH.1.1, rainfall = (rainfall * 25.4), 
                      intensity = (intensity * 25.4), 
                      Air.temp = (Air.temp - 32)/1.8, 
                      Bottom.temp = (Bottom.temp - 32)/1.8, 
                      Bottom.depth = (Bottom.depth * 30.48), 
                      Middle.temp = (Middle.temp - 32)/1.8, 
                      Middle.depth = (Middle.depth * 30.48), 
                      Top.temp = (Top.temp - 32)/1.8, 
                      Top.depth = (Top.depth * 30.48), 
                      raincum = cumsum(rainfall))
#View(TFC_RWH.1.2)











## Remove extra data; only rain and time stamp for delineation
RWH.rain <- (TFC_RWH.1) %>%
  select(date.time,
         rainfall)
# View(RSC.rain)

## Rainfall event delineation
# Exstract from Drizzle0.9.5
depth <- RSC.rain$rainfall.mm
depth[depth == 0] <- NA
nalocf <- function(x) na.locf(x, maxgap = 359, na.rm = FALSE)
rain.index <- cumsum(diff(!is.na(c(NA, nalocf(depth)))) > 0) + nalocf(0*depth)

## Addend rain index to file to be delineated
RSC.hydro.m[,"rain.index"] <- rain.index

## Runoff event delineation
# Extend rain.idex for drawdown period of 12 hours in present case
# to change drawdown change DD; depends of data interval (DD.in.hours*60)/data.interval.in.minutes
runoff.del <- function(x, DD=362) {
  l <- cumsum(! is.na(x))
  c(NA, x[! is.na(x)])[replace(l, ave(l, l, FUN=seq_along) > DD, 0) + 1]
}
# vetor to process
k <- RSC.hydro.m$rain.index
# function operation
RSC.hydro.m$storm.index <- runoff.del(k)

## Replace NAs with zero
# rain index
RSC.hydro.m$rain.index <- (RSC.hydro.m$rain.index) %>%
  replace_na(0)
# View(RSC.hydro.m)
# storm index
RSC.hydro.m$storm.index <- (RSC.hydro.m$storm.index) %>%
  replace_na(0)
# View(RSC.hydro.m)

## Split into list of events
RainEvents <- split(RSC.hydro.m, RSC.hydro.m$storm.index) 
# Returns a list of events 
# View(RainEvents)

## Calculates mean of Duration & Rainfall Accumulaiton 
# Returns a data frame of values same length as list
Rainsum <- RainEvents %>%
  map_df(function(df) {summarise(df, Duration = ((max(timestamp)-min(timestamp))/3600),
                                 Accumulation = sum(rainfall.mm, na.rm = TRUE),
                                 max.intensity5 = max(int.5min, na.rm = TRUE),
                                 runoff.est.in1 = runoff.in1(Accumulation, CN = 86),
                                 in1.vol = sum(in1.m_flow, na.rm = TRUE) * (Duration * 3600),
                                 dryout.vol = sum(dryout.m_flow, na.rm = TRUE) * (Duration * 3600),
                                 runoff.est.in2 = runoff.in2(Accumulation, CN = 84),
                                 in2.vol = sum(in2.m_flow, na.rm = TRUE) * (Duration * 3600),
                                 in2.hobo.vol = sum(in2.hobo.m_flow, na.rm = TRUE) * (Duration * 3600),
                                 runoff.est.runon = runoff.runon(Accumulation, CN = 87),
                                 out.vol = sum(out.flow, na.rm = TRUE) * (Duration * 3600),
                                 out.vol.roll = sum(out.flow.roll.ASABE, na.rm = TRUE) * (Duration * 3600))})
# View(Rainsum)

## Mutate to provide additional hydrology analsis
Rainsum_event_analysis <- (Rainsum) %>%
  subset(Accumulation >= 5.0) %>%
  mutate(in.sum = in1.vol + in2.hobo.vol + runoff.est.runon,
         flow.vol.perc_diff.roll = ((as.numeric(in.sum) - as.numeric(out.vol.roll)) / as.numeric(in.sum)) * 100,
         flow.vol.perc_diff = ((as.numeric(in.sum) - as.numeric(out.vol)) / as.numeric(in.sum)) * 100)
#View(Rainsum_event_analysis)
## Summarise rainfall info
Rainfall_event.summary <- (Rainsum[-1, ]) %>%
  select(Duration,
         Accumulation,
         max.intensity5) %>%
  summarise_all(funs(median, min, max), na.rm = TRUE) 
#View(Rainfall_event.summary)














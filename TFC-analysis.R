##### RE-Analysis post-1st review
# The Free Clinics Rain Water Harvester monitoring data
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

## User defined functions
# Runoff estimation function
runoff.in <- function(acc, CN, WA = 0.05968) {  # WA in acres
  # convert accumulation to inches
  acc.in <- acc * 3.94
  # surface storage
  S <- (1000/CN) - 10
  # runoff units: inches
  Q <- ((acc.in - (0.2 * S))^2)/(acc.in + (0.8 * S))
  # conversion to volume: cubic feet
  Q.vol <- Q * WA * (43560 / 12)
  # conversion to cubic meters
  Q.vol <- Q.vol * 0.0283
  return(Q.vol)
}

# Orifice outflow calculation
flow.outRWH <- function(bottom.depth, Cd = 0.51, center.diff = 206.4, inv.diff = 198.8, area.orf.m = 0.0090729, grav = 9.8, D = 0.49869) { 
  ## modify following equaiton for outlet
  # inv.diff == difference btw measurement point and invert 
  # Hobo 198.8-cm below orifice outlet
  # Hobo 206.4-cm below orifice outlet
  # Else equation is in standard units and converted to metric

  
  ifelse(bottom.depth > center.diff, ((Cd * area.orf.m) * sqrt(2 * grav * (bottom.depth - center.diff))), ((((4.464 * Cd * D) * ((bottom.depth - inv.diff) * (0.394/12))^(1.5)) * 0.3048^(3))))     
}

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
                      Top.depth = (Top.depth * 30.48),
                      Outflow = flow.outRWH(Bottom.depth))
#View(TFC_RWH.m)

#Replace rainfall NAs with zero
TFC_RWH.m$Outflow[is.nan(TFC_RWH.m$Outflow)] <- 0

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
                                 avetopdepth = mean(Top.depth, na.rm = TRUE),
                                 varbot = var(Bottom.depth, na.rm = TRUE),
                                 varmid = var(Middle.depth, na.rm = TRUE),
                                 vartop = var(Top.depth, na.rm = TRUE))})
# View(RWHsum)

## Breaking events into pre and post 
## subset to provide additional hydrology analsis
RWH_event_pre1012 <- (RWHsum[-c(1),]) %>%
  subset(Date <= "2017/10/12" & Accumulation >= 2.38) 
#View(RWH_event_pre1012)
## subset to provide additional hydrology analsis
RWH_event_post1012 <- (RWHsum[-c(1),]) %>%
  subset(Date >= "2017/10/12" & Accumulation >= 2.38) 
#View(RWH_event_post1012)

## Wilcoxon test
# median bottom
wilcox.test(RWH_event_pre1012$medbottemp, alternative = "t", mu = 21, paired = FALSE, conf.int = TRUE, conf.level = 0.95)
# max bottom
wilcox.test(RWH_event_pre1012$maxbottemp, alternative = "t", mu = 21, paired = FALSE, conf.int = TRUE, conf.level = 0.95)
# median middle
wilcox.test(RWH_event_pre1012$medmidtemp, alternative = "g", mu = 21, paired = FALSE, conf.int = TRUE, conf.level = 0.95)
# max middle
wilcox.test(RWH_event_pre1012$maxmidtemp, alternative = "g", mu = 21, paired = FALSE, conf.int = TRUE, conf.level = 0.95)
# median top
wilcox.test(RWH_event_pre1012$medtoptemp, alternative = "g", mu = 21, paired = FALSE, conf.int = TRUE, conf.level = 0.95)
# max top
wilcox.test(RWH_event_pre1012$maxtoptemp, alternative = "g", mu = 21, paired = FALSE, conf.int = TRUE, conf.level = 0.95)

## Wilcoxon test
# median bottom
wilcox.test(RWH_event_post1012$medbottemp, alternative = "l", mu = 21, paired = FALSE, conf.int = TRUE, conf.level = 0.95)
# max bottom
wilcox.test(RWH_event_post1012$maxbottemp, alternative = "l", mu = 21, paired = FALSE, conf.int = TRUE, conf.level = 0.95)
# median middle
wilcox.test(RWH_event_post1012$medmidtemp, alternative = "l", mu = 21, paired = FALSE, conf.int = TRUE, conf.level = 0.95)
# max middle
wilcox.test(RWH_event_post1012$maxmidtemp, alternative = "l", mu = 21, paired = FALSE, conf.int = TRUE, conf.level = 0.95)
# median top
wilcox.test(RWH_event_post1012$medtoptemp, alternative = "l", mu = 21, paired = FALSE, conf.int = TRUE, conf.level = 0.95)
# max top
wilcox.test(RWH_event_post1012$maxtoptemp, alternative = "l", mu = 21, paired = FALSE, conf.int = TRUE, conf.level = 0.95)

## box plots of pre-1012
# median data
RWHpre1012med_box <- (RWH_event_pre1012) %>%
  select(medbottemp,
         medmidtemp,
         medtoptemp) %>%
  melt()
#View(RWHpre1012med_box)
# maximum data
RWHpre1012max_box <- (RWH_event_pre1012) %>%
  select(maxbottemp,
         maxmidtemp,
         maxtoptemp) %>%
  melt()
#View(RWHpre1012max_box)

# plot median temps
ggplot(data = RWHpre1012med_box)+
      geom_boxplot(aes(x = variable, y = value))+
      geom_hline(aes(yintercept = 21, color = "Trout Threshold"))+
      scale_x_discrete(labels = c("Bottom", "Middle", "Top"))+
      scale_y_continuous(limits = c(10,30), expand = c(0,0)) +
      theme(legend.position = "bottom", 
            legend.title = element_blank())+
      labs(x = "Temperature Location", y = "Temperature (°C)")


# plot max temps
ggplot(data = RWHpre1012max_box)+
      geom_boxplot(aes(x = variable, y = value))+
      geom_hline(aes(yintercept = 21, color = "Trout Threshold"))+
      scale_x_discrete(labels = c("Bottom", "Middle", "Top"))+
      scale_y_continuous(limits = c(10,30), expand = c(0,0)) +
      theme(legend.position = "bottom", 
            legend.title = element_blank())+
      labs(x = "Temperature Location", y = "Temperature (°C)")


## box plots of post-1012
# median data
RWHpost1012med_box <- (RWH_event_post1012) %>%
  select(medbottemp,
         medmidtemp,
         medtoptemp) %>%
  melt()
#View(RWHpost1012med_box)
# maximum data
RWHpost1012max_box <- (RWH_event_post1012) %>%
  select(maxbottemp,
         maxmidtemp,
         maxtoptemp) %>%
  melt()
#View(RWHpost1012max_box)

# plot median temps
ggplot(data = RWHpost1012med_box)+
  geom_boxplot(aes(x = variable, y = value))+
  geom_hline(aes(yintercept = 21, color = "Trout Threshold"))+
  scale_x_discrete(labels = c("Bottom", "Middle", "Top"))+
  scale_y_continuous(limits = c(10,30), expand = c(0,0)) +
  theme(legend.position = "bottom", 
        legend.title = element_blank())+
  labs(x = "Temperature Location", y = "Temperature (°C)")


# plot max temps
ggplot(data = RWHpost1012max_box)+
  geom_hline(aes(yintercept = 21, color = "Trout Threshold"))+
  geom_boxplot(aes(x = variable, y = value))+
  scale_x_discrete(labels = c("Bottom", "Middle", "Top"))+
  scale_y_continuous(limits = c(10,30), expand = c(0,0)) +
  theme(legend.position = "bottom", 
        legend.title = element_blank())+
  labs(x = "Temperature Location", y = "Temperature (°C)")



## Difference between measurement depths Pre
# Median
# bottom and middle
wilcox.test(RWH_event_pre1012$medbottemp, RWH_event_pre1012$medmidtemp, alternative = "t", paired = TRUE, conf.int = TRUE, conf.level = 0.95)
# middle and top
wilcox.test(RWH_event_pre1012$medmidtemp, RWH_event_pre1012$medtoptemp, alternative = "t", paired = TRUE, conf.int = TRUE, conf.level = 0.95)
# bottom and top
wilcox.test(RWH_event_pre1012$medbottemp, RWH_event_pre1012$medtoptemp, alternative = "t", paired = TRUE, conf.int = TRUE, conf.level = 0.95)
# maximum
# bottom and middle
wilcox.test(RWH_event_pre1012$maxbottemp, RWH_event_pre1012$maxmidtemp, alternative = "t", paired = TRUE, conf.int = TRUE, conf.level = 0.95)
# middle and top
wilcox.test(RWH_event_pre1012$maxmidtemp, RWH_event_pre1012$maxtoptemp, alternative = "t", paired = TRUE, conf.int = TRUE, conf.level = 0.95)
# bottom and top
wilcox.test(RWH_event_pre1012$maxbottemp, RWH_event_pre1012$maxtoptemp, alternative = "t", paired = TRUE, conf.int = TRUE, conf.level = 0.95)
# Variance
# bottom and middle
wilcox.test(RWH_event_pre1012$varbot, RWH_event_pre1012$varmid, alternative = "t", paired = TRUE, conf.int = TRUE, conf.level = 0.95)
# middle and top
wilcox.test(RWH_event_pre1012$varmid, RWH_event_pre1012$vartop, alternative = "t", paired = TRUE, conf.int = TRUE, conf.level = 0.95)
# bottom and top
wilcox.test(RWH_event_pre1012$varbot, RWH_event_pre1012$vartop, alternative = "t", paired = TRUE, conf.int = TRUE, conf.level = 0.95)

## Difference between measurement depths post
# Median
# bottom and middle
wilcox.test(RWH_event_post1012$medbottemp, RWH_event_post1012$medmidtemp, alternative = "t", paired = TRUE, conf.int = TRUE, conf.level = 0.95)
# middle and top
wilcox.test(RWH_event_post1012$medmidtemp, RWH_event_post1012$medtoptemp, alternative = "t", paired = TRUE, conf.int = TRUE, conf.level = 0.95)
# bottom and top
wilcox.test(RWH_event_post1012$medbottemp, RWH_event_post1012$medtoptemp, alternative = "t", paired = TRUE, conf.int = TRUE, conf.level = 0.95)
# maximum
# bottom and middle
wilcox.test(RWH_event_post1012$maxbottemp, RWH_event_post1012$maxmidtemp, alternative = "t", paired = TRUE, conf.int = TRUE, conf.level = 0.95)
# middle and top
wilcox.test(RWH_event_post1012$maxmidtemp, RWH_event_post1012$maxtoptemp, alternative = "t", paired = TRUE, conf.int = TRUE, conf.level = 0.95)
# bottom and top
wilcox.test(RWH_event_post1012$maxbottemp, RWH_event_post1012$maxtoptemp, alternative = "t", paired = TRUE, conf.int = TRUE, conf.level = 0.95)
# Variance
# bottom and middle
wilcox.test(RWH_event_post1012$varbot, RWH_event_post1012$varmid, alternative = "t", paired = TRUE, conf.int = TRUE, conf.level = 0.95)
# middle and top
wilcox.test(RWH_event_post1012$varmid, RWH_event_post1012$vartop, alternative = "t", paired = TRUE, conf.int = TRUE, conf.level = 0.95)
# bottom and top
wilcox.test(RWH_event_post1012$varbot, RWH_event_post1012$vartop, alternative = "t", paired = TRUE, conf.int = TRUE, conf.level = 0.95)

# # Variance by event
# RWHvar <- (RWH_event_pre1012) %>%
#   mutate(varbotT = var(medbottemp, na.rm = TRUE),
#           varmidT = var(medmidtemp, na.rm = TRUE),
#           vartopT = var(medtoptemp, na.rm = TRUE),
#           varbotTT = var(maxbottemp, na.rm = TRUE),
#           varmidTT = var(maxmidtemp, na.rm = TRUE),
#           vartopTT = var(maxtoptemp, na.rm = TRUE))
# #View(RWHvar)
# 
# # Variance Median
# # bottom and middle
# wilcox.test(RWHvar$varbotT, RWHvar$varmidT, alternative = "t", paired = TRUE)
# # middle and top
# wilcox.test(RWHvar$varmidT, RWHvar$vartopT, alternative = "t", paired = TRUE, conf.int = TRUE, conf.level = 0.95)
# # bottom and top 
# wilcox.test(RWHvar$varbotT, RWHvar$vartopT, alternative = "t", paired = TRUE, conf.int = TRUE, conf.level = 0.95)
# # Variance Max
# # bottom and middle
# wilcox.test(RWHvar$varbotTT, RWHvar$varmidTT, alternative = "t", paired = TRUE, conf.int = TRUE, conf.level = 0.95)
# # middle and top
# wilcox.test(RWHvar$varmidTT, RWHvar$vartopTT, alternative = "t", paired = TRUE, conf.int = TRUE, conf.level = 0.95)
# # bottom and top
# wilcox.test(RWHvar$varbotTT, RWHvar$vartopTT, alternative = "t", paired = TRUE, conf.int = TRUE, conf.level = 0.95)

## Plot depth and rainfall
tot.mon.plot <- (TFC_RWH.m) %>%
  select(date.time,
         Bottom.depth,
         Middle.depth,
         Top.depth,
         rainfall) 
  colnames(tot.mon.plot) <- c("date.time",
                               "Bottom",
                               "Middle",
                               "Top",
                               "Rainfall")
# Replace rainfall NAs with zero
tot.mon.plot$Rainfall <- (tot.mon.plot$Rainfall) %>%
  replace_na(0)
# Prep plotting dataset
tot.mon.plot <- (tot.mon.plot) %>%
  mutate(Rainfall = cumsum(Rainfall)) %>%
  select(date.time,
         Bottom,
         Middle,
         Top,
         Rainfall) %>%
  subset(date.time <= as.POSIXct("2017-09-08 16:00:00") | date.time >= as.POSIXct("2017-09-08 20:00:00")) %>%
  subset(date.time <= as.POSIXct("2017-10-04 12:00:00") | date.time >= as.POSIXct("2017-10-04 18:00:00")) %>%
  subset(date.time <= as.POSIXct("2017-10-25 09:00:00") | date.time >= as.POSIXct("2017-10-25 15:00:00")) %>%
  subset(date.time <= as.POSIXct("2017-11-22 12:00:00")) %>%
  melt(id = "date.time")
# View(tot.mon.plot)
# plot depths
ggplot(data = tot.mon.plot)+
  geom_line(aes(x = date.time, y = value, color = variable))+
  labs(x = "Date", y = "Depth (cm)")+
  theme(legend.position = "bottom", 
        legend.title = element_blank())+
  scale_x_datetime(date_labels = "%m/%d", date_breaks = "10 days")+
  scale_y_continuous(sec.axis = sec_axis(~./1, name = "Rainfall (mm)"))
 
## Plot temperature and rainfall
tot.mon.plot2 <- (TFC_RWH.m) %>%
  select(date.time,
         Air.temp,
         Bottom.temp,
         Middle.temp,
         Top.temp) 
colnames(tot.mon.plot2) <- c("date.time",
                            "Air",
                            "Bottom",
                            "Middle",
                            "Top")

# Prep plotting dataset
tot.mon.plot2 <- (tot.mon.plot2) %>%
  select(date.time,
         Air,
         Bottom,
         Middle,
         Top) %>%
  subset(date.time <= as.POSIXct("2017-09-08 16:00:00") | date.time >= as.POSIXct("2017-09-08 20:00:00")) %>%
  subset(date.time <= as.POSIXct("2017-10-04 12:00:00") | date.time >= as.POSIXct("2017-10-04 18:00:00")) %>%
  subset(date.time <= as.POSIXct("2017-10-25 09:00:00") | date.time >= as.POSIXct("2017-10-25 15:00:00")) %>%
  subset(date.time <= as.POSIXct("2017-11-22 12:00:00")) %>%
  melt(id = "date.time")
# View(tot.mon.plot2)
# Dataset just for rainfall
cum.rain <- (TFC_RWH.m) %>%
  select(date.time,
         rainfall) 
colnames(cum.rain) <- c("date.time",
                        "rainfall")
# Replace rainfall NAs with zero
cum.rain$rainfall <- (cum.rain$rainfall) %>%
  replace_na(0) 
cum.rain <- (cum.rain) %>%
  mutate(Rainfall = cumsum(rainfall))
# plot depths
ggplot()+
  geom_line(data = tot.mon.plot2, aes(x = date.time, y = value, color = variable))+
  geom_line(data = cum.rain, aes(x = date.time, y = Rainfall/5, color = "Rainfall"))+
  labs(x = "Date", y = "Temperature (°C)")+
  theme(legend.position = "bottom", 
        legend.title = element_blank())+
  scale_x_datetime(date_labels = "%m/%d", date_breaks = "10 days")+
  scale_y_continuous(sec.axis = sec_axis(~.*5, name = "Rainfall (mm)"))


# ## Plot September 21 event rainfall + depth
# # Decided to omit this plot
# plot912 <- (TFC_RWH.m) %>%
#   select(date.time,
#          Bottom.depth,
#          Middle.depth,
#          Top.depth) 
# colnames(plot912) <- c("date.time",
#                              "Bottom",
#                              "Middle",
#                              "Top")
# # Prep plotting dataset
# plot912 <- (plot912) %>%
#   select(date.time,
#          Bottom,
#          Middle,
#          Top) %>%
#   subset(date.time >= as.POSIXct("2017-09-19 00:00:00") & date.time <= as.POSIXct("2017-09-23 00:00:00")) %>%
#   melt(id = "date.time")
# # View(plot912)
# # Dataset just for rainfall
# cum.rain <- (TFC_RWH.m) %>%
#   select(date.time,
#          rainfall) 
# colnames(cum.rain) <- c("date.time",
#                         "rainfall")
# # Replace rainfall NAs with zero
# cum.rain$rainfall <- (cum.rain$rainfall) %>%
#   replace_na(0) 
# cum.rain <- (cum.rain) %>%
#   mutate(Rainfall = cumsum(rainfall))%>%
#   subset(date.time >= as.POSIXct("2017-09-19 00:00:00") & date.time <= as.POSIXct("2017-09-23 00:00:00")) 
# # plot depths
# ggplot()+
#   geom_line(data = plot912, aes(x = date.time, y = value, color = variable))+
#   geom_line(data = cum.rain, aes(x = date.time, y = Rainfall*2.5, color = "Rainfall"))+
#   labs(x = "Date", y = "Depth (cm)")+
#   theme(legend.position = "bottom", 
#         legend.title = element_blank())+
#   scale_x_datetime(date_labels = "%m/%d", date_breaks = "1 days")+
#   scale_y_continuous(sec.axis = sec_axis(~./2.5, name = "Rainfall (mm)"))

## Plot temperature and rainfall
max.temp.plot <- (TFC_RWH.m) %>%
  select(date.time,
         Air.temp,
         Bottom.temp,
         Middle.temp,
         Top.temp) 
colnames(max.temp.plot) <- c("date.time",
                             "Air",
                             "Bottom",
                             "Middle",
                             "Top")
# Prep plotting dataset
max.temp.plot <- (max.temp.plot) %>%
  select(date.time,
         Air,
         Bottom,
         Middle,
         Top) %>%
  subset(date.time >= as.POSIXct("2017-09-25 16:00:00") & date.time <= as.POSIXct("2017-10-07 20:00:00")) %>%
  melt(id = "date.time")
# View(max.temp.plot)
# plot depths
ggplot()+
  geom_line(data = max.temp.plot, aes(x = date.time, y = value, color = variable))+
  labs(x = "Date", y = "Temperature (°C)")+
  theme(legend.position = "bottom", 
        legend.title = element_blank())+
  scale_x_datetime(date_labels = "%m/%d", date_breaks = "2 days")

# ## subset to provide additional hydrology analsis
# RWH_event_analysis <- (RWHsum) %>%
#   subset(Accumulation >= 5.0) 
# #View(RWH_event_analysis)
# 
# # ## Summarise rainfall info
# # Rainfall_event.summary <- (Rainsum[-1, ]) %>%
# #   select(Duration,
# #          Accumulation,
# #          max.intensity5) %>%
# #   summarise_all(funs(median, min, max), na.rm = TRUE)
# # #View(Rainfall_event.summary)

## Antecedant dry period analysis
## Rainfall event delineation
# Exstract from Drizzle0.9.5 + modified
event <- TFC_RWH.m$event
event[event != 0] <- NA
ADP.index <- cumsum(diff(!is.na(c(NA, (event)))) > 0) + (0*event)
# Create new dataset
TFC_RWH.ADP <- TFC_RWH.m
# Add ADP index as new variable
TFC_RWH.ADP[, "ADP.index"] <- ADP.index
#Replace rainfall NAs with zero
TFC_RWH.ADP$ADP.index[is.na(TFC_RWH.ADP$ADP.index)] <- 0 
# Confirm
# View(TFC_RWH.ADP)

## Summary of ADP
ADP.sum <- (TFC_RWH.ADP) %>%
  group_by(ADP.index) %>%
  summarise(duation = (max(date.time) - min(date.time)),
            maxbotD = max(Bottom.depth),
            avgbotD = mean(Bottom.depth),
            maxmidD = max(Middle.depth),
            avgmidD = mean(Middle.depth),
            maxtopD = max(Top.depth),
            avgtopD = mean(Top.depth)) 
#View(ADP.sum)
# Range in days
# 1.16-82.5
# Median in days
# 6.93
## Approximated sensor depths
#Top medians
#99.9-cm
#Middle median
#147.8
#Bottom median
#198.8

# Scatter plot of all medians and maximums
tot.scat <- (RWHsum) %>%
  subset(Accumulation >= 2.38)%>%
  select(Date,
         medbottemp,
         maxbottemp,
         medmidtemp,
         maxmidtemp,
         medtoptemp,
         maxtoptemp) 
colnames(tot.scat) <- c("Date",
                        "Median Bottom",
                        "Maximum Bottom",
                        "Median Middle",
                        "Maximum Middle",
                        "Median Top",
                        "Maximum Top")
# View(tot.scat)
# Melt data set
tot.scat <- (tot.scat) %>%
  melt(id = "Date")
# Plot
ggplot(data = tot.scat, aes(x = Date))+
  geom_point(aes(y = value, shape = variable))+ 
  geom_hline(aes(yintercept = 21, color = "Trout Threshold"))+
  geom_vline(aes(xintercept = as.numeric(as.POSIXct("2017-10-12")), color = "Analysis Division"))+
  scale_shape_manual(values = c(1,16,0,15,2,17))+
  theme(legend.position = "bottom", 
        legend.title = element_blank())+
  scale_y_continuous(limits = c(5,30), expand = c(0,0))+
  scale_x_datetime(date_labels = "%m/%d", date_breaks = "10 days", limits = c(as.POSIXct("2017-08-28"),as.POSIXct("2017-11-22")))+
  labs(x = "Date", y = "Temperature (°C)")

# Hydrology analysis
# Returns a data frame of values same length as list


RWHflow.ana <- TFC_RWH.m %>%
  summarise(Accumulation = sum(rainfall, na.rm = TRUE),
            Runoff.vol = runoff.in(Accumulation, CN = 98),
            Outflow.vol = sum(Outflow, na.rm = TRUE),
            frac.out = (Outflow.vol/Runoff.vol) * 100,
            perc.red = ((Runoff.vol - Outflow.vol) / Runoff.vol) * 100)
# View(RWHflow.ana)

## Plot depth and rainfall
lar.evt.plot <- (RWHevents$`20`) %>%
  select(date.time,
         Bottom.depth,
         Middle.depth,
         Top.depth,
         rainfall) 
colnames(lar.evt.plot) <- c("date.time",
                            "Bottom",
                            "Middle",
                            "Top",
                            "Rainfall")
# Replace rainfall NAs with zero
lar.evt.plot$Rainfall <- (lar.evt.plot$Rainfall) %>%
  replace_na(0)
# Prep plotting dataset
lar.evt.plot <- (lar.evt.plot) %>%
  mutate(Rainfall = cumsum(Rainfall)) %>%
  select(date.time,
         Bottom,
         Middle,
         Top,
         Rainfall) %>%
  melt(id = "date.time")
# View(lar.evt.plot)
# plot depths
ggplot(data = lar.evt.plot)+
  geom_line(aes(x = date.time, y = value, color = variable))+
  labs(x = "Date", y = "Depth (cm)")+
  theme(legend.position = "bottom", 
        legend.title = element_blank())+
  scale_x_datetime(date_labels = "%m/%d", date_breaks = "10 days")+
  scale_y_continuous(sec.axis = sec_axis(~./1, name = "Rainfall (mm)"))

## How many events greater than 1in
inch.st <- (RWHsum) %>%
  subset(Accumulation >= 25.4)
## Rainfall histogram
rain.acc <- (RWHsum) %>%
  select(Accumulation)
ggplot(data = rain.acc, aes(x = Accumulation))+
  geom_histogram(binwidth = 8.128)+
  labs(x = "Rainfall Accumulation (mm)", y = "Discrete Events (count)")
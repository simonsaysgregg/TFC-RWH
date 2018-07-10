##### Alternative analysis following LSVFS
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

## Some Histograms
#hist(TFC_RWH.1$rainfall)
## Need to alter if interested no in.temp
#hist(TFC_RWH.1$in.temp)
#median(TFC_RWH.1$in.temp)
#median(TFC_RWH.1$in.temp, na.rm = TRUE)

### Normality Test
### Needs altering
#shapiro.test(CBC_LSVFS.1$in.temp, na.rm = TRUE)
#shapiro.test(CBC_LSVFS.1$in.temp)


###### Some monthyl stats
TFC_air.monthly.stats <- group_by(TFC_RWH.1, month=floor_date(TFC_RWH.1$date.time, "month")) %>% 
  summarise(Average.Temperature = mean(Air.temp, na.rm = TRUE),                                                                                     
            Maximum.Temperature = max(Air.temp, na.rm = TRUE),
            rain.sum = sum(rainfall, na.rm = TRUE))

##### Confirm
View(TFC_air.monthly.stats)

################ Seperate actions*************************
TFC_RWH.1.1 <- read.csv("TFC_RWH.DEL.csv")
## View to confirm proper read
#View(TFC_RWH.1.1)

## Set date time fomat
TFC_RWH.1.1$date.time <- as.POSIXct(TFC_RWH.1.1$date.time, format = "%m/%d/%y %H:%M", tz = "est")

## Confirm
#class(TFC_RWH.1.1[,1])

### rename columns
colnames(TFC_RWH.1.1) <- c("date.time", "rainfall", "intensity", "Air.temp", "Bottom.temp", "Bottom.depth", "Middle.temp", "Middle.depth", "Top.temp", "Top.depth", "event")
##Confirm
#View(TFC_RWH.1.1)


###### Change NAs to 0
TFC_RWH.1.1$rainfall[is.na(TFC_RWH.1.1$rainfall)] <- 0

#View(TFC_RWH.1.1)


####### Convert rainfall
### Add sumulative rainfall
## Need to convert intensity to mm/hr
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

## Confirm 
#View(TFC_RWH.1.2)

##### melting data set

TFC_RWH.1.2.m <- melt(TFC_RWH.1.2, id = c("date.time", 
                                    "rainfall", 
                                    "intensity", 
                                    "event", 
                                    "Bottom.depth", 
                                    "Middle.depth", 
                                    "Top.depth",
                                    "raincum"))

## Confirm
#View(TFC_RWH.1.2.m)

### rename columns
colnames(TFC_RWH.1.2.m) <- c("Date", 
                         "Rainfall", 
                         "Intensity", 
                         "Event", 
                         "Bottom.depth", 
                         "Middle.depth", 
                         "Top.depth",
                         "CumRainfall",
                         "Temperature_Location", 
                         "Temperature")
##Confirm
#View(TFC_RWH.1.2.m)

## Spliting text
RWHsplit.4 <- unlist(strsplit(as.character(TFC_RWH.1.2.m$Temperature_Location), "\\."))

## Reorganizing split
dim(RWHsplit.4) = c(2, nrow(TFC_RWH.1.2.m))
RWHsplit.4 <- data.frame(t(RWHsplit.4))

## Confirm
#View(RWHsplit.4)

#### Adding it back
TFC_RWH.1.2.m$Temperature_Location <- RWHsplit.4[,1]
#TFC_RWH.1.2m$temp <- RWHsplit.4[,2]

## Confirm
#View(TFC_RWH.1.2.m)

###### Second Melt
##### melting data set

TFC_RWH.1.2.m.1 <- melt(TFC_RWH.1.2.m, id = c("Date", "Rainfall", "Intensity", "Event", "CumRainfall", "Temperature_Location", "Temperature"))
## Confirm
#View(TFC_RWH.1.2.m.1)

### rename columns
colnames(TFC_RWH.1.2.m.1) <- c("Date", "Rainfall", "Intensity", "Event", "CumRainfall", "Temperature_Location", "Temperature", "Depth_Location", "Depth")
##Confirm
#View(TFC_RWH.1.2.m.1)

## Spliting text
RWHsplit.5 <- unlist(strsplit(as.character(TFC_RWH.1.2.m.1$Depth_Location), "\\."))

## Reorganizing split
dim(RWHsplit.5) = c(2, nrow(TFC_RWH.1.2.m.1))
RWHsplit.5 <- data.frame(t(RWHsplit.5))

## Confirm
#View(RWHsplit.5)

#### Adding it back
TFC_RWH.1.2.m.1$Depth_Location <- RWHsplit.5[,1]
#TFC_RWH.1.2.m.1$temp <- RWHsplit.5[,2]

## Confirm
#View(TFC_RWH.1.2.m.1)


### Plot raincum
ggplot(data = TFC_RWH.1.2.m.1)+
  geom_line(aes( x = Date, y = CumRainfall, linetype = "dots"))

### Complete plot
ggplot(data = TFC_RWH.1.2.m.1, aes(x = Date))+
  geom_line(aes(y = Temperature, linetype = Temperature_Location))+
  geom_line(aes(y = CumRainfall))+
  #scale_y_continuous(expand = c(0,0), limits = c(0,35)) +
  #theme_bw()+
  #theme_classic()+
  theme(plot.margin = unit(c(0,5,5,8.5),units="points")) +
  theme(legend.position = "bottom")+
  theme(legend.title = element_blank())+
  labs(x = "Date", y = "Temperature, (C)")
#geom_line(aes(y = Temperature, linetype = Temperature_Location))

##### Another attempt
p <- ggplot(data = TFC_RWH.1.2.m.1, aes(x = Date))+
     ggtitle("Temperature & Rainfall Plot")
p <- p + geom_line(aes(y = Temperature, linetype = Temperature_Location))

# adding the relative humidity data, transformed to match roughly the range of the temperature
p <- p + geom_line(aes(y = CumRainfall/6, linetype = "Rainfall"))

# now adding the secondary axis, following the example in the help file ?scale_y_continuous
# and, very important, reverting the above transformation
p <- p + scale_y_continuous(sec.axis = sec_axis(~.*6, name = "Cummulative Rainfall (mm)"))

# modifying colours and theme options
#p <- p + scale_colour_manual(values = c("blue", "red"))
p <- p + labs(y = "Temperature (°C)",
              x = "Date")
p <- p + theme(legend.position = "bottom", legend.title = element_blank(), plot.title = element_text(hjust = 0.5))
p
##################################^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


###### Original actions ***************
##### melting data set

TFC_RWH.m <- melt(TFC_RWH.1, id = c("date.time", 
                                    "Rainfall", 
                                    "Intensity", 
                                    "event", 
                                    "Bottom.depth", 
                                    "Middle.depth", 
                                    "Top.depth"))

## Confirm
#View(TFC_RWH.m)

### rename columns
colnames(TFC_RWH.m) <- c("Date", "Rainfall", "Intensity", "Event", "Bottom.depth", "Middle.depth", "Top.depth", "Temperature_Location", "Temperature")
##Confirm
#View(TFC_RWH.m)

## Spliting text
RWHsplit <- unlist(strsplit(as.character(TFC_RWH.m$Temperature_Location), "\\."))

## Reorganizing split
dim(RWHsplit) = c(2, nrow(TFC_RWH.m))
RWHsplit <- data.frame(t(RWHsplit))

## Confirm
#View(RWHsplit)

#### Adding it back
TFC_RWH.m$Temperature_Location <- RWHsplit[,1]
#TFC_RWH.m$temp <- RWHsplit[,2]

## Confirm
#View(TFC_RWH.m)

###### Second Melt
##### melting data set

TFC_RWH.m.1 <- melt(TFC_RWH.m, id = c("Date", "Rainfall", "Intensity", "Event", "Temperature_Location", "Temperature"))
## Confirm
#View(TFC_RWH.m.1)

### rename columns
colnames(TFC_RWH.m.1) <- c("Date", "Rainfall", "Intensity", "Event", "Temperature_Location", "Temperature", "Depth_Location", "Depth")
##Confirm
#View(TFC_RWH.m.1)

## Spliting text
RWHsplit.1 <- unlist(strsplit(as.character(TFC_RWH.m.1$Depth_Location), "\\."))

## Reorganizing split
dim(RWHsplit.1) = c(2, nrow(TFC_RWH.m.1))
RWHsplit.1 <- data.frame(t(RWHsplit.1))

## Confirm
#View(RWHsplit.1)

#### Adding it back
TFC_RWH.m.1$Depth_Location <- RWHsplit.1[,1]
#TFC_RWH.m$temp <- RWHsplit[,2]

## Confirm
#View(TFC_RWH.m.1)


## Mutate adds variables; converte temperatures for F to C
## Mutate adds new column variables
## Need to convert intensity to mm/hr
TFC_RWH.2 <- mutate(TFC_RWH.m.1, Rainfall = (Rainfall * 25.4), Intensity = (Intensity * 25.4), Temperature = (Temperature - 32)/1.8, Depth = (Depth * 30.48))

## Confirm 
#View(TFC_RWH.2)

### Replacing NAs######################## Seperate actions
#TFC_RWH.2[is.na(TFC_RWH.2)] <- 0

### Add sumulative rainfall
#TFC_RWH.2.1 <- ddply(TFC_RWH.2, .(Date), mutate, raincum = cumsum(Rainfall))

## Confirm 
#View(TFC_RWH.2.1)

### Plot raincum
#ggplot(data = TFC_RWH.2.1)+
#  geom_point(aes( x = Date, y = raincum))
###############################################^^^^^^^^^^^^^^^^^^^^^^^
##Write to file
write.csv(TFC_RWH.2, file = "TFC_RWH.2.csv")

## Need to convert units
TFC_RWH.3 <- mutate(TFC_RWH.1, rainfall = (rainfall * 25.4), intensity = (intensity * 25.4), air.temp = (air.temp - 32)/1.8, bot.temp = (bot.temp - 32)/1.8, mid.temp = (mid.temp - 32)/1.8, top.temp = (top.temp - 32)/1.8, bot.depth = (bot.depth * 30.48), mid.depth = (mid.depth * 30.48), top.depth = (top.depth * 30.48))

### Confirm
View(TFC_RWH.3)

##### finding events of interest
#### using subset
######## Needs work *******************************
#sum.max.evt <- TFC_RWH.2[TFC_RWH.2$Temperature_Location == "bot" & which.max(TFC_RWH.2$Temperature),]

### View summary
#View(sum.max.evt)

### Event == 20
#max.precip.event <- subset(TFC_RWH.2, Event == 20)

## confirm
#View(max.precip.event)
#################################^^^^^^^^^^^^^^^^^^

############# Finding maximum temperatures 
### subset August 
TFC_RWH.3.1 <- subset(TFC_RWH.3, date.time > as.POSIXct("2017-08-28") & date.time < as.POSIXct("2017-09-01"))

##### Confirm
#View(TFC_RWH.3.1)

### Max bot.temp August
TFC_RWH.3.1[which.max(TFC_RWH.3.1$bot.temp), ]
### Max mid.temp August
TFC_RWH.3.1[which.max(TFC_RWH.3.1$mid.temp), ]
### Max top.temp August
TFC_RWH.3.1[which.max(TFC_RWH.3.1$top.temp), ]

### subset September 
TFC_RWH.3.2 <- subset(TFC_RWH.3, date.time > as.POSIXct("2017-09-01") & date.time < as.POSIXct("2017-10-01"))

##### Confirm
#View(TFC_RWH.3.2)

### Max bot.temp September
TFC_RWH.3.2[which.max(TFC_RWH.3.2$bot.temp), ]
### Max mid.temp September
TFC_RWH.3.2[which.max(TFC_RWH.3.2$mid.temp), ]
### Max top.temp September
TFC_RWH.3.2[which.max(TFC_RWH.3.2$top.temp), ]

### subset October
TFC_RWH.3.3 <- subset(TFC_RWH.3, date.time > as.POSIXct("2017-10-01") & date.time < as.POSIXct("2017-11-01"))

View(TFC_RWH.3.3)

## Remove 10-04
TFC_RWH.3.3.a <- subset(TFC_RWH.3.3, date.time < as.POSIXct("2017-10-04") & date.time > as.POSIXct("2017-10-04"))

##### Confirm
View(TFC_RWH.3.3.a)

### Max bot.temp October
TFC_RWH.3.3.a[which.max(TFC_RWH.3.3.a$bot.temp), ]
### Max mid.temp October
TFC_RWH.3.3.a[which.max(TFC_RWH.3.3.a$mid.temp), ]
### Max top.temp October
TFC_RWH.3.3.a[which.max(TFC_RWH.3.3.a$top.temp), ]

### subset november 
TFC_RWH.3.4 <- subset(TFC_RWH.3, date.time > as.POSIXct("2017-11-01") & date.time < as.POSIXct("2017-12-01"))

##### Confirm
View(TFC_RWH.3.4)

### Max bot.temp november
TFC_RWH.3.4[which.max(TFC_RWH.3.4$bot.temp), ]



##### Boxplot Temperature all locations################# 
ggplot(data = TFC_RWH.2)+
  ggtitle("Monitoring Period Temperatures")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x = "Temperature Location", y = "Temperature (C)")+
  geom_boxplot(aes(x = Temperature_Location, y = Temperature))
  

### Remove Event 0
TFC_RWH.4 <- subset(TFC_RWH.2, Event != 0)

##### Boxplot Temperature No Event 0 ********************
ggplot(data = TFC_RWH.4)+
  geom_boxplot(aes(x = Temperature_Location, y = Temperature))+
  ggtitle("Event Temperatures")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x = "Temperature Location", y = "Temperature (C)")+
  geom_boxplot(aes(x = Temperature_Location, y = Temperature))


### histogram temperature
## needs stat_bin() info
#ggplot(data = TFC_RWH.2)+
 # geom_histogram(aes(x = Temperature_Location, y = Temperature, colour = Temperature_Location))

### Line plot
ggplot(data = TFC_RWH.2)+
  geom_line(aes(x = Date, y = Temperature, colour = Temperature_Location))

##### Advanced Plot
####### Verticl Rainfall plot
####### Needs some improvement; how to prevent top y-axis from being partially cut-off

g.top <- ggplot(TFC_RWH.2, aes(x = Date, y = Rainfall, ymin=0, ymax=Rainfall)) +
  #theme_bw()+
  geom_linerange() +
  scale_y_continuous(limits = c(1.0,0), expand = c(0,0), trans = "reverse") +
  #theme_classic()+
  theme(plot.margin = unit(c(5,5,-5,1),units="points"),
        axis.title.y = element_text(vjust =0.3)) +
  ggtitle("Rainfall & Temperature Plot")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(y = "Rain (mm/hr)")

g.bottom <- ggplot(data = TFC_RWH.2, aes(x = Date))+
  geom_line(aes(y = Temperature, linetype = Temperature_Location))+
  #geom_line(aes(y = Depth, linetype = Depth_Location))+
  #scale_y_continuous(expand = c(0,0), limits = c(0,35)) +
  #theme_bw()+
  #theme_classic()+
  theme(plot.margin = unit(c(0,5,5,8.5),units="points")) +
  theme(legend.position = "bottom")+
  theme(legend.title = element_blank())+
  labs(x = "Date", y = "Temperature, (C)")
#geom_line(aes(y = Temperature, linetype = Temperature_Location))

grid.arrange(g.top, g.bottom, heights = c(1/5, 4/5)) 



#### Summary of Events
event.sum.1 <- ddply(TFC_RWH.2, .(Event, Temperature_Location), summarise,
                     Start = min(Date),
                     End = max(Date),
                     Duration = max(Date) - min(Date),
                     Rainfall.Accumulation = sum(Rainfall, na.rm = TRUE),
                     Maximum.Intensity = max(Intensity),
                     Average.Temperature = mean(Temperature),
                     Median.Temperature = median(Temperature),
                     Mininum.Temperature = min(Temperature),
                     Maximum.Temperature = max(Temperature),
                     Temperature.Variance = var(Temperature))
#### Confirm 
#View(event.sum.1)

### Write .csv export data table
write.csv(event.sum.1, "Event.Sum.total.csv")

### Remove Event 0
event.sum.2 <- subset(event.sum.1, Event != 0)

### confirm
#View(event.sum.2)

### Write .csv export data table
write.csv(event.sum.2, "Event.Sum.short.csv")

### Reworking to get median and Maximum Boxplots###########
## Working with event.sum.2
### Remove air temperature
event.sum.2.1 <- subset(event.sum.2, Temperature_Location != "Air")

### View
#View(event.sum.2.1)

##### Boxplot Median Event Temperature 
ggplot(data = event.sum.2.1)+
  geom_boxplot(aes(x = Temperature_Location, y = Median.Temperature))+
  ggtitle("Median Event Temperatures")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(limits = c(5,30), expand = c(0,0)) +
  labs(x = "Temperature Location", y = "Temperature (°C)")
  #geom_boxplot(aes(x = Temperature_Location, y = Temperature))

##### Boxplot Maximum Event Temperature 
ggplot(data = event.sum.2.1)+
  geom_boxplot(aes(x = Temperature_Location, y = Maximum.Temperature))+
  ggtitle("Maximum Event Temperatures")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(limits = c(5,30), expand = c(0,0)) +
  labs(x = "Temperature Location", y = "Temperature (°C)")
#geom_boxplot(aes(x = Temperature_Location, y = Temperature))


###### Monthly stats
RWH_runoff.monthly.stats.1 <- group_by(event.sum.2, month=floor_date(event.sum.2$Start, "month")) %>% summarise(Median.Temperature = median(Median.Temperature), Maximum.Temperature = max(Maximum.Temperature))

##### Confirm
View(RWH_runoff.monthly.stats.1)

### boxplot
boxplot(RWH_runoff.monthly.stats.1$med.bot, RWH_runoff.monthly.stats.1$med.mid, RWH_runoff.monthly.stats.1$med.top, xlab = "Depth", ylab = "Temperature", boxfill = "light gray", outpch = 21:25, outlty = 2, bg = "pink", lwd = 2, medcol = "dark blue", medcex = 2, medpch = 20)

##### boxplot
ggplot(data = event.sum.2)
  geom_boxplot()

### Median Event Temperature Plots
ggplot(data = event.sum.2)+
  #theme_bw()+
  theme(legend.position = "bottom")+
  theme(legend.title = element_blank())+
  ggtitle("Median Temperature Plot")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_point(aes(x = Start, y = Median.Temperature, shape = Temperature_Location))+
  labs(x = "Date", y = "Median Temperature, (C)")
  
  
### Maximum Temperature Plots
  ggplot(data = event.sum.2)+
    #theme_bw()+
    geom_point(aes(x = Start, y = Maximum.Temperature, shape = Temperature_Location))

#### Finding events of interest
##### max precip
event.sum.1[which.max(event.sum.1$Rainfall.Accumulation),]

### Event == 20
max.precip.event <- subset(TFC_RWH.2, Event == 20)

## confirm
View(max.precip.event)

#### Plot event
ggplot(data = max.precip.event)+
  geom_line(aes(x = Date, y = Temperature, colour = Temperature_Location))

### Rainfall
ggplot(data = max.precip.event)+
  geom_point(aes(x = Date, y = Rainfall), na.rm = T)

##### find maximum temperature ######****** This returns the general maximum temperature
##### how do you apply another constratint to for subset commands, for location
event.sum.1[which.max(event.sum.1$Maximum.Temperature),]

#### subset event Event == 17
max.temp.event <- subset(TFC_RWH.2, Event == 17)

### confirm
View(max.temp.event)

#### Plot event
ggplot(data = max.temp.event)+
  geom_line(aes(x = Date, y = Temperature, colour = Temperature_Location))
  
#### Find event max temp @ bottom
event.sum.1[which(which.max(event.sum.1$Median.Temperature) & event.sum.1$Temperature_Location == "bot"),]

#### returns Event == 17 again
##### Subset Event == 
#max.med.event <- subset(CBC_LSVFS.m.1, Event == 14)

###### Plot Depth
ggplot(data = max.temp.event)+ 
  geom_line(aes(x = Date, y = Depth, colour = Depth_Location))
  
############### Monthly Stats############## Need to finish recreating monthly stats below*************
RWH_monthly.stats <- group_by(TFC_RWH.2, TFC_RWH.2$Event & month=floor_date(TFC_RWH.2$Date, "month")) %>%     summarise(min.lenght = min(d), max.lenght = max(duration), med.length = median(duration), min.rain = min(rainfall_acc, na.rm = TRUE), max.rain = max(rainfall_acc, na.rm = TRUE), med.rain = median(rainfall_acc, na.rm = TRUE), rainfall_sum = sum(rainfall_acc, na.rm = TRUE), max.intensity = max(intensity.peak, na.rm = TRUE), min.air = min(min.air, na.rm = TRUE), max.air = max(max.air, na.rm =TRUE), med.air = median(med.air, na.rm = TRUE))

############^^^^^^^^^^^^^^

######## Removal of small events from event summary*************
####### no events smaller than 0.3in 
event.sum.2[which.min(event.sum.2$Rainfall.Accumulation),]
###### Events greater than 1.365mm
length(event.sum.2[which(event.sum.2$Rainfall.Accumulation > 1.365), ])
##### Event s greater than 2.38mm
length(event.sum.2[which(event.sum.2$Rainfall.Accumulation > 2.38), ])
###### summary of rainfall
summary(event.sum.2$Rainfall.Accumulation)
##### sum rainfall
sum(event.sum.2$Rainfall.Accumulation)

###### Rainfall info#### histogram
ggplot(event.sum.2, aes())+
  geom_histogram(aes(x = Rainfall.Accumulation), binwidth = 5 )+
  ggtitle(" TFC Rainfall Accumulation Histogram")+
  labs( x = "Rainfall Accumulation (mm)", y = "Event Count")+
  theme(plot.title = element_text(hjust = 0.5))

### Subset events grater than 2.38mm
event.sum.2.2 <- subset(event.sum.2, Rainfall.Accumulation > 2.38)
## Veiw
#View(event.sum.2.2)
### Subset events remove air
event.sum.2.3 <- subset(event.sum.2.2, Temperature_Location != "Air")
## Veiw
View(event.sum.2.3)

##### Boxplot Median Event Temperature 
ggplot(data = event.sum.2.3)+
  geom_boxplot(aes(x = Temperature_Location, y = Median.Temperature))+
  ggtitle("Median Event Temperatures")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(limits = c(5,30), expand = c(0,0)) +
  labs(x = "Temperature Location", y = "Temperature (°C)")
#geom_boxplot(aes(x = Temperature_Location, y = Temperature))

##### Boxplot Maximum Event Temperature 
ggplot(data = event.sum.2.3)+
  geom_boxplot(aes(x = Temperature_Location, y = Maximum.Temperature))+
  ggtitle("Maximum Event Temperatures")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(limits = c(5,30), expand = c(0,0)) +
  labs(x = "Temperature Location", y = "Temperature (°C)")
#geom_boxplot(aes(x = Temperature_Location, y = Temperature))
###### Subsetting max middle and top temp events
### subset September 28-30 *******************
TFC_RWH.9.28.2 <- subset(TFC_RWH.3, date.time > as.POSIXct("2017-09-28") & date.time < as.POSIXct("2017-10-01"))

##### View 
View(TFC_RWH.9.28.2)

######### Maximums
TFC_RWH.9.28.2[which.max(TFC_RWH.9.28.2$bot.temp), ]

###### Add a couple more days for understanding
### subset September 28-30 
TFC_RWH.9.28 <- subset(TFC_RWH.3, date.time > as.POSIXct("2017-09-25") & date.time < as.POSIXct("2017-10-08"))

##### View 
View(TFC_RWH.9.28)

###### Plot temp
#### Plot event
ggplot(data = TFC_RWH.9.28.1)+
  geom_line(aes(x = date.time, y = air.temp))+
  geom_line(aes(x = date.time, y = bot.temp))+
  geom_line(aes(x = date.time, y = mid.temp))+
  geom_line(aes(x = date.time, y = top.temp))

######## Finding data collection of 10-04
### subset October 4 
TFC_RWH.10.04 <- subset(TFC_RWH.3, date.time > as.POSIXct("2017-10-04") & date.time < as.POSIXct("2017-10-05"))

##### View 
#View(TFC_RWH.10.04)

#### Plot 10-04
ggplot(data = TFC_RWH.10.04)+
  geom_line(aes(x = date.time, y = air.temp))+
  geom_line(aes(x = date.time, y = bot.temp))+
  geom_line(aes(x = date.time, y = mid.temp))+
  geom_line(aes(x = date.time, y = top.temp))

##### remove hour of 15
TFC_RWH.10.04.1 <- subset(TFC_RWH.10.04, date.time < as.POSIXct("2017-10-05 15:00") & date.time > as.POSIXct("2017-10-04 17:00"))

#### Plot 10-04 removal
ggplot(data = TFC_RWH.10.04.1)+
  geom_line(aes(x = date.time, y = air.temp))+
  geom_line(aes(x = date.time, y = bot.temp))+
  geom_line(aes(x = date.time, y = mid.temp))+
  geom_line(aes(x = date.time, y = top.temp))

### rename columns
colnames(TFC_RWH.9.28) <- c("date.time", "rainfall", "intensity", "Air.temp", "Bottom.temp", "Bottom.depth", "Middle.temp", "Middle.depth", "Top.temp", "Top.depth", "event")

##### Melting Data set for proper ploting 
##### melting data set

TFC_RWH.9.28.m <- melt(TFC_RWH.9.28, id = c("date.time", "rainfall", "intensity", "event", "Bottom.depth", "Middle.depth", "Top.depth"))

## Confirm
#View(TFC_RWH.9.28.m)

### rename columns
colnames(TFC_RWH.9.28.m) <- c("Date", "Rainfall", "Intensity", "Event", "Bottom.depth", "Middle.depth", "Top.depth", "Temperature_Location", "Temperature")
##Confirm
#View(TFC_RWH.9.28.m)

## Spliting text
RWHsplit.2 <- unlist(strsplit(as.character(TFC_RWH.9.28.m$Temperature_Location), "\\."))

## Reorganizing split
dim(RWHsplit.2) = c(2, nrow(TFC_RWH.9.28.m))
RWHsplit.2 <- data.frame(t(RWHsplit.2))

## Confirm
#View(RWHsplit.2)

#### Adding it back
TFC_RWH.9.28.m$Temperature_Location <- RWHsplit.2[,1]
#TFC_RWH.m$temp <- RWHsplit[,2]

## Confirm
#View(TFC_RWH.9.28.m)

###### Second Melt
##### melting data set

TFC_RWH.9.28.m.1 <- melt(TFC_RWH.9.28.m, id = c("Date", "Rainfall", "Intensity", "Event", "Temperature_Location", "Temperature"))
## Confirm
#View(TFC_RWH.9.28.m.1)

### rename columns
colnames(TFC_RWH.9.28.m.1) <- c("Date", "Rainfall", "Intensity", "Event", "Temperature_Location", "Temperature", "Depth_Location", "Depth")
##Confirm
#View(TFC_RWH.9.28.m.1)

## Spliting text
RWHsplit.3 <- unlist(strsplit(as.character(TFC_RWH.9.28.m.1$Depth_Location), "\\."))

## Reorganizing split
dim(RWHsplit.3) = c(2, nrow(TFC_RWH.9.28.m.1))
RWHsplit.3 <- data.frame(t(RWHsplit.3))

## Confirm
#View(RWHsplit.3)

#### Adding it back
TFC_RWH.9.28.m.1$Depth_Location <- RWHsplit.3[,1]
#TFC_RWH.m$temp <- RWHsplit[,2]

## Confirm
View(TFC_RWH.9.28.m.1)


### Points of interest
#TFC_RWH.9.28.topmax <- TFC_RWH.9.28[which.max(TFC_RWH.9.28$top.temp), ]

#View(TFC_RWH.9.28.topmax)

#TFC_RWH.9.28.midmax <- TFC_RWH.9.28[which.max(TFC_RWH.9.28$mid.temp), ]

### Max air temp during interest period
#TFC_RWH.9.28[which.max(TFC_RWH.9.28$air.temp), ]
## Max air temp of entire event
#TFC_RWH.3[which.max(TFC_RWH.3$air.temp), ]

###### Plot temperature
###### Plot Depth
######## Try to add points of interest at top and mid max temps
ggplot(data = TFC_RWH.9.28.m.1)+ 
  geom_line(aes(x = Date, y = Temperature, linetype = Temperature_Location))+
  #theme_bw()+
  #geom_point(data = TFC_RWH.9.28.topmax, aes(x = date.time, y = top.temp))+
  theme(legend.position = "bottom")+
  theme(legend.title = element_blank())+
  ggtitle("Middle & Top Maximum Temperature Event")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x = "Date", y = "Temperature, (°C)")



###### Reproducing median and maximums boxplots############################
####******** Unnecessary **************
### starting with beginning
#View(TFC_RWH.1)

### New dataframe
#TFC_RWH.5 <- TFC_RWH.1
## Confirm
#View(TFC_RWH.5)

### rename columns
#colnames(TFC_RWH.5) <- c("date.time", "rainfall", "intensity", "Air.temp", "Bottom.temp", "Bottom.depth", "Middle.temp", "Middle.depth", "Top.temp", "Top.depth", "event")
##Confirm
#View(TFC_RWH.5)

### Removing columns
#TFC_RWH.5.1 <- TFC_RWH.5[,c(1,4,5,7,9,11)]

### View
#View(TFC_RWH.5.1)

### rename columns
#colnames(TFC_RWH.5.1) <- c("Date", "Air", "Bottom", "Middle", "Top", "Event")
##Confirm
#View(TFC_RWH.5.1)

#TFC_RWH.5.2 <- mutate(TFC_RWH.5.1, Air = (Air - 32)/1.8, Bottom = (Bottom - 32)/1.8, Middle = (Middle - 32)/1.8, Top = (Top - 32)/1.8)

## Confirm 
#View(TFC_RWH.5.2)

#### Summary of Events: median
#event.sum.3.med <- ddply(TFC_RWH.5.2, .(Event), summarise,
#                     Air = median(Air),
#                     Bottom = median(Bottom),
#                     Middle = median(Middle),
#                     Top = median(Top))
#
#### Summary of Events: max
#event.sum.3.max <- ddply(TFC_RWH.5.2, .(Event), summarise,
#                         Air = max(Air),
 #                        Bottom = max(Bottom),
  #                       Middle = max(Middle),
   #                      Top = max(Top))

#### Confirm 
#View(event.sum.3)

## Subset to remove zero
#event.sum.3.med.1 <- subset(event.sum.3.med, Event != 0)

## Subset to remove zero
#event.sum.3.max.1 <- subset(event.sum.3.max, Event != 0)

## View
#View(event.sum.3.med.1)
## View
#View(event.sum.3.max.1)

#### Boxplots medians
#boxplot(event.sum.3.med.1$Bottom, 
 #       event.sum.3.med.1$Middle, 
  #      event.sum.3.med.1$Top, 
   #     xlab = "Depth", 
    #    ylab = "Temperature (C)", 
     #   boxfill = "light gray", 
      #  outpch = 21:25, 
       # outlty = 2, 
        #bg = "pink", 
#        lwd = 2, 
 #       medcol = "dark blue", 
  #      medcex = 2, 
   #     medpch = 20)

#### Boxplots medians
#ggplot(data = event.sum.3.med.1, aes(x = Air, y = Temparature))+
 # geom_boxplot()


############# Second attempt
####### Working with TFC_RWH.4
#View(TFC_RWH.4)
#############################^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

###### Rainfall info####
ggplot(event.sum.2, aes())+
  geom_histogram(aes(x = Rainfall.Accumulation), binwidth = 5 )+
  ggtitle("Rainfall Accumulation Histogram")+
  labs( x = "Rainfall Accumulation (mm)", y = "Event Count")+
  theme(plot.title = element_text(hjust = 0.5))

###### Plot depth
ggplot(data = TFC_RWH.2, aes( x = Date))+
  geom_line(aes(y = Depth, colour = Depth_Location))+
  theme(legend.position = "bottom")

###Another Depth Plot
p <- ggplot(data = TFC_RWH.1.2.m.1, aes(x = Date))+
  ggtitle("Depth & Rainfall Plot")
p <- p + geom_line(aes(y = Depth, linetype = Depth_Location))

# adding the relative humidity data, transformed to match roughly the range of the temperature
p <- p + geom_line(aes(y = CumRainfall, linetype = "Rainfall"))

# now adding the secondary axis, following the example in the help file ?scale_y_continuous
# and, very important, reverting the above transformation
p <- p + scale_y_continuous(sec.axis = sec_axis(~.*1, name = "Cummulative Rainfall (mm)"))

# modifying colours and theme options
#p <- p + scale_colour_manual(values = c("blue", "red"))
p <- p + labs(y = "Depth (cm)",
              x = "Date")
p <- p + theme(legend.position = "bottom", legend.title = element_blank(), plot.title = element_text(hjust = 0.5))
p

#### Depth plot without periods of interuption ***** didnt work#######################
View(TFC_RWH.1.2.m.1)
class(TFC_RWH.1.2.m.1$Date)

TFC_RWH.1.2.m.2 <- subset(TFC_RWH.1.2.m.1, Date != "2017-09-08")
View(TFC_RWH.1.2.m.2)


###Another Depth Plot W/O collection periods******  didnt work
p <- ggplot(data = TFC_RWH.1.2.m.2, aes(x = Date))+
  ggtitle("Depth & Rainfall Plot")
p <- p + geom_line(aes(y = Depth, linetype = Depth_Location))

# adding the relative humidity data, transformed to match roughly the range of the temperature
p <- p + geom_line(aes(y = CumRainfall, linetype = "Depth"))

# now adding the secondary axis, following the example in the help file ?scale_y_continuous
# and, very important, reverting the above transformation
p <- p + scale_y_continuous(sec.axis = sec_axis(~.*1, name = "Cummulative Rainfall (mm)"))

# modifying colours and theme options
#p <- p + scale_colour_manual(values = c("blue", "red"))
p <- p + labs(y = "Depth (cm)",
              x = "Date")
p <- p + theme(legend.position = "bottom", legend.title = element_blank(), plot.title = element_text(hjust = 0.5))
p



######### Event == 14 by date
###Beginning with 
View(TFC_RWH.1.2)

colnames(TFC_RWH.1.2) <- c("Date", "Rainfall", "Intensity", "Air.temp", "Bottom.temp", "Bottom.depth", "Middle.temp", "Middle.depth", "Top.temp", "Top.depth", "event", "RainCum")

event.14 <- subset(TFC_RWH.1.2, Date >= as.POSIXct("2017-09-19") & Date <= as.POSIXct("2017-09-23"))
View(event.14)

##### Melting Data set for proper ploting 
##### melting data set

event.14.1 <- melt(event.14, id = c("Date", "Rainfall", "Intensity", "event", "Bottom.depth", "Middle.depth", "Top.depth", "RainCum"))

## Confirm
#View(event.14.1)

### rename columns
colnames(event.14.1) <- c("Date", "Rainfall", "Intensity", "Event", "Bottom.depth", "Middle.depth", "Top.depth", "RainCum", "Temperature_Location", "Temperature")
##Confirm
#View(event.14.1)

## Spliting text
RWHsplit.6 <- unlist(strsplit(as.character(event.14.1$Temperature_Location), "\\."))

## Reorganizing split
dim(RWHsplit.6) = c(2, nrow(event.14.1))
RWHsplit.6 <- data.frame(t(RWHsplit.6))

## Confirm
#View(RWHsplit.6)

#### Adding it back
event.14.1$Temperature_Location <- RWHsplit.6[,1]
#TFC_RWH.m$temp <- RWHsplit[,2]

## Confirm
#View(event.14.1)

###### Second Melt
##### melting data set

event.14.2 <- melt(event.14.1, id = c("Date", "Rainfall", "Intensity", "Event", "RainCum", "Temperature_Location", "Temperature"))
## Confirm
#View(event.14.2)

### rename columns
colnames(event.14.2) <- c("Date", "Rainfall", "Intensity", "Event", "RainCum", "Temperature_Location", "Temperature", "Depth_Location", "Depth")
##Confirm
#View(event.14.2)

## Spliting text
RWHsplit.7 <- unlist(strsplit(as.character(event.14.2$Depth_Location), "\\."))

## Reorganizing split
dim(RWHsplit.7) = c(2, nrow(event.14.2))
RWHsplit.7 <- data.frame(t(RWHsplit.7))

## Confirm
#View(RWHsplit.7)

#### Adding it back
event.14.2$Depth_Location <- RWHsplit.7[,1]
#TFC_RWH.m$temp <- RWHsplit[,2]

## Confirm
#View(event.14.2)




p <- ggplot(data = event.14.2, aes(x = Date))+
  ggtitle("September 21st Rainfall & Depth Plot")
p <- p + geom_line(aes(y = Depth, linetype = Depth_Location))

# adding the relative humidity data, transformed to match roughly the range of the temperature
p <- p + geom_line(aes(y = RainCum*2.5, linetype = "Rainfall"))

# now adding the secondary axis, following the example in the help file ?scale_y_continuous
# and, very important, reverting the above transformation
p <- p + scale_y_continuous(sec.axis = sec_axis(~./2.5, name = "Cummulative Rainfall (mm)"))

# modifying colours and theme options
#p <- p + scale_colour_manual(values = c("blue", "red"))
p <- p + labs(y = "Depth (cm)",
              x = "Date")
p <- p + theme(legend.position = "bottom", legend.title = element_blank(), plot.title = element_text(hjust = 0.5))
p

########## Rainfall Histogram
ggplot(sum.stats.rwh.1, aes())+
  geom_histogram(aes(x = rainfall_acc), binwidth = 5 )+
  ggtitle("TFC Rainfall Accumulation Histogram")+
  labs( x = "Rainfall Accumulation (mm)", y = "Event Count")+
  theme(plot.title = element_text(hjust = 0.5))



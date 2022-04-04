#Edited by Nu Perera
#email: nuperera@ou.edu

install.packages("raster")
library(raster)
library(sp)
library(rgdal)
library(sf)
library(ggplot2)
library(geosphere)
library(dplyr)
library(lubridate)
library(sf)
library(maptools)
library(suncalc) 
library(tidyverse)
library(scales)
library(tidyr)
library(motus)
library(motusData)

setwd("/Users/gamageperera/Desktop/Motus/Motus/data_tools_master")
source("functions/data_manager.R")
source("functions/localization.R")


###EDIT THESE VALUES
infile <- "/Users/gamageperera/Desktop/Motus/Motus/data_tools_master/Winterbirds/tag_detections/Sensorstations/Tumbleweed_working/F21E1A341DB5"
outpath <- "/Users/gamageperera/Desktop/Motus/Motus/data_tools_master/Winterbirds/tag_detections/Sensorstations/Tumbleweed_working/F21E1A341DB5"

all_data <- load_data(infile)
beep_data <- all_data[[1]][[1]]
beep_data <- beep_data %>% 
  filter (!is.na(NodeId))

###looking for a file with the column names NodeId, lat, lng IN THAT ORDER
nodes <- node_file(all_data[[2]][[1]])
nodes$NodeId <- toupper(nodes$NodeId) 
write.csv(nodes,file = "/Users/gamageperera/Desktop/Motus/Motus/data_tools_master/nodes.csv" )
#nodes <- read.csv("/Users/gamageperera/Desktop/Motus/Motus/data_tools_master/nodes.csv")
tags <- read.csv("/Users/gamageperera/Desktop/Motus/Motus/data_tools_master/Winterbirds stationbeep_ 2021-09-16_220737 .csv", as.is=TRUE, na.strings=c("NA", "")) #uppercase node letters
#filter(Validated ==1)

#tags<-tags[!duplicated(tags$TagId),]
#tags<-tags %>%
  #filter(Validated==1)

#EXAMPLE POSSIBLE VALUES
tag_id <- tags$TagId

freq <- c("3 min", "10 min")
#freq<- c("1 min")

max_nodes <- 0 #how many nodes should be used in the localization calculation?
df <- merge_df(beep_data, nodes, tag_id, latlng = TRUE)

resampled <- advanced_resampled_stats(beeps = beep_data, node = nodes, freq = freq[1], tag_id = tag_id)
#removing N/A tagRSSI
resampled<- resampled %>%
  filter(!is.na(TagRSSI_sd))
resampled
p3 = ggplot(data=resampled, aes(x=freq, y=TagRSSI_max, group=NodeId, colour=NodeId)) +
  geom_line()
p3

#total number of observations for each bird
LOdf<- resampled %>%
  filter(!is.na(TagRSSI_sd)) %>%
  group_by(TagId) %>%
  summarise(number = n())
#three birds with most observations
LOdf_subset<-filter(resampled, TagId %in% c('61780778', '614B7878', '19784C52'))

write.csv(LOdf_subset,file = "/Users/gamageperera/Desktop/Motus/Motus/data_tools_master/LO_nodes.csv" )

#Activity budget
#If you want to change the date, make sure to have consecutive dates (not just the min and max time)
actbud<- dplyr::select(resampled, TagId, freq, NodeId, TagRSSI_sd, beep_count)
node_sum<-actbud %>%
  group_by(NodeId)%>%
  summarise(number = n())
write.csv(actbud,file = "/Users/gamageperera/Desktop/Motus/Motus/data_tools_master/actbud.csv" )

#Renaming nodes
actbud$NodeId=gsub("3287DE","W1",actbud$NodeId)
actbud$NodeId=gsub("328C94","W2",actbud$NodeId)
actbud$NodeId=gsub("328C26","W3",actbud$NodeId)
actbud$NodeId=gsub("328591","W4",actbud$NodeId)
actbud$NodeId=gsub("328497","W5",actbud$NodeId)
actbud$NodeId=gsub("32624B","N1",actbud$NodeId)
actbud$NodeId=gsub("328A8E","N2",actbud$NodeId)
actbud$NodeId=gsub("328147","N3",actbud$NodeId)
actbud$NodeId=gsub("325D6E","N4",actbud$NodeId)
actbud$NodeId=gsub("32926A","N5",actbud$NodeId)
actbud$NodeId=gsub("328E58","E1",actbud$NodeId)
actbud$NodeId=gsub("329306","E2",actbud$NodeId)
actbud$NodeId=gsub("325D80","E3",actbud$NodeId)
actbud$NodeId=gsub("325EAA","E4",actbud$NodeId)
actbud$NodeId=gsub("328D8E","E5",actbud$NodeId)
actbud$NodeId=gsub("328B4C","S1",actbud$NodeId)
actbud$NodeId=gsub("325CF3","S2",actbud$NodeId)
actbud$NodeId=gsub("327C23","S3",actbud$NodeId)
actbud$NodeId=gsub("3263FF","S4",actbud$NodeId)
actbud$NodeId=gsub("329934","S5",actbud$NodeId)
actbud

#Splitting date and time
actbud<- actbud %>%
  separate (freq, c("Date", "Time"), " ")
actbud

freq_sum2<- actbud %>%
  filter(TagId==61780778) %>%
  group_by(Time)

ggplot(data = filter(actbud, TagId == 61780778, 
                     Time > ("13:00:00"),
                     Time < ("23:58:00")),
       aes(x = Time, y = beep_count, col = as.factor(NodeId))) +
  theme_bw() + 
  geom_point() + 
  labs(title = "TagID 61780778", x = "Time of day", y = "Beep count") +
  scale_color_discrete(name = "NodeId") +
  facet_grid(NodeId ~ .) 

#sum of beep count across all nodes vs time
actbud1<- actbud %>%
  group_by(Time)%>%
  summarise(count=n(), beep_count=sum(beep_count))

ggplot(data = filter(actbud, TagId == 61780778, 
                     Time > ("13:00:00"),
                     Time < ("23:58:00")),
       aes(x = Time, y = beep_count, col = as.factor(NodeId))) +
  theme_bw() + 
  geom_point() + 
  labs(title = "TagID 61780778", x = "Time of day", y = "Beep count") +
  scale_color_discrete(name = "NodeId") +
  facet_grid(NodeId ~ .)


#
actbud2<-actbud1%>%
  filter(Time>"13:00:00")

ggplot(data=actbud2, mapping = aes(x=Time, y=beep_count))+
  theme(panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank())+
  theme(panel.grid.minor.x=element_blank(),panel.grid.major.x=element_blank())+
  geom_point()

#Plotting each individual separately
ggplot(data = actbud, aes(x=Time, y = beep_count)) +
  geom_point() +
  facet_wrap(facets = vars(TagId))


#adding sunrise
str(resampled)

resampled<-resampled%>%
  dplyr::rename_with("freq"="ts")

install.packages("lutz")
library(lutz)
actbud.tags <- resampled %>%
  arrange(TagId, freq) %>%
  mutate(freq = round_date(freq, "3 min")) %>% 
  count(TagId, freq) %>% #or use floor_date or ceiling_date, depending on what works
  mutate(node_lat_mode = 	36.37, node_lng_mode = -102.80) %>%
 dplyr::mutate(motus::sunRiseSet(.)) 
  collect() %>%
  as.data.frame()


#actbud.sub<-sunriset(resampled, lat="node_lat_mean", lon="node_lng_mean")
#Kate used "move" package to get solar day and night
#part of the motus package
#mutate(recvDeployLat = 	36.37, recvDeployLon = -102.80) %>%
#mutate(timeToSunriset(.)) %>%
#DARN.tags.angulate.final <- DARN.tags.angulate.final %>%
#arrange(motusTagID, ts) %>%
 # mutate(ts = as_datetime(ts + 0, tz = "UTC")) %>%
 # select (tagProjID, ts, sig, mfgID, motusTagID, runLen, 
        #  tagDepLat, tagDepLon, tagDeployID, recvDeployLat, recvDeployLon, recvDeployName, antBearing) %>%
#  mutate(timeToSunriset(.)) %>%
 # collect() %>%
 # as.data.frame()

DARN.tags.angulate.final <- rbind(DARN.tags.angulate.p1,DARN.tags.angulate.p2)
rm(DARN.tags.angulate.p1,DARN.tags.angulate.p2)

DARN.tags.angulate.final <- DARN.tags.angulate.final %>%
  arrange(motusTagID, ts) %>%
  mutate(ts = as_datetime(ts + 0, tz = "UTC")) %>%
  select (tagProjID, ts, sig, mfgID, motusTagID, runLen, 
          tagDepLat, tagDepLon, tagDeployID, recvDeployLat, recvDeployLon, recvDeployName, antBearing) %>%
  mutate(timeToSunriset(.)) %>%
  collect() %>%
  as.data.frame()

#-------------------------------------------------------------------------------------------------------------

df.alltags.sub <- sunRiseSet(df.alltags, lat = "recvDeployLat", lon = "recvDeployLon") 
geom_vline(aes(xintercept = sunrise), col = "orange") + 
  geom_vline(aes(xintercept = sunset), col = "blue")

ggplot(data = filter(actbud, TagId == 61780778, 
       freq > ("2021-03-26 19:33:00"),
       freq < ("2021-03-27 19:18:00")),
       aes(x = freq, y = beep_count, col = as.factor(NodeId))) +
  theme_bw() + 
  geom_point() + 
  labs(title = "TagID 61780778", x = "Time of day", y = "Beep count") +
  scale_color_discrete(name = "NodeId") +
  facet_grid(NodeId ~ .)
  
actbud_1<-actbud %>%
  filter(TagId==61780778) %>%
  filter(freq<"2021-03-27 19:18:000" & freq>"2021-03-26 19:33:00")
p4 = ggplot(data=actbud_1, aes(x=NodeId, y=beep_count, group=freq, colour=freq)) +
  geom_point()
p4

#-----------------------------------------------------------------------------------------
#getting sunrise and sunset times for Tumbleweed
day <-
  getSunlightTimes(
    date = seq.Date(as.Date("2021-01-20"), as.Date("2021-03-28"), by = 1),
    keep = c("sunrise", "sunriseEnd", "sunset", "sunsetStart"),
    lat = 36.490484, 
    lon = -102.649942,
    tz = "GMT"
  )

# Sunrise/set
day %>%
  mutate(
    date = as.POSIXct(date),
    day_length = as.numeric(sunset - sunrise)
  ) %>%
  ggplot(aes(x = date, y = day_length)) +
  geom_area(fill = "#FDE725FF", alpha = .4) +
  geom_line(color = "#525252") +
  scale_x_datetime(
    expand = c(0, 0),
    labels = date_format("%b '%y"),
    breaks =  seq(as.POSIXct(min(day$date)), as.POSIXct(max(day$date)), "month"),
    minor_breaks = NULL
  ) +
  scale_y_continuous(
    limits = c(0, 24),
    breaks = seq(0, 24, 2),
    expand = c(0, 0),
    minor_breaks = NULL
  ) +
  labs(x = "Date", y = "Hours", title = "Tumbleweed - Daytime duration") +
  theme_bw()


#Adding sunrise and sunset
sunrise.set <- function (lat, long, date, timezone = "GMT", direction = c("sunrise", "sunset"), num.days = 1) 
sunrise.set <- function (lat, long, date, timezone = "GMT", direction = c("sunrise", "sunset"))
# Why an argument saying how many days? You have the length of your dates
#lat.long <- matrix(c(long, lat), nrow = 1)
lat.long <- cbind(lon, lat)
day <- as.POSIXct(freq, tz = timezone)
# sequence <- seq(from = day, length.out = num.days, by = "days") # Your days object is fine
sunrise <- sunriset(lat.long, day, direction = "sunrise", POSIXct = TRUE)
sunset <- sunriset(lat.long, day, direction = "sunset", POSIXct = TRUE)
# I've replaced sequence with day here
ss <- data.frame(sunrise, sunset)
ss <- ss[, -c(1, 3)]
colnames(ss) <- c("sunrise", "sunset")
 if (direction == "sunrise") {
#return(ss[1,1])
 return(ss[,1])
 } else {
 #return(ss[1,2])
 return(ss[,2])
   }       


# add sunrise and sunset times to the dataframe
df.alltags.sub <- sunRiseSet(df.alltags, lat = "recvDeployLat", lon = "recvDeployLon") 

ggplot(data = filter(df.alltags.sub, mfgID == 78557878,
                     ts > ymd("2020-12-12"),
                     ts < ymd("2021-02-17"),
                     recvDeployName == "Tumbleweed"), 
       aes(x = ts, y = sig)) +
  theme_bw() + 
  geom_point() + 
  labs(x = "Time of year", y = "Signal strength") +
  geom_vline(aes(xintercept = sunrise), col = "orange") + 
  geom_vline(aes(xintercept = sunset), col = "blue")

# add sunrise and sunset times to the dataframe
actbud.sub <- sunRiseSet(resampled, lat = "node_lat_mean", lon = "node_lng_mean") 

ggplot(data = filter(actbud.sub, TagId==61780778,
                     freq > ("2021-03-26 19:33:00"),
                     freq < ("2021-03-27 19:18:00")),
       aes(x = freq, y = beep_count, col = as.factor(NodeId))) +
  theme_bw() + 
  geom_point() + 
  labs(title = "TagID 61780778", x = "Time of day", y = "Beep count") +
  scale_color_discrete(name = "NodeId") +
  facet_grid(NodeId ~ .)
  geom_vline(aes(xintercept = sunrise), col = "orange") + 
  geom_vline(aes(xintercept = sunset), col = "blue")

#-------------------------------------------------------------------------------------------  

#bird 61780778 was pinged at 3287DE the most, which is the closest west node. 32624B (north) and 328A8E(south) are in second

#grouping by freq
freq_sum<- actbud %>%
  filter(TagId==61780778) %>%
  group_by(freq)

ggplot(data = filter(actbud, TagId == 61780778, 
                     freq > ("2021-01-19 19:33:00"),
                     freq < ("2021-03-26 19:18:00")),
       aes(x = freq, y = beep_count, col = as.factor(NodeId))) +
  theme_bw() + 
  geom_point() + 
  labs(title = "TagID 61780778", x = "Time of day", y = "Beep count") +
  scale_color_discrete(name = "NodeId") +
  facet_grid(NodeId ~ .) 



#for one tagged bird
resampled_bird <- advanced_resampled_stats(beeps = beep_data, node = nodes, freq = freq[1], tag_id = tag_id)
#removing N/A tagRSSI
resampled_bird<- resampled_bird %>%
  filter(!is.na(TagRSSI_sd)) %>%
  filter(TagId==52196133) %>%
  filter(freq<"2021-01-22 19:48:00" & freq>"2021-01-21 21:03:00")
resampled_bird
p5 = ggplot(data=resampled_bird, aes(x=freq, y=TagRSSI_max, group=NodeId, colour=NodeId)) +
  geom_line()
p5

##### LOCATION METHODS########
###Example 1: Weighted Average### 

#code from tutorial
locations<- weighted_average(freq[1], beep_data, nodes, node_health=NULL, MAX_NODES=0, tag_id=NULL, 
                             calibrate = NULL, keep_cols = NULL, latlng = TRUE, minRSSI = 0)

#code from script
#locations <- weighted_average(freq[1], beep_data, nodes, all_data[[2]][[1]], 0, tag_id=tag_id, minRSSI = -105)



#locations <- weighted_average(freq[1], beep_data, nodes, all_data[[2]][[1]], 0, tag_id, minRSSI = -123)
#multi_freq <- lapply(freq, weighted_average, beeps=beep_data, node=nodes) 
#export_locs(freq, beep_data, nodes, tag_id, outpath)
######################



###Example 2: Triangulation###
#calibration data frame needs column names: pt, session_id, start, end, TagId, TagLat, TagLng
#start and end need to be in UTC
calibration <- read.csv("/Users/gamageperera/Desktop/Motus/Motus/data_tools_master/node_calibration.csv")
calibration<-subset(calibration, select=c(pt,session_id,start, end, TagId, TagLat, TagLng))
sapply(calibration,class)
#calibration$start<-as.numeric(as.factor(calibration$start))
#calibration$end<-as.numeric(as.factor(calibration$end))
calibration$start <- as.POSIXct(calibration$start, tz="UTC")
calibration$end <- as.POSIXct(calibration$end, tz="UTC")
calibrated <- calibrate(beep_data, calibration, nodes, calibrate = TRUE)
all_data <- calibrated[[1]]
relation <- relate(calibrated[[2]], calibrated[[3]], calibrated[[4]])
out <- triangulate(all_data, distance = relation)
##############################

#this is an example of filtering out locations based on a minimum number of 3 nodes
n <- 1
locations <- locations[locations$unique_nodes > n,]
#now convert to a .csv
locations <- cbind(locations, locations@coords)
CCLO_motus<-locations@data
#change path name where you want .csv to be saved
write.csv(CCLO_motus,file = "/Users/gamageperera/Desktop/Motus/Motus/data_tools_master/CCLO_motus.csv" )

time <- "1 day"
move <- as.data.table(locations)[, .(Lat = mean(avg_y), Lon = mean(avg_x), std_lat = sd(avg_y), 
                                     std_lon = sd(avg_x), .N), by = .(cut(freq, time),TagId)] #V = mean(SolarVolts), , 
move$lowlat <- move$Lat - move$std_lat
move$uplat <- move$Lat + move$std_lat
move$lowlon <- move$Lon - move$std_lon
move$uplon <- move$Lon + move$std_lon
move$d <- distVincentyEllipsoid(cbind(move$lowlon, move$lowlat), cbind(move$uplon, move$uplat))
move$d <- (move$d)/1000

nodes_spatial <- nodes
coordinates(nodes_spatial) <- 3:2
crs(nodes_spatial) <- CRS("+proj=longlat +datum=WGS84") 

#boulder_df <- locations[,c("TagId","avg_x","avg_y")]
#coordinates(boulder_df) <- 2:3
#utm <- CRS(paste0("+proj=utm +zone=", locations$zone[1], "+datum=WGS84"))
#crs(boulder_df) <- utm
#boulder_df_geog <- spTransform(locations, proj4string(nodes_spatial))
my_locs <- locations[,1]
locs <- st_as_sf(my_locs)
my_nodes <- st_as_sf(nodes_spatial)

p5 <- ggplot() + 
  #geom_point(data=my_locs, aes(x=long,y=lat))
  #ggmap(ph_basemap) +
  geom_sf(data = locs, aes(colour=TagId), inherit.aes = FALSE) + 
  geom_sf(data = my_nodes) +
  geom_text(data = nodes, aes(x=lng, y=lat, label = NodeId), size = 4)
p5




#one tag
locs2<- locs %>%
  filter(TagId == "19552D55")

p6 <- ggplot() + 
  geom_sf(data = locs2, aes(colour=TagId), inherit.aes = FALSE) + 
  geom_sf(data = my_nodes) +
  geom_text(data = nodes, aes(x=lng, y=lat, label = NodeId), size = 4)
p6



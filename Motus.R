install.packages("rworldmap")
install.packages("plyr")
install.packages("circular")
install.packages("cowplot")
install.packages("patchwork")
install.packages("rnaturalearth")

library(devtools)
library(remotes)
library(maps)
library(tidyverse)
library(rworldmap)
library(ggmap)
library(lubridate)
library(motus)
library(motusData)
library(DBI)
library(RSQLite)
library(plyr)
library(dplyr)
library(circular)
library(ggplot2)
library(jpeg)
library(cowplot)
library(patchwork)
library(sf)
library(rnaturalearth)

#Accessing detection data 
#Jeremy's API token
my_token <- "a3f2ac24f321ada46740d251f365be284c1d5e18f3657de29ece0a7c9753da3d"

#https://github.com/rstudio/rstudio/issues/9219
options(connectionObserver = NULL)

# install motus for data download
#manipulation, visualization and analysis
install_github("MotusWTS/motus", force = TRUE)

#Install motusClient for data download
install_github("MotusWTS/motusClient", force = TRUE)

# install motusData package which contains sample datasets, 
#e.g., vanishBearing used in Chapter 7
install_github("MotusWTS/motusData", force = TRUE)

packageVersion("motus")

#Set system environment time to Greenwich mean time (UTC)
Sys.setenv(TZ = "GMT")

#Download data
setwd("/Users/gamageperera/Desktop/Motus/Motus")
getwd()
proj.num <-129
sql.motus<- tagme(projRecv = proj.num, new = TRUE, update = TRUE, 
                   dir = "./tag_data/")
sql.motus<- tagme(projRecv = proj.num, new = FALSE, update = FALSE, 
                  dir = "./tag_data/")
sql.motus

#downloading node data
node <- tbl(sql.motus, "nodeData")

nodeData <- function(src, resume = FALSE) {
  
  getBatches <- function(src) {
    dplyr::tbl(src$con, "batches") %>%
      dplyr::filter(.data$source == "ctt") %>%
      dplyr::pull(.data$batchID)
  }
  
  pageInitial <- function(batchID, projectID) {
    srvNodes(batchID = batchID, projectID = projectID)
  }
  
  pageForward <- function(b, batchID, projectID) {
    # Page forward
    nodeDataID <- b$nodeDataID[nrow(b)]
    
    # Try again
    srvNodes(batchID = batchID, projectID = projectID, nodeDataID = nodeDataID) 
  }
  
  pageDataByBatch(src, table = "nodeData", resume = resume,
                  getBatches, pageInitial, pageForward)
  
}
# specify the filepath where your .motus file is
# saved, and the file name.
file.name <- dbConnect(SQLite(), "./tag_data/project-129.motus")

# get a list of tables in the .motus file specified above.
dbListTables(file.name)

# get a list of variables in the 'species' table in
# the .motus file.
dbListFields(file.name, "species")

# this retrieves the 'alltags' table from the
# 'sql.motus' SQLite file we read in earlier
tbl.alltags <- tbl(sql.motus, "alltags") # virtual table
str(tbl.alltags)

# list the variable names in the table
tbl.alltags %>% collect() %>% names() 

df.alltags <- tbl.alltags %>% collect() %>% as.data.frame()

# field names
names(df.alltags) 

str(df.alltags) # structure of your data fields
head(df.alltags) # prints the first 6 rows of your df to the console
summary(df.alltags) # summary of each column in your df

#date time (ts) is in numeric, represents seconds since January 1, 1970.
#therefore, convert to time column in date/time format
df.alltags <- tbl.alltags %>%
  collect() %>%
  as.data.frame() %>% # for all fields in the df (data frame)
  mutate(ts = as_datetime(ts, tz = "UTC", origin = "1970-01-01"))


# the tz = "UTC" is not necessary here, provided you have set your system time to UTC
# ... but it serves as a useful reminder!



saveRDS(df.alltags, "./tag_data/df_alltags.rds")
write.csv(df.alltags, "./tag_data/df_alltags.csv")

#Number of registered tags
tbl.tags <- tbl(sql.motus, "tags") 
df.tags <- tbl.tags %>%
  filter(projectID == 129) %>%
  collect() %>%
  as.data.frame()

nrow(df.tags) # number of registered tags in the database
unique(df.tags$tagID) #View the motusTagIDs

#Number of registered tags that were deployed
tbl.tagDeps <- tbl(sql.motus, "tagDeps") 

df.tagDeps <- tbl.tagDeps %>%
  filter(projectID == 129) %>%
  collect() %>%
  as.data.frame() %>% # once in df format, can format dates with lubridate
  mutate(timeStart = as_datetime(tsStart),
         timeEnd = as_datetime(tsEnd)) 

anti_join(df.tags, df.tagDeps, by = "tagID") 

#Number of deployments per tag
df.alltags <- tbl(sql.motus, "alltags") %>% 
  collect() %>% 
  as.data.frame() %>%     # for all fields in the df (data frame)
  mutate(time = as_datetime(ts))

df.alltags %>%
  select(motusTagID, tagDeployID) %>%
  filter(!(is.na(tagDeployID))) %>% # remove NA tagDeployIDs
  distinct() %>%
  group_by(motusTagID) %>%
  mutate(n = n()) %>%
  filter(n > 1)

#Number of project receiver deployments
tbl.recvDeps <- tbl(sql.motus, "recvDeps") 

df.projRecvs <- tbl.recvDeps %>%
  filter(projectID == 129) %>%
  collect() %>%
  as.data.frame() %>%
  mutate(timeStart = as_datetime(tsStart),
         timeEnd = as_datetime(tsEnd))

summary(df.projRecvs)

#Timing of project receiver deployments
# put data in long format to simplify plotting (or use geom_segment)
df.projRecvs.long <- df.projRecvs %>%
  select(deviceID, deployID, timeStart, timeEnd) %>% 
  gather(when, time, c(timeStart, timeEnd)) %>%
  # fake end date:
  mutate(time = if_else(is.na(time), max(time, na.rm = TRUE) + duration(1, "month"), time)) 

ggplot(data = df.projRecvs.long, 
       aes(x = time, y = as.factor(deviceID), colour = as.factor(deployID))) +
  theme(legend.position = "none") +
  geom_line(lwd = 3) + 
  # instead, centre to the right
  geom_text(data = filter(df.projRecvs.long, when == "timeStart"), 
            aes(label = deployID), hjust = "left", nudge_y = 0.2, size = 3, angle = 45) +
  scale_color_viridis_d() +
  theme_bw() +
  labs(x = "Year", y = "Receiver ID")

#More detail for a given year (or all years)
ggplot(data = df.projRecvs.long, 
       aes(x = yday(time), y = as.factor(deviceID), colour = as.factor(deployID))) +
  theme_bw() +
  theme(legend.position = "none") + 
  geom_line(lwd = 3) + 
  # centre labels to the left
  geom_text(data = filter(df.projRecvs.long, when == "timeStart"), 
            aes(label = deployID), hjust = "left", nudge_y = 0.4, size = 3) +
  labs(x = "Day of year", y = "Receiver ID") +
  facet_grid(year(time) ~ ., scales = "free") +
  scale_color_viridis_d()

-----------------------------------------------------------------------------------------------------------------------------
#Load all receiver metadata
df.recvDeps <- tbl.recvDeps %>%
  collect() %>%
  as.data.frame() %>%
  mutate(timeStart = as_datetime(tsStart),
         timeEnd = as_datetime(tsEnd))
#Load base map
# include all of the Americas to begin
install.packages("rnaturalearthdata")
world <- ne_countries(scale = "medium", returnclass = "sf") 

"#Where do you download ne_lakes?"
lakes <- ne_load(type = "lakes", scale = "medium", category = 'physical',
                 returnclass = "sf",
                 destdir = paste0(getwd(), "./tag_data")) # use this if already downloaded shapefiles

# set map limits using detection locations; 
# ensure they include the deployment locations
xmin <- min(df.recvDeps$longitude, na.rm = TRUE) - 2
xmax <- -20 # restrict to the Americas (excluding a few points in Europe)
ymin <- -60 #min(df.recvDeps$longitude, na.rm = TRUE) - 2
ymax <- max(df.recvDeps$latitude, na.rm = TRUE) + 2

# map
#ggplot(data = world) + 
  #geom_sf(colour = NA) +
  #geom_sf(data = lakes, colour = NA, fill = "white") +
  #coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = FALSE) +
  #labs(x = "", y = "") + 
  #theme_bw() +  
  #geom_point(data = df.recvDeps, 
             #aes(longitude, latitude, colour = as.logical(projectID == 129)), 
             #size = 0.8, shape = 19) +
  #scale_colour_manual(values = c("grey30", "red"), name = "Project 129 Deployment") 

------------------------------------------------------------------------------------------------------------------------------------
#Data cleaning
sql.motus <- tagme(129, update = TRUE, dir = "./tag_data/")

#Preliminary filtering
tbl(sql.motus, "alltags") %>%
  select(hitID, runID, batchID, ts, motusFilter)

#Omitting dubious runs (0) and keep only good runs (1)
tbl.alltags.sub <- tbl(sql.motus, "alltags") %>%
  filter(motusFilter == 1)

#Filtering for short runs
tbl(sql.motus, "alltags") %>%
  select(hitID, runID, batchID, motusTagID, runLen) %>%
  filter(runLen <= 3)

#Compare to newly created filtered table
tbl.alltags.sub %>%
  select(hitID, runID, batchID, motusTagID, runLen) %>%
  filter(runLen <= 3)

#Tracking removed detections
df.block.0 <- tbl(sql.motus, "alltags") %>%
  filter(motusFilter == 0) %>%
  collect()

#Preliminary data checks
#Checking receivers
tbl.alltags.sub %>%
  filter(is.na(recvDeployLat) | is.na(recvDeployName)) %>%
  select(recvDeployLat, recvDeployLon, recvDeployName, recvDeployID, recv, 
         recvProjID, recvProjName) %>%
  distinct()

#------------------------------------------------------------------------------------
#If you take this route, remember that in all future examples you’ll 
#want to use your new, flatted data frame df.alltags.sub, not the 
#un-flatted tbl.alltags.sub
df.alltags.sub <- tbl.alltags.sub %>%
  collect() %>% # flatten your data
  mutate(recvDeployName = if_else(is.na(recvDeployName), 
                                  paste0(recvDeployLat, ":", recvDeployLon),
                                  recvDeployName))
#-----------------------------------------------------------------------------------

tbl.alltags.sub %>%
  filter(is.na(recvDeployLat) | is.na(recvDeployName)) %>%
  select(recvDeployLat, recvDeployLon, recvDeployName, recvDeployID, recv, 
         recvProjID, recvProjName, runLen) %>%
  distinct()

#Summarizing tag detection
#Simplify the data for plotting
df.summary <- tbl.alltags.sub %>%
  filter(tagProjID == 129, # keep only tags registered to the sample project
         !is.na(recvDeployLat) | !(recvDeployLat == 0)) %>% # drop data without lon/lat
  group_by(motusTagID, runID, recvDeployName, ambigID, 
           tagDepLon, tagDepLat, recvDeployLat, recvDeployLon) %>%
   dplyr::summarize(max.runLen = max(runLen), 
            ts = mean(ts), .groups = "drop") %>% 
  collect() %>%
  arrange(motusTagID, ts) %>%
  mutate(time = as_datetime(ts))
df.summary

#summarizing by runID to get max run length and mean time stamp:


#Subsetting Tumbleweed from df.alltags
df.tumbleweed<- df.alltags %>%
  filter(!is.na(speciesEN)) %>%
  filter(tagDeployTest == 0) %>%
  filter(recvDeployName == "Tumbleweed") %>%
  select(hitID,ts, sig, mfgID, port, speciesEN, antBearing) %>%
  group_by(mfgID) %>%
  arrange(mfgID, ts, sig, port) %>%
  collect() %>% as.data.frame() 

#Eliminate Port 5 (port 5 is nodes)
df.tumbleweed.subset<- df.tumbleweed[!(df.tumbleweed$port =="L5"),]

tbl.alltags <- tbl(sql.motus, "alltagsGPS")

#--------------------------------------------------------------------------------------
df.alltags.sub <- readRDS("./tag_data/df_alltags.rds") # change dir to local directory
# obtain a table object of the filter
tbl.filter <- getRunsFilters(sql.motus, "filtAmbigFalsePos")

# filter and convert the table into a dataframe, with a few modications
df.alltags.sub <- left_join(tbl.alltags, tbl.filter, by = c("runID", "motusTagID")) %>%
  mutate(probability = ifelse(is.na(probability), 1, probability)) %>%
  filter(probability > 0) %>%
  select(-noise, -slop, -burstSlop, -done, -bootnum, -codeSet, 
         -mfg, -nomFreq, -markerNumber, -markerType, -tagDepComments, 
         -fullID, -deviceID, -recvDeployAlt, 
         -speciesGroup, -gpsLat, -gpsLon, -recvSiteName) %>%
  collect() %>%
  mutate(time = as_datetime(ts),  # work with times AFTER transforming to flat file
         tagDeployStart = as_datetime(tagDeployStart),
         tagDeployEnd = as_datetime(tagDeployEnd),
         recvDeployName = if_else(is.na(recvDeployName), 
                                  paste(recvDeployLat, recvDeployLon, sep=":"), 
                                  recvDeployName))

#-------------------------------------------------------------------------------------

#Summarizing your data
sql.motus %>% 
  tbl("alltags") %>% 
  select(ts, motusTagID, runLen, speciesEN, tagDepLat, tagDepLon, 
         recvDeployLat, recvDeployLon) %>% 
  collect() %>%
  mutate(time = as_datetime(ts)) %>% 
  summary()

#Error in as.POSIXlt.default(x, tz = tz(x)) : 
#do not know how to convert 'x' to class “POSIXlt”
df.alltags.sub <- df.alltags.sub %>%
  mutate(year = year(time), # extract year from time
         doy = yday(time)) %>% # extract numeric day of year from time
  filter(!is.na(recvDeployLat))

head(df.alltags.sub)


df.alltags.sub.2 <- df.alltags.sub %>%
  mutate(hour = hour(time)) %>% 
  select(motusTagID, port, tagDeployStart, tagDepLat, tagDepLon, 
         recvDeployLat, recvDeployLon, recvDeployName, antBearing, speciesEN, year, doy, hour) %>% 
  distinct()

ggplot(data = df.alltags.sub.2, aes(x = hour, y = as.factor(motusTagID))) +
  theme_bw() +
  geom_point() + 
  labs(x = "Hour", y = "MotusTagID")

df.alltags.sub.2 <- arrange(df.alltags.sub.2, hour)

ggplot(data = filter(df.alltags.sub.2, year(tagDeployStart) == 2016), 
       aes(x = hour, y = recvDeployLat, col = as.factor(motusTagID), 
           group = as.factor(motusTagID))) + 
  theme_bw() + 
  geom_point() +
  geom_path() +
  labs(x = "Hour", y = "Receiver latitude") +
  scale_colour_discrete(name = "MotusTagID")

#This code used to work fine.
#Plotting data
#tags detected by tumbleweed
df.tumbleweed.subset.2 <- df.tumbleweed %>%
  mutate(hour = as.POSIXct(round(ts, "hour"))) %>% 
  distinct()

ggplot(data = df.tumbleweed.subset.2, aes(x = hour, y = as.factor(mfgID))) +
  theme_bw() +
  geom_point() + 
  labs(x = "Time (rounded to hour)", y = "mfgID")

#plotting signal strength
ggplot(data = filter(df.alltags, 
                     mfgID == 78557878,
                     recvDeployName == "Tumbleweed"),
       aes(x = ts, y = sig)) +
  theme_bw() + 
  geom_point() + 
  labs(x = "Time", y = "Signal strength")+
  facet_grid(recvDeployName ~ .)

#plotting signal strength
ggplot(data = filter(df.alltags, 
                     mfgID == 78557878,
                     recvDeployName == "Tumbleweed"),
       aes(x = ts, y = sig)) +
  theme_bw() + 
  geom_point() + 
  labs(x = "Time", y = "Signal strength")+
  facet_grid(recvDeployName ~ .)
  
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

#adding antenna bearings to the plot
ggplot(data = filter(df.alltags.sub, motusTagID == 49564, 
                     !is.na(recvDeployName),
                     ts > ymd("2020-12-12"),
                     ts < ymd("2021-02-17")),
       aes(x = ts, y = sig, col = as.factor(antBearing))) +
  theme_bw() + 
  geom_point() + 
  labs(title = "motusTagID 49564", x = "Time of day", y = "Signal strength") +
  scale_color_discrete(name = "Antenna bearing") +
  facet_grid(recvDeployName ~ .)

#plotting 49564 at tumbleweed
ggplot(data = filter(df.alltags.sub, motusTagID == 49564, 
                     recvDeployName == "Tumbleweed",
                     !is.na(recvDeployName),
                     ts > ymd("2020-12-12"),
                     ts < ymd("2021-02-17")),
       aes(x = ts, y = sig, col = as.factor(antBearing))) +
  theme_bw() + 
  geom_point() + 
  labs(title = "motusTagID 49564", x = "Time of day", y = "Signal strength") +
  scale_color_discrete(name = "Antenna bearing") +
  facet_grid(recvDeployName ~ .)

#plotting motusTagID == 49564 at tumbleweed from 01/01/2021 - 01/04/2021
ggplot(data = filter(df.alltags.sub, motusTagID == 49564, 
                     recvDeployName == "Tumbleweed",
                     !is.na(recvDeployName),
                     ts > ymd("2021-01-03"),
                     ts < ymd("2021-01-04")),
       aes(x = ts, y = sig, col = as.factor(antBearing))) +
  theme_bw() + 
  geom_point() + 
  labs(title = "motusTagID 49564 01/03/2021", x = "Time of day", y = "Signal strength") +
  scale_color_discrete(name = "Antenna bearing") +
  facet_grid(recvDeployName ~ .)

ggplot(data = filter(df.alltags.sub, motusTagID == 49564,
                     recvDeployName == "Tumbleweed",
                     !is.na(recvDeployName),
                     ts > ymd("2021-01-03"),
                     ts < ymd("2021-01-05")),
       aes(x = ts, y = sig, col = as.factor(antBearing))) +
  theme_bw() + 
  geom_point() + 
  labs(x = "Time of year", y = "Signal strength") +
  geom_vline(aes(xintercept = sunrise), col = "orange") + 
  geom_vline(aes(xintercept = sunset), col = "blue")


#Plotting 2A612A55 (motusTagID 49569) detections from all sensor stations
ggplot(data = filter(df.alltags.sub, motusTagID == 49564, 
                     !is.na(recvDeployName),
                     ts > ymd("2020-12-12"),
                     ts < ymd("2021-03-31")),
       aes(x = ts, y = sig, col = as.factor(antBearing))) +
  theme_bw() + 
  geom_point() + 
  labs(title = "motusTagID 49564", x = "Time of day", y = "Signal strength") +
  scale_color_discrete(name = "Antenna bearing") +
  facet_grid(recvDeployName ~ .)

#plotting 2A612A55 (motusTagID 49569) at tumbleweed from 01/017/2021 - 01/18/2021
#bird was detected by the handheld antenna at 10:30-10:35AM (16:30-16:35)

motusTagID_49569 <- ggplot(data = filter(df.alltags.sub, motusTagID == 49569, 
                     recvDeployName == "Tumbleweed",
                     !is.na(recvDeployName),
                     !is.na(antBearing),
                     ts > ("2021-01-17 16:30:00"),
                     ts < ("2021-01-17 16:35:00")),
       aes(x = ts, y = sig, col = as.factor(antBearing))) +
  theme_bw() + 
  geom_point() + 
  labs(title = "motusTagID 49569 - 01/17/2021", x = "Time of day", y = "Signal strength") +
  scale_color_discrete(name = "Antenna bearing") +
  facet_grid(recvDeployName ~ .)

#plotting 344B1961 (motusTagID 44632 ) at tumbleweed from 01/12/2021 - 01/13/2021
#bird was detected by the handheld antenna at 12:18PM (18:18)
motusTagID_44632<-ggplot(data = filter(df.alltags.sub, motusTagID == 44632, 
                     recvDeployName == "Tumbleweed",
                     !is.na(recvDeployName),
                     !is.na(antBearing),
                     ts > ("2021-01-12 17:00:00"),
                     ts < ("2021-01-12 23:00:00")),
       aes(x = ts, y = sig, col = as.factor(antBearing))) +
  theme_bw() + 
  geom_point() + 
  labs(title = "motusTagID 44632 - 01/12/2021", x = "Time of day", y = "Signal strength") + 
  scale_color_discrete(name = "Antenna bearing") +
  facet_grid(recvDeployName ~ .)

#Plotting 34784B2D (motusTagID 49550) detections from all sensor stations
ggplot(data = filter(df.alltags.sub, motusTagID == 49550, 
                     !is.na(recvDeployName),
                     ts > ymd("2020-12-12"),
                     ts < ymd("2021-03-31")),
       aes(x = ts, y = sig, col = as.factor(antBearing))) +
  theme_bw() + 
  geom_point() + 
  labs(title = "motusTagID 49550", x = "Time of day", y = "Signal strength") +
  scale_color_discrete(name = "Antenna bearing") +
  facet_grid(recvDeployName ~ .)

#Plotting 34784B2D (motusTagID 49550) detections from tumbleweed on 12/29/2020
#bird was detected by hand held antenna at tumbleweed at 14:55 (20:55)
motusTagID_49550<-ggplot(data = filter(df.alltags.sub, motusTagID == 49550, 
                     recvDeployName == "Tumbleweed",
                     !is.na(recvDeployName),
                     !is.na(antBearing),
                     ts > ("2020-12-29 20:50:00"),
                     ts < ("2020-12-29 21:00:00")),
       aes(x = ts, y = sig, col = as.factor(antBearing))) +
  theme_bw() + 
  geom_point() + 
  labs(title = "motusTagID 49550 - 12/29/2020", x = "Time of day", y = "Signal strength") + 
  scale_color_discrete(name = "Antenna bearing") +
  facet_grid(recvDeployName ~ .)

#Plotting 614B7878 (motusTagID 44636) detections from tumbleweed on 01/19/2021
#bird was detected by hand held antenna at tumbleweed at 11:14 (17:14)
motusTagID_44636<-ggplot(data = filter(df.alltags.sub, motusTagID == 44636, 
                     recvDeployName == "Tumbleweed",
                     !is.na(recvDeployName),
                     !is.na(antBearing),
                     ts > ("2021-01-19 17:00:00"),
                     ts < ("2021-01-19 17:20:00")),
       aes(x = ts, y = sig, col = as.factor(antBearing))) +
  theme_bw() + 
  geom_point() + 
  labs(title = "motusTagID 44636 - 01/19/2021", x = "Time of day", y = "Signal strength") + 
  scale_color_discrete(name = "Antenna bearing") +
  facet_grid(recvDeployName ~ .)

#Plotting 52196133 (motusTagID 49566) detections from all sensor stations
ggplot(data = filter(df.alltags.sub, motusTagID == 49566, 
                     !is.na(recvDeployName),
                     !is.na(antBearing),
                     ts > ymd("2020-12-12"),
                     ts < ymd("2021-03-31")),
       aes(x = ts, y = sig, col = as.factor(antBearing))) +
  theme_bw() + 
  geom_point() + 
  labs(title = "motusTagID 49566", x = "Time of day", y = "Signal strength") +
  scale_color_discrete(name = "Antenna bearing") +
  facet_grid(recvDeployName ~ .)

#Plotting 52196133 (motusTagID 49566) detections from all stations on 01/28/2021
#bird was detected by hand held antenna at RB 137 (close to Tumbleweed) at 13:44 (19:44), 14:20 (20:20) 
motusTagID_49566<-ggplot(data = filter(df.alltags.sub, motusTagID == 49566, 
                     !is.na(recvDeployName),
                     !is.na(antBearing),
                     ts > ("2021-01-28 19:35:00"),
                     ts < ("2021-01-28 20:25:00")),
       aes(x = ts, y = sig, col = as.factor(antBearing))) +
  theme_bw() + 
  geom_point() + 
  labs(title = "motusTagID 49566 - 01/28/2021", x = "Time of day", y = "Signal strength") + 
  scale_color_discrete(name = "Antenna bearing") +
  facet_grid(recvDeployName ~ .)




plot_grid(motusTagID_44632, motusTagID_44636, motusTagID_49550, motusTagID_49566, 
          motusTagID_49569, labels = "AUTO")


#testing one day
#Plotting 52196133 (motusTagID 49566) detections from all stations on 01/28/2021
#bird was detected by hand held antenna at RB 137 (close to Tumbleweed) at 13:44 (19:44), 14:20 (20:20) 
ggplot(data = filter(df.alltags.sub, motusTagID == 49566, 
                     !is.na(recvDeployName),
                     !is.na(antBearing),
                     ts > ("2021-01-28 12:00:00"),
                     ts < ("2021-01-28 21:00:00")),
       aes(x = ts, y = sig, col = as.factor(antBearing))) +
  theme_bw() + 
  geom_point() + 
  labs(title = "motusTagID 49566 - 01/28/2021", x = "Time of day", y = "Signal strength") + 
  scale_color_discrete(name = "Antenna bearing") +
  facet_grid(recvDeployName ~ .)


#Load detection data
sql.motus<-tagme(129, update = TRUE, dir = "./tag_data/")

#Plotting signal strength vs. time
tags <- tagme(129, update = FALSE, new = FALSE)
df_tags <- tbl(tags, "alltags") %>%
  filter(!is.na(recvDeployLat)) %>%  # Omit unknown receiver locations
  collect() %>%
  mutate(time = as_datetime(ts),
         date = as_date(time),
         ambig = if_else(!is.na(ambigID), "ambiguous tag", "okay tag"),
         # Create antenna bearing category
         antBearing_cat = case_when(antBearing ==90 ~ "N",
                                    antBearing == 0 ~ "E",
                                    antBearing == 180 ~ "S",
                                    antBearing == 270 ~ "W"),
         # Create run length category
         runLen_cat = cut(runLen, breaks = c(0, 5, 10, 20, Inf)),
         # Create hour bin category
         hourBin = hour(time))


#vanish bearing
df.vanish <- df.alltags %>%
  filter(!is.na(speciesEN)) %>%
  filter(tagDeployTest == 0) %>%
  filter(tagProjName == "U_OK") %>%
  mutate(recvDeployName = reorder(recvDeployName, recvDeployLat),
         motusTagID = as.factor(as.character(motusTagID))) %>% # order sites by latitude
  arrange(ts) # arrange by ts

ggplot(data = df.vanish, aes(x = ts, y = recvDeployLat, 
                             colour = as.factor(recvSiteName))) + 
  geom_point(pch = 21) +
  facet_wrap(~ motusTagID, scales = "free", ncol = 1) +
  theme_bw()



#Select individuals that show clear departure detections
ggplot(data = df.vanish, aes(x = ts, y = recvDeployLat, colour = as.factor(recvDeployName))) + 
  geom_point(pch = 21) +
  facet_wrap(~ motusTagID, scales = "free", ncol = 1) +
  theme_bw()


ggplot(data = filter(df.vanish, motusTagID == 49566, ts > "2021-01-17 00:00:00"), 
       aes(x = ts, y = sig, colour = as.factor(port))) + 
  theme_bw() + 
  geom_point(pch = 21) +
  facet_grid(recvDeployName ~ .)

#test# this bird was detected by the handheld antenna on 1/19/2021 at 17:14
#motusTagID 44636 on 2021-01-17
ggplot(data = filter(df.vanish, motusTagID == 44636, ts > "2021-01-19 17:00:00", ts < "2021-01-20 00:00:00"), 
       aes(x = ts, y = sig, colour = as.factor(port))) + 
  theme_bw() + 
  geom_point(pch = 21) +
  facet_grid(recvDeployName ~ .)

#obtain departure times
ggplot(data = filter(df.vanish, motusTagID == 44636), 
       aes(x = ts, y = sig, colour= as.factor(antBearing))) + 
  theme_bw() + 
  geom_point() +
  facet_grid(recvSiteName ~ .)

TagID_44636 <- ggplot(data = filter(df.vanish, 
                     motusTagID == 44636,  
                     ts > "2021-01-19 17:15:00",  
                     ts < "2021-01-19 17:30:00"), 
       aes(x = ts, y = sig, colour= as.factor(antBearing))) + 
  theme_bw() + 
  geom_point()

#motus tag id 49569 detected at tumbleweed on 1/17 at 16:30
ggplot(data = filter(df.vanish, motusTagID == 49569), 
       aes(x = ts, y = sig, colour= as.factor(antBearing))) + 
  theme_bw() + 
  geom_point() +
  facet_grid(recvSiteName ~ .)

TagID_49569<- ggplot(data = filter(df.vanish, 
                     motusTagID == 49569,  
                     ts > "2021-01-17 16:25:00",  
                     ts < "2021-01-17 16:40:00"), 
       aes(x = ts, y = sig, colour= as.factor(antBearing))) + 
  theme_bw() + 
  geom_point()

#motus tag id 44632 detected at tumbleweed on 12/12/2020 18:18
TagID_44632<- ggplot(data = filter(df.vanish, 
                     motusTagID == 44632,  
                     ts > "2020-12-12 18:15:00",  
                     ts < "2020-12-12 18:20:00"), 
       aes(x = ts, y = sig, colour= as.factor(antBearing))) + 
  theme_bw() + 
  geom_point()


#motus tag id 49550 detected at tumbleweed on 12/29/2020 20:55
TagID_49550 <- ggplot(data = filter(df.vanish, 
                     motusTagID == 49550,  
                     ts > "2020-12-29 20:50:00",  
                     ts < "2020-12-29 21:00:00"), 
       aes(x = ts, y = sig, colour= as.factor(antBearing))) + 
  theme_bw() + 
  geom_point()


plot_grid(TagID_44632, TagID_44636, TagID_49550, TagID_49569, labels = c('TagID_44632', 'TagID_44636', 'TagID_49550', 'TagID_49569'))


## create dataframe and assign column names
dep.49566 <- as.data.frame(cbind(49566, "2021-03-06 00:00:00"))

 ## create dataframes for the other two tags:
dep.49564 <- as.data.frame(cbind(49564, "2021-03-06 00:00:00"))
dep.49569 <- as.data.frame(cbind(49569, "2021-03-06 00:00:00"))

## put them all together
df.departTime <- rbind(dep.49566, dep.49564, dep.49569)
names(df.departTime) <- c("motusTagID", "ts_depart")

## convert time to posixCT using Lubridate functionality
df.departTime <- mutate(df.departTime, ts_depart = ymd_hms(ts_depart))

df.departTime

## Merge sample data with departure times, subset data, and calculate vanishing bearing

## Note that we use the recvSiteName to specify the departure station of
## interest. Depending on whether the station has moved or changed names with
## deployments, recvDeployID might be more appropriate.

depart.station <- "Tumbleweed"
min.sig <- -78.0691 # normally max/min sig comes from the complete raw data for a station
max.sig <- -17.8707

# in this case, right join should drop any individuals that don't have departure
# times in df.departTime
df.vanishBearing <- right_join(df.vanish, df.departTime, by = "motusTagID") %>%
  filter(ts >= ts_depart,
         recvDeployName == depart.station) %>%
  distinct() %>%
  mutate(sig.norm = (sig - (min.sig))/((max.sig)-(min.sig)), 
         circ.bear = circular(antBearing, 
                              type = c("angles"), 
                              units = c("degrees"), 
                              rotation = c("clock"))) %>% 
  group_by(motusTagID, recvDeployName, recvDeployLat, recvDeployLon) %>% 
  summarise(vanish.bearing = weighted.mean(circ.bear, sig.norm, na.rm = FALSE, 
                                           control.circular = list(type = "angles", 
                                                                   units = "degrees", 
                                                                   template = "none", 
                                                                   rotation = "clock")),
            minutes.used = as.duration(min(ts) %--% max(ts))) %>%
  as.data.frame()

# if you have many bearings/points, can use stack = TRUE
plot.circular(df.vanishBearing$vanish.bearing, zero = pi/2)
arrows.circular(mean(df.vanishBearing$vanish.bearing), zero = pi/2)

tagID <- 49564





#Read CSV
longspur_motus <- read.csv("longspur_motus.csv")
longspur_subset<- longspur_motus %>%
  filter(!is.na(recvDeployName)) %>%
  filter(tagDeployTest == 0) %>%
  select(motusTagID, sig, tsCorrected, recvDeployLat, recvDeployLon) %>%
  arrange(desc(sig)) %>%
  collect() %>% as.data.frame() 

#creating matrix
u.df.lo<-unique(x=longspur_subset[,c("motusTagID","tsCorrected","recvDeployLat","recvDeployLon")])
u.df.lo$point<-paste0("X",rownames(u.df.lo))

df.with.pointnames.lo<-left_join(longspur_subset,u.df.lo)
df.with.pointnames.lo$presence<-1
df.with.pointnames.lo<-unique(df.with.pointnames.lo)

#Error: Must extract column with a single valid subscript.
#x Subscript `var` has the wrong type `function`.
#ℹ It must be numeric or character.
attribute.LO<-spread(data=df.with.pointnames.lo,key=motusTagID,
                     value=presence,fill=0)
attribute.LO

attribute.LONG<-data.frame(t(attribute.LO[,12:ncol(attribute.LO)]))

names(attribute.LONG)<-attribute.LO$point
attribute.LONG

assoc_LONG=(as.matrix(attribute.LONG))
assoc_LONG[is.na(assoc_LONG)]=0
assoc_LONG

#Total points recorded for each bird
rowSums(assoc_LONG)

#Eliminating birds who has less than 5 data points
assoc_LONG.1=assoc_LONG[which(rowSums(assoc_LONG)>4),]
assoc_LONG.1
rowSums(assoc2.1)

#Eliminating empty columns
assoc2.2<-assoc2.1[,which(colSums(assoc2.1)>0)]
assoc2.2



# access tag and receiver metadata associated with
# project 129
metadata(sql.motus, projectIDs = proj.num)

#Number of registered tags
tbl.tags <- tbl(sql.motus, "tags")
df.tags <- tbl.tags %>% filter(projectID == proj.num) %>%
  collect() %>% as.data.frame()

# number of registered tags in the database
nrow(df.tags)

#View motus TagIDs
unique(df.tags$tagID)

#Number of registered tags that were deployed
tbl.tagDeps <- tbl(sql.motus, "tagDeps")
df.tagDeps <- tbl.tagDeps %>%
  filter(projectID == proj.num) %>%
  collect() %>%
  as.data.frame() %>% # once in df format, can format dates with lubridate
  mutate(tsStart = as_datetime(tsStart, tz = "UTC", origin = "1970-01-01"),
         tsEnd = as_datetime(tsEnd, tz = "UTC", origin = "1970-01-01"))
anti_join(df.tags, df.tagDeps, by = "tagID")

#look at range of metadata values
df.tagDeps %>% select(tagID, projectID, tsStart, tsEnd,
                      speciesID, latitude, longitude) %>% summary()

# generate list of species IDs in project 176
# metadata
sp.list <- unique(df.tagDeps$speciesID)
sp.list

# Species metadata
tbl.species <- tbl(sql.motus, "species")
tbl.species %>% filter(id %in% sp.list) %>% collect() %>%
  as.data.frame()




# save the df.alltags file as a .csv file in your
# data folder
write.csv(df.alltags, "./df.alltags.CSV")

# save the df.alltags file as an RDS file in your
# data folder
saveRDS(df.alltags, "./df.alltags.RDS")


checkVersion(sql.motus)

# reads in your file 'df.alltags.rds' saved in the
df.alltags.saved <- readRDS("./df.alltags.rds") 






#Load base map files
# Include all of the Americas to begin
na.lakes <- map_data(map = "lakes")
na.lakes <- mutate(na.lakes, long = long - 360)

na.map <- map_data(map = "world2")
na.map <- filter(na.map, region %in% c("USA"))

na.map <- mutate(na.map, long = long - 360)

#Map the locations of tag deployments

 # set limits to map based on locations of
# detections, ensuring they include the deployment
 # locations
xmin <- -100 #min(df.tagDeps$longitude, na.rm = TRUE) - 5
xmax <- max(df.tagDeps$longitude, na.rm = TRUE) + 5
ymin <- min(df.tagDeps$latitude, na.rm = TRUE) - 5
ymax <- max(df.tagDeps$latitude, na.rm = TRUE) + 5

# map using ggplot
ggplot(na.lakes, aes(long, lat)) + geom_polygon(data = na.map,
                    aes(long, lat, group = group), colour = "grey",
                    fill = "grey98") + geom_polygon(aes(group = group),
                    colour = "grey", fill = "white") + coord_map(projection = "mercator",
                                                                                                                             xlim = c(xmin, xmax), ylim = c(ymin, ymax)) + xlab("") +
  ylab("") + theme_bw() + geom_point(data = filter(df.tagDeps,
                      projectID == 176), aes(longitude, latitude), cex = 2,
                                     pch = 1, colour = "red")


#Load detections data
tbl.alltags<- tbl(cclo.motus,"alltags")
str(df.alltags)
df.alltags <- tbl.alltags %>%
  mutate(recvLat = if_else((is.na(gpsLat)|gpsLat == 0),
                           recvDeployLat, gpsLat),
         recvLon = if_else((is.na(gpsLon)|gpsLon == 0),
                           recvDeployLon, gpsLon),
         recvAlt = if_else(is.na(gpsAlt), recvDeployAlt, gpsAlt)) %>%
  select(-noise, -slop, -burstSlop, -done, -bootnum, -mfgID,
         -codeSet, -mfg, -nomFreq, -markerNumber, -markerType,
         -tagDeployComments, -fullID, -deviceID, -recvDeployLat,
         -recvDeployLon, -recvDeployAlt, -speciesGroup, -gpsLat,
         -gpsLon, - recvAlt, - recvSiteName) %>%
  collect() %>%
  as.data.frame() %>%
  mutate(ts = as_datetime(ts))
         # work with dates AFTER transforming to flat file


df.alltags <- tbl.alltags %>%
  select(-noise, -slop, -burstSlop, -done, -bootnum, -mfgID,
         -codeSet, -mfg, -nomFreq, -markerNumber, -markerType,
         -fullID, -deviceID, -recvDeployLat,
         -recvDeployLon, -recvDeployAlt, -speciesGroup,
         - recvSiteName) %>%
  collect() %>%
  as.data.frame() %>%
  mutate(ts = as_datetime(ts))


df.alltags %>%
  filter(tagProjID == proj.num) %>% # subset to include only tags registered to project
  mutate(rl.gt.2 = runLen == 2) %>%
  group_by(motusTagID, rl.gt.2) %>%
  tally() %>%
  spread(key = rl.gt.2, value=n)
df.alltags.sub <- filter(df.alltags, runLen > 2)
df.block.0 <- filter(df.alltags, runLen == 2) %>% select(motusTagID,
                                                         runID) %>% distinct()
filter(df.alltags.sub, is.na(recvLat)) %>% select(recvLat,
                                                  recvLon, recvDeployName, recvDeployID, recv, recvProjID,
                                                  recvProjName) %>% distinct()

#Unnecessary--------------------------------------------------------------------------------------------------------#
#Download data for a receiver for the first time - Tumbleweed
proj.num = "CTT-F21E1A341DB5"
sql.tumbleweed <- tagme(projRecv = proj.num, new = TRUE,
                        update = TRUE)

# specify the filepath where your .motus file is
# saved, and the file name.
file.name_tum <- dbConnect(SQLite(), "./CTT-F21E1A341DB5.motus")

# get a list of tables in the .motus file specified above.
dbListTables(file.name_tum)

# get a list of variables in the 'species' table in
# the .motus file.
dbListFields(file.name_tum, "species")

# this retrieves the 'alltags' table from the
# 'sql.motus' SQLite file we read in earlier
tbl.alltags.tum <- tbl(sql.tumbleweed, "alltags") # virtual table
str(tbl.alltags.tum)

# list the variable names in the table
tbl.alltags.tum %>% collect() %>% names() 

df.alltags.tum <- tbl.alltags.tum %>% collect() %>% as.data.frame()

# field names
names(df.alltags.tum) 

str(df.alltags.tum) # structure of your data fields
head(df.alltags.tum) # prints the first 6 rows of your df to the console
summary(df.alltags.tum) # summary of each column in your df

df.alltags.tum <- tbl.alltags.tum %>%
  collect() %>%
  as.data.frame() %>% # for all fields in the df (data frame)
  mutate(ts = as_datetime(ts, tz = "UTC", origin = "1970-01-01"))


#Select columns I need for Tumbleweed
df.cclo_tum<- df.alltags.tum %>%
  select(hitID, ts, tsCorrected, sig, motusTagID) %>%
  arrange(motusTagID, desc(sig)) %>%
  collect() %>% as.data.frame()

#Test
df.cclo_tum2 <- tbl.alltags.tum %>%
  collect() %>%
  as.data.frame() %>% # for all fields in the df (data frame)
  mutate(ts = as_datetime(ts, tz = "UTC", origin = "1970-01-01") - 21600,
         test_date = format(ts, "%m-%d-%Y"),
         test_time = format(ts, "%H:%M:%S")) 


write.csv(df.cclo_tum, "/Users/gamageperera/Desktop/Motus/Motus\\df.cclo_tum.csv", row.names = FALSE)

write.csv(df.cclo_tum, "/Users/gamageperera/Desktop/Motus/Motus\\df.cclo_tum2.csv", row.names = FALSE)


#Convert UTC to CDT - unnecessary
# Test (extract year and numeric day from ts )

df.alltags2 <- tbl.alltags %>%
  collect() %>%
  as.data.frame() %>% # for all fields in the df (data frame)
  mutate(ts = as_datetime(ts, tz = "UTC", origin = "1970-01-01"),
         test_date = format(ts, "%m-%d-%Y"),
         test_time = format(ts, "%H:%M:%S")) #extract numeric day of year from ts

#Test (use tsCorected instead ts)
df.alltags3 <- tbl.alltags %>%
  collect() %>%
  as.data.frame() %>% # for all fields in the df (data frame)
  mutate(tsCorrected = as_datetime(tsCorrected, tz = "UTC", origin = "1970-01-01"),
         test_date = format(tsCorrected, "%m-%d-%Y"),
         test_time = format(tsCorrected, "%H:%M:%S")) #extract numeric day of year from ts

#Test (adjusting time zone)
df.alltags4 <- tbl.alltags %>%
  filter(speciesEN == "Chestnut-collared Longspur") %>%
  collect() %>%
  as.data.frame() %>% # for all fields in the df (data frame)
  mutate(tsCorrected = as_datetime(tsCorrected, tz = "UTC", origin = "1970-01-01") - 21600,
         test_date = format(tsCorrected, "%m-%d-%Y"),
         test_time = format(tsCorrected, "%H:%M:%S")) 

#Test (adjusting timezone using ts)
df.alltags5 <- tbl.alltags %>%
  filter(speciesEN == "Chestnut-collared Longspur") %>%
  collect() %>%
  as.data.frame() %>% # for all fields in the df (data frame)
  mutate(ts = as_datetime(ts, tz = "UTC", origin = "1970-01-01") - 21600,
         test_date = format(ts, "%m-%d-%Y"),
         test_time = format(ts, "%H:%M:%S"))   

#Test (adjusting timezone using ts) - only Tumbleweed
df.alltags6 <- tbl.alltags %>%
  filter(recvDeployName == "Tumbleweed") %>%
  collect() %>%
  as.data.frame() %>% # for all fields in the df (data frame)
  mutate(ts = as_datetime(ts, tz = "UTC", origin = "1970-01-01") - 21600,
         test_date = format(ts, "%m-%d-%Y"),
         test_time = format(ts, "%H:%M:%S")) 

#Selecting columns I need
df.cclo_tumbleweed<- df.alltags6 %>%
  select(hitID, test_date, test_time, sig, motusTagID, port, speciesEN) %>%
  arrange(motusTagID, desc(sig)) %>%
  collect() %>% as.data.frame()

#Test (Getting rid of N/As in SpeciesEN column)
df.cclo_tumbleweed2 <- df.alltags6 %>%
  filter(!is.na(speciesEN)) %>%
  filter(tagDeployTest == 0) %>%
  select(hitID, test_date, test_time, sig, mfgID, port, speciesEN) %>%
  arrange(mfgID, desc(sig)) %>%
  collect() %>% as.data.frame()
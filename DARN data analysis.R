# ##########################################################################
# # DUSTBOWL AUTOMATED RADIO-TELEMETRY NETWORK (DARN) DATA ANALYSIS - REVISED
# # Optimized for DARN data focus and departure inference
# # ##########################################################################

# Load necessary libraries
# Note: Install them first if needed: install.packages(c("tidyverse", "motus", "DBI", "RSQLite", "lubridate", "geosphere", "suncalc", "sf", "data.table"))
library(tidyverse)
library(motus)
library(DBI)
library(RSQLite)
library(lubridate)
library(geosphere) # For bearing and distance calculations
library(suncalc)   # For twilight calculations
library(sf)        # For modern spatial data handling (replaces sp/rgdal)
library(data.table)

# Set global environment variables
Sys.setenv(TZ = "UTC")
setwd("/Users/kimber/Longspurs") #Alter as needed for individual computers

# DARN Project ID
proj.num.OU <- 129 # Motus project "University of Oklahoma" (#129)

# -------------------------------------------------------------------------
# 1. Data Download, Filtering, and Antenna Edits
# -------------------------------------------------------------------------

# Download/load OU project data
OU.motus <- tagme(
  projRecv = proj.num.OU,
  new = FALSE,
  forceMeta = TRUE,
  update = TRUE, # Set to FALSE if you are only loading from a local file
  dir = "/Users/kimber/Longspurs" #Alter as needed for individual computers 
)

# Define DARN spatial boundaries
DARN_BOUNDS <- list(
  lat_min = 36.2, lat_max = 36.6,
  lon_min = -103.1, lon_max = -102.1
)

# Extract and immediately filter to DARN tags/detections (EFFICIENT LOADING)
full_data <- tbl(OU.motus, "alltags") %>%
  # --- Mandatory Filters ---
  filter(tagDeployTest == 0) %>% # Restrict to non-test deployments
  filter(recvDeployName != "CTT HQ") %>%
  # --- Filter to DARN RECEIVER area ---
  filter(between(recvDeployLat, DARN_BOUNDS$lat_min, DARN_BOUNDS$lat_max)) %>%
  filter(between(recvDeployLon, DARN_BOUNDS$lon_min, DARN_BOUNDS$lon_max)) %>%
  # --- Select core columns for analysis ---
  select(
    speciesID, speciesEN, tagProjID, ts, sig, port, mfgID, motusTagID, runLen,
    tagDepLat, tagDepLon, tagDeployID, recvDeployLat, recvDeployLon, recvDeployName,
    antBearing, antHeight, nodeNum
  ) %>%
  collect() %>% # Bring data from SQL table into an R data frame
  # --- Final cleanup & preparation ---
  mutate(
    ts = as_datetime(ts, tz = "UTC", origin = "1970-01-01") # Ensure correct POSIXct format
  ) %>%
  filter(ts > "2023-10-01") # Apply the date filter from your original script

# Apply Antenna Bearing Edits (Consolidating the edits from original script)
DARN_detections <- full_data %>%
  mutate(
    antBearing_adj = case_when(
      recvDeployName == "New Playa" ~ antBearing - 15,
      recvDeployName == "Felt" ~ antBearing - 15,
      recvDeployName == "Nosebleed" ~ antBearing - 10,
      recvDeployName == "O.R.B." ~ antBearing - 7.5,
      recvDeployName == "Mirage" ~ antBearing - 2.5,
      recvDeployName == "Blowdart" ~ antBearing - 5,
      recvDeployName == "Ying-Yang" ~ antBearing + 25,
      recvDeployName == "Windblown" ~ antBearing - 7,
      TRUE ~ antBearing # Keep original if no match
    )
  )

# -------------------------------------------------------------------------
# 2. Multi-Tower Detections & Twilight Analysis (Goal 1)
# -------------------------------------------------------------------------

WINDOW_SIZE_SEC <- 2.5 # Window for near-simultaneous detections

# Step 1: Group detections into single 'events'
DARN_events <- DARN_detections %>%
  arrange(motusTagID, ts, recvDeployName, port) %>%
  group_by(motusTagID) %>%
  mutate(ts_diff = as.numeric(ts) - lag(as.numeric(ts))) %>%
  mutate(is_new_event = ifelse(is.na(ts_diff) | ts_diff > WINDOW_SIZE_SEC, 1, 0)) %>%
  mutate(event_id = cumsum(is_new_event)) %>%
  ungroup() %>%
  group_by(event_id) %>%
  mutate(
    w_towers = n_distinct(recvDeployName), # Count of towers in event
    w_antennas = n_distinct(recvDeployName, port) # Count of unique antennas
  ) %>%
  ungroup() %>%
  select(-ts_diff, -is_new_event)

# Step 2: Twilight Analysis (Using 'suncalc')
darn_lat <- mean(DARN_detections$recvDeployLat)
darn_lon <- mean(DARN_detections$recvDeployLon)

twilight_data <- DARN_events %>%
  mutate(Date = as_date(ts)) %>%
  distinct(Date) %>%
  rowwise() %>%
  mutate(
    twilight_times = list(
      getSunlightTimes(
        date = Date, lat = darn_lat, lon = darn_lon,
        keep = c("nauticalDawn", "sunrise", "sunset", "nauticalDusk"),
        tz = "UTC"
      )
    )
  ) %>%
  unnest(twilight_times) %>% 
  ungroup()

# Join twilight data and assign time-of-day category
DARN_events_twilight <- DARN_events %>%
  mutate(date = as_date(ts)) %>%
  left_join(twilight_data, by = "date") %>%
  mutate(
    time_of_day = case_when(
      ts <= nauticalDawn | ts >= nauticalDusk ~ "Night",
      ts > nauticalDawn & ts < sunrise ~ "Pre-Sunrise Twilight (Nautical/Civil)",
      ts > sunset & ts < nauticalDusk ~ "Post-Sunset Twilight (Civil/Nautical)",
      TRUE ~ "Day"
    )
  )

# Goal 1 Output: Evaluate multi-tower detections relative to twilight
DARN_twilight_summary <- DARN_events_twilight %>%
  group_by(event_id) %>%
  slice(1) %>%
  ungroup() %>%
  group_by(time_of_day, w_towers) %>%
  summarise(
    event_count = n(),
    .groups = "drop"
  )

print("--- Goal 1 Summary (Events by Twilight and Tower Count) ---")
print(DARN_twilight_summary)

# -------------------------------------------------------------------------
# 3. Triangulation Approximation (Weighted Centroid) (Goal 3)
# -------------------------------------------------------------------------

# Step 1: Filter to multi-tower events
Triangulation_Events <- DARN_events %>%
  filter(w_towers >= 2) %>%

  # Step 2: Calculate Weights and Centroids
  group_by(event_id) %>%
  mutate(
    # 1. Transform Signal Strength to a Weight
    max_sig = max(sig, na.rm = TRUE),
    # Stronger signal (sig closer to max_sig) gets a higher weight.
    weight = max_sig - sig + 1
  ) %>%
  summarise(
    # Calculate Simple Centroid
    simple_lat = mean(recvDeployLat),
    simple_lon = mean(recvDeployLon),

    # Calculate WEIGHTED Centroid
    weighted_lat = weighted.mean(recvDeployLat, w = weight),
    weighted_lon = weighted.mean(recvDeployLon, w = weight),

    ts = first(ts), # Keep time of first detection
    w_towers = first(w_towers),
    n_pings = n(),
    .groups = "drop"
  )

# Step 3: Convert Weighted Centroids to SF Points
Weighted_Centroid_sf <- Triangulation_Events %>%
  st_as_sf(coords = c("weighted_lon", "weighted_lat"), crs = 4326) %>%
  st_transform(crs = 32613) # UTM Zone 13N (for accurate spatial joins)

print("--- Goal 3: Weighted Centroid Dataframe Head ---")
print(head(Triangulation_Events, 5))

# -------------------------------------------------------------------------
# 4. Inferring Departure Location and Direction (Restructured Goal 2)
# -------------------------------------------------------------------------

# Step 1: Define the Last Significant Event (LSE) for Each Tag

# Get time of last multi-tower event for each tag
Last_Centroid_Time <- Triangulation_Events %>%
  group_by(event_id) %>%
  summarise(
    last_centroid_ts = max(ts),
    .groups = "drop"
  )

# Join the last centroid location back to the tag list
Departure_Inferences <- Last_Centroid_Time %>%
  left_join(Triangulation_Events, by = c("event_id", "last_centroid_ts" = "ts")) %>%
  select(event_id, last_centroid_ts, weighted_lat, weighted_lon, w_towers, n_pings) %>%
  rename(Last_Centroid_Lat = weighted_lat, Last_Centroid_Lon = weighted_lon)

# -------------------------------------------------------------------------
# Step 2: Analyze Final Pings for Directionality (The Departure Signature)
# -------------------------------------------------------------------------

# Find time of the absolute last ping (even if single) for context
Last_Ping_Time <- full_data %>% 
  group_by(motusTagID) %>% 
  summarise(
    last_ping_ts = max(ts),
    .groups = "drop"
  )

Departure_Inferences <- Departure_Inferences %>%
  left_join(Last_Ping_Time, by = "motusTagID") ##PROBLEM -- I can't figure this out. The left_join isn't working as intended.  

# Define a window for final directional pings after the Last Centroid Event
DEPARTURE_WINDOW_MINUTES <- 30

# Identify the absolute final ping that occurred after the LSE
Final_Pings_Info <- full_data %>%
  left_join(Departure_Inferences, by = "motusTagID") %>%
  # Filter only to the window after the LSE
  filter(ts > last_centroid_ts &
         ts <= (last_centroid_ts + minutes(DEPARTURE_WINDOW_MINUTES))) %>%
  
  # For each tag, identify the very final detection in that window
  group_by(motusTagID) %>%
  filter(ts == max(ts)) %>%
  slice(1) %>% # Handle ties
  select(motusTagID, Final_Ping_TS = ts, Final_RecvName = recvDeployName, Final_Port = port, Final_Sig = sig, Final_AntBearing = antBearing_adj) %>%
  ungroup()

# Add the Final Ping information
Departure_Inferences <- Departure_Inferences %>%
  left_join(Final_Pings_Info, by = "motusTagID")

# -------------------------------------------------------------------------
# Step 3: Infer Direction based on Final Antenna Bearing (Leveraging Yagi Data)
# -------------------------------------------------------------------------

# MOCK LOOKUP TABLE - ***REPLACE THIS WITH YOUR ACTUAL ANTENNA DATA***
# This step is crucial for linking the antenna bearing to an inferred direction.
# You need a table that defines the relationship between the final antenna's bearing 
# (Final_AntBearing) and the corresponding wind/movement direction.

# Example: If Final_AntBearing is 90 degrees (East), the inferred departure direction is East.
Departure_Inferences <- Departure_Inferences %>%
  mutate(
    Inferred_Direction_Deg = Final_AntBearing, # Assuming the final antenna's bearing is the best inference
    # Classify bearing into cardinal directions for behavioral ecology:
    Inferred_Direction_Cardinal = case_when(
      between(Inferred_Direction_Deg, 337.5, 22.5) ~ "North",
      between(Inferred_Direction_Deg, 22.5, 67.5) ~ "Northeast",
      between(Inferred_Direction_Deg, 67.5, 112.5) ~ "East",
      between(Inferred_Direction_Deg, 112.5, 157.5) ~ "Southeast",
      between(Inferred_Direction_Deg, 157.5, 202.5) ~ "South",
      between(Inferred_Direction_Deg, 202.5, 247.5) ~ "Southwest",
      between(Inferred_Direction_Deg, 247.5, 292.5) ~ "West",
      between(Inferred_Direction_Deg, 292.5, 337.5) ~ "Northwest",
      TRUE ~ "Unknown/Omni"
    )
  )

# Final clean table for Departure
Departure_Summary <- Departure_Inferences %>%
  select(
    motusTagID,
    Last_Centroid_TS,
    Last_Centroid_Lat,
    Last_Centroid_Lon,
    Final_Ping_TS,
    Final_RecvName,
    Final_Antenna_Bearing = Inferred_Direction_Deg,
    Inferred_Direction_Cardinal
  )

print("--- Goal 4: Departure Summary (Last Known Position & Direction) ---")
print(head(Departure_Summary))


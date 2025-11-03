# Load necessary libraries
# Note: Install them first if needed: install.packages(c("tidyverse", "motus", "DBI", "RSQLite", "lubridate", "geosphere", "suncalc", "sf", "data.table"))
library(tidyverse)
library(motus)
library(DBI)
library(RSQLite)
library(lubridate)
library(geosphere) # For bearing calculations
library(suncalc)   # For twilight calculations
library(sf)        # For modern spatial data handling (replaces sp/rgdal)
library(data.table) # For efficient data manipulation if needed (though dplyr handles most here)

# Set global environment variables
Sys.setenv(TZ = "UTC")
# setwd("C:/Users/19189/OneDrive - University of Oklahoma/Winterbirds/AutomatedTelemetry/TagDetections") # Adjust as necessary

# DARN Project ID
proj.num.OU <- 129 # Motus project "University of Oklahoma" (#129)

#### 1. Data Download, Filtering, and Antenna Edits (Revised) ####

# Download/load OU project data
# Use 'new = TRUE' only the first time you download
OU.motus <- tagme(
  projRecv = proj.num.OU,
  new = FALSE,
  forceMeta = TRUE,
  update = TRUE, # Set to FALSE if you are only loading from a local file
  dir = "./tag_data/"
)

# Define DARN spatial boundaries
DARN_BOUNDS <- list(
  lat_min = 36.2, lat_max = 36.6,
  lon_min = -103.1, lon_max = -102.1
)

# Extract and immediately filter to DARN tags/detections
# This is much more efficient than collecting all data first.
DARN_detections_raw <- tbl(OU.motus, "alltags") %>%
  # --- Mandatory Filters (from original script) ---
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

# Apply Antenna Bearing Edits (Consolidating the edits from lines 415-424)
DARN_detections <- DARN_detections_raw %>%
  mutate(
    # This is your antenna bearing correction list (a single operation)
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

# Remove the large raw file to free memory
rm(DARN_detections_raw)


#### 2. Multi-Tower Detections & Twilight Analysis (Goal 1) ####

# Window for near-simultaneous detections (e.g., within 2.5 seconds)
WINDOW_SIZE_SEC <- 2.5

# Step 1: Group detections into single 'events' (replacing the complex fill/mutate chain)
DARN_events <- DARN_detections %>%
  # Sort by tag and time
  arrange(motusTagID, ts, recvDeployName, port) %>%
  group_by(motusTagID) %>%
  # Calculate time difference to the *previous* detection of the same tag
  mutate(ts_diff = as.numeric(ts) - lag(as.numeric(ts))) %>%
  # Flag the start of a new, independent event (diff > window or first detection)
  mutate(is_new_event = ifelse(is.na(ts_diff) | ts_diff > WINDOW_SIZE_SEC, 1, 0)) %>%
  # Create a continuous event ID for all overlapping pings
  mutate(event_id = cumsum(is_new_event)) %>%
  ungroup() %>%
  # Group by the new event ID to calculate event summaries
  group_by(event_id) %>%
  mutate(
    w_towers = n_distinct(recvDeployName), # Goal: Count of towers in event
    w_antennas = n_distinct(recvDeployName, port) # Count of unique antennas
  ) %>%
  ungroup() %>%
  select(-ts_diff, -is_new_event) # Clean up temporary columns

# Step 2: Twilight Analysis (Replacing source("timeToSunriset_edit.R"))
# Use suncalc to calculate local twilight for the DARN centroid or each tower.
# We'll use the DARN centroid for simplicity/efficiency in a large join.

# Calculate DARN centroid (for rough twilight approximation)
darn_lat <- mean(DARN_detections$recvDeployLat)
darn_lon <- mean(DARN_detections$recvDeployLon)

# Get twilight times for all dates in the dataset
twilight_data <- DARN_events %>%
  mutate(date = as_date(ts)) %>%
  distinct(date) %>%
  rowwise() %>%
  mutate(
    # Calculate twilight for the DARN centroid
    twilight_times = list(
      getSunlightTimes(
        date = date,
        lat = darn_lat,
        lon = darn_lon,
        keep = c("nauticalDawn", "sunrise", "sunset", "nauticalDusk"),
        tz = "UTC" # Ensure times are in UTC to match detection 'ts'
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

# --- Goal 1 Output: Evaluate multi-tower detections relative to twilight ---
# Focus on unique events (not every ping)
DARN_twilight_summary <- DARN_events_twilight %>%
  group_by(event_id) %>%
  slice(1) %>% # Select one row per unique event
  ungroup() %>%
  group_by(time_of_day, w_towers) %>%
  summarise(
    event_count = n(),
    .groups = "drop"
  )

print("--- Goal 1 Summary (Events by Twilight and Tower Count) ---")
print(DARN_twilight_summary)


#### 3. Departure Movement & Weather Analysis (Goal 2) ####

# Step 1: Identify Last DARN Detection for each tag
Last_DARN_Detections <- DARN_detections %>%
  group_by(motusTagID) %>%
  filter(ts == max(ts)) %>% # Find the single last detection for each tag
  # Handle ties (in case of simultaneous detections at max(ts)) by picking one
  slice(1) %>%
  ungroup()

# Step 2: Link to Distant Detections (to find departure bearing)
# The user's code for 'Out of area detections' (DARN.distDets) is needed here.
DARN_distDets <- tbl(OU.motus, "alltags") %>%
  # Filter to tags deployed within DARN (based on deployment metadata)
  filter(between(tagDepLat, DARN_BOUNDS$lat_min, DARN_BOUNDS$lat_max)) %>%
  filter(between(tagDepLon, DARN_BOUNDS$lon_min, DARN_BOUNDS$lon_max)) %>%
  # Exclude detections *within* DARN's operational receiver area
  filter(!between(recvDeployLat, DARN_BOUNDS$lat_min, DARN_BOUNDS$lat_max) |
           !between(recvDeployLon, DARN_BOUNDS$lon_min, DARN_BOUNDS$lon_max)) %>%
  filter(tagDeployTest == 0) %>%
  # Select key movement data and collect
  select(motusTagID, ts, recvDeployLat, recvDeployLon, recvDeployName, tagDepLat, tagDepLon) %>%
  collect() %>%
  mutate(ts = as_datetime(ts, tz = "UTC", origin = "1970-01-01"))

# Find the *first* distant detection after leaving DARN
First_Distant_Detections <- Last_DARN_Detections %>%
  # Join last DARN detection with all distant detections for that tag
  left_join(DARN_distDets, by = "motusTagID", suffix = c(".last_darn", ".first_distant")) %>%
  # Keep only distant detections *after* the last DARN detection
  filter(ts.first_distant > ts.last_darn) %>%
  # Find the earliest of those distant detections
  group_by(motusTagID) %>%
  filter(ts.first_distant == min(ts.first_distant)) %>%
  slice(1) %>% # Handle ties
  ungroup()

# Step 3: Calculate Departure Bearing
Departure_Analysis <- First_Distant_Detections %>%
  rowwise() %>%
  mutate(
    # Calculate great circle bearing from last DARN tower to first distant tower
    departure_bearing = bearing(
      p1 = c(recvDeployLon.last_darn, recvDeployLat.last_darn),
      p2 = c(recvDeployLon.first_distant, recvDeployLat.first_distant)
    )
  ) %>%
  ungroup() %>%
  select(
    motusTagID, ts.last_darn, recvDeployName.last_darn,
    ts.first_distant, recvDeployName.first_distant,
    departure_bearing
  )

# Step 4: Weather Integration (Conceptual)
# *Note: Mesonet data is external and not provided. This is the integration step.*
# You would need to load your Mesonet data (e.g., from a CSV) and join it.

# Conceptual Mesonet Data Frame (You replace this with your loaded data)
# Mesonet_data <- read_csv("your_mesonet_data.csv") %>%
#   mutate(ts = as_datetime(ts, tz = "UTC")) # Ensure UTC timestamps

# Departure_Analysis_with_Weather <- Departure_Analysis %>%
#   left_join(Mesonet_data, by = c("ts.last_darn" = "ts")) # Simple time match
# # A more complex spatial/temporal nearest-neighbor match (using Mesonet station lat/lon)
# # may be required here for best results.


print("--- Goal 2 Output: Example Departure Bearing ---")
print(head(Departure_Analysis, 5))


# --- 4. Triangulation and Landcover Linkage (Goal 3 - Spatial Prep) ---

# Step 2: Select Multi-Tower Events for Centroid Calculation
# We reuse the DARN_events data frame created in Goal 1
Triangulation_Events <- DARN_events %>%
  filter(w_towers >= 2) %>%
  
  # --- Centroid Calculation Logic ---
  group_by(event_id) %>%
  
  # 1. Transform Signal Strength to a Weight
  # Max(sig) is the least negative/strongest signal.
  # We shift and scale the sig values so that the strongest signal in the event
  # has a high positive value (near zero or above) and the weakest has a low one.
  mutate(
    # Get the max signal in the event (strongest signal = closer tower)
    max_sig = max(sig, na.rm = TRUE),
    # Scale signal relative to the max signal in the event.
    # We use '10^(sig / 10)' which relates dBm back to power magnitude (mW),
    # then scale by the max power to get a weight from 0 to 1.
    # Alternatively, a simpler linear shift can be used:
    # weight = (sig - min(sig, na.rm=T)) + 1 # Simple linear weight
    
    # Use the max-scaled linear weight for robustness:
    weight = max_sig - sig + 1 # Strength difference: Stronger signal (sig closer to max_sig) gets a higher weight.
  ) %>%
  
  # 2. Calculate Simple Centroid (for comparison)
  summarise(
    simple_lat = mean(recvDeployLat),
    simple_lon = mean(recvDeployLon),
    
    # 3. Calculate WEIGHTED Centroid
    # Use the standard weighted mean function for Latitude and Longitude
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

# --- Comparison of Centroids ---
# To see the difference between the simple and weighted locations
comparison_output <- Triangulation_Events %>%
  select(event_id, simple_lat, weighted_lat, simple_lon, weighted_lon) %>%
  mutate(
    # Calculate the distance in meters between the two centroid types
    distance_m = distHaversine(
      p1 = cbind(simple_lon, simple_lat),
      p2 = cbind(weighted_lon, weighted_lat)
    )
  )

print("--- Weighted Centroid Output (Comparison) ---")
print(head(comparison_output, 5))


# Step 4: Landcover Linkage (Conceptual - using the Weighted Centroid)
# This step remains the same, but uses the improved sf object

# Load Landcover/Playa data (Assuming you have shapefiles)
# Federal_Grasslands_sf <- st_read("Federal_Grasslands.shp") %>% st_transform(32613)
# Playa_Lakes_sf <- st_read("Playa_Lakes.shp") %>% st_transform(32613)

# Spatial Join (Intersection)
# Centroid_Landcover_Joined <- Weighted_Centroid_sf %>%
#   st_join(Federal_Grasslands_sf, join = st_intersects) %>%
#   st_join(Playa_Lakes_sf, join = st_intersects)

print("--- Goal 3 Output: Spatial Data Objects Created ---")
print("DARN_ants_sf (Antenna locations in UTM):")
print(DARN_ants_sf)
print("Centroid_sf (Approximate Bird Locations):")
print(Centroid_sf)
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
setwd("C:/Users/19189/OneDrive - University of Oklahoma/Winterbirds/AutomatedTelemetry/TagDetections")

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
  update = FALSE, # Set to FALSE if you are only loading from a local file
  dir = "C:/Users/19189/OneDrive - University of Oklahoma/Winterbirds/AutomatedTelemetry/TagDetections/tag_data/"  #Alter as needed for individual computers 
)

# Define DARN spatial boundaries
DARN_BOUNDS <- list(
  lat_min = 36.2, lat_max = 36.6,
  lon_min = -103.1, lon_max = -102.1
)

# Step 1: Re-extract Tag Metadata from your Motus project object
# We assume 'pro' is your Motus project object (e.g., from tagme)
Tag_Metadata <- OU.motus$alltags %>%
  select(motusTagID, mfgID, model, manufacturer) %>%
  mutate(
    tag_type = case_when(
      grepl("LifeTag", model, ignore.case = TRUE) ~ "LifeTag (Solar)",
      grepl("Hybrid", model, ignore.case = TRUE) ~ "Hybrid (Batt/Solar)",
      TRUE ~ "Other/Unknown"
    )
  )

# Step 2: Push this into your Raw_Analysis_Final
Raw_Analysis_Final <- Raw_Analysis_Final %>%
  left_join(Tag_Metadata, by = "motusTagID")

cat("\n--- Tag Type Distribution Audit ---\n")
print(table(Raw_Analysis_Final$tag_type, Raw_Analysis_Final$outcome))

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
    antBearing, antHeight, nodeNum, tagModel
  ) %>%
  collect() %>% # Bring data from SQL table into an R data frame
  # --- Final cleanup & preparation ---
  mutate(
    ts = as_datetime(ts, tz = "UTC", origin = "1970-01-01") # Ensure correct POSIXct format
  ) #%>%
  #filter(ts > "2023-10-01") # messing with this


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
    ),
    # FIX: Wrap the adjusted bearing around the 360-degree circle 
    # to eliminate negative numbers or numbers > 359
    antBearing_adj = antBearing_adj %% 360,
    
    # NEW: Identify Local Deployments vs Passage Migrants early for all downstream analyses
    Local_Dep = between(tagDepLat, DARN_BOUNDS$lat_min, DARN_BOUNDS$lat_max) & 
      between(tagDepLon, DARN_BOUNDS$lon_min, DARN_BOUNDS$lon_max)
  )

# -------------------------------------------------------------------------
# 2. Multi-Tower Detections & Twilight Analysis (Goal 1)
# -------------------------------------------------------------------------

WINDOW_SIZE_SEC <- 2.5 # Window for near-simultaneous detections
VISIT_SIZE_DAYS <- 5 #SET THIS AS THE NUMBER OF DAYS BETWEEN DETECTIONS TO CONSIDER VISITS INDEPENDENT
VISIT_SIZE_SECS <- VISIT_SIZE_DAYS*24*60*60
  #432000 #this is the number of seconds in 5 days

# Step 1: Group detections into single 'events'
DARN_events <- DARN_detections %>%
  arrange(motusTagID, ts, recvDeployName, port) %>%
  group_by(motusTagID) %>%
  mutate(ts_diff = as.numeric(ts) - lag(as.numeric(ts))) %>%
  mutate(is_new_event = ifelse(is.na(ts_diff) | ts_diff > WINDOW_SIZE_SEC, 1, 0)) %>%
  # days between visits -- are they over 5 days, if yes, then it's a new visit.
  mutate(visit_gap_bigger_5 = ifelse(is.na(ts_diff) | ts_diff > VISIT_SIZE_SECS, 1, 0)) %>% 
  
  # NEW: Create sequential visit numbers (1, 2, 3...) per bird
  mutate(visit_seq = cumsum(visit_gap_bigger_5)) %>% 
  mutate(bird_event_seq = cumsum(is_new_event)) %>%
  ungroup() %>%
  
  mutate(event_id = paste(motusTagID, bird_event_seq, sep = "_")) %>%
  mutate(visit_id = paste(motusTagID, visit_seq, sep = "_")) %>% 
  
  group_by(event_id) %>%
  mutate(
    w_towers = n_distinct(recvDeployName), 
    w_antennas = n_distinct(recvDeployName, port),
    # Ensure variables pass through
    bird_event_seq = first(bird_event_seq),
    motusTagID = first(motusTagID),
    visit_id = first(visit_id),
    visit_seq = first(visit_seq)
  ) %>%
  ungroup() %>%
  select(-ts_diff, -is_new_event, -visit_gap_bigger_5)

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
  filter(Local_Dep == TRUE) %>% # FIX: Restrict analysis to locally-tagged wintering birds
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
Triangulation_Events <- DARN_events %>% #here is the start of the problem where motus tag ID is lost
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
    motusTagID = first(motusTagID), # Fix: Keep the Tag ID
    visit_id = first(visit_id),     # Fix: Keep the Visit ID
    
    # Calculate Simple Centroid
    simple_lat = mean(recvDeployLat),
    simple_lon = mean(recvDeployLon),
    
    # Calculate WEIGHTED Centroid
    weighted_lat = weighted.mean(recvDeployLat, w = weight),
    weighted_lon = weighted.mean(recvDeployLon, w = weight),
    
    ts = first(ts), 
    w_towers = first(w_towers),
    n_pings = n(),
    .groups = "drop"
  )

# Step 3: Convert Weighted Centroids to SF Points
Weighted_Centroid_sf <- Triangulation_Events %>%
  st_as_sf(coords = c("weighted_lon", "weighted_lat"), crs = 4326) %>%
  st_transform(crs = 32613) # UTM Zone 13N (for accurate spatial joins)


ggplot(Weighted_Centroid_sf) + 
  geom_sf(data = Weighted_Centroid_sf, color = "black") ##Plot of the weighted centroid of the triangulation events

#library(png)
#img <- readPNG("DARN.png")
#library(ggplot2)
#ggplot(Weighted_Centroid_sf) +
  #(annotation_raster(img, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)) +
  #geom_sf(data = Weighted_Centroid_sf, color = "darkolivegreen2")


print("--- Goal 3: Weighted Centroid Dataframe Head ---")
print(head(Triangulation_Events, 5))

# -------------------------------------------------------------------------
# 4. Extracting Arrivals and Departures for 'movetrack' Analysis
# -------------------------------------------------------------------------

# Step 1: Extract DEPARTURES (All uncollapsed detection rows within 4 hours of the final ping)
Departures_Data <- DARN_events %>%
  group_by(visit_id) %>%
  # Identify the absolute final timestamp of the visit
  mutate(final_ts = max(ts)) %>%
  # 4 Hours = 14,400 seconds. Filter events occurring within 4h of final_ts
  filter(as.numeric(final_ts) - as.numeric(ts) <= 14400) %>%
  arrange(visit_id, ts) %>%
  ungroup() %>%
  mutate(movement_type = "Departure")

# Step 2: Extract ARRIVALS (All uncollapsed detection rows within 4 hours of the first returning ping)
Arrivals_Data <- DARN_events %>%
  # CRITICAL: Exclude the very first visit (capture/release)
  filter(visit_seq > 1) %>%
  group_by(visit_id) %>%
  # Identify the absolute first timestamp of the visit
  mutate(first_ts = min(ts)) %>%
  # Filter events occurring within 4h of first_ts
  filter(as.numeric(ts) - as.numeric(first_ts) <= 14400) %>%
  arrange(visit_id, ts) %>%
  ungroup() %>%
  mutate(movement_type = "Arrival")

# Step 3: Combine into a clean dataframe ready for 'movetrack'
Movetrack_Prep <- bind_rows(Departures_Data, Arrivals_Data) %>%
  arrange(motusTagID, ts) %>%
  # Select the crucial columns, keeping tower and antenna data fully intact per row
  select(
    motusTagID, visit_id, visit_seq, movement_type, event_id, bird_event_seq, ts, 
    recvDeployName, recvDeployLat, recvDeployLon, antBearing_adj, sig, port, w_towers,
    Local_Dep # NEW: Keep the deployment flag so it passes to the plotting sections
  )

print("--- Data Extracted for movetrack (First 15 Rows) ---")
print(head(Movetrack_Prep, 15))

# Optional: Write out to CSV to move to your movetrack project
# write_csv(Movetrack_Prep, "DARN_movetrack_inputs.csv")

# -------------------------------------------------------------------------
# 5. 'movetrack' Installation, Mapping, and Rose Plots
# -------------------------------------------------------------------------

# Step 1: Install movetrack from the r-universe repository (if needed)
if (!requireNamespace("movetrack", quietly = TRUE)) {
  install.packages("movetrack", repos = c("https://g-rppl.r-universe.dev", "https://cloud.r-project.org"))
}

# -------------------------------------------------------------------------
# 2. Movetrack Prep (TRUNCATED: Local Residents Only)
# -------------------------------------------------------------------------

Movetrack_Prep <- Movetrack_Prep %>%
  # PRIMARY FILTER: Truncate to Local Wintering Birds only
  filter(Local_Dep == TRUE) %>%
  left_join(Tag_Type_Key, by = "motusTagID") %>% # Join hardware metadata early
  group_by(visit_id) %>%
  mutate(
    visit_base_ts = min(as.numeric(ts)), 
    month_val = month(ts),
    season = case_when(
      month_val %in% c(11, 12) ~ "Early Winter",
      month_val %in% c(1, 2) ~ "Mid-winter",
      month_val %in% c(3, 4, 5) ~ "Late Winter",
      TRUE ~ "Other"
    ),
    season = factor(season, levels = c("Early Winter", "Mid-winter", "Late Winter", "Other"))
  ) %>%
  ungroup()

# Step 3: Map the Tracks (Separated by Cohort)
# Note: This plots the raw empirical tower detections. Once you run movetrack's HMM, 
# you can swap 'recvDeployLat'/'recvDeployLon' for the modeled coordinates.

# 3A: Map for Local Wintering Birds
track_map_local <- ggplot(Movetrack_Prep %>% filter(cohort == "Local Wintering (DARN Tagged)"), 
                          aes(x = recvDeployLon, y = recvDeployLat)) +
  geom_path(aes(group = visit_id, color = as.numeric(ts)), 
            arrow = arrow(type = "closed", length = unit(0.1, "inches")),
            linewidth = 0.8, alpha = 0.7) +
  geom_point(aes(color = as.numeric(ts)), size = 2) +
  scale_color_viridis_c(
    option = "plasma", name = "Date (Gradient)",
    breaks = c(min(as.numeric(Movetrack_Prep$ts)), max(as.numeric(Movetrack_Prep$ts))),
    labels = c("Early (Dec)", "Late (Apr)")
  ) +
  facet_wrap(~ movement_type) + # Split into Arrivals vs Departures
  theme_minimal() +
  labs(
    title = "DARN Array: Local Wintering Tracks",
    subtitle = "Color gradient represents time of season",
    x = "Longitude", y = "Latitude"
  )

print(track_map_local)

# 3B: Map for Passage Migrants
track_map_passage <- ggplot(Movetrack_Prep %>% filter(cohort == "Passage Migrant (Tagged Outside)"), 
                            aes(x = recvDeployLon, y = recvDeployLat)) +
  geom_path(aes(group = visit_id, color = as.numeric(ts)), 
            arrow = arrow(type = "closed", length = unit(0.1, "inches")),
            linewidth = 0.8, alpha = 0.7) +
  geom_point(aes(color = as.numeric(ts)), size = 2) +
  scale_color_viridis_c(
    option = "plasma", name = "Date (Gradient)",
    breaks = c(min(as.numeric(Movetrack_Prep$ts)), max(as.numeric(Movetrack_Prep$ts))),
    labels = c("Early (Dec)", "Late (Apr)")
  ) +
  facet_wrap(~ movement_type) + # Split into Arrivals vs Departures
  theme_minimal() +
  labs(
    title = "DARN Array: Passage Migrant Tracks",
    subtitle = "Color gradient represents time of season",
    x = "Longitude", y = "Latitude"
  )

print(track_map_passage)

# Step 4: Rose Plots for Final Bearings
# EXTRACTING TERMINAL EVENTS: For departures, we want the last known bearing. For arrivals, the first.
Final_Bearings <- Movetrack_Prep %>%
  group_by(visit_id, movement_type) %>%
  filter(
    (movement_type == "Departure" & ts == max(ts)) | 
      (movement_type == "Arrival" & ts == min(ts))
  ) %>%
  slice(1) %>% # Handle exact time ties
  ungroup()

# ROSE PLOT GENERATION (Separated by Cohort)

# 4A: Rose Plot for Local Wintering Birds
rose_plot_local <- ggplot(Final_Bearings %>% filter(cohort == "Local Wintering (DARN Tagged)"), 
                          aes(x = (antBearing_adj + 22.5) %% 360, fill = season)) +
  geom_histogram(breaks = seq(0, 360, by = 45), color = "black", alpha = 0.8) +
  coord_polar(start = -22.5 * pi / 180, direction = 1) + 
  scale_x_continuous(
    limits = c(0, 360), breaks = seq(22.5, 337.5, by = 45),
    labels = c("N", "NE", "E", "SE", "S", "SW", "W", "NW")
  ) +
  scale_fill_manual(values = c("Early Winter" = "steelblue", "Mid-winter" = "mediumpurple", "Late Winter" = "darkorange")) +
  theme_minimal() +
  facet_wrap(~movement_type) + 
  labs(
    title = "Rose Plot: Local Wintering Terminal Bearings",
    subtitle = "Separated by Arrivals/Departures and Time of Season",
    x = NULL, y = "Count of Events", fill = "Season"
  ) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

print(rose_plot_local)

# 4B: Rose Plot for Passage Migrants
rose_plot_passage <- ggplot(Final_Bearings %>% filter(cohort == "Passage Migrant (Tagged Outside)"), 
                            aes(x = (antBearing_adj + 22.5) %% 360, fill = season)) +
  geom_histogram(breaks = seq(0, 360, by = 45), color = "black", alpha = 0.8) +
  coord_polar(start = -22.5 * pi / 180, direction = 1) + 
  scale_x_continuous(
    limits = c(0, 360), breaks = seq(22.5, 337.5, by = 45),
    labels = c("N", "NE", "E", "SE", "S", "SW", "W", "NW")
  ) +
  scale_fill_manual(values = c("Early Winter" = "steelblue", "Mid-winter" = "mediumpurple", "Late Winter" = "darkorange")) +
  theme_minimal() +
  facet_wrap(~movement_type) + 
  labs(
    title = "Rose Plot: Passage Migrant Terminal Bearings",
    subtitle = "Separated by Arrivals/Departures and Time of Season",
    x = NULL, y = "Count of Events", fill = "Season"
  ) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

print(rose_plot_passage)

# -------------------------------------------------------------------------
# 6a. Prepping for the movetrack Hidden Markov Model (HMM)
# -------------------------------------------------------------------------

# Step 0: Set up the Stan Backend (Required for movetrack)
if (!requireNamespace("cmdstanr", quietly = TRUE)) {
  install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
}
library(cmdstanr)

# Check for CmdStan path safely
cmdstan_installed <- FALSE
try({
  if (cmdstan_path() != "") cmdstan_installed <- TRUE
}, silent = TRUE)

if (!cmdstan_installed) {
  print("CmdStan not found. Installing CmdStan backend. This may take a few minutes...")
  # This requires Rtools (Windows) or Xcode (Mac) to be installed on your system
  install_cmdstan(check_toolchain = TRUE)
} else {
  print(paste("CmdStan found at:", cmdstan_path()))
}

# -------------------------------------------------------------------------
# 6a. Empirical Range Calculation & HMM Prepping
# -------------------------------------------------------------------------

library(geosphere)
# Calculate distances between towers for simultaneous pings to find objective range
Simultaneous_Pings <- DARN_detections %>%
  group_by(motusTagID, ts) %>%
  filter(n_distinct(recvDeployName) > 1) %>%
  summarise(
    tower_pairs = list(combn(unique(recvDeployName), 2, simplify = FALSE)),
    .groups = "keep"
  ) %>%
  unnest(tower_pairs) %>%
  rowwise() %>%
  mutate(
    t1 = tower_pairs[1], t2 = tower_pairs[2],
    lat1 = DARN_detections$recvDeployLat[match(t1, DARN_detections$recvDeployName)],
    lon1 = DARN_detections$recvDeployLon[match(t1, DARN_detections$recvDeployName)],
    lat2 = DARN_detections$recvDeployLat[match(t2, DARN_detections$recvDeployName)],
    lon2 = DARN_detections$recvDeployLon[match(t2, DARN_detections$recvDeployName)],
    dist_km = distVincentySphere(c(lon1, lat1), c(lon2, lat2)) / 1000
  )

# DYNAMIC PARAMETERS: Set these once here; they will propagate to all plots/models
target_quantile <- 0.90
range_val       <- quantile(Simultaneous_Pings$dist_km, target_quantile, na.rm = TRUE)
hmm_dTime       <- 5  # time window for model building (minutes)
hmm_states      <- 1  # 1 = transient movements only; 2 = transient and local states

# Visualizing the objective range distribution
dist_plot <- ggplot(Simultaneous_Pings, aes(x = dist_km)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "white") +
  geom_vline(xintercept = range_val, color = "red", linetype = "dashed") +
  labs(title = paste0("Simultaneous Detection Distances (", target_quantile*100, "% Threshold)"),
       subtitle = paste0("Empirical Range Cutoff: ", round(range_val, 2), " km"),
       x = "Distance between Towers (km)", y = "Count of Pairs") +
  theme_minimal()
print(dist_plot)

# -------------------------------------------------------------------------
# 6b. Running & Mapping the 'movetrack' HMM
# -------------------------------------------------------------------------
library(movetrack)

# -------------------------------------------------------------------------
# 6. HMM Modeling (Restricted to Residents)
# -------------------------------------------------------------------------

# Filter for tracks long enough for Stan (N >= 5)
Movetrack_Model_Input <- Movetrack_Prep %>%
  group_by(visit_id, movement_type) %>%
  filter(n() >= 5) %>% 
  ungroup() %>%
  mutate(
    track_id = paste(visit_id, movement_type, sep = "_"),
    aType = "5-element Yagi",
    ID = track_id, tagDeployID = track_id,
    lat = recvDeployLat, lon = recvDeployLon, antBearing = antBearing_adj
  )

# Run HMM using the empirical range_val calculated previously
movetrack_fit90 <- track(
  Movetrack_Model_Input,
  ID = "ID", ts = "ts", sig = "sig", aType = "aType",
  dTime = hmm_dTime, states = hmm_states, aRange = range_val, seed = 42
)

# Step 3: Extract & Post-Process (Streamlined with Dynamic Labels)
Modeled_Path_90 <- as.data.frame(movetrack_fit90) %>%
  mutate(
    movement_type = ifelse(grepl("Arrival", ID), "Arrival", "Departure"),
    # STREAMLINED: Extract visit_id by removing the '_Arrival' or '_Departure' suffix
    visit_id = sub("_[^_]+$", "", ID)
  ) %>%
  filter(lon < -90 & lat > 30) %>% 
  left_join(Movetrack_Model_Input %>% distinct(ID, season), by = "ID")

# Step 4: Map with Dynamic Titles
modeled_map_90 <- ggplot() +
  geom_path(data = Modeled_Path_90, aes(x = lon, y = lat, group = ID, color = movement_type), alpha = 0.5) +
  geom_point(data = Movetrack_Model_Input, aes(x = lon, y = lat), color = "black", size = 1, alpha = 0.3) +
  facet_wrap(~movement_type) + 
  coord_quickmap() + theme_minimal() +
  labs(
    title = paste0("HMM Modeled Pathways (", hmm_states, "-State Transit)"),
    subtitle = paste0("Interval: ", hmm_dTime, " min | Range Constraint: ", round(target_quantile, 2), " km"),
    x = "Longitude", y = "Latitude"
  )
print(modeled_map_90)


# Step 5: Calculate Whole-Track Displacement & Performance
Modeled_Vectors_90 <- Modeled_Path_90 %>%
  group_by(ID) %>%
  arrange(time) %>%
  # We still use the bookends (first and last pings of the 4-hour window)
  filter(row_number() == 1 | row_number() == n()) %>%
  mutate(
    # 1. Spatial displacement
    next_lon = lead(lon),
    next_lat = lead(lat),
    flight_dist_km = distVincentySphere(cbind(lon, lat), cbind(next_lon, next_lat)) / 1000,
    
    # 2. Temporal displacement
    next_time = lead(time),
    duration_hrs = as.numeric(difftime(next_time, time, units = "hours")),
    
    # 3. Flight Performance
    ground_speed_kmh = flight_dist_km / duration_hrs,
    flight_bearing = (bearing(cbind(lon, lat), cbind(next_lon, next_lat)) + 360) %% 360
  ) %>%
  # Keep only the first row which now contains the 'next' (end) values
  drop_na(flight_bearing) %>% 
  slice(1) %>% 
  ungroup() %>%
  # Clean up helper columns
  select(-next_lon, -next_lat, -next_time)

# Quick Sanity Check: What are our average speeds?
print("--- Modeled Flight Performance Summary ---")
Modeled_Vectors_90 %>% 
  group_by(movement_type) %>% 
  summarise(avg_speed_kmh = mean(ground_speed_kmh, na.rm=TRUE), 
            avg_dist_km = mean(flight_dist_km, na.rm=TRUE)) %>%
  print()

# Step 6: Rose Plot
modeled_rose_final <- ggplot(Modeled_Vectors, aes(x = (flight_bearing + 22.5) %% 360, fill = season)) +
  geom_histogram(breaks = seq(0, 360, by = 45), color = "black", alpha = 0.8) +
  coord_polar(start = -22.5 * pi / 180, direction = 1) + 
  scale_x_continuous(limits = c(0, 360), breaks = seq(22.5, 337.5, by = 45),
                     labels = c("N", "NE", "E", "SE", "S", "SW", "W", "NW")) +
  scale_fill_viridis_d(option = "plasma") + theme_minimal() + facet_wrap(~movement_type) + 
  labs(
    title = paste0("Rose Plot: ", hmm_states, "-State Transit Vectors"),
    subtitle = paste0("Based on Whole-Track Displacement | Range: ", round(range_val, 2), " km"),
    x = NULL, y = "Count of Events"
  )
print(modeled_rose_final)

# -------------------------------------------------------------------------
# 7. Raw-Data Outcome Classification (No HMM Filtering)
# -------------------------------------------------------------------------

# Step 1: Identify Visit Status from the full record
Visit_Status_Raw <- DARN_events %>%
  group_by(motusTagID) %>%
  mutate(is_terminal_visit = (visit_seq == max(visit_seq))) %>%
  group_by(visit_id) %>%
  summarise(is_terminal = first(is_terminal_visit), .groups = "drop")

# Step 2: Aggregate Raw Events (One row per Arrival/Departure window)
Raw_Event_Analysis <- Movetrack_Prep %>%
  group_by(visit_id, movement_type) %>%
  summarise(
    last_ts = max(ts),
    ping_count = n(),
    .groups = "drop"
  ) %>%
  left_join(Visit_Status_Raw, by = "visit_id") %>%
  mutate(
    # Logic: Arrivals are ALWAYS returns. Departures are split by survival/return.
    outcome = case_when(
      movement_type == "Arrival" ~ "Confirmed Return (Arrival)",
      movement_type == "Departure" & !is_terminal ~ "Temporary Departure (Returnee)",
      movement_type == "Departure" & is_terminal  ~ "Terminal Disappearance"
    ),
    # Local Time Conversion
    hour_local = (hour(last_ts) - 6) %% 24 #rounded to nearest hour -- this could introduce oversimplification
  )

# -------------------------------------------------------------------------
# 8. Biological Timing & Raw Data Outcome Analysis (Consolidated Bins)
# -------------------------------------------------------------------------

# Step 1: Calculate Biological Time Bins (Merging undersampled periods)
Biological_Timing_Raw <- Movetrack_Prep %>%
  group_by(motusTagID, visit_id, movement_type) %>%
  summarise(
    last_ts = max(ts),
    ping_count = n(),
    .groups = "drop"
  ) %>%
  mutate(date = as_date(last_ts)) %>%
  left_join(twilight_data, by = "date") %>%
  mutate(
    # Duration of Nautical Twilight (approx 60-70 mins in OK)
    twilight_dur = as.numeric(difftime(sunrise, nauticalDawn, units = "secs")),
    
    # Categorize into Consolidated Biological Periods
    bio_period = case_when(
      # Pure Night: Before Nautical Dawn or after Nautical Dusk
      last_ts <= nauticalDawn | last_ts >= nauticalDusk ~ "Nocturnal",
      
      # Sunrise Window: Merging Dawn Transition (Pre-Sunrise) with Post-Sunrise Activity
      last_ts > nauticalDawn & last_ts <= (sunrise + twilight_dur) ~ "Sunrise Window",
      
      # Sunset Window: Merging Pre-Sunset Activity with Dusk Transition (Post-Sunset)
      last_ts >= (sunset - twilight_dur) & last_ts < nauticalDusk ~ "Sunset Window",
      
      # Mid-Day Split: Remaining daylight hours
      last_ts > (sunrise + twilight_dur) & last_ts < (sunrise + (sunset-sunrise)/2) ~ "Morning",
      TRUE ~ "Afternoon"
    ),
    bio_period = factor(bio_period, levels = c(
      "Nocturnal", "Sunrise Window", "Morning", "Afternoon", "Sunset Window"
    ))
  )

# Step 2: Join with Outcome
Raw_Analysis_Final <- Biological_Timing_Raw %>%
  left_join(Visit_Status_Raw, by = "visit_id") %>%
  mutate(
    outcome = case_when(
      movement_type == "Arrival" ~ "Confirmed Return",
      movement_type == "Departure" & !is_terminal ~ "Temporary Departure",
      movement_type == "Departure" & is_terminal  ~ "Terminal Disappearance"
    )
  )

# -------------------------------------------------------------------------
# 9. Statistical Matrix & Permutation Analysis
# -------------------------------------------------------------------------

# Step 1: Construct the Observed Contingency Matrix using consolidated periods
obs_matrix <- table(Raw_Analysis_Final$outcome, Raw_Analysis_Final$bio_period)

# Step 2: Filter out any remaining zero-sum columns (Safety check for NaN)
obs_matrix_clean <- obs_matrix[, colSums(obs_matrix) > 0]

# Step 3: Re-run the Permuted Chi-Squared (B=5000 for high precision)
chisq_test_clean <- chisq.test(obs_matrix_clean, simulate.p.value = TRUE, B = 5000)

# Step 4: Extract Pearson Residuals
# These represent how much each 'cell' deviates from what we expect by chance.
residuals_df <- as.data.frame(chisq_test_clean$residuals) %>%
  rename(Outcome = Var1, Period = Var2, Influence = Freq)

# Step 5: Visualize the Matrix of Differences
matrix_diff_plot <- ggplot(residuals_df, aes(x = Period, y = Outcome, fill = Influence)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "firebrick", mid = "white", high = "steelblue", 
                       name = "Residual\nStrength") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Matrix of Biological Contrasts (Consolidated)",
    subtitle = paste0("Blue = Over-represented spikes | Red = Under-represented gaps\nPermuted p-value: ", 
                      round(chisq_test_clean$p.value, 4)),
    x = "Biological Period", y = "Outcome Group"
  )

print(matrix_diff_plot)

# Step 6: Print Cleaned Statistical Weight and Audit (As requested)
cat("\n--- Cleaned Statistical Weight ---\n")
print(paste("Robust Permuted P-Value:", round(chisq_test_clean$p.value, 4)))

cat("\n--- Nocturnal Spike Audit ---\n")
print(obs_matrix[, "Nocturnal", drop = FALSE])

# -------------------------------------------------------------------------
# 10. Consolidated Bird Signatures (Sample Size Optimization)
# -------------------------------------------------------------------------

# Step 1: Consolidate bins for statistical power
Consolidated_Signatures <- Raw_Analysis_Final %>%
  filter(movement_type == "Departure") %>%
  mutate(
    consolidated_period = case_when(
      bio_period == "Nocturnal" ~ "Nocturnal",
      # Folding Dawn Transition and Post-Sunrise into one 2-hour window
      bio_period %in% c("Dawn Transition", "Post-Sunrise Activity") ~ "Sunrise Window",
      # Folding Pre-Sunset and Dusk into one window
      bio_period %in% c("Pre-Sunset Activity", "Dusk Transition") ~ "Sunset Window",
      # Keeping the strong Daylight peaks
      bio_period == "Morning" ~ "Morning",
      bio_period == "Afternoon" ~ "Afternoon",
      TRUE ~ "Other"
    ),
    consolidated_period = factor(consolidated_period, 
                                 levels = c("Nocturnal", "Sunrise Window", "Morning", 
                                            "Afternoon", "Sunset Window"))
  ) %>%
  group_by(motusTagID, outcome, consolidated_period) %>%
  summarise(n_events = n(), .groups = "drop")

# Step 2: Final Audit Table (Cleaned)
cat("\n--- Consolidated Bird Audit (N Birds) ---\n")
Consolidated_Signatures %>%
  group_by(outcome, consolidated_period) %>%
  summarise(n_birds = n(), .groups = "drop") %>%
  pivot_wider(names_from = outcome, values_from = n_birds, values_fill = 0) %>%
  mutate(Total_Unique_Birds = `Temporary Departure` + `Terminal Disappearance`) %>%
  print()

# Step 3: Final Consolidated Plot
ggplot(Consolidated_Signatures, aes(x = consolidated_period, fill = outcome)) +
  geom_bar(position = "fill", color = "black") +
  theme_minimal() +
  scale_fill_manual(values = c("Temporary Departure" = "#2c3e50", 
                               "Terminal Disappearance" = "#f1c40f")) +
  labs(
    title = "Consolidated Unique Bird Departure Signatures",
    subtitle = "Relative proportions with Dawn/Sunrise folded for robustness",
    x = "Biological Period", y = "Proportion of Birds"
  )

# 1. Create a simple 'Key' from the SQL database
Tag_Type_Key <- tbl(OU.motus, "alltags") %>%
  select(motusTagID, tagModel) %>% # Replace with the name you found
  distinct() %>%
  collect() %>%
  rename(tag_type_raw = 2) # Renames the second column for clarity

# 2. Update your final analysis dataframes directly
Raw_Analysis_Final <- Raw_Analysis_Final %>%
  left_join(Tag_Type_Key, by = "motusTagID")

Consolidated_Signatures <- Consolidated_Signatures %>%
  left_join(Tag_Type_Key, by = "motusTagID")

# 3. Verification: See the Tag Type split across outcomes
table(Raw_Analysis_Final$tag_type_raw, Raw_Analysis_Final$outcome)

# Calculate ping density momentum
Detection_Momentum <- Movetrack_Prep %>%
  group_by(visit_id, movement_type) %>%
  mutate(
    # Time normalized from 0 (the event edge) to 14400 (4 hours away)
    rel_time = if_else(movement_type == "Arrival",
                       as.numeric(difftime(ts, min(ts), units = "secs")),
                       as.numeric(difftime(max(ts), ts, units = "secs")))
  )

# Plotting the functions to show the 'Skulk'
ggplot(Detection_Momentum, aes(x = rel_time, color = movement_type, linetype = tag_type_raw)) +
  geom_density(linewidth = 1) +
  facet_wrap(~movement_type, scales = "free") +
  theme_minimal() +
  labs(
    title = "Detection Momentum: The Skulk vs. The Launch",
    subtitle = "Arrivals: Density since 1st ping | Departures: Density until last ping",
    x = "Seconds from Event Edge", y = "Detection Density"
  )

# -------------------------------------------------------------------------
# 11. Resident Firewall & Join Cleanup (Rule 7 Heartbeat)
# -------------------------------------------------------------------------

# Step A: Create a clean key from the source (avoiding the .x/.y trap)
Resident_Key <- Movetrack_Prep %>%
  distinct(motusTagID, Local_Dep)

# Step B: Re-join and filter immediately
Resident_Analysis <- Raw_Analysis_Final %>%
  # We use select(-any_of(...)) to drop any ghost columns before joining
  select(-any_of(c("Local_Dep", "tag_type_raw.x", "tag_type_raw.y"))) %>%
  left_join(Resident_Key, by = "motusTagID") %>%
  filter(Local_Dep == TRUE)

cat("\n--- HEARTBEAT: RESIDENT FILTER AUDIT ---\n")
cat(glue("Confirmed Local Tags: {length(unique(Resident_Analysis$motusTagID))}\n"))
cat(glue("Total Resident Events: {nrow(Resident_Analysis)}\n\n"))

# If this count is what you expect for your DARN birds, we are safe to proceed.

# Step 2: Plot the 'Loudness' (Frequency)
momentum_plot <- ggplot(Momentum_Data, aes(x = time_bin, y = pings_per_bin, color = movement_type)) +
  stat_smooth(method = "loess", span = 0.4) +
  facet_grid(tag_type_raw ~ movement_type, scales = "free_y") +
  theme_minimal() +
  labs(title = "Ping Frequency Momentum", x = "Minutes from Edge", y = "Pings per 5-min")

# Step 3: Plot the 'Signal Quality' (RSSI Variance)
rssi_plot <- ggplot(Momentum_Data, aes(x = time_bin, y = sd_rssi, color = movement_type)) +
  stat_smooth(method = "loess", span = 0.4) +
  facet_wrap(~movement_type) +
  theme_minimal() +
  labs(title = "RSSI Variance: Skulk (High SD) vs Launch (Low SD)", 
       x = "Minutes from Edge", y = "SD of Signal Strength")

print(momentum_plot)
print(rssi_plot)

#********************************************
#* WEATHER COMPARISONS **********************
#********************************************
  
# --- 12. Collapsing Overlapping Micro-Windows ---

Collapsed_Windows <- Micro_Windows %>%
  arrange(window_start) %>%
  # Identify where a new block must start (gap > 0 days)
  mutate(new_block = window_start > lag(window_end, default = first(window_start))) %>%
  mutate(block_id = cumsum(new_block)) %>%
  group_by(block_id) %>%
  summarise(
    start_dt = min(window_start),
    end_dt = max(window_end),
    .groups = "drop"
  )

cat(glue("\n--- TEMPORAL COLLAPSE COMPLETE ---\n"))
cat(glue("Original Redundant Windows: {nrow(Micro_Windows)}\n"))
cat(glue("Surgical Blocks to Download: {nrow(Collapsed_Windows)}\n\n"))

# --- 13. Averaging KENT and BOIS into Regional Weather ---

cat("\n--- CALCULATING CIMARRON REGIONAL AVERAGE (KENT + BOIS) ---\n")

weather_regional <- weather_clean %>%
  group_by(ts_weather) %>%
  summarise(
    # Average the critical metrics
    tair = mean(tair, na.rm = TRUE),
    srad = mean(srad, na.rm = TRUE),
    wspd = mean(wspd, na.rm = TRUE),
    ts05 = mean(ts05, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(ts_weather) %>%
  mutate(
    # Calculate Regional Inertia based on the averaged temperature
    is_freezing = if_else(tair <= 0, 1, 0),
    cold_inertia_hrs = slider::slide_dbl(is_freezing, sum, .before = 1440) * (5/60)
  )

cat(glue("Regional timeline created with {nrow(weather_regional)} timestamps.\n"))
cat(glue("Max Regional Inertia: {max(weather_regional$cold_inertia_hrs, na.rm=TRUE)} hours.\n\n"))

# -------------------------------------------------------------------------
# 14. Capped Absence Duration vs. Thermal Inertia
# -------------------------------------------------------------------------

cat("\n--- REFINING ABSENCE WINDOWS (MAX 100 DAYS) ---\n")

# 100 days in hours = 2400
max_absence_threshold <- 2400

Inertia_Analysis_Capped <- Resident_Analysis %>%
  group_by(motusTagID) %>%
  arrange(last_ts) %>%
  mutate(
    absence_hrs = as.numeric(difftime(lead(last_ts), last_ts, units = "hours"))
  ) %>%
  # Filter: Must be at least 5 days AND less than 100 days
  filter(movement_type == "Departure", 
         absence_hrs >= 120,
         absence_hrs <= max_absence_threshold) %>% 
  ungroup() %>%
  mutate(time_match = floor_date(last_ts, "5 mins")) %>%
  left_join(weather_regional, by = c("time_match" = "ts_weather"))

# Audit the reduction
n_dropped <- nrow(Inertia_Analysis) - nrow(Inertia_Analysis_Capped)
cat(glue("Dropped {n_dropped} trans-seasonal absences (> 100 days).\n"))
cat(glue("Remaining local behavioral events: {nrow(Inertia_Analysis_Capped)}\n\n"))

# -------------------------------------------------------------------------
# 15. The Refined Sensitivity Plot
# -------------------------------------------------------------------------


ggplot(Inertia_Analysis_Capped, aes(x = cold_inertia_hrs, y = absence_hrs)) +
  geom_point(aes(color = srad), size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", color = "firebrick", linewidth = 1.2) +
  scale_color_viridis_c(option = "plasma", name = "Solar Gain (W/m^2)") +
  theme_minimal() +
  labs(
    title = "Regional Thermal Sensitivity: Local Residents",
    subtitle = "Absences: 5–100 Day Range | Regional Weather Avg",
    x = "Cumulative Freezing Hours (Previous 120h)",
    y = "Absence Duration (Hours)"
  )

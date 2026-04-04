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
library(glue)

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


# Extract and immediately filter to DARN tags/detections (EFFICIENT LOADING)
full_data <- tbl(OU.motus, "alltags") %>%
  # --- Mandatory Filters ---
  filter(tagDeployTest == 0) %>% 
  filter(recvDeployName != "CTT HQ") %>%
  
  # FIX: The "Ghostbuster" Filter (Eliminate isolated radio noise)
  filter(runLen >= 3) %>% 
  
  # --- Filter to DARN RECEIVER area ---
  filter(between(recvDeployLat, DARN_BOUNDS$lat_min, DARN_BOUNDS$lat_max)) %>%
  filter(between(recvDeployLon, DARN_BOUNDS$lon_min, DARN_BOUNDS$lon_max)) %>%
  # --- Select core columns for analysis ---
  select(
    speciesID, speciesEN, tagProjID, ts, sig, port, mfgID, motusTagID, runLen,
    tagDepLat, tagDepLon, tagDeployID, recvDeployLat, recvDeployLon, recvDeployName,
    antBearing, antHeight, nodeNum, tagModel, 
    tagDeployStart # <--- NEW: Pull the deployment timestamp
  ) %>%
  collect() %>% 
  # --- Final cleanup & preparation ---
  mutate(
    # Convert both fields to POSIXct
    ts = as_datetime(ts, tz = "UTC", origin = "1970-01-01"),
    tagDeployStart = as_datetime(tagDeployStart, tz = "UTC", origin = "1970-01-01")
  ) %>%
  # FIX: 6-Month Biological Lifespan Filter
  # Ensure the ping happened AFTER deployment, and NO LATER than 6 months post-deployment
  filter(ts >= tagDeployStart & ts <= (tagDeployStart + months(6)))

cat(glue("\n--- EXTRACTION COMPLETE ---\n"))
cat(glue("Retained {nrow(full_data)} valid detections within 6 months of deployment.\n\n"))


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

# Step 2: Twilight Analysis (Corrected for Local Time Crossover)
darn_lat <- mean(DARN_detections$recvDeployLat)
darn_lon <- mean(DARN_detections$recvDeployLon)

twilight_data <- DARN_events %>%
  # FIX: Convert to local time before extracting the date
  mutate(
    ts_local = with_tz(ts, tzone = "America/Chicago"),
    date_local = as_date(ts_local)
  ) %>%
  distinct(date_local) %>%
  rowwise() %>%
  mutate(
    twilight_times = list(
      getSunlightTimes(
        date = date_local, lat = darn_lat, lon = darn_lon,
        keep = c("nauticalDawn", "sunrise", "sunset", "nauticalDusk"),
        tz = "America/Chicago" # FIX: Request twilight in local time
      ) %>% select(-date) # Prevent unnesting conflict
    )
  ) %>%
  unnest(twilight_times) %>% 
  ungroup()

# Join twilight data and assign time-of-day category
DARN_events_twilight <- DARN_events %>%
  filter(Local_Dep == TRUE) %>% # Restrict analysis to locally-tagged wintering birds
  # FIX: Convert ping to local time and extract local date to match twilight_data
  mutate(
    ts_local = with_tz(ts, tzone = "America/Chicago"),
    date_local = as_date(ts_local)
  ) %>%
  # FIX: Join using the matching local date column
  left_join(twilight_data, by = "date_local") %>%
  mutate(
    # FIX: Compare the local timestamp to the local twilight bounds
    time_of_day = case_when(
      ts_local <= nauticalDawn | ts_local >= nauticalDusk ~ "Night",
      ts_local > nauticalDawn & ts_local < sunrise ~ "Pre-Sunrise Twilight (Nautical/Civil)",
      ts_local > sunset & ts_local < nauticalDusk ~ "Post-Sunset Twilight (Civil/Nautical)",
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
  select(
    motusTagID, visit_id, visit_seq, movement_type, event_id, bird_event_seq, ts, 
    recvDeployName, recvDeployLat, recvDeployLon, antBearing_adj, sig, port, w_towers,
    Local_Dep, tagModel # ADDED HERE
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

Movetrack_Prep <- Movetrack_Prep %>%
  mutate(cohort = if_else(Local_Dep == TRUE, 
                          "Local Wintering (DARN Tagged)", 
                          "Passage Migrant (Tagged Outside)"))

# 3: Map for Local Wintering Birds
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

# 4: Rose Plot for Local Wintering Birds
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

Biological_Timing_Raw <- Movetrack_Prep %>%
  group_by(motusTagID, visit_id, movement_type) %>%
  summarise(
    last_ts = max(ts),
    ping_count = n(),
    .groups = "drop"
  ) %>%
  # FIX: Convert final ping to local time to prevent the UTC midnight split
  mutate(
    last_ts_local = with_tz(last_ts, tzone = "America/Chicago"),
    date_local = as_date(last_ts_local)
  ) %>%
  left_join(twilight_data, by = "date_local") %>%
  mutate(
    twilight_dur = as.numeric(difftime(sunrise, nauticalDawn, units = "secs")),
    
    bio_period = case_when(
      last_ts_local <= nauticalDawn | last_ts_local >= nauticalDusk ~ "Nocturnal",
      last_ts_local > nauticalDawn & last_ts_local <= (sunrise + twilight_dur) ~ "Sunrise Window",
      last_ts_local >= (sunset - twilight_dur) & last_ts_local < nauticalDusk ~ "Sunset Window",
      last_ts_local > (sunrise + twilight_dur) & last_ts_local < (sunrise + (sunset-sunrise)/2) ~ "Morning",
      TRUE ~ "Afternoon"
    ),
    bio_period = factor(bio_period, levels = c(
      "Nocturnal", "Sunrise Window", "Morning", "Afternoon", "Sunset Window"
    ))
  )

# Step 2: Join with Outcome and Define Native Hardware Class
Raw_Analysis_Final <- Biological_Timing_Raw %>%
  left_join(Visit_Status_Raw, by = "visit_id") %>%
  # Bring in tagModel natively from Movetrack_Prep (No Tag_Metadata DB join needed!)
  left_join(Movetrack_Prep %>% distinct(visit_id, tagModel), by = "visit_id") %>%
  mutate(
    outcome = case_when(
      movement_type == "Arrival" ~ "Confirmed Return",
      movement_type == "Departure" & !is_terminal ~ "Temporary Departure",
      movement_type == "Departure" & is_terminal  ~ "Terminal Disappearance"
    ),
    # Define hardware natively
    hardware_class = case_when(
      grepl("LifeTag", tagModel, ignore.case = TRUE) ~ "LifeTag (Solar)",
      grepl("Hybrid", tagModel, ignore.case = TRUE) ~ "Hybrid (Batt/Solar)",
      TRUE ~ "Other/Unknown"
    )
  )

# -------------------------------------------------------------------------
# 9B. Statistical Matrix & Permutation Analysis
# -------------------------------------------------------------------------

# Step 1: Construct the Observed Contingency Matrix (Overall)
obs_matrix <- table(Raw_Analysis_Final$outcome, Raw_Analysis_Final$bio_period)

# NEW: Construct the 3-Way Matrix subset by TagModel (Hardware Class)
cat("\n--- 3-Way Outcome Matrix (Subset by TagModel) ---\n")
# ftable creates a clean, readable 'flat' table for 3 variables
matrix_by_tag <- ftable(
  Hardware = Raw_Analysis_Final$hardware_class, 
  Outcome = Raw_Analysis_Final$outcome, 
  Period = Raw_Analysis_Final$bio_period
)
print(matrix_by_tag)

# Step 2: Filter out any remaining zero-sum columns (Safety check for NaN)
# (Using the original 2-way matrix for the overall Chi-Squared test)
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


# -------------------------------------------------------------------------
# 9b. Hardware Timing Bias Audit (The "Solar Blackout" Test)
# -------------------------------------------------------------------------

cat("\n--- HARDWARE TIMING BIAS AUDIT (Terminal Disappearances Only) ---\n")

# Isolate terminal birds. NO JOIN NEEDED because hardware_class is natively there!
Terminal_Events <- Raw_Analysis_Final %>%
  filter(outcome == "Terminal Disappearance")

# Create a contingency table: When did the different tags 'disappear'?
hardware_timing_matrix <- table(Terminal_Events$hardware_class, Terminal_Events$bio_period)
print(hardware_timing_matrix)

# Statistical Test for Bias
# H0: Tag type does not affect the observed time of disappearance.
# HA: Tag type significantly biases the observed time (e.g., LifeTags skew Daylight/Sunset).
bias_test <- fisher.test(hardware_timing_matrix, simulate.p.value = TRUE, B = 5000)

cat(glue("\nHardware Bias Permuted p-value: {round(bias_test$p.value, 4)}\n"))
cat("Interpretation: If p < 0.05, LifeTags and Hybrids are recording fundamentally different departure schedules.\n\n")

# Visualizing the Bias
bias_plot <- ggplot(Terminal_Events, aes(x = bio_period, fill = hardware_class)) +
  geom_bar(position = "dodge", color = "black", alpha = 0.8) +
  scale_fill_manual(values = c("LifeTag (Solar)" = "#f1c40f", "Hybrid (Batt/Solar)" = "#2c3e50")) +
  theme_minimal() +
  labs(
    title = "Observed Departure Timing by Hardware Class",
    subtitle = glue("Terminal Disappearances Only | Fisher's p = {round(bias_test$p.value, 4)}"),
    x = "Biological Period of Last Ping",
    y = "Number of Birds",
    fill = "Tag Technology"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(bias_plot)

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
  
# -------------------------------------------------------------------------
# 12. Continuous Winter Weather Extraction (Oklahoma Mesonet)
# -------------------------------------------------------------------------

cat("\n--- DOWNLOADING CONTINUOUS WINTER CLIMATE DATA ---\n")

library(mesonet)
library(tidyverse)

# 1. Define the continuous winter seasons
# Using simple YYYY-MM-DD as preferred by mnet_retrieve
winter_seasons <- data.frame(
  start = c("2020-11-15", "2021-11-15", "2022-11-15"),
  end   = c("2021-04-30", "2022-04-30", "2023-04-30")
)

target_stations <- c("KENT", "BOIS")

# 2. Use your proven map_df() and mnet_retrieve() syntax
weather_raw <- map_df(1:nrow(winter_seasons), function(i) {
  cat(glue("Fetching Season {i}: {winter_seasons$start[i]} to {winter_seasons$end[i]}...\n"))
  
  mnet_retrieve(
    start_date = winter_seasons$start[i],
    end_date = winter_seasons$end[i],
    stid = target_stations
  )
})

# 3. Standardize, compute wind vectors, and average the stations
weather_micro <- weather_raw %>%
  rename_with(tolower) %>%
  rename(any_of(c(time = "date"))) %>%
  mutate(
    time = as_datetime(time, tz = "America/Chicago"),
    # Decompose wind into linear vectors BEFORE averaging
    u_wind = -wspd * sin(wdir * pi / 180),
    v_wind = -wspd * cos(wdir * pi / 180)
  ) %>%
  group_by(time) %>%
  summarise(
    tair = mean(tair, na.rm = TRUE),
    srad = mean(srad, na.rm = TRUE),
    wspd = mean(wspd, na.rm = TRUE),
    u_wind = mean(u_wind, na.rm = TRUE),
    v_wind = mean(v_wind, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(date = time) %>%
  arrange(date)

cat(glue("Mesonet download complete. Unbroken baseline of {nrow(weather_micro)} intervals retrieved.\n"))


# -------------------------------------------------------------------------
# 13. Weather Integration: Smoothed Anomaly, Shock & Consecutive Freeze
# -------------------------------------------------------------------------

cat("\n--- CALCULATING WEATHER TRIGGERS (SMOOTHED ANOMALY, SHOCK & DURATION) ---\n")

# Ensure the 'zoo' package is loaded
if(!require(zoo)) install.packages("zoo")
library(zoo)

# Step 1: Base formatting and extract Julian Day
weather_base <- weather_micro %>%
  rename_with(tolower) %>%
  mutate(
    ts_weather = ymd_hms(substr(as.character(date), 1, 19), tz = "America/Chicago"),
    tair = as.numeric(tair), 
    srad = as.numeric(srad),
    wspd = as.numeric(wspd),       # NEW: Capture wind speed
    u_wind = as.numeric(u_wind),   # NEW: Capture U-vector
    v_wind = as.numeric(v_wind),   # NEW: Capture V-vector
    day_of_year = yday(ts_weather) # Extract Julian Day (1-365/366)
  ) %>%
  filter(!is.na(ts_weather)) %>%
  group_by(ts_weather, day_of_year) %>%
  summarise(
    tair = mean(tair, na.rm = TRUE), 
    srad = mean(srad, na.rm = TRUE),
    wspd = mean(wspd, na.rm = TRUE),       # NEW: Pass it through
    u_wind = mean(u_wind, na.rm = TRUE),   # NEW: Pass it through
    v_wind = mean(v_wind, na.rm = TRUE),   # NEW: Pass it through
    .groups="drop"
  ) %>%
  arrange(ts_weather)

# Step 2: Calculate the RAW daily averages
raw_normals <- weather_base %>%
  group_by(day_of_year) %>%
  summarise(raw_tair = mean(tair, na.rm = TRUE), .groups = "drop") %>%
  arrange(day_of_year)

# Step 3: The Triplicate Wrap (7-Day Centered Moving Average)
# Triplicate the year to seamlessly calculate rolling means over Dec 31 -> Jan 1
daily_normals <- bind_rows(raw_normals, raw_normals, raw_normals) %>%
  mutate(
    # 7-day window: 3 days before, the current day, and 3 days after
    normal_tair = rollmean(raw_tair, k = 7, fill = NA, align = "center")
  ) %>%
  # Slice exactly the middle year to drop the padding and keep the perfectly smoothed 365 days
  slice((nrow(raw_normals) + 1) : (nrow(raw_normals) * 2)) %>%
  select(day_of_year, normal_tair)

# Step 4: Merge and calculate the Anomaly, Shock, and Stopwatch
weather_regional <- weather_base %>%
  left_join(daily_normals, by = "day_of_year") %>%
  mutate(
    # 1. The Climatological Anomaly (Departure from the SMOOTHED Normal)
    temp_anomaly = tair - normal_tair,
    
    # 2. Acute Shock (24h Delta-T)
    tair_24h_ago = lag(tair, 288),
    delta_t_24h = tair - tair_24h_ago,
    
    # 3. Freezing Flag & Stopwatch
    is_freezing = if_else(tair <= 0, 1, 0),
    thaw_id = cumsum(is_freezing == 0)
  ) %>%
  group_by(thaw_id) %>%
  mutate(
    consecutive_freeze_hrs = cumsum(is_freezing) * (5/60)
  ) %>%
  ungroup() %>%
  select(-thaw_id) # Clean up

cat(glue("Weather loaded. Max Freeze Streak: {round(max(weather_regional$consecutive_freeze_hrs, na.rm=T), 1)} hrs.\n"))
cat(glue("Most Extreme Cold Anomaly: {round(min(weather_regional$temp_anomaly, na.rm=T), 1)} C below 7-day smoothed normal.\n"))

# -------------------------------------------------------------------------
# 14. The Acute Shock Trigger Analysis (Density Curves & K-S Test)
# -------------------------------------------------------------------------

cat("\n--- PLOTTING ACUTE SHOCK DENSITY CURVES ---\n")

# Ensure the dataframe exists and is filtered
Acute_Shock_Analysis <- Raw_Analysis_Final %>%
  filter(movement_type == "Departure") %>%
  ungroup() %>%
  mutate(time_match = floor_date(last_ts_local, "5 mins")) %>%
  left_join(weather_regional, by = c("time_match" = "ts_weather")) %>%
  filter(!is.na(delta_t_24h))

# Console Summary for Statistical Proof (Adding Standard Deviation for Spread)
cat("\n--- Acute Shock (\u0394T) Statistics by Outcome ---\n")
summary_stats <- Acute_Shock_Analysis %>%
  group_by(outcome) %>%
  summarise(
    Mean_24h_Delta = round(mean(delta_t_24h, na.rm=TRUE), 2),
    Median_24h_Delta = round(median(delta_t_24h, na.rm=TRUE), 2),
    SD_Delta = round(sd(delta_t_24h, na.rm=TRUE), 2), # Measures how 'fat' the tails are
    N_Events = n()
  )
print(summary_stats)

# Two-Sample Kolmogorov-Smirnov Test
# Evaluates if the Temporary and Terminal distributions have fundamentally different shapes
temp_departures <- Acute_Shock_Analysis %>% filter(outcome == "Temporary Departure") %>% pull(delta_t_24h)
term_disappearances <- Acute_Shock_Analysis %>% filter(outcome == "Terminal Disappearance") %>% pull(delta_t_24h)

ks_result <- ks.test(temp_departures, term_disappearances)

cat(glue("\n--- Kolmogorov-Smirnov Test for Distribution Difference ---\n"))
cat(glue("D-statistic: {round(ks_result$statistic, 4)} | p-value: {round(ks_result$p.value, 4)}\n"))
cat("Interpretation: If p < 0.05, the shapes/spreads of these two departure types are statistically distinct.\n\n")

# Density Plot (Smooth Curves instead of stacked bars)
acute_density_plot <- ggplot(Acute_Shock_Analysis, aes(x = delta_t_24h, fill = outcome, color = outcome)) +
  geom_density(alpha = 0.4, linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray20", linewidth = 1) +
  scale_fill_manual(values = c("Temporary Departure" = "#2c3e50", "Terminal Disappearance" = "#f1c40f")) +
  scale_color_manual(values = c("Temporary Departure" = "#2c3e50", "Terminal Disappearance" = "#f1c40f")) +
  theme_minimal() +
  labs(
    title = "Acute Shock Departure Signatures: Density Curves",
    subtitle = glue("K-S Test p-value: {round(ks_result$p.value, 4)} | Comparing Distribution Shapes"),
    x = "24-Hour Temperature Change (\u00B0C) (Negative = Sudden Plunge)",
    y = "Density (Proportion of Events)",
    fill = "Event Outcome",
    color = "Event Outcome"
  )

print(acute_density_plot)

# -------------------------------------------------------------------------
# 15. Cold Snap Duration (The "Persistence" Trigger)
# -------------------------------------------------------------------------

cat("\n--- ANALYZING COLD SNAP PERSISTENCE TRIGGERS ---\n")

# Join the new consecutive freeze data to the departures
Duration_Analysis <- Raw_Analysis_Final %>%
  filter(movement_type == "Departure") %>%
  ungroup() %>%
  mutate(time_match = floor_date(last_ts_local, "5 mins")) %>%
  left_join(weather_regional, by = c("time_match" = "ts_weather")) %>%
  filter(!is.na(consecutive_freeze_hrs))

# Console Summary for Statistical Proof
cat("\n--- Consecutive Freeze Hours at Time of Departure ---\n")
print(
  Duration_Analysis %>%
    group_by(outcome) %>%
    summarise(
      Mean_Freeze_Duration = round(mean(consecutive_freeze_hrs, na.rm=TRUE), 1),
      Median_Freeze_Duration = round(median(consecutive_freeze_hrs, na.rm=TRUE), 1),
      Max_Endurance_Hrs = round(max(consecutive_freeze_hrs, na.rm=TRUE), 1),
      N_Events = n()
    )
)

# Plot 15A: The Pooled "Breaking Point" (All Departures)
pooled_duration_plot <- ggplot(Duration_Analysis, aes(x = consecutive_freeze_hrs)) +
  geom_density(fill = "steelblue", alpha = 0.5, color = "black", linewidth = 1) +
  theme_minimal() +
  labs(
    title = "The Breaking Point: Pooled Departure Thresholds",
    subtitle = "Density of all departures relative to the duration of the active freeze streak",
    x = "Consecutive Hours Below Freezing at Time of Departure",
    y = "Density (Proportion of Departures)"
  )

print(pooled_duration_plot)

# Plot 15B: The Split "Tolerance vs. Attrition" (Temporary vs. Terminal)
split_duration_plot <- ggplot(Duration_Analysis, aes(x = consecutive_freeze_hrs, fill = outcome, color = outcome)) +
  geom_density(alpha = 0.4, linewidth = 1) +
  scale_fill_manual(values = c("Temporary Departure" = "#2c3e50", "Terminal Disappearance" = "#f1c40f")) +
  scale_color_manual(values = c("Temporary Departure" = "#2c3e50", "Terminal Disappearance" = "#f1c40f")) +
  theme_minimal() +
  labs(
    title = "Tolerance vs. Attrition: Duration-Driven Departures",
    subtitle = "Comparing temporary foraging flights to permanent relocation thresholds",
    x = "Consecutive Hours Below Freezing at Time of Departure",
    y = "Density (Proportion of Departures)",
    fill = "Event Outcome",
    color = "Event Outcome"
  )

print(split_duration_plot)
# -------------------------------------------------------------------------
# 15. Cold Snap Duration (The 8-Hour Peak Smoothing)
# -------------------------------------------------------------------------

cat("\n--- ANALYZING COLD SNAP PERSISTENCE (8-HOUR PEAK) ---\n")

# Ensure the 'zoo' package is loaded for rolling window calculations
if(!require(zoo)) install.packages("zoo")
library(zoo)

# 1. Apply the 8-hour rolling peak to the weather data
weather_smoothed <- weather_regional %>%
  arrange(ts_weather) %>%
  mutate(
    # 8 hours = 96 intervals of 5 minutes. rollmaxr looks strictly backwards.
    peak_freeze_8h = rollmaxr(consecutive_freeze_hrs, k = 96, fill = NA)
  )

# 2. Join the smoothed data to the departures
Duration_Analysis_Smoothed <- Raw_Analysis_Final %>%
  filter(movement_type == "Departure") %>%
  ungroup() %>%
  mutate(time_match = floor_date(last_ts_local, "5 mins")) %>%
  left_join(weather_smoothed, by = c("time_match" = "ts_weather")) %>%
  filter(!is.na(peak_freeze_8h))

# 3. Console Summary for Statistical Proof
cat("\n--- Peak Freeze Hours (Within 8h of Departure) ---\n")
print(
  Duration_Analysis_Smoothed %>%
    group_by(outcome) %>%
    summarise(
      Mean_Peak_Freeze = round(mean(peak_freeze_8h, na.rm=TRUE), 1),
      Median_Peak_Freeze = round(median(peak_freeze_8h, na.rm=TRUE), 1),
      Max_Endurance_Hrs = round(max(peak_freeze_8h, na.rm=TRUE), 1),
      N_Events = n()
    )
)

# Plot 15B: The Split "Tolerance vs. Attrition" (Smoothed)
split_duration_smoothed_plot <- ggplot(Duration_Analysis_Smoothed, aes(x = peak_freeze_8h, fill = outcome, color = outcome)) +
  geom_density(alpha = 0.4, linewidth = 1) +
  scale_fill_manual(values = c("Temporary Departure" = "#2c3e50", "Terminal Disappearance" = "#f1c40f")) +
  scale_color_manual(values = c("Temporary Departure" = "#2c3e50", "Terminal Disappearance" = "#f1c40f")) +
  theme_minimal() +
  labs(
    title = "Tolerance vs. Attrition: Duration-Driven Departures",
    subtitle = "Peak consecutive freezing hours experienced within 8 hours prior to departure",
    x = "Peak Consecutive Hours Below Freezing (8h Window)",
    y = "Density (Proportion of Departures)",
    fill = "Event Outcome",
    color = "Event Outcome"
  )

print(split_duration_smoothed_plot)

# -------------------------------------------------------------------------
# 15c. Statistical Measurement of the Duration Shift
# -------------------------------------------------------------------------

cat("\n--- STATISTICAL SHIFT TEST: WILCOXON RANK-SUM ---\n")

# Extract the smoothed duration vectors for both outcomes
temp_durations <- Duration_Analysis_Smoothed %>% 
  filter(outcome == "Temporary Departure") %>% 
  pull(peak_freeze_8h)

term_durations <- Duration_Analysis_Smoothed %>% 
  filter(outcome == "Terminal Disappearance") %>% 
  pull(peak_freeze_8h)

# Run the non-parametric Wilcoxon test to check for a shift in central tendency
shift_test <- wilcox.test(temp_durations, term_durations, exact = FALSE)

cat(glue("Wilcoxon W-statistic: {round(shift_test$statistic, 2)}\n"))
cat(glue("p-value: {round(shift_test$p.value, 4)}\n"))
cat("Interpretation: If p < 0.05, the median shift between Temporary (11.2h) and Terminal (5.3h) is statistically significant.\n\n")

# Run the K-S test again just to see if the overall distribution shapes differ
ks_duration_test <- ks.test(temp_durations, term_durations)
cat(glue("Kolmogorov-Smirnov D-statistic: {round(ks_duration_test$statistic, 4)}\n"))
cat(glue("K-S p-value: {round(ks_duration_test$p.value, 4)}\n"))
cat("Interpretation: If p < 0.05, the overall shape/spread of the two curves are fundamentally distinct.\n")

# -------------------------------------------------------------------------
# 16. Discrete-Time Expansion (The Bird-Day Matrix)
# -------------------------------------------------------------------------

cat("\n--- BUILDING BIRD-DAY MATRIX FOR GLM MODELING ---\n")

# Step 1: Collapse the Long event data into Wide visit boundaries
Visit_Bounds <- Raw_Analysis_Final %>%
  group_by(motusTagID, hardware_class, visit_id) %>%
  summarise(
    # Find the first and last timestamps within this specific visit
    start_date = as.Date(min(last_ts_local, na.rm = TRUE), tz = "America/Chicago"),
    end_date   = as.Date(max(last_ts_local, na.rm = TRUE), tz = "America/Chicago"),
    # Isolate the outcome (Temporary/Terminal) from the Departure row of this visit
    departure_type = first(na.omit(outcome[movement_type == "Departure"])),
    .groups = "drop"
  ) %>%
  # Safety catch: Drop any visits that completely lack timestamps
  filter(!is.infinite(start_date) & !is.infinite(end_date))

# Step 2: Expand the Boundaries into Daily Occurrences
Bird_Day_Visits <- Visit_Bounds %>%
  rowwise() %>%
  mutate(date = list(seq(start_date, end_date, by = "day"))) %>%
  unnest(date) %>%
  # Categorize the biological state of the bird on that specific day
  mutate(
    daily_state = case_when(
      date == start_date & date == end_date ~ "Arrival & Departure",
      date == start_date ~ "Arrival",
      date == end_date ~ "Departure",
      TRUE ~ "Present"
    ),
    # The Binomial Response Variable: 1 if it left that day, 0 if it stayed
    departure_event = if_else(grepl("Departure", daily_state), 1, 0),
    
    # Only attach the departure type to the actual day it left
    departure_type = if_else(departure_event == 1, departure_type, NA_character_)
  ) %>%
  ungroup() %>%
  select(motusTagID, hardware_class, visit_id, date, daily_state, departure_event, departure_type)

# Step 3: Fill in the Gaps (The Absences)
Bird_Day_Matrix <- Bird_Day_Visits %>%
  group_by(motusTagID, hardware_class) %>%
  # complete() automatically finds missing days between visits and creates rows
  complete(date = seq(min(date), max(date), by = "day")) %>%
  mutate(
    daily_state = replace_na(daily_state, "Absent"),
    departure_event = replace_na(departure_event, 0), # Absent days are not new departures
    visit_id = zoo::na.locf(visit_id, na.rm = FALSE)  # Carries the last visit ID forward
  ) %>%
  ungroup() %>%
  arrange(motusTagID, date)

# Console Audit
cat(glue("Expansion complete: {nrow(Bird_Day_Matrix)} total bird-days logged.\n"))
cat("--- Breakdown of Daily States ---\n")
print(table(Bird_Day_Matrix$daily_state))

# -------------------------------------------------------------------------
# 17. Daily Weather Aggregation & GLM Join
# -------------------------------------------------------------------------

cat("\n--- AGGREGATING DAILY WEATHER FOR GLM ---\n")

Daily_Weather <- weather_regional %>%
  # Extract the pure calendar day from the 5-minute timestamp
  mutate(calendar_date = as.Date(ts_weather, tz = "America/Chicago")) %>%
  group_by(calendar_date) %>%
  summarise(
    # 1. Thermal Means & Extremes
    t_mean = mean(tair, na.rm = TRUE),
    t_min  = min(tair, na.rm = TRUE),
    t_max  = max(tair, na.rm = TRUE),
    
    # 2. Heating Degree Days (HDD): Cumulative cold stress below 18C baseline
    hdd_18 = if_else(t_mean < 18, 18 - t_mean, 0),
    
    # 3. Anomalies & Acute Shocks
    anomaly_mean = mean(temp_anomaly, na.rm = TRUE),
    anomaly_min  = min(temp_anomaly, na.rm = TRUE), # The most extreme drop below normal
    shock_max    = min(delta_t_24h, na.rm = TRUE),  # The sharpest 24h plunge that day
    
    # 4. Persistence (Ice)
    freeze_hrs_peak = max(consecutive_freeze_hrs, na.rm = TRUE),
    
    # 5. Solar & Wind Energy
    srad_sum = sum(srad, na.rm = TRUE),     # Total daily solar energy input
    wspd_mean = mean(wspd, na.rm = TRUE),   # Overall kinetic wind energy
    u_wind_mean = mean(u_wind, na.rm = TRUE), # Net East/West force
    v_wind_mean = mean(v_wind, na.rm = TRUE), # Net North/South force
    
    .groups = "drop"
  )

# Join the Daily Weather to the Bird-Day matrix!
GLM_Dataset <- Bird_Day_Matrix %>%
  left_join(Daily_Weather, by = c("date" = "calendar_date")) %>%
  # Filter out days where we have no weather data (e.g., API gaps)
  filter(!is.na(t_mean))

# Console Audit
cat(glue("GLM Matrix constructed: {nrow(GLM_Dataset)} bird-days with full weather metrics.\n"))
cat("Preview of Model Predictors (First 3 Rows):\n")
print(head(GLM_Dataset %>% select(date, departure_event, t_min, hdd_18, anomaly_min, freeze_hrs_peak, v_wind_mean), 3))

# -------------------------------------------------------------------------
# 18. Binomial GLM & AIC Hypothesis Testing
# -------------------------------------------------------------------------

cat("\n--- RUNNING BINOMIAL GLM & AIC COMPETITION ---\n")

# Step 1: Enforce the Golden Rule, Filter for Mid-Winter, and Scale
Model_Data <- GLM_Dataset %>%
  # NEW: The Mid-Winter Firewall (Isolate Nov, Dec, Jan, Feb)
  filter(month(date) %in% c(11, 12, 1, 2)) %>%
  
  select(departure_event, hdd_18, shock_max, anomaly_min, freeze_hrs_peak, v_wind_mean, srad_sum) %>%
  drop_na() %>%
  mutate(across(-departure_event, ~scale(.)[,1]))

cat(glue("Data standardized & Spring amputated. Running competition on {nrow(Model_Data)} pure winter bird-days...\n\n"))

# Step 2: Build the Candidate Biological Hypotheses
# Family = binomial tells the GLM we are predicting a 0 or 1 outcome
m_null    <- glm(departure_event ~ 1, family = binomial(link = "logit"), data = Model_Data)
m_thermal <- glm(departure_event ~ hdd_18, family = binomial(link = "logit"), data = Model_Data)
m_shock   <- glm(departure_event ~ shock_max, family = binomial(link = "logit"), data = Model_Data)
m_anomaly <- glm(departure_event ~ anomaly_min, family = binomial(link = "logit"), data = Model_Data)
m_ice     <- glm(departure_event ~ freeze_hrs_peak, family = binomial(link = "logit"), data = Model_Data)
m_wind    <- glm(departure_event ~ v_wind_mean, family = binomial(link = "logit"), data = Model_Data)

# The Additive Model: Do birds wait for high thermal stress + ice + a strong North wind to push them south?
m_global  <- glm(departure_event ~ hdd_18 + freeze_hrs_peak + v_wind_mean, family = binomial(link = "logit"), data = Model_Data)

# Step 3: Construct the Delta AIC Leaderboard
aic_table <- tibble(
  Hypothesis = c(
    "Null (Random Departures)", 
    "Thermal Stress (Heating Degree Days)", 
    "Acute Shock (Max 24h Plunge)",
    "Climatological Anomaly (Drop below normal)", 
    "Persistence (Peak Consecutive Ice)", 
    "Flight Subsidies (V-Wind / North-South)", 
    "Global Additive (Stress + Ice + Wind)"
  ),
  AIC_Score = c(AIC(m_null), AIC(m_thermal), AIC(m_shock), AIC(m_anomaly), AIC(m_ice), AIC(m_wind), AIC(m_global))
) %>%
  mutate(
    Delta_AIC = AIC_Score - min(AIC_Score),
    # Akaike Weights indicate the probability (0 to 1) that this is the best model in the set
    Weight = exp(-0.5 * Delta_AIC) / sum(exp(-0.5 * Delta_AIC))
  ) %>%
  arrange(Delta_AIC) %>%
  mutate(across(where(is.numeric), ~round(., 3)))

# Console Readouts
cat("--- AIC MODEL LEADERBOARD ---\n")
print(as.data.frame(aic_table))

cat("\n--- COEFFICIENTS OF THE GLOBAL MODEL ---\n")
cat("Note: Because variables are scaled, larger absolute estimates = stronger biological drivers.\n")
print(summary(m_global)$coefficients)

# -------------------------------------------------------------------------
# 19. Full-Season Phenology & Departure Fate Models
# -------------------------------------------------------------------------

cat("\n--- RUNNING FULL-SEASON & FATE MODELS ---\n")

# Step 1: Format the Data and Fix the Julian Wrap-Around
Model_Data <- GLM_Dataset %>%
  mutate(
    # Create a continuous "Days Since Nov 1" index to avoid the Dec31->Jan1 reset
    season_start = as.Date(paste0(year(date) - if_else(month(date) < 8, 1, 0), "-11-01")),
    phenology_day = as.numeric(date - season_start)
  ) %>%
  # Only drop rows where the *weather* is missing, don't drop NA departure types
  drop_na(anomaly_min, hdd_18) %>%
  # Scale predictors so coefficients can be directly compared
  mutate(
    anomaly_scaled = scale(anomaly_min)[,1],
    pheno_scaled   = scale(phenology_day)[,1]
  )

cat(glue("Full-Season Matrix compiled: {nrow(Model_Data)} bird-days.\n\n"))

# -------------------------------------------------------------------------
# MODEL A: The Phenological Shift (Does the weather trigger change over time?)
# Testing the Interaction (*) between Anomaly and Phenology Day
m_season <- glm(departure_event ~ anomaly_scaled * pheno_scaled, family = binomial, data = Model_Data)

cat("--- MODEL A: OVERALL DEPARTURE PROBABILITY ---\n")
cat("Testing if the reaction to cold anomalies shifts as spring approaches.\n")
print(summary(m_season)$coefficients)
cat("\n")

# -------------------------------------------------------------------------
# MODEL B: The Fate Model (Temporary vs. Terminal)
# Isolating ONLY the days a departure occurred to determine what drove the outcome
Fate_Data <- Model_Data %>%
  filter(departure_event == 1) %>%
  # 1 = Terminal Disappearance, 0 = Temporary Departure
  mutate(is_terminal = if_else(departure_type == "Terminal Disappearance", 1, 0))

m_fate <- glm(is_terminal ~ anomaly_scaled + pheno_scaled, family = binomial, data = Fate_Data)

cat("--- MODEL B: DEPARTURE FATE (GIVEN A DEPARTURE OCCURRED) ---\n")
cat("1 = Terminal, 0 = Temporary.\n")
cat("Positive Phenology estimate = More likely to be Terminal as season progresses.\n")
cat("Positive Anomaly estimate = More likely to be Terminal when weather is WARMER/NORMAL.\n")
print(summary(m_fate)$coefficients)

# -------------------------------------------------------------------------
# 20. The Hiatus Matrix & Return-Trigger Models
# -------------------------------------------------------------------------

cat("\n--- BUILDING HIATUS MATRIX FOR RETURN MODELS ---\n")

# Step 1: Isolate the "Away" streaks that ended in a successful return
Hiatus_Data <- GLM_Dataset %>%
  arrange(motusTagID, date) %>%
  group_by(motusTagID) %>%
  mutate(
    # Flag days the bird is away from the array (Absent or the day it Arrives)
    is_away = if_else(daily_state %in% c("Absent", "Arrival", "Arrival & Departure"), 1, 0),
    # Create a unique ID for each continuous block of away time
    away_streak = cumsum(is_away != lag(is_away, default = 0))
  ) %>%
  filter(is_away == 1) %>%
  group_by(motusTagID, away_streak) %>%
  # CRITICAL: Drop terminal disappearances! Only keep streaks that actually end in an Arrival
  filter(any(grepl("Arrival", daily_state))) %>%
  ungroup() %>%
  mutate(
    # The Binomial Response: 1 on the day it returns, 0 for every day it stayed away
    return_event = if_else(grepl("Arrival", daily_state), 1, 0)
  )

# Step 2: Enforce the Mid-Winter Firewall and Scale Predictors (Now with Anomaly)
Return_Model_Data <- Hiatus_Data %>%
  filter(month(date) %in% c(11, 12, 1, 2)) %>%
  # Adding anomaly_mean to the selection
  select(return_event, hdd_18, t_max, v_wind_mean, srad_sum, anomaly_mean) %>%
  drop_na() %>%
  mutate(across(-return_event, ~scale(.)[,1]))

# Step 3: Build Candidate Return Hypotheses (Including Anomaly)
m_ret_null    <- glm(return_event ~ 1, family = binomial, data = Return_Model_Data)
m_ret_stress  <- glm(return_event ~ hdd_18, family = binomial, data = Return_Model_Data)
m_ret_thaw    <- glm(return_event ~ t_max, family = binomial, data = Return_Model_Data)
m_ret_wind    <- glm(return_event ~ v_wind_mean, family = binomial, data = Return_Model_Data)
m_ret_anomaly <- glm(return_event ~ anomaly_mean, family = binomial, data = Return_Model_Data)

# Global Model: Does a return require the anomaly to break + a tailwind?
m_ret_global  <- glm(return_event ~ anomaly_mean + v_wind_mean + t_max, family = binomial, data = Return_Model_Data)

# Step 4: Construct the Return Delta AIC Leaderboard
aic_return_table <- tibble(
  Hypothesis = c(
    "Null (Random Returns)", 
    "Stress Relaxation (HDD)", 
    "The Thaw (T-Max)", 
    "Tailwind Subsidy (V-Wind)", 
    "Return to Normal (Anomaly)",
    "Global Additive"
  ),
  AIC_Score = c(AIC(m_ret_null), AIC(m_ret_stress), AIC(m_ret_thaw), AIC(m_ret_wind), AIC(m_ret_anomaly), AIC(m_ret_global))
) %>%
  mutate(
    Delta_AIC = AIC_Score - min(AIC_Score),
    Weight = exp(-0.5 * Delta_AIC) / sum(exp(-0.5 * Delta_AIC))
  ) %>%
  arrange(Delta_AIC) %>%
  mutate(across(where(is.numeric), ~round(., 3)))

# Console Readouts
cat("--- UPDATED AIC RETURN LEADERBOARD ---\n")
print(as.data.frame(aic_return_table))

cat("\n--- COEFFICIENTS OF THE ANOMALY RETURN MODEL ---\n")
print(summary(m_ret_anomaly)$coefficients)

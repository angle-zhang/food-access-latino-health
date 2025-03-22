# ------ CALCULATE PROXIMITY MEASURES ------ #
proximity_measure <- function (pop_cent, poi, mode='classic') {
  # Calculate proximity measures
  nearest <- st_nearest_feature(pop_cent, poi, check_crs = TRUE)
  if(mode == 'classic') {
    dist <- st_distance(pop_cent, poi[nearest,], by_element = TRUE)
  } else if(mode == 'network') {
    # TODO calculate distance by street network
  }
  #assign row number of nearest poi to population data
  pop_cent$nearest_poi <- nearest
  pop_cent$nearest_dist <- dist
}


# get household points from parcel data
la_hh <- get_lac_households(proj_crs) #

la_hh_cleaned <- la_hh %>%
  filter(is.na(EXCLUDE) & UseType=="Residential") %>% # column "EXCLUDE" which is either null or "1" (~107 K) — if "1" we leave it out of our analyses because we don't want to include these units for various reasons.
  st_transform(4326) %>% # OSM data is in 4326
  mutate(id=row_number()) %>%
  mutate(lon = st_coordinates(.)[,1], lat = st_coordinates(.)[,2])


# ------ LOAD FOOD INSPECTION POI DATA ------ #
# Load LA County food inspection data (2021-2024)
foodinsp_21_24 <- get_foodinsp_lacounty()

# Load LA County food inspection data (2023-2024) from SSI
foodinsp23_24_SSI <- get_foodins_lacounty_ssi(proj_crs)

# remove duplicates with facility ID, make sure to get the most recent status open vs closed
foodinsp23_24_SSIclean <- foodinsp23_24_SSI %>% 
  mutate(SOURCE=factor(SOURCE, ordered=T, levels=c("Dec_2023", "March_2024","June_2024", "Dec_2024" ))) %>%
  group_by(FACILITY_ID) %>% 
  filter(SOURCE == max(SOURCE)) %>% 
  ungroup() %>%
  st_transform(4326) %>% # OSM data is in 4326
  rename(id=OBJECTID) %>%
  mutate(lon = st_coordinates(.)[,1], lat = st_coordinates(.)[,2]) %>%
  mutate(count=1)
# TODO test
#mutate(SIZE = str_extract(PE_DESCRIPTION, "\\d*-\\d*")) 
base_path <- "C:/Users/angie/OneDrive/Desktop/data-analysis/0_shared-data/raw/"


# setup r5r
data_path <- paste0(base_path, "osm_socal")

r5r_core <- setup_r5(data_path = data_path)

#r5r_sitrep()
#Sys.timezone(location = TRUE)

# split la_hh_cleaned into two datasets



# first find walkability for all points at various cutoffs
access_walk1 <- accessibility(r5r_core,
                        origins = la_hh_cleaned[1:1000000,],
                        destinations = foodinsp23_24_SSIclean,
                        opportunities_colnames = "count",
                        mode = "WALK",
                        decay_function = "step",
                        cutoffs = c(5, 10, 15, 20),
                        departure_datetime =  as.POSIXct(tz="America/Los_Angeles", "2025-03-21 18:00:00"), # 6pm on a friday
                        time_window = 60,
                        progress = F)

as.POSIXct(tz="America/Los_Angeles", "2025-03-21 18:00:00")

access_walk2 <- accessibility(r5r_core,
                              origins = la_hh_cleaned[1000001:2000000,],
                              destinations = foodinsp23_24_SSIclean,
                              opportunities_colnames = "count",
                              mode = "WALK",
                              decay_function = "step",
                              cutoffs = c(5, 10, 15, 20),
                              departure_datetime =  as.POSIXct(tz="America/Los_Angeles", "2025-03-21 18:00:00"), # 6pm on a friday
                              time_window = 60,
                              progress = F)

access_walk3 <- accessibility(r5r_core,
                              origins = la_hh_cleaned[2000001:3000000,],
                              destinations = foodinsp23_24_SSIclean,
                              opportunities_colnames = "count",
                              mode = "WALK",
                              decay_function = "step",
                              cutoffs = c(5, 10, 15, 20),
                              departure_datetime =  as.POSIXct(tz="America/Los_Angeles", "2025-03-21 18:00:00"), # 6pm on a friday
                              time_window = 60,
                              progress = F)

access_walk4 <- accessibility(r5r_core,
                              origins = la_hh_cleaned[3000001:nrow(la_hh_cleaned),],
                              destinations = foodinsp23_24_SSIclean,
                              opportunities_colnames = "count",
                              mode = "WALK",
                              decay_function = "step",
                              cutoffs = c(5, 10, 15, 20),
                              departure_datetime =  as.POSIXct(tz="America/Los_Angeles", "2025-03-21 18:00:00"), # 6pm on a friday
                              time_window = 60,
                              progress = F)




# write access_walk to file
write_sf(access_walk1, paste0(processed_path, "LAC_origins/access_walk1.gpkg"))
write_sf(access_walk2, paste0(processed_path, "LAC_origins/access_walk2.gpkg"))
write_sf(access_walk3, paste0(processed_path, "LAC_origins/access_walk3.gpkg"))
write_sf(access_walk4, paste0(processed_path, "LAC_origins/access_walk4.gpkg"))

# calculate accessibility for drive times for 50 points at a time in la_hh_cleaned until all points are processed
# use a for loop
# RBIND

access_drive = data.frame()

for (i in seq(1, nrow(la_hh_cleaned), 150)) {  
  print(i)
   access_drive <- accessibility(r5r_core,
                        origins = la_hh_cleaned[i:(i+150),],
                        destinations = foodinsp23_24_SSIclean,
                        opportunities_colnames = "count",
                        mode = "CAR",
                        decay_function = "step",
                        cutoffs = c(5, 10, 15, 20),
                        departure_datetime =  as.POSIXct(tz="America/Los_Angeles", "2025-03-21 18:00:00"), # 6pm on a friday
                        time_window = 60,
                        progress = F) %>%
   rbind(access_drive)
   
}

write_sf(access_drive, paste0(processed_path, "LAC_origins/access_drive.gpkg"))


access <- accessibility(r5r_core,
                        origins = la_hh_sample,
                        destinations = foodinsp23_24_SSIclean,
                        opportunities_colnames = "count",
                        mode = "WALK",
                        decay_function = "step",
                        cutoffs = c(15),
                        departure_datetime =  as.POSIXct("2025-03-21 18:00:00"),
                        time_window = 60,
                        progress = T)


# may need to run this in VM
travel_times <- travel_time_matrix(
  r5r_core,
  origins = la_hh_sample,
  destinations = foodinsp23_24_SSIclean,
  max_trip_duration = 30,
  mode = c("CAR"),  # Adjust as needed
  departure_datetime = as.POSIXct("2025-03-21 08:00:00"),
  time_window = 30,
  n_threads = parallel::detectCores() - 1
)
# processing 5000 points
# started 1:30pm 
# ended 


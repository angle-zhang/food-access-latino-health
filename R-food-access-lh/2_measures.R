source("0_Libraries.R")


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

la_city <- get_city_boundary(4326)
tm_shape(la_city) + tm_borders() # inspect city

la_city_hh <- la_hh_cleaned %>%
  filter(st_within(., la_city, sparse = FALSE) | st_intersects(., la_city, sparse = FALSE))

# Compare number of households in LA County vs LA City 
print(nrow(la_hh_cleaned))
print(nrow(la_city_hh))


# ------ LOAD FOOD MARKET POI DATA ------ #
# Load LA County food inspection data (2021-2024)
foodinsp_21_24 <- get_foodinsp_lacounty()

# Load LA County food inspection data (2023-2024) from SSI
foodinsp23_24_SSI <- get_foodins_lacounty_ssi(proj_crs)
foodmarket_23_DA <- get_retail_food_markets_LB_PAS(proj_crs)

# remove duplicates with facility ID, make sure to get the most recent status open vs closed
foodinsp23_24_SSIclean <- foodinsp23_24_SSI %>% 
  mutate(SOURCE=factor(SOURCE, ordered=T, levels=c("Dec_2023", "March_2024","June_2024", "Dec_2024"))) %>%
  group_by(FACILITY_ID) %>% 
  filter(SOURCE == max(SOURCE)) %>% 
  ungroup() %>%
  mutate(SOURCE="food_inspection") %>%
  mutate(
    small = ifelse(grepl("1-1,999", PE_DESCRIPTION), 1, 0),
    large = ifelse(grepl("2,000", PE_DESCRIPTION), 1, 0)
  ) %>%
  st_transform(4326) %>% # OSM data is in 4326
  mutate(lon = GEOCODE_LONGITUDE, lat = GEOCODE_LATITUDE) %>%
  mutate(count=1) %>%
  select(lon, lat, count, small, large, MATCH_ADDR, FACILITY_NAME, SOURCE)

# clean data axle market data from 2023
foodmarket_23_DA_clean <- foodmarket_23_DA %>%
  mutate(SOURCE="data_axle") %>%
  rename(FACILITY_NAME = COMPANY_NAME) %>%
  st_transform(4326) %>% # OSM data is in 4326
  mutate(lon = st_coordinates(.)[,1], lat = st_coordinates(.)[,2]) %>%
  mutate(count=1) %>%
  select(lon, lat, count, MATCH_ADDR, FACILITY_NAME, SOURCE)
  
foodmarket_merged <- bind_rows(foodinsp23_24_SSIclean, foodmarket_23_DA_clean)

# setup r5r
data_path <- paste0(base_path, "osm_socal")

r5r_core <- setup_r5(data_path = data_path)

# function for computing accessibility measures
compute_accessibility <- function(origins, destinations, mode, chunk_size, base_path, cutoffs = c(5, 10, 15, 20, 25, 30, 35, 40, 45), colnames,
                                  time_window = 30, departure_time = "2025-03-21 18:00:00", progress = FALSE, point_type="parcel") {
                                                            
  # Convert departure time to POSIXct
  departure_time <- as.POSIXct(departure_time)
  departure_time_formatted <- format(departure_time, "%Y%m%d_%H%M")
  
  # Construct the output file name
  output_path <- paste0(processed_path, "LAC_accessibility/", point_type, "_access_", tolower(mode), "_", departure_time_formatted, ".csv")

  # Get the number of rows in the origins dataset
  num_rows <- nrow(origins)
  
  # Initialize an empty list to store results
  access_results <- list()
  very_start <- Sys.time()
  
  # Loop through the dataset in chunks
  for (i in seq(1, num_rows, by = chunk_size)) {
    # Define the end index for the current chunk
    end_idx <- min(i + chunk_size - 1, num_rows)
    start <- Sys.time()
    
    # Compute accessibility for the current chunk
    access_chunk_res <- accessibility(
      r5r_core,
      origins = origins[i:end_idx, ],
      destinations = destinations,
      opportunities_colnames = colnames,
      mode = mode,
      decay_function = "step",
      cutoffs = cutoffs,
      departure_datetime = departure_time, 
      time_window = time_window,
      progress = progress
    )
    end <- Sys.time()
    time <- end - start
    total_time <- end - very_start
    print(paste("Processed rows:", i, "to", end_idx, ">>", time))
    print(paste("Total time elapsed:", total_time))
    
    # Store the chunk in the results list
    access_results[[length(access_results) + 1]] <- access_chunk_res
    write.csv(access_chunk_res, output_path, append=T)
    
    
  }
  
  print("Finished processing origins")
  # Combine all results into a single dataframe after looping through all of them
  access_data <- bind_rows(access_results)
  print(paste("Saving results to:", output_path))
  write_sf(access_data, output_path, append=F)
  print(paste("Saved results to:", output_path))
  return(access_data)
  
}

unique(foodinsp23_24_SSIclean$PE_DESCRIPTION)

# generate access for parcels
# todo - test time zones
# access_walk <- compute_accessibility(
#   origins = la_hh_cleaned,
#   destinations = foodinsp23_24_SSIclean,
#   mode = "WALK",
#   chunk_size = 500000,
#   base_path = processed_path, 
#   colnames = c("count", "small", "large")
# )

# turn this into function calculating chunk size
calc_chunk_size <- function(ram) { 
  # chunk size
  dt_chunk <- 5000 # DRIVE TIME CHUNK SIZE  for 128 GB ram
  proportion <- ram/128
  chunk_size <- floor(dt_chunk * proportion)  
  return(chunk_size)
}

# calculate only for households within LA city
# calculate access to all markets
access_drive <- compute_accessibility(
  origins = la_city_hh,
  destinations = foodmarket_merged,
  mode = "CAR",
  chunk_size = calc_chunk_size(ram=16),
  base_path = processed_path,
  cutoff=c(5, 10, 15, 20, 25),
  colnames = c("count"), 
  progress=F,
  point_type="parcel_LACITY_allpoi",
)

 
# check progress 
print(base_path)
access <- read_sf(paste0(processed_path, "LAC_accessibility/", "parcel_access_car_20250321_1800.gpkg"))
nrow(access) # see progress

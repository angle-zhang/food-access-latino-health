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
  mutate(
    small = ifelse(grepl("1-1,999", PE_DESCRIPTION), 1, 0),
    large = ifelse(grepl("2,000", PE_DESCRIPTION), 1, 0)
  ) %>%
  st_transform(4326) %>% # OSM data is in 4326
  rename(id=OBJECTID) %>%
  mutate(lon = st_coordinates(.)[,1], lat = st_coordinates(.)[,2]) %>%
  mutate(count=1) 
# TODO test
#mutate(SIZE = str_extract(PE_DESCRIPTION, "\\d*-\\d*")) 


# setup r5r
data_path <- paste0(base_path, "osm_socal")

r5r_core <- setup_r5(data_path = data_path)

# function for computing accessibility measures
compute_accessibility <- function(origins, destinations, mode, chunk_size, base_path, cutoffs = c(5, 10, 15, 20, 25, 30), colnames,
                                  time_window = 60, departure_time = "2025-03-21 18:00:00", progress = FALSE) {
                                                            
  # Convert departure time to POSIXct
  departure_time <- as.POSIXct(departure_time)
  departure_time_formatted <- format(departure_time, "%Y%m%d_%H%M")
  
  # Construct the output file name
  output_path <- paste0(processed_path, "LAC_accessibility/", "access_", tolower(mode), "_", departure_time_formatted, ".gpkg")

  # Get the number of rows in the origins dataset
  num_rows <- nrow(origins)
  
  # Initialize an empty list to store results
  access_results <- list()
  
  # Loop through the dataset in chunks
  for (i in seq(1, num_rows, by = chunk_size)) {
    # Define the end index for the current chunk
    end_idx <- min(i + chunk_size - 1, num_rows)
    print(paste("Processing rows:", i, "to", end_idx))
    
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
    
    # Store the chunk in the results list
    access_results[[length(access_results) + 1]] <- access_chunk_res
    # Write the output file
    print(paste("Saving results to:", output_path))
    write_sf(access_chunk_res, output_path, append=T)
    
  }
  print("Finished processing origins")
  
  # Combine all results into a single dataframe after looping through all of them
  access_data <- bind_rows(access_results)
  
 
  print(paste("Saved results to:", output_path))
  
  return(access_data)
}

unique(foodinsp23_24_SSIclean$PE_DESCRIPTION)


# todo - test time zones
access_walk <- compute_accessibility(
  origins = la_hh_cleaned,
  destinations = foodinsp23_24_SSIclean,
  mode = "WALK",
  chunk_size = 500000,
  base_path = processed_path, 
  colnames = c("count", "small", "large")
)

access_drive <- compute_accessibility(
  origins = la_hh_cleaned,
  destinations = foodinsp23_24_SSIclean,
  mode = "CAR",
  chunk_size = 1000,
  base_path = processed_path,
  colnames = c("count", "small", "large")
)

access_drive2 <- compute_accessibility(
  origins = la_hh_cleaned[1:5000,],
  destinations = foodinsp23_24_SSIclean,
  mode = "CAR",
  chunk_size = 1000,
  base_path = processed_path,
  colnames = c("count", "small", "large"),
  departure_time = "2025-03-21 01:00:00"
)

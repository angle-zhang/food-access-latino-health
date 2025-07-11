source("0_Libraries.R")

# ------ CALCULATE PROXIMITY MEASURES ------ #
# proximity_measure <- function (pop_cent, poi, mode='classic') {
  # Calculate proximity measures
# nearest <- st_nearest_feature(pop_cent, poi, check_crs = TRUE)
#  if(mode == 'classic') {
#    dist <- st_distance(pop_cent, poi[nearest,], by_element = TRUE)
#  } else if(mode == 'network') {
    # TODO calculate distance by street network
#  }
  #assign row number of nearest poi to population data
#  pop_cent$nearest_poi <- nearest
#  pop_cent$nearest_dist <- dist
#}

# ------ LOAD ORIGINS ------ #
# get household points from parcel data
la_hh <- get_lac_households(proj_crs) #

la_hh_cleaned <- la_hh %>%
  filter(is.na(EXCLUDE) & UseType=="Residential") %>% # column "EXCLUDE" which is either null or "1" (~107 K) — if "1" we leave it out of our analyses because we don't want to include these units for various reasons.
  st_transform(4326) %>% # OSM data is in 4326
  mutate(id=row_number()) %>%
  mutate(lon = st_coordinates(.)[,1], lat = st_coordinates(.)[,2])

# save processed data
# to-do wrap in function
write.csv(la_hh_cleaned, paste0(processed_path, "/LAC_origins/la_hh_cleaned.csv"))
rm(la_hh)

la_city <- get_city_boundary(4326)
tm_shape(la_city) + tm_borders() # inspect city

la_city_hh <- la_hh_cleaned %>%
  filter(st_within(., la_city, sparse = FALSE) | st_intersects(., la_city, sparse = FALSE))

# Compare number of households in LA County vs LA City 
print(nrow(la_hh_cleaned))
print(nrow(la_city_hh))


# get census tract centroids and transform them to correct format
la_ctcent_dat <- get_lac_centroids() %>%
  st_transform(4326) %>% # OSM data is in 4326
  mutate(id=row_number()) %>%
  mutate(lon = st_coordinates(.)[,1], lat = st_coordinates(.)[,2])

# make key for id and geoid
la_ct_key <- la_ctcent_dat %>%
  select(id, GEOID) %>%
  st_drop_geometry()

# save key to processed data
write.csv(la_ct_key, paste0(processed_path, "/LAC_origins/la_ct_key.csv"))

# get pop weighted centroids 
la_ct_wtcent_dat <- get_lac_weight_centroids() %>%
  st_transform(4326) %>% # OSM data is in 4326
  merge(la_ct_key, by="GEOID") %>%
  mutate(lon = st_coordinates(.)[,1], lat = st_coordinates(.)[,2])



#!file.exists('../../0_shared-data/processed/LAC_accessibility/density/la_city/parcel_CAR20250321_1800_1.csv')
# setup r5r
data_path <- paste0(base_path, "osm_socal")

r5r_core <- setup_r5(data_path = data_path)

# function for computing accessibility measures
compute_accessibility <- function(origins, destinations, mode, chunk_size, cutoffs = c(5, 10, 15, 20, 25, 30, 35, 40, 45), colnames,
                                  origin_type, output_path, file_id=NULL, # used to keep track of files being generated on multiple machines
                                  time_window = 30, departure_time = "2025-03-21 18:00:00", progress = FALSE) {
                                                            
  # Convert departure time to POSIXct
  departure_time <- as.POSIXct(departure_time)
  departure_time_formatted <- format(departure_time, "%Y%m%d_%H%M")
  
  # Construct the output file name
  if (is.null(file_id)) {
    file_name <- paste0(origin_type, "_", mode, departure_time_formatted, ".csv")
    output_file <- paste0(output_path, file_name)
  } else {
    file_name <- paste0(origin_type, "_", mode, departure_time_formatted, "_", file_id, ".csv")
    output_file <- paste0(output_path, file_name)
  }
  
  # Only create the output file if it doesn't exist
  if(!file.exists(output_file)){
    print("Output file doesn't exist, creating now")
    file.create(output_file)  
  }
  
  # Get the number of rows in the origins dataset
  num_rows <- nrow(origins)
  
  # Initialize an empty list to store results
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
    
    # Store the chunk in the results list, TODO check if this breaks down based on file size
    write.table(access_chunk_res, sep=",", output_file, col.names=NA, append=T)
  }
  print("Finished processing origins")
  # Combine all results into a single dataframe after looping through all of them (may be too large of an operation?)
  #access_data <- bind_rows(access_results)
  # print(paste("Saving results to:", output_path))
  # write.csv(access_data, output_file, append=F)
  # print(paste("Saved results to:", output_path))
  return(output_file)
  
}

# TODO move this to a different file (e.g. helpers)
setup_access_measure_folders <- function(access_path) { 
  # Setup folder structure 
  measures <- c("proximity", "density", "ratio", "gravity")
  geographies <- c("la_city", "la_county")
  categories <- c("all_markets")
  
  dir.create(access_path)
  
  # create folder for each measure and a folder within each measure for each geography
  for (measure in measures) {
    measure_path <- paste(access_path, measure, sep="/")
    for (geography in geographies) dir.create(paste(measure_path, geography, sep="/"), recursive=T)
  }
  
}

# TODO move this to a different file (e.g. helpers)
# turn this into function calculating chunk size
calc_chunk_size <- function(ram, mode) { 
  # chunk size
  if (mode == "WALK") dt_chunk <- 1000000 # WALK TIME CHUNK SIZE for 128 GB ram
  else if (mode == "CAR") dt_chunk <- 4500 # DRIVE TIME CHUNK SIZE  for 128 GB ram
  proportion <- ram/128
  chunk_size <- floor(dt_chunk * proportion)  
  return(chunk_size)
}

# Compute values for LA CITY Parcels
# generate access for parcels
# access_walk <- compute_accessibility(
#   origins = la_city_hh,
#   destinations = foodmarket_merged,
#   mode = "WALK",
#   chunk_size =calc_chunk_size(ram=12, mode="WALK"),  #calc_chunk_size(ram=12, mode="WALK"),
#   output_path = paste(access_path, "density/la_city/", sep="/"),
#   origin_type = "parcel",
#   colnames = c("count", "small", "large")
# )

# Compute values for LA CITY Parcels
# generate access for parcels
# access_walk <- compute_accessibility(
#   origins = la_ctcent_dat,
#   destinations = foodmarket_merged,
#   mode = "WALK",
#   chunk_size =calc_chunk_size(ram=8, mode="WALK"),  #calc_chunk_size(ram=12, mode="WALK"),
#   output_path = paste(access_path, "density/la_city/", sep="/"),
#   origin_type = "ct_cent",
#   colnames = c("count", "small", "large")
# )
# 
# access_walk <- compute_accessibility(
#   origins = la_ct_wtcent_dat,
#   destinations = foodmarket_merged,
#   mode = "WALK",
#   chunk_size =calc_chunk_size(ram=8, mode="WALK"),  #calc_chunk_size(ram=12, mode="WALK"),
#   output_path = paste(access_path, "density/la_city/", sep="/"),
#   origin_type = "ct_wtcent",
#   colnames = c("count", "small", "large")
# )



# TODO add this to a function 
# get ids in la_city_hh that are not in parcel walk
# missing_id <- la_city_hh[!(la_city_hh$id %in% parcel_WALK20250321_1800$id),]
# 
# # make sure # matches
# length(unique(la_city_hh$id))
# length(unique(parcel_WALK20250321_1800$id))
# 
# # get number of NAs in parcel walk
# sum(is.na(parcel_WALK20250321_1800$id))

# calculate only for households within LA city
# calculate access to all markets
# subdivide hh data into two datasets for running on separate devices based on number of devices
# DEVICE 1
nrow(la_city_hh)
split <- round(seq(15795, nrow(la_city_hh), length.out=6))

split[1]
split[2]

#d evice #3 split[3] +  28851 - split [4] -1
# device #4 split[4] - split[5]

typeof(la_city_hh)
match(560452, la_city_hh$id)

# TODO Map all the data inputs so see errors/ issues 
# TODO isolate small region in LA to test on 
# [x] TODO run code for other orgins (CTs, Weighted CTs)
#TODO change return value of access_drive to index?
split[6]- split[5]

la_city_hh_remain <- la_city_hh[!(la_city_hh$id %in% temp$id),]

# finished 1-2 and 5-6
access_drive <- compute_accessibility(
  origins = la_city_hh,
  destinations = foodpoi,
  mode = "CAR",
  chunk_size = calc_chunk_size(ram=7, mode="CAR"),
  cutoff=c(5, 10, 15, 20, 25),
  colnames = c("count"),
  progress=F,
  output_path=paste0(access_path, "/density/la_city", sep="/"),
  origin_type = "parcel", #should be parcel
  file_id=1
) #TODO rename this file and make sure the file names are never replaced this way...

# access_drive <- compute_accessibility(
#   origins = la_ctcent_dat,
#   destinations = foodmarket_merged,
#   mode = "CAR",
#   chunk_size = calc_chunk_size(ram=8, mode="CAR"),
#   cutoff=c(5, 10, 15, 20, 25),
#   colnames = c("count"),
#   progress=F,
#   output_path=paste0(access_path, "/density/la_city", sep="/"),
#   origin_type = "ct_cent"
# )

# access_drive <- compute_accessibility(
#   origins = la_ct_wtcent_dat,
#   destinations = foodmarket_merged,
#   mode = "CAR",
#   chunk_size = calc_chunk_size(ram=8, mode="CAR"),
#   cutoff=c(5, 10, 15, 20, 25),
#   colnames = c("count"),
#   progress=F,
#   output_path=paste0(access_path, "/density/la_city", sep="/"),
#   origin_type = "ct_wtcent"
# )


# check progress 
# print(base_path)
 access <- read.csv(paste0(processed_path, "LAC_accessibility/density/la_city/", "ct_cent_CAR20250321_1800_1.csv"))
 access$id <- as.numeric(access$id) 
 access <- access[!is.na(access$id),]
 offset <- nrow(access)/5
 
 # find all ids for index betwen split 1 and split 2 tht are not in access 
 sub <- la_city_hh[split[2]:split[6],][!(la_city_hh$id %in% access$id),]
 
 
 
 

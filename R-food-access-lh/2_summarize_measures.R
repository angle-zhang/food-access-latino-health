
source("0_Libraries.R")


# get all files with particular format and combine them into one file 
# combine all parcel data for driving times into one data frame
get_and_merge_files <- function(path, pattern){
  files <- list.files(path = path, pattern = pattern, full.names = TRUE)
  print(files)
  data <- lapply(files, read_csv)
  data <- rbindlist(data)
  data$id <- as.numeric(data$id) 
  data <- data[!is.na(data$id),]
  # remove nas
  return(data)
}


driving_times <- vector("list", 4)
names(driving_times) <- c("ct_cent", "ct_wtcent", "household", "household_agg")

density_path <- paste0(access_path, "/density/la_city")
# get ct data
# e.g. pattern ct_cent_CAR, parcel_CAR
driving_times$ct_cent <- get_and_merge_files(density_path, "ct_cent_CAR")
driving_times$ct_wtcent <- get_and_merge_files(density_path, "ct_wtcent_CAR")
driving_times$household <- get_and_merge_files(density_path, "parcel_CAR")


# use la_city_hh
# convert to wide with opportunity and cutoff merged as column name and accessibility as value
driving_times$household <- driving_times$household %>%
  select(-...1) %>%
  unique() %>%
  mutate(opportunity = paste0("driving_", opportunity), 
         accessibility = as.numeric(accessibility)) %>%
  pivot_wider(id_cols = "id", names_from = c("opportunity", "cutoff"), values_from = "accessibility") 

# merge by id col to la_city_hh and aggregate to census tract level
la_city_hhmeasures <- la_city_hh %>%
  left_join(driving_times$household, by = "id") %>%
  select(id, GEOID_20, UseDescription, starts_with("driving"))

# aggregate all columns with driving to census tract level
la_city_ctmeasures <- la_city_hhmeasures %>%
  select(-id, -UseDescription) %>%
  st_drop_geometry() %>%
  group_by(GEOID_20) %>%
  summarise_all(list(mean=mean, median=median)) %>%
  ungroup()

# gget la hh data

# merge la_city_hh GEOID and UseDescription data with driving_times$household by parcel id only 
# (since we are only interested in the household data)

# change ID to numeric, convert to wide format 
driving_times$household <- 
  
  
  
  merge(hh, la_city_hh, by = "id", all.x = TRUE)

# aggregate household data to census tract level
driving_times$household_agg <- driving_times$household %>%
  mutate(GEOID = as.numeric(GEOID)) %>%
  group_by(GEOID_20) %>%
  summarise_all(sum) %>%
  ungroup()

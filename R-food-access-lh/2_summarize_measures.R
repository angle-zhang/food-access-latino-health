
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

# create a function that can be used to process the data for both driving and walking times
# data must come in format 
process_times <- function(data, geoid_key, GEOID="GEOID_20", type, agg=T){
  data <- data %>%
    select(-...1) %>%
    unique() %>%
    mutate(opportunity = paste0(type, "_", opportunity), 
           accessibility = as.numeric(accessibility)) %>%
    pivot_wider(id_cols = "id", names_from = c("opportunity", "cutoff"), values_from = "accessibility") 
  
  print('data after pivot_wider')
  # merge by id col to la_city_hh and aggregate to census tract level
  geoid_joined <- geoid_key %>%
    left_join(data, by = "id") %>%
    select(id, all_of(c(GEOID)), UseDescription, starts_with(type))
  
  if (agg==T) { 
    # aggregate all columns with driving to census tract level
    ct_summary_meas <- geoid_joined %>%
      select(-id, -UseDescription) %>%
      st_drop_geometry() %>%
      group_by(.dots=GEOID) %>%
      summarise_all(list(mean=mean, median=median, range=range)) %>%
      ungroup()
    
    return(ct_summary_meas)
  }
  
  return(geoid_joined)
}

density_path <- paste0(access_path, "/density/la_city")

# get ct data
# e.g. pattern ct_cent_CAR, parcel_CAR
driving_times$ct_cent <- get_and_merge_files(density_path, "ct_cent_CAR")
driving_times$ct_wtcent <- get_and_merge_files(density_path, "ct_wtcent_CAR")
driving_times$household <- get_and_merge_files(density_path, "parcel_CAR")

# use la_city_hh
# convert to wide with opportunity and cutoff merged as column name and accessibility as value
driving_times$household_ct <- process_times(driving_times$household, la_city_hh, agg=T, type="driving")
head(driving_times$household_ct)
# get walking times
walking_times$ct_cent <- get_and_merge_files(density_path, "ct_cent_WALK")
walking_times$ct_wtcent <- get_and_merge_files(density_path, "ct_wtcent_WALK")
walking_times$household <- get_and_merge_files(density_path, "parcel_WALK")

# use la_city_hh
# convert to wide with opportunity and cutoff merged as column name and accessibility as value
walking_times$household_ct <- process_times(walking_times$household, la_city_hh, "walking")
# join ct data with walking times
walking_times$ct_cent <- process_times(walking_times$ct_cent, la_ctcent_dat, "walking", agg=F, GEOID="GEOID")


head(walking_times$household_ct) #inspect
head(walking_times$ct_wtcent) #inspect

# merge all walking times (ct_cent, ct_wtcent, household) by geoid
ct_walking <- walking_times$ct_cent %>%
  left_join(walking_times$ct_wtcent, by = "GEOID_20") %>%
  left_join(walking_times$household_ct, by = "GEOID_20") %>%
  mutate(UseDescription = "Walking") %>%
  select(-id)

head(walking_times$ct_wtcent) #inspect
head(walking_times$ct_cent) #inspect
  


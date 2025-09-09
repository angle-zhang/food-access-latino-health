
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
process_times <- function(data, geoid_key, GEOID="GEOID", type, scale, agg=F){
  
  data <- data %>%
    unique() %>%
    mutate(opportunity = paste0(type, "_", opportunity), 
           accessibility = as.numeric(accessibility),
           scale = scale) %>%
    pivot_wider(id_cols = "id", names_from = c("opportunity", "cutoff", "scale"), values_from = "accessibility") 
  
  geoid_joined <- geoid_key %>%
    left_join(data, by = "id") %>%
    mutate(GEOID := as.numeric(get(!!GEOID))) %>%
    select(id, GEOID, starts_with(type))
  
  print(head(geoid_joined))
  
  if (agg==T) { 
    # merge by id col to la_city_hh and aggregate to census tract level
    
    # aggregate all columns with driving to census tract level
    ct_summary_meas <- geoid_joined %>%
      select(-id) %>%
      st_drop_geometry() %>%
      group_by(GEOID) %>%
      summarise_all(list(mean=mean, median=median)) %>%
      ungroup()
    
    return(ct_summary_meas)
  }

  return(geoid_joined)
}

# get census tracts key for GEOID
la_ct <- read.csv(paste0(processed_path, "/LAC_origins/la_ct_key.csv")) %>%
  select(-X)

density_path <- paste0(access_path, "/density/la_city")
# get working directory 
# get ct data
# e.g. pattern ct_cent_CAR, parcel_CAR
driving_times$ct_cent <- get_and_merge_files(density_path, "ct_cent_CAR")
driving_times$ct_wtcent <- get_and_merge_files(density_path, "ct_wtcent_CAR")
driving_times$household <- get_and_merge_files(density_path, "parcel_CAR")
temp <- driving_times$household
head(driving_times$household)
head(la_city_hh)

# use la_city_hh
# convert to wide with opportunity and cutoff merged as column name and accessibility as value
driving_times$household_ct <- process_times(driving_times$household %>% select(-...1), 
                                            la_city_hh %>% st_drop_geometry(), GEOID="GEOID_20", 
                                            agg=T, scale="parcel", type="driving")
# join ct data with driving times
driving_times$ct_cent <- process_times(driving_times$ct_cent, la_ct, type="driving", scale="ct_cent", agg=F)
driving_times$ct_wtcent <- process_times(driving_times$ct_wtcent, la_ct, type="driving", scale="ct_wtcent", agg=F)

(driving_times$ct_cent) #inspect 
head(driving_times$household_ct)
head
temp <- driving_times$household_ct

# get geoids in la_hh
la_city_GEOID <- la_city_hh %>%
  st_drop_geometry() %>%
  select(GEOID_20) %>%
  unique() %>%
  mutate(GEOID_20 = as.numeric(GEOID_20))

# merge all
# driving times (ct_cent, ct_wtcent, household) by geoid
ct_driving <- driving_times$ct_cent %>%
  select(-id) %>%
  left_join(driving_times$ct_wtcent, by = "GEOID") %>%
  left_join(driving_times$household_ct, by="GEOID") %>%
  mutate(network_type = "Driving") %>%
  filter(GEOID %in% la_city_GEOID$GEOID_20) 

# write driving csv 
write_csv(ct_driving, paste0(processed_path, "LAC_cleaned/ct_driving_times.csv"))
# ----------- PROCESS AND WRITE WALK TIMES --------------- #
# get walking times
walking_times <- list()
walking_times$ct_cent <- get_and_merge_files(density_path, "ct_cent_WALK")
walking_times$ct_wtcent <- get_and_merge_files(density_path, "ct_wtcent_WALK")
walking_times$household <- get_and_merge_files(density_path, "parcel_WALK")

# use la_city_hh
# convert to wide with opportunity and cutoff merged as column name and accessibility as value
walking_times$household_ct <- process_times(walking_times$household %>% select(-...1), 
                                            la_city_hh %>% st_drop_geometry(), GEOID="GEOID_20", 
                                            agg=T, scale="parcel", type="walking")# join ct data with walking times

walking_times$ct_cent <- process_times(walking_times$ct_cent, la_ctcent_dat, "walking", scale="ct_cent", agg=F, GEOID="GEOID") %>%
  st_drop_geometry()
walking_times$ct_wtcent <- process_times(walking_times$ct_wtcent, la_ct_wtcent_dat, "walking", scale="ct_wtcent", agg=F, GEOID="GEOID") %>%
  st_drop_geometry()

head(walking_times$household_ct) #inspect
head(walking_times$ct_wtcent) #inspect

# merge all walking times (ct_cent, ct_wtcent, household) by geoid
ct_walking <- walking_times$ct_cent %>%
  left_join(walking_times$ct_wtcent, by = "GEOID") %>%
  left_join(walking_times$household_ct, by = "GEOID") %>%
  mutate(network_type = "Walking") 

write.csv(ct_walking, paste0(processed_path, "LAC_cleaned/ct_walking_times.csv"))

head(walking_times$ct_wtcent) #inspect
head(walking_times$ct_cent) #inspect
  


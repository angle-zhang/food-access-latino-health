source("0_Libraries.R")
library(tidytable)

la_ct <- get_census_tracts(proj_crs, state="CA", year=2020, county="Los Angeles")
la_hh <- get_lac_households(4326)
la_city <- get_city_boundary(proj_crs)
tm_shape(la_city) + tm_borders() # inspect city
head(la_hh)

la_city_ct <- la_ct %>%
  filter((lengths(st_intersects(., la_city)) > 0)) %>%
  #filter(ALAND > 0) %>%
  st_transform(4326)

# include households with census tract in la city
la_city_hh <- la_hh %>%
  filter(as.numeric(GEOID_20) %in% as.numeric(la_city_ct$GEOID)) 

head(la_city_hh)
# get all files with particular format and combine them into one file 
# combine all parcel data for driving times into one data frame
get_and_merge_files <- function(path, pattern){
  files <- list.files(path = path, pattern = pattern, full.names = TRUE) 
  print(files)
  print("Reading CSVs")
  data <- lapply(files, fread, drop=1) 
  #rnames <<- lapply(data, names) 
  print("binding data")
  data <- rbindlist(data, use.names=FALSE) 
  print("finishing up...")
 
 # print(problems(data))
  data$id <- as.numeric(data$id) 
  data <- data[!is.na(data$id),] #remove IDs that are nas due to their presence as col names 
  return(data)
}

# create a function that can be used to process the data for both driving and walking times
# data must come in format 
# agg = aggregate
process_times <- function(data, geoid_key, GEOID="GEOID", type, scale, agg=F){
  # print(head(data))
  # print(head(geoid_key))
  data <- data %>%
    unique() %>%
    mutate(id = as.numeric(id), 
           opportunity = paste0(type, "_", opportunity), 
           accessibility = as.numeric(accessibility),
           scale = scale) |>
    select(-percentile)
  
  print(head(data))
  data <- data |>
    as.data.table() |>
    dcast(id ~ opportunity + cutoff + scale, value.var="accessibility") |> 
    as.data.frame()
  
  names(data) <- sub(" ", ".", names(data))
  print(head(data))
  
  geoid_joined <- geoid_key %>%
    mutate(id = as.numeric(id)) |>
    left_join(data, by = "id") %>%
    mutate(GEOID := as.numeric(get(!!GEOID))) %>%
    select(id, GEOID, starts_with(type))
  
  #print(geoid_joined$GEOID)
  
  if (agg==TRUE) {
    # merge by id col to la_city_hh and aggregate to census tract level

    # aggregate all columns with driving to census tract level
    ct_summary_meas <- geoid_joined %>%
      select(-id) %>%
      st_drop_geometry() %>%
      group_by(GEOID) %>%
      summarise_all(list(
        mean   = ~mean(., na.rm = TRUE),
        median = ~median(., na.rm = TRUE),
        sd = ~sd(., na.rm = TRUE),
        cv = ~sd(., na.rm = TRUE) / mean(., na.rm = TRUE) * 100
      )) %>%
      ungroup()

     print(head(ct_summary_meas))
  
    return(ct_summary_meas)
  }

  return(geoid_joined)
}

# get census tracts key for GEOID
la_ct_key <- read.csv(paste0(processed_path, "/LAC_origins/la_ct_key.csv")) %>%
  select(-X)

density_path <- paste0(access_path, "/density/la_city/CATG")

# get working directory 
# get ct data
# e.g. pattern ct_cent_CAR, parcel_CAR
dt_ct_cent <- get_and_merge_files(density_path, "ct_cent_CAR")
dt_ct_wtcent <- get_and_merge_files(density_path, "ct_wtcent_CAR")
dt_household <- get_and_merge_files(density_path, "parcel_CAR")
head(dt_household)

sample <- sample(nrow(dt_household), 500)
dt_household <- dt_household[sample,]

# temp <- dt_household
head(dt_household)
head(la_city_hh)

# use la_city_hh
# convert to wide with opportunity and cutoff merged as column name and accessibility as value
dt_household_ct <- process_times(dt_household, la_city_hh %>% st_drop_geometry(), GEOID="GEOID_20", 
                                            agg=TRUE, scale="parcel", type="driving")
head(dt_household_ct)
# join ct data with driving times
dt_ct_centm <- process_times(dt_ct_cent %>% select(-row.names), la_ct_key, type="driving", scale="ct_cent", agg=F)
dt_ct_wtcentm <- process_times(dt_ct_wtcent %>% select(-row.names), la_ct_key, type="driving", scale="ct_wtcent", agg=F)

# temp <- dt_household %>% filter(is.na(id))
# temp <- head(dt_household_ct, 2000)

# get geoids in la_hh
la_city_GEOID <- la_city_hh %>%
  st_drop_geometry() %>%
  select(GEOID_20) %>%
  unique()%>%
  mutate(GEOID_20 = as.numeric(GEOID_20))
# 
# print(unique(dt_household_ct %>% select(GEOID)))
# 
# print(nrow(unique(la_city_hh %>% select(GEOID_20) %>% st_drop_geometry())))

usdafa <- openxlsx::read.xlsx(paste0(base_path, "./USDA_foodatlas/FoodAccessResearchAtlasData2019.xlsx"), sheet=3) 
usdafa_la <- usdafa |> 
  mutate(GEOID=as.numeric(CensusTract)) |> 
  filter(as.numeric(GEOID) %in% la_city_GEOID$GEOID_20) |>
  select(GEOID, starts_with("LA1"), LAhalfand10, starts_with("LAT")) 
  
unique(usdafa$CensusTract)

# merge all
# driving times (ct_cent, ct_wtcent, household) by geoid
ct_driving <- dt_ct_centm %>%
  select(-id) %>%
  left_join(dt_ct_wtcentm, by = "GEOID") %>%
  left_join(dt_household_ct, by="GEOID") %>%
  select(-id) %>%
  pivot_longer(!GEOID, names_to="features", values_to="count") %>%
  tidyr::separate_wider_delim(features, delim="_", names=c("network", "type", "drive", "pop_rep"), too_many="merge", too_few="align_start") |>
  pivot_wider(names_from=pop_rep, values_from=count) |>
  left_join(usdafa_la, by="GEOID") %>%
  filter(GEOID %in% la_city_GEOID$GEOID_20) 

head(dt_household)

#TODO calculate coefficient of variation 
#TODO Save merged datasets
parcel_driving1 <- dt_household |>
  process_times(la_city_hh, GEOID="GEOID_20",
                agg=FALSE, scale="parcel", type="driving") 

head(parcel_drivingdt)

sample <- sample(nrow(parcel_driving1), 200)
temp <- parcel_driving1[sample,]
parcel_drivingdt <- as.data.table(temp) |>
  melt(id.vars = c("GEOID", "id"),
       variable.name = "features",
       value.name = "count") |>
  tidytable::separate_wider_delim(features, delim="_", names=c("network", "type", "drive", "pop_rep"), too_many="merge", too_few="align_start") |>
  drop_na(count) # TODO check nas in parcel_drivingdt are due to missing values in original datasets

head(parcel_drivingdt)

 # pivot_longer(c(GEOID, id), names_to="features", values_to="count") |>
  tidytable::separate_wider_delim(features, delim="_", names=c("network", "type", "drive", "pop_rep"), too_many="merge", too_few="align_start") |>
  pivot_wider(names_from=pop_rep, values_from=count)

head(parcel_driving)

head(parcel_driving)

# temp <- as.numeric(setdiff(la_city_GEOID$GEOID_20, dt_ct_centm$GEOID)) # census tracts missing
# 
# t2 <- dt_ct_centm %>% filter(as.numeric(GEOID) %in% temp)

# write driving csv 
write_csv(ct_driving, paste0(processed_path, "LAC_cleaned/ct_driving_times.csv"))

# write parcel data csv 
write_csv(parcel_drivingdt, paste0(processed_path, "LAC_cleaned/parcel_driving.csv"))

# write aggregated parcel data 
write_csv(dt_household_ct, paste0(processed_path, "LAC_cleaned/dt_household_ct.csv"))
head(dt_household_ct)

temp <- read_csv(paste0(processed_path, "LAC_cleaned/parcel_driving.csv"))
# # ----------- PROCESS AND WRITE WALK TIMES --------------- #
# # get walking times
# walking_times <- list()
# walking_times$ct_cent <- get_and_merge_files(density_path, "ct_cent_WALK")
# walking_times$ct_wtcent <- get_and_merge_files(density_path, "ct_wtcent_WALK")
# walking_times$household <- get_and_merge_files(density_path, "parcel_WALK")
# 
# # use la_city_hh
# # convert to wide with opportunity and cutoff merged as column name and accessibility as value
# walking_times$household_ct <- process_times(walking_times$household %>% select(-...1), 
#                                             la_city_hh %>% st_drop_geometry(), GEOID="GEOID_20", 
#                                             agg=T, scale="parcel", type="walking")# join ct data with walking times
# 
# walking_times$ct_cent <- process_times(walking_times$ct_cent, la_ctcent_dat, "walking", scale="ct_cent", agg=F, GEOID="GEOID") %>%
#   st_drop_geometry()
# 
# walking_times$ct_wtcent <- process_times(walking_times$ct_wtcent, la_ct_wtcent_dat, "walking", scale="ct_wtcent", agg=F, GEOID="GEOID") %>%
#   st_drop_geometry()
# 
# head(walking_times$household_ct) #inspect
# head(walking_times$ct_wtcent) #inspect
# 
# # merge all walking times (ct_cent, ct_wtcent, household) by geoid
# ct_walking <- walking_times$ct_cent %>%
#   left_join(walking_times$ct_wtcent, by = "GEOID") %>%
#   left_join(walking_times$household_ct, by = "GEOID") %>%
#   mutate(network_type = "Walking") 
# 
# write.csv(ct_walking, paste0(processed_path, "LAC_cleaned/ct_walking_times.csv"))
# 
# head(walking_times$ct_wtcent) #inspect
# head(walking_times$ct_cent) #inspect


  


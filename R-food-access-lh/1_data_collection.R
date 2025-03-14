download_foodins_lacounty_ssi()
download_census_tracts()
download_census_blocks()

# ------ LOAD BOUNDARIES AND CRS ------ #
la_county <- get_county_boundary()
proj_crs = as.integer(suggest_crs(la_county)$crs_code[1])
la_ct <- get_census_tracts(proj_crs)
la_cb <- get_census_blocks(proj_crs)

# get geometry type of la_cb
print(unique(st_geometry_type(la_cb)))
unique(st_is_valid(la_cb, reason=T))


# ------ GET POPULATION WEIGHED CENTROID OF CTs ------ #
# gets centroid of census blocks then calculates the population weighted centroid of a census tract based on those
# for centroids that lie outside a census block, st_point_on_surface is used to get a point within the polygon
la_ctcent <- get_pop_weighted_centroid(la_cb, 'TRACTCE20', 'POP20') %>% 
  st_transform(proj_crs)

# merge ct centroids with rest of data
la_ctcent_dat <- la_ct %>%
  st_drop_geometry() %>%
  left_join(la_ctcent, by=c('TRACTCE'='TRACTCE20')) %>%
  st_as_sf()


unique(st_geometry_type(la_ctcent_dat))
st_crs(la_ctcent_dat)

# ------ LOAD SNAP POI DATA ------ #
# Load SNAP historical data for the year 2021
snap_historical <- get_snap_historical(years = 2021, proj_crs = st_crs(la_county))

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
  ungroup() #%>% 
  # TODO test
  #mutate(SIZE = str_extract(PE_DESCRIPTION, "\\d*-\\d*")) 


# sample_food <- foodinsp23_24_SSI[sample(nrow(foodinsp23_24_SSI), 300), ] 
# st_write(sample_food, "../data/sample-poi/sample_poi.gpkg", append=F)

# Display unique 'USER_PE_DESCRIPTION' values
unique_descriptions <- unique(foodinsp23_24_SSI$source)

print(names(foodinsp23_24_SSI))
print(unique_descriptions)


# ------ HEALTH OUTCOME DATA ------ #
CDCPlaces_dict <- get_CDCPlaces_dict()
places_vars <- get_CDCPlaces(geography='census', measure=c("DIABETES", "OBESITY", "FOODSTAMP", "FOODINSECU", "HOUSINSECU"), state="CA", geometry=T, release='2024') %>%
  filter(countyname == 'Los Angeles') 

unique(places_vars$countyname)


# ------ OSM DATA ------ #
source("0_Libraries.R")

# ------ LOAD AND CLEAN FOOD MARKET POI DATA ------ #
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
  select(OBJECTID, lon, lat, count, small, large, MATCH_ADDR, FACILITY_NAME, SOURCE)

# clean data axle market data from 2023
foodmarket_23_DA_clean <- foodmarket_23_DA %>%
  mutate(SOURCE="data_axle") %>%
  rename(FACILITY_NAME = COMPANY_NAME) %>%
  st_transform(4326) %>% # OSM data is in 4326
  mutate(lon = st_coordinates(.)[,1], lat = st_coordinates(.)[,2]) %>%
  mutate(count=1) %>%
  select(OBJECTID, lon, lat, count, MATCH_ADDR, FACILITY_NAME, SOURCE)
         
         
foodmarket_merged <- bind_rows(foodinsp23_24_SSIclean, foodmarket_23_DA_clean) %>%
  mutate(SOURCE_OBJECTID=OBJECTID, id=row_number())


# find all names with # followed by number #9357
foodmarket <- foodmarket_merged %>%
  filter(grepl("#\\d+", FACILITY_NAME)) 

length(unique(foodmarket_merged$FACILITY_NAME)) # get all unique names with # followed by number

# ------ CLEAN AND GET CHAIN NAMES ------ #
# TODO put this in a function
library(googlesheets4)

# if technical_desc contains AND, ignore for now
sic_codes <- read_sheet('https://docs.google.com/spreadsheets/d/1y7TxLRUXCcgd-T4_mGAXaAwAR7R00JxJDjJ9IhAucAA/edit?gid=0#gid=0') 

# split range if they exist 


split_range <- function(x) { 
  y <- c()
  if(length(x)==1) { 
    print("short")
    x=c(x)
  }
  
  for (el in x) { 
    t = str_split(el, "-") %>%
      unlist()

    # if(length(t) > 1) {
    #   t = c(t[1]:t[2])
    # }
    y = append(y, t)

  }

  return(y)
}

sic_codes_cl <- sic_codes %>%
  filter(!is.na(Extract)) %>% 
  mutate(keyword = ifelse(grepl(tolower("CONTAIN"), technical_desc), TRUE, FALSE)) %>%
  mutate(sic_code = ifelse(keyword, NA, str_extract_all(technical_desc, "[0-9]+-[0-9]+|\\d{3,}"))) %>%
  mutate(sic_code_name = ifelse(keyword, str_extract_all(technical_desc, "[0-9]+-[0-9]+|\\d{3,}"), NA)) %>%
  mutate(words_name = ifelse(keyword, str_extract_all(technical_desc, '“[A-z]+”'), NA)) 

# TODO just get these names
sic_codes_cl$sic_code_name_new <- lapply(sic_codes_cl$sic_code_name, split_range)
# sic_codes_cl$sic_code_new <- lapply(sic_codes_cl$sic_code, split_range)


# save sic_code column to csv
write.csv(unlist(sic_codes_cl$sic_code), paste0(processed_path, "sic_codes_cleaned.csv"), row.names = FALSE)
# get first four digits of each string and find unique in 
sic_list4 <- substr(unlist(sic_codes_cl$sic_code), 1, 4) %>%
  as.data.frame() %>%
  filter(!is.na(.)) %>%
  unique()
# write.csv(unlist(sic_codes_cl$sic_code_name), "sic_codes_cleaned.csv", append = T, row.names = FALSE)


# very large 
print(head(sic_codes_cl$sic_code))
# TODO add a column to the data frame with the keyword
# TODO populate SIC_code column with numbers if keyword == False

# ------ CLEAN NAMES AND CATEGORIZE FOOD MARKET POI DATA ------ #

# now remove all the numbers and # signs from the names
# remove trailing spaces
# repalce @ with AT
foodmarket_cleaned <- foodmarket_merged %>%
  mutate(FACILITY_NAME = trimws(FACILITY_NAME)) %>%
  mutate(FACILITY_NAME = str_replace_all(FACILITY_NAME, "#\\d+", "")) %>%
  mutate(FACILITY_NAME = str_replace_all(FACILITY_NAME, "# \\d+", "")) %>%
  mutate(FACILITY_NAME = gsub("@", "AT", FACILITY_NAME)) 

print(paste("Unique names found after removing numbers with # sign before them:",  
            length(unique(foodmarket_merged$FACILITY_NAME))- length(unique(foodmarket_cleaned$FACILITY_NAME)))) # get all unique names with # followed by number







# TODO identify columns in original data that may have info about owners (will give chain status if many of same owner)
# inspect unique names that have had #'s cleaned
foodmarket_names <- foodmarket %>%
  mutate(FACILITY_NAME = str_replace_all(FACILITY_NAME, "#\\d+", "")) 

unique(foodmarket_names$FACILITY_NAME) 


# download data from dataAxle to get chains
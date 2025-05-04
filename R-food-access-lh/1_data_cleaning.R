source("0_Libraries.R")

# ------ LOAD AND CLEAN FOOD MARKET POI DATA ------ #
# Load LA County food inspection data (2021-2024)
foodinsp_21_24 <- get_foodinsp_lacounty()

# Load LA County food inspection data (2023-2024) from SSI
food_23_24_SSI <- get_foodins_lacounty_ssi(proj_crs)
food_23_DA <- get_retail_food_LB_PAS(proj_crs)

# UNUSED CODE
# remove duplicates with facility ID, make sure to get the most recent status open vs closed
clean_food_data <- function(type="markets") {
  chains <- chains1
  food_23_24_SSIclean <- food_23_24_SSI %>% 
    mutate(SOURCE=factor(SOURCE, ordered=T, levels=c("Dec_2023", "March_2024","June_2024", "Dec_2024"))) %>%
    group_by(FACILITY_ID) %>% 
    filter(SOURCE == max(SOURCE)) %>% # get the one with largest value
    ungroup() %>%
    mutate(SOURCE="food_inspection") %>%
    mutate(
      small = ifelse(grepl("1-1,999", PE_DESCRIPTION), 1, 0),
      large = ifelse(grepl("2,000", PE_DESCRIPTION), 1, 0)
    ) %>%
    st_transform(4326) %>% # OSM data is in 4326
    mutate(lon = GEOCODE_LONGITUDE, lat = GEOCODE_LATITUDE) %>%
    mutate(count=1) %>%
    select(OBJECTID, lon, lat, count, small, large, MATCH_ADDR, FACILITY_NAME, SOURCE, TYPE) 

print(paste("Number of", type, "in food inspection:", nrow(food_23_24_SSIclean %>% filter(TYPE==type))))
  
  # clean data axle market data from 2023
  food_23_DA_clean <- food_23_DA %>%
    mutate(SOURCE="data_axle") %>%
    rename(FACILITY_NAME = COMPANY_NAME) %>%
    st_transform(4326) %>% # OSM data is in 4326
    mutate(lon = st_coordinates(.)[,1], lat = st_coordinates(.)[,2]) %>%
    mutate(count=1) %>%
    select(OBJECTID, lon, lat, count, MATCH_ADDR, FACILITY_NAME, SOURCE, TYPE)
  # print number of type in DA
  print(paste("Number of", type, "in data axle:", nrow(food_23_DA_clean %>% filter(TYPE==type))))
  
  food_merged <- bind_rows(food_23_24_SSIclean, food_23_DA_clean) %>%
    filter(TYPE==type) %>%
    mutate(SOURCE_OBJECTID=OBJECTID, id=row_number()) 
  
  return(food_merged)
}

markets <- clean_food_data(type="markets")
restaurants <- clean_food_data(type="res")

# TODO  cleaning names 
# find all names with # followed by number #9357

length(unique(foodmarket_merged$FACILITY_NAME)) # get all unique names with # followed by number

# ------ CLEAN AND GET CATEGORIES NAMES FROM HIRSCH ET AL., 2021 ------ #
  # TODO put this in a function
library(googlesheets4)

# if technical_desc contains AND, ignore for now
sic_codes <- read_sheet('https://docs.google.com/spreadsheets/d/1y7TxLRUXCcgd-T4_mGAXaAwAR7R00JxJDjJ9IhAucAA/edit?gid=0#gid=0') 

# split range if they exist 


split_range <- function(x) { 
  y <- c()
  if(length(x)==1) { 
        x=c(x)
  }
  
  for (el in x) { 
    t = str_split(el, "-") %>%
      unlist()
    if(length(t) > 1) {
      t = c(t[1]:t[2])
    }
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
head(sic_codes_cl$sic_code_name_new) 
# sic_codes_cl$sic_code_new <- lapply(sic_codes_cl$sic_code, split_range)

# save sic_code column to csv
write.csv(unlist(sic_codes_cl$sic_code), paste0(processed_path, "sic_codes_cleaned.csv"), row.names = FALSE)
# get first four digits of each string and find unique in 
sic_list4 <- substr(unlist(sic_codes_cl$sic_code), 1, 4) %>%
  as.data.frame() %>%
  filter(!is.na(.)) %>%
  unique() 

sic_listn4 <- substr(unlist(sic_codes_cl$sic_code_name_new), 1, 4) %>%
  as.data.frame() %>%
  filter(!is.na(.)) %>%
  unique() 


# write.csv(unlist(sic_codes_cl$sic_code_name), "sic_codes_cleaned.csv", append = T, row.names = FALSE)

# very large 
print(head(sic_codes_cl$sic_code))

# filter sic_codes by sic_list4 looking at first four digits of sic_code

# TODO add a column to the data frame with the keyword
# TODO populate SIC_code column with numbers if keyword == False


# TODO identify columns in original data that may have info about owners (will give chain status if many of same owner)
# inspect unique names that have had #'s cleaned
foodmarket_names <- foodmarket %>%
  mutate(FACILITY_NAME = str_replace_all(FACILITY_NAME, "#\\d+", "")) 

unique(foodmarket_names$FACILITY_NAME) 


#'* GET AND CLEAN DATA AXLE DATA FOR ANALYSIS *---------------------------------------------------

# download data from dataAxle to get chains
poi_da <- get_data_axle(year=2022, state="CA") %>%
  filter(!is.na(COMPANY) & !is.na(PRIMARY.SIC.CODE)) 

names(poi_da)

# TODO clean data axle data and assign sic codes 

# simple strat: > 5 stores
# get all chains and get attach the primary sic code with most number of that specific store to all instances of that chain 

# %>%






# ------ CLEAN NAMES AND CATEGORIZE FOOD MARKET POI DATA ------ #
# TODO put this in a function
# now remove all the numbers and # signs from the names
# remove trailing spaces
# repalce @ with AT
foodmarket_cleaned <- foodmarket_merged %>%
  mutate(FACILITY_NAME = trimws(FACILITY_NAME)) %>%
  mutate(FACILITY_NAME = str_replace_all(FACILITY_NAME, "#\\d+", "")) %>%
  mutate(FACILITY_NAME = str_replace_all(FACILITY_NAME, "# \\d+", "")) %>%
  mutate(FACILITY_NAME = gsub("@", "AT", FACILITY_NAME)) %>%
  mutate(FACILITY_NAME = trimws(FACILITY_NAME)) 
# remove trailing spaces

# merge with chain names
foodmarket_cleaned1 <- foodmarket_cleaned %>%
  left_join(chains2, by=c("FACILITY_NAME"="COMPANY")) %>%
  mutate(chain = ifelse(sic_codes!="NULL", TRUE, FALSE)) 

length(unique(chains2$COMPANY))
nrow(foodmarket_cleaned1 %>% filter(chain))

temp <- chains1 %>%
  count(COMPANY)

print(paste("Unique names found after removing numbers with # sign before them:",  
            length(unique(foodmarket_merged$FACILITY_NAME))- length(unique(foodmarket_cleaned$FACILITY_NAME)))) # get all unique names with # followed by number

#'* DON'T DELETE METHOD FOR OBTAINING CHAINS AND ASSOCIATED SIC CODES *---------------------------------------------------
chains <- poi_da %>%
  group_by(COMPANY) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  filter(count>=5)

## for each name, get the most common primary sic code
chains_sic <- poi_da %>%
  filter(COMPANY %in% chains$COMPANY) %>%
  group_by(COMPANY, PRIMARY.SIC.CODE) %>%
  summarize(count=n()) %>%
  slice_max(count)

chains1 <- chains %>% merge(chains_sic, by="COMPANY") %>%
  select(COMPANY, PRIMARY.SIC.CODE, count.x) %>%
  mutate(sic4 = substr(PRIMARY.SIC.CODE, 1, 4)) %>%
  filter(sic4 %in% sic_list4$.) %>% # filter for the ones we are interested in
  group_by(COMPANY) %>%
  summarize(sic_codes=list(PRIMARY.SIC.CODE)) 

# put word market after SIC codes starting with 5411 unless they already have "market" in the
chains2 <- chains1 %>%
  mutate(COMPANY=ifelse(grepl("MARKETPLACE", COMPANY), gsub(" MARKETPLACE", "", COMPANY), COMPANY)) %>%
  # TODO do something about costco
  rowwise() %>%
  mutate(
    COMPANY = if (any(str_starts(as.character(sic_codes), "5411")) && !str_detect(str_to_lower(COMPANY), "market")) 
      paste0(COMPANY, " MARKET") 
    else COMPANY
  ) %>%
  ungroup() %>%
  rbind(chains1) %>%
  unique()


# TODO complex strat: 


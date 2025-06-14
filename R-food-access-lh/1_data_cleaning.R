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

#
poi_da <- get_data_axle(year=2022, state="CA") %>%
  filter(!is.na(COMPANY) & !is.na(PRIMARY.SIC.CODE))



#'* NOT USING THIS: MERGING DA WITH SIC CODE RANGES*---------------------------------------------------


# filter sic_codes by sic_list4 looking at first four digits of sic_code

# TODO add a column to the data frame with the keyword
# TODO populate SIC_code column with numbers if keyword == False


# TODO identify columns in original data that may have info about owners (will give chain status if many of same owner)

## if technical_desc contains AND, ignore for now
# sic_codes <- read_sheet('https://docs.google.com/spreadsheets/d/1y7TxLRUXCcgd-T4_mGAXaAwAR7R00JxJDjJ9IhAucAA/edit?gid=0#gid=0') 
# 
# # split range if they exist 
# 
# split_range <- function(x) { 
#   y <- c()
#   if(length(x)==1) { 
#     x=c(x)
#   }
#   
#   for (el in x) { 
#     # do not use these for now
#     # t = str_split(el, "-") %>%
#     #   unlist()
#     # 
#     # # if there is a range, create new column
#     # if(length(t) > 1) {
#     #   t = c(t[1]:t[2])
#     # }
#     y = append(y, t)
#   }
#   
#   return(y)
# }
# 
# # for removing nested parentheses
# remove_nested_parens <- function(x) {
#   # Handles character vectors
#   x <- as.character(x)
#   repeat {
#     new_x <- str_remove_all(x, "\\([^()]*\\)")
#     if (identical(new_x, x)) break
#     x <- new_x
#   }
#   return(x)
# }
# 
# sic_codes_cl <- sic_codes %>%
#   filter(!is.na(Extract)) %>% 
#   mutate(keyword = ifelse(grepl(tolower("CONTAIN"), technical_desc), TRUE, FALSE)) %>%
#   mutate(technical_desc_new = remove_nested_parens(technical_desc)) %>% # remove all text in parentheses
#   mutate(sic_code = ifelse(keyword, NA, str_extract_all(technical_desc_new, "[0-9]+-[0-9]+|\\d{3,}"))) %>%
#   mutate(sic_code_name = ifelse(keyword, str_extract_all(technical_desc_new, "[0-9]+-[0-9]+|\\d{3,}"), NA)) %>%
#   mutate(words_name = ifelse(keyword, str_extract_all(technical_desc_new, '“[A-z]+”'), NA)) %>%
#   unnest(words_name) %>%
#   mutate(words_name = ifelse(keyword, gsub("“|”", "", words_name), NA)) %>%
#   gather(key="type", value="sic_codes_all", sic_code, sic_code_name) %>% 
#   unnest(sic_codes_all) %>% # turn each vector of sic codes into a row
#   filter((!is.na(sic_codes_all)) & (str_length(sic_codes_all) > 4)) %>%
#   mutate(is_range = str_detect(sic_codes_all, "-")) %>% # detect if it's a range
#   mutate(
#     start = if_else(is_range, str_extract(sic_codes_all, "^[0-9]+"), sic_codes_all),
#     end = if_else(is_range, str_extract(sic_codes_all, "(?<=-)[0-9]+"), sic_codes_all)
#   ) %>% 
#   mutate(
#     start = str_extract(start, "^\\d{1,6}"), #truncate to 6 digit sic code
#     end = str_extract(end, "^\\d{1,6}"),
#     range_str = paste0(start, "-", end),
#     start = as.numeric(start),
#     end = as.numeric(end)
#   ) %>%
#   select(start, end, range_str, keyword, words_name, "3-LTR") 
# 
# 
# t1 <- head(sic_codes_cl,1000)
# # TODO just get these names
# # sic_codes_cl$sic_code_name_new <- lapply(sic_codes_cl$sic_code_name, split_range)
# # head(sic_codes_cl$sic_code_name) 
# # sic_codes_cl$sic_code_new <- lapply(sic_codes_cl$sic_code, split_range)
# 
# # save sic_code column to csv
# write.csv(sic_codes_cl, paste0(processed_path, "sic_codes_cleaned.csv"), row.names = FALSE)
# # # get first four digits of each string and find unique in 
# # sic_list4 <- substr(unlist(sic_codes_cl$sic_code), 1, 4) %>%
# #   as.data.frame() %>%
# #   filter(!is.na(.)) %>%
# #   unique() 
# sic_codes_cl <- read.csv(paste0(processed_path, "sic_codes_cleaned.csv"))
# 
# sic_dt <- as.data.table(sic_codes_cl)
# 
# # download data from dataAxle to get chains
# poi_da <- get_data_axle(year=2022, state="CA") %>%
#   filter(!is.na(COMPANY) & !is.na(PRIMARY.SIC.CODE)) 
# 
# names(poi_da)
# head(poi_da)
# 
# # TODO clean data axle data and assign sic codes 
# # gather all sic codes 
# # get all names wiht SIC
# sic_code_cols <- names(poi_da)[grepl("SIC\\.CODE", names(poi_da))] # get all column names with SIC)
# print(sic_code_cols)
# 
# poida_cleaned <- poi_da %>% # create one row per sic code in poi data 
#   gather(key="sic_names", value="SIC.CODE", all_of(sic_code_cols)) %>%
#   mutate(SIC.CODE = as.numeric(SIC.CODE)) %>%
#   as.data.table()
# 
# head(poida_cleaned)
# # find SIC codes in intervals defined
# 
# #temp <- foverlaps(sic_dt, poida_cleaned, by.x = c("start", "end"), type = "within")
# 
# t1 <- head(poida_cleaned, 400)
# temp <- sic_dt[poida_cleaned, on = .(start <= SIC.CODE, end >= SIC.CODE), nomatch = 0]# select variabbles in poida_cleaned between sic code
# t <- head(temp, 400)
# 
# # if keyword is true, then check if name is in company name
# # TODO warning with grepl
# temp1 <- temp[keyword==FALSE | (keyword==TRUE & str_detect(COMPANY, regex(words_name, ignore_case = TRUE)))]
# 
# t3 <- temp1[X3.LTR=="WRS"] 
# 
# t2 <- head(temp1, 40000)
# 
# #temp1[, c("keyword", "words_name", "start", "end", "range_str") := NULL]
# temp1[, dummy:=1] # create dummy variable to use in dcast
# 
# temp2 <- dcast(temp1, ...1 + COMPANY + ADDRESS.LINE.1 + CITY + ZIPCODE + ZIP4~ X3.LTR, value.var="dummy", fill=0) # summarize to wide format with new columns representing food POI categories



# TODO NEXT STEP - deal with columns that have exceptions (e.g. EAT) 5/29/25
# can use other codes e.g. BKS EAO, EAP, EEU, etc.


#'* 3LTR codes for restaurants that are not fast food, pizza, coffee shops, or bakeries* 

# EAO	Ethnic Restaurants – Other Asian
# EAP	Ethnic Restaurants – Popular Asian
# EAT	Other restaurants / eating places
# EEP	Ethnic Restaurants – Popular Ethnic
# EEU	Ethnic Restaurants – European

# regeocode data 


# filter for keywords 

#head(unique(temp1$words_name))
  #%>%
  # rowwise() %>%
  # mutate(
  #   matching_range = list(sic_codes_cl %>% filter(SIC.CODE >= start & SIC.CODE <= end) %>% select("3-LTR"))
  # ) %>%
  # unnest(matching_range, keep_empty = TRUE)# %>%
  # select(SIC.CODE, range_start = start, range_end = end, range_str)



#head(poida_cleaned)

# filter by sic_codes_cl


#head(poida_cleaned)

# TODO geocode data


# simple strat: > 5 stores
# get all chains and get attach the primary sic code with most number of that specific store to all instances of that chain 

# %>%






# ------ CLEAN NAMES AND CATEGORIZE FOOD MARKET, FOOD INSPECTION POI DATA ------ #
# TODO put this in a function
# now remove all the numbers and # signs from the names
# remove trailing spaces

# inspect unique names that have had #'s cleaned
foodmarket_names <- foodmarket %>%
  mutate(FACILITY_NAME = str_replace_all(FACILITY_NAME, "#\\d+", "")) 

unique(foodmarket_names$FACILITY_NAME) 
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
  fuzzyjoin::stringdist_left_join(chains2, by=c("FACILITY_NAME"="COMPANY"), max_dist=1) %>%
  mutate(chain = ifelse(sic_codes!="NULL", TRUE, FALSE)) 

# TODO remove duplicates?

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


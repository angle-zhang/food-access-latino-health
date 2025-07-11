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

naics <- read_sheet('https://docs.google.com/spreadsheets/d/1y7TxLRUXCcgd-T4_mGAXaAwAR7R00JxJDjJ9IhAucAA/edit?gid=0#gid=0') 

# TODO make key to  reassign chains to the same NAICS code

code_cols <- names(poi_da)[grepl("NAICS\\.CODE", names(poi_da))] # get all column names with SIC)
print(sic_code_cols)

poida_cleaned <- poi_da %>% # create one row per sic code in poi data
  mutate(NAICS.CODE.trunc = as.numeric(str_extract(PRIMARY.NAICS.CODE, "^\\d{1,6}"))) %>%
  as.data.table()

naics_dt <- as.data.table(naics)

temp <- naics_dt[poida_cleaned, on = .(code == NAICS.CODE.trunc), nomatch = 0] # select variabbles in poida_cleaned between sic code
temp[, dummy:=1]
temp2 <- dcast(temp, ...1 + COMPANY + ADDRESS.LINE.1 + CITY + ZIPCODE + ZIP4 ~ `zhang-2025`, value.var="dummy", fill=0) # summarize to wide format with new columns representing food POI categories

foodpoi <- temp %>%
  as.data.frame() %>%
  select(-c(6:8)) 

# write foodpoi to file
write.csv(foodpoi, paste0(processed_path, "foodpoi.csv"), row.names = FALSE)

#'*sensitivity analysis: inspect to see if chains are consistently coded *
# find that the chains are consistently coded 
# 697 rows for NAICS and name pairs that had a count >5
# only 1 inconsistency was found "albertson's delicatssen" was coded as restaurant and supermarket
#' # kept as is since there may be reason for coding this way
#' chains <- temp[, .(count = .N), by = .(`zhang-2025`, code, COMPANY)][count>5]
#' 
#' 
#' 
#' # TODO clip to a buffer
#' 
#' 
#' foodmarket_names <- foodmarket %>%
#'   mutate(FACILITY_NAME = str_replace_all(FACILITY_NAME, "#\\d+", "")) 
#' 
#' unique(foodmarket_names$FACILITY_NAME) 
#' # repalce @ with AT
#' foodmarket_cleaned <- foodmarket_merged %>%
#'   mutate(FACILITY_NAME = trimws(FACILITY_NAME)) %>%
#'   mutate(FACILITY_NAME = str_replace_all(FACILITY_NAME, "#\\d+", "")) %>%
#'   mutate(FACILITY_NAME = str_replace_all(FACILITY_NAME, "# \\d+", "")) %>%
#'   mutate(FACILITY_NAME = gsub("@", "AT", FACILITY_NAME)) %>%
#'   mutate(FACILITY_NAME = trimws(FACILITY_NAME)) 
#' # remove trailing spaces
#' 
#' # merge with chain names
#' foodmarket_cleaned1 <- foodmarket_cleaned %>%
#'   fuzzyjoin::stringdist_left_join(chains2, by=c("FACILITY_NAME"="COMPANY"), max_dist=1) %>%
#'   mutate(chain = ifelse(sic_codes!="NULL", TRUE, FALSE)) 
#' 
#' # TODO remove duplicates?
#' 
#' length(unique(chains2$COMPANY))
#' nrow(foodmarket_cleaned1 %>% filter(chain))
#' 
#' temp <- chains1 %>%
#'   count(COMPANY)
#' 
#' print(paste("Unique names found after removing numbers with # sign before them:",  
#'             length(unique(foodmarket_merged$FACILITY_NAME))- length(unique(foodmarket_cleaned$FACILITY_NAME)))) # get all unique names with # followed by number
#' 
#' #'* DON'T DELETE METHOD FOR OBTAINING CHAINS AND ASSOCIATED SIC CODES *---------------------------------------------------
#' chains <- poi_da %>%
#'   group_by(COMPANY) %>%
#'   summarise(count = n()) %>%
#'   arrange(desc(count)) %>%
#'   filter(count>=5)
#' 
#' ## for each name, get the most common primary sic code
#' chains_sic <- poi_da %>%
#'   filter(COMPANY %in% chains$COMPANY) %>%
#'   group_by(COMPANY, PRIMARY.SIC.CODE) %>%
#'   summarize(count=n()) %>%
#'   slice_max(count)
#' 
#' chains1 <- chains %>% merge(chains_sic, by="COMPANY") %>%
#'   select(COMPANY, PRIMARY.SIC.CODE, count.x) %>%
#'   mutate(sic4 = substr(PRIMARY.SIC.CODE, 1, 4)) %>%
#'   filter(sic4 %in% sic_list4$.) %>% # filter for the ones we are interested in
#'   group_by(COMPANY) %>%
#'   summarize(sic_codes=list(PRIMARY.SIC.CODE)) 
#' 
#' # put word market after SIC codes starting with 5411 unless they already have "market" in the
#' chains2 <- chains1 %>%
#'   mutate(COMPANY=ifelse(grepl("MARKETPLACE", COMPANY), gsub(" MARKETPLACE", "", COMPANY), COMPANY)) %>%
#'   # TODO do something about costco
#'   rowwise() %>%
#'   mutate(
#'     COMPANY = if (any(str_starts(as.character(sic_codes), "5411")) && !str_detect(str_to_lower(COMPANY), "market")) 
#'       paste0(COMPANY, " MARKET") 
#'     else COMPANY
#'   ) %>%
#'   ungroup() %>%
#'   rbind(chains1) %>%
#'   unique()
#' 
#' 
#' # TODO complex strat: 



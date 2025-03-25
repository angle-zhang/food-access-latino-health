
          
library(tidyverse)
library(CDCPLACES)
library(tmap)
library(crsuggest)
library(sf)
library(tigris)
library(dplyr)
library(osmextract)



# 1
library(centr)

options(java.parameters = "-Xmx8G")
library(r5r)

base_path <- "../../0_shared-data/raw/"
processed_path <- "../../0_shared-data/processed/"

source('./helper/universal_variables.R')
source('./helper/data_functions.R')
source('../../0_helper-functions/get-food-data.R')
source('../../0_helper-functions/get-la-county-admin-data.R')
source('../../0_helper-functions/get-health-data.R')

proj_crs = as.integer(suggest_crs(get_county_boundary())$crs_code[1])

#library(reticulate)
#py_run_file('C:/Users/angie/OneDrive/Desktop/data-analysis/0_helper-functions/get_osm_data.py')


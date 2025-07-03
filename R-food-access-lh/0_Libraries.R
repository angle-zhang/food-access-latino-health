
          
library(tidyverse)
library(CDCPLACES)
library(tmap)
library(crsuggest)
library(sf)
library(tigris)
library(dplyr)
library(osmextract)
library(data.table)
#library(osmdata)



# 1
library(centr)

# run once - only for SSI computers
library(rJavaEnv)
# java_quick_install(version = 21)
Sys.setenv(JAVA_HOME="C:\\Users\\lab.DTS-MJ0LQJJJ\\AppData\\Local//R//cache//R//rJavaEnv//installed//windows//x64//21")
java_check_version_rjava()
options(java.parameters = "-Xmx40G")
library(r5r)

base_path <- "D:/data-analysis/0_shared-data/food-environment-measures/raw/"
processed_path <- "D:/data-analysis/0_shared-data/food-environment-measures/processed/"
access_path <- paste0(processed_path, "LAC_accessibility")

source('./helper/universal_variables.R')
source('./helper/data_functions.R')
source('../../0_helper-functions/get-food-data.R')
source('../../0_helper-functions/get-la-county-admin-data.R')
source('../../0_helper-functions/get-health-data.R')

proj_crs = as.integer(suggest_crs(get_county_boundary())$crs_code[1])

library(googlesheets4)
gs4_auth()

#library(reticulate)
#py_run_file('C:/Users/angie/OneDrive/Desktop/data-analysis/0_helper-functions/get_osm_data.py')


#### Preamble ####
# Purpose: Downloads and saves the data from Open Data Toronto about Toronto Public Library annual number of visits
# Author: Christina Nguyen
# Date: 24 September 2024
# Contact: christinadinh.nguyen@utoronto.ca
# License: MIT
# Pre-requisites: none
# Any other information needed? none


#### Workspace setup ####
library(opendatatoronto)
library(tidyverse)


#### Download data and resources ####
package <- show_package("932efbb4-644f-401b-9f59-af0b31713f44")
resources <- list_package_resources("932efbb4-644f-401b-9f59-af0b31713f44")

# identify datastore resources; by default, Toronto Open Data sets datastore resource format to CSV for non-geospatial and GeoJSON for geospatial resources
datastore_resources <- filter(resources, tolower(format) %in% c('csv', 'geojson'))

# load the first datastore resource as a sample
raw_data <- filter(datastore_resources, row_number()==1) %>% get_resource()
raw_data #This printed data correctly in the console.

#### Save data ####
write_csv(raw_data, "data/raw_data/raw_data.csv") 

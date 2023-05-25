# Load required libraries
library(auk)
library(lubridate)
library(sf)
library(gridExtra)
library(tidyverse)
library(ggplot2)
library(raster)
library(data.table)
library(readr)
library(stringr)

#resolve namespace conflicts
select <- dplyr::select

#read in western ghats shapefile for spatial filtering
wg <- st_read("data/WG.shp")

#specify input and output files
f_in <- "data/ebd_IN_relNov-2022.txt" 
f_out <- "data/ebd-filtered-WG.txt"

#define filters
ebird_filters <- f_in %>%
  #reference file
  auk_ebd() %>% 
  #spatial filter
  auk_bbox(wg) %>%
  #filtering for 8 years of data with 3 full seasons per year, filtering till 2021 to match TeraClime data
  auk_date(date = c("2013-03-01", "2021-02-28")) %>% 
  #protocol filter
  auk_protocol(protocol = c("Stationary", "Traveling", "Area")) %>% 
  #effort distance filter
  auk_distance(distance = c(0, 1)) %>% 
  #duration filter
  auk_duration(duration = c(0, 300)) %>% 
  #include only complete checklists where observers reported all species identified
  auk_complete()
#check filters
ebird_filters 

#run filters
auk_filter(ebird_filters, file = f_out)

#read in filtered file
ebird_data <- read_ebd("data/ebd-filtered-WG.txt")

##sub-setting the ebird data to include only passerine birds
#read in the eBird taxonomy file
taxonomy <- read.csv("data/ebird_taxonomy_v2022.csv")

#clean capitalization and column names for merging
new_col_names <- colnames(taxonomy) %>%
  str_replace("SCI_NAME", "scientific_name") %>% 
  str_to_lower()
setnames(taxonomy, new_col_names)
 
#merge with ebird data
ebird_data <- merge(ebird_data, taxonomy, by = "scientific_name")

#subset to include only passerine birds
ebird_data_passer <- subset(ebird_data, order1 == "Passeriformes")

##additional filtering
ebird_data_filt <- ebird_data_passer %>% 
  #keeping only approved observations
  filter(reviewed == 0 | approved == 1) %>% 
  #keep only single sampling id
  mutate(sampling_id = ifelse(is.na(group_identifier), 
                              sampling_event_identifier, 
                              group_identifier)) %>% 
  #include checklists with only less than 10 observers
  filter(number_observers <= 10) %>%
  #removing repeats
  group_by(sampling_id,scientific_name) %>% 
  slice(1) %>% 
  ungroup %>%
  #add number of species column
  group_by(sampling_id) %>% 
  mutate(no_sp = n_distinct(scientific_name)) %>% 
  #adding year month day column
  mutate(date = as_date(observation_date),
         year = year(observation_date),
         month = month(observation_date),
         day = day(observation_date),
         DoY = yday(observation_date)) %>% 
  #removing observation count X = 1
  mutate(count = ifelse(observation_count == "X", "1",
                        observation_count)) %>% 
  ungroup()

#select useful columns
ebird <- ebird_data_filt %>% 
  select(scientific_name, common_name, sampling_id, date, 
         year, month, day, DoY, category.x, state, state_code,
         locality_id, latitude, longitude, time_observations_started,
         protocol_type, duration_minutes, effort_distance_km, effort_area_ha,
         no_sp, count)

##cleaning up variables

#changing effort distance to 0 for stationary checklists and converting to meters
dat_ebird <- ebird %>% 
  mutate(effort_distance = if_else(protocol_type == "Stationary", 
                                    0, effort_distance_km))
dat_ebird$effort_distance <- dat_ebird$effort_distance*1000

#changing column name category.x to category
names(dat_ebird)[names(dat_ebird) == "category.x"] <- "category"

##adding elevation column
#read in ASTER GDEM tiff file
aster <- raster("data/aster-gdem-wg.tiff")

#create an object storing unique locations of all checklists in the filtered eBird data
locations <- dat_ebird %>%
  distinct(latitude, longitude, .keep_all = T) %>% 
  dplyr::select(locality_id, latitude, longitude)

#create a spatial data frame from filtered eBird data
ebird_elev <- st_as_sf(dat_ebird, coords = c("longitude", "latitude"), 
                       crs = 4326, remove = "F")

#extract elevation from ASTER GDEM 
dat_elev <- raster::extract(aster, ebird_elev)

#bind elevation with ebird data
ebird_elevation <- cbind(dat_ebird, dat_elev)

#rename elevation column
names(ebird_elevation)[names(ebird_elevation) == "dat_elev"] <- "elev"

##adding a season column
seasons = function(x){
  if(x %in% 3:5) return("summer")
  if(x %in% 6:10) return("monsoon")
  if(x %in% c(11,12,1,2)) return("winter")
  
}
ebird_elevation$season <- sapply(ebird_elevation$month, seasons)

##creating final data frame for analysis
dat <- ebird_elevation %>% 
  select(date, year, month, day, DoY, scientific_name, common_name, category, count, 
         sampling_id, state, state_code, locality_id, latitude, longitude, no_sp, 
         protocol_type, time_observations_started, duration_minutes, effort_distance, 
         elev, season)

#write a csv file with final data frame and unique locations 
write.csv(dat, file = "data/ebird-elev-WG.csv", row.names = FALSE)
write.csv(locations, file = "data/unique-chklst-locations-WG.csv", row.names = FALSE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Data Summary
#Coder: Nate Jones (cnjones7@ua.edu)
#Date: 11/28/2020
#Purpose: Create leaflet map of available data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Helpful link: http://ryanpeek.org/2017-11-21-mapping-with-sf-part-3/

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.0 Setup workspace -----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Clear workspace
remove(list = ls())

#Load relevant packages
library(tidyverse)
library(sf)
library(mapview)
library(htmlwidgets)

#create temp workspace
file.create("data/temp")

# Data summaries obtained from Michelle
full<-read_csv('data/data_summary/full_data_summary.csv')
bmi<-read_csv("data/data_summary/BMI_Stations_FULL_summary.csv")
logger<-read_csv('data/data_summary/logger_sites_summary.csv')

#Define master projection
p<-"+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Prep Spatial Data----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.1 Convert sites to spatial layer 
full<-full %>% 
  mutate(masterid = paste0(masterid)) %>% 
  select(-stationname) %>% 
  st_as_sf(
    coords = c('TargetLongitude', 'TargetLatitude'), 
    crs = st_crs("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")) %>% 
  #Reproject
  st_transform(., crs=st_crs(p))

logger<-logger %>% 
  filter(!is.na(logger)) %>% 
  filter(logger>0) %>% 
  st_as_sf(
    coords = c('TargetLongitude', 'TargetLatitude'), 
    crs = st_crs("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")) %>% 
    #Reproject
    st_transform(., crs=st_crs(p))

bmi<-bmi %>% 
  filter(!is.na(num_ipi)) %>% 
  filter(num_ipi>0) %>% 
  st_as_sf(
    coords = c('TargetLongitude', 'TargetLatitude'), 
    crs = st_crs("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")) %>% 
  #Reproject
  st_transform(., crs=st_crs(p))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.0 Mapping -------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Convert to coordinate system
all_sites    <- full   %>% st_transform(., crs=4326)
logger_sites <- logger %>% st_transform(., crs=4326)
bmi_sites    <- bmi    %>% st_transform(., crs=4326)

#Add mapview
m<-mapview(all_sites,col.regions="grey30", alpha.regions=0.9, cex=0.1) +
  mapview(logger_sites, zcol='logger') + 
  mapview(bmi_sites, zcol='num_bmi_samples')

#export map
setwd("docs/")
mapshot(m, "data_summary.html")

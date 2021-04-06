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
library(readxl)
library(sf)
library(mapview)
library(htmlwidgets)

# Data summaries obtained from Michelle
long_record<-read_xlsx("download/Temporal Data Summary_Key Sites.xlsx", skip=1, n_max=14)
short_record<-read_xlsx("download/Temporal Data Summary_Key Sites.xlsx", skip=19)
  
#Dist to gage
gage<-read_csv("docs/ca_sites_riv_dist_to_gage.csv") %>% rename(StationCode=site_id)

#Merge riv dist
long_record<-left_join(long_record, gage)
short_record<-left_join(short_record, gage)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Prep Spatial Data----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.1 Convert sites to spatial layer 
long_record<-long_record %>% 
  st_as_sf(
    coords = c('Longitude', 'Latitude'), 
    crs = st_crs("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")) %>% 
  st_transform(., crs=4326)

short_record<-short_record %>% 
  st_as_sf(
    coords = c('Longitude', 'Latitude'), 
    crs = st_crs("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")) %>% 
  st_transform(., crs=4326)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.0 Mapping -------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Add mapview
m<-mapview(short_record,col.regions="dark blue", alpha.regions=0.9) +
  mapview(long_record,col.regions="dark orange", alpha.regions=0.9)

#export map
setwd("docs/")
mapshot(m, "data_summary.html")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Initial Map
#Coder: Nate Jones (cnjones7@ua.edu)
#Date: 11/20/2020
#Purpose: Exploratory Analysis  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.0 Setup workspace -----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Clear workspace
remove(list = ls())

#Load relevant packages
library(tidyverse)
library(readxl)
library(sf)
library(raster)
library(leaflet)
library(xts)
library(htmlwidgets)

#download files of interest
sites<-read_xlsx("data/sites_CA_ALL_Stations.xlsx")
gages<-st_read('data/spatial_data/gagesII/gagesII_9322_sept30_2011.shp')

#Define master projection
p<-'+proj=utm +zone=11 +ellps=GRS80 +datum=NAD83 +units=m +no_defs '

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Overlay Analysis ----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.1 Convert Sites to spatial layer ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Convert to spatial object
sites<-st_as_sf(
  sites, 
  coords = c('TargetLongitude', 'TargetLatitude'), 
  crs = st_crs("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"))

#Reproject
sites<-st_transform(sites, crs=st_crs(p))

#2.2 Reproject gages and subset to CA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
gages<-gages %>% 
  filter(STATE=='CA') %>% 
  st_transform(., crs=st_crs(p))

#2.3 Creat 1 km buffer around gages and overlay~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Create 1km buffer around gages
gages_buffer<-st_buffer(gages, dist = 1000)

#define sites within buffer area
sites_near_gages<-sites[gages_buffer,]

#Limit gages to gages that intersect within 1000 meters of pnt
sites_near_gages_buffer<-st_buffer(sites_near_gages, dist=1000)
gages<-gages[sites_near_gages_buffer,]

#Plot for funzies
sites_near_gages %>% st_geometry() %>% plot(., pch=19, col="orange")
gages %>% st_geometry() %>% plot(., add=T, pch = 19, col="blue")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.0 Create Leaflet Map --------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#reproject layers into appropriate projection
sites %<>% st_transform(., crs=4326) %>% st_zm(.) 
gages %<>% st_transform(., crs=4326) %>% st_zm(.) 
sites_near_gages %<>% st_transform(., crs=4326) %>% st_zm(.) 

#Create map
m<-leaflet(sites_near_gages) %>% 
  #Add Basemaps
  addProviderTiles("Esri.WorldImagery", group = "Ortho") %>% 
  addProviderTiles("Esri.WorldTopoMap", group = "EsriTopo") %>% 
  addProviderTiles("OpenTopoMap", group = "OpenTopo") %>% 
  addTiles(group = "Ortho","Topo", "DEM") %>% 
  #Add Points
  addCircles(data=sites, col="orange", group = "All Sites", fillOpacity = 0.05) %>% 
  addCircles(data=sites_near_gages, col="orange", group = "Sites Near Gages", weight=2, fill=T, fillOpacity = 1) %>% 
  addCircles(data=gages, col="blue", fill=T, weight =4, group = "USGS Gages") %>% 
  #Add Layer Control
  addLayersControl(baseGroups = c("Ortho", "EsriTopo", "OpenTopo"), 
                   overlayGroups = c("USGS Gages",
                                     "All Sites",
                                     "Sites Near Gages"))

#export map
saveWidget(m,"initial_gage_map.html")
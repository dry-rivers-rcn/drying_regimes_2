#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Initial Map
#Coder: Nate Jones (cnjones7@ua.edu)
#Date: 11/20/2020
#Purpose: Exploratory Analysis  
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
library(raster)
library(riverdist)
library(parallel)
library(leaflet)
library(xts)
library(htmlwidgets)

#create temp workspace
file.create("data/temp")

#download files of interest
sites<-read_xlsx("data/sites_CA_ALL_Stations.xlsx")
gages<-st_read('data/spatial_data/gagesII/gagesII_9322_sept30_2011.shp')
streams<-st_read('data/spatial_data/NHDPlus18/Hydrography/NHDFlowline.shp')
sheds<-st_read('data/spatial_data/NHDPlus18/WBD/WBD_Subwatershed.shp')

#Define master projection
p<-"+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Prep Spatial Data----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.1 Convert sites to spatial layer 
sites<-st_as_sf(
  sites, 
  coords = c('TargetLongitude', 'TargetLatitude'), 
  crs = st_crs("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")) %>% 
  #Reproject
  st_transform(., crs=st_crs(p))

#2.2 Reproject gages and subset to CA 
gages<-gages %>% 
  filter(STATE=='CA') %>% 
  st_transform(., crs=st_crs(p))

#2.3 Reproject streams layer
streams<-streams %>% st_transform(., crs=st_crs(p)) %>% st_zm()

#2.4 Reproject sheds
sheds<-sheds %>% st_transform(., crs=st_crs(p)) %>% st_zm()

#2.5 Create list of HUC12 sheds
HUC8<-sheds %>% st_drop_geometry() %>% dplyr::select(HUC_8) %>% unique()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.0 Create fun to estimate river distances-------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.1 Create function -----------------------------------------------------------
fun<-function(n){
  
  #Organize Data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #Load required packages
  library(tidyverse)
  library(sf)
  library(riverdist)
  
  #Define shed of interest
  sheds_temp<-sheds %>% filter(HUC_8==HUC8$HUC_8[n])
  
  #Subset remaining spatial data
  gages_temp<-gages[sheds_temp,]
  sites_temp<-sites[sheds_temp,]
  streams_temp<-streams[sheds_temp,]
  
  #Create list of site ids
  site_id<-sites_temp$StationCode

  #Create a temp folder
  temp_file<-tempdir()
  dir.create(temp_file)
  
  #Estimate distance between sites and gages
  #Initiate if statement
  if(nrow(gages_temp)>0){
    #Export files to temp folder
    st_write(streams_temp, paste0(temp_file,"\\streams.shp"), delete_dsn = T)
    
    #Create flow network file
    rivs <- line2network(path=temp_file, layer="streams")
    
    #save flow network 
    save(rivs, file = paste0(temp_file, "\\riv.rda"))
    
    #Define x and y locations
    gages_temp$x<-st_coordinates(gages_temp)[,1]
    gages_temp$y<-st_coordinates(gages_temp)[,2]
    sites_temp$x<-st_coordinates(sites_temp)[,1]
    sites_temp$y<-st_coordinates(sites_temp)[,2]
    
    #Snap points to flow network
    gages_snap<-xy2segvert(x=gages_temp$x, y=gages_temp$y, rivers=rivs)
    site_snap<-xy2segvert(x=sites_temp$x, y=sites_temp$y, rivers=rivs)
    
    #Run riverdistance fun
    output<-riverdistancetofrom(
      seg1 = site_snap$seg,
      vert1 = site_snap$vert,
      seg2 = gages_snap$seg,
      vert2 = gages_snap$vert,
      rivers = rivs,
      ID1= site_id,
      ID2=gages_temp$STAID,
      stopiferror = F)
  
    #Find shortest distance for each location
    output<-output %>% 
      #Create tibble
      as_tibble(rownames = 'site_id') %>% 
      #pivot longer
      pivot_longer(-site_id, names_to = "gage_id", values_to = "dist_m") %>% 
      #Find shortest dist by gage
      group_by(site_id) %>% 
      slice(which.min(dist_m))  
    
  #Initiate else statement
  }else{
    #Create alternative output 
    output<-tibble(
      site_id = site_id, 
      gage_id = NA, 
      dist_m = NA)
  }

  #remove temp file
  unlink(temp_file)
    
  #Export output
  output
}

#3.2 Execute function in parallel-----------------------------------------------
#Create error function
execute<-function(m){
  output<-tryCatch(
    fun(m), 
    error = function(e){
      tibble(
        site_id = m, 
        gage_id = NA, 
        dist_m = NA)
      })
  #Export
  output
}

#Detect number of cores
n_cores<-detectCores()-1

#Create Cores
cl<-makeCluster(n_cores)

#Export relevant datasets to all cores
clusterExport(cl, c('gages', 'HUC8', 'sheds', 'sites','streams', 'fun'))

#Execute function!
x<-parLapply(
  cl = cl, 
  X = seq(1,nrow(HUC8)),
  fun = execute
)

#stop clusters
stopCluster(cl)

# Code graveyard ---------------------------------------------------------------
# #2.3 Creat 1 km buffer around gages and overlay~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# #Create 1km buffer around gages
# gages_buffer<-st_buffer(gages, dist = 1000)
# 
# #define sites within buffer area
# sites_near_gages<-sites[gages_buffer,]
# 
# #Limit gages to gages that intersect within 1000 meters of pnt
# sites_near_gages_buffer<-st_buffer(sites_near_gages, dist=1000)
# gages<-gages[sites_near_gages_buffer,]
# 
# #Plot for funzies
# sites_near_gages %>% st_geometry() %>% plot(., pch=19, col="orange")
# gages %>% st_geometry() %>% plot(., add=T, pch = 19, col="blue")
# 
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# #3.0 Create Leaflet Map --------------------------------------------------------
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# #reproject layers into appropriate projection
# sites %<>% st_transform(., crs=4326) %>% st_zm(.) 
# gages %<>% st_transform(., crs=4326) %>% st_zm(.) 
# sites_near_gages %<>% st_transform(., crs=4326) %>% st_zm(.) 
# 
# #Create map
# m<-leaflet(sites_near_gages) %>% 
#   #Add Basemaps
#   addProviderTiles("Esri.WorldImagery", group = "Ortho") %>% 
#   addProviderTiles("Esri.WorldTopoMap", group = "EsriTopo") %>% 
#   addProviderTiles("OpenTopoMap", group = "OpenTopo") %>% 
#   addTiles(group = "Ortho","Topo", "DEM") %>% 
#   #Add Points
#   addCircles(data=sites, col="orange", group = "All Sites", fillOpacity = 0.05) %>% 
#   addCircles(data=sites_near_gages, col="orange", group = "Sites Near Gages", weight=2, fill=T, fillOpacity = 1) %>% 
#   addCircles(data=gages, col="blue", fill=T, weight =4, group = "USGS Gages") %>% 
#   #Add Layer Control
#   addLayersControl(baseGroups = c("Ortho", "EsriTopo", "OpenTopo"), 
#                    overlayGroups = c("USGS Gages",
#                                      "All Sites",
#                                      "Sites Near Gages"))
# 
# #export map
# saveWidget(m,"initial_gage_map.html")
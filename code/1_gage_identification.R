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
  
  #Create a temp folder
  temp_file<-tempdir()
  dir.create(temp_file)
  
  #export flownet
  st_write(streams_temp, paste0(temp_file,"\\streams.shp"), delete_dsn = T)
  
  #Create flownet
  flow_net <- line2network(path=temp_file, layer="streams", tolerance = 1)
  
  #save flow network 
  save(flow_net, file = paste0(temp_file, "\\riv.rda"))
  
  #Create inner function for each gage
  inner_fun<-function(m){
    
    #Select gage
    g<-gages_temp[m,]
    
    #Select sites within 10km
    g_buffer<-st_buffer(g, dist=10000)
    s<-sites_temp[g_buffer,]
    
    #Define x and y locations
    g$x<-st_coordinates(g)[,1]
    g$y<-st_coordinates(g)[,2]
    s$x<-st_coordinates(s)[,1]
    s$y<-st_coordinates(s)[,2]
    
    #Snap points to flow network
    g_snap<-xy2segvert(x=g$x, y=g$y, rivers=flow_net)
    s_snap<-xy2segvert(x=s$x, y=s$y, rivers=flow_net)
  
    #Run riverdistance fun
    output<-riverdistancetofrom(
      seg1 = s_snap$seg,
      vert1 = s_snap$vert,
      seg2 = g_snap$seg,
      vert2 = g_snap$vert,
      rivers = flow_net,
      ID1= s$StationCode,
      ID2=g$STAID,
      stopiferror = F)
    
    #Tidy output
    output<-output %>% 
      as_tibble(rownames = 'site_id') %>% 
      pivot_longer(-site_id, names_to = "gage_id", values_to = "dist_m") 
  
    #Export output
    output
  }

#Apply function  
if(nrow(gages_temp)>0){
  #Apply fun
  output<-lapply(
    X=seq(1,nrow(gages_temp)),
    FUN = inner_fun)
  
  #Tidy
  output<-output %>% 
    bind_rows() %>% 
    group_by(site_id) %>% 
      slice(which.min(dist_m))
}else{
  output<-tibble(
    site_id = NA, 
    gage_id = NA, 
    dist_m = NA)
}
  
#remove temp file
unlink(temp_file)

#Export Output
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
n_cores<-detectCores()

#Create Cores
cl<-makeCluster(n_cores)

#Export relevant datasets to all cores
clusterExport(cl, c('gages', 'HUC8', 'sheds', 'sites','streams', 'fun'))

#Execute function!
t0<-Sys.time()
x<-parLapply(
  cl = cl, 
  X = seq(1,nrow(HUC8)),
  fun = execute
)
tf<-Sys.time()
tf-t0

#stop clusters
stopCluster(cl)

# Code graveyard ---------------------------------------------------------------



# fun<-function(n){
#   
#   #Organize Data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   #Load required packages
#   library(tidyverse)
#   library(sf)
#   library(riverdist)
#   
#   #Define shed of interest
#   sheds_temp<-sheds %>% filter(HUC_8==HUC8$HUC_8[n])
#   
#   #Subset remaining spatial data
#   gages_temp<-gages[sheds_temp,]
#   sites_temp<-sites[sheds_temp,]
#   streams_temp<-streams[sheds_temp,]
#   
#   #Subset sites to within 5 km buffer
#   gages_buffer<-st_buffer(gages_temp, 5000)
#   sites_temp<-sites_temp[gages_buffer,]
#   
#   #Create list of site ids
#   site_id<-sites_temp$StationCode
#   
#   #Create a temp folder
#   temp_file<-tempdir()
#   dir.create(temp_file)
#   
#   #Estimate distance between sites and gages
#   #Initiate if statement
#   if(nrow(gages_temp)>0){
#     #Export files to temp folder
#     st_write(streams_temp, paste0(temp_file,"\\streams.shp"), delete_dsn = T)
#     
#     #Create flow network file
#     rivs <- line2network(path=temp_file, layer="streams", tolerance = 1)
#     
#     #save flow network 
#     save(rivs, file = paste0(temp_file, "\\riv.rda"))
#     
#     #Define x and y locations
#     gages_temp$x<-st_coordinates(gages_temp)[,1]
#     gages_temp$y<-st_coordinates(gages_temp)[,2]
#     sites_temp$x<-st_coordinates(sites_temp)[,1]
#     sites_temp$y<-st_coordinates(sites_temp)[,2]
#     
#     #Snap points to flow network
#     gages_snap<-xy2segvert(x=gages_temp$x, y=gages_temp$y, rivers=rivs)
#     site_snap<-xy2segvert(x=sites_temp$x, y=sites_temp$y, rivers=rivs)
#     
#     #Run riverdistance fun
#     output<-riverdistancetofrom(
#       seg1 = site_snap$seg,
#       vert1 = site_snap$vert,
#       seg2 = gages_snap$seg,
#       vert2 = gages_snap$vert,
#       rivers = rivs,
#       ID1= site_id,
#       ID2=gages_temp$STAID,
#       stopiferror = F)
#     
#     #Find shortest distance for each location
#     output<-output %>% 
#       #Create tibble
#       as_tibble(rownames = 'site_id') %>% 
#       #pivot longer
#       pivot_longer(-site_id, names_to = "gage_id", values_to = "dist_m") %>% 
#       #Find shortest dist by gage
#       group_by(site_id) %>% 
#       slice(which.min(dist_m))  
#     
#     #Initiate else statement
#   }else{
#     #Create alternative output 
#     output<-tibble(
#       site_id = site_id, 
#       gage_id = NA, 
#       dist_m = NA)
#   }
#   
#   #remove temp file
#   unlink(temp_file)
#   
#   #Export output
#   output
# }
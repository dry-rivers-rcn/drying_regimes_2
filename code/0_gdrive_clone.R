#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Google Drive Clone
#Coder: Nate Jones (cnjones7@ua.edu)
#Date: 11/17/2020
#Purpose: Download gdrive with complete control
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Helpful link
#   https://community.rstudio.com/t/how-to-download-a-google-drives-contents-based-on-drive-id-or-url/16896/12


#Gdrive of interest
#   https://drive.google.com/drive/folders/1FMAyCj91wB_tQDr6GfeDnTZHcUAbNnpR
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 1: Setup workspace -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Clear memory
remove(list=ls())

#Add libraries of interest
library('tidyverse')
library("googledrive")
library('parallel')

#Define google folder of interest 
#  (https://drive.google.com/drive/folders/[gdrive_id])
gdrive_id<-'19ludmjLkpNLR81mIyV5LIvfwiZcSCoBK'

#Initiate connection with 
drive_find(n_max = 2)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 2: Clone folder schema----------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Create data file
dir.create("data")

#Define total number of folders in gdrive
n_folders<-drive_ls(
  path = as_id(gdrive_id), 
  type='folder', 
  recursive = T) %>% 
  nrow()

#Define google drive folders
files<-drive_ls(
  path = as_id(gdrive_id), 
  type='folder')

#Add local folder name
files<-files %>% 
  mutate(local_name = paste0("data//",name))

#Define counter
n<-1

#Use while loop find all folders imbedded in directory
while(n<=n_folders){
  #Identify subfolders
  temp<-drive_ls(
    path = as_id(files$id[n]), 
    type= 'folder')
  
  #Add local folder name
  temp<-temp %>% 
    mutate(local_name = paste0(files$local_name[n],"//",name))
  
  #bind temp to master file list
  files<-bind_rows(files, temp)
  
  #Remove temp file
  remove(temp)
  
  #Print counter
  print(paste(round(n/n_folders*100,1), "% Done"))
  
  #Add to counter
  n<-n+1
}

#Create local copy of folder schema
lapply(files$local_name, dir.create)

#Add root dir file
files<-
  tibble(
    name = NA,
    id = gdrive_id, 
    drive_resource=NA, 
    local_name = "data") %>% 
  bind_rows(., files)  

#Cleanup env
remove(n)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 3: Create function to copy files in each folder---------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
fun<-function(n){
  
  #Define required libraries
  library('tidyverse')
  library("googledrive")
  
  #Create list of objects in folder
  temp<-drive_ls(as_id(files$id[n]))
  
  #Add output file path
  temp<-temp %>% 
    mutate(local_name = paste0(files$local_name[n],"//",name))
  
  #Create inner download function
  inner_fun<-function(m){
    tryCatch(
      drive_download(
        file = as_id(temp$id[m]),
        path = temp$local_name[m]
      ), 
      error = function(e) NA
    )
  }
  
  #Apply inner function to list of objects
  lapply(seq(1,nrow(temp)), inner_fun)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 4: Apply function in parrallel -------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Define number of cores available
n_cores<-3

#Create clusters
cl <- makeCluster(n_cores)

#Send environmental vars to clusters
clusterExport(cl,list('files','fun'))

#Execute!
parLapply(
  cl = cl, 
  fun = fun, 
  X = seq(1, nrow(files)))

#Remove working cores
stopCluster(cl)

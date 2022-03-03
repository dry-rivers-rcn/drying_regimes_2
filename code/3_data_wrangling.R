################################################################################
#Title: Exploratory Data Analysis
#Date: 3/3/2022
#Coder: Adam Price and Nate Jones
#Purpose: Examine flow regime metrics for so-cal data
################################################################################

################################################################################
#Setup workspace ---------------------------------------------------------------
################################################################################
#Clear Global Env
remove(list=ls())

#Load libraries of interest
library(tidyverse)
library(readxl)

#Download data
sheets <- excel_sheets(path = "data/Loflen_Loggers_All_Data.xlsx")

#Create function
download_fun<-function(n){
  #Download data
  x <- read_excel(
    path = "data/Loflen_Loggers_All_Data.xlsx", 
    sheet = sheets[n]
    )
  
  #Tidy
  x <- x %>% 
    select(
      datetime = starts_with("date"), 
      waterLevel = starts_with("water")) %>% 
    mutate(site_id = sheets[n])
  
  #get rid of extra water level collumns
  if("waterLevel1" %in% colnames(x)){
    x <- x %>% 
      rename(waterLevel = waterLevel1) %>% 
      select(-waterLevel2)
  }
  
  #Export
  x
  
}

#Aplly fun
df<-lapply(X=seq(1,length(sheets)), FUN = download_fun)
df<-bind_rows(df)



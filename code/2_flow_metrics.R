#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Flow metric calculatin
#Date: 9/13/2022
#Coder: Nate Jones
#Purpose: Calculate Flow Metrics
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.0 Setup workspace -----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Clear memory
remove(list=ls())

#Load libraries of interest
library(tidyverse)
library(zoo)
library(lubridate)
library(readxl)
library(xts)
library(dygraphs)
library(patchwork)

#Define events
events <- read_csv("data//event_dates.csv")
start_stop <- read_csv("data//event_start_stop.csv")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Download logger data ------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

#list excel sheets
sheets <- excel_sheets(path = "data/Loflen_Loggers_All_Data.xlsx")

#read sampling dates into R environment
sample_date<-read_csv('data//biologic_sample_days.csv') %>% rename(site_id = stationcode)

#Apply fun
waterLevel<-lapply(X=seq(1,length(sheets)), FUN = download_fun)
waterLevel<-bind_rows(waterLevel)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.0 Estimate flow metrics ------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Tidy event df
events<-events %>% 
  #Filter to station code
  rename(site = stationcode) %>% 
  #join to start and stop
  left_join(., start_stop) %>% 
  # filter for hydro metrics
  filter(hydro_metrics ==  'full') %>% 
  distinct() %>% 
  #Select cols of interest
  select('SampleDate', 'StartEvent', 'EndEvent',  'site')

#3.2 Create functions ----------------------------------------------------------
fun<-function(n){
  #Prep Data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #Identify event of interest
  event<-events[n,]
  
  #Filter logger data to event
  wL <- waterLevel %>% 
    filter(site_id == event$site) %>% 
    filter(datetime > as.POSIXct(event$StartEvent, format = '%m/%d/%Y')) %>% 
    filter(datetime < as.POSIXct(event$EndEvent, format = '%m/%d/%Y'))
  
  #Estimage daily waterLevel
  wL <- wL %>% 
    mutate(day = date(datetime)) %>% 
    group_by(day) %>% 
    summarise(waterLevel = mean(waterLevel, na.rm=T))
  
  #For testing
  wL %>% ggplot() + geom_line(aes(x=day, y = waterLevel))
  
  #Calculate metrics ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #Define sampling date
  sampling_date <- as.POSIXct(event$SampleDate, format="%m/%d/%Y")
  
  #Determine first day of drying
  dry_date <- wL %>% 
    filter(waterLevel == 0) %>% 
    summarise(day = min(day, na.rm=T)) %>% 
    pull() %>% 
    as.POSIXct(., format = "%Y-%m-%d")
  
  #Determine rewet date
  rewet_date <- wL %>% 
    filter(day>dry_date) %>% 
    filter(waterLevel>0) %>% 
    summarise(day = min(day,na.rm=T)) %>% 
    pull() %>% 
    as.POSIXct(., format = "%Y-%m-%d") 
  
  #Determine peak date
  peak_date <- wL %>% 
    filter(day>dry_date) %>%
    filter(day<sampling_date) %>% 
    filter(waterLevel == max(waterLevel, na.rm=T)) %>% 
    summarise(day = min(day,na.rm=T)) %>% 
    pull() %>% 
    as.POSIXct(., format = "%Y-%m-%d") 
   
  #Antedent time period
  antecedent_time_period_days <- sampling_date - dry_date
  antecedent_time_period_days <- antecedent_time_period_days %>% paste %>% as.numeric
  
  #Dry duration
  dry_duration <- rewet_date - dry_date 
  dry_duration <- dry_duration %>% paste %>% as.numeric
  
  #Proportion of wet days
  prop_wet_days <- wL %>% 
    filter(day>dry_date) %>% 
    filter(day<sampling_date) %>% 
    filter(waterLevel>0) %>% 
    summarise(wet_days = n()) %>% 
    mutate(prop_wet_days = wet_days/antecedent_time_period_days) %>% 
    select(prop_wet_days) %>% 
    pull()
  
  #Proportion of dry days
  prop_dry_days <- 1-prop_wet_days
  
  #Rewet duration
  rewet_duration <- peak_date - rewet_date
  rewet_duration <- rewet_duration %>% paste %>% as.numeric
  
  #Rewet Slope
  rewet_slope <- wL %>% 
    filter(day<=peak_date) %>% 
    filter(day>=rewet_date) %>% 
    mutate(slope = lead(waterLevel) - waterLevel) %>% 
    filter(slope>0) %>% 
    summarise(slope = mean(slope, na.rm=T)) %>% 
    pull()
  
  #peak2sample duration
  peak2sample_duration <- peak_date-sampling_date
  peak2sample_duration <- peak2sample_duration %>% paste %>% as.numeric
  
  #Peak2Sample Slope
  peak2sample_slope <- wL %>% 
    filter(day>=peak_date) %>% 
    filter(day<=sampling_date) %>% 
    mutate(slope = lead(waterLevel) - waterLevel) %>% 
    filter(slope<0) %>% 
    summarise(slope = mean(slope, na.rm=T)) %>% 
    pull()
  
  #Peak depth
  peak_depth<- wL %>% 
    filter(day >= rewet_date) %>% 
    summarise(max(waterLevel, na.rm = T)) %>% 
    pull()
  
  #Recession Rate
  r <- wL %>% 
    filter(day>rewet_date) %>% 
    mutate(slope = lead(waterLevel) - waterLevel) %>% 
    filter(slope<0) 
  recession_coef <- -1*summary(lm(r$slope~r$waterLevel))$coef[2,1]
  
  #Create Export
  output<-tibble(
    site = event$site,
    SampleDate = event$SampleDate,
    antecedent_time_period_days, 
    dry_date,
    dry_duration,
    peak_date,
    peak_depth,
    peak2sample_duration,
    peak2sample_slope,
    prop_dry_days,
    prop_wet_days,
    recession_coef,
    rewet_date,
    rewet_duration,
    rewet_slope,
    sampling_date
  )
  
  output
}

#3.3 Run event -----------------------------------------------------------------
#Create wrapper function 
error_fun<-function(n){
  tryCatch(
    expr = fun(n), 
    error = function(e)
      output<-tibble(
        site = events$site[n],
        SampleDate = events$SampleDate[n],
        antecedent_time_period_days = NA, 
        dry_date = NA,
        dry_duration = NA,
        peak_date = NA,
        peak_depth = NA,
        peak2sample_duration = NA,
        peak2sample_slope = NA,
        prop_dry_days = NA,
        prop_wet_days = NA,
        recession_coef = NA,
        rewet_date = NA,
        rewet_duration = NA,
        rewet_slope = NA,
        sampling_date = NA
      )
  )
}  

#Run wrapper fun
metrics <- lapply(
    X = seq(1,nrow(events)), 
    FUN = error_fun) %>% 
  bind_rows()

write_csv(metrics, "docs/metrics.csv")

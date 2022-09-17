#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Flow metric calculation
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
start_stop <- read_csv("docs//event_start_stop.csv")

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
  test_plot<-wL %>% ggplot() + geom_line(aes(x=day, y = waterLevel))
  test_plot
  
  #Find longest dry period and delete everything before it 
  wL <- wL %>% 
    #Define zeros
    mutate(zero = if_else(waterLevel == 0, 1, 0)) %>% 
    #Count zero events
    mutate(dry_start = if_else(zero == 1 & lag(zero) == 0, 1, 0)) %>% 
    mutate(dry_start = if_else(is.na(dry_start), 1, dry_start)) %>% 
    #Create groups
    mutate(dry_event = cumsum(dry_start))
  
  period<- wL %>%   
    #Filter to zeros
    filter(waterLevel==0) %>% 
    #Estimate length of groups
    group_by(dry_event) %>% 
    summarise(count = n()) %>% 
    filter(count == max(count))
  
  wL <- wL %>% 
    filter(dry_event >= period$dry_event) %>% 
    select(day, waterLevel)
  
  #For testing
  test_plot<-wL %>% ggplot() + geom_line(aes(x=day, y = waterLevel))
  test_plot
  
  #Calculate metrics ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #Define sampling date
  sampling_date <- mdy(event$SampleDate)
  test_plot<-test_plot + geom_vline(aes(xintercept = sampling_date), col='red')
  test_plot
  
  #Determine first day of drying
  dry_date <- wL %>% 
    filter(waterLevel == 0) %>% 
    summarise(day = min(day, na.rm=T)) %>% 
    pull() %>% 
    ymd(.)
  test_plot<-test_plot + geom_vline(aes(xintercept = (dry_date)), col='red')
  test_plot
  
  #Determine rewet date
  rewet_date <- wL %>% 
    filter(day>dry_date) %>% 
    filter(waterLevel>0) %>% 
    summarise(day = min(day,na.rm=T)) %>% 
    pull() %>% 
    ymd()
  test_plot<-test_plot + geom_vline(aes(xintercept = (rewet_date)), col='blue')
  test_plot
  
  #Determine peak date
  peak_date <- wL %>% 
    filter(day>dry_date) %>%
    filter(day<sampling_date) %>% 
    filter(waterLevel == max(waterLevel, na.rm=T)) %>% 
    summarise(day = min(day,na.rm=T)) %>% 
    pull() %>% 
    ymd()
  test_plot<-test_plot + geom_vline(aes(xintercept = (peak_date)), col='purple')
  test_plot
   
  #Antedent time period
  antecedent_time_period_days <- sampling_date - dry_date
  antecedent_time_period_days <- antecedent_time_period_days %>% paste %>% as.numeric
  antecedent_time_period_days
  
  #Dry duration
  dry_duration <- rewet_date - dry_date 
  dry_duration <- dry_duration %>% paste %>% as.numeric
  dry_duration
  
  #Proportion of wet days
  prop_wet_days <- wL %>% 
    filter(day>dry_date) %>% 
    filter(day<sampling_date) %>% 
    filter(waterLevel>0) %>% 
    summarise(wet_days = n()) %>% 
    mutate(prop_wet_days = wet_days/antecedent_time_period_days) %>% 
    select(prop_wet_days) %>% 
    pull()
  prop_wet_days
  
  #Proportion of dry days
  prop_dry_days <- 1-prop_wet_days
  prop_dry_days
  
  #Rewet duration
  rewet_duration <- peak_date - rewet_date
  rewet_duration <- rewet_duration %>% paste %>% as.numeric
  rewet_duration
  
  #Rewet Slope
  rewet_slope <- wL %>% 
    filter(day<=peak_date) %>% 
    filter(day>=rewet_date) %>% 
    mutate(slope = lead(waterLevel) - waterLevel) %>% 
    filter(slope>0) %>% 
    summarise(slope = mean(slope, na.rm=T)) %>% 
    pull()
  rewet_slope
  
  #peak2sample duration
  peak2sample_duration <- sampling_date - peak_date
  peak2sample_duration <- peak2sample_duration %>% paste %>% as.numeric
  peak2sample_duration
  
  #Peak2Sample Slope
  peak2sample_slope <- wL %>% 
    filter(day>=peak_date) %>% 
    filter(day<=sampling_date) %>% 
    mutate(slope = lead(waterLevel) - waterLevel) %>% 
    filter(slope<0) %>% 
    summarise(slope = mean(slope, na.rm=T)) %>% 
    pull()
  peak2sample_slope
  
  #Peak depth
  peak_depth<- wL %>% 
    filter(day >= rewet_date) %>% 
    summarise(max(waterLevel, na.rm = T)) %>% 
    pull()
  peak_depth
  
  #Recession Rate
  r <- wL %>% 
    filter(day>rewet_date) %>% 
    mutate(slope = lead(waterLevel) - waterLevel) %>% 
    filter(slope<0) 
  recession_coef <- -1*summary(lm(r$slope~r$waterLevel))$coef[2,1]
  recession_coef
  
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

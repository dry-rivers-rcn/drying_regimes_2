################################################################################
#Title: Exploratory Data Analysis
#Date: 3/3/2022
#Coder: Adam Price and Nate Jones
#Purpose: Examine flow regime metrics for so-cal data
################################################################################

################################################################################
#1.0 Setup workspace -----------------------------------------------------------
################################################################################
#Clear Global Env
remove(list=ls())

#Load libraries of interest
library(tidyverse)
library(zoo)
library(readxl)

################################################################################
#2.0 Download data by sheet ----------------------------------------------------
################################################################################
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

#Apply fun
waterLevel<-lapply(X=seq(1,length(sheets)), FUN = download_fun)
waterLevel<-bind_rows(waterLevel)

#Create vector of unqiue site names
sites<-waterLevel %>% select(site_id) %>% distinct() %>% pull()

#plot
waterLevel %>% 
  ggplot(aes(x=datetime, y=waterLevel)) +
    geom_line() + 
      facet_wrap(vars(site_id))


################################################################################
#3.0 Estimate hydro metrics ----------------------------------------------------
################################################################################
#3.1 Create metrics function ---------------------------------------------------
metrics_fun<-function(n){

  #Identify site
  site<-sites[n]
  
  #A. Identify drying events ----------------------------------------------------
  #Reduce waterLevel to site in question
  df<-waterLevel %>% filter(site_id==site)
  
  #remove NAs
  df<- df %>% na.omit()
  
  #Order by date
  df %>% arrange(datetime)
  
  #Create filters to define initiation of flow and no flow
  df<-df %>% 
    #Estiamte quantile 
    mutate(wL_quant = rank(waterLevel, ties.method = 'min')/nrow(df)) %>% 
    #Round to nearest 10th to kill some of the noise
    mutate(wL_round = round(waterLevel,1)) %>% 
    #30 day moving window
    mutate(
      wL_drying  = rollapply(wL_round, 120, mean, align = 'left',   partial=T), 
      wL_wetting = rollapply(wL_round, 120, mean, align = 'right',  partial=T),
      wL_peaking = rollapply(wL_round, 120, max,  align = 'center', partial=T)) %>% 
    #make sure zero is actually zerio
    mutate(
      wL_drying = if_else(wL_drying<0.001, 0, wL_drying),
      wL_wetting = if_else(wL_wetting<0.001, 0, wL_wetting),
    )
  
  #Define time steps when flow initiates
  df <- df %>% 
    mutate(flow_initation = if_else(lag(wL_wetting)==0 & wL_wetting>0, 1, 0))
  
  #Define starting conditions
  df$flow_initation[1]<-ifelse(df$wL_wetting[1]>0, 1, 0)
  
  #Define individual drying events
  df$event_id<-cumsum(df$flow_initation)
  
  #B Metrics by event ----------------------------------------------------------
  #inner function to estimate metrics by event
  inner_fun <- function(m){
    
    #Isolate individual recession events
    t <- df %>% filter(event_id == m) 
    
    #Convert NA to zero
    t <- t %>% replace_na(list(nf_start=0)) 
    
    #timing metrics ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #Estimate daily stage
    t_daily <- t %>% 
      mutate(day = date(datetime)) %>% 
      group_by(day) %>% 
      summarise(waterLevel=mean(waterLevel, na.rm=T))
    
    #Estimate center of mass for flow 
    t_daily <- t_daily %>% 
      mutate(d = day - min(day))
    flow_centroid<-min(t_daily$day) + weighted.mean(t_daily$d, t_daily$waterLevel)
    
    #Estimate 30 day no-flow from moving window
    no_flow_date<-t %>% 
      select(datetime, wL_drying) %>% 
      filter(wL_drying==0) %>% 
      filter(row_number()==1) %>% 
      select(datetime) %>% 
      mutate(datetime = date(datetime)) %>%
      pull()
    
    #duration metrics ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    no_flow_duration_days = t_daily %>% filter(waterLevel==0) %>% nrow()
    flow_duration_days = t_daily %>% filter(waterLevel>0) %>% nrow()
    no_flow_prop = no_flow_duration_days/(flow_duration_days + no_flow_duration_days)
    
    #magnitude of drying ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #estimate peak2zero
    peak2zero_days<-as.numeric(paste(date(no_flow_date) - date(flow_centroid)))
    
    #recession
    peak_wL <-  max(t$wL_quant, na.rm=T)
    recession <- max(t$wL_quant, na.rm=T)/peak2zero_days
    
    #flashiness ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    flashiness <- t %>% 
      mutate(wL_diff = abs(waterLevel - lag(waterLevel))) %>% 
      drop_na(wL_diff) %>% 
      summarise(flashiness = sum(wL_diff)/sum(waterLevel))
      
      
    #Export output ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    output<-tibble(
      event_id = m, 
      flow_centroid, 
      no_flow_date,
      no_flow_duration_days, 
      flow_duration_days, 
      no_flow_prop,
      flashiness,
      peak_wL,
      peak2zero_days, 
      recession)
    
    output
  }
  
  #Apply function
  metrics<-lapply(seq(1,max(df$event_id, na.rm = T)), inner_fun) %>% bind_rows()
  metrics  
    
  #C Export --------------------------------------------------------------------
  #Add site information
  metrics <- metrics %>% 
    mutate(site = site)
  
  metrics
}

#Apply function ----------------------------------------------------------------
#Create vector of unqiue site names
sites<-waterLevel %>% select(site_id) %>% distinct() %>% pull()

#Remove sites
sites <- sites[-10]

#Apply fun
output<-lapply(seq(1, length(sites)), metrics_fun) %>% bind_rows

#Write output
write_csv(output, "metrics.csv")

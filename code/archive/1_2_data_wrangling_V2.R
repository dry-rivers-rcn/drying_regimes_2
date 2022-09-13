################################################################################
#Title: Exploratory Data Analysis (V2)
#Date: 7/18/2022
#Coder: Nate Jones
#Purpose: Updated flow-metrics based on June project meeting
################################################################################

#Next steps: 
#manual set identification threshold for each gage. Include both major and minor events
#then, isolate storms [update to just include ressession and dry event before]
#then, finally, calculate metrics




################################################################################
#1.0 Setup workspace -----------------------------------------------------------
################################################################################
#Clear Global Env
remove(list=ls())

#Load libraries of interest
library(tidyverse)
library(zoo)
library(lubridate)
library(readxl)
library(xts)
library(dygraphs)

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

#read sampling dates into R environment
sample_date<-read_csv('data//biologic_sample_days.csv') %>% rename(site_id = stationcode)

#Apply fun
waterLevel<-lapply(X=seq(1,length(sheets)), FUN = download_fun)
waterLevel<-bind_rows(waterLevel)

#Create vector of unqiue site names
sites<-waterLevel %>% 
  left_join(sample_date) %>% 
  select(site_id, SampleDate) %>% 
  distinct() %>% 
  drop_na()

################################################################################
#3.0 Plot with dygraphs function-----------------------------------------------
################################################################################
dygraph_ts_fun<-function(df, site){
  
  #Select collumns of interest
  df <- df %>%
    mutate(waterLevel = waterLevel*100) %>% 
    filter(site_id == site)
  
  #format data
  df_xts<-df %>% na.omit() 
  df_xts<-xts(df_xts, order.by=df_xts$datetime)
  df_xts<-df_xts[,-1]
  
  #Plot
  dygraph(df_xts) %>%
    dyRangeSelector() %>%
    dyLegend() %>%
    dyOptions(strokeWidth = 1.5) %>%
    dyOptions(labelsUTC = TRUE) %>%
    dyHighlight(highlightCircleSize = 5,
                highlightSeriesBackgroundAlpha = 0.2,
                hideOnMouseOut = FALSE) %>%
    dyAxis("y", label = "Variable")
}

#Plot
dygraph_ts_fun(df = waterLevel, site=sites$site_id[1])
Sdygraph_ts_fun(df = waterLevel, site=sites[2])

################################################################################
#4.0 Estimate hydro metrics ----------------------------------------------------
################################################################################
#3.1 Create metrics function ---------------------------------------------------
metrics_fun<-function(n){

  #Identify site and sample date of interest
  site   <- sites %>% select(site_id) %>% slice(n) %>% pull()
  s_date <- sites %>% select(SampleDate) %>% slice(n) %>% pull()
  
  #A. Identify drying events ----------------------------------------------------
  #Reduce waterLevel to site in question
  df<-waterLevel %>% filter(site_id==site)
  
  #remove NAs
  df<- df %>% na.omit()
  
  #Order by date
  df<-df %>% arrange(datetime)
  
  #Create filters to define initiation of flow and no flow
  #Remove signals that are less than 2 inches 
  df <- df %>% mutate(wL_round = if_else(waterLevel<0.05, 0, waterLevel)) 
  #Remove false initiations
  df <- df %>% 
    mutate(false_flow = if_else(wL_round > 0 & lead(wL_round) == 0 & lag(wL_round) == 0, 1,0)) %>% 
    filter(false_flow == 0) %>% 
    select(-false_flow)
  #Remove false zeros
  df <- df %>% 
    mutate(false_no_flow = if_else(wL_round == 0 & lead(wL_round) > 0 & lag(wL_round) > 0, 1,0)) %>% 
    filter(false_no_flow == 0) %>% 
    select(-false_no_flow)
    
  #Define time steps when flow initiates
  df <- df %>% 
    #define water leven n-1 and n+1 days
    mutate(
      wL_n_plus  = lead(wL_round),
      wL_n_minus = lag(wL_round)) %>%  
    #Define flow initiation
    mutate(flow_initation = if_else(wL_n_minus==0 & wL_round>0, 1, 0))
  
  #Define starting conditions
  df$flow_initation[1]<-ifelse(df$wL_round[1]>0, 1, 0)
  
  #Define individual drying events
  df$event_id<-cumsum(df$flow_initation)
  
  #Define event of interest
  event<-df %>% 
    mutate(date = date(datetime)) %>% 
    select(date, event_id) %>% 
    distinct() %>% 
    filter(date == mdy(s_date)) %>% 
    select(event_id) %>% 
    pull()
  
  #Define events of interest
  event<-c(event, event-1)
  
  #event
  df<-df %>% filter(event_id %in% event)
    
  #plots for testing
  plot_wL<-df %>% 
    group_by(event_id) %>% 
    summarise(
      date_min = min(datetime, na.rm=T), 
      date_max = max(datetime, na.rm=T), 
      wL_max   = max(wL_round, na.rm=T)) %>% 
    ungroup() %>% 
    left_join(df, .) %>% 
    ggplot() +
      geom_area(aes(x=datetime, y = wL_max), col="grey30", alpha=0.3) +
      geom_line(aes(x=datetime, y=waterLevel)) +
      geom_point(aes(x=as.POSIXct(mdy(s_date)), y = max(wL_max, na.rm=T)), pch = 25, bg='red', cex=3) +
      theme_bw() + 
      xlab('Date') + 
      ylab("Water Level [m]") +
      ggtitle(paste0("Site=",df$site_id[1]))
  
  #view plot
  ggsave(plot = plot_wL, filename = paste0("temp/",df$site_id[1], "_", mdy(s_date),'.png'), width = 6, height = 4, units = "in")

}

#Apply function ----------------------------------------------------------------
#Create vector of unqiue site names
#sites<-waterLevel %>% select(site_id) %>% distinct() %>% pull()

#Remove sites
sites <- sites[-10,]

#Apply fun
output<-lapply(seq(1, nrow(sites)), metrics_fun) %>% bind_rows

#create output plot
l1 <- list.files('temp/', full.names = T)
r1 <- lapply(l1, png::readPNG)
do.call(gridExtra::grid.arrange, r1)

#Write output
#write_csv(output, "docs/metrics.csv")




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Drying event definition
#Date: 9/3/2022
#Coder: Nate Jones
#Purpose: Identify Individual Drying EventDrying 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Here -- a drying event includes both the concurrent and previous flow pulses 
#associated with a single sampling eent

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.0 Setup workspace -----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Clear Global Env
remove(list=ls())

#Load libraries of interest
library(tidyverse)
library(zoo)
library(lubridate)
library(readxl)
library(xts)
library(dygraphs)
library(patchwork)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Download data by sheet ----------------------------------------------------
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

#Create vector of unqiue site names
events<-waterLevel %>% 
  left_join(sample_date) %>% 
  select(site_id, SampleDate) %>% 
  distinct() %>% 
  drop_na()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.0 Plotting functions --------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.1 Create plotting functions -------------------------------------------------
#Interactive plot
dygraph_ts_fun<-function(waterLevel, site){
  
  #Select collumns of interest
  df <- waterLevel %>%
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

#Static plot
#Identify site of interest
event_plot_fun<-function(site_id, drying_events){

  #Identify site
  s <- site_id
  
  #Identify sampling events of interest
  e <- drying_events %>% 
    mutate(SampleDate = as.POSIXct(SampleDate, format = '%m/%d/%Y'))
  
  #Identify drying events
  drying_events <- drying_events %>% 
    mutate(
      SampleDate = as.POSIXct(SampleDate, format = '%m/%d/%Y'),
      StartEvent = as.POSIXct(StartEvent, format = '%m/%d/%Y'), 
      EndEvent  =  as.POSIXct(EndEvent , format = '%m/%d/%Y')
    )
  
  #Create Plot
  waterLevel %>% 
    filter(site_id == s) %>%  
    ggplot() + 
      geom_rect(
        data=drying_events, 
        aes(
          xmin = StartEvent, 
          xmax = EndEvent, 
          ymin = 0,
          ymax = Inf), 
        alpha = 0.3) +
      geom_vline(
        data = e,
        mapping = aes(xintercept=SampleDate), 
        lty = 2, 
        lwd = 1.2,
        col = "red") + 
      geom_line(aes(x=datetime, y = waterLevel)) + 
    theme_bw() + 
    theme(
      axis.title.y = element_text(size = 14), 
      axis.text.y  = element_text(size = 10)
    ) + 
    #Add labels
    xlab(NULL) + 
    ylab("Stage [m]") +
    ggtitle(paste(s, drying_events$SampleDate[1]))
}
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#4.0 Identify storm events -----------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Define list of sites
sites <- events %>% select(site_id) %>% distinct() %>% pull()

#4.1 901BELOLV -----------------------------------------------------------------
#Define site of interest
site <- sites[1]

#Plot interactive polt
dygraph_ts_fun(waterLevel, site)

#Create temporary 
temp <- events %>% 
  filter(site_id == site) %>% 
  select(SampleDate) %>% 
  mutate(StartEvent = NA, EndEvent = NA)

#Mannualy Define Storm
#Event 1
temp$StartEvent[1] <- NA
temp$EndEvent[1]   <- NA
fig_1 <- event_plot_fun(site, drying_events = temp[1,])
#Start of record
#Event 2
temp$StartEvent[2] <- "4/4/2013"
temp$EndEvent[2]   <- "5/6/2014"
fig_2 <- event_plot_fun(site, drying_events = temp[2,])
#Event 3
temp$StartEvent[3] <- "4/4/2013"
temp$EndEvent[3]   <- "5/6/2014"
fig_3 <- event_plot_fun(site, drying_events = temp[3,])
#Event 4
temp$StartEvent[4] <- "2/13/2014"
temp$EndEvent[4]   <- "4/12/2015"
fig_4 <- event_plot_fun(site, drying_events = temp[4,])
#Event 5
temp$StartEvent[5] <- "12/23/2014"
temp$EndEvent[5]   <- "5/12/2016"
fig_5 <- event_plot_fun(site, drying_events = temp[5,])

#Create plot
fig_1 + fig_2 + fig_3 +fig_4 + fig_5
#ggsave(paste0("temp/",site, ".png"))

#Add start and end date to master df
temp$site  <- site
start_stop <- temp

#4.2 901NP9FLC -----------------------------------------------------------------
#Define site of interest
site <- sites[2]

#Plot interactive polt
dygraph_ts_fun(waterLevel, site)

#Create temporary 
temp <- events %>% 
  filter(site_id == site) %>% 
  select(SampleDate) %>% 
  mutate(StartEvent = NA, EndEvent = NA)

#Mannualy Define Storm
#Event 1
temp$StartEvent[1] <- NA
temp$EndEvent[1]   <- NA
fig_1 <- event_plot_fun(site, drying_events = temp[1,])
#Start of record
#Event 2
temp$StartEvent[2] <- NA
temp$EndEvent[2]   <- NA
fig_2 <- event_plot_fun(site, drying_events = temp[2,])
#Event 3
temp$StartEvent[3] <- '11/16/2013'
temp$EndEvent[3]   <- '6/8/2015'
fig_3 <- event_plot_fun(site, drying_events = temp[3,])
#Event 4
temp$StartEvent[4] <- "11/14/2014"
temp$EndEvent[4]   <- "6/1/2016"
fig_4 <- event_plot_fun(site, drying_events = temp[4,])
#Event 5
temp$StartEvent[5] <- "11/28/2015"
temp$EndEvent[5]   <- "8/24/2017"
fig_5 <- event_plot_fun(site, drying_events = temp[5,])

#Create plot
fig_1 + fig_2 + fig_3 +fig_4 + fig_5
#ggsave(paste0("temp/",site, ".png"))

#Add start and end date to master df
temp$site  <- site
start_stop <- bind_rows(start_stop, temp)

#Add start and end date to master df
temp$site  <- site
start_stop <- bind_rows(start_stop, temp)

#4.3 901NP9HJC -----------------------------------------------------------------
#Define site of interest
site <- sites[3]

#Plot interactive polt
dygraph_ts_fun(waterLevel, site)

#Create temporary 
temp <- events %>% 
  filter(site_id == site) %>% 
  select(SampleDate) %>% 
  mutate(StartEvent = NA, EndEvent = NA)
temp
event_plot_fun(site, drying_events = temp[1,])

#Mannualy Define Storm
#Event 1
temp$StartEvent[1] <- "12/5/2014"
temp$EndEvent[1]   <- "4/20/2016"
fig_1 <- event_plot_fun(site, drying_events = temp[1,])

#Create plot
fig_1 
#ggsave(paste0("temp/",site, ".png"))

#Add start and end date to master df
temp$site  <- site
start_stop <- bind_rows(start_stop, temp)

#4.4 901NP9LCC -----------------------------------------------------------------
#Define site of interest
site <- sites[4]

#Plot interactive polt
dygraph_ts_fun(waterLevel, site)

#Create temporary 
temp <- events %>% 
  filter(site_id == site) %>% 
  select(SampleDate) %>% 
  mutate(StartEvent = NA, EndEvent = NA)
temp
event_plot_fun(site, drying_events = temp[2,])

#Mannualy Define Storm
#Event 1
temp$StartEvent[1] <- NA
temp$EndEvent[1]   <- NA
fig_1 <- event_plot_fun(site, drying_events = temp[1,])

#Event 2
temp$StartEvent[2] <- "1/1/2016"
temp$EndEvent[2]   <- "8/24/2017"
fig_2 <- event_plot_fun(site, drying_events = temp[2,])

#Create plot
fig_1 + fig_2 
#ggsave(paste0("temp/",site, ".png"))

#Add start and end date to master df
temp$site  <- site
start_stop <- bind_rows(start_stop, temp)

#4.5 903ACPCT1 -----------------------------------------------------------------
#Define site of interest
site <- sites[5]

#Plot interactive polt
#dygraph_ts_fun(waterLevel, site)

#Create temporary 
temp <- events %>% 
  filter(site_id == site) %>% 
  select(SampleDate) %>% 
  mutate(StartEvent = NA, EndEvent = NA)
temp
event_plot_fun(site, drying_events = temp[2,])

#Mannualy Define Storm
#Event 1
temp$StartEvent[3] <- NA
temp$EndEvent[3]   <- NA
fig_1 <- event_plot_fun(site, drying_events = temp[3,])

#Event 2
temp$StartEvent[4] <- "1/1/2016"
temp$EndEvent[4]   <- "7/8/2017"
fig_2 <- event_plot_fun(site, drying_events = temp[4,])

#Create plot
fig_1 + fig_2 
#ggsave(paste0("temp/",site, ".png"))

#Add start and end date to master df
temp$site  <- site
start_stop <- bind_rows(start_stop, temp)

#4.6 903FCPSPx -----------------------------------------------------------------
#Define site of interest
site <- sites[6]

#Create temporary 
temp <- events %>% 
  filter(site_id == site) %>% 
  select(SampleDate) %>% 
  mutate(StartEvent = NA, EndEvent = NA)
temp

#Plot interactive polt
dygraph_ts_fun(waterLevel, site)


#Mannualy Define Storm
#Event 1
temp$StartEvent[1] <- NA
temp$EndEvent[1]   <- NA
fig_1 <- event_plot_fun(site, drying_events = temp[1,])

#Event 2
temp$StartEvent[2] <- NA
temp$EndEvent[2]   <- NA
fig_2 <- event_plot_fun(site, drying_events = temp[2,])

#Create plot
fig_1 + fig_2 
#ggsave(paste0("temp/",site, ".png"))

#Add start and end date to master df
temp$site  <- site
start_stop <- bind_rows(start_stop, temp)

#4.7 903NP9LWF -----------------------------------------------------------------
#Define site of interest
site <- sites[7]

#Create temporary 
temp <- events %>% 
  filter(site_id == site) %>% 
  select(SampleDate) %>% 
  mutate(StartEvent = NA, EndEvent = NA)
temp

#Plot interactive polt
dygraph_ts_fun(waterLevel, site)


#Mannualy Define Storm
#Event 1
temp$StartEvent[1] <- NA
temp$EndEvent[1]   <- NA
fig_1 <- event_plot_fun(site, drying_events = temp[1,])

#Event 2
temp$StartEvent[2] <- NA
temp$EndEvent[2]   <- NA
fig_2 <- event_plot_fun(site, drying_events = temp[2,])

#Create plot
fig_1 + fig_2
#ggsave(paste0("temp/",site, ".png"))

#Add start and end date to master df
temp$site  <- site
start_stop <- bind_rows(start_stop, temp)

#4.8 903NP9LWF -----------------------------------------------------------------
#Define site of interest
site <- sites[8]

#Create temporary 
temp <- events %>% 
  filter(site_id == site) %>% 
  select(SampleDate) %>% 
  mutate(StartEvent = NA, EndEvent = NA)
temp

#Plot interactive polt
dygraph_ts_fun(waterLevel, site)


#Mannualy Define Storm
#Event 1
temp$StartEvent[1] <- NA
temp$EndEvent[1]   <- NA
fig_1 <- event_plot_fun(site, drying_events = temp[1,])

#Event 2
temp$StartEvent[2] <- "3/1/2014"
temp$EndEvent[2]   <- "3/20/2015"
fig_2 <- event_plot_fun(site, drying_events = temp[2,])

#Event 3
temp$StartEvent[3] <- "3/1/2015"
temp$EndEvent[3]   <- "3/21/2016"
fig_3 <- event_plot_fun(site, drying_events = temp[3,])

#Create plot
fig_1 + fig_2 + fig_3
#ggsave(paste0("temp/",site, ".png"))

#Add start and end date to master df
temp$site  <- site
start_stop <- bind_rows(start_stop, temp)

#4.9 903NP9UAC -----------------------------------------------------------------
#Define site of interest
site <- sites[9]

#Create temporary 
temp <- events %>% 
  filter(site_id == site) %>% 
  select(SampleDate) %>% 
  mutate(StartEvent = NA, EndEvent = NA)
temp

#Plot interactive polt
dygraph_ts_fun(waterLevel, site)

#Mannualy Define Storm
#Event 1
temp$StartEvent[1] <- NA
temp$EndEvent[1]   <- NA
fig_1 <- event_plot_fun(site, drying_events = temp[1,])

#Event 2
temp$StartEvent[2] <- NA
temp$EndEvent[2]   <- NA
fig_2 <- event_plot_fun(site, drying_events = temp[2,])

#Event 3
temp$StartEvent[3] <- NA
temp$EndEvent[3]   <- NA
fig_3 <- event_plot_fun(site, drying_events = temp[3,])

#Create plot
fig_1 + fig_2
#ggsave(paste0("temp/",site, ".png"))

#Add start and end date to master df
temp$site  <- site
start_stop <- bind_rows(start_stop, temp)

#4.10 903NP9LWF -----------------------------------------------------------------
#Define site of interest
site <- sites[10]

#Create temporary 
temp <- events %>% 
  filter(site_id == site) %>% 
  select(SampleDate) %>% 
  mutate(StartEvent = NA, EndEvent = NA)
temp

#Plot interactive polt
dygraph_ts_fun(waterLevel, site)

#Mannualy Define Storm
#Event 1
temp$StartEvent[1] <- "2/21/2015"
temp$EndEvent[1]   <- "5/28/2015"
fig_1 <- event_plot_fun(site, drying_events = temp[1,])

#Event 2
temp$StartEvent[2] <- NA
temp$EndEvent[2]   <- NA
fig_2 <- event_plot_fun(site, drying_events = temp[2,])

#Create plot
fig_1 + fig_2
#ggsave(paste0("temp/",site, ".png"))

#Add start and end date to master df
temp$site  <- site
start_stop <- bind_rows(start_stop, temp)

#4.11 903WE0798 -----------------------------------------------------------------
#Define site of interest
site <- sites[11]

#Create temporary 
temp <- events %>% 
  filter(site_id == site) %>% 
  select(SampleDate) %>% 
  mutate(StartEvent = NA, EndEvent = NA)
temp

#Plot interactive polt
dygraph_ts_fun(waterLevel, site)

#Mannualy Define Storm
#Event 1
temp$StartEvent[1] <- NA
temp$EndEvent[1]   <- NA
fig_1 <- event_plot_fun(site, drying_events = temp[1,])

#Event 2
temp$StartEvent[2] <- "4/8/2015"
temp$EndEvent[2]   <- "6/26/2016"
fig_2 <- event_plot_fun(site, drying_events = temp[2,])

#Create plot
fig_1 + fig_2
#ggsave(paste0("temp/",site, ".png"))

#Add start and end date to master df
temp$site  <- site
start_stop <- bind_rows(start_stop, temp)

#4.12 903WE0900 -----------------------------------------------------------------
#Define site of interest
site <- sites[12]

#Create temporary 
temp <- events %>% 
  filter(site_id == site) %>% 
  select(SampleDate) %>% 
  mutate(StartEvent = NA, EndEvent = NA)
temp

#Plot interactive polt
dygraph_ts_fun(waterLevel, site)

#Mannualy Define Storm
#Event 1
temp$StartEvent[1] <- NA
temp$EndEvent[1]   <- NA
fig_1 <- event_plot_fun(site, drying_events = temp[1,])

#Event 2
temp$StartEvent[2] <- "4/9/2015"
temp$EndEvent[2]   <- "3/22/2016"
fig_2 <- event_plot_fun(site, drying_events = temp[2,])

#Create plot
fig_1 + fig_2
#ggsave(paste0("temp/",site, ".png"))

#Add start and end date to master df
temp$site  <- site
start_stop <- bind_rows(start_stop, temp)

#4.13 905DGCC1x -----------------------------------------------------------------
#Define site of interest
site <- sites[13]

#Create temporary 
temp <- events %>% 
  filter(site_id == site) %>% 
  select(SampleDate) %>% 
  mutate(StartEvent = NA, EndEvent = NA)
temp

#Plot interactive polt
dygraph_ts_fun(waterLevel, site)

#Mannualy Define Storm
#Event 1
temp$StartEvent[1] <- NA
temp$EndEvent[1]   <- NA
fig_1 <- event_plot_fun(site, drying_events = temp[1,])

#Event 2
temp$StartEvent[2] <- NA
temp$EndEvent[2]   <- NA
fig_2 <- event_plot_fun(site, drying_events = temp[2,])

#Event 3
temp$StartEvent[3] <- "1/1/2016"
temp$EndEvent[3]   <- "6/24/2017"
fig_3 <- event_plot_fun(site, drying_events = temp[3,])

#Create plot
fig_1 + fig_2 + fig_3
#ggsave(paste0("temp/",site, ".png"))

#Add start and end date to master df
temp$site  <- site
start_stop <- bind_rows(start_stop, temp)

#4.14 905SDBDN9 -----------------------------------------------------------------
#Define site of interest
site <- sites[14]

#Create temporary 
temp <- events %>% 
  filter(site_id == site) %>% 
  select(SampleDate) %>% 
  mutate(StartEvent = NA, EndEvent = NA)
temp

#Plot interactive polt
dygraph_ts_fun(waterLevel, site)

#Mannualy Define Storm
#Event 1
temp$StartEvent[1] <- NA
temp$EndEvent[1]   <- NA
fig_1 <- event_plot_fun(site, drying_events = temp[1,])

#Event 2
temp$StartEvent[2] <- "1/1/2015"
temp$EndEvent[2]   <- "3/28/2016"
fig_2 <- event_plot_fun(site, drying_events = temp[2,])

#Event 3
temp$StartEvent[3] <- NA
temp$EndEvent[3]   <- NA
fig_3 <- event_plot_fun(site, drying_events = temp[3,])

#Create plot
fig_1 + fig_2 + fig_3
#ggsave(paste0("temp/",site, ".png"))

#Add start and end date to master df
temp$site  <- site
start_stop <- bind_rows(start_stop, temp)

#4.15 905WE0679 -----------------------------------------------------------------
#Define site of interest
site <- sites[15]

#Create temporary 
temp <- events %>% 
  filter(site_id == site) %>% 
  select(SampleDate) %>% 
  mutate(StartEvent = NA, EndEvent = NA)
temp

#Plot interactive polt
dygraph_ts_fun(waterLevel, site)

#Mannualy Define Storm
#Event 1
temp$StartEvent[1] <- NA
temp$EndEvent[1]   <- NA
fig_1 <- event_plot_fun(site, drying_events = temp[1,])

#Event 2
temp$StartEvent[2] <- NA
temp$EndEvent[2]   <- NA
fig_2 <- event_plot_fun(site, drying_events = temp[2,])

#Event 3
temp$StartEvent[3] <- NA
temp$EndEvent[3]   <- NA
fig_3 <- event_plot_fun(site, drying_events = temp[3,])

#Create plot
fig_1 + fig_2 + fig_3
#ggsave(paste0("temp/",site, ".png"))

#Add start and end date to master df
temp$site  <- site
start_stop <- bind_rows(start_stop, temp)

#4.16 909S00282 -----------------------------------------------------------------
#Define site of interest
site <- sites[16]

#Create temporary 
temp <- events %>% 
  filter(site_id == site) %>% 
  select(SampleDate) %>% 
  mutate(StartEvent = NA, EndEvent = NA)
temp

#Plot interactive polt
dygraph_ts_fun(waterLevel, site)

#Mannualy Define Storm
#Event 1
temp$StartEvent[1] <- NA
temp$EndEvent[1]   <- NA
fig_1 <- event_plot_fun(site, drying_events = temp[1,])

#Event 2
temp$StartEvent[2] <- "2/26/2015"
temp$EndEvent[2]   <- "6/17/2016"
fig_2 <- event_plot_fun(site, drying_events = temp[2,])

#Create plot
fig_1 + fig_2 
#ggsave(paste0("temp/",site, ".png"))

#Add start and end date to master df
temp$site  <- site
start_stop <- bind_rows(start_stop, temp)

#4.17 909SWCASR -----------------------------------------------------------------
#Define site of interest
site <- sites[17]

#Create temporary 
temp <- events %>% 
  filter(site_id == site) %>% 
  select(SampleDate) %>% 
  mutate(StartEvent = NA, EndEvent = NA)
temp

#Plot interactive polt
dygraph_ts_fun(waterLevel, site)

#Mannualy Define Storm
#Event 1
temp$StartEvent[1] <- "3/1/2015"
temp$EndEvent[1]   <- "6/3/2016"
fig_1 <- event_plot_fun(site, drying_events = temp[1,])

#Event 2
temp$StartEvent[2] <- "12/20/2015"
temp$EndEvent[2]   <- "8/5/2017"
fig_2 <- event_plot_fun(site, drying_events = temp[2,])

#Create plot
fig_1 + fig_2 
#ggsave(paste0("temp/",site, ".png"))

#Add start and end date to master df
temp$site  <- site
start_stop <- bind_rows(start_stop, temp)

#4.18 911COPPER -----------------------------------------------------------------
#Define site of interest
site <- sites[18]

#Create temporary 
temp <- events %>% 
  filter(site_id == site) %>% 
  select(SampleDate) %>% 
  mutate(StartEvent = NA, EndEvent = NA)
temp

#Plot interactive polt
dygraph_ts_fun(waterLevel, site)

#Mannualy Define Storm
#Event 1
temp$StartEvent[1] <- NA
temp$EndEvent[1]   <- NA
fig_1 <- event_plot_fun(site, drying_events = temp[1,])

#Event 2
temp$StartEvent[2] <- "12/15/2014"
temp$EndEvent[2]   <- "4/1/2015"
fig_2 <- event_plot_fun(site, drying_events = temp[2,])

#Event 3
temp$StartEvent[3] <- NA
temp$EndEvent[3]   <- NA
fig_3 <- event_plot_fun(site, drying_events = temp[3,])

#Create plot
fig_1 + fig_2 + fig_3
#ggsave(paste0("temp/",site, ".png"))

#Add start and end date to master df
temp$site  <- site
start_stop <- bind_rows(start_stop, temp)

#4.19 911KCKCRx -----------------------------------------------------------------
#Define site of interest
site <- sites[19]

#Create temporary 
temp <- events %>% 
  filter(site_id == site) %>% 
  select(SampleDate) %>% 
  mutate(StartEvent = NA, EndEvent = NA)
temp

#Plot interactive polt
dygraph_ts_fun(waterLevel, site)

#Mannualy Define Storm
#Event 1
temp$StartEvent[1] <- NA
temp$EndEvent[1]   <- NA
fig_1 <- event_plot_fun(site, drying_events = temp[1,])

#Event 2
temp$StartEvent[2] <- NA
temp$EndEvent[2]   <- NA
fig_2 <- event_plot_fun(site, drying_events = temp[2,])

#Event 3
temp$StartEvent[3] <- '2/25/2015'
temp$EndEvent[3]   <- '6/10/2016'
fig_3 <- event_plot_fun(site, drying_events = temp[3,])

#Create plot
fig_1 + fig_2 + fig_3
#ggsave(paste0("temp/",site, ".png"))

#Add start and end date to master df
temp$site  <- site
start_stop <- bind_rows(start_stop, temp)

#4.20 911NP9ATC -----------------------------------------------------------------
#Define site of interest
site <- sites[20]

#Create temporary 
temp <- events %>% 
  filter(site_id == site) %>% 
  select(SampleDate) %>% 
  mutate(StartEvent = NA, EndEvent = NA)
temp

#Plot interactive polt
dygraph_ts_fun(waterLevel, site)

#Mannualy Define Storm
#Event 1
temp$StartEvent[1] <- NA
temp$EndEvent[1]   <- NA
fig_1 <- event_plot_fun(site, drying_events = temp[1,])

#Event 2
temp$StartEvent[2] <- NA
temp$EndEvent[2]   <- NA
fig_2 <- event_plot_fun(site, drying_events = temp[2,])

#Event 3
temp$StartEvent[3] <- '11/5/2013'
temp$EndEvent[3]   <- '6/15/2015'
fig_3 <- event_plot_fun(site, drying_events = temp[3,])

#Create plot
fig_1 + fig_2 + fig_3
#ggsave(paste0("temp/",site, ".png"))

#Add start and end date to master df
temp$site  <- site
start_stop <- bind_rows(start_stop, temp)

#4.21 911NP9UCW -----------------------------------------------------------------
#Define site of interest
site <- sites[21]

#Create temporary 
temp <- events %>% 
  filter(site_id == site) %>% 
  select(SampleDate) %>% 
  mutate(StartEvent = NA, EndEvent = NA)
temp

#Plot interactive polt
dygraph_ts_fun(waterLevel, site)

#Mannualy Define Storm
#Event 1
temp$StartEvent[1] <- NA
temp$EndEvent[1]   <- NA
fig_1 <- event_plot_fun(site, drying_events = temp[1,])

#Event 2
temp$StartEvent[2] <- "11/16/2013"
temp$EndEvent[2]   <- "7/2/2015"
fig_2 <- event_plot_fun(site, drying_events = temp[2,])

#Event 3
temp$StartEvent[3] <- '11/19/2014'
temp$EndEvent[3]   <- '5/31/2016'
fig_3 <- event_plot_fun(site, drying_events = temp[3,])

#Event 4
temp$StartEvent[4] <- '11/2/2015'
temp$EndEvent[4]   <- '6/21/2017'
fig_4 <- event_plot_fun(site, drying_events = temp[4,])


#Create plot
fig_1 + fig_2 + fig_3 + fig_4
#ggsave(paste0("temp/",site, ".png"))

#Add start and end date to master df
temp$site  <- site
start_stop <- bind_rows(start_stop, temp)

#4.22 911S00858 -----------------------------------------------------------------
#Define site of interest
site <- sites[22]

#Create temporary 
temp <- events %>% 
  filter(site_id == site) %>% 
  select(SampleDate) %>% 
  mutate(StartEvent = NA, EndEvent = NA)
temp

#Plot interactive polt
dygraph_ts_fun(waterLevel, site)

#Mannualy Define Storm
#Event 1
temp$StartEvent[1] <- NA
temp$EndEvent[1]   <- NA
fig_1 <- event_plot_fun(site, drying_events = temp[1,])

#Event 2
temp$StartEvent[2] <- "1/6/2016"
temp$EndEvent[2]   <- "9/20/2017"
fig_2 <- event_plot_fun(site, drying_events = temp[2,])

#Create plot
fig_1 + fig_2 
#ggsave(paste0("temp/",site, ".png"))

#Add start and end date to master df
temp$site  <- site
start_stop <- bind_rows(start_stop, temp)

#4.23 911S01142 -----------------------------------------------------------------
#Define site of interest
site <- sites[23]

#Create temporary 
temp <- events %>% 
  filter(site_id == site) %>% 
  select(SampleDate) %>% 
  mutate(StartEvent = NA, EndEvent = NA)
temp

#Plot interactive polt
dygraph_ts_fun(waterLevel, site)

#Mannualy Define Storm
#Event 1
temp$StartEvent[1] <- NA
temp$EndEvent[1]   <- NA
fig_1 <- event_plot_fun(site, drying_events = temp[1,])

#Event 2
temp$StartEvent[2] <- NA
temp$EndEvent[2]   <- NA
fig_2 <- event_plot_fun(site, drying_events = temp[2,])

#Create plot
fig_1 + fig_2 
#ggsave(paste0("temp/",site, ".png"))

#Add start and end date to master df
temp$site  <- site
start_stop <- bind_rows(start_stop, temp)

#4.24 911TJKC1x -----------------------------------------------------------------
#Define site of interest
site <- sites[24]

#Create temporary 
temp <- events %>% 
  filter(site_id == site) %>% 
  select(SampleDate) %>% 
  mutate(StartEvent = NA, EndEvent = NA)
temp

#Plot interactive polt
dygraph_ts_fun(waterLevel, site)

#Mannualy Define Storm
#Event 4
temp$StartEvent[4] <- NA
temp$EndEvent[4]   <- NA
fig_4 <- event_plot_fun(site, drying_events = temp[4,])

#Mannualy Define Storm
#Event 5
temp$StartEvent[5] <- "3/11/2015"
temp$EndEvent[5]   <- "5/31/2016"
fig_5 <- event_plot_fun(site, drying_events = temp[5,])

#Mannualy Define Storm
#Event 5
temp$StartEvent[6] <- "12/22/2015"
temp$EndEvent[6]   <- "9/27/2017"
fig_6 <- event_plot_fun(site, drying_events = temp[6,])

#Create plot
fig_4 + fig_5 + fig_6
#ggsave(paste0("temp/",site, ".png"))

#Add start and end date to master df
temp$site  <- site
start_stop <- bind_rows(start_stop, temp)

#4.25 911TJLCC2 -----------------------------------------------------------------
#Define site of interest
site <- sites[25]

#Create temporary 
temp <- events %>% 
  filter(site_id == site) %>% 
  select(SampleDate) %>% 
  mutate(StartEvent = NA, EndEvent = NA)
temp

#Plot interactive polt
dygraph_ts_fun(waterLevel, site)

#Mannualy Define Storm
#Event 4
temp$StartEvent[4] <- NA
temp$EndEvent[4]   <- NA
fig_4 <- event_plot_fun(site, drying_events = temp[4,])

#Mannualy Define Storm
#Event 5
temp$StartEvent[5] <- "3/11/2015"
temp$EndEvent[5]   <- "5/31/2016"
fig_5 <- event_plot_fun(site, drying_events = temp[5,])

#Mannualy Define Storm
#Event 5
temp$StartEvent[6] <- "12/22/2015"
temp$EndEvent[6]   <- "9/27/2017"
fig_6 <- event_plot_fun(site, drying_events = temp[6,])

#Create plot
fig_4 + fig_5 + fig_6
#ggsave(paste0("temp/",site, ".png"))

#Add start and end date to master df
temp$site  <- site
start_stop <- bind_rows(start_stop, temp)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#5.0 Export --------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Export start and stop
write_csv(start_stop, "docs//event_start_stop.csv")

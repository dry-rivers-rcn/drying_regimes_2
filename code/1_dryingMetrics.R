library(readxl)
path <- "./data/Loflen_Loggers_All_Data.xlsx"
yy = excel_sheets(path = path)


df = read_excel(path,yy[4])

colnames(df) = c("dateTime","temp","cond","waterLevel","last")

### Prelim plot data

plot(df$dateTime,df$temp)
plot(df$dateTime,df$cond)
plot(df$dateTime,df$waterLevel)

df <-
  df %>%
  select(dateTime, waterLevel) %>%
  na.omit() %>%
  mutate(dateTime = lubridate::ymd_hms(dateTime))
  
#For testing
#n<-which(str_detect(files, '14034500'))

#Setup workspace~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Download libraries of interest
library(lubridate)
library(tidyverse)

#Identify inidividual drying events~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Create new collumn with flow data 25% quantile for peak id
df<-df %>%
  #Round to nearest tenth
  mutate(waterLevel = round(waterLevel, 1)) %>%
  #1% quantile thresholds
  mutate(q_peak = if_else(waterLevel>quantile(waterLevel,0.25),  waterLevel, 0))

#Define peaks using slope break method
df<-df %>% 
  #Define forward and backward slope at each point
  mutate(
    slp_b = (q_peak-lag(q_peak))/as.numeric((dateTime-lag(dateTime))), 
    slp_f = (lead(q_peak)-q_peak)/as.numeric(lead(dateTime)-dateTime)
  ) %>% 
  #now flag those derivative changes
  mutate(peak_flag = if_else(slp_b>0.0001 & slp_f<0, 1,0),
         peak_flag = if_else(is.na(peak_flag), 0, peak_flag)) 

#Define initiation of no flow
df<-df %>%   
  #Define individual storm events
  mutate(event_id = cumsum(peak_flag)+1) %>% 
  #Flag initiation of no flow
  mutate(nf_start = if_else(waterLevel== 0 & lag(waterLevel) != 0, 1, 0)) 

p = ggplot()+
  geom_line(data = df,aes(x=dateTime,y = waterLevel))

p = p + geom_point(data = df,
               aes(x=dateTime,y=waterLevel),
               alpha = df$nf_start,
               color="red")
p

#Recession metrics~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

m = 3
#recession_fun<-function(m)
  #Isolate indivdual recession events
  t<-df %>% filter(event_id == m) 
  
  #Convert NA to zero
  t<-t %>% replace_na(list(nf_start=0)) 
  
  #Compute drying regime stats if the following conditions exist
  if(sum(t$nf_start, na.rm=T)!=0 & #there is a dry period in this event
     t$waterLevel[1]!=0 &                   #the event dosn't start with waterLevel=0
     sum(t$waterLevel_peak)!=0){            #there is no peak value     
    
    #Define recession event as the length of time between peak and longest dry 
    #    event before the next peak. 
    #Define all drying event
    t<-t %>% 
      #Number drying events
      mutate(dry_event_id = cumsum(nf_start)) %>% 
      #Remove id number when > 0
      mutate(dry_event_id = if_else(waterLevel>0, 0, dry_event_id)) 
    
    #Define dry date as the start of the longest drying event
    dry_date <- t %>% 
      #Count length of indivdiual drying events
      filter(dry_event_id>0) %>% 
      group_by(dry_event_id) %>% 
      summarise(
        n = n(),
        date = min(date)) %>% 
      #filter to max
      arrange(-n, date) %>% 
      filter(row_number()==1) %>% 
      #isolate just the date
      select(date)
    
    #Dry Date
    t<-t %>% filter(date<=dry_date$date)
    
    #Define event_id
    event_id <- t$event_id[1]
    
    #Define Peak Data
    peak_date <- as.POSIXlt(t$date[1], "%Y-%m-%d")$yday[1]
    peak_value <- t$waterLevel[1]
    peak_quantile <- ecdf(df$waterLevel)(peak_value)
    
    #Define Peak to zero metric
    peak2zero <- nrow(t)
    
    #Create linear model of dQ vs q
    t<- t %>% mutate(dwaterLevel = lag(waterLevel) - waterLevel) %>% filter(dwaterLevel>=0)
    model<-lm(log10(dwaterLevel+0.1)~log10(waterLevel+0.1), data=t)
    
    #Estimate drying rate [note the error catch for low slopes]
    drying_rate <- tryCatch(model$coefficients[2], error = function(e) NA)
    p_value <- tryCatch(summary(model)$coefficients[2,4], error = function(e) NA)
    
    #Create output tibble
    output<-tibble(event_id, peak_date, peak_value, peak_quantile, peak2zero, drying_rate, p_value)
    
  }else{
    output<-tibble(
      event_id = t$event_id[1],
      peak_date = NA,
      peak_value = NA,
      peak_quantile = NA,
      peak2zero = NA,
      drying_rate = NA,
      p_value = NA
    )
  }
  
  #Export 
  output
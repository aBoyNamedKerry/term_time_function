## Create a loop to pull data where dates are in different folders.
## Created 15/02/2018
## Kerry Cella


#create sequence of dates

# our series of dates to loop through
dates<- seq(as.Date("2018-01-15"), as.Date("2018-01-19"), by = "days")


#somewhere to store results - you can't create this in the loop
d_list<- list()


#we create a loop to call data from multiple years from the API
for (i in seq_along(dates)){
  require(data.table)
  
  di<- dates[i]
  
  data_cut<- read.csv(paste0("//lonnetapp01/asddata/EY-Schools-SEND/EYARU/EY Policy-30 hours delivery/30 hours/Dashboards/Data/",di,"/",di,"_graph7_summer_spring_autumn_data.csv"))
  
  
  d = last(data_cut$All_cumulative_codes_inc_exp)
  
  d_list[[i]] <- d
}


values<- unlist(d_list)

values

## With tryCatch

#somewhere to store results - you can't create this in the loop

dates<- seq(as.Date("2018-01-15"), as.Date("2018-02-16"), by = "days")


d_list<- list()


#we create a loop to call data from multiple years from the API
for (i in seq_along(dates)){
  require(data.table)
  
  #tryCatch allows us to move past the errors and 
  tryCatch({
  di<- dates[i]
  
  data_cut<- read.csv(paste0("//lonnetapp01/asddata/EY-Schools-SEND/EYARU/EY Policy-30 hours delivery/30 hours/Dashboards/Data/",di,"/",di,"_graph7_summer_spring_autumn_data.csv"))
  
  
  d = last(data_cut$All_cumulative_codes_inc_exp)
  
  d_list[[i]] <- d}, error = function(i){return(NA)})
}


values<- unlist(d_list)

values

## Other method with filepath instead of try catch


library(tidyverse)
library(lubridate)
today<-Sys.Date()


# our series of dates to loop through
dates<- seq(as.Date("2017-12-31"), as.Date("2018-03-31"), by = "days")


#somewhere to store results - you can't create this in the loop
d_list<- list()
date_list <- list()

d_listSameDateSnapshot <- list()

#date under investigation. Just use a fairly recent date
endDec <- "2017-12-31"

#we create a loop to call data from multiple years from the API
for (i in seq_along(dates)){
  require(data.table)
  
  di<- dates[i]
  
  filepath<- paste0("//lonnetapp01/asddata/EY-Schools-SEND/EYARU/EY Policy-30 hours delivery/30 hours/Dashboards/Data/",di,"/",di,"_graph7_summer_spring_autumn_data.csv")
  if (file.exists(filepath)) #will only add data where it exists or will give nulls
  {
    data_cut<- read.csv(paste0("//lonnetapp01/asddata/EY-Schools-SEND/EYARU/EY Policy-30 hours delivery/30 hours/Dashboards/Data/",di,"/",di,"_graph7_summer_spring_autumn_data.csv"))
    
    #Most recent Graph 7 Data  
    d = last(data_cut$All_cumulative_codes_inc_exp)
    
    #Get Graph 7 data for a snapshot, needs to be fairly recent. The differences can then be used to work out reconf  
    FilteredGraph7Table <- filter(data_cut, ECSDateTime==endDec)
    d2 = as.integer(select(FilteredGraph7Table,All_cumulative_codes_inc_exp))
    
    d_list[[i]] <- d
    d_listSameDateSnapshot[[i]] <- d2
    
    date_list[[i]]<- di
  }
}

#testtable <- data.frame(dates=as.tibble(date_list),cumulativetotal=as.tibble(d_list))


values<- unlist(d_list)
snapshotvalues <- unlist(d_listSameDateSnapshot)
datelist <- as.Date(unlist(date_list),origin = "1970-01-01")

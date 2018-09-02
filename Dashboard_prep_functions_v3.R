##All functions for the daily dashboard  construction
#Last Updated 2017-11-15



la_function <- function(data, LA_field1, LA_field2, cumulative_col,  date_field, term, term_field, date = Sys.Date()){
#function for creating the LA data in the format a step before plotting in the daily dashboard which gives the total of one criteria against the second
#
# Args:
#   data = A data frame to apply the function to based on the ECS data
#   LA_field1 = First local authority field
#   LA_field2 = Second local authority field
#   cumulative_col = Logical variable to apply cumulative function when grouping
#   term = The term(s) to filter on. Should take the values Autumn, Spring or Summer with year _YYYY 
#   term_field = the field that contains the terms if term field is used
#   date = a date to filter the data on for the event date; format must be in R's date form so "YYYY-MM-DD'  
#
# Returns
#   A data frame grouped by Local Authority with additional column calculating total count against total of logical field
  
library(dplyr)
  
  #tranfer the parematers to be used in the function
  date_field <- enquo(date_field)
  LA_field1<- enquo(LA_field1)
  LA_field2<- enquo(LA_field2)
  cumulative_col <- enquo(cumulative_col)
  term_field <- enquo(term_field)
  
  
  if(missing(term)){ # this only applies if the term date is left blank
    
    
    data<- filter(data,(!!date_field)< date)
  
  la_data_1<- data %>% group_by(!!LA_field1) %>%
    summarise(total_group1 = n()) %>%
    arrange(!!LA_field1)
    
  la_data_2<- data %>% group_by(!!LA_field2) %>%
    summarise(total_group2 = sum(!!cumulative_col, na.rm=TRUE)) %>%
    arrange(!!LA_field2)
  
  by = set_names(quo_name(LA_field2), quo_name(LA_field1)) # create the ability to join in left_join
  
  la_data<- left_join(la_data_1, la_data_2, by = by) %>% 
    mutate (total_percent_of_group1 = round(total_group2 / total_group1 *100))
  
  #remove NAs accross the data frame
  la_data[is.na(la_data)] <- 0
  
  return(la_data)
  
  } else { # will implement term filtering 
  
  data<- data<- filter(data,(!!term_field) %in% !!term)
        
  data<- filter(data,(!!date_field)< date)

  la_data_1<- data %>% group_by(!!LA_field1) %>%
  summarise(total_group1 = n()) %>%
  arrange(!!LA_field1)

  la_data_2<- data %>% group_by(!!LA_field2) %>%
  summarise(total_group2 = sum(!!cumulative_col, na.rm=TRUE)) %>%
  arrange(!!LA_field2)

  by = set_names(quo_name(LA_field2), quo_name(LA_field1))

  la_data<- left_join(la_data_1, la_data_2, by = by) %>% 
  mutate (total_percent_of_group1 = round(total_group2 / total_group1 *100))

#remove NAs accross the data frame
  la_data[is.na(la_data)] <- 0
    
    return(la_data)
  }
}

#Test function

#la_function(data=composite_autumn, LA_field1 = OwningLAName, LA_field2 = LastCheckLAName,
            #cumulative_col = Found, date_field = ECSDateTime) 


#la_function(data=composite_autumn, LA_field1 = OwningLAName, LA_field2 = LastCheckLAName,
            #cumulative_col = Found, term = "Spring_2018", term_field = elapsed_term,  date_field = ECSDateTime) 



#for creating the graphs with all data including expired codes or grace periods

date_cumsum_df<- function(data, date_field, col1_name, col2_name, cumulative_col, term_field, term, minimum_date = "2017-04-21") {
#function for creating a dataframe using the data.table package for plotting a cumualtive chart without filtering by date
# requires the user to have data.table package installed.
#
# Args:
#   data = A data frame to apply the function to based on the ECS data
#   date_field = The variable used to group the data by date for cumulative function, must be date class in format "%Y-%m-%d"  
#   col1_name = Character string for the first object being grouped on placed in outputs
#   col2_name = Character string representing the name for the second object group on for outputs
#   cumulative_col = Logical variable to apply cumulative function when grouping
#   term_field = the field that contains the terms if term field is used
#   term = The term(s) to filter data on. Should take the values Autumn, Spring or Summer or combination of the three
#   minimum_date = filter that is applied for a minimum date filter, defaults to "2017-04-21", to align with opening of childcare service.
#   
#
# Returns
#   A data frame grouped by day showing the number of events and checks and the cumulative totals.
#   Currently the output is intended to be merged with the output of the graph_data functions.

  library(dplyr)
  library(lubridate)
  
  #check if date field supplied
  date_check<- deparse(substitute(date_field))# convert to a string field
  
  if(!is.Date(data[[date_check]]) & !is.POSIXt(data[[date_check]])) stop("date_field must be of class Date or POSIXct object of format %Y-%m-%d")
  
  #turn into date format to make grouping by date based on day only
  data[[deparse(substitute(date_field))]] <- as.Date(data[[deparse(substitute(date_field))]])
  
  #Create the fields to parse into dplyr environment for renaming and using
  date_field <- enquo(date_field)
  col_new<- paste0(col1_name)
  col_new2<- paste0(col2_name)
  col1_name <- paste0("cumulative_", col1_name)
  col2_name <- paste0("cumulative_", col2_name)
  cumulative_col <- enquo(cumulative_col)
  term_field <- enquo(term_field)
  

  
  if(missing(term)){ # this only applies if the term date is left blank
    
    
    data %>% group_by(!!date_field) %>% 
      summarise(total1 = n(), total2 = sum(!!cumulative_col, na.rm=TRUE)) %>%
      mutate(!!col1_name := cumsum(total1), !!col2_name := cumsum(total2)) %>%
      ungroup() %>% filter((!!date_field) >= minimum_date) %>%
      rename(!!col_new := total1, !!col_new2 := total2)
    

} else { # will implement term filtering 
    
    
    data %>% filter((!!term_field) %in% term) %>%
      group_by(!!date_field) %>% 
      summarise(total1 = n(), total2 = sum(!!cumulative_col, na.rm=TRUE)) %>%
      mutate(!!col1_name := cumsum(total1), !!col2_name := cumsum(total2)) %>%
      ungroup() %>% filter((!!date_field) >= !!minimum_date) %>%
      rename(!!col_new := total1, !!col_new2 := total2)

    }  
}


## Testing
#test_df<- date_cumsum_df(data = composite_autumn, date_field = ECSDateTime,
                         #col1_name = "codes", col2_name = "checks", cumulative_col = Found,
                         #minimum_date = "2017-04-21")

#test_df2<- date_cumsum_df(data = composite_all, date_field = ECSDateTime,
                         #col1_name = "codes", col2_name = "checks", cumulative_col = Found, 
                         #term_field = elapsed_term, term = "Autumn_2018",
                         #minimum_date = "2017-04-21")


#for creating the graphs including validity end date filter


##Function for creating cumualtive graph data using the processed date instead

graph_pro <- function(data, date_field1, date_field2, col1_name, col2_name, cumulative_col, term_field, term, date = Sys.Date(), join = left_join,
                      minimum_date = "2017-04-21"){
#function for creating a data frame for plotting a cumualtive chart and filters
#In other 'graph' functions checks are based on event days but here the checks are seprated and based on the day the check was made
#Requires the user to have data.table package installed and dplyr
#
# Args:
#   data = A data frame to apply the function to based on the ECS data
#   term = The term(s) to filter on. Should take the values Autumn, Spring or Summer  
#   date = A date to filter the data on for the event date; format must be in R's date format so "YYYY-MM-DD'  
#
# Returns:
#   A data frame grouped by day showing the number of events and checks and the cumulative totals.
#   The key difference is the days the checks are grouped by is based on the check day as opposed to the event / code day
  
  
  require(dplyr)
  require(tidyr)
  require(lubridate)
  
  #check if date field supplied
  date_check<- deparse(substitute(date_field1))# convert to a string field
  
  if(!is.Date(data[[date_check]]) & !is.POSIXt(data[[date_check]])) stop("date_field must be of class Date or POSIXct object of format %Y-%m-%d")
  
  
  #parese the date from the function arguments into function to be used.
  date_field1 <- enquo(date_field1)
  date_field2 <- enquo(date_field2)
  col_new<- paste0(col1_name) # name to be inserted on exit
  col_new2<- paste0(col2_name) # name to be insterted on exit
  col1_name <- paste0("cumulative_", col1_name) 
  col2_name <- paste0("cumulative_", col2_name)
  cumulative_col <- enquo(cumulative_col) # should be a logical column for counting on
  term_field <- enquo(term_field)
  
  if(missing(term)){
    
    data<- data %>% filter((!!date_field1) < date)
    
    data1<- data %>% group_by(!!date_field1) %>% 
      summarise(total1 = n()) %>%
      mutate(!!col1_name := cumsum(total1)) %>%
      ungroup() %>% filter((!!date_field1) >= minimum_date) %>%
      rename(!!col_new := total1)
    
    data2<- data %>% group_by(!!date_field2) %>% 
      summarise(total2 = sum(!!cumulative_col, na.rm=TRUE)) %>%
      mutate(!!col2_name := cumsum(total2)) %>%
      arrange(!!date_field2) %>%
      ungroup() %>% filter((!!date_field2) >= minimum_date) %>%
      rename(!!col_new2 := total2)
    
    by = set_names(quo_name(date_field2), quo_name(date_field1))
    
    composite<- join(data1, data2, by= by) %>%
      fill(!!col_new2, !!col2_name)
    
    return(composite)
    
  } else {
    
    data<- data %>% filter((!!date_field1) < date, !!term_field %in% term)
    
    data1<- data %>% group_by(!!date_field1) %>% 
      summarise(total1 = n()) %>%
      mutate(!!col1_name := cumsum(total1)) %>%
      ungroup() %>% filter((!!date_field1) >= minimum_date) %>%
      rename(!!col_new := total1)
    
    data2<- data %>% group_by(!!date_field2) %>% 
      summarise(total2 = sum(!!cumulative_col, na.rm=TRUE)) %>%
      mutate(!!col2_name := cumsum(total2)) %>%
      arrange(!!date_field2) %>%
      ungroup() %>% filter((!!date_field2) >= minimum_date) %>%
      rename(!!col_new2 := total2)
    
    by = set_names(quo_name(date_field2), quo_name(date_field1))
    
    composite<- join(data1, data2, by= by) %>%
      fill(!!col_new2, !!col2_name)
    
    
    return(composite)
    
  }  
  
  
}


#Test the function
#my_df<- graph_pro(data = composite_all, date_field1 = ECSDateTime, date_field2 = FirstCheckDate,
                  #col1_name = "codes", col2_name = "checks", cumulative_col = Found,
                  #term_field = Term, term = "Autumn")


#spring_la<- composite_spring_all %>% filter(ECSDateTime < "2018-01-12") %>%
  #la_function(data = ., LA_field1 = OwningLAName, LA_field2 = LA_checked,
                        #cumulative_col = Found, date_field = ECSDateTime, term_field = Term,
                        #term = c("Spring", "Autumn"))


## elapsed code function --------------------------------------

elapsed_function <- function(data, date_filter = Sys.Date(), date_field, Found = TRUE, logic_field, term_field, term, grace_end_date, term_end_date){
# function for creating a plot read data frame for a bar plot indicating where codes may have elapsed and attributing a status
#
#
# Args:
#   data = A data frame to apply the function to based on the ECS data
#   date_filter = A date to filter the data on for the event date; format must be in R's date format so "YYYY-MM-DD' 
#   date_field =  The field which is used to filter the data on
#   Found =  A true por false argument indicating whether to use checked only codes from th Found column
#   logic_field = The field used where Found = TRUE in the date filtering
#   term_field = Whee term is needed to be filtered on the filed where the term information is stored
#   term = The term(s) to filter on. Should take the values Autumn, Spring or Summerwith a suffix _2018
#   grace_end_date = A date to filter representing a grace period transition date (usually middle of a term); format must be in R's date format so "YYYY-MM-DD'  
#   term_end_date = A date to filter representing a term end date that works in conjuction withe grcae period; format must be in R's date format so "YYYY-MM-DD'
#  
# Returns:
#   A data frame where codes are allocated a status based on their validity end date and how it interacts with the grace period
#   Status:
#     Red: Danger zone, if the code fails to reconfirm it will no longer be eligbiel for the following term
#     Amber: Risk, the code, if checked, is theoretically within the grace period for the next term and should be allowed to continue but should be reconfirmed
#     Green: The validity end date takes the code into the following term and therefore should be eligible to continue or take up a place.

  require(tidyverse)
  require(data.table)
  
  date_field <- enquo(date_field)
  logic_field <- enquo(logic_field)
  term_field <- enquo(term_field)

  if(Found == TRUE){
  
  data <- data %>% filter((!!date_field) < date_filter, (!!logic_field) == TRUE, (!!term_field) %in% term)
  
  data<- data %>% 
    mutate(Status = ifelse(ValidityEndDate <= grace_end_date, "Danger", 
                           ifelse(ValidityEndDate > grace_end_date & ValidityEndDate<= term_end_date, "Risk","Safe")))
  
  return(data)
  
  } else{
    
    data <- data %>% filter((!!date_field) < date_filter, (!!term_field) %in% term)
    
    data<- data %>% 
      mutate(Status = ifelse(ValidityEndDate <= grace_end_date, "Danger", 
                             ifelse(ValidityEndDate > grace_end_date & ValidityEndDate<= term_end_date, "Risk","Safe")))
    
    return(data)
  }
  
  
  }




## data testing ----------------------------------

#test_la<- la_function(composite)

#test_la_autumn<- la_function(composite_all, term = "Autumn", date = "2017-09-01")

#tail(test_la_autumn %>% arrange(desc(Unique_checks_percent)))

#Elapsed date test  
#my_data<- elapsed_function(data = composite_all, date_filter = "2018-06-12",
                          #date_field = ECSDateTime, 
                           #Found = TRUE, logic_field = Found, term_field = Term, term = c("Summer", "Spring"), grace_end_date = "2018-05-26",
                           #term_end_date = "2018-08-31")

#count(my_data, Status)

## testing function building comparrison ----------

#set.seed(35)

##set up test data frame
#x<- sample(1:100, 10)
#y<- sample(c(TRUE, FALSE), 10, replace = TRUE)
#date<- seq(as.Date("2018-01-01"), as.Date("2018-01-10"), by =1)


#my_df<- data.frame(x = x, y =y, date =date)

#test<- my_df %>% group_by(date) %>% 
  #summarise(total = n(), total_2 = sum(y ==TRUE, na.rm=TRUE)) %>%
  #mutate(cumulative_a = cumsum(total), cumulative_b = cumsum(total_2)) %>%
  #ungroup() %>% filter(date >= "2018-01-03")


## test function

#cumsum_df<- function(data, date_field, cumulative_y, minimum_date = "2017-04-21") {
  
 # date_field <- enquo(date_field)
  #cumulative_y <- enquo(cumulative_y)
 # 
  #data %>% group_by(!!date_field) %>% 
    #summarise(total = n(), total_2 = sum(!!cumulative_y ==TRUE, na.rm=TRUE)) %>%
    #mutate(cumulative_a = cumsum(total), cumulative_b = cumsum(total_2)) %>%
    #ungroup() %>% filter((!!date_field) >= minimum_date)
  
#}

#test2<- cumsum_df(data = my_df, date_field = date, cumulative_y = y, minimum_date = "2018-01-03")


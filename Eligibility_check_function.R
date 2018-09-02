## Script for building a function to identify termly eligibility date
## Created 22/05/2018




term_calc_30h<- function(x, term_start_year = format(Sys.Date(), "%Y")) {
# Function for returning the applicable term a child is eligible for 30 hours based on thir date of birth and the current academic year the term starts
#
#  
# Args
#  x = a vector of dates representing the dates of birth. Must be a character or date vector
#  term_year = the year in which the academic year begins, should be character and one year, e.g. "2016"
#
# Returns
#  A character vector of the term and the year that a child is eligible to start their 30 hour childcare


  require(lubridate)
  require(dplyr)  

#stop if not date object
  if(class(x) != "Date") stop("vector x must be date class object %Y-%m-%d")
  
  
# create term start date

term_start<- as.POSIXlt(as.Date(paste0(term_start_year,"-09-01"))) # create term starting date

## Create the eligibility blocks - uses the lubridate months function

#autumn start
autumn_eleg_start<- term_start-months(48) # equivalent to four years

#autumn eligiblity end
autumn_eleg_end<- term_start-months(36) # equivalent to three years

#spring starts
spring_eleg_start<- term_start-months(36)# equivalent to three years

#spring end
spring_eleg_end<- term_start-months(32) # equivalent to 2 years and 8 months for January

#summer starts
summer_eleg_start<- term_start-months(32) #equivalent to 2 years and 8 months for January

#summer end
summer_eleg_end<- term_start-months(29) #equivalent to 2 years and 5 months for April

#autumn start
autumn_eleg_start_2<- term_start-months(29) # equivalent to four years

#autumn eligiblity end
autumn_eleg_end_2<- term_start-months(24) # equivalent to three years


## Creat the vector output with the case-when function attaching the correct terms and retunring NA when outside of criteria

term<- case_when(x>= autumn_eleg_start & x < autumn_eleg_end ~ paste0("Autumn_", year(term_start)),
                  x>=  spring_eleg_start & x < spring_eleg_end ~ paste0("Spring_", year(term_start + years(1))),
                  x>= summer_eleg_start & x < summer_eleg_end ~ paste0("Summer_", year(term_start + years(1))),
                  x>= autumn_eleg_start_2 & x < autumn_eleg_end_2 ~ paste0("Autumn_", year(term_start + years(1))),
                  TRUE ~ as.character(NA))

return(term)

}



## Some testing----------------------

#dates<- as.Date(c("2014-09-10", "2015-04-11", "2014-05-09", "2015-01-05", "2014-10-11",  "2015-10-07",
                  #"2016-05-10"))


#(eligible_terms<- term_calc_30h(dates, term_start_year = "2018"))


#table(eligible_terms, useNA = "a")

# To show error
#term_calc_30h(1:3)

# to test generally
#term_calc_30h(Sys.Date() - 900, )

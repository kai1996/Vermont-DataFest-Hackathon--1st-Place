###################################################################################################
# This code was written to analyze data from Expedia hotel bookings for Vermont DataFest 2017. 
# The purpose of the analysis was to design assumptions, conduct exploratory analysis, and generate
# vizualizations that provide insights for some business recommendation. 
###################################################################################################



                        ################################
                        ######## IMPORTING DATA ########
                        ################################



# install packages that haven't been installed yet
list.of.packages <- c ("ggplot2", "tidyverse", "readr", "lubridate", "stringr", "maps")
new.packages <- list.of.packages[! (list.of.packages %in% installed.packages ()[, "Package"])]
if (length (new.packages)) install.packages (new.packages)

#Load packages
library (tidyverse)
library (readr)
library (lubridate)
library (stringr)
library (maps)

#Import data (NOTE: Data files were deleted later due to legal reasons)
sample.data <- read_tsv ("Downloads/DataFest2017_Data/data_sampled.txt")
destination.data <- read_tsv ("Downloads/DataFest2017_Data/test.txt")
data <- read_tsv ("Downloads/DataFest2017_Data/data.txt")

  


                        ################################
                        #########   ANALYSIS    ########
                        ################################



###################################################################################### 

#Identifying hourly probability of booking by customer type

hourly.customer <- data %>%
  mutate (visit.day.of.week <- weekdays(date.time)) %>%
  mutate (trip.length <- srch_co -srch_ci) %>%
  filter (trip.length >= 1) %>%
 
  #removed missing values
  filter (is.na (trip.length) == FALSE) %>%
  separate (date.time, into = c ("session.date", "session.time"), sep = " ") %>%
  
  mutate (
    visit.day.of.week <- weekdays (session.date),
    trip.start.day.of.week <- weekdays (srch_ci),
    trip.end.day.of.week <- weekdays (srch_co),
    business.rooms <- ifelse (srch_children_cnt ==0 & srch_adults_cnt == 1, 1, 0)) %>%
  
  mutate (customer.type <- ifelse (
    (trip.start.day.of.week == "Monday" |
     trip.start.day.of.week == "Tuesday" | 
     trip.start.day.of.week == "Wednesday" | 
     trip.start.day.of.week == "Thursday") &
    (trip.end.day.of.week   == "Tuesday" | 
     trip.end.day.of.week == "Wednesday" | 
     trip.end.day.of.week == "Thursday" |
     trip.end.day.of.week == "Friday") &
    trip.length <= 5 &
    (visit.day.of.week != "Saturday" & visit.day.of.week != "Sunday") &
    business.rooms == 1, "Business", "Everyone Else")) %>%
  
  mutate (customer.type <- ifelse (
    (trip.start.day.of.week == "Friday" |trip.start.day.of.week == "Saturday") &
    (trip.end.day.of.week   == "Saturday" | trip.end.day.of.week == "Sunday") &
    search.children.count == 0 &  
    trip.length <= 3, "Weekend Getaway", customer.type)) %>%
  
  mutate (customer.type <- ifelse (
    
    search.children.count > 0, "Family Trip", customer.type)) %>%
    separate (session.time, into = c ("h", "m", "s"), sep = ":") %>%
    select (session.date, h,user.id, visit.day.of.week, has.booked, customer.type) %>%
    distinct () %>%
    group_by (h, visit.day.of.week, customer.type) %>%
  
  mutate (total.visit.count <- n ()) %>%
  group_by (h, visit.day.of.week, has.booked, total.visit.count, customer.type) %>%
  summarize (booking.visit.count <- n ()) %>%
  filter (has.booked == 1) %>%
  
  mutate (proportion.booked <- booking.visit.count/total.visit.count) %>%
  ungroup () %>%
  
  mutate (customer.type <- factor (customer.type, levels  = c ("Business", 
                                                           "Weekend Getaway", 
                                                           "Family Trip", 
                                                           "Everyone Else"))) %>%
  
  mutate (visit.day.of.week <- factor (visit.day.of.week, levels = c ("Sunday",
                                                                "Monday",
                                                                "Tuesday",
                                                                "Wednesday", 
                                                                "Thursday", 
                                                                "Friday", 
                                                                "Saturday"))) %>%
  arrange (desc (visit.day.of.week)) %>%
  arrange (desc (customer.type))




###################################################################################### 


#Plot hourly probability of booking for different customers 

plot.hourly.customer <- hourly.customer %>%
  ggplot (aes (h, group = customer.type)) +
  geom_line (aes (y = proportion.booked, color = customer.type)) +
  facet_grid (.~visit.day.of.week) +
  ggtitle ("Hourly and Daily Booking Probability by Travel Category") +
  xlab ("Hour and Day of Week") +
  ylab ("Probability of Booking") +
  geom_line (aes (y= .1844935)) +
  scale_color_discrete (name = "Travel Category") +
  scale_x_discrete (breaks = c ("06", "12", "18"))
  

###################################################################################### 


#Booking patterns by customer type based on how early they plan the vacation

plantime.customer <- data %>%
  mutate (visit.day.of.week <- weekdays (date.time)) %>%
  mutate (trip.length <- srch_co - srch_ci) %>%
  filter (trip.length >= 1) %>%
  mutate (date.of.visit <- as_date (date.time, tz <- NULL)) %>%
  mutate (plan.time <- srch_ci - date.of.visit) %>%
  mutate (na.pt <- is.na (plan.time) == TRUE)%>%
  mutate (percentage.null <- mean (na.pt)) %>%
  filter (is.na (plan.time) == FALSE) %>%
  mutate (negative.plan.time <- ifelse (plan.time < 0, 1, 0)) %>%
  mutate (percentage.negative.plan.time <- mean (negative.plan.time)) %>%
  # Less than3  of obs are negative, so we drop them
  
  mutate (
    
    visit.day.of.week <- weekdays (session.date),
    trip.start.day.of.week <- weekdays (srch_ci),
    trip.end.day.of.week <- weekdays (srch_co),
    business.rooms <- ifelse (srch_children_cnt ==0 & srch_adults_cnt == 1, 1, 0)) %>%
  
  mutate (customer.type <- ifelse (
    (trip.start.day.of.week == "Monday" |
    trip.start.day.of.week == "Tuesday" | 
    trip.start.day.of.week == "Wednesday" | 
    trip.start.day.of.week == "Thursday") &
   
    (trip.end.day.of.week   == "Tuesday" | 
    trip.end.day.of.week == "Wednesday" | 
    trip.end.day.of.week == "Thursday" |
    trip.end.day.of.week == "Friday") &
    
    trip.length <= 5 &
      (visit.day.of.week != "Saturday" & visit.day.of.week != "Sunday") &
      business.rooms == 1, "Business", "Everyone Else")) %>%
  
  mutate (customer.type <- ifelse (
    (trip.start.day.of.week == "Friday" |
    trip.start.day.of.week == "Saturday") &
    (trip.end.day.of.week   == "Saturday" | 
     trip.end.day.of.week == "Sunday") &
    
    search.children.count == 0 &  
    trip.length <= 3, "Weekend Getaway", customer.type)) %>%
  
  mutate (customer.type <- ifelse (srch_children_cnt > 0, "Family Trip", customer.type)) %>%
  filter (negative.plan.time == 0) %>%
  select (plan.time, has.booked, date.of.visit, user.id, customer.type) %>%
  distinct () %>%
  group_by (plan.time, customer.type) %>%
  mutate (number.of.visits <- n ()) %>%
  group_by (plan.time, has.booked, number.of.visits, customer.type) %>%
  summarize (booking.visit.count <- n ()) %>%
  filter (has.booked == 1) %>%
  mutate (proportion.booked <- booking.visit.count/number.of.visits) %>%
  mutate (customer.type <- factor (customer.type, levels = c ("Business", 
                                                           "Weekend Getaway", 
                                                           "Family Trip", 
                                                           "Everyone Else")))





plot_pt_cust <- plantime.customer %>%
  ggplot (aes (plan.time, proportion.booked, group <- customer.type)) +
  geom_line (aes (color <- customer.type)) +
  xlim (0, 250) +
  ggtitle ("Plan Time and Booking Probability by Travel Category") +
  xlab ("Plan Time") +
  ylab ("Probability of Booking") +
  scale_color_discrete (name <- "Travel Category")
  
  
  
  
###################################################################################### 
  
#Identifying patterns of booking by channel, and later, by customer type

channel.cust.prob <- data %>%
  mutate (visit.day.of.week <- weekdays (date.time)) %>%
  mutate (trip.length <- srch_co - srch_ci) %>%
  filter (trip.length >= 1) %>%
  mutate (date.of.visit <- as_date (date.time, tz <- NULL)) %>%
  mutate (plan.time <- srch_ci - date.of.visit) %>%
  mutate (na.pt <- is.na (plan.time) == TRUE)%>%
  mutate (percentage.null <- mean (na.pt)) %>%
  filter (is.na (plan.time) == FALSE) %>%
  mutate (negative.plan.time <- ifelse (plan.time < 0, 1, 0)) %>%
  mutate (percentage.negative.plan.time <- mean (negative.plan.time)) %>%
  
  
  mutate (
    #visit.day.of.week <- weekdays (session.date),
    trip.start.day.of.week <- weekdays (srch_ci),
    trip.end.day.of.week <- weekdays (srch_co),
    business.rooms <- ifelse (srch_children_cnt == 0 & srch_adults_cnt == 1, 1, 0)) %>%
  
  mutate (customer.type <- ifelse (
    (trip.start.day.of.week == "Monday" |trip.start.day.of.week == "Tuesday" | 
    trip.start.day.of.week == "Wednesday" | trip.start.day.of.week == "Thursday") &
    (trip.end.day.of.week   == "Tuesday" | 
    trip.end.day.of.week == "Wednesday" | 
    trip.end.day.of.week == "Thursday" |
    trip.end.day.of.week == "Friday") &
    trip.length <= 5 &
      
    (visit.day.of.week != "Saturday" & visit.day.of.week != "Sunday") &
    business.rooms == 1, "Business", "Everyone Else")) %>%
  
  mutate (customer.type <- ifelse (
    (trip.start.day.of.week == "Friday" |
    trip.start.day.of.week == "Saturday") &
    (trip.end.day.of.week   == "Saturday" | 
    trip.end.day.of.week == "Sunday") &
    search.children.count == 0 &  
    trip.length <= 3, "Weekend Getaway", customer.type)) %>%
 
  mutate (customer.type <- ifelse (srch_children_cnt > 0, "Family Trip", customer.type)) %>%
  filter (negative.plan.time == 0) %>%
  
  
  
  select (channel, has.booked, date.of.visit, user.id, customer.type) %>%
  distinct () %>%
  group_by (channel, customer.type) %>%
  mutate (number.of.visits <- n ()) %>%
  group_by (channel, has.booked, number.of.visits, customer.type) %>%
  summarize (booking.visit.count<- n ()) %>%
  filter (has.booked == 1) %>%
  mutate (proportion.booked <- booking.visit.count/number.of.visits) %>%
  ungroup () %>%
  mutate (customer.type <- factor (customer.type, levels = c ("Business", 
                                                           "Weekend Getaway", 
                                                           "Family Trip", 
                                                           "Everyone Else"))) %>%
  mutate (channel <- factor (channel)) %>%
  arrange (desc (customer.type))

plot.channel.and.customer.type <- channel.cust.prob %>%
  ggplot (aes (customer.type, proportion.booked, fill <- factor (channel))) +
  geom_bar (stat <- "identity", position <- "dodge", linetype <-10) +
  scale_fill_brewer (palette <- "Spectral", name <- "Channel") +
  xlab ("Travel Category") +
  ylab ("Booking Proabability") +
  ggtitle ("Booking Probability by Travel Category and Channel")
  
  
###################################################################################### 

#World visualization

all_world <- map_data ("world")

destination_plot_world <- data %>%
  filter (country.user.located == "UNITED STATES OF AMERICA") %>%
  filter (has.booked == 1) %>%
  mutate (week <- week (srch_ci)) %>%
  left_join (destination.data, by <- "srch_destination_id") %>%
  mutate (srch.destination.latitude <- as.numeric (srch.destination.latitude)) %>%
  mutate (srch.destination.longitude <- as.numeric (srch.destination.longitude)) %>%
  ggplot () +
  geom_polygon (data = all_world, aes (x = long, y = lat, group = group), colour = "black", fill = "white") +
  geom_point (aes (x = srch.destination.longitude, y = srch.destination.latitude, color = week))+
  scale_colour_gradient (colours = c ("blue", "white","red", "white", "blue"), name = "Week of the Year") +
  ggtitle ("US Booked Destinations, by Season") +
  xlab ("Longitude") +
  ylab ("Latitude")
  


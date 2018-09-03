# install.packages("tidyverse")
library(tidyverse)
library(readr)
library(lubridate)
library(stringr)
library(maps)

#Import data 

sample <- read_tsv("Downloads/DataFest2017_Data/data_sampled.txt")
dest <- read_tsv("Downloads/DataFest2017_Data/dest.txt")
data <- read_tsv("Downloads/DataFest2017_Data/data.txt")


#GEOM_POINT OR GEOM_LINE
#hourly pbook by customer type

hourly_customer <- data %>%
  mutate(visit_dow = weekdays(date_time)) %>%
  mutate(trip_length = srch_co -srch_ci) %>%
  filter(trip_length >= 1) %>%
  #limited trip length >= 1
  
  #removed missing values
  filter(is.na(trip_length) == FALSE) %>%
  separate(date_time, into = c("session_date", "session_time"), sep = " ") %>%
  mutate(
    
    #visit_dow = weekdays(session_date),
         trip_start_dow = weekdays(srch_ci),
         trip_end_dow = weekdays(srch_co),
         business_rooms = ifelse(srch_children_cnt ==0 & srch_adults_cnt == 1, 1, 0)) %>%
  mutate(cust_type = ifelse(
    (trip_start_dow == "Monday" |trip_start_dow == "Tuesday" | trip_start_dow == "Wednesday" | trip_start_dow == "Thursday") &
    (trip_end_dow   == "Tuesday" | trip_end_dow == "Wednesday" | trip_end_dow == "Thursday" |trip_end_dow == "Friday") &
    trip_length <= 5 &
    (visit_dow != "Saturday" & visit_dow != "Sunday") &
    business_rooms == 1, "Business", "Everyone Else")) %>%
  mutate(cust_type = ifelse(
    (trip_start_dow == "Friday" |trip_start_dow == "Saturday") &
    (trip_end_dow   == "Saturday" | trip_end_dow == "Sunday") &
    srch_children_cnt == 0 &  
    trip_length <= 3, "Weekend Getaway", cust_type)) %>%
  mutate(cust_type = ifelse(srch_children_cnt > 0, "Family Trip", cust_type)) %>%
  separate(session_time, into = c("h", "m", "s"), sep = ":") %>%
  select(session_date, h, user_id, visit_dow, is_booking, cust_type) %>%
  distinct() %>%
  group_by(h, visit_dow, cust_type) %>%
  mutate(total_visit_count = n() ) %>%
  group_by(h, visit_dow, is_booking, total_visit_count, cust_type) %>%
  summarize(booking_visit_count = n()) %>%
  filter(is_booking == 1) %>%
  mutate(booking_pct = booking_visit_count/total_visit_count) %>%
  ungroup() %>%
  mutate(cust_type = factor(cust_type, levels  = c("Business", "Weekend Getaway", "Family Trip", "Everyone Else"))) %>%
  mutate(visit_dow = factor(visit_dow, levels=c('Sunday','Monday','Tuesday','Wednesday', "Thursday", "Friday", "Saturday"))) %>%
  arrange(desc(visit_dow)) %>%
  arrange(desc(cust_type))


#plot hourly customer pbook
plot_hourly_customer2 <- hourly_customer %>%
  ggplot(aes(h, group = cust_type)) +
  geom_line(aes(y = booking_pct, color = cust_type)) +
  facet_grid(.~visit_dow) +
  ggtitle("Hourly and Daily Booking Probability by Travel Category") +
  xlab("Hour and Day of Week") +
  ylab("Probability of Booking") +
  geom_line(aes(y= .1844935)) +
  scale_color_discrete(name = "Travel Category") +
  scale_x_discrete(breaks = c("06", "12", "18"))
  
#lines by customer type
#mean line
###########################################################

#plantime booking pct by customer
plantime_customer <- data %>%
  mutate(visit_dow = weekdays(date_time)) %>%
  mutate(trip_length = srch_co -srch_ci) %>%
  filter(trip_length >= 1) %>%
  mutate(visit_date = as_date(date_time, tz = NULL)) %>%
  mutate(plantime = srch_ci - visit_date) %>%
  mutate(na_pt = is.na(plantime) == TRUE)%>%
  mutate(percentageofnull = mean(na_pt)) %>%
  filter(is.na(plantime) == FALSE) %>%
  mutate(negative_pt = ifelse(plantime < 0, 1, 0)) %>%
  mutate(percentageofnegative = mean(negative_pt)) %>%
  # Less than3  of obs are negative, so we drop them
  # percentageofnull NA values
  mutate(
    
    #visit_dow = weekdays(session_date),
    trip_start_dow = weekdays(srch_ci),
    trip_end_dow = weekdays(srch_co),
    business_rooms = ifelse(srch_children_cnt ==0 & srch_adults_cnt == 1, 1, 0)) %>%
  mutate(cust_type = ifelse(
    (trip_start_dow == "Monday" |trip_start_dow == "Tuesday" | trip_start_dow == "Wednesday" | trip_start_dow == "Thursday") &
      (trip_end_dow   == "Tuesday" | trip_end_dow == "Wednesday" | trip_end_dow == "Thursday" |trip_end_dow == "Friday") &
      trip_length <= 5 &
      (visit_dow != "Saturday" & visit_dow != "Sunday") &
      business_rooms == 1, "Business", "Everyone Else")) %>%
  mutate(cust_type = ifelse(
    (trip_start_dow == "Friday" |trip_start_dow == "Saturday") &
      (trip_end_dow   == "Saturday" | trip_end_dow == "Sunday") &
      srch_children_cnt == 0 &  
      trip_length <= 3, "Weekend Getaway", cust_type)) %>%
  mutate(cust_type = ifelse(srch_children_cnt > 0, "Family Trip", cust_type)) %>%
  filter(negative_pt == 0) %>%
  select(plantime, is_booking, visit_date, user_id, cust_type) %>%
  distinct() %>%
  group_by(plantime, cust_type) %>%
  mutate(visit_count = n()) %>%
  group_by(plantime, is_booking, visit_count, cust_type) %>%
  summarize(booking_visit_count = n()) %>%
  filter(is_booking == 1) %>%
  mutate(booking_pct = booking_visit_count/visit_count) %>%
  mutate(cust_type = factor(cust_type, levels  = c("Business", "Weekend Getaway", "Family Trip", "Everyone Else")))

plot_pt_cust <- plantime_customer %>%
  ggplot(aes(plantime, booking_pct, group = cust_type)) +
  geom_line(aes(color = cust_type)) +
  xlim(0, 250) +
  ggtitle("Plan Time and Booking Probability by Travel Category") +
  xlab("Plan Time") +
  ylab("Probability of Booking") +
  scale_color_discrete(name = "Travel Category")
  
  
  
  
############################################################################  
  
#Probability of booking by channel, and later, customer typ
channel_cust_prob <- data %>%
  mutate(visit_dow = weekdays(date_time)) %>%
  mutate(trip_length = srch_co -srch_ci) %>%
  filter(trip_length >= 1) %>%
  mutate(visit_date = as_date(date_time, tz = NULL)) %>%
  mutate(plantime = srch_ci - visit_date) %>%
  mutate(na_pt = is.na(plantime) == TRUE)%>%
  mutate(percentageofnull = mean(na_pt)) %>%
  filter(is.na(plantime) == FALSE) %>%
  mutate(negative_pt = ifelse(plantime < 0, 1, 0)) %>%
  mutate(percentageofnegative = mean(negative_pt)) %>%
  # Less than3  of obs are negative, so we drop them
  # percentageofnull NA values, we also dr
  mutate(
    #visit_dow = weekdays(session_date),
    trip_start_dow = weekdays(srch_ci),
    trip_end_dow = weekdays(srch_co),
    business_rooms = ifelse(srch_children_cnt ==0 & srch_adults_cnt == 1, 1, 0)) %>%
  mutate(cust_type = ifelse(
    (trip_start_dow == "Monday" |trip_start_dow == "Tuesday" | trip_start_dow == "Wednesday" | trip_start_dow == "Thursday") &
      (trip_end_dow   == "Tuesday" | trip_end_dow == "Wednesday" | trip_end_dow == "Thursday" |trip_end_dow == "Friday") &
      trip_length <= 5 &
      (visit_dow != "Saturday" & visit_dow != "Sunday") &
      business_rooms == 1, "Business", "Everyone Else")) %>%
  mutate(cust_type = ifelse(
    (trip_start_dow == "Friday" |trip_start_dow == "Saturday") &
      (trip_end_dow   == "Saturday" | trip_end_dow == "Sunday") &
      srch_children_cnt == 0 &  
      trip_length <= 3, "Weekend Getaway", cust_type)) %>%
  mutate(cust_type = ifelse(srch_children_cnt > 0, "Family Trip", cust_type)) %>%
  filter(negative_pt == 0) %>%
  
  
  #What do we do do with similar time visits?
  select(channel, is_booking, visit_date, user_id, cust_type) %>%
  distinct() %>%
  group_by(channel, cust_type) %>%
  mutate(visit_count = n()) %>%
  group_by(channel, is_booking, visit_count, cust_type) %>%
  summarize(booking_visit_count= n()) %>%
  filter(is_booking == 1) %>%
  mutate(booking_pct = booking_visit_count/visit_count) %>%
  ungroup() %>%
  mutate(cust_type = factor(cust_type, levels  = c("Business", "Weekend Getaway", "Family Trip", "Everyone Else"))) %>%
  mutate(channel = factor(channel)) %>%
  arrange(desc(cust_type))

plot_channel_cust <- channel_cust_prob %>%
  ggplot(aes(cust_type, booking_pct, fill = factor(channel))) +
  geom_bar(stat = "identity", position = "dodge", linetype =10) +
  scale_fill_brewer(palette = "Spectral", name = "Channel") +
  xlab("Travel Category") +
  ylab("Booking Proabability") +
  ggtitle("Booking Probability by Travel Category and Channel")
  
  
###########################################

#Good World visualization
all_world <- map_data("world")

destination_plot_world <- data %>%
  filter(user_location_country == "UNITED STATES OF AMERICA") %>%
  filter(is_booking == 1) %>%
  mutate(week = week(srch_ci)) %>%
  left_join(dest, by = "srch_destination_id") %>%
  mutate(srch_destination_latitude = as.numeric(srch_destination_latitude)) %>%
  mutate(srch_destination_longitude = as.numeric(srch_destination_longitude)) %>%
  ggplot() +
  geom_polygon(data=all_world, aes(x=long, y=lat, group = group), colour="black", fill="white") +
  geom_point(aes(x = srch_destination_longitude, y = srch_destination_latitude, color = week))+
  scale_colour_gradientn(colours = c("blue", "white","red", "white", "blue"), name = "Week of the Year") +
  ggtitle("US Booked Destinations, by Season") +
  xlab("Longitude") +
  ylab("Latitude")
  


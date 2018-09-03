# Understanding useful trends for Price Discrimination

Goal: Increase Expedia’s producer surplus through the mechanism of price discrimination

Approach:
1.	Categorize Travels (e.g. business, family, weekend getaway)
2.	Find distinct patterns of the probability of booking per travel category of characteristics 
    such as channel, booking time, plan time, and destination
3.	Business recommendation: Discriminate advertisements and offers/deals to increase 
    probability of bookings across the different travel categories


Definitions:
•	Travel Category:
1.	Business Trip (4.2%): Trip made and booked during the week, 1 person per room.
2.	Weekend  Getaway  (17.2%):  For  couples/single  individuals/  friends.  	
    Trips  made  on Saturday or Sunday and No children
3.	Family Trip (17.9%): Any-type and length of trip with Children
4.	Everyone Else (60.7%)

•	Probability of booking: # of people who booked/ # of people who visited Expedia site
•	Plan time: Checking date - booking date



Observations/Results:

1.	Channel: 
    The probability of booking per travel category depends on the type of channel

2.	Booking Time: 
    Identification per travel category of: i) the most and least popular booking days of the week and 
    ii) the most and the least popular weekly booking hours

3.	Plan Time: 
	As plan time increases, the probability of booking decreases. However, for more than 200-days of plan time, 
	the findings are noisy due to the low number of observations.

4.	Popular Destination by season (US bookings): 
	Identification of the popular places by seasonality based on bookings made across US

5.	Hotel Characteristics: 
	No distinct pattern across travel categories, depending on characteristics such as branded, star-rating, 
	banded history price, or package option.


Discussion:

We found distinct patterns of probability of booking and popularity across groups, time, and locations with our analysis. 
This provides Expedia useful information to better target advertising on channels correlating to higher probability of booking. 
Additionally, non-bookers can be incentivized to make a reservation offering a variety of deals, such as price or quantity discounts. 
Based on the above observations we suggest the following deals:

1.	Channel: 
	Better advertise channels with higher probability of booking per travel category and offer deals/incentives if consumers are 
	coming from channels with low probability of booking.

2.	Booking Time: 
	We can target incentives to customers, who are booking at a time (day of the week and hour) where the probability of 
	booking is below the mean probability.

3.	Plan time: 
	Provide more deals as P(B) decreases with increasing plantime.

4.	Destination: 
	Provide deals to destinations that are less popular, depending on the season.

5.	Hotel Characteristics: 
	Not as useful to price discriminate across travel categories using these characteristics (as we could not 
	find clear distinct patterns.

Limitations:

•	We dropped data imperfect observations due to the time constraint of our analysis. However, the percentage of 
    such observations remained low. Therefore, even if we added those observations, our inferences would not significantly vary.

•	Our definition of travel is limited to three main categories: business trip, family trip, and weekend getaway trip. 
    For later analysis we could better define and expand on such categorizations to better describe the 60.7% of the observations 
    coded as “Everyone else” (For instance, include non-family long vacation trips).





This project was a joint effort of Team DataBugs: Kailash Pandey, Ana Sanchez Chico, Ben Czekanski 

Time: April 7 2017(5pm) - April 9 2017(2PM)

Data Source: Expedia Inc (Hotel Bookings)

Data File: 
The data had some made up numbers to prevent the results for being used by unauthorized parties for business decisions. 
The data file isn't available because it was deleted due to legal reasons. 
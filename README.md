# Airline-Bookings

STAT167 Group11 FinalProject:Airline Bookings

Link to Kaggle dataset: https://www.kaggle.com/datasets/anandshaw2001/airlines-booking-csv

By: Brian Uong, AmyLau, EmlynZhai, LindsayPhan, and AdelricLow

 Project Introduction:
 The goal of this research project is to analyze an airline booking dataset from Kaggle to determine what factors influence airline bookings. Specifically, our objective is to determine which factors passengers value 
 most when purchasing an airline ticket. By identifying these factors, we hope to better understand passengers’ decision-making process, which can be valuable for airlines to optimize their services and marketing strategies.
 Through our analyses, we aim to discover patterns in booking behaviors so that we can see which elements impact the passengers’ decision to book a flight.

 ResearchQuestion:
 What factors influence airline booking?
 
 Additional Research Questions:
* Does stay length have any correlation with purchase lead?
* Does sales channel correlate with purchase lead?
* Is there any correlation between booking origin and flight route?
* Does origin of route affect the flight duration?
* Is trip type correlated with flight day?
* Does wanting extra baggage/preferred seats/in-flight meals affect airline booking?
 
Project Description:
To achieve our goal and answer the research questions, we will perform exploratory data analysis and
determine if there is any correlation between the factors in our data set. We will also create two classification
models to predict whether an instance is “yes” or “no” for booking_complete. Naive Bayes and Random
Forest are suitable for our data set because booking_complete is a binary variable. Random Forest will
also allow us to quantify the importance of the variables in our data set and determine which ones are most
influential in predicting whether a booking is completed or not. Our models will be evaluated by calculating
the classification metrics and performing 10-fold cross-validation for each model

Conclusion:
In conclusion, we were able to achieve our project goal of determining what factors influence an airline booking. Although the exploratory data analysis showed some trends in our data, the correlation heat maps showed that there was no significant correlation between the variables and an airline booking, as well as no correlation between the variables themselves. Before creating our classification models, we had to undersample the data to create a more balanced training and test set with equal "yes" and "no" instances for booking_complete. For our models, Naive Bayes with under-sampling had a relatively high sensitivity of 0.69 and a relatively low specificity of 0.72. Random Forest with under-sampling had a sensitivity of 0.62 and a specificity of 0.63. Although both models did not perform very well, when we compared the two directly using classification metrics and the 10-fold cross-validation, we found that the Naive Bayes model proved to be better. However, based on the variable importance metrics from the Random Forest Model, we were also able to determine that passengers value the following factors the most: length_of_stay flight_duration, purchase_lead, flight_hour, and wants_extra_baggage. There were some limitations to our project due to the fact that there are many fewer instances of people completing a booking than not completing a booking in our data set. Despite this, we were still able to identify factors and trends that influenced people to complete bookings.

# Airline-Bookings

STAT167 Group11 FinalProject:Airline Bookings

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

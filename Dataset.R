## Dataset
[Airlines Booking](https://www.kaggle.com/datasets/anandshaw2001/airlines-booking-csv)
```{r, include =FALSE}
# use path for customer_booking file 
bookings <- read.csv("C:/Users/uongb/Documents/School/Junior/Spring/Stat 167/proj/FINAL PROJECT/customer_booking.csv")

bookings
```
```{r}
# install.packages("e1071", dep = TRUE)
# libraries
# install.packages("randomForest")
library(reshape2)
library(ggplot2)
library(tidyverse)
library(pROC)
library(tidyr)
library(dplyr)
library(airportr)
library(maps)
library(gridExtra)
library(ggdendro)
library(vcd)
library(e1071)
library(randomForest)
library(boot)
library(caret)
```
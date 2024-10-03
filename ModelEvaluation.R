## Model Evaluation
In addition to classification evaluation metrics, we have also performed 10 Fold Cross Validation to evaluate our models.

### Naive Bayes 10 Fold Cross Validation 
```{r}
# set seed
set.seed(167)

new_bookings$booking_complete <- as.factor(new_bookings$booking_complete)

# Function for manual 10-fold cross-validation to calculate MSE
cross_validate_naive_bayes_mse <- function(data, k) {
  folds <- createFolds(data$booking_complete, k = k)
  mses <- numeric(k)
  
  for (i in 1:k) {
    test_indices <- folds[[i]]
    train_data <- data[-test_indices, ]
    test_data <- data[test_indices, ]
    
    model <- naiveBayes(booking_complete ~ ., data = train_data)
    predictions <- predict(model, test_data, type = "raw")  # Get probabilities
    # Convert probabilities to numeric predictions (0 or 1)
    numeric_predictions <- as.numeric(predictions[,2] > 0.5)
    numeric_actual <- as.numeric(test_data$booking_complete) - 1  # Convert factor to numeric
    
    mses[i] <- mean((numeric_predictions - numeric_actual)^2)
  }
  
  return(mses)
}

# Run the cross-validation
cv_mse <- cross_validate_naive_bayes_mse(new_bookings, k = 10)
cv.df <- tibble(fold = 1:10, MSE.cv = cv_mse)

ggplot(data = cv.df, mapping = aes(x = fold, y = MSE.cv)) +
geom_point() + geom_line() + ylab("10-fold CV MSE") +
  labs(title = "Naive Bayes (Undersampled) 10-fold CV")
```

### Random Forest 10 Fold Cross Validation 
```{r}
# Function for manual 10-fold cross-validation to calculate MSE on random forest model
cross_validate_random_forest_mse <- function(data, k) {
  folds <- createFolds(data$booking_complete, k = k)
  mses <- numeric(k)
  
  for (i in 1:k) {
    test_indices <- folds[[i]]
    train_data <- data[-test_indices, ]
    test_data <- data[test_indices, ]
    
    model <- randomForest(booking_complete ~ num_passengers + sales_channel + trip_type + 
                         purchase_lead + length_of_stay + flight_hour + flight_day + 
                         wants_extra_baggage + wants_preferred_seat + 
                         wants_in_flight_meals + flight_duration, 
                         data = train_data, ntree = 500, mtry = 3, importance = TRUE)
    # get predictions
    predictions <- predict(model, test_data)  # Get class predictions
    
    # convert factor to numeric
    numeric_predictions <- as.numeric(predictions) - 1  
    numeric_actual <- as.numeric(test_data$booking_complete) - 1  
    
    mses[i] <- mean((numeric_predictions - numeric_actual)^2)
  }
  
  return(mses)
}

# Run the cross-validation
set.seed(167)  # For reproducibility
cv_mse <- cross_validate_random_forest_mse(rf_df, k = 10)
cv.df <- tibble(fold = 1:10, MSE.cv = cv_mse)

ggplot(data = cv.df, mapping = aes(x = fold, y = MSE.cv)) +
geom_point() + geom_line() + ylab("10-fold CV MSE") +
    labs(title = "Random Forest (Undersampled) 10-fold CV")
```
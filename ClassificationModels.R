## Classification Models
Since the data set is very large and is made up of a majority of "no" instances for booking_complete, we decided to use under sampling so that the classification models can be trained on a more balanced data set. We counted the number of completed bookings in the data, and sampled an equal number of instances where the booking was not completed.

### Under Sampled Data
```{r, echo = TRUE}
# set the seed
set.seed(167)

# count number of yes and no for booking complete
num_yes <- sum(bookings$booking_complete == 1)
num_no <- sum(bookings$booking_complete == 0)

# define the number of samples to be taken from each class
n_samples <- num_yes  # Adjust this number based on your dataset

# sample indices for "yes" bookings
yes_indices <- sample(which(bookings$booking_complete == 1), n_samples)

# sample indices for "no" bookings
no_indices <- sample(which(bookings$booking_complete == 0), n_samples)

# combine the sampled indices
sampled_indices <- c(yes_indices, no_indices)

# create the sampled data set
undersampled_data <- bookings[sampled_indices, ]
```

### Naive Bayes
```{r, echo = TRUE}
# mutate the data to change the categorical variables to factors
new_bookings <- undersampled_data |>
  mutate(
  sales_channel = as.factor(sales_channel),
  trip_type = as.factor(trip_type),
  flight_day = as.factor(flight_day),
  route = as.factor(route),
  booking_origin = as.factor(booking_origin)
)

# set the seed
set.seed(167)

# split the data into training and test sets
train_indices <- sample(seq_len(nrow(new_bookings)), size = 0.7 * nrow(new_bookings))
train_data <- new_bookings[train_indices, ]
test_data <- new_bookings[-train_indices, ]

# train the naive bayes model
model <- naiveBayes(booking_complete ~ ., data = train_data)

# predict on the test set
predictions <- predict(model, test_data)
```

### Naive Bayes Model Evaluation
```{r, echo = FALSE}
# create a confusion matrix to evaluate the model
confusion_matrix <- table(predictions, test_data$booking_complete)
confusion_matrix

# calculate accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
cat("Accuracy:", accuracy, "\n")

sensitivity <- 1685 / (1685 + 745)
cat("Sensitivity: ", sensitivity, "\n")

specificity <- 1476 / (1476 + 581)
cat("Specificity: ", specificity, "\n")
```

```{r, echo = FALSE}
# predicted probabilities for the test set
pred_probs <- predict(model, test_data, type = "raw")[,2]

# ROC curve object
roc_curve <- roc(test_data$booking_complete, pred_probs)

# Area Under Curve
auc_value <- auc(roc_curve)

# plot ROC curve
plot(roc_curve, main = paste("ROC Curve for Naive Bayes (AUC =", round(auc_value, 2), ")"))
```

### Random Forest
```{r, echo = TRUE}
# convert relevant columns to factors
rf_df <- undersampled_data |>
  mutate(
    sales_channel = as.factor(sales_channel),
    trip_type = as.factor(trip_type),
    route = as.factor(route),
    booking_origin = as.factor(booking_origin),
    wants_extra_baggage = as.factor(wants_extra_baggage),
    wants_preferred_seat = as.factor(wants_preferred_seat),
    wants_in_flight_meals = as.factor(wants_in_flight_meals),
    booking_complete = as.factor(booking_complete)
  )

# split data into training and testing sets
set.seed(167)
n <- nrow(rf_df)

# randomly sample indices for the training set (70%)
train.idx <- sample(n, size = n * 0.7)

# create the training and test sets
train <- rf_df[train.idx, ]
test <- rf_df[-train.idx, ]

rf_model <- randomForest(booking_complete ~ num_passengers + sales_channel + trip_type + 
                         purchase_lead + length_of_stay + flight_hour + flight_day + 
                         wants_extra_baggage + wants_preferred_seat + 
                         wants_in_flight_meals + flight_duration, 
                         data = train, ntree = 500, mtry = 3, importance = TRUE)

# Make predictions on the test data
rf_predicted_classes <- predict(rf_model, newdata = test)
```
```{r, echo = TRUE}
# # Calculate misclassification rate
# misclassification_rate <- mean(rf_predicted_classes != test$booking_complete)
# # print(paste("Misclassification rate:", misclassification_rate))
# cat("Misclassification rate:", misclassification_rate, "\n\n")

# Create a confusion matrix
confusion_matrix <- table(Actual = test$booking_complete, Predicted = rf_predicted_classes)
print("Confusion Matrix:")
print(confusion_matrix)

# Calculate accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
cat("Accuracy:", accuracy, "\n")

sensitivity <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
specificity <- confusion_matrix[1, 1] / sum(confusion_matrix[1, ])

cat("Sensitivity:", sensitivity, "\n")
cat("Specificity:", specificity, "\n\n")

# Print variable importance
print("Variable Importance:")
print(importance(rf_model))

# Plot variable importance
varImpPlot(rf_model)
```

```{r, echo = FALSE}
# Make predictions on the test data
rf_predicted_probs <- predict(rf_model, newdata = test, type = "prob")[, 2]

# ROC Curve and AUC
roc_curve <- roc(test$booking_complete, rf_predicted_probs, levels = rev(levels(test$booking_complete)))
auc_value <- auc(roc_curve)

cat("AUC:", auc_value, "\n")

# Plot ROC curve
plot(roc_curve, main = paste("ROC Curve for Random Forest (AUC =", round(auc_value, 2), ")"))
```
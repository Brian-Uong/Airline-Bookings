## Limitations

### Model Attempts
```{r, echo = FALSE}
# split data into training and testing sets
set.seed(167)
existingBookings <- na.omit(bookings) %>% select(purchase_lead, length_of_stay)
n <- nrow(existingBookings)

# randomly sample indices for the training set (50%)
train.idx <- sample(n, n / 2)


# create the training and test sets
train <- existingBookings[train.idx, ]
test <- existingBookings[-train.idx, ]

# determining num clusters
res <- c()
iList <- c(1:15)
for (i in iList) {
  res[i] <- kmeans(train, centers = i, nstart = 20)$tot.withinss
}

plot(iList, res)

# result is not useful
result <- kmeans(train, centers = 2, nstart = 20)

plot(train[,1:2], col = result$cluster)
```
## Data Exploration and Visualization
### Number of Passengers vs. Purchase Lead Time
```{r, echo = FALSE}
ggplot(data = bookings, aes(x = reorder(factor(num_passengers), purchase_lead, median), y = purchase_lead)) +
  geom_boxplot(fill = "plum") +
  labs(title = "Number of Passengers vs. Purchase Lead Time",
       x = "Number of Passengers",
       y = "Purchase Lead Time (days)")
```

### Bookings By Flight Hour, Sales Channel, and Booking Status
```{r, echo = FALSE}
num_bookings <- bookings |>
  filter(booking_complete == 1) |>
  group_by(sales_channel) |>
  summarize(num_bookings = n())

# Filter the data to include only complete bookings
complete_bookings <- bookings |>
  filter(booking_complete == 1)

# Create the plot
ggplot(bookings, aes(x = factor(flight_hour), fill = factor(booking_complete))) +
  geom_bar(position = "stack") +
  facet_wrap(~ sales_channel, nrow = 3) +
  labs(x = "Flight Hour",
       y = "Count",
       fill = "Booking Status",
       title = "Bookings by Flight Hour, Sales Channel, and Booking Status") +
  scale_fill_manual(values = c("0" = "steelblue", "1" = "maroon"))
```

### Bookings vs. Hour of Flight Departure
```{r, echo = FALSE}
# aggregate the data to count the number of bookings for each hour
booking_counts_hour <- bookings |>
  filter(booking_complete == 1) |>
  group_by(flight_hour) |>
  summarise(bookings = n())

# plot bookings over hours with a smooth line
ggplot(booking_counts_hour, aes(x = flight_hour, y = bookings)) +
  geom_point(size = 3) +  # Add points
  geom_smooth(method = "loess", color = "blue", se = FALSE) +  # add smooth line
  labs(x = "Hour of Flight Departure", y = "Number of Bookings", title = "Bookings vs. Hour of Flight Departure") +
  theme_minimal()
```

### Complete Bookings by Customer Preference Combination
```{r, echo = FALSE}
# create a new variable for combinations of preferences
preferences_df<- bookings |>
  filter(booking_complete == 1) |>
  mutate(booking_combination = case_when(
    wants_extra_baggage == 1 & wants_preferred_seat == 1 & wants_in_flight_meals == 1 ~ "All",
    wants_extra_baggage == 1 & wants_preferred_seat == 1 ~ "Extra Baggage & Preferred Seat",
    wants_extra_baggage == 1 & wants_in_flight_meals == 1 ~ "Extra Baggage & In-flight Meals",
    wants_preferred_seat == 1 & wants_in_flight_meals == 1 ~ "Preferred Seat & In-flight Meals",
    wants_extra_baggage == 1 ~ "Extra Baggage Only",
    wants_preferred_seat == 1 ~ "Preferred Seat Only",
    wants_in_flight_meals == 1 ~ "In-flight Meals Only",
    TRUE ~ "None"
  )) |>
  group_by(booking_combination) |>
  summarise(total_sales = n()) |>
  arrange(desc(total_sales))

# create a bar plot of sales for combinations of preferences
ggplot(preferences_df, aes(x = reorder(booking_combination, desc(total_sales)), y = total_sales)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(x = "Booking Combination", y = "Number of Complete Bookings", title = "Complete Bookings by Customer Preference Combination") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

### Flight Routes
```{r, echo = TRUE}
route <- data.frame(ogRoute = unique(bookings$route))
routeOrigin <- mutate(route, origin = substr(route$ogRoute, start = 1, stop = 3))
routeDest <- mutate(route, dest = substr(route$ogRoute, start = 4, stop = 6))

mergedOrigin <- merge(x = airports[, c("IATA", "Latitude", "Longitude", "Country")], y = routeOrigin, by.x = "IATA", by.y = "origin")
mergedDest <- merge(x = airports[, c("IATA","Latitude", "Longitude", "Country")], y = routeDest, by.x = "IATA", by.y = "dest")

colnames(mergedOrigin)[1] <- "origin"
colnames(mergedOrigin)[2] <- "originLat"
colnames(mergedOrigin)[3] <- "originLon"
colnames(mergedOrigin)[4] <- "originCountry"
colnames(mergedDest)[1] <- "dest"
colnames(mergedDest)[2] <- "destLat"
colnames(mergedDest)[3] <- "destLon"
colnames(mergedDest)[4] <- "destCountry"

mergedRoute <- merge(mergedOrigin, mergedDest)
mergedAll <- merge(mergedRoute[, c("ogRoute", "originLat", "originLon", "destLat", "destLon", "originCountry", "destCountry")], bookings, by.x = "ogRoute", by.y = "route")

world_map <- map_data("world")
ggplot(data = world_map) + geom_polygon(aes(x = long, y = lat, group = group), fill = "white", color = "black") + geom_curve(data = mergedAll, aes(x =originLon, y = originLat, xend = destLon, yend = destLat, color = trip_type, linetype = trip_type)) +
   labs(title = "Flight Routes") +
  coord_quickmap(xlim = c(-25.0, 193.0), ylim = c(-56.0, 78.0))
```

### Destination Country Frequency
```{r, echo = FALSE}
ggplot(data = mergedAll, mapping = aes(x = destCountry)) + geom_bar() + theme(axis.text.x = element_text(angle = 90)) + labs(x = "Destination Country", title = "Destination Country Frequency")
```

### Correlation Heat Map of Numerical Variables
```{r, echo = FALSE}
numeric_columns <- bookings |>
  select_if(is.numeric)

corr_mat <- round(cor(numeric_columns),2)
melted_corr_mat <- melt(corr_mat)
ggplot(data = melted_corr_mat, aes(x=Var1, y=Var2,
                                   fill=value)) + 
geom_tile() + 
  scale_fill_gradient(low = "white", high = "blue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("Correlation Heatmap for Numerical Variables")
```

### Correlation Heat Map of Categorical Variables
```{r, echo = FALSE}
# define the categorical variables
categorical_vars <- c("trip_type", "flight_day", "route", "booking_origin", "booking_complete")

# function to calculate Cramer's V for a pair of categorical variables
calculate_cramer_v <- function(var1, var2) {
  assocstats(table(bookings[[var1]], bookings[[var2]]))$cramer
}

# initialize an empty matrix to store Cramer's V values
cramer_v_matrix <- matrix(NA, nrow = length(categorical_vars), ncol = length(categorical_vars),
                          dimnames = list(categorical_vars, categorical_vars))

# calculate Cramer's V for each pair of categorical variables
for (i in 1:length(categorical_vars)) {
  for (j in 1:length(categorical_vars)) {
    cramer_v_matrix[i, j] <- calculate_cramer_v(categorical_vars[i], categorical_vars[j])
  }
}

# plot heatmap
library(ggplot2)
ggplot(data = as.data.frame.table(cramer_v_matrix), aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "lavender", high = "purple4") +
  theme_minimal() +
  labs(title = "Correlation Heat Map for Categorical Variables")
```

### Completed Bookings By Destination
```{r, echo = TRUE}
# get complete bookings by destination
completed_bookings_by_dest <- bookings |>
  # create new row for destination
  mutate(dest = substr(route, nchar(route) - 2, nchar(route))) |>
  filter(booking_complete == 1) |>
  group_by(dest) |>
  summarize(total_completed_bookings = n())

# get airport subset of IATA, lat, long, and country
airports_subset <- airports %>%
  select(IATA, Latitude, Longitude, Country)

# merge the completed_bookings_by_dest with airports_subset
merged_data <- completed_bookings_by_dest %>%
  left_join(airports_subset, by = c("dest" = "IATA"))

# check for any missing values and remove them
merged_data <- merged_data %>%
  filter(!is.na(Latitude) & !is.na(Longitude))

head(merged_data)

# get world map
world_map <- map_data("world")

# plot the world map 
ggplot(data = world_map) +
  geom_polygon(aes(x = long, y = lat, group = group), fill = "white", color = "black") +
  # plot points for booking destinations, setting size and color to total completed bookings
  geom_point(data = merged_data, aes(x = Longitude, y = Latitude, size = total_completed_bookings, color = total_completed_bookings), alpha = 0.7) +
  scale_size_continuous(range = c(1, 10)) + 
  # add title
  ggtitle("Completed Bookings by Destination") +
  # set limits for better visualization
  coord_quickmap(xlim = c(-25.0, 193.0), ylim = c(-56.0, 78.0))
```
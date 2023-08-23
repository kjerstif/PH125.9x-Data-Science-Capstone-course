## ----setup,echo = TRUE,warning=FALSE,message=FALSE------------------------------------------------------------------
#########################################################
# Create edx and final_holdout_test sets 
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(gt)) install.packages("gt", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(tidyr)) install.packages("tidyr", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(ggpubr)) install.packages("ggpubr", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")



library(tidyverse)
library(caret)
library(gt)
library(ggplot2)
library(tidyr)
library(scales)
library(knitr)
library(ggpubr)
library(gridExtra)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

options(timeout = 120)

dl <- "ml-10M100K.zip"
if(!file.exists(dl))
  download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings_file <- "ml-10M100K/ratings.dat"
if(!file.exists(ratings_file))
  unzip(dl, ratings_file)

movies_file <- "ml-10M100K/movies.dat"
if(!file.exists(movies_file))
  unzip(dl, movies_file)

ratings <- as.data.frame(str_split(read_lines(ratings_file), fixed("::"), simplify = TRUE),
                         stringsAsFactors = FALSE)
colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")
ratings <- ratings %>%
  mutate(userId = as.integer(userId),
         movieId = as.integer(movieId),
         rating = as.numeric(rating),
         timestamp = as.integer(timestamp))

movies <- as.data.frame(str_split(read_lines(movies_file), fixed("::"), simplify = TRUE),
                        stringsAsFactors = FALSE)
colnames(movies) <- c("movieId", "title", "genres")
movies <- movies %>%
  mutate(movieId = as.integer(movieId))

movielens <- left_join(ratings, movies, by = "movieId")

# Final hold-out test set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
# set.seed(1) # if using R 3.5 or earlier
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in final hold-out test set are also in edx set
final_holdout_test <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from final hold-out test set back into edx set
removed <- anti_join(temp, final_holdout_test)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)
knitr::opts_chunk$set(echo = FALSE,warning=FALSE)


## ----echo = TRUE,warning=FALSE,message=FALSE------------------------------------------------------------------------
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
# set.seed(1) # if using R 3.5 or earlier
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
edx_train <- edx[-test_index,]
edx_temp <- edx[test_index,]

# Make sure userId and movieId in final hold-out test set are also in edx set
edx_test <- edx_temp %>% 
  semi_join(edx_train, by = "movieId") %>%
  semi_join(edx_train, by = "userId")

# Add rows removed from final hold-out test set back into edx set
removed <- anti_join(edx_temp, edx_test)
edx_train <- rbind(edx_train, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed, edx_temp)



## ----edxSummary,out.width="80%"-------------------------------------------------------------------------------------
str(edx)


## ----edxSummary2,out.width="80%"------------------------------------------------------------------------------------
summary(edx)


## ----edxRatings, echo=FALSE,out.width="60%", fig.align='left'-------------------------------------------------------
# Load the ggplot2 library for data visualization
library(ggplot2)

# Plotting a histogram for the 'rating' column of the 'edx' dataset
ggplot(edx, aes(x=rating)) +  # Start the plot specifying 'rating' as the x-variable

  # Add a histogram to the plot with specified design details
  geom_histogram(binwidth=0.5, fill="blue", color="white", alpha=1) +

  # Annotate each bar in the histogram with its count (frequency) value
  stat_count(aes(x = rating, y = ..count.., label = format(..count.., big.mark = " ")), 
             geom = "text", position = position_nudge(y = 30), vjust = -0.5) +

  # Define the title and axis labels of the plot
  labs(title="Distribution of Ratings", x="Rating", y="Frequency") +

  # Apply a minimal theme to the plot
  theme_minimal() +

  # Further customize the plot theme to remove certain elements and grids
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +

  # Define x-axis breaks and remove expansion around the data in the x-axis
  scale_x_continuous(breaks = seq(min(edx$rating, na.rm = TRUE), max(edx$rating, na.rm = TRUE), by = 0.5), 
                     expand = c(0, 0))




## ----edxGenres, echo=FALSE, message=FALSE, out.width="80%", fig.align='left'----------------------------------------


# Summarizing both the rating count and average rating
result <- edx %>%
  separate_rows(genres, sep="\\|") %>%
  group_by(genres) %>%
  summarise(average_rating = mean(rating, na.rm = TRUE),
            rating_count = n())

# Sorting based on rating_count
sorted_result <- result %>%
  arrange(desc(rating_count))

# Plot for Rating Counts by Genre
p1 <- ggplot(sorted_result, aes(x = reorder(genres, rating_count), y = rating_count)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_text(aes(label = comma(rating_count, big.mark = " ")), hjust = -0.1, color = "black", size = 3) + 
  coord_flip() +
  labs(title = "Rating Counts by Genre", x = "", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_y_continuous(limits = c(0, max(sorted_result$rating_count) + 0.11 * max(sorted_result$rating_count)), expand = c(0, 0))

print(p1)

# Plot for Average Rating by Genre
p2 <- ggplot(sorted_result, aes(x = reorder(genres, rating_count), y = average_rating)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = sprintf("%.2f", average_rating)), hjust = -0.1, color = "black", size = 3) + 
  coord_flip() +
  labs(title = "Average Rating by Genre (sorted by Rating Count)", x = "", y = "Average Rating") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

print(p2)



## ----edxtitles, echo=FALSE, message=FALSE, out.width="80%", fig.align='left'----------------------------------------

# Using the 'edx' dataset to find the top ten most rated movies
top_ten_most_rated_movies <- edx %>%

  # Group data by the 'title' column
  group_by(title) %>%

  # Summarize each group to calculate the number of ratings and the average rating
  summarise(
    rating_count = n(),  # Count the number of ratings for each movie
    average_rating = mean(rating, na.rm = TRUE)  # Calculate the average rating for each movie, ignoring NA values
  ) %>%

  # Sort the summarized data in descending order based on the rating count
  arrange(desc(rating_count)) %>%

  # Select the top 10 movies from the sorted list
  head(10)

# Display the top ten most rated movies using 'kable' for a neat tabular format with a caption
kable(top_ten_most_rated_movies, caption = "Top 10 Most Rated Movies with Average Ratings")



## ----edxrank, echo=FALSE, message=FALSE, out.width="80%", fig.align='left'------------------------------------------

# Group data by rating, count frequency, and rank
rating_rank <- edx %>%
  group_by(rating) %>%
  summarise(freq = n()) %>%
  arrange(desc(freq)) %>%
  mutate(rank = row_number())

# Custom label function
comma_format <- function(x) {
  format(x, big.mark = " ")
}

# Create a bar chart
ggplot(rating_rank, aes(x = reorder(rating, freq), y = freq)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_text(aes(label = comma_format(freq)), hjust = 1, color = "white") +  # Use the custom label function
  coord_flip() +
  labs(title = "Frequency of Ratings Ranked", x = "Rating", y = "Frequency") +
  theme_minimal()



## ----edxratingsWhole, echo=FALSE, message=FALSE, out.width="50%", fig.align='left'----------------------------------


# Creating a summary of the ratings based on whether they are whole numbers or not
rating_summary <- edx %>%

  # Add a new column 'is_whole_number' which indicates if a rating is a whole number or not
  mutate(is_whole_number = ifelse(rating == floor(rating), "Whole Number", "Not Whole Number")) %>%

  # Group data by the 'is_whole_number' column
  group_by(is_whole_number) %>%

  # Summarize each group to calculate the count of ratings
  summarise(count = n())

# Visualizing the distribution of whole number ratings versus non-whole number ratings using a pie chart
ggplot(rating_summary, aes(x = "", y = count, fill = is_whole_number)) +  # Setting up the plot with empty x-axis and 'count' as y-axis

  # Add bars to the plot, using the 'count' values to determine bar height
  geom_bar(stat = "identity", width = 1) +

  # Convert the bar chart to a pie chart
  coord_polar(theta = "y") +

  # Define the title and legend labels of the plot
  labs(title = "Distribution of Ratings: Whole vs. Not Whole Number", fill = "Rating Type") +

  # Apply a minimal theme to the plot
  theme_minimal() +

  # Further customize the plot theme to remove certain elements and grids for cleaner pie chart visualization
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank())





## ----edxratingsovertime, echo=FALSE, message=FALSE, out.width="80%", fig.align='left'-------------------------------

# Convert the 'timestamp' column from the 'edx' dataset into a date format
edx$date <- as.Date(as.POSIXct(edx$timestamp, origin="1970-01-01", tz="UTC"))
# The 'timestamp' is assumed to be in a Unix timestamp format. 
# We're converting it to a POSIXct object using an origin of "1970-01-01" (standard for Unix time) and then into a Date object.

# Extract the year from the newly created 'date' column and store it in a new column 'year'
edx$year <- year(edx$date)

# Extract the month as a number from the 'date' column and store it in a new column 'month_number'
edx$month_number <- month(edx$date)

# Extract the month as text (e.g., "Jan" for January) from the 'date' column and store it in a new column 'month_text'
edx$month_text <- month(edx$date, label = TRUE)
# The 'label = TRUE' argument ensures that the month is returned as a textual abbreviation.


# Adjusting the date and extracting the year
edx$date <- as.Date(as.POSIXct(edx$timestamp, origin="1970-01-01", tz="UTC"))
edx$year <- year(edx$date)

# Grouping by year and summarizing
yearly_ratings <- edx %>%
  group_by(year) %>%
  summarise(rating_count = n())

# Creating the plot with ggplot2
p <- ggplot(yearly_ratings, aes(x = as.factor(year), y = rating_count)) +
  geom_line(aes(group = 1), color = "blue", size = 1) +  # Group = 1 ensures the line connects across years
  geom_point(color = "red", size = 1.5) + 
  labs(title = "Number of Ratings per Year", x = "Year", y = "Number of Ratings") +
  theme_minimal() + 
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10), 
                     labels = function(x) format(x, big.mark = " ")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the plot
print(p)




## ----Model_build, include=TRUE,echo=TRUE, message=FALSE, out.width="100%", fig.align='left'-------------------------
# Define a function 'RMSE' to calculate the Root Mean Square Error between true ratings and predicted ratings.
RMSE <- function(true_ratings, predicted_ratings) {
  sqrt(mean((true_ratings - predicted_ratings)^2))
}



## ----Model1_1, include=FALSE,echo=TRUE, message=FALSE, out.width="100%", fig.align='left'---------------------------
mu <- mean(edx_train$rating)
mu


## ----Model1_2,echo=TRUE, message=FALSE, out.width="100%", fig.align='left'------------------------------------------
#Creating the first model
Model1_rmse <- RMSE(edx_test$rating,mu)
model_results <- tibble(Model = "1. Average rating model", RMSE = Model1_rmse)



## ----Model1_2_table,echo=FALSE, message=FALSE, out.width="100%", fig.align='left'-----------------------------------

#Creating the table containing the model results
formatted_table <- model_results %>%
  gt() %>%
  tab_header(
    title = "Model Results",
    subtitle = "Performance Metrics"
  ) %>%
  cols_label(
    Model = "Model Name",
    RMSE = "RMSE"
  ) %>%
  fmt_number(
    columns = c("RMSE"),
    decimals = 4  # Format RMSE with 2 decimal places
  ) %>%
  tab_options(
    heading.title.font.size = 20,
    heading.subtitle.font.size = 16
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(columns = everything())
  )

formatted_table



## ----Model2, echo=FALSE, message=FALSE, out.width="80%", fig.align='left'-------------------------------------------

# Calculate the average bias (b_i) for each movie from the 'edx' dataset
avgs <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))

# Create a histogram plot of the calculated b_i values
hist_plot <- ggplot(avgs, aes(x = b_i)) + 

  # Add bars to the histogram
  geom_histogram(aes(y = ..density..), bins = 10, fill = "blue", color = "black") +

  # Add a density curve to the histogram
  geom_density(alpha = .2, fill = "#FF6666") +

  # Set the title and axis labels for the plot
  labs(
    title = "Histogram of b_i Values",
    x = "b_i Value",
    y = "Density"
  ) +

  # Apply a minimal theme to the plot
  theme_minimal() +

  # Adjust the x-axis text for better readability
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1)
  )

# Print the finalized histogram plot
print(hist_plot)



## ----Model2_1, echo=TRUE, message=FALSE, out.width="80%", fig.align='left'------------------------------------------

# Predict ratings using a baseline and bias for each movie
prediction <- mu + edx_test %>%
  left_join(avgs, by='movieId') %>%  # Join 'edx_test' with 'avgs' on 'movieId'
  pull(b_i)                          # Extract bias values

# Calculate the RMSE between predicted ratings and actual ratings from 'edx_test'
Model2_rmse <- RMSE(prediction, edx_test$rating)



## ----Model2_1_table, echo=FALSE, message=FALSE, out.width="80%", fig.align='left'-----------------------------------


new_row <- tibble(Model = "2. Movie effect model", RMSE = Model2_rmse)

# Remove the existing row for "2. Movie effect model"
model_results <- model_results %>%
  filter(Model != "2. Movie effect model")

# Append the new row
model_results <- bind_rows(model_results, new_row)

library(gt)

formatted_table <- model_results %>%
  gt() %>%
  tab_header(
    title = "Model Results",
    subtitle = "Performance Metrics"
  ) %>%
  cols_label(
    Model = "Model Name",
    RMSE = "RMSE"
  ) %>%
  fmt_number(
    columns = c("RMSE"),
    decimals = 4  # Format RMSE with 2 decimal places
  ) %>%
  tab_options(
    heading.title.font.size = 20,
    heading.subtitle.font.size = 16
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(columns = everything())
  )

formatted_table


## ----Model3_1, echo=FALSE, message=FALSE, out.width="100%", fig.align='left',fig.ncol=2, fig.layout="matrix"--------

# Filter users who have rated 100 or more movies and compute their average rating
user_avg_rating <- edx %>%
  group_by(userId) %>%
  filter(n() >= 100) %>%
  summarize(average_rating = mean(rating))

# Plot the scatterplot of users' average ratings
scatter_plot <- ggplot(user_avg_rating, aes(x = userId, y = average_rating)) +
  geom_point(aes(color = average_rating), size = 3) + 
  geom_hline(aes(yintercept = mean(average_rating)), color = "red", linetype="dashed") +
  labs(title = "Avg Rating Users (100+ Ratings)", x = "User ID", y = "Avg Rating") +
  theme_minimal() +
  scale_color_gradient(low="blue", high="red") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))

# Plot histogram
histogram_plot <- ggplot(user_avg_rating, aes(average_rating)) +
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black") +
  labs(
    title = "Avg Ratings Users(100+ movies)", 
    y = "Number of Users",
    x = "Avg Rating"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10))

# Histogram with density curve
density_plot <- ggplot(user_avg_rating, aes(x=average_rating)) +
  geom_histogram(aes(y=..density..), binwidth=0.1, fill="blue", color="black", alpha=0.7) +
  geom_density(color="red", lwd=1) +
  labs(title="Histogram of Avg Ratings w/ DC", 
       x="Avg Rating", 
       y="Density") +
  theme_minimal()

# Q-Q Plot
qq_plot <- ggqqplot(user_avg_rating$average_rating, 
         ylab = "Average Ratings", 
         xlab = "Theoretical Quantiles") +
  labs(title="Q-Q Plot of Avg Ratings") +
  theme_minimal()

# Displaying plots in a 2x2 grid
library(gridExtra)
grid.arrange(scatter_plot, histogram_plot, density_plot, qq_plot, ncol=2)




## ----Model3_2, echo=TRUE, message=FALSE, out.width="100%", fig.align='left'-----------------------------------------

# Calculate the average rating bias for each user in the 'edx_train' dataset
user_avg_rating_edx_train <- edx_train %>% 
  left_join(avgs, by='movieId') %>%        # Join with 'avgs' to get movie biases (b_i)
  group_by(userId) %>%                     # Group by user
  summarize(b_u=mean(rating - mu - b_i))   # Calculate user bias (b_u) for each user

# Predict ratings for 'edx_test' using global average, movie bias, and user bias
prediction_model3 <- edx_test %>% 
  left_join(avgs, by='movieId') %>%                           # Join with 'avgs' to get movie biases (b_i)
  left_join(user_avg_rating_edx_train, by='userId') %>%       # Join with user biases (b_u)
  mutate(pred_m3 = mu + b_i + b_u) %>%                        # Compute the predicted ratings
  pull(pred_m3)                                               # Extract the predicted values

# Calculate the RMSE between predicted ratings and actual ratings from 'edx_test' for Model 3
model3_rmse <-  RMSE(prediction_model3, edx_test$rating)



## ----Model3_2_table, echo=FALSE, message=FALSE, out.width="100%", fig.align='left'----------------------------------


# Create a new row
new_row <- tibble(Model = "3. Movie and user effect model", RMSE = model3_rmse)

# Remove the existing row for "3. Movie and user effect model"
model_results <- model_results %>%
  filter(Model != "3. Movie and user effect model")


# Append the new row
model_results <- bind_rows(model_results, new_row)

# Display the formatted table using gt
library(gt)

formatted_table <- model_results %>%
  gt() %>%
  tab_header(
    title = "Model Results",
    subtitle = "Performance Metrics"
  ) %>%
  cols_label(
    Model = "Model Name",
    RMSE = "RMSE"
  ) %>%
  fmt_number(
    columns = c("RMSE"),
    decimals = 4  # Format RMSE with 4 decimal places
  ) %>%
  tab_options(
    heading.title.font.size = 20,
    heading.subtitle.font.size = 16
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(columns = everything())
  )

formatted_table





## ----Model4, echo=TRUE, message=FALSE, out.width="100%", fig.align='left'-------------------------------------------
# Initialize a sequence of lambda values from 0 to 10 in increments of 0.1
lambdas <- seq(0, 10, 0.25)

# Use sapply to calculate the RMSE for each lambda value
RMSES <- sapply(lambdas, function(l){
  # Calculate the global mean rating from the edx_train dataset
  edx_train_mu <- mean(edx_train$rating)
  
  # Calculate the bias term b_i for each movie in the edx_train dataset
  b_i <- edx_train %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - edx_train_mu)/(n() + l))
  
  # Calculate the bias term b_u for each user in the edx_train dataset
  b_u <- edx_train %>%
    left_join(b_i, by='movieId') %>% 
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - edx_train_mu)/(n() +l))
  
  # Predict the ratings for the edx_test dataset based on the bias terms b_i and b_u
  predicted_ratings <- edx_test %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = edx_train_mu + b_i +  b_u) %>% .$pred
  
  # Return the RMSE between the predicted ratings and the actual ratings in the edx_test dataset
  return(RMSE(predicted_ratings, edx_test$rating))
})

# Find the lambda value that gives the lowest RMSE
lambda <- lambdas[which.min(RMSES)]
lambda





## ----Model4_plot, echo=FALSE, message=FALSE, out.width="100%", fig.align='left'-------------------------------------

qplot(lambdas,RMSES)


## ----Model4_model, echo=TRUE, message=FALSE, out.width="100%", fig.align='left'-------------------------------------
edx_train_mu <- mean(edx_train$rating)

# Calculate the bias term b_i for each movie in the edx_train dataset
b_i <- edx_train %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - edx_train_mu)/(n()+lambda))

# Calculate the bias term b_u for each user in the edx_train dataset
b_u <-edx_train %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - edx_train_mu)/(n()+lambda))

# Predict the ratings for the edx_test dataset based on the bias terms b_i and b_u
prediction_model4 <- edx_test %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(predictions = edx_train_mu + b_i + b_u) %>% .$predictions

model4_rmse <-RMSE(prediction_model4,edx_test$rating)



## ----ModelSummary, echo=FALSE, message=FALSE, out.width="100%", fig.align='left'------------------------------------
# Create a new row for model 4
new_row_model4 <- tibble(Model = "4. (Regularized) Movie and user effect model", RMSE = model4_rmse)

# Append the new row for model 4
model_results <- bind_rows(model_results, new_row_model4)

# Display the formatted table using gt
library(gt)

formatted_table <- model_results %>%
  gt() %>%
  tab_header(
    title = "Model Results",
    subtitle = "Performance Metrics"
  ) %>%
  cols_label(
    Model = "Model Name",
    RMSE = "RMSE"
  ) %>%
  fmt_number(
    columns = c("RMSE"),
    decimals = 4  # Format RMSE with 4 decimal places
  ) %>%
  tab_options(
    heading.title.font.size = 20,
    heading.subtitle.font.size = 16
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(columns = everything())
  )

formatted_table




## ----Model1_1_final, include=FALSE,echo=TRUE, message=FALSE, out.width="100%", fig.align='left'---------------------
mu_final <- mean(edx$rating)
mu_final


## ----Model1_2_final,echo=TRUE, message=FALSE, out.width="100%", fig.align='left'------------------------------------
#Creating the first model
Model1_rmse_final <- RMSE(final_holdout_test$rating,mu_final)



## ----Model1_2_table_final,echo=FALSE, message=FALSE, out.width="100%", fig.align='left'-----------------------------
#Creating table
model_results <- tibble(
  Model = c("1. Average rating model"),
  RMSE = c(Model1_rmse) 
)


Model1_rmse_final_value <- Model1_rmse_final 



model_results <- model_results %>%
  mutate(RMSE_Final = case_when(
    Model == "1. Average rating model" ~ Model1_rmse_final_value,
    TRUE ~ NA_real_
  ))


formatted_table <- model_results %>%
  gt() %>%
  tab_header(
    title = "Model Results",
    subtitle = "Performance Metrics"
  ) %>%
  cols_label(
    Model = "Model Name",
    RMSE = "RMSE",
    RMSE_Final = "RMSE Final"
  ) %>%
  fmt_number(
    columns = c("RMSE", "RMSE_Final"),
    decimals = 4  # Format RMSE with 2 decimal places
  ) %>%
  tab_options(
    heading.title.font.size = 20,
    heading.subtitle.font.size = 16
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(columns = everything())
  )


formatted_table





## ----Model2_1_final, echo=TRUE, message=FALSE, out.width="80%", fig.align='left'------------------------------------
# Calculate the average bias (b_i_final) for each movie from the 'edx' dataset
avgs_final <- edx %>%
  group_by(movieId) %>%
  summarize(b_i_final = mean(rating - mu_final))  # Compute bias by subtracting global average (mu_final)

# Predict ratings for 'final_holdout_test' using the global average (mu_final) and movie bias (b_i_final)
prediction_final <- mu_final + final_holdout_test %>%
  left_join(avgs_final, by='movieId') %>%         # Join with 'avgs_final' to get the movie biases
  pull(b_i_final)                                 # Extract the movie bias values

# Calculate the RMSE between the predicted ratings and actual ratings from 'final_holdout_test'
Model2_rmse_final <- RMSE(prediction_final, final_holdout_test$rating)




## ----Model2_1_final_table,echo=FALSE, message=FALSE, out.width="100%", fig.align='left'-----------------------------
#Creating table
model_results <- tibble(
  Model = c("1. Average rating model", "2. Movie effect model"),
  RMSE = c(Model1_rmse, Model2_rmse) 
)


Model1_rmse_final_value <- Model1_rmse_final 
Model2_rmse_final_value <- Model2_rmse_final 


model_results <- model_results %>%
  mutate(RMSE_Final = case_when(
    Model == "1. Average rating model" ~ Model1_rmse_final_value,
    Model == "2. Movie effect model" ~ Model2_rmse_final_value,
    TRUE ~ NA_real_
  ))


formatted_table <- model_results %>%
  gt() %>%
  tab_header(
    title = "Model Results",
    subtitle = "Performance Metrics"
  ) %>%
  cols_label(
    Model = "Model Name",
    RMSE = "RMSE",
    RMSE_Final = "RMSE Final"
  ) %>%
  fmt_number(
    columns = c("RMSE", "RMSE_Final"),
    decimals = 4  # Format RMSE with 2 decimal places
  ) %>%
  tab_options(
    heading.title.font.size = 20,
    heading.subtitle.font.size = 16
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(columns = everything())
  )


formatted_table




## ----Model3_2_final, echo=TRUE, message=FALSE, out.width="100%", fig.align='left'-----------------------------------
# Calculate the user-specific bias (b_u_final) from the 'edx' dataset
user_avg_rating_edx <- edx %>% 
  left_join(avgs_final, by='movieId') %>%          # Join with 'avgs_final' to get movie biases (b_i_final)
  group_by(userId) %>%                             # Group by user
  summarize(b_u_final=mean(rating - mu_final - b_i_final))   # Compute user bias by subtracting global average and movie bias

# Predict ratings for 'final_holdout_test' using global average (mu_final), movie bias (b_i_final), and user bias (b_u_final)
prediction_model3_final <- final_holdout_test %>% 
  left_join(avgs_final, by='movieId') %>%                          # Join with 'avgs_final' to get movie biases
  left_join(user_avg_rating_edx, by='userId') %>%                  # Join with user biases
  mutate(pred_m3_final = mu_final + b_i_final + b_u_final) %>%     # Compute the predicted ratings
  pull(pred_m3_final)                                              # Extract the predicted values

# Calculate the RMSE for Model 3 between predicted and actual ratings from 'final_holdout_test'
model3_rmse_final <-  RMSE(prediction_model3_final, final_holdout_test$rating)
model3_rmse_final  # Output the RMSE value



## ----Model3_1_final_table,echo=FALSE, message=FALSE, out.width="100%", fig.align='left'-----------------------------


model_results <- tibble(
  Model = c("1. Average rating model", "2. Movie effect model","3. Movie and user effect model"),
  RMSE = c(Model1_rmse, Model2_rmse,model3_rmse) 
)

Model1_rmse_final_value <- Model1_rmse_final 
Model2_rmse_final_value <- Model2_rmse_final 
Model3_rmse_final_value <- model3_rmse_final

model_results <- model_results %>%
  mutate(RMSE_Final = case_when(
    Model == "1. Average rating model" ~ Model1_rmse_final_value,
    Model == "2. Movie effect model" ~ Model2_rmse_final_value,
    Model == "3. Movie and user effect model" ~ Model3_rmse_final_value,
    TRUE ~ NA_real_
  ))

formatted_table <- model_results %>%
  gt() %>%
  tab_header(
    title = "Model Results",
    subtitle = "Performance Metrics"
  ) %>%
  cols_label(
    Model = "Model Name",
    RMSE = "RMSE",
    RMSE_Final = "RMSE Final"
  ) %>%
  fmt_number(
    columns = c("RMSE", "RMSE_Final"),
    decimals = 4  # Format RMSE with 2 decimal places
  ) %>%
  tab_options(
    heading.title.font.size = 20,
    heading.subtitle.font.size = 16
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(columns = everything())
  )


formatted_table




## ----Model4_model_final, echo=TRUE, message=FALSE, out.width="100%", fig.align='left'-------------------------------
edx_mu <- mean(edx$rating)

# Calculate the bias term b_i for each movie in the edx_train dataset
b_i_final <- edx %>% 
  group_by(movieId) %>%
  summarize(b_i_final = sum(rating - edx_mu)/(n()+lambda))

# Calculate the bias term b_u for each user in the edx_train dataset
b_u_final <-edx %>% 
  left_join(b_i_final, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u_final = sum(rating - b_i_final - edx_mu)/(n()+lambda))

# Predict the ratings for the edx_test dataset based on the bias terms b_i and b_u
prediction_model4_final <- final_holdout_test %>% 
  left_join(b_i_final, by = "movieId") %>%
  left_join(b_u_final, by = "userId") %>%
  mutate(predictions = edx_mu + b_i_final + b_u_final) %>% .$predictions

model4_rmse_final <-RMSE(prediction_model4_final,final_holdout_test$rating)



## ----Model4_1_final_table,echo=FALSE, message=FALSE, out.width="100%", fig.align='left'-----------------------------


model_results <- tibble(
  Model = c("1. Average rating model", "2. Movie effect model","3. Movie and user effect model","4. (Regularized) Movie and user effect model"),
  RMSE = c(Model1_rmse, Model2_rmse,model3_rmse,model4_rmse) 
)

Model1_rmse_final_value <- Model1_rmse_final 
Model2_rmse_final_value <- Model2_rmse_final 
Model3_rmse_final_value <- model3_rmse_final
Model4_rmse_final_value <- model4_rmse_final

model_results <- model_results %>%
  mutate(RMSE_Final = case_when(
    Model == "1. Average rating model" ~ Model1_rmse_final_value,
    Model == "2. Movie effect model" ~ Model2_rmse_final_value,
    Model == "3. Movie and user effect model" ~ Model3_rmse_final_value,
    Model == "4. (Regularized) Movie and user effect model" ~ Model4_rmse_final_value,
    TRUE ~ NA_real_
  ))


formatted_table <- model_results %>%
  gt() %>%
  tab_header(
    title = "Model Results",
    subtitle = "Performance Metrics"
  ) %>%
  cols_label(
    Model = "Model Name",
    RMSE = "RMSE",
    RMSE_Final = "RMSE Final"
  ) %>%
  fmt_number(
    columns = c("RMSE", "RMSE_Final"),
    decimals = 4  # Format RMSE with 2 decimal places
  ) %>%
  tab_options(
    heading.title.font.size = 20,
    heading.subtitle.font.size = 16
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(columns = everything())
  )


formatted_table




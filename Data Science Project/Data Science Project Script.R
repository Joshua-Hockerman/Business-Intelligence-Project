## Step 1: Data Exploration
# Load necessary libraries
library(tidyverse)
library(readxl)

# Read the dataset
video_game_data <- read_excel("~/R Programs/Business Intelligence/Data Science Project/VideoGameData.xlsx")

# Inspect the data types and summary statistics
glimpse(video_game_data)
summary(video_game_data)

# Check for missing values
missing_values <- video_game_data %>% is.na() %>% sum()
cat("Number of missing values:", missing_values, "\n")

## Step 2: Feature Engineering
# Handle missing values (you can choose any method)
# Replace "tbd" values in "User_Rating" with "Meta_Score" divided by 10
video_game_data <- video_game_data %>%
  mutate(User_Rating = ifelse(User_Rating == "tbd", as.character(Meta_Score / 10), User_Rating))

# Convert "User_Rating" column to numeric data type
video_game_data$User_Rating <- as.numeric(video_game_data$User_Rating)

summary(video_game_data)

# Convert date columns to datetime format if necessary
video_game_data$date <- as.Date(video_game_data$Release_Date, format = "%Y-%m-%d")

# Create an "Age" column by subtracting the release year from the current year
video_game_data <- video_game_data %>%
  mutate(Release_Year = as.numeric(format(date, "%Y")),
         Age = as.integer(Sys.Date() %>% format("%Y")) - Release_Year)

# Create logarithmic transformation columns for sales figures
video_game_data <- video_game_data %>%
  mutate(
    log_NA_Sales = log10(NA_Sales + 1),
    log_EU_Sales = log10(EU_Sales + 1),
    log_JP_Sales = log10(JP_Sales + 1),
    log_Other_Sales = log10(Other_Sales + 1),
    log_Global_Sales = log10(Global_Sales + 1)
  )


## Step 3: Visualization
# Load necessary libraries
library(ggplot2)

# Sum the Global_Sales by Genre
sales_by_genre <- video_game_data %>%
  group_by(Genre) %>%
  summarize(Total_Sales = sum(Global_Sales, na.rm = TRUE))

# Visualize the distribution of total game sales by genre
ggplot(sales_by_genre, aes(x = reorder(Genre, -Total_Sales), y = Total_Sales)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(title = "Total Game Sales by Genre",
       x = "Genre",
       y = "Total Sales (Millions)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Sum the Global_Sales by Publisher (selecting top 20 publishers for better visualization)
sales_by_publisher <- video_game_data %>%
  group_by(Publisher) %>%
  summarize(Total_Sales = sum(Global_Sales, na.rm = TRUE)) %>%
  top_n(20, Total_Sales)

# Visualize the distribution of total game sales by publisher
ggplot(sales_by_publisher, aes(x = reorder(Publisher, -Total_Sales), y = Total_Sales)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(title = "Total Game Sales by Publisher (Top 20)",
       x = "Publisher",
       y = "Total Sales (Millions)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
'''
# Create a scatter plot for Meta_Score vs. sales in different regions
ggplot(video_game_data, aes(x = Meta_Score, y = NA_Sales)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  labs(title = "Meta Score vs. North America Sales",
       x = "Meta Score",
       y = "NA Sales (Millions)") +
  theme_minimal()

ggplot(video_game_data, aes(x = Meta_Score, y = EU_Sales)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  labs(title = "Meta Score vs. Europe Sales",
       x = "Meta Score",
       y = "EU Sales (Millions)") +
  theme_minimal()

ggplot(video_game_data, aes(x = Meta_Score, y = JP_Sales)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  labs(title = "Meta Score vs. Japan Sales",
       x = "Meta Score",
       y = "JP Sales (Millions)") +
  theme_minimal()

# Create a scatter plot for User_Rating vs. sales in different regions
ggplot(video_game_data, aes(x = User_Rating, y = NA_Sales)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  labs(title = "User Rating vs. North America Sales",
       x = "User Rating",
       y = "NA Sales (Millions)") +
  theme_minimal()

ggplot(video_game_data, aes(x = User_Rating, y = EU_Sales)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  labs(title = "User Rating vs. Europe Sales",
       x = "User Rating",
       y = "EU Sales (Millions)") +
  theme_minimal()

ggplot(video_game_data, aes(x = User_Rating, y = JP_Sales)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  labs(title = "User Rating vs. Japan Sales",
       x = "User Rating",
       y = "JP Sales (Millions)") +
  theme_minimal()
  '''

# Create a scatter plot for Meta_Score vs. log sales in different regions
ggplot(video_game_data, aes(x = Meta_Score, y = log_NA_Sales)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  labs(title = "Meta Score vs. Log North America Sales",
       x = "Meta Score",
       y = "Log NA Sales") +
  theme_minimal()

ggplot(video_game_data, aes(x = Meta_Score, y = log_EU_Sales)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  labs(title = "Meta Score vs. Log Europe Sales",
       x = "Meta Score",
       y = "Log EU Sales") +
  theme_minimal()

ggplot(video_game_data, aes(x = Meta_Score, y = log_JP_Sales)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  labs(title = "Meta Score vs. Log Japan Sales",
       x = "Meta Score",
       y = "Log JP Sales") +
  theme_minimal()

# Create a scatter plot for User_Rating vs. log sales in different regions
ggplot(video_game_data, aes(x = User_Rating, y = log_NA_Sales)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  labs(title = "User Rating vs. Log North America Sales",
       x = "User Rating",
       y = "Log NA Sales") +
  theme_minimal()

ggplot(video_game_data, aes(x = User_Rating, y = log_EU_Sales)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  labs(title = "User Rating vs. Log Europe Sales",
       x = "User Rating",
       y = "Log EU Sales") +
  theme_minimal()

ggplot(video_game_data, aes(x = User_Rating, y = log_JP_Sales)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  labs(title = "User Rating vs. Log Japan Sales",
       x = "User Rating",
       y = "Log JP Sales") +
  theme_minimal()


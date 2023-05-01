## Step 1: Data Exploration
# Load necessary libraries
library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)
library(corrplot)
library(cor.test)

# Read the dataset
video_game_data <- read_excel("Data Science Project/VideoGameData.xlsx")

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

# Calculate correlations between numeric variables
correlations <- cor(video_game_data %>% 
                      select(Meta_Score, User_Rating, log_NA_Sales, log_EU_Sales, log_JP_Sales, log_Other_Sales, log_Global_Sales))

# Create a correlation heatmap
corrplot(correlations, method = "color", type = "upper", tl.col = "black", addCoef.col = "black")

# Box plot of Meta_Score across genres
ggplot(video_game_data, aes(x = Genre, y = Meta_Score)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Meta Score Distribution by Genre",
       x = "Genre",
       y = "Meta Score") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Box plot of User_Rating across genres
ggplot(video_game_data, aes(x = Genre, y = User_Rating)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "User Rating Distribution by Genre",
       x = "Genre",
       y = "User Rating") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Average Meta_Score by Genre
average_meta_score_by_genre <- video_game_data %>%
  group_by(Genre) %>%
  summarize(Average_Meta_Score = mean(Meta_Score, na.rm = TRUE))

# Bar plot of average Meta_Score across genres
ggplot(average_meta_score_by_genre, aes(x = reorder(Genre, -Average_Meta_Score), y = Average_Meta_Score)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(title = "Average Meta Score by Genre",
       x = "Genre",
       y = "Average Meta Score") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Total log sales by release year
total_log_sales_by_year <- video_game_data %>%
  group_by(Release_Year) %>%
  summarize(Total_Log_Sales = sum(log_Global_Sales, na.rm = TRUE))

# Line plot of total log sales over time
ggplot(total_log_sales_by_year, aes(x = Release_Year, y = Total_Log_Sales)) +
  geom_line(color = "steelblue") +
  theme_minimal() +
  labs(title = "Total Log Sales Over Time",
       x = "Release Year",
       y = "Total Log Sales")

# Scatter plot of Meta_Score vs. log_NA_Sales, facetted by Genre
ggplot(video_game_data, aes(x = Meta_Score, y = log_NA_Sales)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  facet_wrap(~Genre) +
  theme_minimal() +
  labs(title = "Meta Score vs. Log North America Sales by Genre",
       x = "Meta Score",
       y = "Log NA Sales")

# Sales composition by genre
sales_composition_by_genre <- video_game_data %>%
  group_by(Genre) %>%
  summarize(
    Total_Log_NA_Sales = sum(log_NA_Sales, na.rm = TRUE),
    Total_Log_EU_Sales = sum(log_EU_Sales, na.rm = TRUE),
    Total_Log_JP_Sales = sum(log_JP_Sales, na.rm = TRUE),
    Total_Log_Other_Sales = sum(log_Other_Sales, na.rm = TRUE)
  ) %>%
  gather(key = "Region", value = "Total_Log_Sales", -Genre)

# Stacked bar plot of log sales composition by genre
ggplot(sales_composition_by_genre, aes(x = reorder(Genre, -Total_Log_Sales), y = Total_Log_Sales, fill = Region)) +
  geom_bar(stat = "identity", position = "stack") +
  theme_minimal() +
  labs(title = "Log Sales Composition by Genre",
       x = "Genre",
       y = "Total Log Sales") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set1", name = "Region")

# Define the reference date
reference_date <- as.Date("2023-04-30")

# Calculate the age of each game in years
video_game_data <- video_game_data %>%
  mutate(Age_Years = as.numeric(difftime(reference_date, date, units = "weeks")) / 52)

# Calculate the average sales per year for each video game
video_game_data <- video_game_data %>%
  mutate(Average_Sales = log_Global_Sales / Age_Years)

# Box plot of Average_Sales across genres
ggplot(video_game_data, aes(x = Genre, y = Average_Sales)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Average Sales Distribution by Genre",
       x = "Genre",
       y = "Average Sales") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 4. Hypothesis Testing and Correlation Analysis

# Test correlation between user and meta scores with total and average sales
# Correlation test for User_Rating and log_Global_Sales
cor.test(video_game_data$User_Rating, video_game_data$log_Global_Sales)

# Correlation test for User_Rating and Average_Sales
cor.test(video_game_data$User_Rating, video_game_data$Average_Sales)

# Correlation test for Meta_Score and log_Global_Sales
cor.test(video_game_data$Meta_Score, video_game_data$log_Global_Sales)

# Correlation test for Meta_Score and Average_Sales
cor.test(video_game_data$Meta_Score, video_game_data$Average_Sales)

#Test correlation between various genres and total sales numbers
# Create a new column indicating whether the genre is Action, Sports, or Shooter
video_game_data <- video_game_data %>%
  mutate(Genre_Positive = ifelse(Genre %in% c("Action", "Sports", "Shooter"), "Positive", "Other"))

# Perform chi-squared test for the association between Genre_Positive and log_Global_Sales
chisq.test(video_game_data$Genre_Positive, cut(video_game_data$log_Global_Sales, breaks = 4))

# Perform chi-squared test for the association between Genre_Positive and Average_Sales
chisq.test(video_game_data$Genre_Positive, cut(video_game_data$Average_Sales, breaks = 4))

# Create a new column indicating whether the genre is Simulation, Adventure, or Strategy
video_game_data <- video_game_data %>%
  mutate(Genre_Negative = ifelse(Genre %in% c("Simulation", "Adventure", "Strategy"), "Negative", "Other"))

# Perform chi-squared test for the association between Genre_Negative and log_Global_Sales
chisq.test(video_game_data$Genre_Negative, cut(video_game_data$log_Global_Sales, breaks = 4))

# Perform chi-squared test for the association between Genre_Negative and Average_Sales
chisq.test(video_game_data$Genre_Negative, cut(video_game_data$Average_Sales, breaks = 4))

"""
Based on the results of the tests performed, we can interpret the outcomes in the context of your stated hypotheses as follows:

User score is positively correlated with total and average sales.
Both p-values are < 2.2e-16, which is less than the 0.05 significance level. This suggests that there is a significant positive correlation between user score and both total and average sales.
Meta score is positively correlated with total and average sales.
Both p-values are < 2.2e-16, which is less than the 0.05 significance level. This suggests that there is a significant positive correlation between meta score and both total and average sales.
The Action, Sports, and Shooter genres are positively correlated with total and average sales.
The p-value for the association between the Action, Sports, and Shooter genres and total sales (log_Global_Sales) is 0.003126, which is less than the 0.05 significance level. This suggests that there is a significant association between these genres and higher total sales.
The p-value for the association between the Action, Sports, and Shooter genres and average sales (Average_Sales) is 0.07418, which is greater than the 0.05 significance level. This suggests that there is no significant association between these genres and higher average sales.
The Simulation, Adventure, and Strategy genres are negatively correlated with total and average sales.
The p-value for the association between the Simulation, Adventure, and Strategy genres and total sales (log_Global_Sales) is 4.426e-07, which is less than the 0.05 significance level. This suggests that there is a significant association between these genres and lower total sales.
The p-value for the association between the Simulation, Adventure, and Strategy genres and average sales (Average_Sales) is 0.006827, which is less than the 0.05 significance level. This suggests that there is a significant association between these genres and lower average sales.
In summary, your hypotheses regarding user scores, meta scores, and the genres of Simulation, Adventure, and Strategy are supported by the data. The hypothesis regarding the Action, Sports, and Shooter genres is supported for total sales but not for average sales.

"""

# Calculate the average meta score for each publisher
publisher_meta_scores <- video_game_data %>%
  group_by(Publisher) %>%
  summarize(Avg_Meta_Score = mean(Meta_Score, na.rm = TRUE))

# Calculate regional sales as a percentage of total sales for each publisher
publisher_sales <- video_game_data %>%
  group_by(Publisher) %>%
  summarize(
    NA_Sales_Percentage = sum(NA_Sales) / sum(Global_Sales),
    EU_Sales_Percentage = sum(EU_Sales) / sum(Global_Sales),
    JP_Sales_Percentage = sum(JP_Sales) / sum(Global_Sales),
    Other_Sales_Percentage = sum(Other_Sales) / sum(Global_Sales)
  )

# Join the average meta score data with the regional sales percentage data
publisher_data <- inner_join(publisher_meta_scores, publisher_sales, by = "Publisher")

# Select the top 25 publishers based on their average meta scores
top_publishers <- publisher_data %>%
  arrange(desc(Avg_Meta_Score)) %>%
  head(25)

# Create a correlation matrix
cor_matrix <- cor(top_publishers[, -1])

# Plot the correlation heatmap
corrplot(cor_matrix, method = "square", type = "upper", tl.col = "black", tl.srt = 45)
  
# Select the top 25 publishers based on their average meta scores
top_publishers <- publisher_data %>%
  arrange(desc(Avg_Meta_Score)) %>%
  head(25)

# Reshape the data to a long format suitable for ggplot2
top_publishers_long <- top_publishers %>%
  select(Publisher, Avg_Meta_Score, NA_Sales_Percentage, EU_Sales_Percentage, JP_Sales_Percentage, Other_Sales_Percentage) %>%
  gather(key = "Region", value = "Sales_Percentage", -Publisher, -Avg_Meta_Score)

# Create the matrix plot
ggplot(top_publishers_long, aes(x = Region, y = reorder(Publisher, -Avg_Meta_Score), fill = Sales_Percentage)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "steelblue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Region", y = "Publisher", fill = "Sales %", title = "Top 25 Publishers: Regional Sales as a Percentage of Total Sales")


# Calculate Pearson correlation coefficients for each region and meta/user scores
cor_meta_na <- cor.test(video_game_data$Meta_Score, video_game_data$NA_Sales)
cor_meta_eu <- cor.test(video_game_data$Meta_Score, video_game_data$EU_Sales)
cor_meta_jp <- cor.test(video_game_data$Meta_Score, video_game_data$JP_Sales)
cor_meta_other <- cor.test(video_game_data$Meta_Score, video_game_data$Other_Sales)

cor_user_na <- cor.test(video_game_data$User_Rating, video_game_data$NA_Sales)
cor_user_eu <- cor.test(video_game_data$User_Rating, video_game_data$EU_Sales)
cor_user_jp <- cor.test(video_game_data$User_Rating, video_game_data$JP_Sales)
cor_user_other <- cor.test(video_game_data$User_Rating, video_game_data$Other_Sales)

# Print the correlation coefficients and p-values
cat("Meta Score and NA Sales: Correlation =", cor_meta_na$estimate, "P-value =", cor_meta_na$p.value, "\n")
cat("Meta Score and EU Sales: Correlation =", cor_meta_eu$estimate, "P-value =", cor_meta_eu$p.value, "\n")
cat("Meta Score and JP Sales: Correlation =", cor_meta_jp$estimate, "P-value =", cor_meta_jp$p.value, "\n")
cat("Meta Score and Other Sales: Correlation =", cor_meta_other$estimate, "P-value =", cor_meta_other$p.value, "\n")
cat("\n")
cat("User Rating and NA Sales: Correlation =", cor_user_na$estimate, "P-value =", cor_user_na$p.value, "\n")
cat("User Rating and EU Sales: Correlation =", cor_user_eu$estimate, "P-value =", cor_user_eu$p.value, "\n")
cat("User Rating and JP Sales: Correlation =", cor_user_jp$estimate, "P-value =", cor_user_jp$p.value, "\n")
cat("User Rating and Other Sales: Correlation =", cor_user_other$estimate, "P-value =", cor_user_other$p.value, "\n")

"""
Based on these results, it appears that there are some differences in the strength of correlations between ratings (both Meta Score and User Rating) and sales in different regions. However, all of the correlations are positive, indicating that higher ratings are generally associated with higher sales in all regions.

To compare the correlations, let's take a closer look at the correlation coefficients and p-values:

Meta Score correlations:
NA Sales: Correlation = 0.2442, P-value < 0.0001
EU Sales: Correlation = 0.2208, P-value < 0.0001
JP Sales: Correlation = 0.1858, P-value < 0.0001
Other Sales: Correlation = 0.2041, P-value < 0.0001
User Rating correlations:
NA Sales: Correlation = 0.1135, P-value < 0.0001
EU Sales: Correlation = 0.0834, P-value < 0.0001
JP Sales: Correlation = 0.1628, P-value < 0.0001
Other Sales: Correlation = 0.0820, P-value < 0.0001
All the p-values are highly significant (p < 0.0001), which indicates that the correlations are statistically significant and not due to random chance.

Although the correlation coefficients differ somewhat among regions, the differences are not extremely large. Overall, the results suggest that higher ratings are associated with higher sales across all regions, but the strength of this association varies. The association between Meta Score and sales is generally stronger than that between User Rating and sales.

In summary, there are some differences in the strength of correlations between ratings and sales in different regions, but the overall positive relationship between higher ratings and higher sales is consistent across all regions.
"""


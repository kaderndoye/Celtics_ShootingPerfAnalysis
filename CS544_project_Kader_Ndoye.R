library(tidyverse)
library(readr)

# Import datasets
path <- '/Users/kaderndoye/Downloads/CS544/Project/archive'
boxscore_totals <- read_csv(file.path(path, 'celtics_data - boxscore_totals.csv'))
dribbles_shooting <- read_csv(file.path(path, 'celtics_data - dribbles_shooting.csv'))
shot_clock_shooting <- read_csv(file.path(path, 'celtics_data - shot_clock_shooting.csv'))
touch_time_shooting <- read_csv(file.path(path, 'celtics_data - touch_time_shooting.csv'))

# View the structure and summary of each dataset
spec(boxscore_totals)
summary(boxscore_totals)

spec(dribbles_shooting)
summary(dribbles_shooting)

spec(shot_clock_shooting)
summary(shot_clock_shooting)

spec(touch_time_shooting)
summary(touch_time_shooting)


# Check for missing values and clean datasets
missing_values <- sum(is.na(boxscore_totals))
cat("Number of missing values:", missing_values, "\n")
missing_values <- sum(is.na(dribbles_shooting))
cat("Number of missing values:", missing_values, "\n")
missing_values <- sum(is.na(shot_clock_shooting))
cat("Number of missing values:", missing_values, "\n")
missing_values <- sum(is.na(touch_time_shooting))
cat("Number of missing values:", missing_values, "\n")


# Function to replace N/A values in a data frame
replace_na_values <- function(df) {
  df %>% mutate(across(where(is.numeric), ~ replace_na(., mean(., na.rm = TRUE)))) %>%
    mutate(across(where(is.character), ~ replace_na(., "Unknown")))
}

# Replace N/A values in each dataset
boxscore_totals_clean <- replace_na_values(boxscore_totals)
dribbles_shooting_clean <- replace_na_values(dribbles_shooting)
shot_clock_shooting_clean <- replace_na_values(shot_clock_shooting)
touch_time_shooting_clean <- replace_na_values(touch_time_shooting)


# Summary statistics for a numerical variable: points scored by Celtics in boxscore_totals
summary(boxscore_totals_clean$bos_pts)

# Frequency table for a categorical variable: win/loss in boxscore_totals
table(boxscore_totals_clean$w_l)

# Plot distribution of points scored
ggplot(boxscore_totals_clean, aes(x = bos_pts)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(title = "Distribution of Points Scored by Celtics",
       x = "Points", y = "Frequency")

# Plot the distribution of win/loss
ggplot(boxscore_totals_clean, aes(x = w_l)) +
  geom_bar(fill = "green", color = "black") +
  labs(title = "Distribution of Wins and Losses",
       x = "Win/Loss", y = "Count")




# Analysis of a set of two or more variables
# Plot the relationship between 'bos_pts' and 'ftm'
ggplot(boxscore_totals_clean, aes(x = ftm, y = bos_pts)) +
  geom_point(color = "purple", alpha = 0.5) +
  labs(title = "Relationship between Points Scored and Free Throws Made",
       x = "Free Throws Made", y = "Points Scored")

# Adding a regression line to the scatter plot
ggplot(boxscore_totals_clean, aes(x = ftm, y = bos_pts)) +
  geom_point(color = "purple", alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Relationship between Points Scored and Free Throws Made",
       x = "Free Throws Made", y = "Points Scored")



# Central Limit Theorem with random sampling
# Function to draw random samples and plot their means
plot_sample_means <- function(data, variable, sample_sizes, num_samples = 1000) {
  sample_means <- list()
  
  for (size in sample_sizes) {
    means <- replicate(num_samples, mean(sample(data[[variable]], size, replace = TRUE)))
    sample_means[[as.character(size)]] <- data.frame(Mean = means)
  }
  
  sample_means_df <- bind_rows(sample_means, .id = "Sample_Size")
  
  ggplot(sample_means_df, aes(x = Mean)) +
    geom_histogram(binwidth = 0.5, alpha = 0.7, fill = "darkgreen", color = "black") +
    facet_wrap(~ Sample_Size, scales = "free_y", ncol = 1) +
    labs(title = "Distribution of Sample Means for Points Scored (Celtics)",
         x = "Sample Mean", y = "Frequency") +
    theme(legend.position = "none", 
          strip.text = element_text(size = 12),
          plot.title = element_text(hjust = 0.5))
}


sample_sizes <- c(30, 50, 100)
plot_sample_means(boxscore_totals_clean, "bos_pts", sample_sizes)




# Sampling methods
# Simple Random Sampling
set.seed(123)
simple_random_sample <- boxscore_totals_clean %>% sample_n(82)

# Stratified Sampling by 'w_l'
set.seed(123)
stratified_sample <- boxscore_totals_clean %>% 
  group_by(w_l) %>% 
  sample_frac(0.1)

# Systematic Sampling
set.seed(123)
systematic_sample <- boxscore_totals_clean %>% 
  slice(seq(1, n(), by = 10))

population_mean <- mean(boxscore_totals_clean$bos_pts)
cat("Population mean:", population_mean, "\n")

simple_random_mean <- mean(simple_random_sample$bos_pts)
cat("Simple random sample mean:", simple_random_mean, "\n")

stratified_mean <- mean(stratified_sample$bos_pts)
cat("Stratified sample mean:", stratified_mean, "\n")

systematic_mean <- mean(systematic_sample$bos_pts)
cat("Systematic sample mean:", systematic_mean, "\n")



# Additional feature: point difference between Celtics and opponents
boxscore_totals_clean <- boxscore_totals_clean %>%
  mutate(PTS_diff = bos_pts - mean(bos_pts, na.rm = TRUE))

# Summary and plot
summary(boxscore_totals_clean$PTS_diff)

ggplot(boxscore_totals_clean, aes(x = PTS_diff)) +
  geom_histogram(binwidth = 2, fill = "orange", color = "black") +
  labs(title = "Distribution of Point Difference for Celtics",
       x = "Point Difference", y = "Frequency")

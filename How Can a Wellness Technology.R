# Preparation Phase
---
library(tidyverse) #helps wrangle data
library(lubridate) #helps wrangle date attributes
library(ggplot2) #helps visualize data
library(dplyr) #helps clean data
library(tidyr) #helps clean data

getwd()

# Set the base path for the data files
base_path <- "Fitabase Data 4.12.16-5.12.16/"

# Read the different data files
dailyActivity_merged <- read.csv(paste0(base_path, "dailyActivity_merged.csv"))
dailyCalories_merged <- read.csv(paste0(base_path, "dailyCalories_merged.csv"))
dailyIntensities_merged <- read.csv(paste0(base_path, "dailyIntensities_merged.csv"))
dailySteps_merged <- read.csv(paste0(base_path, "dailySteps_merged.csv"))
heartrate_seconds_merged <- read.csv(paste0(base_path, "heartrate_seconds_merged.csv"))
hourlyCalories_merged <- read.csv(paste0(base_path, "hourlyCalories_merged.csv"))
hourlyIntensities_merged <- read.csv(paste0(base_path, "hourlyIntensities_merged.csv"))
hourlySteps_merged <- read.csv(paste0(base_path, "hourlySteps_merged.csv"))
minuteCaloriesNarrow_merged <- read.csv(paste0(base_path, "minuteCaloriesNarrow_merged.csv"))
minuteCaloriesWide_merged <- read.csv(paste0(base_path, "minuteCaloriesWide_merged.csv"))
minuteIntensitiesNarrow_merged <- read.csv(paste0(base_path, "minuteIntensitiesNarrow_merged.csv"))
minuteIntensitiesWide_merged <- read.csv(paste0(base_path, "minuteIntensitiesWide_merged.csv"))
minuteMETsNarrow_merged <- read.csv(paste0(base_path, "minuteMETsNarrow_merged.csv"))
minuteSleep_merged <- read.csv(paste0(base_path, "minuteSleep_merged.csv"))
minuteStepsNarrow_merged <- read.csv(paste0(base_path, "minuteStepsNarrow_merged.csv"))
minuteStepsWide_merged <- read.csv(paste0(base_path, "minuteStepsWide_merged.csv"))
sleepDay_merged <- read.csv(paste0(base_path, "sleepDay_merged.csv"))
weightLogInfo_merged <- read.csv(paste0(base_path, "weightLogInfo_merged.csv"))

# Inspect column names of relevant dataframes
# colnames(dailyActivity_merged)
# colnames(dailyCalories_merged)
# colnames(dailyIntensities_merged)
# colnames(dailySteps_merged)
colnames(heartrate_seconds_merged)
# colnames(hourlyCalories_merged)
# colnames(hourlyIntensities_merged)
# colnames(hourlySteps_merged)
colnames(minuteCaloriesNarrow_merged)
colnames(minuteCaloriesWide_merged)
colnames(minuteIntensitiesNarrow_merged)
colnames(minuteIntensitiesWide_merged)
colnames(minuteMETsNarrow_merged)
colnames(minuteSleep_merged)
colnames(minuteStepsNarrow_merged)
colnames(minuteStepsWide_merged)
colnames(sleepDay_merged)
colnames(weightLogInfo_merged)

# Inspect structure of dataframes
str(dailyActivity_merged)
str(dailyCalories_merged)
str(dailyIntensities_merged)
str(dailySteps_merged)
str(heartrate_seconds_merged)
str(hourlyCalories_merged)
str(hourlyIntensities_merged)
str(hourlySteps_merged)
str(minuteCaloriesNarrow_merged)
str(minuteCaloriesWide_merged)
str(minuteIntensitiesNarrow_merged)
str(minuteIntensitiesWide_merged)
str(minuteMETsNarrow_merged)
str(minuteSleep_merged)
str(minuteStepsNarrow_merged)
str(minuteStepsWide_merged)
str(sleepDay_merged)
str(weightLogInfo_merged)

# Check for max VeryActiveMinutes
print(max(dailyActivity_merged$VeryActiveMinutes))

# Check for duplicated values between TotalDistance and TrackerDistance
duplicated_values <- intersect(dailyActivity_merged$TotalDistance,dailyActivity_merged$TrackerDistance)
print(duplicated_values)

dailyActivity_merged <- dailyActivity_merged %>%
  mutate(is_duplicate = TotalDistance %in% TrackerDistance)
print(table(dailyActivity_merged$is_duplicate))

# Check compatibility of shared columns between hourlyCalories and hourlySteps
check_compatibility <- all(hourlyCalories_merged$Id == hourlySteps_merged$id & hourlySteps_merged$ActivityHour== hourlyCalories_merged$ActivityHour)

if (check_compatibility) {
  print("The values in the shared columns are compatible between the tables.")
} else {
  print("The values in the shared columns are not compatible. Please check.")
}
print(check_compatibility)


# Process Phase
---

# Rename column in dailyActivity_merged for consistent merging
colnames(dailyActivity_merged)[colnames(dailyActivity_merged) == "ActivityDate"] <- "ActivityDay"

# Merge daily activity dataframes
daily_mearg <- merge(dailyActivity_merged, dailyCalories_merged, by = c("Id","ActivityDay"))
daily_mearg <- merge(daily_mearg, dailyIntensities_merged, by = c("Id", "ActivityDay"))
daily_mearg <- merge(daily_mearg, dailySteps_merged, by = c("Id", "ActivityDay"))
str(daily_mearg)

# Function to compare column values for identity
compare_columns <- function(df) {
  column_names <- names(df)
  result <- matrix(FALSE, ncol = ncol(df), nrow = ncol(df))
  colnames(result) <- column_names
  rownames(result) <- column_names

  for (i in seq_along(column_names)) {
    for (j in seq_along(column_names)) {
      if (i != j) {
        result[i, j] <- identical(df[[column_names[i]]], df[[column_names[j]]])
      }
    }
  }

  return(result)
}
compare_columns(daily_mearg)

# Round WeightKg to nearest integer
weightLogInfo_merged$WeightKg <- floor(weightLogInfo_merged$WeightKg)

# Remove redundant columns from daily_mearg
daily_mearg <- daily_mearg[, setdiff(names(daily_mearg), c("Calories.x","StepTotal"))]

# Merge hourly dataframes
hour_merg <- merge(hourlyCalories_merged, hourlyIntensities_merged,by = c("Id","ActivityHour"))
hour_merg <- merge(hour_merg, hourlySteps_merged, by = c("Id", "ActivityHour"))
str(hour_merg)
compare_columns(hour_merg)

# Merge minute dataframes (narrow format)
min_merg <- merge(minuteCaloriesNarrow_merged, minuteIntensitiesNarrow_merged,by = c("Id","ActivityMinute"))
min_merg <- merge(min_merg,minuteMETsNarrow_merged, by = c("Id", "ActivityMinute"))
min_merg <- merge(min_merg,minuteStepsNarrow_merged, by = c("Id", "ActivityMinute"))
str(min_merg)
compare_columns(min_merg)

# Display dimensions and summaries of all processed dataframes
dim(daily_mearg)
dim(hour_merg)
dim(min_merg)
dim(heartrate_seconds_merged)
dim(minuteSleep_merged)
dim(sleepDay_merged)
dim(weightLogInfo_merged)

nrow(daily_mearg)
nrow(hour_merg)
nrow(min_merg)
nrow(heartrate_seconds_merged)
nrow(minuteSleep_merged)
nrow(sleepDay_merged)
nrow(weightLogInfo_merged)

summary(daily_mearg)
summary(hour_merg)
summary(min_merg)
summary(heartrate_seconds_merged)
summary(minuteSleep_merged)
summary(sleepDay_merged)
summary(weightLogInfo_merged)

head(minuteSleep_merged,100)
unique(minuteSleep_merged$value)
unique(sleepDay_merged$TotalSleepRecords)

# Convert date/time columns to appropriate formats and extract time components
str(sleepDay_merged)
str(weightLogInfo_merged)

# daily_mearg date processing
daily_mearg$month <- format(as.Date(daily_mearg$ActivityDay, format = "%m/%d/%Y"), "%m")
daily_mearg$day <- format(as.Date(daily_mearg$ActivityDay, format = "%m/%d/%Y"), "%d")
daily_mearg$year <- format(as.Date(daily_mearg$ActivityDay, format = "%m/%d/%Y"), "%Y")
daily_mearg$day_of_week <- format(as.Date(daily_mearg$ActivityDay, format = "%m/%d/%Y"), "%A")

# hour_merg date/time processing
hour_merg$month <- format(as.Date(hour_merg$ActivityHour, format = "%m/%d/%Y"), "%m")
hour_merg$day <- format(as.Date(hour_merg$ActivityHour, format = "%m/%d/%Y"), "%d")
hour_merg$year <- format(as.Date(hour_merg$ActivityHour, format = "%m/%d/%Y"), "%Y")
hour_merg$day_of_week <- format(as.Date(hour_merg$ActivityHour, format = "%m/%d/%Y"), "%A")
hour_merg$hour <- format(as.POSIXct(hour_merg$ActivityHour, format = "%m/%d/%Y %I:%M:%S %p"), "%I%p")
str(hour_merg)

# heartrate_seconds_merged time processing
str(heartrate_seconds_merged)
head(heartrate_seconds_merged,25)
heartrate_seconds_merged$hour <- format(as.POSIXct(heartrate_seconds_merged$Time, format = "%m/%d/%Y %I:%M:%S %p"), "%I%p")

# Sleep and weight data cleaning and merging
str(sleepDay_merged)
str(minuteSleep_merged)
str(weightLogInfo_merged)
head(sleepDay_merged)
head(minuteSleep_merged)
head(weightLogInfo_merged)
head(heartrate_seconds_merged)

# Convert sleep timestamps to datetime objects
minuteSleep_merged$datetime <- as.POSIXct(minuteSleep_merged$date, format = "%m/%d/%Y %I:%M:%S %p")
sleepDay_merged$datetime <- as.POSIXct(sleepDay_merged$SleepDay, format = "%m/%d/%Y %I:%M:%S %p")
minuteSleep_merged$datetime <- NULL # Remove this column as it's not needed for merging later
sleepDay_merged$datetime <- NULL # Remove this column as it's not needed for merging later

# Add hour column to minuteSleep_merged
minuteSleep_merged$hour <- format(as.POSIXct(minuteSleep_merged$date, format = "%m/%d/%Y %I:%M:%S %p"), "%I%p")
# Remove potential redundant hour_am_pm column if it exists
minuteSleep_merged <- minuteSleep_merged %>% select(-contains("hour_am_pm"))

# Extract date only for merging sleep data
sleepDay_merged$date_only <- as.Date(sleepDay_merged$SleepDay, format = "%m/%d/%Y %I:%M:%S %p")
minuteSleep_merged$date_only <- as.Date(minuteSleep_merged$date, format = "%m/%d/%Y %I:%M:%S %p")

# Merge sleep day and minute sleep data
merged_min_day <- merge(sleepDay_merged, minuteSleep_merged, by = c("Id", "date_only"), all.y = TRUE)
head(merged_min_day)

# Remove 'Fat' column from weightLogInfo_merged
weightLogInfo_merged <- weightLogInfo_merged %>% select(-Fat)

# Remove duplicate weight entries based on Id and WeightKg
weightLogInfo_merged <- weightLogInfo_merged[!duplicated(weightLogInfo_merged[c("Id", "WeightKg")]), ]
unique(weightLogInfo_merged$Id)
str(merged_min_day)

# Function to count NA values in a dataframe
count_na <- function(data) {
  sapply(data, function(column) sum(is.na(column)))
}
na_counts <- count_na(sleepDay_merged)
print(na_counts)

# Function to fill NA values with the mean of the column
fill_na_with_mean <- function(data, columns) {
  for (col in columns) {
    if (col %in% names(data)) {
      data[[col]][is.na(data[[col]])] <- mean(data[[col]], na.rm = TRUE)
    } else {
      warning(paste("Column", col, "not found in the data."))
    }
  }
  return(data)
}

# Fill NA values in specified columns of merged_min_day
columns_to_fill <- c("TotalMinutesAsleep","TotalTimeInBed")
merged_min_day <- fill_na_with_mean(merged_min_day, columns_to_fill)

# Convert date fields for merging sleep and weight data
sleepDay_merged$SleepDay <- as.Date(sleepDay_merged$SleepDay, format = "%m/%d/%Y")
weightLogInfo_merged$Date <- as.Date(weightLogInfo_merged$Date, format = "%m/%d/%Y")

# Calculate average sleep minutes per Id
average_sleep <- aggregate(TotalMinutesAsleep ~ Id, data = sleepDay_merged, FUN = mean)
colnames(average_sleep)[2] <- "AverageMinutesAsleep"

# Merge average sleep with weight log info
merged_data_S_W <- merge(average_sleep, weightLogInfo_merged, by = "Id")
print(merged_data_S_W)

# Check for missing values in 'Calories.y' column in daily_mearg
field_name <- "Calories.y"
missing_values_count <- sum(is.na(daily_mearg[[field_name]]))
cat("Number of missing values in", field_name, ":", missing_values_count, "\n")


# Analysis Phase
---

# Function to analyze unique values in each column
analyze_columns <- function(data) {
  if (!is.data.frame(data)) {
    stop("Input must be a data frame.")
  }
  result <- list()
  for (col_name in names(data)) {
    unique_values <- unique(data[[col_name]])
    result[[col_name]] <- unique_values
  }
  return(result)
}

result <- analyze_columns(hour_merg)
print(result)

# Summarize daily calories and steps by day of the week
daily_mearg %>%
  group_by(day_of_week) %>%
  summarise(Calories.y =median(Calories.y, na.rm = TRUE), .groups = 'drop') %>%
  arrange(day_of_week)

daily_mearg %>%
  group_by(day_of_week) %>%
  summarise(TotalSteps =median(TotalSteps, na.rm = TRUE), mean_calories = mean(Calories.y, na.rm = TRUE),.groups = 'drop') %>%
  arrange(day_of_week)

# Summarize hourly activity (calories, intensity)
options(dplyr.print_max = Inf)
print(hour_merg %>%
        group_by(hour) %>%
        summarise(
          Calories = median(Calories, na.rm = TRUE),
          TotalIntensity = median(TotalIntensity, na.rm = TRUE),
          .groups = 'drop',
          Calories_max = max(Calories, na.rm = TRUE),
          TotalIntensity_max = max(TotalIntensity, na.rm = TRUE),
          Calories_min = min(Calories, na.rm = TRUE),
          TotalIntensity_min = min(TotalIntensity, na.rm = TRUE)
        ) %>%
        mutate(hour = factor(hour, levels = c(
          "12AM", "01AM", "02AM", "03AM", "04AM", "05AM", "06AM", "07AM", "08AM", "09AM", "10AM", "11AM",
          "12PM", "01PM", "02PM", "03PM", "04PM", "05PM", "06PM", "07PM", "08PM", "09PM", "10PM", "11PM"
        ))) %>%
        arrange(hour)
)

# Summarize hourly heart rate
heartrate_seconds_merged %>%
  group_by(hour) %>%
  summarise(mean_val=mean(Value,na.rm = TRUE),med=median(Value,na.rm = TRUE),max=max(Value,na.rm = TRUE),min=min(Value,na.rm = TRUE),.groups = 'drop') %>%
  mutate(hour = factor(hour, levels = c(
    "12AM", "01AM", "02AM", "03AM", "04AM", "05AM", "06AM", "07AM", "08AM", "09AM", "10AM", "11AM",
    "12PM", "01PM", "02PM", "03PM", "04PM", "05PM", "06PM", "07PM", "08PM", "09PM", "10PM", "11PM"
  ))) %>%
  arrange(hour)

# Calculate difference between total time in bed and total minutes asleep, and average it by user and month
sleepDay_merged <- sleepDay_merged %>%
  mutate(
    diff = TotalTimeInBed - TotalMinutesAsleep,
    month = month(date_only)
  )
avg_diff_per_user_month <- sleepDay_merged %>%
  group_by(Id, month) %>%
  summarise(avg_diff = mean(diff, na.rm = TRUE)) %>%
  ungroup()

overall_median <- median(avg_diff_per_user_month$avg_diff, na.rm = TRUE)

print(avg_diff_per_user_month)
cat("\nOverall Median of avg_diff:", overall_median, "\n")


# Share Phase
---

# Plot: Relationship between Weight and Average Minutes Asleep
if (!require(ggplot2)) install.packages("ggplot2")
ggplot(data = merged_data_S_W, aes(x = WeightKg, y = AverageMinutesAsleep)) +
  geom_point(color = "blue", size = 3) +
  labs(title = "Relationship between Weight and Average Minutes Asleep",
       x = "Weight (kg)",
       y = "Average Minutes Asleep") +
  theme_minimal()

# Plot: Average Total Time Asleep by Weight
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(dplyr)) install.packages("dplyr")

# Merge sleep day and weight info
DAYWeiUmerged_data <- merge(sleepDay_merged, weightLogInfo_merged, by.x = "Id", by.y = "Id")

# Aggregate data by weight and calculate average total time asleep
library(dplyr)
aggregated_data <- DAYWeiUmerged_data %>%
  group_by(WeightKg) %>%
  summarise(AverageMinutesAsleep = mean(TotalMinutesAsleep, na.rm = TRUE))

# Create bar chart
ggplot(data = aggregated_data, aes(x = as.factor(WeightKg), y = AverageMinutesAsleep)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Average Total Time Asleep by Weight",
       x = "Weight (kg)",
       y = "Average Total Time Asleep (minutes)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot: Average Calories by Step Range
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(dplyr)) install.packages("dplyr")

# Aggregate data by step ranges and calculate average calories
daily_mearg <- daily_mearg %>%
  mutate(StepRange = cut(TotalSteps, breaks = seq(0, max(TotalSteps, na.rm = TRUE), by = 5000),
                         labels = paste(seq(0, max(TotalSteps, na.rm = TRUE) - 5000, by = 5000),
                                        seq(5000, max(TotalSteps, na.rm = TRUE), by = 5000), sep = "-")))

aggregated_data <- daily_mearg %>%
  group_by(StepRange) %>%
  summarise(AverageCalories = mean(Calories.y, na.rm = TRUE)) %>%
  na.omit()

# Create bar chart
ggplot(data = aggregated_data, aes(x = StepRange, y = AverageCalories)) +
  geom_bar(stat = "identity", fill = "yellow") +
  labs(title = "Average Calories by Step Range",
       x = "Step Range",
       y = "Average Calories") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

# Plot: Comparison of Steps, Calories, and Very Active Minutes
library(ggplot2)
library(dplyr)

# Create ranges for Calories and VeryActiveMinutes
daily_mearg <- daily_mearg %>%
  mutate(
    CaloriesRange = case_when(
      Calories.y < 2000 ~ "<2000",
      Calories.y >= 2000 & Calories.y < 3000 ~ "2000-3000",
      Calories.y >= 3000 & Calories.y < 4000 ~ "3000-4000",
      TRUE ~ "4000+"
    ),
    VeryActiveMinutesRange = case_when(
      VeryActiveMinutes.y < 15 ~ "<15 mins",
      VeryActiveMinutes.y >= 15 & VeryActiveMinutes.y < 30 ~ "15-30 mins",
      VeryActiveMinutes.y >= 30 ~ "30+ mins"
    )
  )

# Create faceted bar plot
ggplot(daily_mearg, aes(x = StepRange, fill = CaloriesRange)) +
  geom_bar(position = "dodge") +
  facet_wrap(~ VeryActiveMinutesRange) +
  labs(
    title = "Comparison of Steps, Calories, and Very Active Minutes",
    x = "Step Range",
    y = "Number of Individuals",
    fill = "Calories Range"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
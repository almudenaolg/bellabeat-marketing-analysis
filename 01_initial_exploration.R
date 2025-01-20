# set the path to the directory 
setwd("C:/Users/almou/Desktop/IRONHACK/bellabeat-marketing-analysis")


# check the data in the directory
getwd()
list.files("data/")


# first steps to explore data
install.packages("tidyverse")
library(tidyverse)


# load the files
daily_activity <- read_csv("data/dailyActivity_merged.csv")
heart_rate <- read_csv("data/heartrate_seconds_merged.csv")
hourly_calories <- read_csv("data/hourlyCalories_merged.csv")
hourly_intensities <- read_csv("data/hourlyIntensities_merged.csv")
hourly_steps <- read_csv("data/hourlySteps_merged.csv")
minute_calories <- read_csv("data/minuteCaloriesNarrow_merged.csv")
minute_intensities <- read_csv("data/minuteIntensitiesNarrow_merged.csv")
minute_mets <- read_csv("data/minuteMETsNarrow_merged.csv")
minute_sleep <- read_csv("data/minuteSleep_merged.csv")
minute_steps <- read_csv("data/minuteStepsNarrow_merged.csv")
weight_info <- read_csv("data/weightLogInfo_merged.csv")


###############################################################
###               DAILY ACTIVITY EXPLORATION               ###
###############################################################
head(daily_activity)
str(daily_activity)
summary(daily_activity)

# Steps: Median: 5986, Mean: 6547, Maximum: 28497.
# Calories burned: Median: 2062, Mean: 2189, Maximum: 4562.
# Activity levels (minutes). Very active minutes: Mean: 16.6 (low overall), Max: 202.
# Lightly active minutes mean: 170.1 (users tend to spend more time in light activity)
# Sedentary minutes mean: 995.3 (the vast majority of time is spent in sedentary activities, more than 16.5 hours on average)

# Relation between steps and calories
ggplot(daily_activity, aes(x = TotalSteps, y = Calories)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Relationship Between Steps and Calories Burned",
       x = "Total Steps",
       y = "Calories Burned")

# pivot the data and save it in activity_minutesin order to compare the different types
activity_minutes <- daily_activity %>%
  select(VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes, SedentaryMinutes) %>%
  pivot_longer(cols = everything(),
               names_to = "ActivityType",
               values_to = "Minutes")

# Calculate mean since activity minutes
activity_minutes_avg <- activity_minutes %>%
  group_by(ActivityType) %>%
  summarize(AvgMinutes = mean(Minutes))

# Create a barplot comparing the mean of each activity type
ggplot(activity_minutes_avg, aes(x = ActivityType, y = AvgMinutes, fill = ActivityType)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(title = "Average Minutes by Activity Type",
       x = "Activity Type",
       y = "Average Minutes")


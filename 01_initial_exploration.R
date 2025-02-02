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
###                DAILY ACTIVITY EXPLORATION               ###
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

###############################################################
###                 HEART RATE EXPLORATION                  ###
###############################################################
head(heart_rate)
str(heart_rate)
summary(heart_rate)

#Convert the Time column to datetime to extract hour and group by period of the day
heart_rate <- heart_rate %>%
  mutate(Time = as.POSIXct(Time, format = "%m/%d/%Y %I:%M:%S %p"),
         Period = case_when(format(Time, "%H") < 12 ~ "Morning",
                            format(Time, "%H") >= 12 & format(Time, "%H") < 18 ~ "Afternoon",
                            TRUE ~ "Evening"))

# Calculate the mean for each period of the day
heart_rate_summary <- heart_rate %>%
  group_by(Period) %>%
  summarize(AvgHeartRate = mean(Value, na.rm = TRUE))

# Visualize the relationship between the heart rate and the period of the day
ggplot(heart_rate_summary, aes(x = Period, y = AvgHeartRate, fill = Period)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(title = "Average Heart Rate by Period of the Day",
       x = "Period of the Day",
       y = "Average Heart Rate")

# Calculate the mean for each hour
heart_rate_hour <- heart_rate %>%
  mutate(Time = as.POSIXct(Time, format = "%m/%d/%Y %I:%M:%S %p"),
         Hour = format(Time, "%H")) %>%
  group_by(Hour) %>%
  summarize(AvgHeartRate = mean(Value))

#Create a line graph to visualize the evolution of heart rate throughout the day
ggplot(heart_rate_hour, aes(x = as.numeric(Hour), y = AvgHeartRate)) +
  geom_line(color = "red") +
  labs(title = "Average Heart Rate by Hour",
       x = "Hour of the Day", y = "Average Heart Rate")



###############################################################
###                HOURLY CALORIES EXPLORATION              ###
###############################################################
head(hourly_calories)
str(hourly_calories)
summary(hourly_calories)

# Transformr data to calculate the average calories burned by hour
hourly_calories_avg <- hourly_calories %>%
  mutate(ActivityHour = as.POSIXct(ActivityHour, format = "%m/%d/%Y %I:%M:%S %p"),
         Hour = format(ActivityHour, "%H")) %>%
  group_by(Hour) %>%
  summarize(AvgCalories = mean(Calories))

# Visualize the calories per hour
ggplot(hourly_calories_avg, aes(x = as.numeric(Hour), y = AvgCalories)) +
  geom_line(color = "blue") +
  labs(title = "Average Calories Burned by Hour",
       x = "Hour of the Day", y = "Average Calories")



###############################################################
###                 HOURLY STEPS EXPLORATION                ###
###############################################################
head(hourly_steps)
str(hourly_steps)
summary(hourly_steps)

# Transformar y resumir datos
hourly_steps_avg <- hourly_steps %>%
  mutate(ActivityHour = as.POSIXct(ActivityHour, format = "%m/%d/%Y %I:%M:%S %p"),
         Hour = format(ActivityHour, "%H")) %>%
  group_by(Hour) %>%
  summarize(AvgSteps = mean(StepTotal))

# Gráfico
ggplot(hourly_steps_avg, aes(x = as.numeric(Hour), y = AvgSteps)) +
  geom_line(color = "green") +
  labs(title = "Average Steps by Hour",
       x = "Hour of the Day", y = "Average Steps")



###############################################################
###                 MINUTE SLEEP EXPLORATION                ###
###############################################################
head(minute_sleep)
str(minute_sleep)
summary(minute_sleep)

sleep_summary <- minute_sleep %>%
  mutate(date = as.Date(date, format = "%m/%d/%Y %I:%M:%S %p")) %>%
  group_by(Id, date) %>%
  summarize(TotalMinutes = n(),
            LightSleepMinutes = sum(value == 1),
            DeepSleepMinutes = sum(value == 2),
            RestlessMinutes = sum(value == 3)) %>%
  mutate(LightSleepPercentage = LightSleepMinutes / TotalMinutes * 100,
         DeepSleepPercentage = DeepSleepMinutes / TotalMinutes * 100,
         RestlessPercentage = RestlessMinutes / TotalMinutes * 100)

ggplot(sleep_summary, aes(x = DeepSleepPercentage)) +
  geom_histogram(bins = 30, fill = "blue") +
  labs(title = "Distribution of Deep Sleep Percentage",
       x = "Percentage of Deep Sleep",
       y = "Frequency")

# The majority of users have a very low percentage of deep sleep compared to what is recommended (20%).



###############################################################
###                 WEIGHT INFO EXPLORATION                 ###
###############################################################
head(weight_info)
str(weight_info)
summary(weight_info)

length(unique(weight_info$Id))

#With only 33 entries and 11 users, the data are not representative for assessing weight loss as a primary goal.



###############################################################
###             SEGMENTATION BY LEVEL ACTIVITY              ###
###############################################################

# Classify users by their level of physical activity based on TotalSteps
daily_activity <- daily_activity%>%
  mutate (ActivityLevel = case_when(
    TotalSteps < 5000 ~ "Sedentary",
    TotalSteps >= 5000 & TotalSteps < 10000 ~ "Moderately Active",
    TotalSteps >= 10000 ~ "Highly Active"))

ggplot(daily_activity, aes(x=ActivityLevel)) +
         geom_bar(fill = "red") +
         labs(
           title = "Distribution of Activity Levels",
           x = "Activity Level",
           y = "Number of Users")

# Relating activity levels to quality of sleep
# First we need to unify the date format
daily_activity <- daily_activity %>%
  mutate(ActivityDate = as.Date(as.character(ActivityDate), format = "%m/%d/%Y"))

sleep_summary <- sleep_summary %>%
  mutate(date = as.Date(as.character(date), format = "%Y-%m-%d"))

combined_data <- merge(daily_activity, sleep_summary, by.x = c("Id", "ActivityDate"), by.y = c("Id", "date"))


# Calculate mean for each type of sleep by the activity level
sleep_vs_activity <- combined_data %>%
  group_by(ActivityLevel) %>%
  summarize(
    AvgLightSleep = mean(LightSleepPercentage),
    AvgDeepSleep = mean(DeepSleepPercentage),
    AvgRestless = mean(RestlessPercentage))

# Visualization
ggplot(sleep_vs_activity, aes(x = ActivityLevel, y = AvgDeepSleep, fill = ActivityLevel)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Deep Sleep Percentage by Activity Level",
    x = "Activity Level",
    y = "Average Deep Sleep Percentage")



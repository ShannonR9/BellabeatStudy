# BellabeatStudy
---
title: "Bellabeat Case Study"
author: "Shannon Richardson"
date: "02-14-2024"
output:
  pdf_document: default
  word_document: default
  html_document: default
mainfont: SourceSansPro
header-includes:
- \usepackage[default]{sourcesanspro}
- \usepackage[T1]{fontenc}
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE)

```



# How Can a Wellness Company Play It Smart?

# Intro

Bellabeat is a a high-tech manufacturer of health-focused products for women. Their products work by tracking user data with activity, sleep, stress, and reproductive health. Currently they are a successful small company, but they have the potential to become a larger player in the global smart device market. My job is to analyze data from similar devices to determine how the company can make changes to marketing and products to maximize growth.

# Project Overview - Business Task

In this project, I will be analyzing competitor data to determine habits of current smart device users and identify potential opportunities for growth. I will provide recommendations for the improvement of Bellabeat's marketing strategy based on my findings. 


**Key Stakeholders:**

-   Urška Sršen: Bellabeat’s co-founder and Chief Creative Officer
-   Sando Mur: Mathematician and Bellabeat’s co-founder

**Initial Questions to Explore:**

1.  What are some trends in smart device usage?
2.  Is there a relationship between sleep and activity?
3.  How could these trends help influence Bellabeat marketing strategy?

# Data Overview

**Source**

The data being used in this case study can be found here: [FitBit Fitness Tracker Data] <https://www.kaggle.com/datasets/arashnic/fitbit> CC0: Public Domain, dataset made available through [Mobius] <https://www.kaggle.com/arashnic>

**Limitations**

The data set is comprised of data from 33 Fitbit users who consented to the submission of personal tracker data, including minute-level output for physical activity, heart rate, and sleep monitoring. It also includes data from manual weight tracking. This data is from 04/2016-05/2016. While limited in size, demographic, and length, general marketing insights can still be drawn from the data.

## Prepare

# Setting up my environment:

```{r eval = FALSE}

install.packages("lubridate")
install.packages("skimr")
install.packages("janitor")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("ggplot2")

```

Installing the necessary packages.

```{r results = 'hide', message = FALSE}

library(lubridate)
library(skimr)
library(janitor)
library(dplyr)
library(tidyverse)
library(ggplot2)

```

After installing and loading the necessary packages, I need to import the datasets I'm working with.

```{r importing datasets and creating dataframes}
daily_activity<-read.csv("dailyActivity_merged.csv")
hourly_activity<-read.csv("hourly_activity.csv")
sleep_day<-read.csv("sleepDay_merged.csv")
weight_log_info<-read.csv("weightLogInfo_merged.csv")

```

**The 'hourly_activity' csv file is a dataset that I worked with in Excel to merge three different datasets: hourly intensity, hourly steps and hourly calories prior to uploading in R.**

```{r cleaning data}
daily_activity<-clean_names(daily_activity)
hourly_activity<-clean_names(hourly_activity)
sleep_day<-clean_names(sleep_day)
weight_log_info<-clean_names(weight_log_info)

```

I used the clean_names function to ensure all column names across all datasets were cohesive.

```{r results = 'hide', message = FALSE}

get_dupes(daily_activity) #no duplicates found
get_dupes(hourly_activity) #no duplicates found
get_dupes(weight_log_info) #no duplicates found
sleep_dupes<-get_dupes(sleep_day) #duplicates found, saved them to anti-join
#anti-join sleep_dupes and sleep_day to remove duplicates
sleep_day <- anti_join(sleep_day, sleep_dupes)
rm(sleep_dupes)

```

I checked for duplicate combinations of data per row and removed them from the dataframe.

```{r results = 'hide', message = FALSE}
view(daily_activity)
view(hourly_activity)
view(sleep_day)
view(weight_log_info)
head(daily_activity)
colnames(daily_activity)
head(hourly_activity)
colnames(hourly_activity)
head(sleep_day) 
colnames(sleep_day)
head(weight_log_info)
colnames(weight_log_info)

```

I familiarized myself with the data and previewed it.

## Analyze

```{r finding the number of participants in each dataset}
n_distinct(daily_activity$id) #33
n_distinct(hourly_activity$id) #33
n_distinct(sleep_day$id) #24
n_distinct(weight_log_info$id) #8

```

While the total number of participants is 33, only 24 contributed to the sleep tracking data and only 8 contributed to the weight tracking data. This could indicate a need for easier sleep and weight tracking features. Creating healthy incentives for tracking both of those categories could also prove useful. However, due to a lack of participants in the weight tracking, I will no longer be utilizing this data set.

```{r summary of the daily_activity set}
daily_activity %>%
    select(total_steps,
          total_distance,
          calories) %>%
  summary(data)
  
```

```{r summary of the active minutes columns }
daily_activity %>%
     select(very_active_minutes,
            fairly_active_minutes,
            lightly_active_minutes,
            sedentary_minutes) %>%
     summary(data)

```

```{r summary of the sleep_day dataset}
sleep_day %>%
    select(total_sleep_records,
           total_minutes_asleep,
           total_time_in_bed) %>%
    summary(data)
```

```{r}
hourly_activity %>%
    select(calories) %>%
    summary(data)
```

**Initial Observations**

-   Sedentary minutes on average is 16.5 hours

-   The average number of steps per day is 7,638

-   In terms of active minutes, the majority of the participants are lightly active

-   The average participant burns 97 calories per hour

-   The average sleep schedule was approximately 7 hours or 6.98 hours to be exact

**Before creating any visualizations I need to merge the Sleep Day data set with the Daily Activity data set.**

```{r}
merged_data <- merge(sleep_day, daily_activity)

```

#Visualize

```{r results = 'hide', warning = FALSE}
ggplot(data = daily_activity, aes(x = total_steps, y = calories, color = calories)) + geom_point() + geom_smooth() + labs(title = "Total Steps vs. Calories", x = "Total Steps", y = "Calories", color = "Calories")

```

This chart correlates with the summary findings from earlier, indicating the average steps per day are around 7,638. The more steps, the more calories burned. According to a study from the CDC [cdc.gov] <https://www.cdc.gov/pcd/issues/2016/pdf/16_0111.pdf> the recommended daily steps is 10,000. Users for this data fall just under that.

Now I want to take a look at the relationship between the time users were spending in bed vs the time spent sleeping:

```{r tidy = FALSE}
ggplot(data = sleep_day, aes(x = total_minutes_asleep, y = total_time_in_bed, alpha = total_minutes_asleep, color = total_time_in_bed)) + geom_point() + labs(title = "Total Time Asleep vs Total Time in Bed", color = "Total Time In Bed", alpha = "Total Time In Bed")+ scale_color_gradient(low = "blue", high = "red")

```

This visualization shows us a positive relationship between time asleep vs time in bed. Bellabeat can expand on this by adding interactive features like customizing a sleep schedule, bedtime reminders, or rewards for manually entering sleep data to make tracking sleep more appealing for their users.

I want to take a look at the relationship between sleep duration and sedentary time.

**My previously merged data sets created an overcrowded plot. I need to aggregate the data to make my visualization more coherent.**

```{r tidy = FALSE}
aggregated_data <- merged_data %>%
  group_by(sedentary_minutes) %>%
  summarise(total_minutes_asleep = mean(total_minutes_asleep))
```

Now that the data is aggregated I can create a better visualization of the relationship between sedentary minutes and sleep duration.

```{r tidy = FALSE}

ggplot(aggregated_data, aes(x = sedentary_minutes, y = total_minutes_asleep, color = sedentary_minutes)) + geom_point() + labs(title= "Sleep Duration and Sedentary Time", y = "Total Minutes Asleep", x = "Sedentary Minutes", color = "Sedentary Minutes")+ scale_color_gradient(low = "red", high = "green")

```

Let's see the correlation coefficient for the two.

```{r results = 'hide', message = FALSE, tidy = FALSE}
cor(aggregated_data$total_minutes_asleep,aggregated_data$sedentary_minutes)

```

-0.3758623

There is a negative correlation between sedentary minutes and total minutes asleep. The less active a user is, the less sleep they usually get. Bellabeat may be able to take advantage of this by setting interactive tech balance reminders, exercise suggestions, or bedtime alerts for healthy sleep habits to help users adhere to a regular schedule.

Now lets take a look at what day of the week users are taking the most steps. In order to create a visualization showing the days of the week I need to convert the column to a proper date format and then extract the day of the week. After that I want to convert the 'day_of_week' column to a factor with specified levels starting from Monday.

```{r results = 'hide', message = FALSE, tidy = FALSE}

daily_activity$activity_date <- as.Date(daily_activity$activity_date, format = "%m/%d/%Y")

daily_activity$day_of_week <- weekdays(daily_activity$activity_date)

daily_activity$day_of_week <- substr(daily_activity$day_of_week, 1, 3)

daily_activity$day_of_week <- factor(daily_activity$day_of_week, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))

```

```{r echo = F, dev = "png", tidy = FALSE}

ggplot(aggregate(total_steps ~ day_of_week, data = daily_activity, FUN = sum), aes(x = day_of_week, y = total_steps, fill = total_steps)) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_fill_gradient(low = "mediumslateblue", high = "springgreen", name = "Total Steps") + labs(title = "Total Steps vs. Day of the Week", x = "Day of the Week", y = "Total Steps")

```

This visualization highlights Tuesday as the day of the week showing the highest step count, followed by Wednesday and then Thursday. This could indicate users walk more during the workweek, either at work or on a commute to work.

Next lets take a look at what days users were most active and most sedentary.

```{r echo = F, dev = "png", tidy = FALSE}
ggplot(aggregate(very_active_minutes ~ day_of_week, data = daily_activity, FUN = sum), aes(x = day_of_week, y = very_active_minutes, fill = very_active_minutes)) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_fill_gradient(low = "green", high = "yellow", name = "Very Active Minutes") +
  labs(title = "Very Active Minutes vs. Day of the Week", x = "Day of the Week", y = "Very Active Minutes")

```

The days where users are the most active correlate with the days that have the most steps. Tuesday, Wednesday, and Thursday are users most active days.

Now lets look at what days users are the most sedentary.

```{r echo = F, dev = "png", tidy = FALSE}

ggplot(aggregate(sedentary_minutes ~ day_of_week, data = daily_activity, FUN = sum), aes(x = day_of_week, y = sedentary_minutes, fill = sedentary_minutes)) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_fill_gradient(low = "blue", high = "red", name = "Sedentary Minutes") +
  labs(title = "Sedentary Minutes vs. Day of the Week", x = "Day of the Week", y = "Sedentary Minutes")


```

The days of the week users are the most sedentary or least active coincide with the days users take the most step, possibly indicating users do not take steps or workout outside of their normal daily routine, i.e. work, school etc.

# Recommendations

1.  Introduce goals and streaks for weight tracking, sleep, and work outs.

    -   Allow users to customize sleep schedules and introduce interactive bedtime reminders.
    - Add rewards for manually entering sleep or weight data.

2.  Introduce new notifications that are personal to each user. 

    -   Notifications for excersize reccomendations based on user schedules, i.e. on days where user has more sedentary minutes, or days where they have lightly active minutes. 
    
    -  Tech or screen time reminders for weeks where users have a lot of screen time and less activity. 

3. Subscriptions

    - Bellabeat has a subscription based service that gives users access to fully personalized guidance on nutrition, activity, sleep, health and beauty based on their lifestyle and goals. A free trial or trial based on rewards system could be helpful. For example once you reach a certain number of rewards for tracking sleep and workouts, a 7-14 day trial can be made available for users to get a feel of the subscription and see real results. 

    - Prior to a subscription the free version of Bellabeats app can offer notifications on easy, on-the-go breakfast or lunch options for users who's schedule show a typical 9-5 workweek to encourage user engagement.
    
4. Sundays

    - We can see through the vizualizations that Sunday is the day where the least activity and steps takes place. Bellabeat can offer personalized notifications for meditation, light workouts, and suggestions for walks on this specific day to help users stay active throughout the entire week.

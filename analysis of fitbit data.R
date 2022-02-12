#Install and load packages
library(tidyverse)
library(skimr)
library(lubridate)
library(ggpubr)
library(ggrepel)
library(janitor)
library(rmarkdown)
library(scales)

#Import the data sets.
daily_activity <- read.csv (file = "/Users/User/Documents/fitbit_tracker_data/fitbit_data/dailyActivity_merged.csv")
daily_sleep <- read.csv(file = "/Users/User/Documents/fitbit_tracker_data/fitbit_data/sleepDay_merged.csv")
hourly_steps <- read.csv(file = "/Users/User/Documents/fitbit_tracker_data/fitbit_data/hourlySteps_merged.csv")

#Preview the data sets.
head(daily_activity)
glimpse(daily_activity)

head(daily_sleep)
glimpse(daily_sleep)

head(hourly_steps)
glimpse(hourly_steps)

#Clean and rename the column names.
daily_activity <- rename_with(daily_activity, tolower)
daily_sleep <- rename_with(daily_sleep, tolower)
hourly_steps <- rename_with(hourly_steps, tolower)

#Verify the number of users
n_distinct(daily_activity$id)
n_distinct(daily_sleep$id)
n_distinct(hourly_steps$id)

#Check for duplicates
sum(duplicated(daily_activity))
sum(duplicated(daily_sleep))
sum(duplicated(hourly_steps))

#Remove any duplicates.
daily_activity <- daily_activity %>% 
  distinct() %>% 
  drop_na()

daily_sleep <- daily_sleep %>% 
  distinct() %>% 
  drop_na()

hourly_steps <- hourly_steps %>% 
  distinct() %>% 
  drop_na()

#Verify that the duplicate in daily_sleep has been removed.
sum(duplicated(daily_sleep))

#Format the date and time column to ensure consistency.
daily_activity <- daily_activity %>% 
  rename(date = activitydate) %>% 
  mutate(date = as_date(date, format = "%m/%d/%Y"))

daily_sleep <- daily_sleep %>% 
  rename(date = sleepday) %>% 
  mutate(date = as_date(date, format = "%m/%d/%Y %I:%M:%S %p" , tz=Sys.timezone()))

hourly_steps<- hourly_steps %>% 
  rename(date_time = activityhour) %>% 
  mutate(date_time = as.POSIXct(date_time,format ="%m/%d/%Y %I:%M:%S %p" , tz=Sys.timezone()))

head(daily_activity)
head(daily_sleep)
head(hourly_steps)

#Merge daily_activity and daily_sleep data sets for analysis.
daily_activity_sleep <- merge(daily_activity, daily_sleep, by=c("id", "date"))
glimpse(daily_activity_sleep)
head(daily_activity_sleep)
View(daily_activity_sleep)

#Determine the daily steps average by user.
daily_average <- daily_activity_sleep %>% 
  group_by(id) %>% 
  summarize(mean_daily_steps = mean(totalsteps), mean_daily_calories = mean(calories), mean_daily_sleep = mean(totalminutesasleep))
head(daily_average)

#Classify the users by the daily average steps.
user_type <- daily_average %>% 
  mutate(user_type = case_when(
    mean_daily_steps < 5000 ~ 'Sedentary',
    mean_daily_steps >= 5000 & mean_daily_steps < 7499 ~ 'Low Active',
    mean_daily_steps >= 7499 & mean_daily_steps < 9999 ~'Fairly Active',
    mean_daily_steps >= 10000 ~ 'Very Active'
  ))
head(user_type)

#Create a new column to calculate the percent for each user type.
user_type_percent <- user_type %>% 
  group_by(user_type) %>% 
  summarise(total = n()) %>%  
  mutate(totals = sum(total)) %>% 
  group_by(user_type) %>% 
  summarise(total_percent = total / totals) %>% 
  mutate(labels = scales::percent(total_percent))

user_type_percent$user_type <- factor(user_type_percent$user_type, levels = c("Very Active", "Fairly Active", "Low Active", "Sedentary"))
head(user_type_percent)

#Create a pie chart to visualize each user type and the percent apportioned to each.
user_type_percent %>% 
  ggplot(aes(x="", y = total_percent, fill = user_type)) +
  geom_bar(stat = "identity", width = 1)+
  coord_polar("y", start=0)+
  theme_minimal()+
  theme(axis.title.x= element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(), 
        panel.grid = element_blank(), 
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5, size=14, face = "bold")) +
  scale_fill_manual(values = c("#85e085","#e6e600", "#ffd480", "#ff8080")) +
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5)) +
  labs(title = "User Type Distribution") 


#Steps taken per week and minutes asleep per week. 
#Calculate the weekdays based on the column date and calculate the average steps walked and minutes by weekday.

weekday_steps_sleep <- daily_activity_sleep %>% 
  mutate(weekday = weekdays(date))

weekday_steps_sleep$weekday <- ordered(weekday_steps_sleep$weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

weekday_steps_sleep <- weekday_steps_sleep %>% 
  group_by(weekday) %>% 
  summarise(daily_steps = mean(totalsteps), daily_sleep = mean(totalminutesasleep))
head(weekday_steps_sleep)

#Create bar charts to visualize daily steps taken and minutes asleep per weekday.
ggarrange(
  ggplot(weekday_steps_sleep) +
    geom_col(aes(weekday, daily_steps), fill = "#006699") +
    geom_hline(yintercept = 7500) +
    labs(title = "Daily Steps per Weekday", x= "", y = "") +
    theme(axis.text.x = element_text(angle = 45,vjust = 0.5, hjust = 1)),
  ggplot(weekday_steps_sleep, aes(weekday, daily_sleep)) +
    geom_col(fill = "#85e0e0") +
    geom_hline(yintercept = 480) +
    labs(title = "Minutes Asleep per Weekday", x= "", y = "") +
    theme(axis.text.x = element_text(angle = 45,vjust = 0.5, hjust = 1))
)

#Determine what hours of the day users are most active. Use the hourly_steps data set and separate the date-time column.
#Get the average total steps.
hourly_steps <- hourly_steps %>%
  separate(date_time, into = c("date", "time"), sep= " ") %>%
  mutate(date = ymd(date)) 

#Create a graph to visualize the hours and average steps.
hourly_steps %>% 
  group_by(time) %>% 
  summarise(average_steps = mean(steptotal)) %>% 
  ggplot()+
  geom_col(mapping = aes(x=time, y = average_steps, fill = average_steps)) + 
  labs(title = "Hourly steps throughout the day", x="", y="") + 
  scale_fill_gradient(low = "green", high = "red")+
  theme(axis.text.x = element_text(angle = 90))

#Determine if there is any correlation between total steps and calories
ggplot(daily_activity_sleep, aes(x=totalsteps, y=calories))+
  geom_jitter() +
  geom_smooth(color = "red") + 
  labs(title = "Daily steps vs Calories", subtitle = "Correlation between daily steps and calories") +
  theme(panel.background = element_blank(),
        plot.title = element_text( size=14))

#Determine if there is any correlation between total steps and total minutes asleep
ggplot(daily_activity_sleep, aes(x=totalsteps, y=totalminutesasleep))+
  geom_jitter() +
  geom_smooth(color = "red") + 
  labs(title = "Daily steps vs Minutes asleep", x = "Daily steps", y= "Minutes asleep") +
  theme(panel.background = element_blank(),
        plot.title = element_text( size=14))

#Determine if there is any correlation between total minutes asleep and total time in bed.
ggplot(daily_activity_sleep, aes(x=totaltimeinbed, y=totalminutesasleep))+
  geom_jitter() +
  geom_smooth(color = "red") + 
  labs(title = "Total time in bed vs Minutes asleep", x = "Total time in bed", y= "Minutes asleep") +
  theme(panel.background = element_blank(),
        plot.title = element_text( size=14))

#Determine the daily usage of the smart device by users and categorize them.
daily_use <- daily_activity_sleep %>%
  group_by(id) %>%
  summarize(days_used=sum(n())) %>%
  mutate(usage = case_when(
    days_used >= 1 & days_used <= 10 ~ "low use",
    days_used >= 11 & days_used <= 20 ~ "moderate use", 
    days_used >= 21 & days_used <= 31 ~ "high use", 
  )) 
head(daily_use)

#Add another column to show the different users' percentages per daily usage.
daily_use_percent <- daily_use %>%
  group_by(usage) %>%
  summarise(total = n()) %>%
  mutate(totals = sum(total)) %>%
  group_by(usage) %>%
  summarise(total_percent = total / totals) %>%
  mutate(labels = scales::percent(total_percent))

daily_use_percent$usage <- factor(daily_use_percent$usage, levels = c("high use", "moderate use", "low use"))
head(daily_use_percent)

#Construct a pie chart to clearly visualize each category and their percent.
daily_use_percent %>%
  ggplot(aes(x="",y=total_percent, fill=usage)) +
  geom_bar(stat = "identity", width = 1)+
  coord_polar("y", start=0)+
  theme_minimal()+
  theme(axis.title.x= element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(), 
        panel.grid = element_blank(), 
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5, size=14, face = "bold")) +
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5))+
  scale_fill_manual(values = c("#006633","#00e673","#80ffbf"),
                    labels = c("High use - 21 to 31 days",
                               "Moderate use - 11 to 20 days",
                               "Low use - 1 to 10 days"))+
  labs(title="Daily Use of Smart Device")




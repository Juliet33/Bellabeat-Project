---
Title: "Bellabeat Project on Smart Device Usage"
Author: "Juliet Uadiale"
Date: "2/9/2022"
---



### **Table of Contents**



 1. Introduction
  
 2. Statement of Business Task
  
 3. Data Preparation
       + 3.1 About Data
       + 3.2 Data Accessibility and Privacy
       + 3.3 Data Limitations
       + 3.4 Data Sorting
  
 4. Data Processing
       + 4.1 Load packages
       + 4.2 Import data sets
       + 4.3 Preview imported data sets 
       + 4.4 Clean data
      
  
 5. Data Analysis
       + 5.1 Aggregate data sets
       + 5.2 Determine the daily steps average by users
       + 5.3 Determine steps taken per weekday and minutes asleep per weekday
       + 5.4 Determine what hours of the day users are most active
       + 5.5 Determine Correlations
       + 5.6 Determine the usage of the smart device by users
  
 6. Conclusions
       + 6.1 Recommendations
       + 6.2 Additional Suggestion




### **1.   Introduction** 

This project is focused on Bellabeat, a high-tech company founded in 2013. Bellabeat manufactures health-focused smart products;


  - **Bellabeat app:** provides users with health data related to their activity, sleep, stress,menstrual cycle, and mindfulness habits. 
  
  - **Leaf:** The Leaf tracker can be worn as a bracelet, necklace, or clip and it connects to the Bellabeat app to track activity, sleep,            and stress.
  
  - **Time:** The Time watch connects to the Bellabeat app to provide you with insights into your daily wellness.
  
  - **Spring:** The Spring bottle connects to the Bellabeat app to track your hydration levels.
  
  - **Bellabeat membership:** Membership gives users 24/7 access to fully personalized guidance on nutrition, activity, sleep, health and
    beauty, and mindfulness based on their lifestyle and goals.


![An Image of a Bellabeat Leaf](./bellabeat.leaf.jpg)


For this case study,the main goal is to analyze smart device usage data in order to gain insight into how consumers use non-Bellabeat smart devices and select one Bellabeat product to apply these insights to.

We'll focus on the Bellabeat app which provides users with health data related to their activity, sleep, stress, menstrual cycle, and mindfulness habits. This data can help users better understand their current habits and make healthy decisions.




### **2.   Statement of Business Task**

Identify potential growth opportunities and give recommendations to help improve the Bellabeat marketing strategy based on the trends in users' smart device usage. 

**Key Stakeholders involved;** 

 - **Urška Sršen:** Bellabeat’s cofounder and Chief Creative Officer.
 - **Sando Mur:** Mathematician and Bellabeat’s cofounder; key member of the Bellabeat executive team.
 - **Bellabeat marketing analytics team.**



### **3.  Data Preparation**
This phase entails the following; 



##### **3.1   About Data:** 

The data set to be used is the [FitBit Fitness Tracker Data](https://www.kaggle.com/arashnic/fitbit). It contains personal fitness tracker from 30 fitbit users. Data is organized in 18 .csv files and the data is considered long, each subject will have data in multiple rows. Every user has a unique ID and different rows since data is tracked by day and time.



##### **3.2   Data Accessibility and privacy:**

30 fitbit users consented to the submission of personal tracker data, including minute-level output for physical activity, heart rate, and sleep monitoring. Also, the data set was spooled from a public domain and can be accessed by anyone.
The data set is provided by the FiBit fitness tracker. 



##### **3.3   Data Limitations:**

The following limitations were discovered;

 - **No Demographics:** Information on demographic such as gender, age, occupation were not provided.
 
 - **Limitation of size:** Only 30 users consented to the submission of personal tracker data. Such factor may contribute to another data      bias.
 
 - **No Metadata Provided:** Information such as location, lifestyle, weather, temperature, humidity etc. would provide a deeper context      to the data obtained.
 


##### **3.4   Data Sorting :**

All the files were previewed using an Excel work sheet and subsets were discovered. We will use three files to carry out our analysis because they contain all the relevant information that we need. They are;

  - dailyActivity_merged.csv
  - sleepDay_merged.csv
  - hourlySteps_merged.csv



### **4.   Data Processing**
In this phase, we will check for errors, clean and transform data.



##### **4.1   Load packages:**
 
```{r}
library(tidyverse)
library(skimr)
library(lubridate)
library(ggpubr)
library(ggrepel)
library(janitor)
```



##### **4.2   Import data sets:**

```{r}
daily_activity <- read.csv (file = "/Users/User/Documents/fitbit_tracker_data_analysis/fitbit_data/dailyActivity_merged.csv")

daily_sleep <- read.csv(file = "/Users/User/Documents/fitbit_tracker_data_analysis/fitbit_data/sleepDay_merged.csv")

hourly_steps <- read.csv(file = "/Users/User/Documents/fitbit_tracker_data_analysis/fitbit_data/hourlySteps_merged.csv")

```


##### **4.3   Preview the imported files**
We will preview our selected data frames and check the summary of each column.

```{r}
head(daily_activity)
glimpse(daily_activity)

head(daily_sleep)
glimpse(daily_sleep)

head(hourly_steps)
glimpse(hourly_steps)
```



##### **4.4   Clean Data**


##### **Make the column names consistent**

Clean and rename columns.The column names have an inconsistent format so we'll make them consistent by changing everything to lower cases.

```{r}
daily_activity <- rename_with(daily_activity, tolower)
daily_sleep <- rename_with(daily_sleep, tolower)
hourly_steps <- rename_with(hourly_steps, tolower)
```



##### **Verify the number of users**

Verify that there all id numbers are unique in each data set.

```{r}
n_distinct(daily_activity$id)
n_distinct(daily_sleep$id)
n_distinct(hourly_steps$id)
```



##### **Check for duplicates**

```{r}
sum(duplicated(daily_activity))
sum(duplicated(daily_sleep))
sum(duplicated(hourly_steps))
```



##### **Remove any duplicates**
The daily_sleep data set contains 3 duplicates so, we'll remove these duplicates to prevent any errors..

```{r}
daily_activity <- daily_activity %>% 
  distinct() %>% 
  drop_na()

daily_sleep <- daily_sleep %>% 
  distinct() %>% 
  drop_na()

hourly_steps <- hourly_steps %>% 
  distinct() %>% 
  drop_na()
``` 



Verify that the duplicates have been removed.

```{r}
sum(duplicated(daily_sleep))
```



##### **Format the date and time column**
The date formats for each data set are not consistent so we have to convert the daily_sleep from date-time to date format, to ensure an error free aggregation process. The hourly_steps will be converted to date-time format.  

```{r}
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
```






### **5.   Data Analysis **
In this phase we will aggregate data sets, create tables, determine correlations, and put all these in visualizations for better understanding.

##### **5.1   Aggregate data sets:**
We'll merge the daily_activity and daily_sleep data sets for analysis.

```{r}
daily_activity_sleep <- merge(daily_activity, daily_sleep, by=c("id", "date"))

head(daily_activity_sleep)
```



##### **5.2   Determine the daily steps average by users:**
We'll carry out a summary statics to determine the average daily steps of users.

```{r}
daily_average <- daily_activity_sleep %>% 
  group_by(id) %>% 
  summarize(mean_daily_steps = mean(totalsteps), mean_daily_calories = mean(calories), mean_daily_sleep = mean(totalminutesasleep))

head(daily_average)
```



We'll add a new column that will categorize users according to the number of steps the take on a daily basis. This category is based on the ['Counting your steps' article](https://www.10000steps.org.au/articles/counting-steps/).

```{r}
user_type <- daily_average %>% 
  mutate(user_type = case_when(
    mean_daily_steps < 5000 ~ 'Sedentary',
    mean_daily_steps >= 5000 & mean_daily_steps < 7499 ~ 'Low Active',
    mean_daily_steps >= 7499 & mean_daily_steps < 9999 ~'Fairly Active',
    mean_daily_steps >= 10000 ~ 'Very Active'
  ))

head(user_type)
```



We'll construct a table showing each user type and the percent of each user type.
This is necessary to enable us construct a pie chart.

```{r}
user_type_percent <- user_type %>% 
  group_by(user_type) %>% 
  summarise(total = n()) %>%  
  mutate(totals = sum(total)) %>% 
  group_by(user_type) %>% 
  summarise(total_percent = total / totals) %>% 
  mutate(labels = scales::percent(total_percent))

user_type_percent$user_type <- factor(user_type_percent$user_type, levels = c("Very Active", "Fairly Active", "Low Active", "Sedentary"))
head(user_type_percent)
```



**Construct the Pie chart**


```{r}
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
```


  - User types are fairly distributed based on daily steps taken.
  
     + 38% of the users take between 7499 - 9999 steps daily.
     
     + 21% of the users take less than 5000 steps daily.
     
     + 21% of the users take between 5000 - 7499 steps daily.
     
     + 21% of the users take more than 10000 steps daily.


##### **5.3   Determine steps taken per weekday and minutes asleep per weekday**
We want to find out the days of the week that users are active and also the days that the sleep more.
Calculate the weekdays based on the column date and calculate the average steps walked and minutes by weekday.

```{r}
weekday_steps_sleep <- daily_activity_sleep %>% 
  mutate(weekday = weekdays(date))

weekday_steps_sleep$weekday <- ordered(weekday_steps_sleep$weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

weekday_steps_sleep <- weekday_steps_sleep %>% 
  group_by(weekday) %>% 
  summarise(daily_steps = mean(totalsteps), daily_sleep = mean(totalminutesasleep))
head(weekday_steps_sleep)
```



Now for a clearer, better understanding, we'll use a visualization to explain better.

```{r}
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
```


In the above, we can conclude that;

  - For daily steps per weekday;
  
       + Users take the most steps on Saturdays.
       + Users do not observe the required daily steps on Sundays.
  
 
  - For minutes asleep per weekday;
  
       + Users do not have the required amount of sleep.
       + Users sleep the most on Sundays.



##### **5.4   Determine what hours of the day users are most active**
Use the hourly_steps data set and separate the date-time column.

```{r}
hourly_steps <- hourly_steps %>%
  separate(date_time, into = c("date", "time"), sep= " ") %>%
  mutate(date = ymd(date)) 
```

Get the average total steps and create a visualization for better understanding.

```{r}
hourly_steps %>% 
  group_by(time) %>% 
  summarise(average_steps = mean(steptotal)) %>% 
  ggplot()+
  geom_col(mapping = aes(x=time, y = average_steps, fill = average_steps)) + 
  labs(title = "Hourly steps throughout the day", x="", y="") + 
  scale_fill_gradient(low = "green", high = "red")+
  theme(axis.text.x = element_text(angle = 90))
```




From the diagram above, we can observe the following;

  - Users are more active from 8am to 7pm.
  - Users are most active from 12pm - 2pm and also from 5pm - 7pm.



##### **5.5   Determine correlations**

##### **Determine if there is any correlation between total steps and calories burned:**

```{r}
ggplot(daily_activity_sleep, aes(x=totalsteps, y=calories))+
    geom_jitter() +
    geom_smooth(color = "red") + 
    labs(title = "Daily steps vs Calories", subtitle = "Correlation between daily steps and calories") +
    theme(panel.background = element_blank(),
          plot.title = element_text( size=14))
```


From the above, we can see that the higher the number of daily steps taken, the more calories are burned. Hence, there is a positive correlation between daily steps taken and calories burned.



##### **Determine if there is any correlation between total steps and total minutes asleep:**

```{r}
ggplot(daily_activity_sleep, aes(x=totalsteps, y=totalminutesasleep))+
  geom_jitter() +
  geom_smooth(color = "red") + 
  labs(title = "Daily steps vs Minutes asleep", x = "Daily steps", y= "Minutes asleep") +
  theme(panel.background = element_blank(),
        plot.title = element_text( size=14))
```


From the graph above, we can see that there is no correlation between daily steps and minutes asleep. This is because, when daily steps are going higher, it does not affect the minutes asleep.

##### **Determine if there is any correlation between total minutes asleep and total time in bed:** 

```{r}
ggplot(daily_activity_sleep, aes(x=totaltimeinbed, y=totalminutesasleep))+
  geom_jitter() +
  geom_smooth(color = "red") + 
  labs(title = "Total time in bed vs Minutes asleep", x = "Total time in bed", y= "Minutes asleep") +
  theme(panel.background = element_blank(),
        plot.title = element_text( size=14))
```

From the graph, we can see that there is a positive correlation between total time in bed and total minutes asleep.




##### **5.6   Determine the daily usage of the smart device by users:**
We want to know the usage of the smart device by users on a daily basis and categorize them.

```{r}
daily_use <- daily_activity_sleep %>%
  group_by(id) %>%
  summarize(days_used=sum(n())) %>%
  mutate(usage = case_when(
    days_used >= 1 & days_used <= 10 ~ "low use",
    days_used >= 11 & days_used <= 20 ~ "moderate use", 
    days_used >= 21 & days_used <= 31 ~ "high use", 
  ))

head(daily_use)
```


We have to construct a pie chart, but to do this, we must first create a new table showing the percent per daily usage.

```{r}
daily_use_percent <- daily_use %>%
  group_by(usage) %>%
  summarise(total = n()) %>%
  mutate(totals = sum(total)) %>%
  group_by(usage) %>%
  summarise(total_percent = total / totals) %>%
  mutate(labels = scales::percent(total_percent))

daily_use_percent$usage <- factor(daily_use_percent$usage, levels = c("high use", "moderate use", "low use"))

head(daily_use_percent)
```


Now, we'll construct the pie chart.

```{r}
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
```


From the pie chart above, we can see that;

  - 50% of the users use their smart devices frequently.
  
  - 12% of the users use their smart devices moderately.
  
  - 38% of the users rarely use their devices.
  
  
  
  
##### **6.   Conclusions**

   - From our analysis, users sleep for less than 8 hours every day in a week. The minimum being 6.5 hours on Thursdays and the maximum being 7.5 hours on Sundays.
   
   - They reach the target for daily steps every day of the week except on Sundays. This is justified seeing that they tend to sleep more on Sundays.
   
   - Users take more steps between 8am - 7pm. Within this time frame, users take the most steps between the hours of 12pm - 2pm in the afternoons and 5pm - 7pm in the evenings. 
     
   - There is a positive correlation between daily steps taken and amount of calories burned. The more steps taken, the more calories are burned. 
   
   - There is no correlation between daily steps taken and total minutes asleep.
   
   - There is a positive correlation between total time in bed and total minutes asleep.The higher the time spent in bed, the higher the total hours slept.
   
   - We also discovered that 50% of users use the smart device very frequently, 12% use the smart device at a moderate rate, and 38% rarely use the smart device.


##### **6.1   Recommendations**
Based on our conclusions above, the recommendations that we will give to Bellabeat to help improve their marketing strategy are as follows;
 
  - **Send weekly reports:** Weekly summary statistics can be sent to users showing them;
      + How many steps they took during the week.
        
      + How many calories they burned during the week.
        
      + New milestones their attained and the rewards attached to such milestones for example, a Bellabeat coin.
    
  - **Send notifications:** 
      + Users should be notified to go to bed on time. The app can send these notifications 30 minutes prior to the time so users can prepare to go to bed. 
      
      + Users should also be notified to meet their daily steps target.
  
  - Pop up the benefits of subscribing to the Bellabeat membership so that users will be aware and motivated to subscribe.
    
  - **Create a Bellabeat community:** Users should be able to see and connect with other users on the app. This way, the know they are not alone
    and get motivated by other users' accomplishments.
    
    

##### **6.2   Additional Suggestion**
Due to the data limitations highlighted earlier, there is a need for Bellabeat to get more data especially in the following areas;

  - Demographics such as age, gender, location.
  
  - Sample size
  
  - Data from Bellabeat users.

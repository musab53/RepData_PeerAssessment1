---
title: "Reproducible Research Week 2 Project"
author: "Masaab Al Khalaf"
date: "26 April 2020"
output: html_document
---

## Loading the Data for processing 

```{r lodingdata}

data <- read.csv("/Users/hayosh/Desktop/Coursera/repdata_data_activity/activity.csv", header = TRUE)

head(data)

```


##1 Calculating total number of steps taken each day 

```{r }




data_date <- tapply(data$steps, data$date, FUN = sum, na.rm = TRUE)

hist(data_date, main = "Daily Total Stpes", xlab = "Daily Steps", col = "Blue", breaks = 30)

```

##2 Calculating and repot the Mean & Median of total number of stpes taken per day 

Mean

```{r}

mean(data_date)

```

Median 
```{r}

median(data_date)

```

## 3 What is the average daily pattern?

1. Making a time series 
```{r}
library(ggplot2)

daily_avg <- aggregate(x = list(steps=data$steps), by=list(interval=data$interval), FUN = mean, na.rm = TRUE)

ggplot(data = daily_avg, aes(x = interval, y = steps)) + geom_line()  + xlab("5-minute interval") + ylab("average number of steps")

```

2.Which 5-minute interval, On average across all the days in the dataset, contains the maximum number of steps ?

```{r}

daily_avg[which.max(daily_avg$steps),]

```


## Imputing missing values 

1. Calculating and report the toalt number of missing values in dataset 

```{r}

missing_values <- sum(is.na(data))

# number of missing values 
missing_values

```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.



3. Creating a new dataset that is equal to the original dataset but with the missing data filled in.
```{r}

# I will replace the missing values with mean for 5-minute interval 

library(dplyr)

filling_missing_values <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))

replaced_data <- data%>% group_by(interval) %>% mutate(steps = filling_missing_values(steps))


# checking if data is replaced 
head(replaced_data)  


```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.

```{r}

steps_total <- tapply(replaced_data$steps, replaced_data$date, FUN = sum)


hist(steps_total, main = "Daily Total Stpes", xlab = "Daily Steps", col = "Blue", breaks = 30)


```

Mean and Median
```{r}
mean(steps_total)


median(steps_total)

```
Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

Answer :  After filling the missing values both mean and median increases, as we ignored NA values in the first part, and by replacing NA values mean and median got increased. 


##Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```{r}

# unsing the filled dataset to check day if weekdays or weekends 

replaced_data$date <- as.Date(replaced_data$date)
replaced_data$weekday <- weekdays(replaced_data$date)

replaced_data$weekend <- ifelse(replaced_data$weekday == "Saturday" | replaced_data$weekday == "Sunday", "Weekend", "Weekday")

```

2. Make a panel plot containing a time series plot  of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```{r}

library(ggplot2)

week_end_day_mean <- aggregate(replaced_data$steps, by=list(replaced_data$weekend, replaced_data$interval), na.omit(mean))

names(week_end_day_mean) <- c("weekend", "interval", "steps")

ggplot(week_end_day_mean, aes(x = interval, y = steps)) + geom_line() + facet_grid(weekend~.) +xlab("Interval") +ylab("Mean") + ggtitle("Avg Number of steps, Weekdays & Weekdends")



```

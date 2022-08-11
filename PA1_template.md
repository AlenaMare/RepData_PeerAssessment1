---
title: "PA1_template.Rmd"
output: rm_document
---

This file contains my submitted assignment for Reproducible Research - Week 2` project.

Loading the data
================
```{r, echo = TRUE}
unzip(zipfile = "activity.zip")
activity <- read.csv("activity.csv")
activity$date <- as.Date(activity$date)
```

What is mean total number of steps taken per day?
================
Calculating the total number of steps taken per day
```{r, echo = TRUE}
steps.per.day <- aggregate(steps ~ date, activity, sum, na.rm=TRUE)
print(steps.per.day)
```

Making a histogram of the total number of steps taken each day.
```{r, echo = TRUE}
hist(steps.per.day$steps, main = "Histogram of the total number of steps per day",
     col = "yellow", xlab = "Steps", ylim = c(0, 30))
```

Calculating and reporting the mean of the total number of steps taken per day
```{r, echo = TRUE}
mean.steps.per.day <- mean(steps.per.day$steps, na.rm = TRUE)
print(mean.steps.per.day)
```

Calculating and reporting the mean of the total number of steps taken per day
```{r, echo = TRUE}
median.steps.per.day <- median(steps.per.day$steps, na.rm = TRUE)
print(median.steps.per.day)
```

What is the average daily activity pattern?
=================
Showing the average daily activity pattern, using a time series plot of the 5-minute interval and the avarage number of steps taken, avaraged across all days.
```{r, echo = TRUE}
stepsPerInterval <-aggregate(steps~interval, data=activity, mean, na.rm=TRUE)
plot(steps~interval, data=stepsPerInterval, type="l",
     xlab = "5-minute interval", ylab = "Avarage Number of Steps", main = "Daily activity pattern of intervals and average number of steps")
```

Showing the 5-minute interval with the maximum number of steps, on avarage across al the days in the dataset.
```{r, echo = TRUE}
max_steps <- stepsPerInterval[which.max(stepsPerInterval$steps),]$interval
print(max_steps)
```

Imputing missing values
===================
Calculating and reporting the total number of missing values in the dataset.
```{r, echo = TRUE}
missing <- sum(is.na(activity$steps))
print(missing)
```

Filling in all of the missing values in the dataset, using mean number of steps per interval.
```{r, echo = TRUE}
activity_without_NA <- activity  
for (i in 1:nrow(activity)){
        if(is.na(activity$steps[i])){
                activity_without_NA$steps[i]<- stepsPerInterval$steps[activity_without_NA$interval[i] == stepsPerInterval$interval]
        }
}
```

Making a histogram of the total number of steps taken each day. 
```{r, echo = TRUE}
library(dplyr)
stepsPerDay <- activity_without_NA %>%
        group_by(date) %>%
        summarize(sumsteps = sum(steps, na.rm = TRUE)) 

hist(stepsPerDay$sumsteps, main = "Histogram of the Total Number of Steps", 
    xlab="Steps")
```

Calculating and reporting the mean and median total number of steps taken per day. 
```{r, echo = TRUE}
mean <- round(mean(stepsPerDay$sumsteps), digits = 2)
median <- round(median(stepsPerDay$sumsteps), digits = 2)
print(mean)
print(median)
```

Comparing old and new mean and median.
```{r, echo = TRUE}
Compare <- data.frame(mean = c(mean.steps.per.day,mean),median = c(median.steps.per.day,median))
rownames(Compare) <- c("Pre NA Transformation", "Post NA Transformation")
print(Compare)
```

Regarding mean total number of steps, the value is exaxtly same as the estimates from the first part of the assignment. The mean differs slightly, originaly being 10765, and now plus added 1.19.There was zero impact of imputing missing data on the estimates of the mean total daily number of steps, because it was filled with mean number. Contrary, it had slight impact on median, as mean was slightly higher, therefore increasing median.

Are there differences in activity patterns between weekdays and weekends?
==================

Creating a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
```{r, echo = TRUE}
activity_without_NA$date <- as.Date(activity_without_NA$date)
activity_without_NA$day <- weekdays(activity_without_NA$date)
for (i in 1:nrow(activity_without_NA)) {
  if (activity_without_NA[i,]$day %in% c("sobota", "neděle")) {
      activity_without_NA[i,]$day<- "weekend"
  } 
  else{
      activity_without_NA[i,]$day <- "weekday"
    }
}
```

Making a panel plot containing a time series plot of the 5-minute interval and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 
```{r, echo = TRUE}
Weekday_activity <- filter(activity_without_NA, activity_without_NA$day == "weekday")
Weekend_activity <- filter(activity_without_NA, activity_without_NA$day == "weekend")

Weekday_activity <- Weekday_activity %>%
        group_by(interval) %>%
        summarize(steps = mean(steps)) 
Weekday_activity$day <- "weekday"

Weekend_activity <- Weekend_activity %>%
        group_by(interval) %>%
        summarize(steps = mean(steps)) 
Weekend_activity$day <- "weekend"

par(mfcol = c(1,2))
plot(Weekday_activity$interval, Weekday_activity$steps,
     type = "l",
     xlab = "Steps",
     ylab = "Interval",
     main = "Number of Steps on Weekdays")
plot(Weekend_activity$interval, Weekend_activity$steps,
     type = "l",
     xlab = "Steps",
     ylab = "Interval",
     main = "Number of Steps on Weekends")
```


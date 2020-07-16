---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
- Set Global Echo = On

- Load data
```
if (!file.exists("activity.csv") )
{
  dlurl <- 'http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip'  
  download.file(dlurl,destfile='repdata%2Fdata%2Factivity.zip',mode='wb')  
  unzip('repdata%2Fdata%2Factivity.zip')
}
```

- Read data
```
data <- read.csv("activity.csv")  
```

## What is mean total number of steps taken per day?
```
steps_by_day <- aggregate(steps ~ date, data, sum)
hist(steps_by_day$steps, main = paste("Total Steps Each Day"), col="green",xlab="Number of Steps")
```
![Figure 1](https://github.com/Angiepao/RepData_PeerAssessment1/tree/master/Figures/PLOT1.png?raw=true "Total Steps Each Day")
```
rmean <- mean(steps_by_day$steps)
rmean

# 10766.19

rmedian <- median(steps_by_day$steps)
rmedian

# 10765
```
A: The mean is 1.076618910^{4} and the median is 10765

## What is the average daily activity pattern?
```
steps_by_interval <- aggregate(steps ~ interval, data, mean)
plot(steps_by_interval$interval,steps_by_interval$steps, type="l", xlab="Interval", ylab="Number of Steps",main="Average Number of Steps per Day by Interval")
```
![Figure 2](https://github.com/Angiepao/RepData_PeerAssessment1/tree/master/Figures/PLOT2.png?raw=true "Average Number of Steps per Day by Interval")

``` 
max_interval <- steps_by_interval[which.max(steps_by_interval$steps),1]
max_interval

```
A: The interval with most steps is 835

## Imputing missing values
- Calculate and report the total number of missing values in the dataset
```
NATotal <- sum(!complete.cases(data))
NATotal
```
A: Total Number of Missing values are 2304
- Using Mean for the day compute missing values
```
StepsAverage <- aggregate(steps ~ interval, data = data, FUN = mean)
fillNA <- numeric()
for (i in 1:nrow(data)) {
  obs <- data[i, ]
  if (is.na(obs$steps)) {
    steps <- subset(StepsAverage, interval == obs$interval)$steps
  } else {
    steps <- obs$steps
  }
  fillNA <- c(fillNA, steps)
}
```
- Create a new dataset including the imputed missing values
```
new_activity <- data
new_activity$steps <- fillNA
```
- Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.
```
StepsTotalUnion <- aggregate(steps ~ date, data = new_activity, sum, na.rm = TRUE)
hist(StepsTotalUnion$steps, main = paste("Total Steps Each Day"), col="blue", xlab="Number of Steps")
# Create Histogram to show difference. 
hist(steps_by_day$steps, main = paste("Total Steps Each Day"), col="green", xlab="Number of Steps", add=T)
legend("topright", c("Imputed", "Non-imputed"), col=c("blue", "green"), lwd=10)
```
![Figure 3](https://github.com/Angiepao/RepData_PeerAssessment1/tree/master/Figures/PLOT3.png?raw=true "Total Steps Each Day")

- Calculate mean
```
rmeantotal <- mean(StepsTotalUnion$steps)
rmeantotal
```
- Calculate median
```
rmediantotal <- median(StepsTotalUnion$steps)
rmediantotal
```
- Do these values differ from the estimates from the first part of the assignment?
```
rmediandiff <- rmediantotal - rmedian
rmediandiff

rmeandiff <- rmeantotal - rmean
rmeandiff
```
A: The mean(Mean Var: 0) is the same however the median does have a small variance(Median Var:1.1886792). Between the total which includes the missing values to the base
- What is the impact of imputing missing data on the estimates of the total daily number of steps?
A: On observation the impact of the missing data has the biggest effect on the 10000 - 150000 step interval and changes frequency from 27.5 to 35 a variance of 7.5

## Are there differences in activity patterns between weekdays and weekends?
- Created a plot to compare and contrast number of steps between the week and weekend. There is a higher peak earlier on weekdays, and more overall activity on weekends.
```
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", 
              "Friday")
new_activity$dow = as.factor(ifelse(is.element(weekdays(as.Date(new_activity$date)),weekdays), "Weekday", "Weekend"))
StepsTotalUnion <- aggregate(steps ~ interval + dow, new_activity, mean)
library(lattice)
xyplot(StepsTotalUnion$steps ~ StepsTotalUnion$interval|StepsTotalUnion$dow, main="Average Steps per Day by Interval",xlab="Interval", ylab="Steps",layout=c(1,2), type="l")
```
![Figure 4](https://github.com/Angiepao/RepData_PeerAssessment1/tree/master/Figures/PLOT4.png?raw=true "Average Steps per Day by Interval")

A: Yes, in weekday there is a peak between 500 and 1000 intervals until 200 steps, whilst weekend shows peaks more constants between 50 and 150 at all intervals. 

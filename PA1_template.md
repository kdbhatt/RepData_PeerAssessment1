# Reproducible Research: Peer Assessment 1


```r
#Replace with your current working directory
setwd("C:/Krishna/MyProject/DataScience/5_Reproducible_Research/RepData_PeerAssessment1")
```

## Loading and preprocessing the data

#####1. Load the data (i.e. read.csv())


```r
activity <- read.csv("activity.csv", stringsAsFactors=FALSE)
```

#####2.Process/transform the data (if necessary) into a format suitable for your analysis



```r
#Convert string to date 
activity$date <- as.POSIXct(activity$date, format="%Y-%m-%d")
```

## What is mean total number of steps taken per day?

#####1. Calculate the total number of steps taken per day


```r
total_activity <- aggregate(activity$steps, by=list(activity$date), FUN=sum, na.rm=TRUE)
names(total_activity) <- c("date", "total")
head(total_activity,10)
```

```
##          date total
## 1  2012-10-01     0
## 2  2012-10-02   126
## 3  2012-10-03 11352
## 4  2012-10-04 12116
## 5  2012-10-05 13294
## 6  2012-10-06 15420
## 7  2012-10-07 11015
## 8  2012-10-08     0
## 9  2012-10-09 12811
## 10 2012-10-10  9900
```

#####2. Make a histogram of the total number of steps taken each day

```r
hist(total_activity$total, 
     col="blue", 
     xlab="Total number of steps", 
     main="Histogram of the total number of steps taken each day")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)

#####3. Calculate and report the mean and median of the total number of steps taken per day

```r
mean(total_activity$total)
```

```
## [1] 9354.23
```

```r
median(total_activity$total)
```

```
## [1] 10395
```


## What is the average daily activity pattern?

#####1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
mean_activity <- aggregate(activity$steps, 
                       by=list(activity$interval), 
                       FUN=mean, 
                       na.rm=TRUE)
names(mean_activity) <- c("interval", "mean")
head(mean_activity,10)
```

```
##    interval      mean
## 1         0 1.7169811
## 2         5 0.3396226
## 3        10 0.1320755
## 4        15 0.1509434
## 5        20 0.0754717
## 6        25 2.0943396
## 7        30 0.5283019
## 8        35 0.8679245
## 9        40 0.0000000
## 10       45 1.4716981
```


```r
plot(mean_activity$interval, 
     mean_activity$mean, 
     type="l", 
     col="blue", 
     lwd=2, 
     xlab="Interval [minutes]", 
     ylab="Average number of steps", 
     main="Time-series of the average number of steps per intervals")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)

#####2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
max_pos <- which(mean_activity$mean == max(mean_activity$mean))
mean_activity[max_pos, 1]
```

```
## [1] 835
```


## Imputing missing values

#####1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
NA_count <- sum(is.na(activity$steps))
```

#####2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
na_pos <- which(is.na(activity$steps))
mean_vec <- rep(mean(activity$steps, na.rm=TRUE), times=length(na_pos))
head(mean_vec,10)
```

```
##  [1] 37.3826 37.3826 37.3826 37.3826 37.3826 37.3826 37.3826 37.3826
##  [9] 37.3826 37.3826
```

#####3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
activity[na_pos, "steps"] <- mean_vec
head(activity,10)
```

```
##      steps       date interval
## 1  37.3826 2012-10-01        0
## 2  37.3826 2012-10-01        5
## 3  37.3826 2012-10-01       10
## 4  37.3826 2012-10-01       15
## 5  37.3826 2012-10-01       20
## 6  37.3826 2012-10-01       25
## 7  37.3826 2012-10-01       30
## 8  37.3826 2012-10-01       35
## 9  37.3826 2012-10-01       40
## 10 37.3826 2012-10-01       45
```

#####4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
hist(sum_data$total, 
     breaks=seq(from=0, to=25000, by=2500),
     col="blue", 
     xlab="Total number of steps", 
     ylim=c(0, 30), 
     main="Histogram of the total number of steps taken each day\n(NA replaced by mean value)")
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)


## Are there differences in activity patterns between weekdays and weekends?

#####1. Create a new factor variable in the dataset with two levels ??? ???weekday??? and ???weekend??? indicating whether a given date is a weekday or weekend day.


Add new field for day

```r
activity <- cbind(activity,weekday=tolower(weekdays(activity$date)))
```

add new field for weekday or weekend


```r
activity <- cbind(activity,
                  daytype=ifelse(activity$weekday == "saturday" |activity$weekday == "sunday", "weekend","weekday"))
```

#####2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.



```r
mean_activity <- aggregate(activity$steps, 
                       by=list(activity$daytype, 
                               activity$weekday, activity$interval), mean)
names(mean_activity) <- c("daytype", "weekday", "interval", "mean")
```

plot


```r
library(lattice)

xyplot(mean ~ interval | daytype, mean_activity, 
type="l", 
lwd=1, 
xlab="Interval", 
ylab="Number of steps", 
layout=c(1,2))
```

![](PA1_template_files/figure-html/unnamed-chunk-17-1.png)


---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

``` r
data <- read.csv("activity.csv")
data$date <- as.Date(data$date)
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```



## What is mean total number of steps taken per day?

``` r
# Remove missing values
data_noNA <- na.omit(data)

# Total steps per day
total_steps_day <- aggregate(steps ~ date, data=data_noNA, sum)

head(total_steps_day)
```

```
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```

``` r
hist(total_steps_day$steps,
     main="Histogram of Total Steps Per Day",
     xlab="Total Steps Per Day",
     col="lightblue",
     border="black")
```

![](PA1_template_files/figure-html/histogram-steps-1.png)<!-- -->


``` r
mean_steps <- mean(total_steps_day$steps)
median_steps <- median(total_steps_day$steps)

mean_steps
```

```
## [1] 10766.19
```

``` r
median_steps
```

```
## [1] 10765
```



## What is the average daily activity pattern?

``` r
# Average steps per 5-minute interval
avg_interval <- aggregate(steps ~ interval, data=data_noNA, mean)

head(avg_interval)
```

```
##   interval     steps
## 1        0 1.7169811
## 2        5 0.3396226
## 3       10 0.1320755
## 4       15 0.1509434
## 5       20 0.0754717
## 6       25 2.0943396
```

``` r
plot(avg_interval$interval,
     avg_interval$steps,
     type="l",
     col="blue",
     xlab="5-minute Interval",
     ylab="Average Steps",
     main="Average Daily Activity Pattern")
```

![](PA1_template_files/figure-html/time-series-plot-1.png)<!-- -->

``` r
max_interval <- avg_interval[which.max(avg_interval$steps), ]
max_interval
```

```
##     interval    steps
## 104      835 206.1698
```
## Imputing missing values

``` r
sum(is.na(data$steps))
```

```
## [1] 2304
```

``` r
# Copy original dataset
data_filled <- data

# Create named vector of interval means
interval_means <- avg_interval$steps
names(interval_means) <- avg_interval$interval

# Replace NA values
for(i in 1:nrow(data_filled)){
  if(is.na(data_filled$steps[i])){
    data_filled$steps[i] <- interval_means[as.character(data_filled$interval[i])]
  }
}

sum(is.na(data_filled$steps))
```

```
## [1] 0
```

``` r
total_steps_filled <- aggregate(steps ~ date, data=data_filled, sum)

mean(total_steps_filled$steps)
```

```
## [1] 10766.19
```

``` r
median(total_steps_filled$steps)
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?

``` r
# Add weekday/weekend column
data_filled$day_type <- ifelse(weekdays(data_filled$date) %in% 
                                 c("Saturday", "Sunday"),
                               "Weekend", "Weekday")

# Average steps per interval by day type
avg_daytype <- aggregate(steps ~ interval + day_type,
                         data=data_filled, mean)

head(avg_daytype)
```

```
##   interval day_type      steps
## 1        0  Weekday 2.25115304
## 2        5  Weekday 0.44528302
## 3       10  Weekday 0.17316562
## 4       15  Weekday 0.19790356
## 5       20  Weekday 0.09895178
## 6       25  Weekday 1.59035639
```

``` r
library(lattice)

xyplot(steps ~ interval | day_type,
       data=avg_daytype,
       type="l",
       layout=c(1,2),
       xlab="5-minute Interval",
       ylab="Average Steps",
       main="Weekday vs Weekend Activity Pattern")
```

![](PA1_template_files/figure-html/plot-weekday-weekend-1.png)<!-- -->

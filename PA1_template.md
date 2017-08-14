# Reproducible Research: Peer Assessment 1
##As requested we need to set the default of echo to be true.

```r
library(knitr)
opts_chunk$set(echo = TRUE)
```

##Loading required libraries

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:base':
## 
##     date
```

```r
library(ggplot2)
```

## Loading and preprocessing the data (Point 1 of assignment)


```r
if(!file.exists('activity.csv')){
    unzip('activity.zip')
}
activityData <- read.csv('activity.csv')
```
## Process/transform the data (if necessary) into a format suitable for your analysis

```r
data <- read.csv("activity.csv", header = TRUE, sep = ',', colClasses = c("numeric", "character", "integer"))

data$date <- ymd(data$date)
```
## We can check the data with str() and head() for confirmation.

```r
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
head(data)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```
## What is mean total number of steps taken per day?
## We use dplyr to calculate total number of steps per day and group by dates.

```r
steps <- data %>%
  filter(!is.na(steps)) %>%
  group_by(date) %>%
  summarize(steps = sum(steps)) %>%
  print
```

```
## # A tibble: 53 x 2
##          date steps
##        <date> <dbl>
##  1 2012-10-02   126
##  2 2012-10-03 11352
##  3 2012-10-04 12116
##  4 2012-10-05 13294
##  5 2012-10-06 15420
##  6 2012-10-07 11015
##  7 2012-10-09 12811
##  8 2012-10-10  9900
##  9 2012-10-11 10304
## 10 2012-10-12 17382
## # ... with 43 more rows
```

## Create histogram (Point 2 of assignment)

```r
ggplot(steps, aes(x = steps)) +
  geom_histogram(fill = "firebrick", binwidth = 1000) +
  labs(title = "Histogram of Steps per day", x = "Steps per day", y = "Frequency")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

## Calculating Mean and Median (Point 3 of assignment)

```r
mean_steps <- mean(steps$steps, na.rm = TRUE)

median_steps <- median(steps$steps, na.rm = TRUE)

median_steps
```

```
## [1] 10765
```

```r
## [1] 10765
mean_steps
```

```
## [1] 10766.19
```

```r
## [1] 10766.19
```

## What is the average daily activity pattern?

## Lets first calculated the average number of steps taken in each 5 minute interval using dplyr and group by interbal

```r
interval <- data %>%
  filter(!is.na(steps)) %>%
  group_by(interval) %>%
  summarize(steps = mean(steps))
```
## Now we can use ggplot for the 5 minute interval and average steps taken time series. (Point 4 of Assignment)

```r
  ggplot(interval, aes(x=interval, y=steps)) +
  geom_line(color = "firebrick")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->
  
## The 5-minute interval that, on average, contains the maximum number of steps (Point 5 of assignment)

```r
  interval[which.max(interval$steps),]
```

```
## # A tibble: 1 x 2
##   interval    steps
##      <int>    <dbl>
## 1      835 206.1698
```

```r
## Imputing missing values (Point 6 of assignment)

## First lets sum all missing values

sum(is.na(data$steps))
```

```
## [1] 2304
```

```r
## [1] 2304
## so we have 2304 missing values
## we will fill these missing values with the average number of steps in the same 5 minute interval.

data_full <- data
nas <- is.na(data_full$steps)
avg_interval <- tapply(data_full$steps, data_full$interval, mean, na.rm=TRUE, simplify=TRUE)
data_full$steps[nas] <- avg_interval[as.character(data_full$interval[nas])]

##Check that there are no missing values:
## we can confirm that there are no missing values in the new dataset we created.

sum(is.na(data_full$steps))
```

```
## [1] 0
```

```r
## [1] 0
## so there are no missing values now.
```

## Now Lets Calculate the number of steps taken in each 5-minute interval per day using dplyr and group by interval. Use ggplot for making the histogram:


```r
steps_full <- data_full %>%
  filter(!is.na(steps)) %>%
  group_by(date) %>%
  summarize(steps = sum(steps)) %>%
  print
```

```
## # A tibble: 61 x 2
##          date    steps
##        <date>    <dbl>
##  1 2012-10-01 10766.19
##  2 2012-10-02   126.00
##  3 2012-10-03 11352.00
##  4 2012-10-04 12116.00
##  5 2012-10-05 13294.00
##  6 2012-10-06 15420.00
##  7 2012-10-07 11015.00
##  8 2012-10-08 10766.19
##  9 2012-10-09 12811.00
## 10 2012-10-10  9900.00
## # ... with 51 more rows
```

```r
##Histogram of the total number of steps taken each day after missing values are imputed (Point 7 of assignment)
  ggplot(steps_full, aes(x = steps)) +
  geom_histogram(fill = "firebrick", binwidth = 1000) +
  labs(title = "Histogram of Steps per day, including missing values", x = "Steps per day", y = "Frequency")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

```r
mean_steps_full <- mean(steps_full$steps, na.rm = TRUE)

mean_steps_full
```

```
## [1] 10766.19
```

```r
## [1] 10766.19

median_steps_full <- median(steps_full$steps, na.rm = TRUE)
median_steps_full
```

```
## [1] 10766.19
```

```r
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?

```r
data_full <- mutate(data_full, weektype = ifelse(weekdays(data_full$date) == "Saturday" | weekdays(data_full$date) == "Sunday", "weekend", "weekday"))
data_full$weektype <- as.factor(data_full$weektype)
head(data_full)
```

```
##       steps       date interval weektype
## 1 1.7169811 2012-10-01        0  weekday
## 2 0.3396226 2012-10-01        5  weekday
## 3 0.1320755 2012-10-01       10  weekday
## 4 0.1509434 2012-10-01       15  weekday
## 5 0.0754717 2012-10-01       20  weekday
## 6 2.0943396 2012-10-01       25  weekday
```

```r
## Lets Calculate the average steps in the 5-minute interval and use ggplot for making the time series of the 5-minute interval for weekday and weekend, and compare the average steps:

interval_full <- data_full %>%
  group_by(interval, weektype) %>%
  summarise(steps = mean(steps))
  
s <- ggplot(interval_full, aes(x=interval, y=steps, color = weektype)) +
  geom_line() +
  facet_wrap(~weektype, ncol = 1, nrow=2)
print(s)
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

# Reproducible Research: Peer Assessment 1
This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

## Loading and preprocessing the data


```r
data <- read.csv("activity.csv")
summary(data)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```
Initially, time is represented in military time intervals.  
  
To make time more readable, I have changed the class to POSIXlt through strptime() and formatted it %H:%M. Here is the resulting table:

```r
data$interval <- as.character(data$interval)
for (i in 1:length(data$interval)) {
    if (nchar(data$interval[i]) == 1){
        data$interval[i] <- paste("000", data$interval[i], sep = "")
    } else if (nchar(data$interval[i]) == 2){
        data$interval[i] <- paste("00", data$interval[i], sep = "")
    } else if (nchar(data$interval[i]) == 3){
        data$interval[i] <- paste("0", data$interval[i], sep = "")
    }
}
data$interval <- format(strptime(data$interval, format = "%H%M"), format = "%H:%M")
head(data)
```

```
##   steps       date interval
## 1    NA 2012-10-01    00:00
## 2    NA 2012-10-01    00:05
## 3    NA 2012-10-01    00:10
## 4    NA 2012-10-01    00:15
## 5    NA 2012-10-01    00:20
## 6    NA 2012-10-01    00:25
```

## What is mean total number of steps taken per day?

To answer this question, calculate the sum of all steps taken in the data across the days spanned.


```r
library("dplyr")
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
sumsteps <- sum(data$steps, na.rm = TRUE)
days <- length(levels(data$date))
```

The sum of all steps taken was 570608. And a look at the levels of factors in the date column tells us that there were 61 days measured in the data set.  
That makes the mean across all the days:  

```r
sumsteps / days
```

```
## [1] 9354.23
```

Let's take a look at groupings by each of the 61 days.  


```r
avgdata <- data %>%
    group_by(date) %>%
    summarize(total_steps = sum(steps, na.rm = TRUE), avg_steps = mean(steps, na.rm = TRUE), median_steps = median(steps, na.rm = TRUE))
head(avgdata)
```

```
## Source: local data frame [6 x 4]
## 
##         date total_steps avg_steps median_steps
## 1 2012-10-01           0       NaN           NA
## 2 2012-10-02         126   0.43750            0
## 3 2012-10-03       11352  39.41667            0
## 4 2012-10-04       12116  42.06944            0
## 5 2012-10-05       13294  46.15972            0
## 6 2012-10-06       15420  53.54167            0
```

```r
hist(avgdata$total_steps, breaks = 20)
```

![](PeerAssessment1_files/figure-html/unnamed-chunk-5-1.png) 
  
The mean of total number of steps per day is 9354.2295082 and the median of the total number of steps per day is 10395.

## What is the average daily activity pattern?


```r
plot.ts(avgdata$total_steps)
```

![](PeerAssessment1_files/figure-html/unnamed-chunk-6-1.png) 

```r
max_steps_date <- data[which.max(data$steps), 'date'] 
max_steps_time <- data[which.max(data$steps), 'interval']
```

On 2012-11-27 at 06:15 was when the maximum number of steps was taken during a 5 minute interval over the course of the data set.

## Inputing missing values

Let's start with calculating the number of missing elements in the data set.

```r
sum(is.na(data$steps))
```

```
## [1] 2304
```
Let's do something meaningful about these missing elements.


```r
data[is.na(data$steps), 'steps'] <- sumsteps / days / 288
sum(is.na(data$steps))
```

```
## [1] 0
```

We took the average steps per interval of all days and replaced the NAs.


```r
avgdata <- data %>%
    group_by(date) %>%
    summarize(total_steps = sum(steps, na.rm = TRUE), avg_steps = mean(steps, na.rm = TRUE), median_steps = median(steps, na.rm = TRUE))
hist(avgdata$total_steps, breaks = 20)
```

![](PeerAssessment1_files/figure-html/unnamed-chunk-9-1.png) 

```r
head(avgdata)
```

```
## Source: local data frame [6 x 4]
## 
##         date total_steps avg_steps median_steps
## 1 2012-10-01     9354.23  32.47996     32.47996
## 2 2012-10-02      126.00   0.43750      0.00000
## 3 2012-10-03    11352.00  39.41667      0.00000
## 4 2012-10-04    12116.00  42.06944      0.00000
## 5 2012-10-05    13294.00  46.15972      0.00000
## 6 2012-10-06    15420.00  53.54167      0.00000
```

## Are there differences in activity patterns between weekdays and weekends?


# Reproducible Research: Peer Assessment 1
Author
Rudolf Stolz  
Version 1.0.0  
Date 140609

## Loading and preprocessing the data

```r
activity <- read.csv(unz("activity.zip", "activity.csv"), header = TRUE, sep = ",")
sum(is.na(activity$steps))
```

```
## [1] 2304
```



## What is mean total number of steps taken per day?



## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?








# Reproducible Research: Peer Assessment 1



## Loading and preprocessing the data

Loading the data:


```r
data <- read.csv(unz("activity.zip", "activity.csv"), sep = ",", header = TRUE)
data$date <- as.Date(data$date)
```

## What is mean total number of steps taken per day?

Missing values in the dataset are ignored.


```r
total <- tapply(data$steps, as.factor(data$date), sum)
hist(total, breaks = 20, main = "Histogram of the total number of steps taken each day", xlab = "Number of steps")
```

![plot of chunk histogramteps](figure/histogramteps.png) 

Mean total number of steps taken per day:

```r
steps.mean <- mean(total, na.rm = TRUE)
steps.mean
```

```
## [1] 10766
```

Median total number of steps taken per day:

```r
steps.median <- median(total, na.rm = TRUE)
steps.median
```

```
## [1] 10765
```

## What is the average daily activity pattern?

Time series plot of the 5-minute interval and the average number of steps taken, averaged across all days:


```r
x <- levels(as.factor(data$interval))
y <- sapply(x, function(i) {mean(data$steps[data$interval == i], na.rm = TRUE)})
plot(x, y, type = "l", main = "Average number of steps taken versus the 5-minute intervals", xlab = "Interval", ylab = "Average")
```

![plot of chunk plot](figure/plot.png) 

Calculating, which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps:

```r
max.steps <- x[match(max(y), y)]
max.steps
```

```
## [1] "835"
```

## Imputing missing values

Total number of missing values in the dataset (i.e. the total number of rows with NAs):

```r
missing <- sum(!complete.cases(data))
missing
```

```
## [1] 2304
```

Missing values in the dataset are filled in with means for given 5-minute intervals.

```r
fdata <- data
for (i in seq(nrow(data))) {
    if (is.na(data$steps[i])) {
        fdata$steps[i] <- mean(data$steps[data$interval == data$interval[i]], na.rm = TRUE)
    }
}
```

After missing values were imputed:


```r
ftotal <- tapply(fdata$steps,as.factor(data$date),sum)
hist(ftotal, breaks = 20, main = "Histogram of the total number of steps taken each day", xlab = "Number of steps")
```

![plot of chunk fhistogramsteps](figure/fhistogramsteps.png) 

Mean total number of steps taken per day:

```r
steps.mean <- mean(ftotal)
steps.mean
```

```
## [1] 10766
```

Median total number of steps taken per day:

```r
steps.median <- median(ftotal)
steps.median
```

```
## [1] 10766
```

## Are there differences in activity patterns between weekdays and weekends?

Creating a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day:

```r
fdata$weektime <- as.factor(ifelse(as.POSIXlt(fdata$date)$wday %in% c(0,6),"weekend", "weekday"))
```
Making a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis):

```r
par(mfrow=c(2,1))
fx1 <- levels(as.factor(fdata$interval[fdata$weektime=="weekday"]))
fy1 <- sapply(fx1, function(i) {mean(fdata$steps[fdata$weektime=="weekday" & fdata$interval == i])})
fx2 <- levels(as.factor(fdata$interval[fdata$weektime=="weekend"]))
fy2 <- sapply(fx2, function(i) {mean(fdata$steps[fdata$weektime=="weekend" & fdata$interval == i])})
plot(fx1, fy1, type = "l", main = "Weekday", xlab = "Interval", ylab = "Number of steps")
plot(fx2, fy2, type = "l", main = "Weekend", xlab = "Interval", ylab = "Number of steps")
```

![plot of chunk fplot](figure/fplot.png) 

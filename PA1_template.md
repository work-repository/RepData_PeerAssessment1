---
title: "Reproducible Research Week 2 - Course Project 1"
output: 
  html_document:
    keep_md: yes 
---





## Loading and preprocessing the data
1. Load the data (i.e. \color{red}{\verb|read.csv()|}read.csv())

```r
data <- read.csv(raw_data_file, na.strings = "NA", colClasses = c("numeric","character","numeric"))
```

2. Process/transform the data (if necessary) into a format suitable for your analysis

```r
data$date <- as.Date(data$date, "%Y-%m-%d")
```

## Q1. What is mean total number of steps taken per day?
1. Calculate the total number of steps taken per day

```r
data_date_sum <- data %>% filter(!is.na(steps)) %>% group_by(date) %>% summarize(total = sum(steps))
data_mean <- mean(data_date_sum$total)
data_median <- median(data_date_sum$total)
```

2. Make a histogram of the total number of steps taken each day

```r
hist(data_date_sum$total, col = "black", main = "Total number of steps per day", xlab = "Number of steps", ylab = "Frequency (number of days)")
abline(v = data_mean, col = "blue", lty = 2)
abline(v = data_median, col = "red", lty = 2)
legend("right", lty = 2, col = c("blue","red"), legend = c("Mean", "Median"))
```

![](PA1_template_files/figure-html/q1.2-1.png)<!-- -->

3. Calculate and report the mean and median of the total number of steps taken per day
  - Mean = 10766.19
  - Median = 10765.00

## Q2. What is the average daily activity pattern?
1. Make a time series plot (i.e. \color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
data_int_avg <- data %>% filter(!is.na(steps)) %>% group_by(interval) %>% summarize(avg = mean(steps))
with(data_int_avg, {
  plot(interval, avg, type = "l", xlab = "Interval", ylab = "Steps")
})
```

![](PA1_template_files/figure-html/q2.1-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
data_int_avg %>% filter(avg == max(avg)) %>% .$interval
```

```
## [1] 835
```

## Q3. Imputing missing values
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with \color{red}{\verb|NA|}NAs)

```r
data %>% filter(is.na(steps)) %>% count() %>% .$n
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
    - Will use mean for 5-minute interval.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
data_corr <- merge(data, data_int_avg, by = "interval") %>% mutate(steps = ifelse(is.na(steps), avg, steps)) %>% select(-avg)
head(data_corr)
```

```
##   interval    steps       date
## 1        0 1.716981 2012-10-01
## 2        0 0.000000 2012-11-23
## 3        0 0.000000 2012-10-28
## 4        0 0.000000 2012-11-06
## 5        0 0.000000 2012-11-24
## 6        0 0.000000 2012-11-15
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
data_corr_date_sum <- data_corr %>% group_by(date) %>% summarize(total = sum(steps))

data_corr_mean <- mean(data_corr_date_sum$total)
data_corr_median <- median(data_corr_date_sum$total)
hist(data_corr_date_sum$total, col = "black", main = "Total number of steps per day (no missing data)", xlab = "Number of steps", ylab = "Frequency (number of days)")
abline(v = data_corr_mean, col = "blue", lty = 2)
abline(v = data_corr_median, col = "red", lty = 2)
legend("right", lty = 2, col = c("blue","red"), legend = c("Mean", "Median"))
```

![](PA1_template_files/figure-html/q3.4-1.png)<!-- -->

```r
data.frame(
  missing = c("Y", "N"),
  mean = c(data_mean, data_corr_mean),
  median = c(data_median, data_corr_median)
)
```

```
##   missing     mean   median
## 1       Y 10766.19 10765.00
## 2       N 10766.19 10766.19
```
  
  - Mean is still the same and equal to the Median. Median frequency is increased.

## Q4. Are there differences in activity patterns between weekdays and weekends?
1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
day_levels <- c("weekday", "weekend")
data_corr$day <- factor(ifelse(weekdays(data$date, abbreviate = T) %in% c("Sat", "Sun"), day_levels[2], day_levels[1]))
```

2. Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
data_corr_day_avg <- data_corr %>% group_by(day, interval) %>% summarize(avg = mean(steps))

par(mfrow = c(2,1), mar = c(0, 0, 0, 0), oma = c(4, 4, 4, 0.5), cex = 0.6)
with(filter(data_corr_day_avg, day == day_levels[1]), {
  plot(interval, avg, type = "l", col="blue")
})
with(filter(data_corr_day_avg, day == day_levels[2]), {
  plot(interval, avg, type = "l", col="red")
})
mtext("Interval", side = 1, outer = T, cex = 0.7, line = 2.2)
mtext("Number of steps", side = 2, outer = T, cex = 0.7, line = 2.2)
legend(1700, 295, xpd = NA, horiz = T, bty = "n", lty = 1, col = c("blue","red"), legend = day_levels)
```

![](PA1_template_files/figure-html/q4.2-1.png)<!-- -->

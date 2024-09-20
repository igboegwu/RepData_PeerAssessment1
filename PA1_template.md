---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

---


``` r
file <- "activity.zip"
unzip(file,exdir = "Activity_monitoring_data_unzip")
Activity_monitoring_data <- read.csv("Activity_monitoring_data_unzip//activity.csv")
Activity_monitoring_data$date <- as.Date(Activity_monitoring_data$date)
```
   


## What is mean total number of steps taken per day?

---


``` r
library(ggplot2)
library(dplyr)
total_steps_perday <- Activity_monitoring_data %>% group_by(date) %>%
                        summarise(total_steps=sum(steps,na.rm = T))
ggplot(data=total_steps_perday,aes(x=total_steps)) + 
        geom_histogram(binwidth = 1000,col="blue",fill="black")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png)

``` r
mean_total_steps_perday <- mean(total_steps_perday$total_steps)
mean_total_steps_perday
```

```
## [1] 9354.23
```

``` r
median_total_steps_perday <- median(total_steps_perday$total_steps)
median_total_steps_perday
```

```
## [1] 10395
```
   


## What is the average daily activity pattern?

---


``` r
average_steps_perinterval <- Activity_monitoring_data %>% 
        group_by(interval) %>% summarise(average_steps = mean(steps,na.rm = T))
with(data = average_steps_perinterval,plot(x=interval,y=average_steps,type="l"))
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

``` r
max_point <- average_steps_perinterval %>% 
        filter(average_steps==max(average_steps)) %>% select(interval)
max_point[[1]]
```

```
## [1] 835
```
   
   
   

## Imputing missing values

---


``` r
library(data.table)
tatal_NAs <- sum(!complete.cases(Activity_monitoring_data))
tatal_NAs
```

```
## [1] 2304
```

``` r
Filling_missing_values_using_the_average_for_that_interval <- function(data) {
        mean_steps_perinterval <- data %>%
                group_by(interval) %>%
                summarise(mean_steps = mean(steps, na.rm = TRUE))
        data_dt <- as.data.table(data)
        mean_steps_dt <- as.data.table(mean_steps_perinterval)
        for (i in seq_len(nrow(mean_steps_dt))) {
                interval_value <- mean_steps_dt$interval[i]
                mean_value <- mean_steps_dt$mean_steps[i]
                data_dt[interval == interval_value & is.na(steps),
                        steps := mean_value]
        }
        return(data_dt)
}

filled_data <- Filling_missing_values_using_the_average_for_that_interval(Activity_monitoring_data)


new_total_steps_perday <- filled_data %>% group_by(date) %>%
        summarise(total_steps=sum(steps,na.rm = T))

ggplot(data=new_total_steps_perday,aes(x=total_steps)) + 
        geom_histogram(binwidth = 1000,col="blue",fill="black")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

``` r
mean_new_total_steps_perday <- mean(new_total_steps_perday$total_steps)
mean_new_total_steps_perday
```

```
## [1] 10749.77
```

``` r
median_new_total_steps_perday <- median(new_total_steps_perday$total_steps)
median_new_total_steps_perday
```

```
## [1] 10641
```




## Are there differences in activity patterns between weekdays and weekends?

---


``` r
library(lattice)
Partitioned_data <- copy(filled_data)
Partitioned_data[,typedays:=ifelse(weekdays(date)%in%c("Saturday","Sunday"),
                      "weekend","weekday")]

new_average_steps_perinterval <- Partitioned_data %>% 
        group_by(typedays, interval) %>% 
        summarise(average_steps = mean(steps,na.rm = T))
```

```
## `summarise()` has grouped output by 'typedays'. You can override using the `.groups` argument.
```

``` r
xyplot(average_steps ~ interval | typedays, data = new_average_steps_perinterval, 
       type = "l",
       layout = c(1, length(unique(new_average_steps_perinterval$typedays))))
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)


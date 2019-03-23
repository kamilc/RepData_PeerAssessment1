---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

## Loading and preprocessing the data

Let's first include `dplyr` and `ggplot2` (among others):


```r
library(tidyverse)
```


```r
# let's extract the dataset first:
unzip("activity.zip")

dataset <- read_csv("activity.csv")
```

## What is mean total number of steps taken per day?

Let's first subset the data to only work with complete cases:


```r
completes <- dataset[complete.cases(dataset), ]
```

We're interested in the total number of steps taken on each day:


```r
completes_with_totals <-
  completes %>%
  group_by(date) %>%
  summarize(total=sum(steps))

mean_total_steps <- completes_with_totals$total %>% mean
median_total_steps <- completes_with_totals$total %>% median

print(paste("Mean total steps:", mean_total_steps))
```

```
## [1] "Mean total steps: 10766.1886792453"
```

```r
print(paste("Median total steps:", median_total_steps))
```

```
## [1] "Median total steps: 10765"
```

Now, let's plot a histogram. It's a bar plot in its essense, counting up the total number of occurances per some dimension. This is exactly what we have computed in the last step.


```r
completes_with_totals %>%
  ggplot(aes(date, total)) +
  geom_bar(stat="identity", width=0.75) +
  geom_hline(yintercept=mean_total_steps, colour="red") +
  annotate(
    "text",
    x=min(completes_with_totals$date) + 8,
    y=mean_total_steps - 800,
    label=paste("mean = ", mean_total_steps),
    colour="red"
  ) +
  geom_hline(yintercept=median_total_steps, colour="orange") +
  annotate(
    "text",
    x=max(completes_with_totals$date) - 5,
    y=median_total_steps - 800,
    label=paste("median = ", median_total_steps),
    colour="orange"
  )
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

Let's double-check the above plot's results by looking at the mean of the total number of steps per day:
?

```r
(completes %>% group_by(date) %>% summarize(observations=n()))$observations %>% mean()
```

```
## [1] 288
```

Let's do the same to calculate the median:


```r
(completes %>% group_by(date) %>% summarize(observations=n()))$observations %>% median()
```

```
## [1] 288
```

## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?

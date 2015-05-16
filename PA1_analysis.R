library(dplyr)

# 1
activities <- read.csv('./activity.csv')
activities$date <- as.Date(activities$date, format = "%Y-%m-%d")

# 2
steps_day <- group_by(activities, date)
total_steps <- summarize(steps_day, total_steps = sum(steps))

# hist(total_steps$total_steps, main = "Total Number of Steps Taken Each Day", xlab = "Total Steps")

mean(total_steps$total_steps, na.rm = T)
median(total_steps$total_steps, na.rm = T)

#3
steps_interval <- group_by(activities, interval)
avg_interval_steps <- summarize(steps_interval, avg_steps = mean(steps, na.rm = T))
# plot(
#     x = avg_interval_steps$interval,
#     y = avg_interval_steps$avg_steps,
#     type = "l",
#     main = "Avg Number of Steps by 5-minute Interval",
#     ylab = "Average Number of Steps",
#     xlab = "Interval"
# )

avg_interval_steps[which.max(avg_interval_steps$avg_steps),]

#4
sum(is.na(activities$steps))

activities.imputed <- cbind(activities, avg_interval_steps$avg_steps)
names(activities.imputed) <- c("steps", "date", "interval", "avg_steps")
activities.imputed <- transmute(
    activities.imputed,
    steps = ifelse(is.na(steps),avg_steps, steps),
    date = date,
    interval = interval
)

imputed_steps_day <- group_by(activities.imputed, date)
imputed_total_steps <- summarize(imputed_steps_day, total_steps = sum(steps))
hist(imputed_total_steps$total_steps, main = "Imputed Total Number of Steps Taken Each Day", xlab = "Total Steps")
mean(imputed_total_steps$total_steps)
median(imputed_total_steps$total_steps)

#5

activities.imputed$day_type <- weekdays(activities.imputed$date)
activities.imputed <- transmute(
    activities.imputed,
    steps = steps,
    date = date,
    interval = interval,
    day_type = ifelse(day_type == "Saturday" | day_type == "Sunday", "weekend", "weekday")
)
activities.imputed$day_type <- as.factor(activities.imputed$day_type)

weekdays_steps_interval <- group_by(activities.imputed, interval, day_type)
weekdays_avg_interval_steps <- summarize(weekdays_steps_interval, avg_steps = mean(steps))
library(ggplot2)
qplot(interval, avg_steps, data = weekdays_avg_interval_steps, facets = day_type ~ ., binwidth = 2)








# libraries ---------------------------------------------------------------

       library(tidyverse)
       library(magrittr)



# load data --------------------------------------------------------------------


       unzip("Reproducible Research/RepData_PeerAssessment1/activity.zip")

       df_activity <- read.csv("Reproducible Research/RepData_PeerAssessment1/activity.csv") %>% as_tibble()



# process data ------------------------------------------------------------


       df_activity %<>%  mutate(date = as.Date(date, format = "%Y-%m-%d"))
       
       

# total steps taken per day -----------------------------------------------

       #calculate total steps by day
       df_steps <- df_activity %>% 
              group_by(date) %>% 
              summarise(total_steps = sum(steps, na.rm = FALSE))
       
       #histogram
       df_steps %>% ggplot(aes(total_steps)) +
              geom_histogram(bins = 10) +
              ggtitle("Total steps taken each day") +
              xlab("Number of steps") +
              ylab("Frequency")
       
       #mean and median
       summary(df_steps$total_steps)

       

# average daily activity pattern ------------------------------------------


       #average steps
       df_daily <- df_activity %>%
              group_by(interval) %>% 
              summarise(average_steps = mean(steps, na.rm = TRUE))
       
       #time series plot
       df_daily %>% ggplot(aes(x = interval, y = average_steps)) +
              geom_line() +
              ggtitle("Average daily activity pattern") +
              xlab("Interval") +
              ylab("Average number of steps")
       
       #calculate max interval
       df_daily[which.max(df_daily$average_steps),]
       
       
       

# imputing missing values -------------------------------------------------


       #calculate number of missing values
       sum(is.na(df_activity$steps))
       
       #fill in missing values with mean for that 5 min interval
       df_impute <- df_activity %>% 
              left_join(df_daily, by = "interval") %>% 
              mutate(steps = as.numeric(steps))
       
       df_impute[is.na(df_impute$steps), "steps"] <- df_impute[is.na(df_impute$steps), "average_steps"]
       
       #histogram of total steps taken each day
       df_impute_steps <- df_impute %>%
              group_by(date) %>% 
              summarise(total_steps = sum(steps))
       
       #histogram
       df_impute_steps %>% ggplot(aes(total_steps)) +
              geom_histogram(bins = 10) +
              ggtitle("Total steps taken each day") +
              xlab("Number of steps") +
              ylab("Frequency")
       
       #mean and median
       summary(df_impute_steps$total_steps) #mean and median are the same!
       
       

# weekends vs weekdays ----------------------------------------------------


       #identify weekend vs weekday
       df_day <- df_impute %>% 
              mutate(day = weekdays(df_impute$date)) %>% 
              mutate(day_cat = ifelse(day == c("Saturday", "Sunday"), "weekend", "weekday"))
       
       #time series plot
       df_day_steps <- df_day %>% 
              group_by(interval, day_cat) %>% 
              summarise(average_steps = mean(steps))
       
       df_day_steps %>% ggplot(aes(x = interval, y = average_steps)) +
              geom_line() +
              facet_grid(rows = vars(day_cat)) +
              ggtitle("Average daily activity pattern") +
              xlab("Interval") +
              ylab("Average number of steps")
       



       
       
       
       
       
       
       
       
       
       
       
       
       
       









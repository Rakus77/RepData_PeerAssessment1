library(ggplot2)
library(dplyr)

filedest <- getwd()
unzip("activity.zip", exdir = filedest)
step_data <- read.csv("activity.csv", header = TRUE, colClasses = c("numeric", 
			"Date", "numeric"))

#       Add column for the day of the week (DOW).
step_data$DOW <- weekdays(step_data$date)         
                
#...    What is mean total number of steps taken per day?

#       Create data set of complete cases.
step_data_complete <- step_data[complete.cases(step_data),]

#       Create data frame of rows with NAs.
step_data_na <- step_data[!complete.cases(step_data),]

#       Sum steps by day.
steps_by_day <- with(step_data_complete,(aggregate(steps~date, FUN = sum, na.rm = TRUE )))


#       Plot histogram of frequency of daily steps in buckets 1000 steps wide.
histogram <- ggplot(data = steps_by_day)+
		geom_histogram(aes(x = steps), binwidth = 1000, color = "black",
			       fill = "gold")+
		ylab("Frequency")+
		ylim(0,15)+
		xlab("Daily Steps Taken")+
		ggtitle("Frequency of Steps Taken in a Day")+
		theme(plot.title = element_text(hjust = 0.5))
print(histogram)

#       Calculate the mean and median of the daily steps.
mean(steps_by_day$steps)
median(steps_by_day$steps)


#       Find mean steps by interval.
mean_steps_by_int <- with(step_data,(aggregate(steps~interval, FUN = mean, na.rm = TRUE)))


#       Plot a time series of the mean steps by interval across 
#       all days that have records.
tseries <- ggplot(data = mean_steps_by_int, aes(interval, steps))+
		geom_line(col = "red")+
		ggtitle("Average Steps in Five Minute Intervals")+
		ylab("Average Steps")+
		xlab("Interval")+
		theme(plot.title = element_text(hjust = 0.5))
print(tseries)

#       Find the interval with the maximum average steps across
#       all days that have records.
mean_steps_by_int[which.max(mean_steps_by_int$steps), ]


#       Calculate the total number of missing values in the original data set.
colSums(is.na(step_data))[1:3]


#       Calculate the mean steps by interval and weekday (DOW) of complete cases.
mean_steps_by_int <- with(step_data_complete,(aggregate(steps~interval + DOW, FUN = mean, na.rm = TRUE)))

#       Merge replacement value column for NA values with mean steps for 
#       corresponding interval and day of week. Remove NA column. Rename 
#       replacement column. Reorder columns to match original data set.
step_data_imputed <- merge(step_data_na, mean_steps_by_int, by = c("interval", "DOW"))

step_data_imputed <- step_data_imputed[,-3]

names(step_data_imputed)[4] <- "steps"

step_data_imputed <- step_data_imputed[,c("steps","date","interval","DOW")]

#       Merge rows of complete cases and imputed cases.
step_data_backfill <- rbind(step_data_complete, step_data_imputed)

steps_by_day_backfill <- with(step_data_backfill,(aggregate(steps~date, FUN = sum, na.rm = TRUE )))


#       Plot histogram of frequency of daily steps in buckets 1000 steps wide
#       with imputed values included. 
histogram <- ggplot(data = steps_by_day_backfill)+
        geom_histogram(aes(x = steps), binwidth = 1000, color = "black",
                       fill = "gold")+
        ylab("Frequency")+
        ylim(0,15)+
        xlab("Daily Steps Taken")+
        ggtitle("Freq. of Steps Taken in a Day (Imputation)")+
        theme(plot.title = element_text(hjust = 0.5))
print(histogram)


#	Calculate new mean and median with imputed data included.
mean(steps_by_day_backfill$steps)
median(steps_by_day_backfill$steps)

#	Create weekday and weekend vectors.
wday <- c("Monday", "Tuesday", "Wednesday","Thursday" , "Friday")
wend <- c( "Saturday" , "Sunday")

#	Create new column in data set.
step_data_backfill2 <-  step_data_backfill %>% 
			mutate(DOW.Type = ifelse(DOW %in% wday, "weekday", "weekend"))
	
#	Coerce new column to factor instead of character.	
step_data_backfill2$DOW.Type <- as.factor(step_data_backfill2$DOW.Type)

#	Calculate the mean steps by interval and type of day (DOW.Type).
steps_by_day_backfill2 <- with(step_data_backfill2,(aggregate(steps~interval+DOW.Type, FUN = mean, na.rm = TRUE )))
	

#	Plot the mean steps across all intervals broken out by type of day (DOW.Type).
tseries.factor <- ggplot(data = steps_by_day_backfill2)+
			geom_line(aes(x = interval, y = steps, 
				      color = factor(DOW.Type)))+
			facet_grid(DOW.Type~.)+
			scale_color_manual(values = c("weekday" = "magenta2", "weekend" = "royalblue"))+
			xlab("Interval")+
			ylab("Average Steps")+
			labs(   title = "Average Steps by Interval",
				subtitle = "Comparing the weekend to weekdays")+
			theme(legend.title = element_blank(), 
				legend.position = "bottom", 
				plot.title = element_text(hjust = 0.5),
				plot.subtitle = element_text(hjust = 0.5))

print(tseries.factor)










				
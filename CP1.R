library(ggplot2)


#url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
filedest <- getwd()
#download.file(url, destfile = paste(filedest,"/instructions_fig"))
unzip("activity.zip", exdir = filedest)
step_data <- read.csv("activity.csv", header = TRUE, colClasses = c("numeric", 
			"Date", "numeric"))
step_data$date <- as.Date(step_data$date, format = "%Y%m%d")


steps_by_day <- with(step_data,(aggregate(steps~date, FUN = sum, na.rm = TRUE,
			na.action = na.pass )))			


histogram <- ggplot(data = steps_by_day)+
		geom_histogram(aes(x = steps), binwidth = 1000, color = "black",
			       fill = "gold")+
		ylab("Frequency")+
		ylim(0,10)+
		xlab("Daily Steps Taken")+
		ggtitle("Frequency of Steps Taken in a Day")+
		theme(plot.title = element_text(hjust = 0.5))
print(histogram)

mean(steps_by_day$steps)
median(steps_by_day$steps)



steps_by_int <- with(step_data,(aggregate(steps~interval, FUN = mean, na.rm = TRUE,
					  na.action = na.pass )))	


tseries <- ggplot(data = steps_by_int, aes(interval, steps))+
		geom_line(col = "red")+
		ggtitle("Average Steps in Five Minute Intervals")+
		ylab("Average Steps")+
		xlab("Interval")+
		theme(plot.title = element_text(hjust = 0.5))
print(tseries)

steps_by_int[which.max(steps_by_int$steps), ]





















				
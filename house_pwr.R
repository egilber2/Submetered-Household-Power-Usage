
# Description --------------------------------------------------------------------

# Rutgers Data Analytics / Big Data 2017
# Project 4 Deep Analytics and Visualization


# Summary -----------------------------------------------------------------





# Load Packages -----------------------------------------------------------
install.packages('caret')
install.packages('tidyverse')
install.packages('magrittr')
install.packages('lubridate')
install.packages('GGally')
install.packages('scales')
install.packages('forecast')
install.packages('xts')
install.packages('timeSeries')
install.packages('Hmisc')
install.packages('stargazer')
install.packages('grid')


library(caret)
library(tidyverse)
library(magrittr)
library(lubridate)
library(VIM)
library(Hmisc)
library(GGally)
library(scales)
library(forecast)
library(stargazer)

# Load Data ---------------------------------------------------------------


house_pwr <- read_delim('household_power_consumption.txt', col_names = TRUE, col_types = cols(Global_active_power='d', Global_reactive_power='d',
Voltage='d', Global_intensity='d', Sub_metering_1='d', Sub_metering_2='d', Sub_metering_3='d'), delim=';',  na='?')

# Convert to a tibble
as_tibble(house_pwr)


# Preprocessing -----------------------------------------------------------

# Combine Date and Time Features
house_pwr <- unite(house_pwr,Date, Time, col='DateTime', sep=' ')

house_pwr$DateTime <- as.POSIXct(house_pwr$DateTime, format="%d/%m/%Y %H:%M:%S", tz='America/New_York')
class(house_pwr$DateTime)
tz(house_pwr$DateTime)


# Rename some columns
colnames(house_pwr)[2] <- 'Glbl_actvPwr'
colnames(house_pwr)[3] <- 'Glbl_ractvPwr'
colnames(house_pwr)[6] <- 'Sub-Meter-1'
colnames(house_pwr)[7] <- 'Sub-Meter-2'
colnames(house_pwr)[8] <- 'Sub-Meter-3'

MonthLst <- c('Jan', 'Feb', 'Mar','Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep',
              'Oct', 'Nov','Dec', 'Jan')

WkdayLst <- c('Mon', 'Tues', 'Wed', 'Thurs', 'Fri')

WkLst <- c('Sun','Mon', 'Tues', 'Wed', 'Thurs', 'Fri', 'Sat', 'Sun' )

WkndList <- c('Sat', 'Sun', 'Mon')

# Assess missing values
aggr_plot <- aggr(house_pwr, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(house_pwr),cex.axis=.7,
  gap=3, ylab=c("Histogram of missing data","Pattern"), digits=2)

# Remove rows with NA's
house_pwr <- na.omit(house_pwr)
sum(is.na(house_pwr))


# Add feature representing remaining active energy consumed every minute (watt hour)
house_pwr <- house_pwr %>%
  mutate(Engy_remain=(Glbl_actvPwr*1000/60)-
  `Sub-Meter-1` - `Sub-Meter-2` - `Sub-Meter-3`)

as_tibble(house_pwr)
str(house_pwr)

# Create tidy tibble
house_pwr_tidy <- house_pwr %>%
  gather(Meter, Watt_hr, `Sub-Meter-1`, `Sub-Meter-2`, `Sub-Meter-3`)

house_pwr_tidy %>% as_tibble(house_pwr_tidy)
is_tibble(house_pwr_tidy)

house_pwr_tidy$Meter <- factor(house_pwr_tidy$Meter)
glimpse(house_pwr_tidy)


# Exploratory Data Analysis -----------------------------------------------

# Proportional and Line Plots across sub-metered zones-------------------------

###- Year-Proportional Plot of total energy across sub-metered zones

##-Year_Proportional Plot
house_pwr_tidy %>%
  filter(year(DateTime)>2006) %>%
  group_by(year(DateTime), Meter) %>%
  summarise(avg=mean(Watt_hr)) %>%
  ggplot(aes(x=factor(`year(DateTime)`), avg, group=Meter,fill=Meter)) +
  labs(x='Year', y='Proportion of Energy Useage') +
  ggtitle('Proportion of Average Yearly Energy Consumption') +
  geom_bar(stat='identity', position='fill', color='black')

##-Year_Line Plot
house_pwr_tidy %>%
  filter(year(DateTime)>2006) %>%
  group_by(year(DateTime), Meter) %>%
  summarise(sum=sum(Watt_hr/1000)) %>%
  ggplot(aes(x=factor(`year(DateTime)`), sum, group=Meter, colour=Meter)) +
  labs(x='Year', y='kWh') +
  ggtitle('Total Yearly Energy Consumption') +
  geom_line(size=1)

###-Month- Proportional Plot
house_pwr_tidy %>%
  filter(year(DateTime)>2006) %>%
  mutate(Month=lubridate::month(DateTime, label=TRUE, abbr=TRUE)) %>%
  group_by(Month, Meter) %>%
  summarise(avg=mean(Watt_hr)) %>%
  ggplot(aes(x=factor(Month), avg, group=Meter,fill=Meter)) +
  labs(x='Month of the Year', y='Proportion of Monthly Energy Useage') +
  ggtitle('Proportion of Average Monthly Energy Useage') +
  geom_bar(stat='identity', position='fill', color='black')

###-Month- Line Plot
house_pwr_tidy %>%
  filter(year(DateTime)>2006) %>%
  mutate(Month=lubridate::month(DateTime, label=TRUE, abbr=TRUE)) %>%
  group_by(Month, Meter) %>%
  summarise(avg=mean(Watt_hr)) %>%
  ggplot(aes(x=factor(Month), avg, group=Meter, colour=Meter)) +
  labs(x='Month of the Year', y='Wh') +
  ggtitle('Average Monthly Energy Useage') +
  geom_line(size=1) +
  geom_line()

### Day of the Month- Line Plot
#Not informative
house_pwr_tidy %>%
  group_by(day(DateTime), Meter) %>%
  summarise(avg=mean(Watt_hr)) %>%
  ggplot(aes(x=factor(`day(DateTime)`), avg, group=Meter,colour=Meter)) +
  labs(x='Day of the Month', y='Avg Watt Hour Useage') +
  ggtitle('Average Daily Watt Hour Useage') +
  geom_line(size=1) +
  geom_line()


###-Day of Week- Porportional Plot
house_pwr_tidy %>%
  filter(year(DateTime)>2006) %>%
  mutate(Day=lubridate::wday(DateTime, label=TRUE, abbr=TRUE)) %>%
  group_by(Day, Meter) %>%
  summarise(avg=mean(Watt_hr)) %>%
  ggplot(aes(x=factor(Day), avg, group=Meter,fill=Meter)) +
  labs(x='Day of the Week', y='Proportion of Energy Useage') +
  ggtitle('Proportion of Average Daily Energy Consumption') +
  geom_bar(stat='identity', position='fill', color='black')

###-Day of Week- Line Plot
house_pwr_tidy %>%
  filter(year(DateTime)>2006) %>%
  mutate(Day=lubridate::wday(DateTime, label=TRUE, abbr=TRUE)) %>%
  group_by(Day, Meter) %>%
  summarise(avg=mean(Watt_hr)) %>%
  ggplot(aes(x=factor(Day), avg, group=Meter, colour=Meter)) +
  labs(x='Day of the Week', y='Wh') +
  ggtitle('Average Daily Energy Consumption') +
  geom_line(size=1) +
  geom_line()

###- Weekend- Proportion Plot
wday <- wday(house_pwr_tidy$DateTime, start = getOption("lubridate.week.start", 7))


house_pwr_tidy %>%
  filter(year(DateTime)>2006) %>%
  mutate(weekend=lubridate::wday(DateTime, label=TRUE, abbr=FALSE)) %>%
  filter(weekend==c('Saturday','Sunday')) %>%
  group_by(weekend, Meter) %>%
  summarise(avg=mean(Watt_hr)) %>%
  ggplot(aes(x=factor(weekend), avg, group=Meter,fill=Meter)) +
  labs(x='Weekend Day', y='Proportion of Energy Useage') +
  ggtitle('Proportion of Average Energy Consumption by Weekend Day') +
  geom_bar(stat='identity', position='fill', color='black')



###-Hour of the Day- Proportional Plot
house_pwr_tidy %>%
  filter(year(DateTime)>2006) %>%
  group_by(hour(DateTime), Meter) %>%
  summarise(avg=mean(Watt_hr)) %>%
  ggplot(aes(x=factor(`hour(DateTime)`), avg, group=Meter,fill=Meter)) +
  labs(x='Hour of the Day', y='Proportion of Energy Useage') +
  ggtitle('Proportion of Average Hourly Energy Consumption') +
  geom_bar(stat='identity', position='fill', color='black')

###-Hour of the Day-Line Plot
house_pwr_tidy %>%
  filter(year(DateTime)>2006) %>%
  group_by(hour(DateTime), Meter) %>%
  summarise(avg=mean(Watt_hr)) %>%
  ggplot(aes(x=factor(`hour(DateTime)`), avg, group=Meter,colour=Meter)) +
  labs(x='Hour of the Day', y='Wh') +
  ggtitle('Average Hourly Energy Consumption') +
  geom_line(size=1) +
  geom_line()



# Correlation plot
ggcorr(house_pwr) +
  ggtitle('Correlation Plot of Energy Consumption Dataset')

ggpairs(house_pwr,
        columns = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
        upper = list(continuous = wrap("cor", size = 10, alpha=0.5)),
        lower = list(continuous = "smooth"))

# Subset ------------------------------------------------------------------
housePWR_yr <- house_pwr %>%
  filter(year(DateTime) > 2006) %>%
  #filter(year(DateTime)<2010) %>%
  group_by(year(DateTime), month(DateTime)) %>%
  summarise(Sub_Meter_1=round(sum(`Sub-Meter-1`/1000), 3),
            Sub_Meter_2=round(sum(`Sub-Meter-2`/1000), 3),
            Sub_Meter_3=round(sum(`Sub-Meter-3`/1000),3),
            first_DateTime = first(DateTime))


# Subset Month
housePWR_mnth <- house_pwr %>%
  filter(year(DateTime) > 2006) %>%
  filter(year(DateTime)<2011) %>%
  group_by(month(DateTime), mday(DateTime)) %>%
  summarise(Sub_Meter_1=round(sum(`Sub-Meter-1`/1000), 3),
            Sub_Meter_2=round(sum(`Sub-Meter-2`/1000), 3),
            Sub_Meter_3=round(sum(`Sub-Meter-3`/1000),3),
            first_DateTime = first(DateTime))


# Subset Day of Week
housePWR_dofWk <- house_pwr %>%
  filter(year(DateTime)>2006) %>%
  #filter(year(DateTime)<2010) %>%
  mutate(DofWk=lubridate::wday(DateTime, label=TRUE, abbr=TRUE)) %>%
  group_by(wday(DateTime), hour(DateTime)) %>%
  summarise(Sub_Meter_1=round(sum(`Sub-Meter-1`/1000),3),
            Sub_Meter_2=round(sum(`Sub-Meter-2`/1000),3),
            Sub_Meter_3=round(sum(`Sub-Meter-3`/1000),3),
            first_DateTime = first(DateTime))

# hour of day
housePWR_hrWk <- house_pwr %>%
  filter(year(DateTime)>2006) %>%
  #filter(year(DateTime)<2010) %>%
  mutate(DofWk=lubridate::wday(DateTime, label=TRUE, abbr=TRUE)) %>%
  group_by(DofWk, hour(DateTime)) %>%
  summarise(Sub_Meter_1=round(sum(`Sub-Meter-1`/1000),3),
            Sub_Meter_2=round(sum(`Sub-Meter-2`/1000),3),
            Sub_Meter_3=round(sum(`Sub-Meter-3`/1000),3),
            first_DateTime = first(DateTime))

##-Subset by Weekday
housePWR_wkday <- house_pwr %>%
  filter(year(DateTime)>2006) %>%
  mutate(Wkday=lubridate::wday(DateTime, label=TRUE, abbr=TRUE)) %>%
  filter(Wkday == c('Mon', 'Tues', 'Wed', 'Thurs', 'Fri')) %>%
  group_by(Wkday, hour(DateTime)) %>%
  summarise(Sub_Meter_1=round(sum(`Sub-Meter-1`/1000),3),
            Sub_Meter_2=round(sum(`Sub-Meter-2`/1000),3),
            Sub_Meter_3=round(sum(`Sub-Meter-3`/1000),3),
            first_DateTime = first(DateTime))


# Subset hour of day
housePWR_hofDay <- house_pwr %>%
  filter(year(DateTime)>2006) %>%
  filter((minute(DateTime) %% 5) == 0) %>%
  group_by(hour(DateTime), minute(DateTime)) %>%
  summarise(Sub_Meter_1=round(sum(`Sub-Meter-1`/1000),3),
            Sub_Meter_2=round(sum(`Sub-Meter-2`/1000),3),
            Sub_Meter_3=round(sum(`Sub-Meter-3`/1000),3),
            first_DateTime = first(DateTime))


# Subset by Weekends
housePWR_wknd <- house_pwr %>%
  filter(year(DateTime)>2006) %>%
  #filter(year(DateTime)<2010) %>%
  mutate(Wknd=lubridate::wday(DateTime, label=TRUE, abbr=TRUE)) %>%
  filter(Wknd == c('Sat', 'Sun')) %>%
  group_by(Wknd, hour(DateTime)) %>%
  summarise(Sub_Meter_1=round(sum(`Sub-Meter-1`/1000),3),
            Sub_Meter_2=round(sum(`Sub-Meter-2`/1000),3),
            Sub_Meter_3=round(sum(`Sub-Meter-3`/1000),3),
            first_DateTime = first(DateTime)) %>%
 arrange(desc(Wknd))

# Convert to Time Series --------------------------------------------------
# Year
housePWR_yrTS <- ts(housePWR_yr[,3:5], frequency = 12, start=c(2007,1), end=c(2010,11))
plot(housePWR_yrTS, plot.type='s', #xaxt='n',
     xaxp = c(2007, 2010, 3),
     col=c('red', 'green', 'blue'),
     main='Total Yearly kWh Consumption (2007-2010)',
     xlab='Year', ylab = 'Total kWh')
minor.tick(nx=12)
b <- c('Sub-meter-1', 'Sub-meter-2', 'Sub-meter-3')
legend('topleft', b, col=c('red', 'green', 'blue'), lwd=2, bty='n')


#Month
housePWR_mnthTS <- ts(housePWR_mnth[,3:5], frequency = 30, start=c(1,1), end=c(12,31))
plot(housePWR_mnthTS, plot.type='s', xaxt='n',
     xaxp = c(1,13,12),
     col=c('red', 'green', 'blue'),
     main='Total Monthly kWh Consumption (2007-2010)',
     xlab='Month', ylab = 'Total kWh',
     ylim=c(0,75))
axis(side=1, at= c(1, 2,3,4,5,6,7, 8,9, 10,11,12,13), labels=MonthLst)
b <- c('Sub-meter-1', 'Sub-meter-2', 'Sub-meter-3')
legend('topleft', b, col=c('red', 'green', 'blue'), lwd=2, bty='n')



# Day of Week
housePWR_dofWkTS <- ts(housePWR_dofWk[,3:5], frequency =24, start=c(1,0), end=c(7,23))
plot(housePWR_dofWkTS, plot.type='s', xaxt='n',
     xaxp = c(1, 7, 6),
     col=c('red', 'green', 'blue'),
     xlab='Day of Week', ylab = 'Total kWh',
     ylim=c(0,200),
     main='Total kWh Consumption by Day of Week')
axis(side=1, at= c(1, 2,3,4,5,6,7,8), labels=WkLst)
#minor.tick(nx=14)
b <- c('Sub-meter-1', 'Sub-meter-2', 'Sub-meter-3')
legend('topleft', b, col=c('red', 'green', 'blue'), lwd=2, bty='n')


# Hour_Day
housePWR_hrWkTS <- ts(housePWR_hrWk[,3:5], frequency=24, start=c(1,0), end = c(7,23))
plot(housePWR_hrWkTS, plot.type='s', xaxt='n',
     xaxp = c(1, 8, 7),
     col=c('red', 'green', 'blue'),
     ylim=c(0,210),
     xlab='Day of Week', ylab = 'Total kWh',
     main='Total Hourly kWh Consumption by Day of the Week')
axis(side=1, at= c(1, 2,3,4,5,6,7, 8), labels=WkLst)
minor.tick(nx=24)
b <- c('Sub-meter-1', 'Sub-meter-2', 'Sub-meter-3')
legend('topleft', b, col=c('red', 'green', 'blue'), lwd=2, bty='n')

ggseasonplot(housePWR_dofWkTS[,3])

##-Weekday
housePWR_wkdayTS <- ts(housePWR_wkday[,3:5], frequency=24)
plot(housePWR_wkdayTS, plot.type='s', #xaxt='n',
     #xaxp = c(1, 5, 4),
     col=c('red', 'green', 'blue'),
     xlab='Weekday', ylab = 'Total kWh',
     main='Total kWh Consumption by Day of the Week (2007-2010)')
minor.tick(nx=24)
axis(side=1, at= c(1,2,3,4,5), labels=WkdayLst)
b <- c('Sub-meter-1', 'Sub-meter-2', 'Sub-meter-3')
legend('topleft', b, col=c('red', 'green', 'blue'), lwd=2, bty='n')

# Hour of Day / 5_Minute
housePWR_hofDayTS <- ts(housePWR_hofDay[,3:5], frequency=12)
plot(housePWR_hofDayTS, plot.type='s',
     #xaxp = c(0,48, 47),
     col=c('red', 'green', 'blue'),
     xlab='Hour of the Day', ylab = 'Total kWh',
     main='Total kWh Consumption by Hour of the Day')
b <- c('Sub-meter-1', 'Sub-meter-2', 'Sub-meter-3')
legend('topleft', b, col=c('red', 'green', 'blue'), lwd=2, bty='n')

##-Weekday Hourly Useage


# Hour_Weekend
housePWR_wkndTS <- ts(housePWR_wknd[,3:5], frequency=24, end(7,23))
plot(housePWR_wkndTS, plot.type='s', xaxt='n',
     xaxp = c(0, 3,2),
     xlim=c(1,3),
     col=c('red', 'green', 'blue'),
     xlab='Weekend Day', ylab = 'Total kWh',
     ylim=c(0,90),
     main='Total Weekend Energy Consumption by Hour')
axis(side = 1, at = c(1,2,3), labels = WkndList)
minor.tick(nx=24)
b <- c('Sub-meter-1', 'Sub-meter-2', 'Sub-meter-3')
legend('topleft', b, col=c('red', 'green', 'blue'), lwd=2, bty='n')


# Forecasting Trend-------------------------------------------------------------

# Year
fit1 <- tslm(housePWR_yrTS ~ trend)
x <- forecast(fit1, h=6, level = c(90, 95), robust=TRUE)
autoplot(x, PI=TRUE, colour=TRUE) +
  xlab('Year') +
  ylab('Total kWh') +
  ggtitle('Forecasted Trend of Yearly Energy Consumption')
summary(fit1)
x

# Month
fit2 <- tslm(housePWR_mnthTS ~ trend)
y <- forecast(fit2,h=20, level=c(85,95))
autoplot(y, PI=TRUE, colour=TRUE) +
  xlab('Month') +
  ylab('Total kWh') +
  ggtitle('Forecasted Monthly Trend of Energy Consumption')
summary(fit2)
y
#level =	Confidence level for prediction intervals.

#day of Week
fit3 <- tslm(housePWR_dofWkTS ~ trend)
z <- forecast(fit3, level=c(90,95))
autoplot(z, PI=TRUE, colour=TRUE) +
  xlab('Day of the Week') +
  ylab('Total kWh') +
  ggtitle('Forecasted Trend of Energy Consumption in a Week')
summary(fit3)
z

# Hour of Day / Minute
fit4 <- tslm(housePWR_hofDayTS ~ trend)
w <- forecast(fit4, h=24)
autoplot(w, PI=TRUE, colour=TRUE) +
  xlab('Hour of Day') +
  ylab('Total kWh') +
  ggtitle('Forecasted Trend of Hourly Energy Consumption in a Day')
summary(w)
w

# Weekend hours
fit5 <- tslm(housePWR_wkndTS ~ trend)
zz <- forecast(fit5, level=c(90,95))
autoplot(zz, PI=TRUE, colour=TRUE) +
  xlab('Weekend Day') +
  ylab('Total kWh') +
  ggtitle('Forecasted Trend of Weekend Energy Consumption')
summary(w)

##-Weekdays
fit6 <- tslm(housePWR_wkdayTS ~ trend)
xx <- forecast(fit6, h=10)
autoplot(xx, PI=TRUE, colour=TRUE) +
  xlab('Weekday') +
  ylab('Total kWh') +
  ggtitle('Forecast Energy Consumption')
summary(xx)


# Decompose Time Series / Remove Seasonality' -------------------------------------------------------------


##############
# Year       #
##############

##-Sub-Meter-1
yr_decomp1 <- decompose(housePWR_yrTS[,1])
autoplot(yr_decomp1, labels=NULL, range.bars = TRUE) +
  xlab('Year') +
  ylab('kWh') +
  ggtitle('Decomposed Monthly Time Series- Sub-Meter-1')
acf(housePWR_yrTS[,1], lag=20)

#remove seasonality
yr_seasonAdj1 <- seasadj(yr_decomp1)
plot(yr_seasonAdj1)

##-Sub-Meter-2
yr_decomp2 <- decompose(housePWR_yrTS[,2])
autoplot(yr_decomp2, labels=NULL, range.bars = TRUE) +
  xlab('Year') +
  ylab('kWh') +
  ggtitle('Decomposed Yearly Time Series- Sub-Meter-2')

#remove seasonality
yr_seasonAdj2 <- seasadj(yr_decomp2)
plot(yr_seasonAdj2)


##-Sub-Meter-3
yr_decomp3 <- decompose(housePWR_yrTS[,3])
autoplot(yr_decomp3,  range.bars = TRUE) +
  xlab('Year') +
  ylab('kWh') +
  ggtitle('Decomposed Monthly Time Series- Sub-Meter-3')
yr_decomp3
acf(housePWR_yrTS[,3])

#-remove seasonality
yr_seasonAdj3 <- seasadj(yr_decomp3)
plot(yr_seasonAdj3, xaxt='n',
     xaxp = c(2007, 2011, 4),
     col='blue',
     xlab='Year', ylab='Total kWh',
     main='Seasonally Adjusted Yearly Time Series for Sub-Meter-3')
minor.tick(nx=12)
b <-'Sub-meter-3'
legend('topleft', b, col='blue', lwd=2, bty='n')

yr_smooth3 <- HoltWinters(yr_seasonAdj3, beta=FALSE, gamma=FALSE)
plot(yr_smooth3)

plot(yr_smooth3, col='blue',
     xaxp=c(2007, 2011, 4),
     xlab='Year', ylab = 'kWh',
     main='Fitted Holt-Winters Model for Yearly Time Series')
minor.tick(nx=12)
axis(side=1, at= c(1, 2,3,4,5,6,7,8,9,10,11,12, 13), labels=MonthLst)
legend('topleft', 'Sub-Meter-3', col='blue', lwd=2, bty='n')



yr_smoothFcast3 <- forecast(yr_smooth3, h=3)
plot(yr_smoothFcast3)
yr_smoothFcast3

plot(yr_smoothFcast3, include=1,
     xaxt='n',
     col='blue',
     xaxp=c(1,13,12),
     xlab='Month', ylab = 'Total kWh',
     #ylim=c(0,250),
     main='One Month Forecast for Sub-Meter 3')
#axis(side=1, at= c(1, 2,3,4,5,6,7,8,9,10,11,12, 13), labels=MonthLst)
legend('topleft', 'Sub-Meter-3', col='blue', lwd=2, bty='n')






acf(yr_seasonAdj3)


#summary(yr_decomp)
#yr_decomp

#autoplot(yr_decomp$seasonal) +
  xlab('Year') +
  ylab('kWh') +
  ggtitle('Seasonal Component Yearly Time Series')
#summary(yr_decomp$seasonal)

#plot(yr_decomp$trend, xlab='Year', ylab='kWh',
     #main='Trend Component for Yearly Time Series')
#summary(yr_decomp$trend)

#plot(yr_decomp$random, xlab='Year',
    # main='Random Component of Yearly Time Series')
#summary(yr_decomp$random)

#######################
# Month / Day of Week #
#######################


mnth_decomp1 <- decompose(housePWR_mnthTS[,1])
autoplot(mnth_decomp1, labels=NULL, range.bars = TRUE) +
  xlab('Month') +
  ylab('kWh') +
  ggtitle('Decomposed Monthly Time Series- Sub-Meter-1')



mnth_decomp2 <- decompose(housePWR_mnthTS[,2])
autoplot(mnth_decomp2, labels=NULL, range.bars = TRUE) +
  xlab('Month') +
  ylab('kWh') +
  ggtitle('Decomposed Monthly Time Series- Sub-Meter-2')
################################################################
##-Sub-Meter-3
mnth_decomp3 <- decompose(housePWR_mnthTS[,3])
autoplot(mnth_decomp3, labels=NULL, range.bars = TRUE) +
  xlab('Month') +
  ylab('kWh') +
  ggtitle('Decomposed Monthly Time Series- Sub-Meter-3')

#-remove seasonality
mnth_seasonAdj3 <- seasadj(mnth_decomp3)
plot(mnth_seasonAdj3, #xaxt='n',
     #xaxp = c(2007, 2011, 4),
     col= 'blue',
     main='',
     xlab='Year', ylab = 'Total kWh')
minor.tick(nx=12)
b <- c('Sub-meter-1', 'Sub-meter-2', 'Sub-meter-3')
legend('topleft', b, col=c('red', 'green', 'blue'), lwd=2, bty='n')

#-Fit Holt Winters Model
mnth_smooth3 <- HoltWinters(mnth_seasonAdj3, beta=FALSE, gamma=FALSE)
plot(mnth_smooth3)

plot(mnth_smooth3, xaxt='n', col='blue',
     xaxp=c(1,13,12),
     xlab='Month', ylab = 'Total kWh',
     ylim=c(0,75),
     main='Fitted Holt-Winters Model for Monthly Time Series')
axis(side=1, at= c(1, 2,3,4,5,6,7,8,9,10,11,12, 13), labels=MonthLst)
legend('topleft', 'Sub-Meter-3', col='blue', lwd=2, bty='n')

#Forecast
mnth_smoothFcast3 <- forecast(mnth_smooth3, h=30)
mnth_smoothFcast3
plot(mnth_smoothFcast3,include=1,
     #xaxt='n',
     col='blue',
     xaxp=c(1,6,1),
     xlab='Month', ylab = 'Total kWh',
     ylim=c(0,100),
     main='One Month Forecast for Sub-Meter 3')
axis(side=1, at= c(1, 6), labels=c('Jan', 'Feb'))
legend('topleft', 'Sub-Meter-3', col='blue', lwd=2, bty='n')



#plot(mnth_decomp$seasonal, xlab='Month', ylab='kWh',
    # main='Seasonal Component for Monthly Time Series')

#plot(mnth_decomp$trend, xlab='Month', ylab='kWh',
   #  main='Trend Component for Monthly Time Series')

#plot(mnth_decomp$random, xlab='Month',
     #main='Random Component of Monthlhy Time Series')

summary(mnth_decomp1)
mnth_decomp1

#####################
# Day of Week / Hour#
#####################

dofW_decomp1 <- decompose(housePWR_dofWkTS[,1])
autoplot(dofW_decomp1, labels=NULL, range.bars = TRUE) +
  xlab('Day of Week') +
  ylab('kWh') +
  ggtitle('Decomposed Daily Time Series- Sub-Meter-1')

##-Sub-Meter-2
dofW_decomp2 <- decompose(housePWR_dofWkTS[,2])
autoplot(dofW_decomp2, labels=NULL, range.bars = TRUE) +
  xlab('Day of Week') +
  ylab('kWh') +
  ggtitle('Decomposed Daily Time Series- Sub-Meter-2')

dofW_seasonAdj2 <- seasadj(dofW_decomp2)
autoplot(dofW_seasonAdj2)
acf(dofW_seasonAdj2)


dofW_decomp3 <- decompose(housePWR_dofWkTS[,3])
autoplot(dofW_decomp3, labels=NULL, range.bars = TRUE, colour=TRUE) +
  xlab('Day of Week') +
  ylab('kWh') +
  ggtitle('Decomposed Daily Time Series- Sub-Meter-3')


#plot(dofW_decomp$seasonal, xlab='Day of Week', ylab='kWh',
    # main='Seasonal Component of Daily Time Series')

#plot(dofW_decomp$trend, xlab='Day of Week', ylab='kWh',
  #   main='Trend Component of Daily Time Series')

#plot(dofW_decomp$random, xlab='Day of Week',
    # main='Random Component of Daily Time Series')

#summary(dofW_decomp)

##########################
# Hour of Day / 5_minute#
##########################

hofDay_decomp1 <- decompose(housePWR_hofDayTS[,1])
autoplot(hofDay_decomp1, labels=NULL, range.bars = TRUE) +
  xlab('Hour of Day') +
  ylab('kWh') +
  ggtitle('Decomposed Hourly Time Series- Sub-Meter-1')

hofDay_decomp2 <- decompose(housePWR_hofDayTS[,2])
autoplot(hofDay_decomp2, labels=NULL, range.bars = TRUE) +
  xlab('Hour of Day') +
  ylab('kWh') +
  ggtitle('Decomposed Hourly Time Series- Sub-Meter-2')

hofDay_decomp3 <- decompose(housePWR_hofDayTS[,3])
autoplot(hofDay_decomp3, labels=NULL, range.bars = TRUE) +
  xlab('Hour of Day') +
  ylab('kWh') +
  ggtitle('Decomposed Hourly Time Series- Sub-Meter-3')



#hofDay_decomp <- decompose(housePWR_hofDayTS)
#autoplot(hofDay_decomp, xlim=c(0,24))

#(hofDay_decomp$seasonal,xlab='Hour of Day', ylab='kWh',
    # main='Seasonal Component of Hourly Time Series')

#plot(hofDay_decomp$trend, xlab='Hour of Day', ylab='kWh',
     #main='Trend Component of Hourly Time Series')

#plot(hofDay_decomp$random, xlab='Hour of Day', ylab='kWh',
    # main='Random Component of Hourly Time Series')

#summary(hofDay_decomp)
#hofDay_decomp

#################
# Weekend Hours #
#################
#Sub-meter-1
Wknd_decomp1 <- decompose(housePWR_wkndTS[,1])
autoplot(Wknd_decomp1, labels=NULL, range.bars = TRUE) +
  xlab('Weekend Day') +
  ylab('kWh') +
  ggtitle('Decomposed Weekend Time Series- Sub-Meter-1')

#-remove sesonality
Wknd_seasonAdj1 <- seasadj(Wknd_decomp1)
plot(Wknd_seasonAdj1, xaxt='n',
     xaxp=c(0,3,2),
     col='red',
     xlab='Weekend Day', ylab = 'Total kWh',
     main='Seasonally Adjusted Weekend Energy Consumption')
axis(side = 1, at = c(1,2,3), labels = WkndList)
minor.tick(nx=24)
b <- 'Sub-meter-1'
legend('topleft', b, col='red', lwd=2, bty='n')


Wknd_decomp2 <- decompose(housePWR_wkndTS[,2])
autoplot(Wknd_decomp2, labels=NULL, range.bars = TRUE) +
  xlab('Weekend Day') +
  ylab('kWh') +
  ggtitle('Decomposed Weekend Time Series- Sub-Meter-2')

Wknd_decomp3 <- decompose(housePWR_wkndTS[,3])
autoplot(Wknd_decomp3, labels=NULL, range.bars = TRUE) +
  xlab('Weekend Day') +
  ylab('kWh') +
  ggtitle('Decomposed Weekend Time Series- Sub-Meter-3')


# Holt-Winters Smooting------------------------------------------------------------

##########
# Yearly #
##########

#sub-meter-1
#remove seasonality
yr_seasonAdj1 <- housePWR_yrTS[,1]-yr_decomp1$seasonal
plot(yr_seasonAdj1)
acf(yr_seasonAdj1)

yr_seasonAdj1 <- seasadj(yr_decomp1)

yr_smooth1 <- HoltWinters(yr_seasonAdj1, beta=FALSE, gamma=FALSE)
plot(yr_smooth1)

yr_smoothFcast1 <- forecast(yr_smooth1)
autoplot(yr_smoothFcast1)

#sub-meter-2
#remove seasonality
yr_seasonAdj2 <- housePWR_yrTS[,2]-yr_decomp2$seasonal
plot(yr_seasonAdj2)
acf(yr_seasonAdj2)

yr_smooth2 <- HoltWinters(yr_seasonAdj2, beta=FALSE, gamma=FALSE)
plot(yr_smooth2)

yr_smoothFcast2 <- forecast(yr_smooth2)
autoplot(yr_smoothFcast2)

#sub-meter-3
#remove seasonality
yr_seasonAdj3 <- housePWR_yrTS[,3]-yr_decomp3$seasonal
plot(yr_seasonAdj3)
acf(yr_seasonAdj3)

yr_smooth3 <- HoltWinters(yr_seasonAdj3, beta=FALSE, gamma=FALSE)
plot(yr_smooth3)

yr_smoothFcast3 <- forecast(yr_smooth3)
autoplot(yr_smoothFcast3)
acf(yr_smoothFcast3$residuals, na.action = na.omit, lag=50)

#######################################################

#sub-meter-1

# Remove seasonal component
yr_seasonAdj <- housePWR_yrTS - yr_decomp$seasonal
plot(yr_seasonAdj, plot.type='s',
     #xaxp = c(2007, 2011, 4),
     col=c('red', 'green', 'blue'),
     xlab='Year', ylab='kWh',
     main='Seasonally-Adjusted Yearly Energy Consumption')
minor.tick(nx=6)
b <- c('Sub-meter-1', 'Sub-meter-2', 'Sub-meter-3')
legend('topleft', b, col=c('red', 'green', 'blue'), lwd=2, bty='n')

yr_smooth1 <- HoltWinters(housePWR_yrTS[,1], beta=FALSE, gamma=FALSE)

plot(yr_smooth1, plot.type='m',
     #xaxp= c(2007, 2010, 3),
     xlab='Year', ylab='kWh',
     ylim= c(0,80),
     main='Holt-Winters Forecast of Yearly Energy Consumption')
b <- 'Sub-meter-1'
legend('topleft', b, col='red', lwd=2, bty='n')


yr_smoothFcast1 <- forecast(yr_smooth1, h=2)
autoplot(yr_smoothFcast1,
         xlim=c(2010.5, 2011.1),
         xlab='Year', ylab='Total kWh',
         main='30 day Forecast of Energy Consumption')





#######################
# Month / Day of Week #
#######################
#sub-meter-1
#-Remove seasonality
mnth_seasonAdj1 <- housePWR_mnthTS[,1]-mnth_decomp1$seasonal
acf(mnth_seasonAdj1)

mnth_smooth1 <- HoltWinters(mnth_seasonAdj1, beta=FALSE, gamma=FALSE)
plot(mnth_smooth1)

mnth_smoothFcast1 <- forecast(mnth_smooth1)
autoplot(mnth_smoothFcast1)

#Sub-Meter-2
#Remove Seasonality
mnth_seasonAdj2 <- housePWR_mnthTS[,2]-mnth_decomp2$seasonal
acf(mnth_seasonAdj2)

mnth_smooth2 <- HoltWinters(mnth_seasonAdj2, beta=FALSE, gamma=FALSE)
plot(mnth_smooth2)

mnth_smoothFcast2 <- forecast(mnth_smooth2)
autoplot(mnth_smoothFcast2)

#Sub-meter-3













acf(mnth_smoothFcast3$residuals, na.action=na.pass, lag=50)


# Remove seasonal component
mnth_seasonAdj <- housePWR_mnthTS - mnth_decomp$seasonal
plot(mnth_seasonAdj, plot.type='s',
     xaxp = c(1, 12, 11),
     col=c('red', 'green', 'blue'),
         xlab='Month', ylab='kWh',
         main='Seasonally-Adjusted Monthly Energy Consumption')
b <- c('Sub-meter-1', 'Sub-meter-2', 'Sub-meter-3')
legend('top', b, col=c('red', 'green', 'blue'), lwd=2, bty='n')

############################################
#sub-meter-1
mnth_smooth1 <- HoltWinters(mnth_seasonAdj[,1], beta=FALSE, gamma=FALSE)
plot(mnth_smooth1)

mnth_smoothFcast1 <- forecast(mnth_smooth1)
autoplot(mnth_smoothFcast1,
         #xlim=c(10,13),
         xlab='Month',
         ylab='kWh',
         main='30 Day Forecast of Sub-Meter-1')

#sub-meter-2
mnth_smooth2 <- HoltWinters(mnth_seasonAdj[,2], beta=FALSE, gamma=FALSE)
plot(mnth_smooth2)

mnth_smoothFcast2 <- forecast(mnth_smooth2)
autoplot(mnth_smoothFcast2,
         xlim=c(10,15),
         xlab='Month',
         ylab='kWh',
         main='30 Day Forecast of Sub-Meter-2')

#sub-meter-3
mnth_smooth3 <- HoltWinters(mnth_seasonAdj[,3], beta=FALSE, gamma=FALSE)
plot(mnth_smooth3)

mnth_smoothFcast3 <- forecast(mnth_smooth3)
autoplot(mnth_smoothFcast3,
         xlim=c(10,15),
         xlab='Month',
         ylab='kWh',
         main='30 Day Forecast of Sub-Meter-3')

#####################
# Day of Week / Hour#
#####################

dofW_seasonAdj <- housePWR_dofWkTS - dofW_decomp$seasonal
plot(dofW_seasonAdj, plot.type='s',
     xaxp = c(1, 7, 6),
     col=c('red', 'green', 'blue'),
     xlab='Day of Week', ylab='kWh',
     main='Seasonally-Adjusted Energy Consumption for Days of the Week')
b <- c('Sub-meter-1', 'Sub-meter-2', 'Sub-meter-3')
legend('topleft', b, col=c('red', 'green', 'blue'), lwd=2, bty='n')


#sub-meter-1
dofW_smooth1 <- HoltWinters(dofW_seasonAdj[,1], beta=FALSE, gamma=FALSE)
plot(dofW_smooth1)

dofW_smoothFcast1 <- forecast(dofW_smooth1, h=30)
autoplot(dofW_smoothFcast1,
         xlim= c(5, 9),
         xlab='Day of the Week',
         ylab='kWh',
         main='Forecast of Sub-Meter-1')

#sub-meter-2
dofW_smooth2 <- HoltWinters(dofW_seasonAdj[,2], beta=FALSE, gamma=FALSE)
plot(dofW_smooth2)

dofW_smoothFcast2 <- forecast(dofW_smooth2)
autoplot(dofW_smoothFcast2)

#sub-meter-3
dofW_smooth3 <- HoltWinters(dofW_seasonAdj[,3], beta=FALSE, gamma=FALSE)
plot(dofW_smooth3)

dofW_smoothFcast3 <- forecast(dofW_smooth3)
autoplot(dofW_smoothFcast3)

##########################
# Hour of Day / 10_minute#
##########################

#sub-meter-1
##-Remove seasonality
hofDay_seasonAdj1 <- housePWR_hofDayTS[,1] - hofDay_decomp1$seasonal
acf(hofDay_seasonAdj1)
hofDay_smooth1 <- HoltWinters(hofDay_seasonAdj1, beta=FALSE, gamma=FALSE)
plot(hofDay_smooth1)

hofDay_smoothFcast1 <- forecast(hofDay_smooth1)
plot(hofDay_smoothFcast1)

#-Sub-meter-2
hofDay_seasonAdj2 <- housePWR_hofDayTS[,2] - hofDay_decomp2$seasonal
acf(hofDay_seasonAdj2)

hofDay_smooth2 <- HoltWinters(hofDay_seasonAdj2, beta=FALSE, gamma=FALSE)
plot(hofDay_smooth2)

hofDay_smoothFcast2 <- forecast(hofDay_smooth2)
plot(hofDay_smoothFcast2)

#-Sub-meter-3
hofDay_seasonAdj3 <- housePWR_hofDayTS[,3] - hofDay_decomp3$seasonal
acf(hofDay_seasonAdj3)
hofDay_smooth2 <- HoltWinters(hofDay_seasonAdj3, beta=FALSE, gamma=FALSE)
plot(hofDay_smooth3)

hofDay_smoothFcast3 <- forecast(hofDay_smooth3, h=50)
plot(hofDay_smoothFcast3)






hofDay_seasonAdj3 <- housePWR_hofDayTS[,3]-hofDay_decomp3$seasonal
plot(hofDay_seasonAdj3, plot.type='s',
     xaxp = c(0, 23, 23),
     col=c('red', 'green', 'blue'),
     xlab='Hour of Day', ylab='kWh',
     main='Seasonally-Adjusted Hourly Energy Consumption')
b <- c('Sub-meter-1', 'Sub-meter-2', 'Sub-meter-3')
legend('topleft', b, col=c('red', 'green', 'blue'), lwd=2, bty='n')

acf(housePWR_hofDayTS, lag=40)
acf(hofDay_seasonAdj3, lag=40)

#sub-meter-1
hofDay_smooth1 <- HoltWinters(hofDay_seasonAdj[,1], beta=FALSE, gamma=FALSE)
plot(hofDay_smooth1)

hofDay_smoothFcast1 <- forecast(hofDay_smooth1, h=24)
autoplot(hofDay_smoothFcast1)

#sub-meter-2
hofDay_smooth2 <- HoltWinters(hofDay_seasonAdj[,2], beta=FALSE, gamma=FALSE)
plot(hofDay_smooth2)

hofDay_smoothFcast2 <- forecast(hofDay_smooth2)
autoplot(hofDay_smoothFcast2)

#sub-meter-3
hofDay_smooth3 <- HoltWinters(hofDay_seasonAdj3, beta=FALSE, gamma=FALSE)
plot(hofDay_smooth3)

hofDay_smoothFcast3 <- forecast(hofDay_smooth3, h=150)
autoplot(hofDay_smoothFcast3)

##########
# Weekend#
##########

Wknd_smooth1 <- HoltWinters(Wknd_seasonAdj1, beta=FALSE, gamma = FALSE)
plot(Wknd_smooth1)

Wknd_smoothFcast1 <- forecast(Wknd_smooth1, h=20)
plot(Wknd_smoothFcast1)

acf(Wknd_smoothFcast1$residuals, na.action = na.pass, lag=48)


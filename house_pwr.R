
# Description --------------------------------------------------------------------

# Rutgers Data Analytics / Big Data 2017
# Project 4 Deep Analytics and Visualization


# Summary -----------------------------------------------------------------





# Load Packages -----------------------------------------------------------


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

housePWR_NA <- house_pwr[rowSums(is.na(house_pwr))>0,]

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

write.csv(house_pwr, file='house_pwr.csv')

housePWR_sum <- house_pwr %>%
  select()

# Exploratory Data Analysis -----------------------------------------------

# Proportional and Line Plots across sub-metered zones-------------------------

###- Year-Proportional Plot of total energy across sub-metered zones

##-Year_Proportional Plot
house_pwr_tidy %>%
  filter(year(DateTime)>2006) %>%
  group_by(year(DateTime), Meter) %>%
  summarise(sum=sum(Watt_hr)) %>%
  ggplot(aes(x=factor(`year(DateTime)`), sum, group=Meter,fill=Meter)) +
  labs(x='Year', y='Proportion of Energy Useage') +
  ggtitle('Proportion of Total Yearly Energy Consumption') +
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

##-Year Bar Chart
house_pwr_tidy %>%
  filter(year(DateTime)>2006) %>%
  #mutate(year=year(DateTime)) %>%
  group_by(year(DateTime), Meter) %>%
  summarise(sum=round(sum(Watt_hr/1000),3)) %>%
  ggplot(aes(x=factor(`year(DateTime)`), y=sum)) +
  labs(x='Year', y='kWh') +
  ggtitle('Total Energy Useage by Year') +
  geom_bar(stat='identity', aes(fill = Meter), colour='black')


#Quarter bar  plot
house_pwr_tidy %>%
  filter(year(DateTime)>2006) %>%
  #filter(year(DateTime)<2010) %>%
  #mutate(quarter=quarter(DateTime)) %>%
  group_by(quarter(DateTime), Meter) %>%
  summarise(sum=round(sum(Watt_hr/1000),3)) %>%
  ggplot(aes(x=factor(`quarter(DateTime)`), y=sum)) +
  labs(x='Quarter of the Year', y='Wh') +
  ggtitle('Average Quarterly Energy Consumption') +
  geom_bar(stat='identity', aes(fill = Meter), color='black')

#Quarter Proportion plot
house_pwr_tidy %>%
  filter(year(DateTime)>2006) %>%
  group_by(quarter(DateTime), Meter) %>%
  #filter(quarter(DateTime)<3) %>%
  summarise(sum=round(sum(Watt_hr)/1000),3) %>%
  ggplot(aes(x=factor(`quarter(DateTime)`), sum, group = Meter, fill=Meter)) +
  labs(x='Quarter of the Year', y='kWh') +
  ggtitle('Total Quarterly Energy Consumption') +
  geom_bar(stat='identity', position='fill', color='black')

###-Month- Proportional Plot
house_pwr_tidy %>%
  filter(year(DateTime)>2006) %>%
  mutate(Month=lubridate::month(DateTime, label=TRUE, abbr=TRUE)) %>%
  group_by(Month, Meter) %>%
  summarise(sum=sum(Watt_hr/1000)) %>%
  ggplot(aes(x=factor(Month), sum, group=Meter,fill=Meter)) +
  labs(x='Month of the Year', y='Proportion of Monthly Energy Useage') +
  ggtitle('Proportion of Average Monthly Energy Useage') +
  geom_bar(stat='identity', position='fill', color='black')

###-Month- Line Plot
house_pwr_tidy %>%
  filter(year(DateTime)>2006) %>%
  mutate(Month=lubridate::month(DateTime, label=TRUE, abbr=TRUE)) %>%
  group_by(Month, Meter) %>%
  summarise(sum=sum(Watt_hr/1000)) %>%
  ggplot(aes(x=factor(Month), sum, group=Meter, colour=Meter)) +
  labs(x='Month of the Year', y='Wh') +
  ggtitle('Average Monthly Energy Useage') +
  geom_line(size=1) +
  geom_line()

###-Month bar chart
house_pwr_tidy %>%
  filter(year(DateTime)>2006) %>%
  #filter(year(DateTime)<2010) %>%
  mutate(Month=lubridate::month(DateTime, label=TRUE, abbr=TRUE)) %>%
  group_by(Month, Meter) %>%
  summarise(sum=round(sum(Watt_hr)/1000),3) %>%
  ggplot(aes(x=factor(Month), y=sum)) +
  labs(x='Month of the Year', y='kWh') +
  ggtitle('Total Energy Useage by Month of the Year') +
  geom_bar(stat='identity', aes(fill = Meter), colour='black')

#Week of the year- line plot
house_pwr_tidy %>%
  filter(year(DateTime)>2006) %>%
  group_by(week(DateTime), Meter) %>%
  summarise(sum=round(sum(Watt_hr)/1000),3) %>%
  ggplot(aes(x=factor(`week(DateTime)`), sum, group=Meter,colour=Meter)) +
  labs(x='Week of the Year', y='kWh') +
  ggtitle('Total Energy Usage by Week of the Year') +
  theme(axis.text.x = element_text(angle=90)) +
  geom_line(size=1) +
  geom_line()

#Week of the year- bar plot
house_pwr_tidy %>%
  #filter(year(DateTime)>2006) %>%
  group_by(week(DateTime), Meter) %>%
  summarise(sum=sum(Watt_hr/1000)) %>%
  ggplot(aes(x=factor(`week(DateTime)`), y=sum)) +
  labs(x='Week of the Year', y='kWh') +
  ggtitle('Total Energy Usage by Week of the Year') +
  theme(axis.text.x = element_text(angle=90)) +
  geom_bar(stat='identity', aes(fill=Meter), colour='black')



### Day of the Month- Line Plot
#Not informative
house_pwr_tidy %>%
  group_by(day(DateTime), Meter) %>%
  summarise(sum=sum(Watt_hr/1000)) %>%
  ggplot(aes(x=factor(`day(DateTime)`), sum, group=Meter,colour=Meter)) +
  labs(x='Day of the Month', y='Avg Watt Hour Useage') +
  ggtitle('Average Daily Watt Hour Useage') +
  geom_line(size=1) +
  geom_line()


###-Day of Week- Porportional Plot
house_pwr_tidy %>%
  filter(year(DateTime)>2006) %>%
  mutate(Day=lubridate::wday(DateTime, label=TRUE, abbr=TRUE)) %>%
  group_by(Day, Meter) %>%
  summarise(sum=sum(Watt_hr/1000)) %>%
  ggplot(aes(x=factor(Day), sum, group=Meter,fill=Meter)) +
  labs(x='Day of the Week', y='Proportion of Energy Useage') +
  ggtitle('Proportion of Average Daily Energy Consumption') +
  geom_bar(stat='identity', position='fill', color='black')

###-Day of Week- Line Plot
house_pwr_tidy %>%
  filter(year(DateTime)>2006) %>%
  mutate(Day=lubridate::wday(DateTime, label=TRUE, abbr=TRUE)) %>%
  group_by(Day, Meter) %>%
  summarise(sum=sum(Watt_hr/1000)) %>%
  ggplot(aes(x=factor(Day), sum, group=Meter, colour=Meter)) +
  labs(x='Day of the Week', y='Wh') +
  ggtitle('Average Daily Energy Consumption') +
  geom_line(size=1) +
  geom_line()

###-Day of Week bar chart
house_pwr_tidy %>%
  filter(year(DateTime)>2006) %>%
  mutate(Day=lubridate::wday(DateTime, label=TRUE, abbr=TRUE)) %>%
  group_by(Day, Meter) %>%
  summarise(sum=sum(Watt_hr/1000)) %>%
  ggplot(aes(x=factor(Day), y=sum)) +
  labs(x='Day of the Week', y='kWh') +
  ggtitle('Total Energy Useage by Day of Week') +
  geom_bar(stat='identity', aes(fill = Meter), colour='black')

#Weekend bar chart
house_pwr_tidy %>%
  filter(year(DateTime)>2006) %>%
  mutate(weekend=lubridate::wday(DateTime, label=TRUE, abbr=FALSE)) %>%
  filter(weekend==c('Saturday','Sunday')) %>%
  group_by(weekend, Meter) %>%
  summarise(sum=sum(Watt_hr/1000)) %>%
  ggplot(aes(x=factor(weekend), sum, group=Meter,fill=Meter)) +
  labs(x='Weekend Day', y='Proportion of Energy Useage') +
  ggtitle('Proportion of Average Energy Consumption by Weekend Day') +
  geom_bar(stat='identity', position='fill', color='black')



###-Hour of the Day- Proportional Plot
house_pwr_tidy %>%
  filter(year(DateTime)>2006) %>%
  group_by(hour(DateTime), Meter) %>%
  summarise(sum=sum(Watt_hr/1000)) %>%
  ggplot(aes(x=factor(`hour(DateTime)`), sum, group=Meter,fill=Meter)) +
  labs(x='Hour of the Day', y='Proportion of Energy Useage') +
  ggtitle('Proportion of Average Hourly Energy Consumption') +
  geom_bar(stat='identity', position='fill', color='black')

###-Hour of the Day-Line Plot
house_pwr_tidy %>%
  filter(year(DateTime)>2006) %>%
  group_by(hour(DateTime), Meter) %>%
  summarise(sum=round(mean(Watt_hr), 3)) %>%
  ggplot(aes(x=factor(`hour(DateTime)`), sum, group=Meter,colour=Meter)) +
  labs(x='Hour of the Day', y='Wh') +
  ggtitle('Average Hourly Energy Consumption') +
  geom_line(size=1) +
  geom_line()

#Hour of day bar chart
house_pwr_tidy %>%
  filter(year(DateTime)>2006) %>%
  group_by(hour(DateTime), Meter) %>%
  summarise(sum=round(sum(Watt_hr)/1000),3) %>%
  ggplot(aes(x=factor(`hour(DateTime)`), y=sum)) +
  labs(x='Hour of the Day', y='kWh') +
  ggtitle('Total Energy Useage by Hour of the Day') +
  geom_bar(stat='identity', aes(fill = Meter), colour='black')

# Correlation plot
ggcorr(house_pwr) +
  ggtitle('Correlation Plot of Energy Consumption Dataset')


# Subset ------------------------------------------------------------------


#-Subset Year
housePWR_yr <- house_pwr %>%
  filter(year(DateTime) > 2006) %>%
  #filter(year(DateTime)<2010) %>%
  group_by(year(DateTime)) %>%
  summarise(Sub_Meter_1=mean(`Sub-Meter-1`, na.rm=TRUE),
            Sub_Meter_2=mean(`Sub-Meter-2`, na.rm=TRUE),
            Sub_Meter_3=mean(`Sub-Meter-3`, na.rm=TRUE),
            first_DateTime = first(DateTime))

#-Subset Semester
housePWR_semstr <- house_pwr %>%
  filter(year(DateTime) > 2006) %>%
  group_by(year(DateTime),semester(DateTime)) %>%
  summarise(Sub_Meter_1=mean(`Sub-Meter-1`, na.rm=TRUE),
            Sub_Meter_2=mean(`Sub-Meter-2`, na.rm=TRUE),
            Sub_Meter_3=mean(`Sub-Meter-3`, na.rm=TRUE),
            first_DateTime = first(DateTime))

#-Subset Quarter
housePWR_qtr <- house_pwr %>%
  filter(year(DateTime) > 2006) %>%
  #filter(year(DateTime)<2010) %>%
  group_by(year(DateTime), quarter(DateTime)) %>%
  summarise(Sub_Meter_1=mean(`Sub-Meter-1`, na.rm=TRUE),
            Sub_Meter_2=mean(`Sub-Meter-2`, na.rm=TRUE),
            Sub_Meter_3=mean(`Sub-Meter-3`, na.rm=TRUE),
            first_DateTime = first(DateTime))

#-Subset Month
housePWR_mnth <- house_pwr %>%
  #filter(year(DateTime) > 2006) %>%
  #filter(year(DateTime)<2011) %>%
  group_by(year(DateTime), month(DateTime)) %>%
  summarise(Sub_Meter_1=mean(`Sub-Meter-1`, na.rm=TRUE),
            Sub_Meter_2=mean(`Sub-Meter-2`, na.rm=TRUE),
            Sub_Meter_3=mean(`Sub-Meter-3`, na.rm=TRUE),
            first_DateTime = first(DateTime))

#-Subset Week of year
housePWR_wkofYr <- house_pwr %>%
  #filter(year(DateTime)>2006) %>%
  #filter(year(DateTime)<2010) %>%
  mutate(DofWk=lubridate::wday(DateTime, label=TRUE, abbr=TRUE)) %>%
  group_by(year(DateTime),week(DateTime)) %>%
  summarise(Sub_Meter_1=mean(`Sub-Meter-1`, na.rm=TRUE),
            Sub_Meter_2=mean(`Sub-Meter-2`, na.rm=TRUE),
            Sub_Meter_3=mean(`Sub-Meter-3`, na.rm=TRUE),
            first_DateTime = first(DateTime))

#-Subset by day of week
housePWR_dofWk <- house_pwr %>%
  filter(year(DateTime)>2006) %>%
  #filter(year(DateTime)<2010) %>%
  mutate(DofWk=lubridate::wday(DateTime, label=TRUE, abbr=TRUE)) %>%
  group_by(week(DateTime),DofWk) %>%
  summarise(Sub_Meter_1=mean(`Sub-Meter-1`, na.rm=TRUE),
            Sub_Meter_2=mean(`Sub-Meter-2`, na.rm=TRUE),
            Sub_Meter_3=mean(`Sub-Meter-3`, na.rm=TRUE),
            first_DateTime = first(DateTime))

#-Subset hour of day
housePWR_hofDay <- house_pwr %>%
  filter(year(DateTime)>2006) %>%
  #filter((minute(DateTime) %% 5) == 0) %>%
  group_by(wday(DateTime), hour(DateTime)) %>%
  summarise(Sub_Meter_1=mean(`Sub-Meter-1`, na.rm=TRUE),
            Sub_Meter_2=mean(`Sub-Meter-2`, na.rm=TRUE),
            Sub_Meter_3=mean(`Sub-Meter-3`, na.rm=TRUE),
            first_DateTime = first(DateTime))

#-Subset by Weekends
housePWR_wknd <- house_pwr %>%
  filter(year(DateTime)>2006) %>%
  #filter(year(DateTime)<2010) %>%
  mutate(Wknd=lubridate::wday(DateTime, label=TRUE, abbr=TRUE)) %>%
  filter(Wknd == c('Sat', 'Sun')) %>%
  group_by(Wknd, hour(DateTime)) %>%
  summarise(Sub_Meter_1=mean(`Sub-Meter-1`, na.rm=TRUE),
            Sub_Meter_2=mean(`Sub-Meter-2`, na.rm=TRUE),
            Sub_Meter_3=mean(`Sub-Meter-3`, na.rm=TRUE),
            first_DateTime = first(DateTime)) %>%
 arrange(desc(Wknd))


# Convert to Time Series --------------------------------------------------


# Year_TS
housePWR_yrTS <- ts(housePWR_yr[,2:4], frequency = 1, start=c(2007))
plot(housePWR_yrTS, plot.type='s', #xaxt='n',
     xaxp = c(2007, 2010, 3),
     col=c('red', 'green', 'blue'),
     main='Total Yearly kWh Consumption (2007-2010)',
     xlab='Year', ylab = 'Total kWh')
b <- c('Sub-meter-1', 'Sub-meter-2', 'Sub-meter-3')
legend('topleft', b, col=c('red', 'green', 'blue'), lwd=2, bty='n')

#Semester_TS
housePWR_semstrTS <- ts(housePWR_semstr[,3:5], frequency = 2, start=c(2007,1), end=c(2010,1))
plot(housePWR_semstrTS, plot.type='s', #xaxt='n',
     #xaxp = c(2007, 2010, 3),
     col=c('red', 'green', 'blue'),
     main='Total Yearly kWh Consumption (2007-2010)',
     xlab='Year', ylab = 'Total kWh')
b <- c('Sub-meter-1', 'Sub-meter-2', 'Sub-meter-3')
legend('topleft', b, col=c('red', 'green', 'blue'), lwd=2, bty='n')

#Quarter_TS
housePWR_qtrTS <- ts(housePWR_qtr[,3:5], frequency=4, start=c(2007,1), end=c(2010,4))
plot(housePWR_qtrTS, plot.type='s',
     #xaxt='n',
     #xlim=c(2007, 2010),
     #xaxp=c(2006, 2011, 5),
     col=c('red', 'green', 'blue'),
     main='Total Quarterly kWh Consumption (2007-2010)',
     xlab='Year', ylab = 'Total kWh')
minor.tick(nx=4)
b <- c('Sub-meter-1', 'Sub-meter-2', 'Sub-meter-3')
legend('topleft', b, col=c('red', 'green', 'blue'), lwd=2, bty='n')


#Month_TS
housePWR_mnthTS <- ts(housePWR_mnth[,3:5], frequency = 12, start=c(2007,1), end=c(2010,11))
plot(housePWR_mnthTS, plot.type='s',#xaxt='n',
     #xaxp = c(1,13,12),
     col=c('red', 'green', 'blue'),
     main='Average Monthly Wh Consumption',
     xlab='Year/Month', ylab = 'Wh')
minor.tick(nx=12)
b <- c('Sub-meter-1', 'Sub-meter-2', 'Sub-meter-3')
legend('topleft', b, col=c('red', 'green', 'blue'), lwd=2, bty='n')


# Week of the year_TS
housePWR_wkofYrTS <- ts(housePWR_wkofYr[,3:5], frequency =53, start=c(2007,1), end=c(2010,48))
plot(housePWR_wkofYrTS, plot.type='s', #xaxt='n',
     #xaxp = c(1, 13, 12),
     col=c('red', 'green', 'blue'),
     xlab ='Year', ylab = 'Wh',
     #ylim=c(0,120),
     main='Average Energy Consumption by Week of the Year')
#axis(side=1, at= c(1, 2,3,4,5,6,7,8,9,10,11,12,13), labels=MonthLst)
minor.tick(nx=52)
b <- c('Sub-meter-1', 'Sub-meter-2', 'Sub-meter-3')
legend('topleft', b, col=c('red', 'green', 'blue'), lwd=2, bty='n')


#Day of Week_TS
housePWR_dofWkTS <- ts(housePWR_dofWk[,3:5], frequency =7, start=c(1,1))
plot(housePWR_dofWkTS, plot.type='s', #xaxt='n',
     col=c('red', 'green', 'blue'),
     xlab ='Week of Year', ylab = 'Wh',
     xlim = c(1,53) , ylim=c(0,75),
     main='Total Energy Consumption by Day of Week')
minor.tick(nx=70)
b <- c('Sub-meter-1', 'Sub-meter-2', 'Sub-meter-3')
legend('topleft', b, col=c('red', 'green', 'blue'), lwd=2, bty='n')


# Hour of Day_TS
housePWR_hofDayTS <- ts(housePWR_hofDay[,3:5], frequency=24, start=c(0,0))
plot(housePWR_hofDayTS, plot.type='s',
     #xaxp = c(0,48, 47),
     col=c('red', 'green', 'blue'),
     xlab='Day', ylab = 'kWh',
     main='Total kWh Consumption by Hour of the Day')
minor.tick(nx=24)
b <- c('Sub-meter-1', 'Sub-meter-2', 'Sub-meter-3')
legend('topleft', b, col=c('red', 'green', 'blue'), lwd=2, bty='n')


# Hour_Weekend_TS
housePWR_wkndTS <- ts(housePWR_wknd[,3:5], frequency=24, end(1,23))
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


# Forecasting -------------------------------------------------------------


# Year_forecast
fit1 <- tslm(housePWR_yrTS ~ trend + season)
x <- forecast(fit1, h=2, level = c(90, 95))
autoplot(x, PI=TRUE, colour=TRUE, showgap=TRUE) +
  xlab('Year') +
  ylab('Total kWh') +
  ggtitle('Forecasted Trend of Yearly Energy Consumption')
summary(x)
x

#Quarter_forecast
fit2 <- tslm(housePWR_qtrTS[,3] ~ trend + season)
y <- forecast(fit2, h=5, level=c(90,95))
plot(y, showgap=FALSE,
     shadecols=c('slategray3','slategray'),
     main='Forecasted Quarterly Energy Consumption')
summary(y)

# Month_forecast
fit3 <- tslm(housePWR_mnthTS[,3] ~ trend + season)
z <- forecast(fit3,h=12, level=c(90,95))
plot(z, showgap=FALSE,
  shadecols=c('slategray3','slategray'),
  xlab ='Month',
  ylab='Average Wh',
  main='Forecasted Monthly Energy Consumption')
summary(z)
z
#level =	Confidence level for prediction intervals.

#Week of year_forecast
fit4 <- tslm(housePWR_wkofYrTS[,3] ~ trend + season)
xx <- forecast(fit4, level=c(90,95), h=53)
plot(xx, showgap=FALSE,
     shadecols=c('slategray3','slategray'),
     xlab ='Week',
     ylab='Average Wh',
     main='Forecasted Weekly Energy Consumption')
summary(xx)
xx

#day of week_forecast
fit4a <- tslm(housePWR_dofWkTS ~ trend)
xxa <- forecast(fit4a, level=c(90,95), h=30)
autoplot(xxa, PI=TRUE, colour=TRUE) +
  xlab('Week of the Year') +
  ylab('Total kWh') +
  ggtitle('Forecasted Trend of Energy Consumption by Day of the Week')
summary(fit4a)
xxa

# Hour of Day_forecast
fit5 <- tslm(housePWR_hofDayTS ~ trend)
yy <- forecast(fit5, h=48)
autoplot(yy, PI=TRUE, colour=TRUE) +
  xlab('Day of the Week') +
  ylab('Total kWh') +
  ggtitle('Forecasted Trend of Hourly Energy Consumption in a Day')
summary(yy)
yy

# Weekend hours_forecast
fit6 <- tslm(housePWR_wkndTS ~ trend)
zz <- forecast(fit6, level=c(90,95))
autoplot(zz, PI=TRUE, colour=TRUE) +
  xlab('Weekend Day') +
  ylab('Total kWh') +
  ggtitle('Forecasted Trend of Weekend Energy Consumption')
summary(zz)
zz

# Decompose Time Series / Remove Seasonality/ HW Smoothing' -------------------------------------------------------------


##############
# Year       #
##############

##-Sub-Meter-1
#-Decompose TS
yr_decomp1 <- decompose(housePWR_yrTS[,1])
autoplot(yr_decomp1, labels=NULL, range.bars = TRUE) +
  xlab('Year') +
  ylab('kWh') +
  ggtitle('Decomposed Monthly Time Series- Sub-Meter-1')

#remove seasonality
yr_seasonAdj1 <- seasadj(yr_decomp1)
plot(yr_seasonAdj1)

##-Sub-Meter-2
#-Decompose TS
yr_decomp2 <- decompose(housePWR_yrTS[,2])
autoplot(yr_decomp2, labels=NULL, range.bars = TRUE) +
  xlab('Year') +
  ylab('kWh') +
  ggtitle('Decomposed Yearly Time Series- Sub-Meter-2')

#remove seasonality
yr_seasonAdj2 <- seasadj(yr_decomp2)
plot(yr_seasonAdj2)


##-Sub-Meter-3
#-Decompose TS
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

#-Fit Holt Winters simple exponetial smoothing model
yr_smooth3 <- HoltWinters(yr_seasonAdj3, beta=FALSE, gamma=FALSE)
plot(yr_smooth3, col='blue',
     xaxp=c(2007, 2010, 3),
     xlab='Year', ylab = 'kWh',
     main='Fitted Holt-Winters Model for Yearly Time Series')
minor.tick(nx=12)
legend('topleft', 'Sub-Meter-3', col='blue', lwd=2, bty='n')

#-Forecast
yr_smoothFcast3 <- forecast(yr_smooth3, h=5)
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

########
Quarter#
########

##-Sub-Meter-3
#-Decompose TS
qtr_decomp3 <- decompose(housePWR_qtrTS[,3])
autoplot(qtr_decomp3,  range.bars = TRUE) +
  xlab('Year') +
  ylab('kWh') +
  ggtitle('Decomposed Quarterly Time Series- Sub-Meter-3')
qtr_decomp3


#-remove seasonality
qtr_seasonAdj3 <- seasadj(qtr_decomp3)
acf(qtr_seasonAdj3, na.action=na.omit,lag=30)

plot(qtr_seasonAdj3, #xaxt='n',
     col='blue',
     xlab='Year', ylab='Total kWh',
     xlim=c(2007, 2010),
     xaxp=c(2006, 2010, 4),
     #ylim=c(100,375),
     main='Seasonally Adjusted Quarterly Time Series')
minor.tick(nx=2)
b <-'Sub-meter-3'
legend('topleft', b, col='blue', lwd=2, bty='n')

#-Fit Holt Winters simple exponetial smoothing model
qtr_smooth3 <- HoltWinters(qtr_seasonAdj3, beta=FALSE, gamma=FALSE)
plot(qtr_smooth3, col='blue', #xaxt='n',
     xlab='Year', ylab = 'kWh',
     xlim=c(2007, 2010),
     xaxp=c(2006, 2010, 4),
     #ylim=c(100,400),
     main='Fitted Holt-Winters Model for Quarterly Time Series')
minor.tick(nx=2)
legend('topleft', 'Sub-Meter-3', col='blue', lwd=2, bty='n')

#-Forecast
qtr_smoothFcast3 <- forecast(qtr_smooth3, robust= TRUE,h=5, level=c(85,90))
plot(qtr_smoothFcast3)
qtr_smoothFcast3
summary(qtr_smoothFcast3)

plot(qtr_smoothFcast3, include=1,
     #xaxt='n',
     shadecols=c('slategray3','slategray'),
     col='blue',
     xaxp=c(2010.0,2011.0,4),
     xlab='Year', ylab = 'Wh',
     xlim=c(2010,2011),
     main='4 Quarter Forecast of Energy Useage on Sub-Meter 3')
#minor.tick(nx=4)
#axis(side=1, at= c(1, 2,3,4,5,6,7,8,9,10,11,12, 13), labels=MonthLst)
legend('topleft', 'Sub-Meter-3', col='blue', lwd=2, bty='n')


#######################
# Month / day of month #
#######################

##-Sub-Meter-3
#-Decompose TS
mnth_decomp3 <- decompose(housePWR_mnthTS[,3])
autoplot(mnth_decomp3, labels=NULL, range.bars = TRUE) +
  xlab('Month') +
  ylab('kWh') +
  ggtitle('Decomposed Monthly Time Series- Sub-Meter-3')

#-remove seasonality
mnth_seasonAdj3 <- seasadj(mnth_decomp3)

#-Fit Holt Winters simple exponetial smoothing model
mnth_smooth3 <- HoltWinters(mnth_seasonAdj3, beta=FALSE, gamma=FALSE)
plot(mnth_smooth3, col='blue',
     #xaxt='n',
     xlab='Month', ylab = 'Total kWh',
     xlim=c(2007,2011),
     xaxp=c(2007,2011,4),
     main='Fitted Holt-Winters Model for Monthly Time Series')
minor.tick(nx=12)
legend('topleft', 'Sub-Meter-3', col='blue', lwd=2, bty='n')

#Forecast
mnth_smoothFcast3 <- forecast(mnth_smooth3, h=5, level=c(90,95))
mnth_smoothFcast3
plot(mnth_smoothFcast3,include=1, showgap=TRUE,
     #xaxt='n',
     col='blue',
     shadecols=c('slategray3','slategray'),
     #xaxp=c(2010.9,2011.2,3),
     xlab='Month', ylab = 'Wh',
     main='Four Month Forecast of Monthly Energy Useage on Sub-Meter 3')
#axis(side=1, at= c(2010.9,  2011.2), labels=c('0', '+4'))
legend('topleft', 'Sub-Meter-3', col='blue', lwd=2, bty='n')
summary(mnth_smoothFcast3)

acf(mnth_smoothFcast3$residuals, na.action = na.omit)


##################
#Week of the Year#
##################

##-Sub-Meter-3
#-Decompose TS
wkofYr_decomp3 <- decompose(housePWR_wkofYrTS[,3])
autoplot(wkofYr_decomp3, labels=NULL, range.bars = TRUE) +
  xlab('Week of the Year') +
  ylab('kWh') +
  ggtitle('Decomposed Weekly Time Series- Sub-Meter-2')

#-remove seasonality
wkofYr_seasonAdj3 <- seasadj(wkofYr_decomp3)
autoplot(wkofYr_seasonAdj3)

#-Fit Holt Winters simple exponetial smoothing model
wkofYr_smooth3 <- HoltWinters(wkofYr_seasonAdj3, beta=FALSE, gamma=FALSE)
plot(wkofYr_smooth3)

plot(wkofYr_smooth3, xaxt='n', col='blue',
     xaxp=c(1,8,7),
     xlab='Day of Week', ylab = 'Total kWh',
     #ylim=c(0,75),
     main='Fitted Holt-Winters Model for Daily Time Series')
axis(side=1, at= c(1, 2,3,4,5,6,7,8), labels=WkLst)
legend('topleft', 'Sub-Meter-2', col='green', lwd=2, bty='n')

#Forecast
wkofYr_smoothFcast3 <- forecast(wkofYr_smooth3, h=5, level=c(90, 95))
wkofYr_smoothFcast3
plot(wkofYr_smoothFcast3,
     include=1, showgap=TRUE,
     PI=TRUE,
     shadecols=c('slategray3','slategray'),
     #xaxt='n',
     fcol='blue',
     #xaxp=c(8,9,1),
     xlab='Day', ylab = 'kWh',
     # ylim=c(0,100),
     main='5-Week Forecast of Energy Usage on Sub-Meter 3')
axis(side=1, at= c(8, 9), labels=c('0', '1'))
legend('topleft', 'Sub-Meter-2', col='blue', lwd=2, bty='n')



#####################
# Day of Week
#####################


##-Sub-Meter-2
#-Decompose TS
dofW_decomp2 <- decompose(housePWR_dofWkTS[,2])
autoplot(dofW_decomp2, labels=NULL, range.bars = TRUE) +
  xlab('Day of Week') +
  ylab('kWh') +
  ggtitle('Decomposed Day of the Week Time Series- Sub-Meter-2')

#-remove seasonality
dofW_seasonAdj2 <- seasadj(dofW_decomp2)
autoplot(dofW_seasonAdj2)

#-Fit Holt Winters simple exponetial smoothing model
dofW_smooth2 <- HoltWinters(dofW_seasonAdj2, beta=FALSE, gamma=FALSE)
plot(dofW_smooth2)

plot(dofW_smooth2, col='green',
     #xaxp=c(1,8,7),
     xlab='Week of the Year', ylab = 'Wh',
     #ylim=c(0,75),
     main='Fitted Holt-Winters Model for Day of the Week Series')
#axis(side=1, at= c(1, 2,3,4,5,6,7,8), labels=WkLst)
minor.tick(nx=52)
legend('topleft', 'Sub-Meter-2', col='green', lwd=2, bty='n')

#Forecast
dofW_smoothFcast2 <- forecast(dofW_smooth2, h=14, level=c(90, 95))
dofW_smoothFcast2
plot(dofW_smoothFcast2,
     include=1,
     PI=TRUE, showgap=FALSE,
     shadecols=c('slategray3','slategray'),
     #xaxt='n',
     fcol='green',
     #xaxp=c(8,9,1),
     xlab='Week', ylab = 'Wh',
     #ylim=c(-0.5,3),
     main='14 Day Forecast of Energy Usage on Sub-Meter 2')
axis(side=1, at= c(8, 9), labels=c('0', '1'))
legend('topleft', 'Sub-Meter-2', col='green', lwd=2, bty='n')



##########################
# Hour of Day / 5_minute#
##########################

#-Sub Meter 3
#-Decompose TS
hofDay_decomp3 <- decompose(housePWR_hofDayTS[,3])
autoplot(hofDay_decomp3, labels=NULL, range.bars = TRUE) +
  xlab('Hour of Day') +
  ylab('kWh') +
  ggtitle('Decomposed Hourly Time Series- Sub-Meter-3')

#-remove seasonality
hofDay_seasonAdj3 <- seasadj(hofDay_decomp3)
autoplot(hofDay_seasonAdj3)

#-Fit Holt Winters simple exponetial smoothing model
hofDay_smooth3 <- HoltWinters(hofDay_seasonAdj3, beta=FALSE, gamma=FALSE)
plot(hofDay_smooth3)

plot(hofDay_smooth3, #xaxt='n',
     col='blue',
     #xaxp=c(1,8,7),
     xlab='Hour of Day', ylab = 'Total kWh',
     #ylim=c(0,75),
     main='Fitted Holt-Winters Model for Hourly Useage in a Day')
#axis(side=1, at= c(1, 2,3,4,5,6,7,8), labels=WkLst)
legend('topleft', 'Sub-Meter-3', col='blue', lwd=2, bty='n')

#Forecast
hofDay_smoothFcast3 <- forecast(hofDay_smooth3, h=150, level=c(90,95))
hofDay_smoothFcast3
plot(hofDay_smoothFcast3,
     include=0,
     PI=TRUE, showgap=FALSE,
     shadecols=c('slategray3','slategray'),
     #xaxt='n',
     fcol='blue',
     #xaxp=c(8,9,1),
     xlab='Hour', ylab = 'Total kWh',
     # ylim=c(0,100),
     main='24 hour Forecast Based on Hourly Energy Usage in a Day')
axis(side=1, at= c(8, 9), labels=c('0', '1'))
legend('topleft', 'Sub-Meter-3', col='blue', lwd=2, bty='n')



#################
# Weekend Hours #
#################
#Sub-meter-1
#-Decompose TS
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

#-Fit Holt Winters simple exponetial smoothing model
Wknd_smooth1 <- HoltWinters(Wknd_seasonAdj1, beta=FALSE, gamma=FALSE)
plot(Wknd_smooth1)

plot(Wknd_smooth1, xaxt='n', col='black',
     xaxp=c(1,3,1),
     xlab='Day of Week', ylab = 'Total kWh',
     #ylim=c(0,75),
     main='Fitted Holt-Winters Model for Weekend Time Series')
axis(side=1, at= c(1, 2,3), labels=WkndList)
legend('topleft', 'Sub-Meter-1', col='black', lwd=2, bty='n')

#Forecast
Wknd_smoothFcast1 <- forecast(Wknd_smooth1, h=48)
Wknd_smoothFcast1
plot(Wknd_smoothFcast1,
     include=1,
     PI=TRUE,
     xaxt='n',
     fcol='red',
     xaxp=c(3,5,1),
     xlab='Day', ylab = 'Total kWh',
     ylim=c(0,20),
     main='Forecast of Weekend Energy Usage on Sub-Meter 1')
axis(side=1, at= c(3, 4, 5), labels=c('Sat', 'Sun', 'Mon'))
legend('topleft', 'Sub-Meter-1', col='red', lwd=2, bty='n')













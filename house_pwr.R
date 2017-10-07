
# Description --------------------------------------------------------------------

# Rutgers Data Analytics / Big Data 2017
# Project 4 Deep Analytics and Visualization


# Summary -----------------------------------------------------------------





# Load Packages -----------------------------------------------------------


library(caret)      #R modeling workhorse
library(tidyverse)  #Package for tidying data
library(magrittr)   #Enables piping
library(lubridate)  #Simplifies working with dates/times of a time series
library(VIM)        #Aids visualization and imputing of missing values
library(Hmisc)      #for descriptive statistics
library(GGally)     #ggcorr provides pretty cor plot
library(scales)
library(forecast)   #forcasting package

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

house_pwrMtrs <- select(house_pwr, DateTime, `Sub-Meter-1`, `Sub-Meter-2`, `Sub-Meter-3`, `Engy_remain`) %>%
  group_by(year(DateTime), day(DateTime), month(DateTime), hour(DateTime), minute(DateTime))

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
  filter(year(DateTime) > 2006 ) %>%
  #filter(year(DateTime)<2010) %>%
  group_by(year(DateTime)) %>%
  summarise(Sub_Meter_1=round(sum(`Sub-Meter-1`/1000, na.rm=TRUE), 3),
            Sub_Meter_2=round(sum(`Sub-Meter-2`/1000, na.rm=TRUE), 3),
            Sub_Meter_3=round(sum(`Sub-Meter-3`/1000, na.rm=TRUE), 3),
            first_DateTime = first(DateTime))

#-Subset Semester
housePWR_semstr <- house_pwr %>%
  filter(year(DateTime) > 2006) %>%
  group_by(year(DateTime),semester(DateTime)) %>%
  summarise(Sub_Meter_1=round(sum(`Sub-Meter-1`/1000, na.rm=TRUE), 3),
            Sub_Meter_2=round(sum(`Sub-Meter-2`/1000, na.rm=TRUE), 3),
            Sub_Meter_3=round(sum(`Sub-Meter-3`/1000, na.rm=TRUE), 3),
            first_DateTime = first(DateTime))

#-Subset Quarter
housePWR_qtr <- house_pwr %>%
  filter(year(DateTime) > 2006) %>%
  #filter(year(DateTime)<2010) %>%
  group_by(year(DateTime), quarter(DateTime)) %>%
  summarise(Sub_Meter_1=round(sum(`Sub-Meter-1`/1000, na.rm=TRUE), 3),
            Sub_Meter_2=round(sum(`Sub-Meter-2`/1000, na.rm=TRUE), 3),
            Sub_Meter_3=round(sum(`Sub-Meter-3`/1000, na.rm=TRUE), 3),
            first_DateTime = first(DateTime))

#-Subset Month
housePWR_mnth <- house_pwr %>%
  #filter(year(DateTime) > 2006) %>%
  #filter(year(DateTime)<2011) %>%
  group_by(year(DateTime), month(DateTime)) %>%
  summarise(Sub_Meter_1=round(sum(`Sub-Meter-1`/1000, na.rm=TRUE), 3),
            Sub_Meter_2=round(sum(`Sub-Meter-2`/1000, na.rm=TRUE), 3),
            Sub_Meter_3=round(sum(`Sub-Meter-3`/1000, na.rm=TRUE), 3),
            first_DateTime = first(DateTime))

#-Subset Week of year
housePWR_wkofYr <- house_pwr %>%
  #filter(year(DateTime)>2006) %>%
  #filter(year(DateTime)<2010) %>%
  mutate(DofWk=lubridate::wday(DateTime, label=TRUE, abbr=TRUE)) %>%
  group_by(year(DateTime),week(DateTime)) %>%
  summarise(Sub_Meter_1=round(sum(`Sub-Meter-1`/1000, na.rm=TRUE), 3),
            Sub_Meter_2=round(sum(`Sub-Meter-2`/1000, na.rm=TRUE), 3),
            Sub_Meter_3=round(sum(`Sub-Meter-3`/1000, na.rm=TRUE), 3),
            first_DateTime = first(DateTime))

#-Subset by day of week
housePWR_dofWk <- house_pwr %>%
  filter(year(DateTime)>2006) %>%
  #filter(year(DateTime)<2010) %>%
  mutate(DofWk=lubridate::wday(DateTime, label=TRUE, abbr=TRUE)) %>%
  group_by(week(DateTime),DofWk) %>%
  summarise(Sub_Meter_1=round(sum(`Sub-Meter-1`/1000, na.rm=TRUE), 3),
            Sub_Meter_2=round(sum(`Sub-Meter-2`/1000, na.rm=TRUE), 3),
            Sub_Meter_3=round(sum(`Sub-Meter-3`/1000, na.rm=TRUE), 3),
            first_DateTime = first(DateTime))

#-Subset hour of day
housePWR_hofDay <- house_pwr %>%
  filter(year(DateTime)>2006) %>%
  #filter((minute(DateTime) %% 5) == 0) %>%
  group_by(wday(DateTime), hour(DateTime)) %>%
  summarise(Sub_Meter_1=round(sum(`Sub-Meter-1`/1000, na.rm=TRUE), 3),
            Sub_Meter_2=round(sum(`Sub-Meter-2`/1000, na.rm=TRUE), 3),
            Sub_Meter_3=round(sum(`Sub-Meter-3`/1000, na.rm=TRUE), 3),
            first_DateTime = first(DateTime))

#-Subset by Weekends
housePWR_wknd <- house_pwr %>%
  filter(year(DateTime)>2006) %>%
  #filter(year(DateTime)<2010) %>%
  mutate(Wknd=lubridate::wday(DateTime, label=TRUE, abbr=TRUE)) %>%
  filter(Wknd == c('Sat', 'Sun')) %>%
  group_by(Wknd, hour(DateTime)) %>%
  summarise(Sub_Meter_1=round(sum(`Sub-Meter-1`/1000, na.rm=TRUE), 3),
            Sub_Meter_2=round(sum(`Sub-Meter-2`/1000, na.rm=TRUE), 3),
            Sub_Meter_3=round(sum(`Sub-Meter-3`/1000, na.rm=TRUE), 3),
            first_DateTime = first(DateTime)) %>%
 arrange(desc(Wknd))


# Convert to Time Series --------------------------------------------------


# Year_TS
housePWR_yrTS <- ts(housePWR_yr[,2:4], frequency = 1, start=c(2007))
plot(housePWR_yrTS, plot.type='s', #xaxt='n',
     xaxp = c(2007, 2010, 3),
     col=c('red', 'green', 'blue'),
     main='Total Yearly kWh Consumption (2007-2010)',
     xlab='Year', ylab = 'kWh')
b <- c('Sub-meter-1', 'Sub-meter-2', 'Sub-meter-3')
legend('topleft', b, col=c('red', 'green', 'blue'), lwd=2, bty='n')

#Semester_TS
housePWR_semstrTS <- ts(housePWR_semstr[,3:5], frequency = 2, start=c(2007,1), end=c(2010,1))
plot(housePWR_semstrTS, plot.type='s', #xaxt='n',
     #xaxp = c(2007, 2010, 3),
     col=c('red', 'green', 'blue'),
     main='Total Energy Consumption by Semester',
     xlab='Year', ylab = 'kWh')
b <- c('Sub-meter-1', 'Sub-meter-2', 'Sub-meter-3')
legend('topleft', b, col=c('red', 'green', 'blue'), lwd=2, bty='n')



#Quarter_TS
housePWR_qtrTS <- ts(housePWR_qtr[,3:5], frequency=4, start=c(2007,1), end=c(2010,3))
plot(housePWR_qtrTS, plot.type='s',
     #xaxt='n',
     #xlim=c(2007, 2010),
     #xaxp=c(2006, 2011, 5),
     col=c('red', 'green', 'blue'),
     main='Total Quarterly kWh Consumption',
     xlab='Year', ylab = 'kWh')
minor.tick(nx=4)
b <- c('Sub-meter-1', 'Sub-meter-2', 'Sub-meter-3')
legend('topleft', b, col=c('red', 'green', 'blue'), lwd=2, bty='n')


#Month_TS
housePWR_mnthTS <- ts(housePWR_mnth[,3:5], frequency = 12, start=c(2007,1), end=c(2010,11))
plot(housePWR_mnthTS, plot.type='s',#xaxt='n',
     #xaxp = c(1,13,12),
     col=c('red', 'green', 'blue'),
     main='Average Monthly Wh Consumption',
     xlab='Year/Month', ylab = 'kWh')
minor.tick(nx=12)
b <- c('Sub-meter-1', 'Sub-meter-2', 'Sub-meter-3')
legend('topleft', b, col=c('red', 'green', 'blue'), lwd=2, bty='n')


# Week of the year_TS
housePWR_wkofYrTS <- ts(housePWR_wkofYr[,3:5], frequency =53, start=c(2006,51), end=c(2010,47))
plot(housePWR_wkofYrTS, plot.type='s', #xaxt='n',
     #xaxp = c(1, 13, 12),
     col=c('red', 'green', 'blue'),
     xlab ='Year', ylab = 'kWh',
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
     xlab ='Week of Year', ylab = 'kWh',
     #xlim = c(1,53) , ylim=c(0,75),
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
     xlab='Weekend Day', ylab = 'kWh',
     ylim=c(0,90),
     main='Total Weekend Energy Consumption by Hour')
axis(side = 1, at = c(1,2,3), labels = WkndList)
minor.tick(nx=24)
b <- c('Sub-meter-1', 'Sub-meter-2', 'Sub-meter-3')
legend('topleft', b, col=c('red', 'green', 'blue'), lwd=2, bty='n')


# LM Forecasting -------------------------------------------------------------


#Quarter_forecast
fit1 <- tslm(housePWR_qtrTS[,3] ~ trend + season)
x <- forecast(fit1, h=4, level=c(90,95))
plot(x, showgap=FALSE, include=3,
     shadecols=c('slategray3','slategray'),
     xlab='Year', ylab='kWh',
     main='4-Quarter Forecast of Quartlerly Energy Consumption for Submeter-3')
minor.tick(nx=2)
summary(x)
summary(fit1)
#Forecasts:
#  Forecasts:
#        Point Forecast    Lo 90     Hi 90    Lo 95    Hi 95
#2010 Q4       1083.575 893.8830 1273.2680 850.3781 1316.773
#2011 Q1       1131.790 939.1564 1324.4236 894.9770 1368.603
#2011 Q2       1018.404 825.7699 1211.0371 781.5905 1255.217
#2011 Q3        797.055 604.4214  989.6886 560.2420 1033.868

# Month_forecast
fit2 <- tslm(housePWR_mnthTS[,3] ~ trend + season)
y <- forecast(fit2,h=12, level=c(90,95))
plot(y, showgap=FALSE, include=12,
  shadecols=c('slategray3','slategray'),
  xlab ='Year',
  ylab=' kWh',
  main='12-Month Forecast of Monthly Energy Consumption')
minor.tick(nx=6)
summary(y)
summary(fit2)

#Forecasts:
#         Point Forecast    Lo 90    Hi 90    Lo 95    Hi 95
#Dec 2010       358.6505 273.0795 444.2215 255.8066 461.4944
#Jan 2011       370.8729 286.6495 455.0963 269.6487 472.0971
#Feb 2011       408.5337 324.3103 492.7570 307.3095 509.7579
#Mar 2011       375.2337 291.0103 459.4570 274.0095 476.4579
#Apr 2011       372.4437 288.2203 456.6670 271.2195 473.6679
#May 2011       348.9937 264.7703 433.2170 247.7695 450.2179
#Jun 2011       365.6509 281.4275 449.8743 264.4267 466.8751
#Jul 2011       328.1799 243.9565 412.4033 226.9557 429.4041
#Aug 2011       257.4384 173.2150 341.6618 156.2142 358.6626
#Sep 2011       231.0557 146.8323 315.2790 129.8315 332.2799
#Oct 2011       332.9819 248.7585 417.2053 231.7577 434.2061
#Nov 2011       360.5009 276.2775 444.7243 259.2767 461.7251


#Week of year_forecast
fit3 <- tslm(housePWR_wkofYrTS[,3] ~ trend + season)
z <- forecast(fit3, level=c(90,95), h=24)
plot(z, showgap=FALSE, include=10,
     shadecols=c('slategray3','slategray'),
     xlab ='Year',
     ylab='kWh',
     main='24-Week Forecast of Weekly Energy Consumption on Submeter-3')
summary(z)
summary(fit3)

#Forecasts:
#  Point Forecast    Lo 90     Hi 90     Lo 95     Hi 95
#2010.887       88.80317 64.90720 112.69914 60.276829 117.32951
#2010.906       87.32184 63.42587 111.21780 58.795496 115.84818
#2010.925       85.38350 61.48754 109.27947 56.857162 113.90984
#2010.943       93.45354 70.22721 116.67988 65.726597 121.18049
#2010.962       88.76804 65.54171 111.99438 61.041097 116.49499
#2010.981       37.39754 14.17121  60.62388  9.670597  65.12449
#2011.000       65.34304 42.11671  88.56938 37.616097  93.06999



# Decompose Time Series/ Remove Seasonality/ HW Smoothing/Predict -------------------------------------------------------------

# Remove Seasonality ------------------------------------------------------




##########
#Semester#
##########

##-Sub-Meter-3
#-Decompose TS
semstr_decomp3 <- decompose(housePWR_semstrTS[,3])
autoplot(semstr_decomp3,  range.bars = TRUE) +
  xlab('Year') +
  ylab('kWh') +
  ggtitle('Decomposed Semester Time Series- Sub-Meter-3')
semstr_decomp3
summary(semstr_decomp3$seasonal)
summary(semstr_decomp3$trend)
summary(semstr_decomp3$random)

#-remove seasonality
smstr_seasonAdj3 <- seasadj(semstr_decomp3)
acf(smstr_seasonAdj3, na.action=na.omit,lag=30)

plot(smstr_seasonAdj3, #xaxt='n',
     col='blue',
     xlab='Year', ylab='kWh',
     #xlim=c(2007, 2010),
     #xaxp=c(2006, 2010, 4),
     main='Seasonally Adjusted Semester Time Series')
minor.tick(nx=2)
b <-'Sub-meter-3'
legend('topleft', b, col='blue', lwd=2, bty='n')

#-Fit Holt Winters simple exponetial smoothing model
smstr_smooth3 <- HoltWinters(smstr_seasonAdj3, beta=FALSE, gamma=FALSE)
plot(smstr_smooth3, col='blue', #xaxt='n',
     xlab='Year', ylab = 'kWh',
     xlim=c(2007, 2010),
     #xaxp=c(2006, 2010, 4),
     main='Simple Exponential Smoothing Holt-Winters Model for Semester Time Series')
legend('topleft', 'Sub-Meter-3', col='blue', lwd=2, bty='n')

#-Forecast
smstr_smoothFcast3 <- forecast(smstr_smooth3, h=5,level = c(90,95))
smstr_smoothFcast3
summary(smstr_smoothFcast3)

plot(smstr_smoothFcast3, include=1, showgap=TRUE,
     #xaxt='n',
     shadecols=c('slategray3','slategray'),
     col='blue',
     #xaxp=c(2010.6,2011.5,4),
     xlab='Year', ylab = 'Wh',
     #xlim=c(2010,2011),
     main='5 Semester Forecast of Energy Useage on Sub-Meter 3')
#minor.tick(nx=4)
#axis(side=1, at= c(1, 2,3,4,5,6,7,8,9,10,11,12, 13), labels=MonthLst)
legend('topleft', 'Sub-Meter-3', col='blue', lwd=2, bty='n')

#Forecasts:
#        Point Forecast    Lo 90    Hi 90    Lo 95    Hi 95
#2010.50       2023.736 1827.709 2219.763 1790.156 2257.317
#2011.00       2023.736 1746.521 2300.951 1693.414 2354.059
#2011.50       2023.736 1684.221 2363.251 1619.180 2428.293
#2012.00       2023.736 1631.700 2415.772 1556.597 2490.875
#2012.50       2023.736 1585.428 2462.044 1501.460 2546.012


########
#Quarter#
########

##-Sub-Meter-3
#-Decompose TS
qtr_decomp3 <- decompose(housePWR_qtrTS[,3])
autoplot(qtr_decomp3,  range.bars = TRUE) +
  xlab('Year') +
  ylab('kWh') +
  ggtitle('Decomposed Quarterly Time Series- Sub-Meter-3')
qtr_decomp3
summary(qtr_decomp3$seasonal)
summary(qtr_decomp3$trend)
summary(qtr_decomp3$random)

#-remove seasonality
qtr_seasonAdj3 <- seasadj(qtr_decomp3)
acf(qtr_seasonAdj3, na.action=na.omit,lag=30)

plot(qtr_seasonAdj3, #xaxt='n',
     col='blue',
     xlab='Year', ylab='kWh',
     #xlim=c(2007, 2010),
     xaxp=c(2006, 2010, 4),
     main='Seasonally Adjusted Quarterly Time Series')
minor.tick(nx=2)
b <-'Sub-meter-3'
legend('topleft', b, col='blue', lwd=2, bty='n')

#-Fit Holt Winters simple exponetial smoothing model
qtr_smooth3 <- HoltWinters(qtr_seasonAdj3, beta=FALSE, gamma=FALSE)
plot(qtr_smooth3, col='blue', #xaxt='n',
     xlab='Year', ylab = 'kWh',
     xlim=c(2007, 2011),
     xaxp=c(2006, 2010, 4),
     main='Simple Exponential Smoothing Holt-Winters Model for Quarterly Time Series')
minor.tick(nx=4)
legend('topleft', 'Sub-Meter-3', col='blue', lwd=2, bty='n')

#-Forecast
qtr_smoothFcast3 <- forecast(qtr_smooth3, h=5,level = c(90,95))
qtr_smoothFcast3
summary(qtr_smoothFcast3)

plot(qtr_smoothFcast3, include=1,
     #xaxt='n',
     shadecols=c('slategray3','slategray'),
     col='blue',
     #xaxp=c(2010.6,2011.5,4),
     xlab='Year', ylab = 'Wh',
     #xlim=c(2010,2011),
     main='5 Quarter Forecast of Energy Useage on Sub-Meter 3')
#minor.tick(nx=4)
#axis(side=1, at= c(1, 2,3,4,5,6,7,8,9,10,11,12, 13), labels=MonthLst)
legend('topleft', 'Sub-Meter-3', col='blue', lwd=2, bty='n')

#Forecasts:
#        Point Forecast    Lo 90    Hi 90    Lo 95    Hi 95
#2010 Q4       906.5673 744.7559 1068.379 713.7571 1099.378
#2011 Q1       906.5673 733.7630 1079.372 700.6583 1112.476
#2011 Q2       906.5673 723.4288 1089.706 688.3443 1124.790
#2011 Q3       906.5673 713.6473 1099.487 676.6890 1136.446
#2011 Q4       906.5673 704.3385 1108.796 665.5968 1147.538


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

summary(mnth_decomp3$seasonal)
summary(mnth_decomp3$trend)
summary(mnth_decomp3$random)


#-remove seasonality
mnth_seasonAdj3 <- seasadj(mnth_decomp3)

#-Fit Holt Winters simple exponetial smoothing model
mnth_smooth3 <- HoltWinters(mnth_seasonAdj3, beta=FALSE, gamma=FALSE)
plot(mnth_smooth3, col='blue',
     #xaxt='n',
     xlab='Month', ylab = 'Total kWh',
     #xlim=c(2007,2011),
     #xaxp=c(2007,2011,4),
     main='Fitted Holt-Winters Model for Monthly Time Series')
minor.tick(nx=12)
legend('topleft', 'Sub-Meter-3', col='blue', lwd=2, bty='n')

#Forecast
mnth_smoothFcast3 <- forecast(mnth_smooth3, h=6, level=c(90,95))
mnth_smoothFcast3
plot(mnth_smoothFcast3,include=1, showgap=TRUE,
     #xaxt='n',
     col='blue',
     shadecols=c('slategray3','slategray'),
     #xaxp=c(2010.9,2011.2,3),
     xlab='Year', ylab = 'kWh',
     main='Five Month Forecast of Monthly Energy Useage on Sub-Meter 3')
#axis(side=1, at= c(2010.9,  2011.2), labels=c('0', '+4'))
legend('topleft', 'Sub-Meter-3', col='blue', lwd=2, bty='n')
summary(mnth_smoothFcast3)

#Forecasts:
#         Point Forecast    Lo 90    Hi 90    Lo 95    Hi 95
#Dec 2010       294.6373 220.1586 369.1161 205.8904 383.3842
#Jan 2011       294.6373 212.1643 377.1103 196.3647 392.9100
#Feb 2011       294.6373 204.8793 384.3954 187.6840 401.5906
#Mar 2011       294.6373 198.1427 391.1320 179.6568 409.6178
#Apr 2011       294.6373 191.8466 397.4280 172.1546 417.1200


##################
#Week of the Year#
##################

##-Sub-Meter-3
#-Decompose TS
wkofYr_decomp3 <- decompose(housePWR_wkofYrTS[,3])
autoplot(wkofYr_decomp3, labels=NULL, range.bars = TRUE) +
  xlab('Week of the Year') +
  ylab('kWh') +
  ggtitle('Decomposed Weekly Time Series- Sub-Meter-3')

summary(wkofYr_decomp3$seasonal)
summary(wkofYr_decomp3$trend)
summary(wkofYr_decomp3$random)



#-remove seasonality
wkofYr_seasonAdj3 <- seasadj(wkofYr_decomp3)
autoplot(wkofYr_seasonAdj3)


#-Fit Holt Winters simple exponetial smoothing model
wkofYr_smooth3 <- HoltWinters(wkofYr_seasonAdj3, beta=FALSE, gamma=FALSE)
plot(wkofYr_smooth3, col='blue',
     #xaxt='n',
     #xaxp=c(1,8,7),
     xlab='Year', ylab = 'kWh',
     #ylim=c(0,75),
     main='Fitted Holt-Winters Model for Week of the Year Time Series')
minor.tick(nx=52)
legend('topleft', 'Sub-Meter-3', col='blue', lwd=2, bty='n')

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
     xlab='Year', ylab = 'kWh',
     # ylim=c(0,100),
     main='5-Week Forecast of Weekly Energy Consumption on Sub-Meter 3')
axis(side=1, at= c(8, 9), labels=c('0', '1'))
legend('topleft', 'Sub-Meter-3', col='blue', lwd=2, bty='n')

#         Point Forecast    Lo 90    Hi 90    Lo 95    Hi 95
#2010.887       63.46413 45.08019 81.84808 41.55831 85.36995
#2010.906       63.46413 44.57299 82.35528 40.95395 85.97432
#2010.925       63.46413 44.07905 82.84921 40.36539 86.56288
#2010.943       63.46413 43.59740 83.33087 39.79146 87.13681
#2010.962       63.46413 43.12714 83.80112 39.23112 87.69715

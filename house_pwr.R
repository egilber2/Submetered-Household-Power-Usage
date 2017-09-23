
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

# Parallel Processing -----------------------------------------------------

cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

stopCluster(cluster)
registerDoSEQ()

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

# Assess missing values
aggr_plot <- aggr(house_pwr, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(house_pwr),cex.axis=.7,
  gap=3, ylab=c("Histogram of missing data","Pattern"), digits=2)

# Remove rows with NA's
house_pwr <- na.omit(house_pwr)
sum(is.na(house_pwr))


# Add feature representing remaining active energy consumed every minute (watt hour)
house_pwr9v <- house_pwr %>%
  mutate(Engy_remain=(Glbl_actvPwr*1000/60)-
  Sub_metering_1 - Sub_metering_2 - Sub_metering_3)

as_tibble(house_pwr9v)
str(house_pwr9v)

# Create tidy tibble
house_pwr_tidy <- house_pwr9v %>%
  gather(Meter, Watt_hr, Sub_metering_1, Sub_metering_2, Sub_metering_3)

house_pwr_tidy %>% as_tibble(house_pwr_tidy)
is_tibble(house_pwr_tidy)

house_pwr_tidy$Meter <- factor(house_pwr_tidy$Meter)
glimpse(house_pwr_tidy)


# Exploratory Data Analysis -----------------------------------------------

str(house_pwr_tidy)

summary(house_pwr_tidy)


#house_pwrMtrs <- select(house_pwr9v, DateTime, Sub_metering_1, Sub_metering_2, Sub_metering_3, Engy_remain) %>%
#  group_by(year(DateTime), day(DateTime), month(DateTime), hour(DateTime), minute(DateTime))



# plot of average HOUR_OF_DAY useage
house_pwr_tidy %>%
  group_by(Meter, hour(DateTime)) %>%
  summarise(avg=mean(Watt_hr), DateTime=first(DateTime)) %>%
  ggplot(aes(x=factor(`hour(DateTime)`), group=Meter ,avg, colour=Meter)) +
  labs(x='Hour of the Day', y='Avg Watt Hour Useage') +
  ggtitle('Average Hourly Watt Hour Useage') +
  geom_line(size=1) +
  geom_line() +
  geom_point()

#house_pwr_tidy %>%
  group_by(hour(DateTime), Meter) %>%
  summarise(avg=mean(Watt_hr)) %>%
  ggplot(aes(x=factor(`hour(DateTime)`), y=avg, colour=Meter)) +
  labs(x='Hour of the Day', y='Avg Watt Hour Useage') +
  ggtitle('Average Hourly Watt Hour Useage') +
  geom_point()

# plot of average DAY_OF_MONTH useage
#Not informative
house_pwr_tidy %>%
  group_by(day(DateTime), Meter) %>%
  summarise(avg=mean(Watt_hr)) %>%
  ggplot(aes(x=factor(`day(DateTime)`), avg, group=Meter,colour=Meter)) +
  labs(x='Day of the Month', y='Avg Watt Hour Useage') +
  ggtitle('Average Daily Watt Hour Useage') +
  geom_line(size=1) +
  geom_line()


# plot of average DAY_OF_WEEK useage
house_pwr_tidy %>%
  #mutate(wday=wday(DateTime, label=TRUE)) %>%
  group_by(wday(DateTime), Meter) %>%
  summarise(avg=mean(Watt_hr)) %>%
  ggplot(aes(x=factor(`wday(DateTime)`), avg, group=Meter,color=Meter)) +
  labs(x='Day of the Week', y='Avg Watt Hour Useage') +
  ggtitle('Average Daily Watt Hour Useage') +
  geom_line(size=1) +
  geom_line()

# plot of proportional use across zones
##-HOUR
house_pwr_tidy %>%
  filter(year(DateTime)==2007 | year(DateTime)==2008 | year(DateTime)==2009) %>%
  group_by(hour(DateTime), Meter) %>%
  summarise(avg=mean(Watt_hr)) %>%
  ggplot(aes(x=factor(`hour(DateTime)`), avg, group=Meter,fill=Meter)) +
  labs(x='Hour of the Day', y='Proportion of Energy Useage') +
  ggtitle('Avg. Hourly Sub-Metered Energy Useage (2007-2009)') +
  geom_bar(stat='identity', position='fill', color='black')

##-Day
house_pwr_tidy %>%
  group_by(wday(DateTime), Meter) %>%
  filter(year(DateTime)==2007 | year(DateTime)==2008 | year(DateTime)==2009) %>%
  summarise(avg=mean(Watt_hr)) %>%
  ggplot(aes(x=factor(`wday(DateTime)`), avg, group=Meter,fill=Meter)) +
  labs(x='Day of the Week', y='Proportion of Energy Useage') +
  ggtitle('Avg Daily Sub-Metered  Energy Useage (2007-2009)') +
  geom_bar(stat='identity', position='fill', color='black')

##-DayII
house_pwr_tidy %>%
  filter(year(DateTime)>2006) %>%
  group_by(wday(DateTime), Meter) %>%
  summarise(avg=mean(Watt_hr)) %>%
  ggplot(aes(x=factor(Meter), avg, fill= factor(`wday(DateTime)`,
  labels = c('Sun', 'Mon', 'Tues', 'Wed', 'Thurs', 'Fri', 'Sat')))) +
  labs(x='Sub-Meter', y='Proportion of Energy Useage') +
  ggtitle('Proportion of Energy Consumption by Sub-Meter and Day of the Week') +
  scale_fill_brewer(palette='Set3') +
  labs(fill='Day of the Week') +
  geom_col(position='fill', color='black')

##-Month
house_pwr_tidy %>%
  filter(year(DateTime)==2007 | year(DateTime)==2008 | year(DateTime)==2009) %>%
  group_by(month(DateTime), Meter) %>%
  summarise(avg=mean(Watt_hr)) %>%
  ggplot(aes(x=factor(`month(DateTime)`), avg, group=Meter,fill=Meter)) +
  labs(x='Month of the Year', y='Proportion of Energy Useage') +
  ggtitle('Metered Monthly Energy Useage') +
  geom_bar(stat='identity', position='fill', color='black')

##-Year
house_pwr_tidy %>%
  filter(year(DateTime)==2007 | year(DateTime)==2008 | year(DateTime)==2009) %>%
  group_by(year(DateTime), Meter) %>%
  summarise(avg=mean(Watt_hr)) %>%
  ggplot(aes(x=factor(`year(DateTime)`), avg, group=Meter,fill=Meter)) +
  labs(x='Year', y='Proportion of Energy Useage') +
  ggtitle('Avg. Yearly Sub-Metered Energy Useage (2007-2009)') +
  geom_bar(stat='identity', position='fill', color='black')


# plot of average MONTHLY
house_pwr_tidy %>%
  group_by(month(DateTime), Meter) %>%
  summarise(avg=mean(Watt_hr)) %>%
  ggplot(aes(x=factor(`month(DateTime)`), avg, group=Meter,colour=Meter)) +
  labs(x='Month', y='Avg Watt Hour Useage') +
  ggtitle('Average Monthly Watt Hour Useage') +
  geom_line(size=1) +
  geom_line()

# plot of MAX_HOUR_OF_DAY useage
house_pwr_tidy %>%
  group_by(hour(DateTime), Meter) %>%
  summarise(max=max(Watt_hr)) %>%
  ggplot(aes(x=factor(`hour(DateTime)`), max, group=Meter,colour=Meter)) +
  geom_line()

# plot of MAX_MONTH_OF_YEAR useage
house_pwr_tidy %>%
  group_by(month(DateTime), Meter) %>%
  summarise(max=max(Watt_hr)) %>%
  ggplot(aes(x=factor(`month(DateTime)`), max, group=Meter,colour=Meter)) +
  geom_line()

# plot AVG_HOUR_OF_DAY across YEARS- facet
house_pwr_tidy %>%
  filter(year(DateTime)>2006) %>%
  #filter(month(DateTime)==7) %>%
  group_by(year(DateTime), hour(DateTime), Meter) %>%
  summarise(avg=mean(Watt_hr)) %>%
  ggplot(aes(x=factor(`hour(DateTime)`), avg, group=Meter,colour=Meter)) +
  geom_line() +
  labs(x='Hour of the Day', y='Avg Watt Hour Useage') +
  ggtitle('Average Hourly Watt Hour Useage for Years 2007-2010') +
  geom_line(size=1)+
  facet_grid(`year(DateTime)` ~.)



# plot MAX Watt_hr useage across years- facet
house_pwr_tidy %>%
  filter(year(DateTime)>2006) %>%
  group_by(year(DateTime), hour(DateTime), Meter) %>%
  summarise(max=max(Watt_hr)) %>%
  ggplot(aes(x=factor(`hour(DateTime)`), max, group=Meter,colour=Meter)) +
  labs(x='Hour of the Day', y='Max Watt Hour Useage') +
  ggtitle('Max Hourly Watt Hour Useage for Years 2007-2010') +
  geom_line(size=1) +
  facet_grid(`year(DateTime)` ~.)




# Energy consumpton total_ breakdown over years -for days of month
#HISTOGRAM
#no obvious trends observed
house_pwr_tidy %>%
  filter(year(DateTime)>2006) %>%
  group_by(year(DateTime), month(DateTime),day(DateTime), hour(DateTime), minute(DateTime),Meter) %>%
  summarise(avg=mean(Watt_hr)/1000) %>%
  ggplot(aes(x=`year(DateTime)`, avg, fill=Meter)) +
  geom_histogram(stat='identity') +
  labs(title='Monitored Energy Consumption as Part of Total',
       x='Year',
       y='Avg kWh Consumption') +
  theme(axis.text.x= element_text(face='bold', size=14)) +
  theme(axis.text.y= element_text(face='bold', size=14)) +
    theme(title = element_text(size = 18),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 20))


# yearly kwatt useage
house_pwr_tidy %>%
  group_by(year(DateTime), Meter) %>%
  summarise(avg=mean(Watt_hr)) %>%
  ggplot(aes(x=factor(`year(DateTime)`), avg, group=Meter, colour=Meter)) +
  geom_line()

# Correlation plot
ggcorr(house_pwr) +
  ggtitle('Correlation Plot of Energy Consumption Dataset')

ggpairs(house_pwr,
        columns = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
        upper = list(continuous = wrap("cor", size = 10, alpha=0.5)),
        lower = list(continuous = "smooth"))

# Subset ------------------------------------------------------------------

# Subset by Year and Month
housePWR_yr <- house_pwr %>%
  filter(year(DateTime)>2006) %>%
  group_by(year(DateTime), month(DateTime)) %>%
  summarise(Sub_Meter_1=round(sum(Sub_metering_1/1000), 3),
            Sub_Meter_2=round(sum(Sub_metering_2/1000), 3),
            Sub_Meter_3=round(sum(Sub_metering_3/1000),3),
            first_DateTime = first(DateTime))


# Subset by Month and Day of Week
housePWR_mnth <- house_pwr %>%
  filter(year(DateTime)>2006) %>%
  group_by(month(DateTime), wday(DateTime)) %>%
  summarise(Sub_Meter_1=round(sum(Sub_metering_1/1000),3),
            Sub_Meter_2=round(sum(Sub_metering_2/1000),3),
            Sub_Meter_3=round(sum(Sub_metering_3/1000),3),
            first_DateTime = first(DateTime))


# Subset by Day of Week and hour of day
housePWR_dofWk <- house_pwr %>%
  filter(year(DateTime)>2006) %>%
  group_by(wday(DateTime), hour(DateTime)) %>%
  summarise(Sub_Meter_1=round(sum(Sub_metering_1/1000),3),
            Sub_Meter_2=round(sum(Sub_metering_2/1000),3),
            Sub_Meter_3=round(sum(Sub_metering_3/1000),3),
            first_DateTime = first(DateTime))


# Subset hour of day
housePWR_hofDay <- house_pwr %>%
  filter(year(DateTime)>2006) %>%
  filter(minute(DateTime)==00 | minute(DateTime)==15 | minute(DateTime)==30 | minute(DateTime)==45) %>%
  group_by(hour(DateTime), minute(DateTime)) %>%
  summarise(Sub_Meter_1=round(sum(Sub_metering_1/1000),3),
            Sub_Meter_2=round(sum(Sub_metering_2/1000),3),
            Sub_Meter_3=round(sum(Sub_metering_3/1000),3),
            first_DateTime = first(DateTime))

# Subset by Weekends
housePWR_wknd <- house_pwr %>%
  filter(year(DateTime)>2006) %>%
  filter(day(DateTime)==1 | day(DateTime)==7) %>%
  group_by(year(DateTime), day(DateTime), hour(DateTime)) %>%
  summarise(Sub_Meter_1=round(sum(Sub_metering_1/1000),3),
            Sub_Meter_2=round(sum(Sub_metering_2/1000),3),
            Sub_Meter_3=round(sum(Sub_metering_3/1000),3),
            first_DateTime = first(DateTime))


# Convert to Time Series --------------------------------------------------

# Year/month
housePWR_yrTS <- ts(housePWR_yr[,3:5], frequency = 12, start = c(2007,1), end=c(2010,11))
plot(housePWR_yrTS, plot.type='s',
     #xaxp = c(2007, 2010, 3),
     col=c('red', 'green', 'blue'),
     main='Total Yearly Kwh Consumption (2007-2010)',
     xlab='Year', ylab = 'Total kWh')
minor.tick(nx=12)
b <- c('Sub-meter-1', 'Sub-meter-2', 'Sub-meter-3')
legend('topleft', b, col=c('red', 'green', 'blue'), lwd=2, bty='n')


# Month/Day of Week
housePWR_mnthTS <- ts(housePWR_mnth[,3:5], frequency = 7, start= 1, end=12)
plot(housePWR_mnthTS, plot.type='s',
        xaxp = c(1, 12, 11),
        col=c('red', 'green', 'blue'),
        xlab='Month', ylab = 'Total kWh',
        main='Total Monthly kWh Consumption (2007-2010)')
minor.tick(nx=14)
b <- c('Sub-meter-1', 'Sub-meter-2', 'Sub-meter-3')
legend('top', b, col=c('red', 'green', 'blue'), lwd=2, bty='n')

# Day of Week / Hour
housePWR_dofWkTS <- ts(housePWR_dofWk[,3:5], frequency=23, start = 1, end=7)
plot(housePWR_dofWkTS, plot.type='s',
     xaxp = c(1, 7, 6),
     col=c('red', 'green', 'blue'),
     xlab='Day of Week', ylab = 'Total kWh',
     main='Total kWh Consumption by Day of the Week (2007-2010)')
minor.tick(nx=24)
b <- c('Sub-meter-1', 'Sub-meter-2', 'Sub-meter-3')
legend('topleft', b, col=c('red', 'green', 'blue'), lwd=2, bty='n')


# Hour of Day / 15_Minute
housePWR_hofDayTS <- ts(housePWR_hofDay[,3:5], frequency=4, start=0, end=23)
plot(housePWR_hofDayTS, plot.type='s',
     xaxp = c(0, 23, 23),
     col=c('red', 'green', 'blue'),
     xlab='Hour of the Day', ylab = 'Total kWh',
     main='Total kWh Consumption by Hour of the Day (2007-2010)')
b <- c('Sub-meter-1', 'Sub-meter-2', 'Sub-meter-3')
legend('topleft', b, col=c('red', 'green', 'blue'), lwd=2, bty='n')

# Weekend hourly use
housePWR_wkndTS <- ts(housePWR_wknd, frequency = 23, start=0, end=7)
plot(housePWR_wkndTS, plot.type='s',
     #xaxp = c(0, 23, 23),
     col=c('red', 'green', 'blue'),
     xlab='Hour of the Day', ylab = 'Total kWh',
     main='Total kWh Consumption by Hour of the Day (2007-2010)')
b <- c('Sub-meter-1', 'Sub-meter-2', 'Sub-meter-3')
legend('topleft', b, col=c('red', 'green', 'blue'), lwd=2, bty='n')

# Forecasting Trend-------------------------------------------------------------

# Year/month
fit1 <- tslm(housePWR_yrTS ~ trend)
x <- forecast(fit1, h=6, level = c(80, 95))
autoplot(x, PI=TRUE, colour = TRUE) +
  xlab('Year') +
  ylab('Total kWh') +
  ggtitle('Forecasted Yearly Trend for Energy Consumption')
summary(fit1)
fit1
x

# Month/Day of Week
fit2 <- tslm(housePWR_mnthTS ~ trend)
y <- forecast(fit2, h=10)
autoplot(y, PI=TRUE, colour=TRUE) +
  xlab('Month') +
  ylab('Total kWh') +
  ggtitle('Forecasted Monthly Trend of Energy Consumption')
summary(fit2)
y

# Day of Week / Hour
fit3 <- tslm(housePWR_dofWkTS ~ trend)
z <- forecast(fit3, h=24)
autoplot(z, PI=TRUE, colour=TRUE) +
  xlab('Day of Week') +
  ylab('Total kWh') +
  ggtitle('Forecasted Daily Trend of Energy Consumption')
summary.lm(fit3)
z

# Hour of Day / Minute
fit4 <- tslm(housePWR_hofDayTS ~ trend)
w <- forecast(fit4, h=20)
autoplot(w, PI=TRUE, colour=TRUE) +
  xlab('Hour of Day') +
  ylab('Total kWh') +
  ggtitle('Forecast Energy Consumption')
summary(w)
w

# Decompose Time Series' -------------------------------------------------------------

##############
# Year/Month #
##############

yr_decomp <- decompose(housePWR_yrTS)
plot(yr_decomp)
summary(yr_decomp)
yr_decomp

autoplot(yr_decomp$seasonal) +
  xlab('Year') +
  ylab('kWh') +
  ggtitle('Seasonal Component Yearly Time Series')
summary(yr_decomp$seasonal)

plot(yr_decomp$trend, xlab='Year', ylab='kWh',
     main='Trend Component for Yearly Time Series')
summary(yr_decomp$trend)

plot(yr_decomp$random, xlab='Year',
     main='Random Component of Yearly Time Series')
summary(yr_decomp$random)

summary(yr_decomp)
yr_decomp

#######################
# Month / Day of Week #
#######################

mnth_decomp <- decompose(housePWR_mnthTS)
plot(mnth_decomp)

plot(mnth_decomp$seasonal, xlab='Month', ylab='kWh',
     main='Seasonal Component for Monthly Time Series')

plot(mnth_decomp$trend, xlab='Month', ylab='kWh',
     main='Trend Component for Monthly Time Series')

plot(mnth_decomp$random, xlab='Month',
     main='Random Component of Monthlhy Time Series')

summary(mnth_decomp)
mnth_decomp

#####################
# Day of Week / Hour#
#####################

dofW_decomp <- decompose(housePWR_dofWkTS)
plot(dofW_decomp)

plot(dofW_decomp$seasonal, xlab='Day of Week', ylab='kWh',
     main='Seasonal Component of Daily Time Series')

plot(dofW_decomp$trend, xlab='Day of Week', ylab='kWh',
     main='Trend Component of Daily Time Series')

plot(dofW_decomp$random, xlab='Day of Week',
     main='Random Component of Daily Time Series')

summary(dofW_decomp)

##########################
# Hour of Day / 15_minute#
##########################

hofDay_decomp <- decompose(housePWR_hofDayTS)
plot(hofDay_decomp)

plot(hofDay_decomp$seasonal,xlab='Hour of Day', ylab='kWh',
     main='Seasonal Component of Hourly Time Series')

plot(hofDay_decomp$trend, xlab='Hour of Day', ylab='kWh',
     main='Trend Component of Hourly Time Series')

plot(hofDay_decomp$random, xlab='Hour of Day', ylab='kWh',
     main='Random Component of Hourly Time Series')

summary(hofDay_decomp)
hofDay_decomp


# Holt-Winters Smooting------------------------------------------------------------

##########
# Yearly #
##########

#sub-meter-1

# Remove seasonal component
yr_seasonAdj <- housePWR_yrTS - yr_decomp$seasonal
plot(yr_seasonAdj, plot.type='s',
     xaxp = c(2007, 2011, 4),
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

#sub-meter-2
yr_smooth2 <- HoltWinters(yr_seasonAdj[,2], beta=FALSE, gamma=FALSE)
plot(yr_smooth2)

yr_smoothFcast2 <- forecast(yr_smooth2)
autoplot(yr_smoothFcast2)

#sub-meter-3
yr_smooth3 <- HoltWinters(yr_seasonAdj[,3], beta=FALSE, gamma=FALSE)
plot(yr_smooth3)

yr_smoothFcast3 <- forecast(yr_smooth3)
autoplot(yr_smoothFcast3)

#######################
# Month / Day of Week #
#######################


# Remove seasonal component

mnth_seasonAdj <- housePWR_mnthTS - mnth_decomp$seasonal
plot(mnth_seasonAdj, plot.type='s',
     xaxp = c(1, 12, 11),
     col=c('red', 'green', 'blue'),
         xlab='Month', ylab='kWh',
         main='Seasonally-Adjusted Monthly Energy Consumption')
b <- c('Sub-meter-1', 'Sub-meter-2', 'Sub-meter-3')
legend('top', b, col=c('red', 'green', 'blue'), lwd=2, bty='n')

#sub-meter-1
mnth_smooth1 <- HoltWinters(mnth_seasonAdj[,1], beta=FALSE, gamma=FALSE)
plot(mnth_smooth1)

mnth_smoothFcast1 <- forecast(mnth_smooth1)
autoplot(mnth_smoothFcast,
         xlim=c(10,13),
         xlab='Month',
         ylab='kWh',
         main='30 Day Forecast of Sub-Meter-1')

#sub-meter-2
mnth_smooth2 <- HoltWinters(mnth_seasonAdj[,2], beta=FALSE, gamma=FALSE)
plot(mnth_smooth2)

mnth_smoothFcast2 <- forecast(mnth_smooth2)
autoplot(mnth_smoothFcast2,
         xlim=c(10,13),
         xlab='Month',
         ylab='kWh',
         main='30 Day Forecast of Sub-Meter-2')

#sub-meter-3
mnth_smooth3 <- HoltWinters(mnth_seasonAdj[,3], beta=FALSE, gamma=FALSE)
plot(mnth_smooth3)

mnth_smoothFcast3 <- forecast(mnth_smooth3)
autoplot(mnth_smoothFcast3,
         xlim=c(10,13),
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
# Hour of Day / 15_minute#
##########################


hofDay_seasonAdj <- housePWR_hofDayTS-hofDay_decomp$seasonal
plot(hofDay_seasonAdj, plot.type='s',
     xaxp = c(0, 23, 23),
     col=c('red', 'green', 'blue'),
     xlab='Hour of Day', ylab='kWh',
     main='Seasonally-Adjusted Hourly Energy Consumption')
b <- c('Sub-meter-1', 'Sub-meter-2', 'Sub-meter-3')
legend('topleft', b, col=c('red', 'green', 'blue'), lwd=2, bty='n')

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
hofDay_smooth3 <- HoltWinters(hofDay_seasonAdj[,3], beta=FALSE, gamma=FALSE)
plot(hofDay_smooth3)

hofDay_smoothFcast3 <- forecast(hofDay_smooth3, h=20)
autoplot(hofDay_smoothFcast3)


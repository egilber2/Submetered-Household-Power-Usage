
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
library(doParallel)
library(parallel)
library(lubridate)
library(VIM)
library(Hmisc)
library(GGally)
library(scales)
library(forecast)
library(xts)
library(stargazer)
library(grid)
library(graphics)
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
  filter(year(DateTime)==2007 | year(DateTime)==2008 | year(DateTime)==2009) %>%
  group_by(year(DateTime), month(DateTime)) %>%
  summarise(sum1=round(sum(Sub_metering_1/1000), 3),
            sum2=round(sum(Sub_metering_2/1000), 3),
            sum3=round(sum(Sub_metering_3/1000),3),
            first_DateTime = first(DateTime))


# Subset by Month and Day of Week
housePWR_mnth <- house_pwr %>%
  filter(year(DateTime)==2007 | year(DateTime)==2008 | year(DateTime)==2009) %>%
  group_by(month(DateTime), wday(DateTime)) %>%
  summarise(sum1=round(sum(Sub_metering_1/1000),3),
            sum2=round(sum(Sub_metering_2/1000),3),
            sum3=round(sum(Sub_metering_3/1000),3),
            first_DateTime = first(DateTime))


# Subset by Day of Week and hour of day
housePWR_dofWk <- house_pwr %>%
  filter(year(DateTime)==2007 | year(DateTime)==2008 | year(DateTime)==2009) %>%
  group_by(wday(DateTime), hour(DateTime)) %>%
  summarise(sum1=round(sum(Sub_metering_1/1000),3),
            sum2=round(sum(Sub_metering_2/1000),3),
            sum3=round(sum(Sub_metering_3/1000),3),
            first_DateTime = first(DateTime))


# Subset hour of day
housePWR_hofDay <- house_pwr %>%
  filter(year(DateTime)==2007 | year(DateTime)==2008 | year(DateTime)==2009) %>%
  filter(minute(DateTime)==00 | minute(DateTime)==15 | minute(DateTime)==30 | minute(DateTime)==45) %>%
  group_by(hour(DateTime), minute(DateTime)) %>%
  summarise(sum1=round(sum(Sub_metering_1/1000),3),
            sum2=round(sum(Sub_metering_2/1000),3),
            sum3=round(sum(Sub_metering_3/1000),3),
            first_DateTime = first(DateTime))


# Convert to Time Series --------------------------------------------------

# Year/month
housePWR_yrTS <- ts(housePWR_yr[,3:5], frequency = 12, start = 2007, end=2010)
plot(housePWR_yrTS, plot.type='s',
     xaxp = c(2007, 2010, 3),
     col=c('red', 'green', 'blue'),
     main='Total Yearly Kwh Consumption (2007-2010)',
     xlab='Year', ylab = 'Total kWh')
minor.tick(nx=6)
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



# Forecasting -------------------------------------------------------------

# Year/month
fit1 <- tslm(housePWR_yrTS ~ trend + season)
x <- forecast(fit1, h=12, level = c(80, 95))
autoplot(x, PI=TRUE, colour=TRUE)
summary(fit1)

# Month/Day of Week
fit2 <- tslm(housePWR_mnthTS ~ trend + season)
y <- forecast(fit2, h=10)
autoplot(y, PI=TRUE, colour=TRUE)
summary(fit2)

# Day of Week / Hour
fit3 <- tslm(housePWR_dofWkTS ~ trend + season)
z <- forecast(fit3, h=24)
autoplot(z, PI=TRUE, colour=TRUE)
summary(fit3)

# Hour of Day / Minute
fit4 <- tslm(housePWR_hofDayTS ~ trend + season)
w <- forecast(fit4, h=20)
autoplot(w, PI=TRUE, colour=TRUE)
summary(fit4)


# HoltWinters -------------------------------------------------------------

##############
# Year/Month #
##############

yr_decompSTL_3 <- stl(housePWR_yrTS[,3], s.window = 'periodic', robust=TRUE)
plot(yr_decompSTL_3, col='blue')

yr_decomp <- decompose(housePWR_yrTS)
plot(yr_decomp)
plot(yr_decomp$seasonal)
plot(yr_decomp$trend)
plot(yr_decomp$random)
summary(yr_decomp)
yr_decomp

# Remove seasonal component
#sub-meter-1
yr_seasonAdj <- housePWR_yrTS-yr_decomp$seasonal
autoplot(yr_seasonAdj)

yr_forecast1 <- HoltWinters(yr_seasonAdj[,1], beta=FALSE, gamma=FALSE)
yr_forecast1$fitted
plot(yr_forecast1)

yr_forecast1HW <- forecast(yr_forecast1)
autoplot(yr_forecast1HW)

#sub-meter-2
yr_forecast2 <- HoltWinters(yr_seasonAdj[,2], beta=FALSE, gamma=FALSE)
yr_forecast1$fitted
plot(yr_forecast2)

yr_forecast2HW <- forecast(yr_forecast2)
autoplot(yr_forecast2HW)

#sub-meter-3
yr_forecast3 <- HoltWinters(yr_seasonAdj[,3], beta=FALSE, gamma=FALSE)
yr_forecast3$fitted
plot(yr_forecast3)

yr_forecast3HW <- forecast(yr_forecast3, h=5)
autoplot(yr_forecast3HW)

#######################
# Month / Day of Week #
#######################

#month_decompSTL_3 <- stl(housePWR_mnthTS[,3], s.window = 'periodic', robust=TRUE)
#plot(month_decompSTL_3, col='blue')
#summary(month_decompSTL_3)

mnth_decomp <- decompose(housePWR_mnthTS)
plot(mnth_decomp)
plot(mnth_decomp$seasonal)
plot(mnth_decomp$trend)
plot(mnth_decomp$random)
summary(mnth_decomp)
mnth_decomp

# Remove seasonal component
#sub-meter-1
mnth_seasonAdj <- housePWR_mnthTS-mnth_decomp$seasonal
autoplot(mnth_seasonAdj)

#sub-meter-1
mnth_forecast1 <- HoltWinters(mnth_seasonAdj[,1], beta=FALSE, gamma=FALSE)
mnth_forecast1$fitted
plot(mnth_forecast1)

mnth_forecast1HW <- forecast(mnth_forecast1)
autoplot(mnth_forecast1HW)

#sub-meter-2
mnth_forecast2 <- HoltWinters(mnth_seasonAdj[,2], beta=FALSE, gamma=FALSE)
mnth_forecast2$fitted
plot(mnth_forecast2)

mnth_forecast2HW <- forecast(mnth_forecast2)
autoplot(mnth_forecast2HW)

#sub-meter-3
mnth_forecast3 <- HoltWinters(mnth_seasonAdj[,3], beta=FALSE, gamma=FALSE)
mnth_forecast3$fitted
plot(mnth_forecast3)

mnth_forecast3HW <- forecast(mnth_forecast3)
autoplot(mnth_forecast3HW)

#####################
# Day of Week / Hour#
#####################

#dofW_decompSTL_3 <- stl(housePWR_dofWkTS[,3], s.window='periodic', robust=TRUE)
#plot(dofW_decompSTL_3, col='blue')


dofW_decomp <- decompose(housePWR_dofWkTS)
plot(dofW_decomp)
plot(dofW_decomp$seasonal)
plot(dofW_decomp$trend)
plot(dofW_decomp$random)
summary(dofW_decomp)

dofW_seasonAdj <- housePWR_dofWkTS-dofW_decomp$seasonal
plot(dofW_seasonAdj)

#sub-meter-1
dofW_forecast1 <- HoltWinters(dofW_seasonAdj[,1], beta=FALSE, gamma=FALSE)
dofW_forecast1$fitted
plot(dofW_forecast1)

dofW_forecast1HW <- forecast(dofW_forecast1)
autoplot(dofW_forecast1HW)

#sub-meter-2
dofW_forecast2 <- HoltWinters(dofW_seasonAdj[,2], beta=FALSE, gamma=FALSE)
dofW_forecast2$fitted
plot(dofW_forecast2)

dofW_forecast2HW <- forecast(dofW_forecast2)
autoplot(dofW_forecast2HW)

#sub-meter-3
dofW_forecast3 <- HoltWinters(dofW_seasonAdj[,3], beta=FALSE, gamma=FALSE)
dofW_forecast3$fitted
plot(dofW_forecast3)

dofW_forecast3HW <- forecast(dofW_forecast3)
autoplot(dofW_forecast3HW)

##########################
# Hour of Day / 15_minute#
##########################

#hofDay_decompSTL_3 <-  stl(housePWR_hofDayTS[,3], s.window='periodic', robust=TRUE)
#plot(hofDay_decompSTL_3, col='blue')

# Remove seasonal component

hofDay_decomp <- decompose(housePWR_hofDayTS)
plot(hofDay_decomp)
plot(hofDay_decomp$seasonal)
plot(hofDay_decomp$trend)
plot(hofDay_decomp$random, col=1:3)
summary(hofDay_decomp)
hofDay_decomp


hofDay_seasonAdj <- housePWR_hofDayTS-hofDay_decomp$seasonal
plot(hofDay_seasonAdj)

#sub-meter-1
hofDay_forecast1 <- HoltWinters(hofDay_seasonAdj[,1], beta=FALSE, gamma=FALSE)
hofDay_forecast1$fitted
plot(hofDay_forecast1)

hofDay_forecast1HW <- forecast(hofDay_forecast1)
autoplot(hofDay_forecast1HW)

#sub-meter-2
hofDay_forecast2 <- HoltWinters(hofDay_seasonAdj[,2], beta=FALSE, gamma=FALSE)
hofDay_forecast2$fitted
plot(hofDay_forecast2)

hofDay_forecast2HW <- forecast(hofDay_forecast2)
autoplot(hofDay_forecast2HW)

#sub-meter-3
hofDay_forecast3 <- HoltWinters(hofDay_seasonAdj[,3], beta=FALSE, gamma=FALSE)
hofDay_forecast3$fitted
plot(hofDay_forecast3)

hofDay_forecast3HW <- forecast(hofDay_forecast3)
autoplot(hofDay_forecast3HW)

# Day of Week TS


# Yearly TS
yr2_decomp <- decompose(housePWR_yr2TS)
plot(yr2_decomp)
plot(yr2_decomp$seasonal)
plot(yr2_decomp$trend)
autoplot(yr2_decomp$random)
summary(yr2_decomp)

yrs_seasonAdj_3 <- housePWR_yr2TS- yr2_decomp$seasonal
plot(yrs_seasonAdj_3)

yr2_decompSTL_1 <- stl(housePWR_yr2TS[,1], s.window = 'periodic', robust=TRUE)
plot(yr2_decompSTL_1, col='red')
summary(yr2_decompSTL)

yr2_decompSTL_2 <- stl(housePWR_yr2TS[,2], s.window = 'periodic', robust=TRUE)
plot(yr2_decompSTL_2, col='green3')
summary()

yr2_decompSTL_3 <- stl(housePWR_yr2TS[,3], s.window = 'periodic', robust=TRUE)
plot(yr2_decompSTL_3, col='blue')


# Holt-Winters Only for Sub-Meter-3 ------------------------------------------------------------

##--Yearly--##

#Remove Seasonal Component
##-stl
#yr_seasonAdj_3 <- seasadj(yr_decompSTL_3)
#autoplot(yr_seasonAdj_3)
##
yr_seasonAdj <- housePWR_yrTS-yr_decomp$seasonal
autoplot(yr_seasonAdj)

yr_forecast <- HoltWinters(yr_seasonAdj, beta=FALSE, gamma=FALSE)
yr_forecast$fitted
plot(yr_forecast)

yr_forecastHW <- forecast(yr_forecast)
plot(yr_forecastHW)

yr_forecast_3HW <- forecast(yr_forecast_3, h=7)
plot(year_forecast_3HW)

##--Monthly--##

# Remove seasonal component
mnth_seasonAdj_3 <- seasadj(month_decompSTL_3)
plot(mnth_seasonAdj_3, col='blue')


mnth_forecast_3 <- HoltWinters(mnth_seasonAdj_3, beta=FALSE, gamma = FALSE, l.start=120.03637)
plot(mnth_forecast_3, main='Fitted Holt-Winters Model for Sub-Meter-3 Using Exponential Smoothing',
     xlab='Month', ylab='Observed / Fitted Total kWh',
     xaxp = c(1, 12, 11))


mnth_forecast_3HW <- forecast(mnth_forecast_3, h=7)
plot(mnth_forecast_3HW, main='Forecast Consumption on Sub-Meter-3',
     xlab='Month', ylab='Total kWh',
     xaxp = c(1, 12, 11))
mnth_forecast_3HW

##--Day of Week--##

#Remove seasonal component


dofW_seasonAdj_3 <- seasadj(dofW_decompSTL_3)
autoplot(dofW_seasonAdj_3)

dofW_forecast_3 <- HoltWinters(dofW_seasonAdj_3, beta=FALSE, gamma = FALSE)
dofW_forecast_3$fitted
plot(dofW_forecast_3, main='Fitted Holt-Winters Model for Sub-Meter-3 Using Exponential Smoothing',
     xlab='Day of Week', ylab='Observed / Fitted Total kWh')

dofW_forecast_3HW <- forecast(dofW_forecast_3, h=28)
plot(dofW_forecast_3HW)
dofW_forecast_3HW

##--Hour of Day--##
hofDay_seasonAdj_3 <- seasadj(hofDay_decompSTL_3)
autoplot(hofDay_seasonAdj_3)

hofDay_forecast_3 <- HoltWinters(hofDay_seasonAdj_3, beta=FALSE, gamma = FALSE)
plot(hofDay_forecast_3)

hofDay_forecast_3HW <- forecast(hofDay_forecast_3)
plot(hofDay_forecast_3HW)
hofDay_forecast_3HW

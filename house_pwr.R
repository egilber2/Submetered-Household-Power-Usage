
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

# Subset Year
housePWR_yr <- house_pwr %>%
  filter(year(DateTime)==2007 | year(DateTime)==2008 | year(DateTime)==2009) %>%
  group_by(year(DateTime)) %>%
  summarise(sum1=round(sum(Sub_metering_1/1000), 3),
            sum2=round(sum(Sub_metering_2/1000), 3),
            sum3=round(sum(Sub_metering_3/1000),3),
            first_DateTime = first(DateTime))

# Subset by Year and Month
housePWR_yr2 <- house_pwr %>%
  filter(year(DateTime)==2007 | year(DateTime)==2008 | year(DateTime)==2009) %>%
  group_by(year(DateTime), month(DateTime)) %>%
  summarise(sum1=round(sum(Sub_metering_1/1000), 3),
            sum2=round(sum(Sub_metering_2/1000), 3),
            sum3=round(sum(Sub_metering_3/1000),3),
            first_DateTime = first(DateTime))


# Subset by Month
housePWR_mnth <- house_pwr %>%
  filter(year(DateTime)==2007 | year(DateTime)==2008 | year(DateTime)==2009) %>%
  group_by(month(DateTime)) %>%
  summarise(avg1=round(sum(Sub_metering_1/1000), 3),
            avg2=round(sum(Sub_metering_2/1000), 3),
            avg3=round(sum(Sub_metering_3/1000),3),
            first_DateTime = first(DateTime))


# Subset by Month and Day of Week
housePWR_mnth2 <- house_pwr %>%
  filter(year(DateTime)==2007 | year(DateTime)==2008 | year(DateTime)==2009) %>%
  group_by(month(DateTime), wday(DateTime)) %>%
  summarise(sum1=round(sum(Sub_metering_1/1000),3),
            sum2=round(sum(Sub_metering_2/1000),3),
            sum3=round(sum(Sub_metering_3/1000),3),
            first_DateTime = first(DateTime))

# Subset by Day of Week
housePWR_dofWk <- house_pwr %>%
  filter(year(DateTime)==2007 | year(DateTime)==2008 | year(DateTime)==2009) %>%
  group_by(wday(DateTime)) %>%
  summarise(sum1=sum(Sub_metering_1),
            sum2=sum(Sub_metering_2),
            sum3=sum(Sub_metering_3),
            first_DateTime = first(DateTime))

# Subset by Day of Week and hour of day
housePWR_dofWk2 <- house_pwr %>%
  filter(year(DateTime)==2007 | year(DateTime)==2008 | year(DateTime)==2009) %>%
  group_by(wday(DateTime), hour(DateTime)) %>%
  summarise(sum1=round(sum(Sub_metering_1/1000),3),
            sum2=round(sum(Sub_metering_2/1000),3),
            sum3=round(sum(Sub_metering_3/1000),3),
            first_DateTime = first(DateTime))


# Subset hour of day
housePWR_hofDay <- house_pwr %>%
  filter(year(DateTime)==2007 | year(DateTime)==2008 | year(DateTime)==2009) %>%
  group_by(hour(DateTime), minute(DateTime)) %>%
  summarise(sum1=round(sum(Sub_metering_1/1000),3),
            sum2=round(sum(Sub_metering_2/1000),3),
            sum3=round(sum(Sub_metering_3/1000),3),
            first_DateTime = first(DateTime))


# Convert to Time Series --------------------------------------------------

#housePWR_yrTS <- ts(housePWR_yr[,2:4], frequency = 1, start = 2007, end=2009)
#plot(housePWR_yrTS, plot.type='s', col=1:3)



housePWR_yr2TS <- ts(housePWR_yr2[,3:5], frequency = 12, start= 2007, end=2010)
plot(housePWR_yr2TS, plot.type='s',
  xaxp = c(2007, 2010, 3),
  col=c('red', 'green', 'blue'),
  main='Total Monthly Kwh Consumption by Year (2007-2010)',
  xlab='Year', ylab = 'Total kWh', grid=TRUE)
minor.tick(nx=6)
b <- c('Sub-meter-1', 'Sub-meter-2', 'Sub-meter-3')
legend('topleft', b, col=c('red', 'green', 'blue'), lwd=2, bty='n')


#housePWR_mnthTS <- ts(housePWR_mnth[,2:4], frequency = 1, start= 1)
#plot.ts(housePWR_mnthTS, plot.type='s', col=1:3)

housePWR_mnth2TS <- ts(housePWR_mnth2[,3:5], frequency = 7, start= 1, end=12)
plot(housePWR_mnth2TS, plot.type='s',
        xaxp = c(1, 12, 11),
        col=c('red', 'green', 'blue'),
        xlab='Month', ylab = 'Total kWh',
        main='Total Monthly kWh Consumption (2007-2010)')
minor.tick(nx=8)
b <- c('Sub-meter-1', 'Sub-meter-2', 'Sub-meter-3')
legend('top', b, col=c('red', 'green', 'blue'), lwd=2, bty='n')


#housePWR_dayTS <- ts(housePWR_day[,3:5], frequency = 365, start = c(2006,350))

#housePWR_dofWkTS <- ts(housePWR_dofWk[,2:4], frequency=1, start = 1)
#plot(housePWR_dofWkTS, plot.type='s', col=1:3)

housePWR_dofWk2TS <- ts(housePWR_dofWk2[,3:5], frequency=23, start = 1, end=7)
plot(housePWR_dofWk2TS, plot.type='s',
     xaxp = c(1, 7, 6),
     col=c('red', 'green', 'blue'),
     xlab='Day of Week', ylab = 'Total kWh',
     main='Total kWh Consumption by Day of the Week (2007-2010)')
minor.tick(nx=24)
b <- c('Sub-meter-1', 'Sub-meter-2', 'Sub-meter-3')
legend('topleft', b, col=c('red', 'green', 'blue'), lwd=2, bty='n')



housePWR_hofDayTS <- ts(housePWR_hofDay[,3:5], frequency=59, start=0, end=23)
plot(housePWR_hofDayTS, plot.type='s',
     xaxp = c(0, 23, 23),
     col=c('red', 'green', 'blue'),
     xlab='Hour of the Day', ylab = 'Total kWh',
     main='Total kWh Consumption by Hour of the Day (2007-2010)')
b <- c('Sub-meter-1', 'Sub-meter-2', 'Sub-meter-3')
legend('topleft', b, col=c('red', 'green', 'blue'), lwd=2, bty='n')




# Forecasting -------------------------------------------------------------

#use housePWR_yr2TS
#use housePWR_mnth2TS for seasonal trend
#use housePWR_hofDayTS for hourly trend in a day
#use housePWR_dofWk2TS for trend observed in day of week subset by hour



fit <- tslm(housePWR_mnth2TS ~ trend + season)
x <- forecast(fit, h=12)
autoplot(x, PI=TRUE, colour=TRUE)
xsummary(fit)

fit2 <- tslm(housePWR_hofDayTS ~ trend + season)
y <- forecast(fit2, h=100)
autoplot(y, PI=TRUE, colour=TRUE)
summary(fit2)

fit3 <- tslm(housePWR_dofWk2TS ~ trend + season)
z <- forecast(fit3, h=24)
autoplot(z, PI=TRUE, colour=TRUE)
summary(fit3)

fit4 <- tslm(housePWR_yr2TS ~ trend + season)
w <- forecast(fit4, h=10)
autoplot(w, PI=TRUE, colour=TRUE)
summary(fit4)


# Decompose & Plot Time Series---------------------------------------------------------------

# Monthly TS
mnth2_decomp <- decompose(housePWR_mnth2TS)
plot(mnth2_decomp)
plot(mnth2_decomp$seasonal)
plot(mnth2_decomp$trend)
plot(mnth2_decomp$random, col=1:3)
summary(mnth2_decomp)
mnth2_decomp

month_decompSTL <- stl(housePWR_mnth2TS[,1], s.window = 'periodic', robust=TRUE)
plot(month_decompSTL, col='red')

month_decompSTL <- stl(housePWR_mnth2TS[,2], s.window = 'periodic', robust=TRUE)
plot(month_decompSTL, col='green3')

month_decompSTL <- stl(housePWR_mnth2TS[,3], s.window = 'periodic', robust=TRUE)
plot(month_decompSTL, col='blue')

month_decompSTL
summary(month_decompSTL)

# Day of Week TS
dofW_decomp <- decompose(housePWR_dofWk2TS)
plot(dofW_decomp)
plot(dofW_decomp$seasonal)
plot(dofW_decomp$trend)
plot(dofW_decomp$random)
summary(dofW_decomp)

dofW_decompSTL <- stl(housePWR_dofWk2TS[,1], s.window = 'periodic', robust=TRUE)
plot(dofW_decompSTL, col='red')

dofW_decompSTL <- stl(housePWR_dofWk2TS[,2], s.window = 'periodic', robust=TRUE)
plot(dofW_decompSTL, col='green3')

dofW_decompSTL <- stl(housePWR_dofWk2TS[,3], s.window = 'periodic', robust=TRUE)
plot(dofW_decompSTL, col='blue')
summary(dofW_decompSTL)

# Yearly TS
yr2_decomp <- decompose(housePWR_yr2TS)
plot(yr2_decomp)
plot(yr2_decomp$seasonal)
plot(yr2_decomp$trend)
autoplot(yr2_decomp$random)
summary(yr2_decomp)

yr2_decompSTL <- stl(housePWR_yr2TS[,1], s.window = 'periodic', robust=TRUE)
plot(yr2_decompSTL, col='red')

yr2_decompSTL <- stl(housePWR_yr2TS[,2], s.window = 'periodic', robust=TRUE)
plot(yr2_decompSTL, col='green3')

yr2_decompSTL <- stl(housePWR_yr2TS[,3], s.window = 'periodic', robust=TRUE)
plot(yr2_decompSTL, col='blue')



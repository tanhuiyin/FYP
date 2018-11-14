##Sensitivity test for weekly, monthly and yearly timescale
##Library used
library(tidyverse)
library(lubridate)
library(dplyr)
library(Hmisc)
library(ggplot2)

##Import dataset
combined_data <- read_csv('data/completed_data.csv')
combined_data <- combined_data[,-1]
date <- as.POSIXct(combined_data$date, format= "%Y-%m-%d %H:%M:%S")
combined_data <- cbind(date,combined_data)
combined_data <- combined_data[,-2]
summary(combined_data)
####################################################################################################################################

## Grouping data hourly##
by_grp_data_mean <- combined_data %>% 
  mutate(time_stamp=as.POSIXct(date)) %>%
  group_by(hour=format(as.POSIXlt(cut(time_stamp,breaks='hour')),'%Y %H')) %>%
  summarise(co2_flux=mean(co2_flux,na.rm=TRUE),
            Site_temp=mean(TS_0_0_1,na.rm=TRUE),
            Chlorophyll=mean(CHL,na.rm=TRUE),
            Sea_Surface_Temp = mean(SST,na.rm=TRUE),
            POC = mean(POC,na.rm=TRUE),
            PIC = mean(PIC,na.rm=TRUE),
            PAR = mean(PAR,na.rm=TRUE),
            Precipitation = mean(HourlyPrecipMM,na.rm=TRUE))

by_grp_data_sd <- combined_data %>% 
  mutate(time_stamp=as.POSIXct(date)) %>%
  group_by(hour=format(as.POSIXlt(cut(time_stamp,breaks='hour')),'%Y %H')) %>%
  summarise(co2_flux_sd=sd(co2_flux,na.rm=TRUE),
            Site_temp_sd=sd(TS_0_0_1,na.rm=TRUE),
            Chlorophyll_sd=sd(CHL ,na.rm=TRUE),
            Sea_Surface_Temp_sd= sd(SST, na.rm=TRUE),
            POC_sd = sd(POC,na.rm=TRUE),
            PIC_sd = sd(PIC,na.rm=TRUE),
            PAR_sd = sd(PAR,na.rm=TRUE),
            Precipitation_sd = sd(HourlyPrecipMM, na.rm=TRUE))

# Merge the two dataframe
by_hourly_grp_data <- merge(by_grp_data_mean, by_grp_data_sd, by='hour')
rm(by_grp_data_mean, by_grp_data_sd)
write.csv(by_hourly_grp_data,file='by_hourly_group_data.csv')
hour <- as.character.POSIXt (by_hourly_grp_data$hour , format='%Y %H')
by_hourly_grp_data <- cbind(hour,by_hourly_grp_data)
by_hourly_grp_data <- by_hourly_grp_data[,-2]


## Grouping data daily ##
by_daily_grp_data_mean <- combined_data %>% 
  mutate(time_stamp=as.POSIXct(date)) %>%
  group_by(day=format(as.POSIXlt(cut(time_stamp,breaks='day')),'%Y %d')) %>%
  summarise(co2_flux=mean(co2_flux,na.rm=TRUE),
            Site_temp=mean(TS_0_0_1,na.rm=TRUE),
            Chlorophyll=mean(CHL,na.rm=TRUE),
            Sea_Surface_Temp = mean(SST,na.rm=TRUE),
            POC = mean(POC,na.rm=TRUE),
            PIC = mean(PIC,na.rm=TRUE),
            PAR = mean(PAR,na.rm=TRUE),
            Precipitation = mean(HourlyPrecipMM,na.rm=TRUE))

by_daily_grp_data_sd <- combined_data %>% 
  mutate(time_stamp=as.POSIXct(date)) %>%
  group_by(day=format(as.POSIXlt(cut(time_stamp,breaks='day')),'%Y %d')) %>%
  summarise(co2_flux_sd=sd(co2_flux,na.rm=TRUE),
            Site_temp_sd=sd(TS_0_0_1,na.rm=TRUE),
            Chlorophyll_sd=sd(CHL ,na.rm=TRUE),
            Sea_Surface_Temp_sd= sd(SST, na.rm=TRUE),
            POC_sd = sd(POC,na.rm=TRUE),
            PIC_sd = sd(PIC,na.rm=TRUE),
            PAR_sd = sd(PAR,na.rm=TRUE),
            Precipitation_sd = sd(HourlyPrecipMM, na.rm=TRUE))

# Merge the two dataframe
by_daily_grp_data <- merge(by_daily_grp_data_mean,by_daily_grp_data_sd,by='day')
rm(by_daily_grp_data_mean,by_daily_grp_data_sd)
write.csv(by_daily_grp_data,file='by_daily_group_data.csv') 
day <- as.character.POSIXt(by_daily_grp_data$day , format='%Y %d')
by_daily_grp_data <- cbind(day,by_daily_grp_data)
by_daily_grp_data <- by_daily_grp_data[,-2]

## Grouping data monthly ##
by_monthly_grp_data_mean <- combined_data %>% 
  mutate(time_stamp=as.POSIXct(date)) %>%
  group_by(month=format(as.POSIXlt(cut(time_stamp,breaks='month')),'%Y %m')) %>%
  summarise(co2_flux=mean(co2_flux,na.rm=TRUE),
            Site_temp=mean(TS_0_0_1,na.rm=TRUE),
            Chlorophyll=mean(CHL,na.rm=TRUE),
            Sea_Surface_Temp = mean(SST,na.rm=TRUE),
            POC = mean(POC,na.rm=TRUE),
            PIC = mean(PIC,na.rm=TRUE),
            PAR = mean(PAR,na.rm=TRUE),
            Precipitation = mean(HourlyPrecipMM,na.rm=TRUE))

by_monthly_grp_data_sd <- combined_data %>% 
  mutate(time_stamp=as.POSIXct(date)) %>%
  group_by(month=format(as.POSIXlt(cut(time_stamp,breaks='month')),'%Y %m')) %>%
  summarise(co2_flux_sd=sd(co2_flux,na.rm=TRUE),
            Site_temp_sd=sd(TS_0_0_1,na.rm=TRUE),
            Chlorophyll_sd=sd(CHL ,na.rm=TRUE),
            Sea_Surface_Temp_sd= sd(SST, na.rm=TRUE),
            POC_sd = sd(POC,na.rm=TRUE),
            PIC_sd = sd(PIC,na.rm=TRUE),
            PAR_sd = sd(PAR,na.rm=TRUE),
            Precipitation_sd = sd(HourlyPrecipMM, na.rm=TRUE))

# Merge the two dataframe
by_monthly_grp_data <- merge(by_monthly_grp_data_mean, by_monthly_grp_data_sd,by='month')
rm(by_monthly_grp_data_mean,by_monthly_grp_data_sd)
month <- paste(by_monthly_grp_data$month,"15"," 00:00:00")
by_monthly_grp_data <- cbind(month,by_monthly_grp_data)
month <- as.POSIXct(month1, format = "%Y %m %d %H:%M:%S", tz = 'Asia/Kuala_Lumpur')
by_monthly_grp_data <- by_monthly_grp_data[,-2]
write.csv(by_monthly_grp_data,file='by_monthly_group_data.csv') 


## Grouping data year ##
yearly_grp_data_mean <- combined_data %>% 
  mutate(time_stamp=as.POSIXct(date)) %>%
  group_by(year=format(as.POSIXlt(cut(time_stamp,breaks='year')),'%Y')) %>%
  summarise(co2_flux=mean(co2_flux,na.rm=TRUE),
            Site_temp=mean(TS_0_0_1,na.rm=TRUE),
            Chlorophyll=mean(CHL,na.rm=TRUE),
            Sea_Surface_Temp = mean(SST,na.rm=TRUE),
            POC = mean(POC,na.rm=TRUE),
            PIC = mean(PIC,na.rm=TRUE),
            PAR = mean(PAR,na.rm=TRUE),
            Precipitation = mean(HourlyPrecipMM,na.rm=TRUE))

yearly_grp_data_sd <- combined_data %>% 
  mutate(time_stamp=as.POSIXct(date)) %>%
  group_by(year=format(as.POSIXlt(cut(time_stamp,breaks='year')),'%Y')) %>%
  summarise(co2_flux_sd=sd(co2_flux,na.rm=TRUE),
            Site_temp_sd=sd(TS_0_0_1,na.rm=TRUE),
            Chlorophyll_sd=sd(CHL ,na.rm=TRUE),
            Sea_Surface_Temp_sd= sd(SST, na.rm=TRUE),
            POC_sd = sd(POC,na.rm=TRUE),
            PIC_sd = sd(PIC,na.rm=TRUE),
            PAR_sd = sd(PAR,na.rm=TRUE),
            Precipitation_sd = sd(HourlyPrecipMM, na.rm=TRUE))
# Merge the two dataframe
by_annual_grp_data <- merge(yearly_grp_data_mean, yearly_grp_data_sd, by='year')
rm(yearly_grp_data_mean, yearly_grp_data_sd)
write.csv(by_annual_grp_data,file='by_annual_grp_data.csv')
year <- as.character.POSIXt(by_annual_grp_data$year , format='%Y')
by_annual_grp_data <- cbind(year,by_annual_grp_data)
by_annual_grp_data <- by_annual_grp_data[,-2]

################################################################################
#### Correlational analysis of monthly averages ####
### For CO2_FLux and SST
plot(by_monthly_grp_data$co2_flux, by_monthly_grp_data$Chlorophyll,pch=19)
lm_co2_CHL_month <- lm(by_monthly_grp_data$co2_flux ~ by_monthly_grp_data$Chlorophyll)
summary(lm_co2_CHL_month)
cor.test(by_monthly_grp_data$co2_flux, by_monthly_grp_data$Chlorophyll)
plot(df_grp_month$WS, df_grp_month$co2_flux, pch =19)
lm_co2_U_month <- lm(df_grp_month$co2_flux ~ df_grp_month$WS)
summary(lm_co2_U_month)

#######################################
library(openair)
library(Hmisc)
library(dplyr)

# This step is needed if the dataframes are named other than 'df'
# Just need to change the name of df here and the rest of the script
# would be the same.
df <- read.csv('FYP/data/completed_data.csv')
df <- df[,-1]
time_stamp <- as.POSIXct(df$date, format= "%Y-%m-%d %H:%M:%S")
df <- cbind(time_stamp,df)
df <- df[,-2]
summary(df)

### Month and Year Classification
Month_class <- NA 
for (i in 1:nrow(df)) {
  if (df$time_stamp[i] < as.POSIXct('2015-11-30 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- 'Nov2015'
  }
  if (df$time_stamp[i] > as.POSIXct('2015-11-30 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur') &
      df$time_stamp[i] < as.POSIXct('2015-12-31 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- 'Dec2015'
  }
  if (df$time_stamp[i] > as.POSIXct('2015-12-31 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur') &
      df$time_stamp[i] < as.POSIXct('2016-01-31 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- 'Jan2016'
  }
  if (df$time_stamp[i] > as.POSIXct('2016-01-31 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur') &
      df$time_stamp[i] < as.POSIXct('2016-02-29 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- 'Feb2016'
  }
  if (df$time_stamp[i] > as.POSIXct('2016-02-29 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur') &
      df$time_stamp[i] < as.POSIXct('2016-03-31 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- 'Mar2016'
  }
  if (df$time_stamp[i] > as.POSIXct('2016-03-31 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur') &
      df$time_stamp[i] < as.POSIXct('2016-04-30 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- 'Apr2016'
  }
  if (df$time_stamp[i] > as.POSIXct('2016-04-30 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur') &
      df$time_stamp[i] < as.POSIXct('2016-05-31 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- 'May2016'
  }
  if (df$time_stamp[i] > as.POSIXct('2016-05-31 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur') &
      df$time_stamp[i] < as.POSIXct('2016-06-30 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- 'Jun2016'
  }
  if (df$time_stamp[i] > as.POSIXct('2016-06-30 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur') &
      df$time_stamp[i] < as.POSIXct('2016-07-31 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- 'Jul2016'
  }
  if (df$time_stamp[i] > as.POSIXct('2016-07-31 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur') &
      df$time_stamp[i] < as.POSIXct('2016-08-31 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- 'Aug2016'
  }
  if (df$time_stamp[i] > as.POSIXct('2016-08-31 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur') &
      df$time_stamp[i] < as.POSIXct('2016-09-30 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- 'Sep2016'
  }
  if (df$time_stamp[i] > as.POSIXct('2016-09-30 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur') &
      df$time_stamp[i] < as.POSIXct('2016-10-31 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- 'Oct2016'
  }
  if (df$time_stamp[i] > as.POSIXct('2016-10-31 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur') &
      df$time_stamp[i] < as.POSIXct('2016-11-30 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- 'Nov2016'
  }
  if (df$time_stamp[i] > as.POSIXct('2016-11-30 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur') &
      df$time_stamp[i] < as.POSIXct('2016-12-31 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- 'Dec2016'
  }
  if (df$time_stamp[i] > as.POSIXct('2016-12-31 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur') &
      df$time_stamp[i] < as.POSIXct('2017-01-31 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- 'Jan2017'
  }
  if (df$time_stamp[i] > as.POSIXct('2017-01-31 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur') &
      df$time_stamp[i] < as.POSIXct('2017-02-28 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- 'Feb2017'
  }
  if (df$time_stamp[i] > as.POSIXct('2017-2-28 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur') &
      df$time_stamp[i] < as.POSIXct('2017-03-31 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- 'Mac2017'
  }
  if (df$time_stamp[i] > as.POSIXct('2017-3-31 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur') &
      df$time_stamp[i] < as.POSIXct('2017-04-30 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- 'Apr2017'
  }
  if (df$time_stamp[i] > as.POSIXct('2017-04-30 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur') &
      df$time_stamp[i] < as.POSIXct('2017-05-31 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- 'May2017'
  }
  if (df$time_stamp[i] > as.POSIXct('2017-05-31 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur') &
      df$time_stamp[i] < as.POSIXct('2017-06-30 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- 'Jun2017'
  }
  if (df$time_stamp[i] > as.POSIXct('2017-06-30 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur') &
      df$time_stamp[i] < as.POSIXct('2017-07-31 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- 'Jul2017'
  }
  if (df$time_stamp[i] > as.POSIXct('2017-07-31 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur') &
      df$time_stamp[i] < as.POSIXct('2017-08-31 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- 'Aug2017'
  }
  if (df$time_stamp[i] > as.POSIXct('2017-08-31 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur') &
      df$time_stamp[i] < as.POSIXct('2017-09-30 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- 'Sep2017'
  }
  if (df$time_stamp[i] > as.POSIXct('2017-09-3 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur') &
      df$time_stamp[i] < as.POSIXct('2017-10-31 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- 'Oct2017'
  }
}
Month_class <- as.factor(Month_class)

df <- cbind(df,Month_class)
df$Month_class <- factor(df$Month_class, levels = c('Nov2015', 'Dec2015',
                                                    'Jan2016', 'Feb2016',
                                                    'Mar2016', 'Apr2016',
                                                    'May2016', 'Jun2016',
                                                    'Jul2016', 'Aug2016',
                                                    'Sep2016', 'Oct2016',
                                                    'Nov2016', 'Dec2016',
                                                    'Jan2017', 'Feb2017',
                                                    'Mar2017', 'Apr2017',
                                                    'May2017', 'Jun2017',
                                                    'Jul2017', 'Aug2017',
                                                    'Sep2017', 'Oct2017'))

## Export into csv
write.csv(df, file = 'df.csv')

######################################################################################
## Plotting the diurnal trend
library(ggplot2)
library(Hmisc)

## Rough Plot by year
# Diurnal co2_flux
plot(by_daily_grp_data$day,by_daily_grp_data$co2_flux,lwd=2,type='l',ylab='CO2_flux',xlab='Day')
plot(weekly_grp_data$week, weekly_grp_data$co2_flux,lwd=2,type='l',ylab='CO2_flux',xlab='Week')
plot(monthly_grp_data$month, monthly_grp_data$co2_flux,lwd=2,type='l',ylab='CO2_flux',xlab='Month')
plot(yearly_grp_data$year, yearly_grp_data$co2_flux,lwd=2,type='l',ylab='CO2_flux',xlab='Year')

# Diurnal Site_temp
plot(daily_grp_data$day,daily_grp_data$Site_temp,lwd=2,type='l',ylab='Site_Temp',xlab='Day')
plot(weekly_grp_data$week, weekly_grp_data$Site_temp,lwd=2,type='l',ylab='Site_Temp',xlab='Week')
plot(monthly_grp_data$month, monthly_grp_data$Site_temp,lwd=2,type='l',ylab='Site_Temp',xlab='Month')
plot(yearly_grp_data$year, yearly_grp_data$Site_temp,lwd=2,type='l',ylab='Site_Temp',xlab='Year')

# Diurnal CHL
plot(daily_grp_data$day,daily_grp_data$Chlorophyll,lwd=2,type='l',ylab='Chlorophyll',xlab='Day')
plot(weekly_grp_data$week, weekly_grp_data$Chlorophyll,lwd=2,type='l',ylab='Chlorophyll',xlab='Week')
plot(monthly_grp_data$month, monthly_grp_data$Chlorophyll,lwd=2,type='l',ylab='Chlorophyll',xlab='Month')
plot(yearly_grp_data$year, yearly_grp_data$Chlorophyll,lwd=2,type='l',ylab='Chlorophyll',xlab='Year')

# Diurnal SST
plot(daily_grp_data$day,daily_grp_data$Sea_Surface_Temp,lwd=2,type='l',ylab='SST',xlab='Day')
plot(weekly_grp_data$week, weekly_grp_data$Sea_Surface_Temp,lwd=2,type='l',ylab='SST',xlab='Week')
plot(monthly_grp_data$month, monthly_grp_data$Sea_Surface_Temp,lwd=2,type='l',ylab='SST',xlab='Month')
plot(yearly_grp_data$year, yearly_grp_data$Sea_Surface_Temp,lwd=2,type='l',ylab='SST',xlab='Year')

# Diurnal POC
plot(daily_grp_data$day,daily_grp_data$POC,lwd=2,type='l',ylab='POC',xlab='Day')
plot(weekly_grp_data$week, weekly_grp_data$POC,lwd=2,type='l',ylab='POC',xlab='Week')
plot(monthly_grp_data$month, monthly_grp_data$POC,lwd=2,type='l',ylab='POC',xlab='Month')
plot(yearly_grp_data$year, yearly_grp_data$POC,lwd=2,type='l',ylab='POC',xlab='Year')

# Diurnal PIC
plot(daily_grp_data$day,daily_grp_data$PIC,lwd=2,type='l',ylab='PIC',xlab='Day')
plot(weekly_grp_data$week, weekly_grp_data$PIC,lwd=2,type='l',ylab='PIC',xlab='Week')
plot(monthly_grp_data$month, monthly_grp_data$PIC,lwd=2,type='l',ylab='PIC',xlab='Month')
plot(yearly_grp_data$year, yearly_grp_data$PIC,lwd=2,type='l',ylab='PIC',xlab='Year')

# Diurnal PAR
plot(daily_grp_data$day,daily_grp_data$PAR,lwd=2,type='l',ylab='PAR',xlab='Day')
plot(weekly_grp_data$week, weekly_grp_data$PAR,lwd=2,type='l',ylab='PAR',xlab='Week')
plot(monthly_grp_data$month, monthly_grp_data$PAR,lwd=2,type='l',ylab='PAR',xlab='Month')
plot(yearly_grp_data$year, yearly_grp_data$PAR,lwd=2,type='l',ylab='PAR',xlab='Year')

## Rough Plot
# Diurnal co2_flux
plot(daily_grp_data$day,daily_grp_data$co2_flux,lwd=2,type='l',ylab='CO2_flux',xlab='Day')
plot(weekly_grp_data$week, weekly_grp_data$co2_flux,lwd=2,type='l',ylab='CO2_flux',xlab='Week')
plot(monthly_grp_data$month, monthly_grp_data$co2_flux,lwd=2,type='l',ylab='CO2_flux',xlab='Month')
plot(yearly_grp_data$year, yearly_grp_data$co2_flux,lwd=2,type='l',ylab='CO2_flux',xlab='Year')

# Diurnal Site_temp
plot(daily_grp_data$day,daily_grp_data$Site_temp,lwd=2,type='l',ylab='Site_Temp',xlab='Day')
plot(weekly_grp_data$week, weekly_grp_data$Site_temp,lwd=2,type='l',ylab='Site_Temp',xlab='Week')
plot(monthly_grp_data$month, monthly_grp_data$Site_temp,lwd=2,type='l',ylab='Site_Temp',xlab='Month')
plot(yearly_grp_data$year, yearly_grp_data$Site_temp,lwd=2,type='l',ylab='Site_Temp',xlab='Year')

# Diurnal CHL
plot(daily_grp_data$day,daily_grp_data$Chlorophyll,lwd=2,type='l',ylab='Chlorophyll',xlab='Day')
plot(weekly_grp_data$week, weekly_grp_data$Chlorophyll,lwd=2,type='l',ylab='Chlorophyll',xlab='Week')
plot(monthly_grp_data$month, monthly_grp_data$Chlorophyll,lwd=2,type='l',ylab='Chlorophyll',xlab='Month')
plot(yearly_grp_data$year, yearly_grp_data$Chlorophyll,lwd=2,type='l',ylab='Chlorophyll',xlab='Year')

# Diurnal SST
plot(daily_grp_data$day,daily_grp_data$Sea_Surface_Temp,lwd=2,type='l',ylab='SST',xlab='Day')
plot(weekly_grp_data$week, weekly_grp_data$Sea_Surface_Temp,lwd=2,type='l',ylab='SST',xlab='Week')
plot(monthly_grp_data$month, monthly_grp_data$Sea_Surface_Temp,lwd=2,type='l',ylab='SST',xlab='Month')
plot(yearly_grp_data$year, yearly_grp_data$Sea_Surface_Temp,lwd=2,type='l',ylab='SST',xlab='Year')

# Diurnal POC
plot(daily_grp_data$day,daily_grp_data$POC,lwd=2,type='l',ylab='POC',xlab='Day')
plot(weekly_grp_data$week, weekly_grp_data$POC,lwd=2,type='l',ylab='POC',xlab='Week')
plot(monthly_grp_data$month, monthly_grp_data$POC,lwd=2,type='l',ylab='POC',xlab='Month')
plot(yearly_grp_data$year, yearly_grp_data$POC,lwd=2,type='l',ylab='POC',xlab='Year')

# Diurnal PIC
plot(daily_grp_data$day,daily_grp_data$PIC,lwd=2,type='l',ylab='PIC',xlab='Day')
plot(weekly_grp_data$week, weekly_grp_data$PIC,lwd=2,type='l',ylab='PIC',xlab='Week')
plot(monthly_grp_data$month, monthly_grp_data$PIC,lwd=2,type='l',ylab='PIC',xlab='Month')
plot(yearly_grp_data$year, yearly_grp_data$PIC,lwd=2,type='l',ylab='PIC',xlab='Year')

# Diurnal PAR
plot(daily_grp_data$day,daily_grp_data$PAR,lwd=2,type='l',ylab='PAR',xlab='Day')
plot(weekly_grp_data$week, weekly_grp_data$PAR,lwd=2,type='l',ylab='PAR',xlab='Week')
plot(monthly_grp_data$month, monthly_grp_data$PAR,lwd=2,type='l',ylab='PAR',xlab='Month')
plot(yearly_grp_data$year, yearly_grp_data$PAR,lwd=2,type='l',ylab='PAR',xlab='Year')
## Correlation Test in diurnal scale
##Daily timescale
cor.test(by_daily_grp_data$co2_flux,by_daily_grp_data$Site_temp)
cor.test(by_daily_grp_data$co2_flux,by_daily_grp_data$Chlorophyll)
cor.test(by_daily_grp_data$co2_flux,by_daily_grp_data$Sea_Surface_Temp)
cor.test(by_daily_grp_data$co2_flux,by_daily_grp_data$POC)
cor.test(by_daily_grp_data$co2_flux,by_daily_grp_data$PIC)
cor.test(by_daily_grp_data$co2_flux,by_daily_grp_data$PAR)
cor.test(by_daily_grp_data$Site_temp,by_daily_grp_data$Chlorophyll)
cor.test(by_daily_grp_data$Site_temp,by_daily_grp_data$Sea_Surface_Temp)
cor.test(by_daily_grp_data$Site_temp,by_daily_grp_data$POC)
cor.test(by_daily_grp_data$Site_temp,by_daily_grp_data$PIC)
cor.test(by_daily_grp_data$Site_temp,by_daily_grp_data$PAR)
cor.test(by_daily_grp_data$Chlorophyll,by_daily_grp_data$Sea_Surface_Temp)
cor.test(by_daily_grp_data$Chlorophyll,by_daily_grp_data$POC)
cor.test(by_daily_grp_data$Chlorophyll,by_daily_grp_data$PIC)
cor.test(by_daily_grp_data$Chlorophyll,by_daily_grp_data$PAR)
cor.test(by_daily_grp_data$Sea_Surface_Temp,by_daily_grp_data$POC)
cor.test(by_daily_grp_data$Sea_Surface_Temp,by_daily_grp_data$PIC)
cor.test(by_daily_grp_data$Sea_Surface_Temp,by_daily_grp_data$PAR)
cor.test(by_daily_grp_data$POC,by_daily_grp_data$PIC)
cor.test(by_daily_grp_data$POC,by_daily_grp_data$PAR)
cor.test(by_daily_grp_data$PAR,by_daily_grp_data$PIC)


##Monthly timescale
cor.test(by_monthly_grp_data$co2_flux,by_monthly_grp_data$Site_temp)
cor.test(by_monthly_grp_data$co2_flux,by_monthly_grp_data$Chlorophyll)
cor.test(by_monthly_grp_data$co2_flux,by_monthly_grp_data$Sea_Surface_Temp)
cor.test(by_monthly_grp_data$co2_flux,by_monthly_grp_data$POC)
cor.test(by_monthly_grp_data$co2_flux,by_monthly_grp_data$PIC)
cor.test(by_monthly_grp_data$co2_flux,by_monthly_grp_data$PAR)
cor.test(by_monthly_grp_data$Site_temp,by_monthly_grp_data$Chlorophyll)
cor.test(by_monthly_grp_data$Site_temp,by_monthly_grp_data$Sea_Surface_Temp)
cor.test(by_monthly_grp_data$Site_temp,by_monthly_grp_data$POC)
cor.test(by_monthly_grp_data$Site_temp,by_monthly_grp_data$PIC)
cor.test(by_monthly_grp_data$Site_temp,by_monthly_grp_data$PAR)
cor.test(by_monthly_grp_data$Chlorophyll,by_monthly_grp_data$Sea_Surface_Temp)
cor.test(by_monthly_grp_data$Chlorophyll,by_monthly_grp_data$POC)
cor.test(by_monthly_grp_data$Chlorophyll,by_monthly_grp_data$PIC)
cor.test(by_monthly_grp_data$Chlorophyll,by_monthly_grp_data$PAR)
cor.test(by_monthly_grp_data$Sea_Surface_Temp,by_monthly_grp_data$POC)
cor.test(by_monthly_grp_data$Sea_Surface_Temp,by_monthly_grp_data$PIC)
cor.test(by_monthly_grp_data$Sea_Surface_Temp,by_monthly_grp_data$PAR)
cor.test(by_monthly_grp_data$POC,by_monthly_grp_data$PIC)
cor.test(by_monthly_grp_data$POC,by_monthly_grp_data$PAR)
cor.test(by_monthly_grp_data$PAR,by_monthly_grp_data$PIC)

#### Correlational analysis and plots between CO2 and environmental drivers ####
# Sat_SST and Sea surface temperature
path_fig <- file.path('/Users/tanhuiyin/FYP/figs/cor.jpg')
jpeg(file=path_fig,width=20,height=15,res=400, units = 'cm')
par(family='Times', mfrow = c(2,2))
par(mar = c(4,4,1,1))
plot(by_monthly_grp_data$Sea_Surface_Temp,
     by_monthly_grp_data$co2_flux, pch = 10,
     xlab = '', ylab = '',
     xlim= c(28,33), ylim = c(-0.5,0.5), col = alpha('red',0.2), yaxt = 'n')
points(by_monthly_grp_data$Sea_Surface_Temp,
       by_monthly_grp_data$co2_flux, pch = 16,
       col = alpha('blue',0.2))
axis(side = 2, at = c(-0.5, -0.25, 0, 0.25, 0.5), 
     labels = c(paste('\u2212',1,sep=''),paste('\u2212',0.5,sep=''),0,0.5,1))
minor.tick()
mtext(expression(paste('CO'['2'],' flux')),side = 2,line=2)
mtext(expression('SST'['Sat']),side = 1,line=2)
lmT <- lm(by_monthly_grp_data$co2_flux ~ 
            by_monthly_grp_data$Sea_Surface_Temp)
lmTenso <- lm(by_monthly_grp_data$co2_flux ~ 
              by_monthly_grp_data$Sea_Surface_Temp)
lmTxenso <- lm(by_monthly_grp_data$co2_flux ~ 
              by_monthly_grp_data$Sea_Surface_Temp)
abline(lmTenso, col = 'red', lwd = 3, lty = 2)
abline(lmTxenso, col = 'blue', lwd = 3, lty = 2)
abline(lmT, col = 'black', lwd = 3, lty = 2)
summary(lmT)
summary(lmTenso)
summary(lmTxenso)


# CHL and CO2_flux
par(mar = c(4,4,1,1))
plot(by_monthly_grp_data$Chlorophyll,
     by_monthly_grp_data$co2_flux, pch = 10,
     xlab = '', ylab = '',
     xlim= c(0.3,1.8), ylim = c(-0.5,0.5), col = alpha('red',0.2), yaxt = 'n')
points(by_monthly_grp_data$Chlorophyll,
       by_monthly_grp_data$co2_flux, pch = 16,
       col = alpha('blue',0.2))
axis(side = 2, at = c(-0.5,-0.25,0,0.25,0.5), 
     labels = c(paste('\u2212',1,sep=''),paste('\u2212',0.5,sep=''),0,0.5,1))
minor.tick()
mtext(expression(paste('CO'['2'],' flux')),side = 2,line=2)
mtext(expression('CHL'['Sat']),side = 1,line=2)
lmT <- lm(by_monthly_grp_data$co2_flux ~ 
            by_monthly_grp_data$Chlorophyll)
lmTenso <- lm(by_monthly_grp_data$co2_flux ~ 
                by_monthly_grp_data$Chlorophyll)
lmTxenso <- lm(by_monthly_grp_data$co2_flux ~ 
                 by_monthly_grp_data$Chlorophyll)
abline(lmTenso, col = 'red', lwd = 3, lty = 2)
abline(lmTxenso, col = 'blue', lwd = 3, lty = 2)
abline(lmT, col = 'black', lwd = 3, lty = 2)
summary(lmT)
summary(lmTenso)
summary(lmTxenso)

# POC and CO2_flux
par(mar = c(4,4,1,1))
plot(by_monthly_grp_data$POC,
     by_monthly_grp_data$co2_flux, pch = 10,
     xlab = '', ylab = '',
     xlim= c(85,375), ylim = c(-0.5,0.5), col = alpha('red',0.2), yaxt = 'n')
points(by_monthly_grp_data$Sea_Surface_Temp,
       by_monthly_grp_data$co2_flux, pch = 16,
       col = alpha('blue',0.2))
axis(side = 2, at = c(-0.5,-0.25,0,0.25,0.5), 
     labels = c(paste('\u2212',1,sep=''),paste('\u2212',0.5,sep=''),0,0.5,1))
minor.tick()
mtext(expression(paste('CO'['2'],' flux')),side = 2,line=2)
mtext(expression('POC'),side = 1,line=2)
lmT <- lm(by_monthly_grp_data$co2_flux ~ 
            by_monthly_grp_data$POC)
lmTenso <- lm(by_monthly_grp_data$co2_flux ~ 
                by_monthly_grp_data$Sea_Surface_Temp)
lmTxenso <- lm(by_monthly_grp_data$co2_flux ~ 
                 by_monthly_grp_data$POC)
abline(lmTenso, col = 'red', lwd = 3, lty = 2)
abline(lmTxenso, col = 'blue', lwd = 3, lty = 2)
abline(lmT, col = 'black', lwd = 3, lty = 2)
summary(lmT)
summary(lmTenso)
summary(lmTxenso)
dev.off()
######################################################################

### Time_Series for 4 parameters
library(reshape2)
library(ggplot2)
path_fig <- file.path('/Users/tanhuiyin/FYP/figs/time_series.jpg')
jpeg(file=path_fig,width=20,height=12,res=400, units = 'cm')
par(family='Times', oma=c(3.1,0.5,0.5,0.5), mfrow = c(4,1))

par(mai=c(0.05,0.6,0.05, 0.65))
plot(by_monthly_grp_data$month, 
by_monthly_grp_data$co2_flux, type = 'l', 
     ylab = '', xlab = '',
     col = 'red', xaxt = 'n', cex.axis = 1.5)
minor.tick()
mtext(side = 2, expression('CO'['2'],'flux'), line = 5, cex = 1.1)
legend(as.POSIXct('2015 11 15 00:00:00', format = '%Y %m %d %H:%M:%S'),
       24, bty = 'n', lwd = 2, col = 'red', text.col = 'red',
       legend = expression('T'['A']), cex = 1.5)
legend(as.POSIXct('2017 10 15 00:00:00', format = '%Y %m %d %H:%M:%S'),
       24, bty = 'n', lwd = 2, col = 'blue', text.col = 'blue',
       legend = expression('e'['a']), cex = 1.5)
par(new = TRUE)
plot(by_monthly_grp_data$month, by_monthly_grp_data$co2_flux, type = 'l',
     axes = FALSE, xlab = '', ylab = '', col = 'blue', ylim = c(1.5,3.7))
lines(c(as.POSIXct('2016-05-31 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
        as.POSIXct('2016-05-31 00:00:00', format = '%Y-%m-%d %H:%M:%S')),
      c(0,120), lwd = 3, lty = 2)
axis(4, cex.axis = 1.5)

par(mai=c(0.05,0.6,0.05, 0.65))
plot(by_monthly_grp_data$month, 
     by_monthly_grp_data$Chlorophyll, type = 'l', xlab = '', ylab = '', col = 'grey40',
     xaxt = 'n', cex.axis = 1.5)
minor.tick()
lines(c(as.POSIXct('2016-05-31 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
        as.POSIXct('2016-05-31 00:00:00', format = '%Y-%m-%d %H:%M:%S')),
      c(0,120), lwd = 3, lty = 2)
mtext(side = 2, 'CHL', line = 2.5, cex = 1.1)



par(mai=c(0.05,0.6,0.05, 0.65))
plot(by_monthly_grp_data$month[as.POSIXct('2016 01 15 00:00:00', format = '%Y %m %d %H:%M:%S'),
                               as.POSIXct('2016 12 15 00:00:00', format = '%Y %m %d %H:%M:%S')], 
     by_monthly_grp_data$Sea_Surface_Temp[month(as.POSIXct('2016 01 15 00:00:00', format = '%Y %m %d %H:%M:%S'),
                                          as.POSIXct('2016 12 15 00:00:00', format = '%Y %m %d %H:%M:%S'))]
     , type = 'l',col = 'darkgreen', ylab = '', xlab = '', ylim = c(26, 34),
     cex.axis = 1.5)
text(as.POSIXct('2016-03-15 00:00:00', format = '%Y-%m-%d %H:%M:%S'), 26.5,
     'ENSO', cex = 1.5)
text(as.POSIXct('2016-08-15 00:00:00', format = '%Y-%m-%d %H:%M:%S'), 26.5,
     'non-ENSO', cex = 1.5)

mtext(side = 2, expression('T'['S']), line = 2.5, cex = 1.1) 
mtext(side = 1, 'Month', line = 2.5, cex = 1.1)
axis(side = 1, at = c(as.POSIXct('2015-12-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
                      as.POSIXct('2016-02-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
                      as.POSIXct('2016-04-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
                      as.POSIXct('2016-06-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
                      as.POSIXct('2016-08-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
                      as.POSIXct('2016-10-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
                      as.POSIXct('2016-12-01 00:00:00', format = '%Y-%m-%d %H:%M:%S')),
     labels = c('Dec', 'Feb', 'Apr', 'Jun', 'Aug', 'Oct','Dec'), cex.axis = 1.5)
#legend(as.POSIXct('2015-10-26 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
#       69, bty = 'n', lwd = 2, col = 'red', text.col = 'red',
#       legend = expression('T'['WS']), cex = 1.5)
legend(as.POSIXct('2015-10-26 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
       34.5, bty = 'n', lwd = 2, col = 'darkgreen', text.col = 'darkgreen',
       legend = expression('T'['S1']), cex = 1.5)
legend(as.POSIXct('2016-01-5 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
       34.5, bty = 'n', lwd = 2, col = 'blue', text.col = 'blue',
       legend = expression('T'['S2']), cex = 1.5)
#axis(4, cex.axis = 1.5)
#mtext(side = 4, expression('T'['S']), line = 2.7, cex = 1.1)
par(new = TRUE)
plot(df$time_stamp, df$TS_2_1_1, type = 'l',
     col = 'blue', ylab = '', xlab = '', ylim = c(26,34), 
     axes = FALSE)
lines(c(as.POSIXct('2016-05-31 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
        as.POSIXct('2016-05-31 00:00:00', format = '%Y-%m-%d %H:%M:%S')),
      c(20,80), lwd = 3, lty = 2)
dev.off()

#### Wind rose ####
path_fig <- file.path('/Users/Yusri/Documents/Work/Data_analysis/muka_head/figs/wind_rose.jpg')
jpeg(file=path_fig,width=8,height=8,res=400, units = 'cm', family = 'serif')
par(mai = c(0.7,0.05,0.05, 0.1))
windRose(df, ws = 'wind_speed', wd = 'wind_dir', paddle = FALSE,
         par.settings=list(fontsize=list(text=8)), angle.scale = 45)
dev.off()

#### CO2 flux time series ####
path_fig <- file.path('/Users/Yusri/Documents/Work/Data_analysis/muka_head/figs/co2_flux_all.jpg')
jpeg(file=path_fig,width=16,height=8,res=400, units = 'cm')
par(family='Times', mar = c(4.1, 4.1, 0.1, 0.1))

plot(df$time_stamp[which(indexCO2_5 == TRUE)],
     df$co2_flux[which(indexCO2_5 == TRUE)], 
     xlab = '', ylab = '',
     type = 'l', cex.axis = 1, yaxt = 'n')
mtext(side = 1, 'Month', line = 2.5, cex = 1.5)
mtext(side = 2, expression(paste('CO'['2'],' flux')), 
      line = 2.1, cex = 1.5)
lines(c(as.POSIXct('2016-05-31 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
        as.POSIXct('2016-05-31 00:00:00', format = '%Y-%m-%d %H:%M:%S')),
      c(-10,10), lwd = 3, lty = 2)
text(as.POSIXct('2016-03-15 00:00:00', format = '%Y-%m-%d %H:%M:%S'), -4.3,
     'ENSO', cex = 1.5)
text(as.POSIXct('2016-08-15 00:00:00', format = '%Y-%m-%d %H:%M:%S'), -4.3,
     'non-ENSO', cex = 1.5)
axis(side = 1, at = c(as.POSIXct('2015-12-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
                      as.POSIXct('2016-02-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
                      as.POSIXct('2016-04-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
                      as.POSIXct('2016-06-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
                      as.POSIXct('2016-08-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
                      as.POSIXct('2016-10-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
                      as.POSIXct('2016-12-01 00:00:00', format = '%Y-%m-%d %H:%M:%S')),
     labels = c('Dec', 'Feb', 'Apr', 'Jun', 'Aug', 'Oct','Dec'), cex.axis = 1)
axis(side = 2, at = c(-4,-2,0,2,4), 
     labels = c(paste('\u2212',4,sep=''),paste('\u2212',2,sep=''),0,2,4))
minor.tick(nx = 1)
dev.off()

#### Boxplots for CO2 between ENSO and non-ENSO ####
path_fig <- file.path('/Users/Yusri/Documents/Work/Data_analysis/muka_head/figs/co2boxplot.jpg')
jpeg(file=path_fig,width=8,height=8,res=400, units = 'cm')
par(family='Times')
par(mar = c(2.1,4.1,0.1, 0.1))
boxplot(df$co2_flux[which(indexCO2_5_enso == TRUE)],
        df$co2_flux[which(indexCO2_5_xenso == TRUE)], outline = FALSE,
        names = c('ENSO','non-ENSO'), 
        ylab = '', cex.lab = 1.2, yaxt = 'n',
        cex.axis = 1.2, ylim = c(-0.5,0.5))
mtext(side = 2, expression(paste('CO'['2'],' flux')), 
      line = 2.1, cex = 1.2)
axis(side = 2, at = c(-0.4,-0.2,0,0.2,0.4),
     labels = c(paste('\u2212',0.4,sep=''),
                paste('\u2212',0.2,sep=''),
                0,0.2,0.4))
dev.off()


#### Overall diurnal CO2 flux plot ####
path_fig <- file.path('/Users/Yusri/Documents/Work/Data_analysis/muka_head/figs/co2_diurnal.jpg')
jpeg(file=path_fig,width=8,height=8,res=400, units = 'cm')
par(family='Times')
par(mar = c(3.1,3.4,0.1, 0.1))
plot(df_grp$hour, df_grp$co2_flux, type = 'l', ylim = c(-1,1.5), xlim = c(-1,25),
     xlab = '', ylab = '', xaxt = 'n')
mtext(side = 1, 'Hour (local time)', line = 2.1, cex = 1.2)
mtext(side = 2, expression(paste('CO'['2'],' flux')), 
      line = 2.1, cex = 1.2)
axis(side = 1, at = c(0,3,6,9,12,18,24), labels = c(0,3,6,9,12,18,24))
axis(side = 1, at = c(15,21), labels = c(15,21))

#hour <- df_grp$hour
#co2_down <- df_grp$co2_flux - df_grp$co2_flux_sd
#co2_up <- df_grp$co2_flux + df_grp$co2_flux_sd
#polygon(c(hour, rev(hour)), c(co2_up, rev(co2_down)),
#        col=adjustcolor("grey",alpha.f=0.5), border = NA)
dev.off()

#### Correlational analysis and plots between CO2 and physical drivers ####
# Sea surface temperature
path_fig <- file.path('/Users/Yusri/Documents/Work/Data_analysis/muka_head/figs/SSTandUcor.jpg')
jpeg(file=path_fig,width=16,height=8,res=400, units = 'cm')
par(family='Times', mfrow = c(1,2))
par(mar = c(3,3,0.5,0))
plot(df$TS_1_1_1[which(indexCO2_5_enso == TRUE)],
     df$co2_flux[which(indexCO2_5_enso == TRUE)], pch = 16,
     xlab = '', ylab = '',
     xlim= c(26,32), ylim = c(-1,1), col = alpha('red',0.2), yaxt = 'n')
points(df$TS_1_1_1[which(indexCO2_5_xenso == TRUE)],
       df$co2_flux[which(indexCO2_5_xenso == TRUE)], pch = 16,
       col = alpha('blue',0.2))
axis(side = 2, at = c(-1,-0.5,0,0.5,1), 
     labels = c(paste('\u2212',1,sep=''),paste('\u2212',0.5,sep=''),0,0.5,1))
minor.tick()
mtext(expression(paste('CO'['2'],' flux')),side = 2,line=2)
mtext(expression('T'['S']),side = 1,line=2)
lmT <- lm(df$co2_flux[which(indexCO2_5 == TRUE)] ~ 
            df$TS_1_1_1[which(indexCO2_5 == TRUE)])
lmTenso <- lm(df$co2_flux[which(indexCO2_5_enso == TRUE)] ~ 
                df$TS_1_1_1[which(indexCO2_5_enso == TRUE)])
lmTxenso <- lm(df$co2_flux[which(indexCO2_5_xenso == TRUE)] ~ 
                 df$TS_1_1_1[which(indexCO2_5_xenso == TRUE)])
abline(lmTenso, col = 'red', lwd = 3, lty = 2)
abline(lmTxenso, col = 'blue', lwd = 3, lty = 2)
abline(lmT, col = 'black', lwd = 3, lty = 2)
summary(lmT)
summary(lmTenso)
summary(lmTxenso)


# Wind speed
# path_fig <- file.path('/Users/Yusri/Documents/Work/Data_analysis/muka_head/figs/Ucor.jpg')
# jpeg(file=path_fig,width=8,height=8,res=400, units = 'cm')
# par(family='Times')
par(mar = c(3,2.5,0.5,0.5))
plot(df$wind_speed[which(indexCO2_5_enso == TRUE)],
     df$co2_flux[which(indexCO2_5_enso == TRUE)], pch = 16,
     xlab = '', ylab = '',
     xlim= c(0,4.5), ylim = c(-1,1), col = alpha('red',0.2),yaxt = 'n')
# points(df$wind_speed[which(indexCO2_5_enso == TRUE)],
#        df$co2_flux[which(indexCO2_5_enso == TRUE)], pch = 16,
#        col = alpha('red',0.2))
points(df$wind_speed[which(indexCO2_5_xenso == TRUE)],
       df$co2_flux[which(indexCO2_5_xenso == TRUE)], pch = 16,
       col = alpha('blue',0.2))
axis(side = 2, at = c(-1,-0.5,0,0.5,1), 
     labels = c(paste('\u2212',1,sep=''),paste('\u2212',0.5,sep=''),0,0.5,1))
#mtext(expression(paste('CO'['2'],' flux')),side = 2,line=2)
mtext('U',side = 1,line=2)
minor.tick()
lmU <- lm(df$co2_flux[which(indexCO2_5 == TRUE)] ~ 
            df$wind_speed[which(indexCO2_5 == TRUE)])
lmUenso <- lm(df$co2_flux[which(indexCO2_5_enso == TRUE)] ~ 
                df$wind_speed[which(indexCO2_5_enso == TRUE)])
lmUxenso <- lm(df$co2_flux[which(indexCO2_5_xenso == TRUE)] ~ 
                 df$wind_speed[which(indexCO2_5_xenso == TRUE)])
abline(lmUenso, col = 'red', lwd = 3, lty = 2)
abline(lmUxenso, col = 'blue', lwd = 3, lty = 2)
abline(lmU, col = 'black', lwd = 3, lty = 2)
summary(lmU)
summary(lmUenso)
summary(lmUxenso)
dev.off()

# Net radiation
path_fig <- file.path('/Users/Yusri/Documents/Work/Data_analysis/muka_head/figs/RNcor.jpg')
jpeg(file=path_fig,width=8,height=8,res=400, units = 'cm')
par(family='Times')
par(mar = c(4.1,4.1,0.5, 0.5))
plot(df$RN_1_1_1[which(indexCO2_5 == TRUE)],
     df$co2_flux[which(indexCO2_5 == TRUE)], pch = 19,
     xlab = 'RN', ylab = expression(paste('CO'['2'],' flux')),
     xlim= c(-100,300), ylim = c(-1,1), col = 'grey60')
axis(side = 2, at = c(-0.2,0), labels = c(-0.2,0))
minor.tick()
lmRN <- lm(df$co2_flux[which(indexCO2_5 == TRUE)] ~ 
             df$RN_1_1_1[which(indexCO2_5 == TRUE)])
lmRNenso <- lm(df$co2_flux[which(indexCO2_5_enso == TRUE)] ~ 
                 df$RN_1_1_1[which(indexCO2_5_enso == TRUE)])
lmRNxenso <- lm(df$co2_flux[which(indexCO2_5_xenso == TRUE)] ~ 
                  df$RN_1_1_1[which(indexCO2_5_xenso == TRUE)])
abline(lmRN, col = 'green', lwd = 3, lty = 2)
abline(lmRNenso, col = 'red', lwd = 3, lty = 2)
abline(lmRNxenso, col = 'blue', lwd = 3, lty = 2)
summary(lmRN)
summary(lmRNenso)
summary(lmRNxenso)
dev.off()


#### CO2_flux, SST, CHL and POC  ####
path_fig <- file.path('/Users/Yusri/Documents/Work/Data_analysis/muka_head/figs/RN_RG.jpg')
jpeg(file=path_fig,width=16,height=8,res=400, units = 'cm')
par(family='Times')
par(mai = c(0.7,0.6,0.05, 0.7))
plot(df$time_stamp, df$RG_1_1_1, type = 'l', 
     ylab = '', xlab = '', #ylim = c(22, 36),
     col = 'red')
axis(side = 1, at = c(as.POSIXct('2015-12-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
                      as.POSIXct('2016-02-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
                      as.POSIXct('2016-04-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
                      as.POSIXct('2016-06-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
                      as.POSIXct('2016-08-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
                      as.POSIXct('2016-10-01 00:00:00', format = '%Y-%m-%d %H:%M:%S')),
     labels = c('Dec', 'Feb', 'Apr', 'Jun', 'Aug', 'Oct'))
par(new = TRUE)
plot(df$time_stamp, df$RN_1_1_1, type = 'l',
     ylab = '', xlab = '', 
     col = 'orange')
dev.off()

#### Monthly boxplots of CO2 fluxes ####
path_fig <- file.path('/Users/Yusri/Documents/Work/Data_analysis/muka_head/figs/co2_box.jpg')
jpeg(file=path_fig,width=16,height=8,res=400, units = 'cm')
par(family='Times', mar = c(4.1, 4.1, 0.1, 0.1))
boxplot(df$co2_flux[indexCO2_5] ~ df$Month_class[indexCO2_5], 
        outline = F, yaxt = 'n',names = c('','Dec', '2016','Feb','',
                                          'Apr','','Jun','','Aug','',
                                          'Oct','','Dec','2017'))
axis(side = 1, at = c(3,15), labels = c('2016','2017'))
axis(side = 2, at = c(-0.4,-0.2,0,0.2,0.4), 
     labels = c(paste('\u2212',0.4,sep=''),paste('\u2212',0.2,sep=''),0,0.2,0.4))
mtext(side = 1, 'Month', line = 2.5, cex = 1.5)
mtext(side = 2, expression(paste('CO'['2'],' flux')), 
      line = 2.1, cex = 1.5)
lines(c(8,8),c(-1,1), lwd = 3, lty = 2)
minor.tick(nx=0)
dev.off()

boxplot(df$TS_2_1_1 ~ df$Month_class)
boxplot(df$wind_speed ~ df$Month_class)
boxplot(df$u.[indexCO2_5] ~ df$Month_class[indexCO2_5])

boxplot(df$precip~df$Month_class, outline = F)
lines(c(8,8),c(-1,12), lwd = 3, lty = 2)







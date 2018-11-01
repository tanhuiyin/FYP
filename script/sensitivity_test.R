##Sensitivity test for weekly, monthly and yearly timescale
##Library used
library(tidyverse)
library(lubridate)
library(dplyr)
library(Hmisc)

##Import dataset
combined_data <- read_csv('data/completed_data.csv')
combined_data <- combined_data[,-1]
date <- as.POSIXct(combined_data$date, format= "%Y-%m-%d %H:%M:%S")
combined_data <- cbind(date,combined_data)
combined_data <- combined_data[,-2]
summary(combined_data)

## Grouping data hourly##
grp_data_mean <- combined_data %>% 
  mutate(time_stamp=as.POSIXct(date)) %>%
  group_by(hour=format(as.POSIXlt(cut(time_stamp,breaks='hour')),'%H')) %>%
  summarise(co2_flux=mean(co2_flux,na.rm=TRUE),
            Site_temp=mean(TS_0_0_1,na.rm=TRUE),
            Chlorophyll=mean(CHL,na.rm=TRUE),
            Sea_Surface_Temp = mean(SST,na.rm=TRUE),
            POC = mean(POC,na.rm=TRUE),
            PIC = mean(PIC,na.rm=TRUE),
            PAR = mean(PAR,na.rm=TRUE),
            Precipitation = mean(HourlyPrecipMM,na.rm=TRUE))

grp_data_sd <- combined_data %>% 
  mutate(time_stamp=as.POSIXct(date)) %>%
  group_by(hour=format(as.POSIXlt(cut(time_stamp,breaks='hour')),'%H')) %>%
  summarise(co2_flux_sd=sd(co2_flux,na.rm=TRUE),
            Site_temp_sd=sd(TS_0_0_1,na.rm=TRUE),
            Chlorophyll_sd=sd(CHL ,na.rm=TRUE),
            Sea_Surface_Temp_sd= sd(SST, na.rm=TRUE),
            POC_sd = sd(POC,na.rm=TRUE),
            PIC_sd = sd(PIC,na.rm=TRUE),
            PAR_sd = sd(PAR,na.rm=TRUE),
            Precipitation_sd = sd(HourlyPrecipMM, na.rm=TRUE))

# Merge the two dataframe
hourly_grp_data <- merge(grp_data_mean, grp_data_sd, by='hour')
rm(grp_data_mean, grp_data_sd)

## Grouping data daily ##
daily_grp_data_mean <- combined_data %>% 
  mutate(time_stamp=as.POSIXct(date)) %>%
  group_by(day=format(as.POSIXlt(cut(time_stamp,breaks='day')),'%d')) %>%
  summarise(co2_flux=mean(co2_flux,na.rm=TRUE),
            Site_temp=mean(TS_0_0_1,na.rm=TRUE),
            Chlorophyll=mean(CHL,na.rm=TRUE),
            Sea_Surface_Temp = mean(SST,na.rm=TRUE),
            POC = mean(POC,na.rm=TRUE),
            PIC = mean(PIC,na.rm=TRUE),
            PAR = mean(PAR,na.rm=TRUE),
            Precipitation = mean(HourlyPrecipMM,na.rm=TRUE))

daily_grp_data_sd <- combined_data %>% 
  mutate(time_stamp=as.POSIXct(date)) %>%
  group_by(day=format(as.POSIXlt(cut(time_stamp,breaks='day')),'%d')) %>%
  summarise(co2_flux_sd=sd(co2_flux,na.rm=TRUE),
            Site_temp_sd=sd(TS_0_0_1,na.rm=TRUE),
            Chlorophyll_sd=sd(CHL ,na.rm=TRUE),
            Sea_Surface_Temp_sd= sd(SST, na.rm=TRUE),
            POC_sd = sd(POC,na.rm=TRUE),
            PIC_sd = sd(PIC,na.rm=TRUE),
            PAR_sd = sd(PAR,na.rm=TRUE),
            Precipitation_sd = sd(HourlyPrecipMM, na.rm=TRUE))

# Merge the two dataframe
daily_grp_data <- merge(daily_grp_data_mean,daily_grp_data_sd,by='day')
rm(daily_grp_data_mean,daily_grp_data_sd)

## Grouping data weekly ##
weekly_grp_data_mean <- combined_data %>% 
  mutate(time_stamp=as.POSIXct(date)) %>%
  group_by(week=format(as.POSIXlt(cut(time_stamp,breaks='7 days')),'%Y-%m-%d')) %>%
  summarise(co2_flux=mean(co2_flux,na.rm=TRUE),
            Site_temp=mean(TS_0_0_1,na.rm=TRUE),
            Chlorophyll=mean(CHL,na.rm=TRUE),
            Sea_Surface_Temp = mean(SST,na.rm=TRUE),
            POC = mean(POC,na.rm=TRUE),
            PIC = mean(PIC,na.rm=TRUE),
            PAR = mean(PAR,na.rm=TRUE),
            Precipitation = mean(HourlyPrecipMM,na.rm=TRUE))

weekly_grp_data_sd <- combined_data %>% 
  mutate(time_stamp=as.POSIXct(date)) %>%
  group_by(week=format(as.POSIXlt(cut(time_stamp,breaks='7 days')),'%Y-%m-%d')) %>%
  summarise(co2_flux_sd=sd(co2_flux,na.rm=TRUE),
            Site_temp_sd=sd(TS_0_0_1,na.rm=TRUE),
            Chlorophyll_sd=sd(CHL ,na.rm=TRUE),
            Sea_Surface_Temp_sd= sd(SST, na.rm=TRUE),
            POC_sd = sd(POC,na.rm=TRUE),
            PIC_sd = sd(PIC,na.rm=TRUE),
            PAR_sd = sd(PAR,na.rm=TRUE),
            Precipitation_sd = sd(HourlyPrecipMM, na.rm=TRUE))

# Merge the two dataframe
weekly_grp_data <- merge(weekly_grp_data_mean, weekly_grp_data_sd,by='week')
rm(weekly_grp_data_mean,weekly_grp_data_sd)


## Grouping data monthly ##
monthly_grp_data_mean <- combined_data %>% 
  mutate(time_stamp=as.POSIXct(date)) %>%
  group_by(month=format(as.POSIXlt(cut(time_stamp,breaks='month')),'%m')) %>%
  summarise(co2_flux=mean(co2_flux,na.rm=TRUE),
            Site_temp=mean(TS_0_0_1,na.rm=TRUE),
            Chlorophyll=mean(CHL,na.rm=TRUE),
            Sea_Surface_Temp = mean(SST,na.rm=TRUE),
            POC = mean(POC,na.rm=TRUE),
            PIC = mean(PIC,na.rm=TRUE),
            PAR = mean(PAR,na.rm=TRUE),
            Precipitation = mean(HourlyPrecipMM,na.rm=TRUE))

monthly_grp_data_sd <- combined_data %>% 
  mutate(time_stamp=as.POSIXct(date)) %>%
  group_by(month=format(as.POSIXlt(cut(time_stamp,breaks='month')),'%m')) %>%
  summarise(co2_flux_sd=sd(co2_flux,na.rm=TRUE),
            Site_temp_sd=sd(TS_0_0_1,na.rm=TRUE),
            Chlorophyll_sd=sd(CHL ,na.rm=TRUE),
            Sea_Surface_Temp_sd= sd(SST, na.rm=TRUE),
            POC_sd = sd(POC,na.rm=TRUE),
            PIC_sd = sd(PIC,na.rm=TRUE),
            PAR_sd = sd(PAR,na.rm=TRUE),
            Precipitation_sd = sd(HourlyPrecipMM, na.rm=TRUE))

# Merge the two dataframe
monthly_grp_data <- merge(monthly_grp_data_mean, monthly_grp_data_sd,by='month')
rm(monthly_grp_data_mean,monthly_grp_data_sd)

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
yearly_grp_data <- merge(yearly_grp_data_mean,yearly_grp_data_sd,by='year')
rm(yearly_grp_data_mean,yearly_grp_data_sd)

## Plotting the diurnal trend
library(ggplot2)
library(Hmisc)

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
cor.test(daily_grp_data$co2_flux,daily_grp_data$Site_temp)
cor.test(daily_grp_data$co2_flux,daily_grp_data$Chlorophyll)
cor.test(daily_grp_data$co2_flux,daily_grp_data$Sea_Surface_Temp)
cor.test(daily_grp_data$co2_flux,daily_grp_data$POC)
cor.test(daily_grp_data$co2_flux,daily_grp_data$PIC)
cor.test(daily_grp_data$co2_flux,daily_grp_data$PAR)
cor.test(daily_grp_data$Site_temp,daily_grp_data$Chlorophyll)
cor.test(daily_grp_data$Site_temp,daily_grp_data$Sea_Surface_Temp)
cor.test(daily_grp_data$Site_temp,daily_grp_data$POC)
cor.test(daily_grp_data$Site_temp,daily_grp_data$PIC)
cor.test(daily_grp_data$Site_temp,daily_grp_data$PAR)
cor.test(daily_grp_data$Chlorophyll,daily_grp_data$Sea_Surface_Temp)
cor.test(daily_grp_data$Chlorophyll,daily_grp_data$POC)
cor.test(daily_grp_data$Chlorophyll,daily_grp_data$PIC)
cor.test(daily_grp_data$Chlorophyll,daily_grp_data$PAR)
cor.test(daily_grp_data$Sea_Surface_Temp,daily_grp_data$POC)
cor.test(daily_grp_data$Sea_Surface_Temp,daily_grp_data$PIC)
cor.test(daily_grp_data$Sea_Surface_Temp,daily_grp_data$PAR)
cor.test(daily_grp_data$POC,daily_grp_data$PIC)
cor.test(daily_grp_data$POC,daily_grp_data$PAR)
cor.test(daily_grp_data$PAR,daily_grp_data$PIC)


##Monthly timescale
cor.test(monthly_grp_data$co2_flux,monthly_grp_data$Site_temp)
cor.test(monthly_grp_data$co2_flux,monthly_grp_data$Chlorophyll)
cor.test(monthly_grp_data$co2_flux,monthly_grp_data$Sea_Surface_Temp)
cor.test(monthly_grp_data$co2_flux,monthly_grp_data$POC)
cor.test(monthly_grp_data$co2_flux,monthly_grp_data$PIC)
cor.test(monthly_grp_data$co2_flux,monthly_grp_data$PAR)
cor.test(monthly_grp_data$Site_temp,monthly_grp_data$Chlorophyll)
cor.test(monthly_grp_data$Site_temp,monthly_grp_data$Sea_Surface_Temp)
cor.test(monthly_grp_data$Site_temp,monthly_grp_data$POC)
cor.test(monthly_grp_data$Site_temp,monthly_grp_data$PIC)
cor.test(monthly_grp_data$Site_temp,monthly_grp_data$PAR)
cor.test(monthly_grp_data$Chlorophyll,monthly_grp_data$Sea_Surface_Temp)
cor.test(monthly_grp_data$Chlorophyll,monthly_grp_data$POC)
cor.test(monthly_grp_data$Chlorophyll,monthly_grp_data$PIC)
cor.test(monthly_grp_data$Chlorophyll,monthly_grp_data$PAR)
cor.test(monthly_grp_data$Sea_Surface_Temp,monthly_grp_data$POC)
cor.test(monthly_grp_data$Sea_Surface_Temp,monthly_grp_data$PIC)
cor.test(monthly_grp_data$Sea_Surface_Temp,monthly_grp_data$PAR)
cor.test(monthly_grp_data$POC,monthly_grp_data$PIC)
cor.test(monthly_grp_data$POC,monthly_grp_data$PAR)
cor.test(monthly_grp_data$PAR,monthly_grp_data$PIC)

##Yearly timescale
cor.test(yearly_grp_data$co2_flux,yearly_grp_data$Site_temp)
cor.test(yearly_grp_data$co2_flux,yearly_grp_data$Chlorophyll)
cor.test(yearly_grp_data$co2_flux,yearly_grp_data$Sea_Surface_Temp)
cor.test(yearly_grp_data$co2_flux,yearly_grp_data$POC)
cor.test(yearly_grp_data$co2_flux,yearly_grp_data$PIC)
cor.test(yearly_grp_data$co2_flux,yearly_grp_data$PAR)
cor.test(yearly_grp_data$Site_temp,yearly_grp_data$Chlorophyll)
cor.test(yearly_grp_data$Site_temp,yearly_grp_data$Sea_Surface_Temp)
cor.test(yearly_grp_data$Site_temp,yearly_grp_data$POC)
cor.test(yearly_grp_data$Site_temp,yearly_grp_data$PIC)
cor.test(yearly_grp_data$Site_temp,yearly_grp_data$PAR)
cor.test(yearly_grp_data$Chlorophyll,yearly_grp_data$Sea_Surface_Temp)
cor.test(yearly_grp_data$Chlorophyll,yearly_grp_data$POC)
cor.test(yearly_grp_data$Chlorophyll,yearly_grp_data$PIC)
cor.test(yearly_grp_data$Chlorophyll,yearly_grp_data$PAR)
cor.test(yearly_grp_data$Sea_Surface_Temp,yearly_grp_data$POC)
cor.test(yearly_grp_data$Sea_Surface_Temp,yearly_grp_data$PIC)
cor.test(yearly_grp_data$Sea_Surface_Temp,yearly_grp_data$PAR)
cor.test(yearly_grp_data$POC,yearly_grp_data$PIC)
cor.test(yearly_grp_data$POC,yearly_grp_data$PAR)
cor.test(yearly_grp_data$PAR,yearly_grp_data$PIC)


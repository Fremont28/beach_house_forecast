#8/13/18 zillow forecasting (beach real estate)
library(forecast)
library(ggplot2)
library(plyr)
library(dplyr)
library(tidyr)
library(lubridate)

counties=read.csv("cape_may_county.csv")

long_counties=counties %>% gather(RegionName,Metro,SizeRank)
long_counties1=melt(long_counties,id.vars=c("RegionName","Metro"))

#long to wide format 
library(reshape2)
library(zoo)
install.packages("anytime") 
library(anytime)
library(tseries)
counties_long<- melt(counties, id.vars = c("RegionName", "Metro","State",
                                           "StateCodeFIPS","MunicipalCodeFIPS",
                                           "SizeRank"))
counties_long$variable=substr(counties_long$variable,start=2,stop=8)
counties_long$variable=anydate(counties_long$variable)
str(counties_long)

#subset
cape_may=subset(counties_long,RegionName=="Cape May")
dim(cape_may) #102 monthly observations? 
ggplot(cape_may,aes(x=variable,y=value))+geom_point() 

#time series object 
cape_may_ts=xts(cape_may$value,order.by=as.POSIXct(cape_may$variable),format="Y%m%d")

#1.test for stationarity
adf.test(cape_may_ts1,k=0) #p=0.54 fail to reject Ho of non-stationairty 
#auto-correlations 
acf2(cape_may_ts)

#transform the series into stationary por subtracting Xt-1 from Xt for all values t
#(differencing)
plot(diff(cape_may_ts))
#sarima (p,d,q)
sarima(cape_may_ts,0,1,1)
sarima.for(cape_may_ts,12,0,1,1) #new five months forecast (data through june 2018)

#split time series data into quarters
cm_ts_1=ts(cape_may_ts,start=c(2010,01,01),end=c(2018,06,01),frequency=12)
adf.test(cm_ts_1)

#difference the time series (the time series is stationary)
cm_ts_11=diff(cm_ts_1)
adf.test(cm_ts_11)

#plot the differenced time series
plot(diff(cm_ts_1))
acf2(diff(cm_ts_1)) #all significant correlation removed between the lags

#sarima (p,d,q)-auto-regressive order, degree of differencing, and moving average order
sarima(cm_ts_1,0,1,0)
#forecast
sarima.for(cm_ts_1,12,0,1,0)
summary(cape_may_ts)

#virginia beach city
vir_beach=subset(counties_long,RegionName=="Virginia Beach City")
vir_beach_ts=xts(vir_beach$value,order.by=as.POSIXct(vir_beach$variable),format="Y%m%d")

vir_ts_1=ts(vir_beach_ts,start=c(2010,01,01),end=c(2018,06,01),frequency=12)
adf.test(vir_ts_1) # p<0.05 time series is stationary
plot(vir_ts_1)

sarima(vir_ts_1,0,1,0) 
sarima.for(vir_ts_1,12,0,1,0)

ggplot(vir_beach,aes(x=variable,y=value))+geom_line()+
  ylab("Median House Price ($USD)")+xlab("Date")+
  ggtitle("Virginia Beach House Prices Are on the Rise")+
  theme(plot.title = element_text(hjust = 0.5))


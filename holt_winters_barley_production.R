library(lubridate)
library(forecast)
library(tseries)
barley<-read.csv("annual-barley-yields-per-acre-in.csv",stringsAsFactors = F)
View(barley)

barley<-proper_feature_names(barley)
#####reading structure of the data set
str(barley)
colSums(is.na(barley))
barley<-na.omit(barley)
barley$year<-as.Date(barley$year,"%Y")
barley$year<-year(barley$year)
barley$year<-as.factor(barley$year)

ts.barley<-ts(data=barley[,2],start = c(1884))
decomp.barley<-decompose(ts.barley)
plot(decomp.barley)
plot(ts.barley)
#####
ggplot(barley,aes(x=year,y=annual_barley_yields_per_acre_in_england_wales_1884_1939,group=1))+
  geom_line(linetype="dashed",color="green",size=1.4)+
  geom_point(color="red",size=1)+
  theme(axis.text.x = element_text(angle=60,vjust=0.5))+
  ylab("barley yield")
ggplotly()


####making data stationary
plot(diff(ts.barley))
plot(log10(ts.barley))
lag.plot(ts.barley,lags=9,do.lines = F)
#### variance is constant so need to log our time series
pacf(diff(ts.barley))
pacf(ts.barley)
####variance and mean seems to be constant here
kpss.test(ts.barley)
####kpss test is greter p value
###
forecast_barley<-HoltWinters(ts.barley,beta=F,gamma=F)
forecast_barley
pred<-forecast.HoltWinters(forecast_barley,h=8)
acf(pred$residuals,lag.max = 20)
Box.test(pred$residuals,lag=20,type="Ljung-Box")
plot.ts(pred$residuals)
####there is no auto-correlations

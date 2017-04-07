library(lubridate)#date manipulation
library(data.table)#data manipulation
library(ggplot2)# visualization
library(forecast)# time series methods
library(forecastHybrid)##time series using weighted methods(ensemble of different forecasting methods)


##reading the data
barley_data<-fread("barley_tm.csv")
View(barley_data)
str(barley_data)
summary(barley_data$export)
barley_data$week_dates<-mdy(barley_data$week_dates)
barley_data$year<-year(barley_data$week_dates)
barley_data$month<-month(barley_data$week_dates)
write.csv(barley_data,"barley_data.csv",row.names = F)
barley_data<-na.omit(barley_data)
###maipulating monthly forecast,monthly max sales, 2week sales

agg_barley_month_data<-barley_data[,.(total_sales=sum(export)),by=list(month,year)]
View(agg_barley_month_data)
write.csv(agg_barley_month_data,"agg_barley-month_data.csv",row.names = F)
#agg_barley_month_data<-na.omit(agg_barley_month_data)
agg_barley_max_data<-barley_data[,.(max_sales=max(export)),by=list(month,year)]
write.csv(agg_barley_max_data,"agg_barley_max_data.csv",row.names = F)
View(agg_barley_max_data)
n<-2
barley_data$agg_lev<-rep(1:159,each=n)

agg_2week_data<-barley_data[,.(total_sales=sum(export)),by=.(agg_lev)]
View(agg_2week_data)
write.csv(agg_2week_data,"agg_2week_data.csv",row.names = F)

############################################################################################
############################################################################################
############################################################################################
#montly forecasting
agg_month_ts<-ts(agg_barley_month_data$total_sales,start=c(2010,1),frequency = 12)
plot.ts(agg_month_ts,col="tan2",xlim=c(2010,2016))
decomposed_data<-decompose(agg_month_ts)
plot(decomposed_data,col="plum4")
plot(decomposed_data$seasonal,col="blue")#seasonal component
plot(decomposed_data$trend,col="red")# trend component

plot(acf(agg_month_ts))#these two plot shows relation with previous lags
plot(pacf(agg_month_ts))



###arima,holt,tbats, neural nets and hybrid model applied, and check with training accuracy

arima_aggmonth_data<-auto.arima(agg_month_ts)
accuracy(arima_aggmonth_data)
forecast_arima_aggmonth_data<-forecast.Arima(arima_aggmonth_data,h=8)
autoplot(forecast_arima_aggmonth_data)
plot.ts(agg_month_ts)
lines(forecast_arima_aggmonth_data$fitted,col="red")


holt_aggmonth_data<-HoltWinters(agg_month_ts)
sqrt((holt_aggmonth_data$SSE)/nrow(agg_barley_month_data))
forecast_holt_aggmonth_Data<-forecast.HoltWinters(holt_aggmonth_data,h=8)
autoplot(forecast_holt_aggmonth_Data)
##lets check how well the curve fits
plot.ts(agg_month_ts)
lines(forecast_holt_aggmonth_Data$fitted,col="red")


tbats_agg_data<-tbats(agg_month_ts)
accuracy(tbats_agg_data)
forecast_tbats<-forecast.tbats(tbats_agg_data,h=8)
plot.ts(agg_month_ts)
lines(forecast_tbats$fitted,col="red")


nnetar_aggmonth_data<-nnetar(agg_month_ts)
accuracy(nnetar_aggmonth_data)
forecast_nnetar_aggmonth_data<-forecast.nnetar(nnetar_aggmonth_data,h=8)
autoplot(forecast_nnetar_aggmonth_data)
plot.ts(agg_month_ts)
lines(forecast_nnetar_aggmonth_data$fitted,col="red")



###hybrid model takes ensembling of time series of models
hybrid_agg_data<-hybridModel(agg_month_ts)
#plot(forecast(hybrid_allrice))
accuracy(hybrid_agg_data)
forecast_hybrd<-forecast(hybrid_agg_data,h=16)
checkresiduals(forecast_hybrd)
plot.ts(agg_month_ts)
lines(forecast_hybrd$fitted,col="red")


###none of the methods are fitting nicely to the actual data, so we should not be doing montly
##forecasting here
############################################################################################
#############################################################################################
##############################################################################
#max sales monthly forecasting
agg_barley_max_data_ts<-ts(agg_barley_max_data$max_sales,start = c(2010,1),frequency = 12)
plot.ts(agg_barley_max_data_ts,col="violet")
plot(decompose(agg_barley_max_data_ts),col="yellow4")



#####arima model

arima_agg_data<-auto.arima(agg_barley_max_data_ts)
accuracy(arima_agg_data)#checking the accuracy of the fitted model
checkresiduals(arima_agg_data)## residuals should not have any pattern, mean should be 0 not skewed

forecast_arima<-forecast.Arima(arima_agg_data,h=8)# forecasting for next 8months
autoplot(forecast_arima)

plot.ts(agg_barley_max_data_ts)
lines(forecast_arima$fitted,col="red")#these two codes are to visualize how the fitted values resemble the plot


#####exponential smoothening 
holt_agg_data<-HoltWinters(agg_barley_max_data_ts)
forecast_holt<-forecast.HoltWinters(holt_agg_data,h=8)
autoplot(forecast_holt)
checkresiduals(holt_agg_data)
sqrt((holt_agg_data$SSE)/nrow(agg_barley_max_data))#accuracy of the model

plot.ts(agg_month_data_ts)
lines(forecast_holt$fitted,col="red")



##tbats model
tbats_agg_data<-tbats(agg_barley_max_data_ts)
accuracy(tbats_agg_data)
forecast_tbats<-forecast.tbats(tbats_agg_data,h=8)
autoplot(forecast_tbats)

plot.ts(agg_barley_max_data_ts)
lines(forecast_tbats$fitted,col="red")



###neural net model

nnnet_agg_data<-nnetar(agg_barley_max_data_ts)
accuracy(nnnet_agg_data)
forecast_nnet<-forecast.nnetar(nnnet_agg_data,h=8)
checkresiduals(nnnet_agg_data)
autoplot(forecast_nnet)
plot.ts(agg_barley_max_data_ts)
lines(forecast_nnet$fitted,col='red')
##neural nets seems to work perfectly


###hybrid model takes ensembling of time series of models
hybrid_agg_data<-hybridModel(agg_barley_max_data_ts)
accuracy(hybrid_agg_data)
forecast_hybrd<-forecast(hybrid_agg_data,h=16)
checkresiduals(forecast_hybrd)
plot.ts(agg_barley_max_data_ts)
lines(forecast_hybrd$fitted,col="red")



######################################################################################
#############################################################################
###############################################################################
#quarterly forecasting

agg_month_data_ts<-aggregate(agg_month_ts,nfrequency = 4)

plot.ts(agg_month_data_ts,col='orangered')


plot(decompose(agg_month_data_ts),col="maroon4")

plot(acf(agg_month_data_ts))
plot(pacf(agg_month_data_ts))
#these two plots tell if there a pattern exist in our data


#####arima model


arima_agg_data<-auto.arima(agg_month_data_ts)
accuracy(arima_agg_data)#checking the accuracy of the fitted model
checkresiduals(arima_agg_data)## residuals should not have any pattern, mean should be 0 not skewed

forecast_arima<-forecast.Arima(arima_agg_data,h=8)# forecasting for next 8months
autoplot(forecast_arima)

plot.ts(agg_month_data_ts)
lines(forecast_arima$fitted,col="red")#these two codes are to visualize how the fitted values resemble the plot


#####exponential smoothening 
holt_agg_data<-HoltWinters(agg_month_data_ts)
forecast_holt<-forecast.HoltWinters(holt_agg_data,h=8)
autoplot(forecast_holt)
checkresiduals(holt_agg_data)
sqrt((holt_agg_data$SSE)/24)#accuracy of the model


plot.ts(agg_month_data_ts)
lines(forecast_holt$fitted,col="red")

##holt winters giving the best accuracy

##tbats model
tbats_agg_data<-tbats(agg_month_data_ts)
accuracy(tbats_agg_data)
forecast_tbats<-forecast.tbats(tbats_agg_data,h=8)
autoplot(forecast_tbats)

plot.ts(agg_month_data_ts)
lines(forecast_tbats$fitted,col="red")



###neural net model

nnnet_agg_data<-nnetar(agg_month_data_ts)
accuracy(nnnet_agg_data)
forecast_nnet<-forecast.nnetar(nnnet_agg_data,h=8)
checkresiduals(nnnet_agg_data)
autoplot(forecast_nnet)
plot.ts(agg_month_data_ts)
lines(forecast_nnet$fitted,col='red')
##neural nets seems to work perfectly


###hybrid model takes ensembling of time series of models
hybrid_agg_data<-hybridModel(agg_month_data_ts)
accuracy(hybrid_agg_data)
forecast_hybrd<-forecast(hybrid_agg_data,h=16)
checkresiduals(forecast_hybrd)
plot.ts(agg_month_data_ts)
lines(forecast_hybrd$fitted,col="red")




####################################################################################
#####################################################################################
#####################################################################################
#weekly forecasting

agg_month_data_ts<-ts(barley_data$export,start = decimal_date(ymd("2010-01-07")),frequency = 52)

plot.ts(agg_month_data_ts,col="red")

plot(decompose(agg_month_data_ts),col="maroon4")

plot(acf(agg_month_data_ts))
plot(pacf(agg_month_data_ts))
#these two plots tell if there a pattern exist in our data


#####arima model

arima_agg_data<-auto.arima(agg_month_data_ts)
accuracy(arima_agg_data)#checking the accuracy of the fitted model
checkresiduals(arima_agg_data)## residuals should not have any pattern, mean should be 0 not skewed

forecast_arima<-forecast.Arima(arima_agg_data,h=8)# forecasting for next 8months
autoplot(forecast_arima)

plot.ts(agg_month_data_ts)
lines(forecast_arima$fitted,col="red")#these two codes are to visualize how the fitted values resemble the plot


#####exponential smoothening 
holt_agg_data<-HoltWinters(agg_month_data_ts)
forecast_holt<-forecast.HoltWinters(holt_agg_data,h=8)
autoplot(forecast_holt)
checkresiduals(holt_agg_data)
sqrt((holt_agg_data$SSE)/nrow(barley_data))#accuracy of the model

plot.ts(agg_month_data_ts)
lines(forecast_holt$fitted,col="red")



##tbats model
tbats_agg_data<-tbats(agg_month_data_ts)
accuracy(tbats_agg_data)
forecast_tbats<-forecast.tbats(tbats_agg_data,h=8)
autoplot(forecast_tbats)

plot.ts(agg_month_data_ts)
lines(forecast_tbats$fitted,col="red")



###neural net model

nnnet_agg_data<-nnetar(agg_month_data_ts)
accuracy(nnnet_agg_data)
forecast_nnet<-forecast.nnetar(nnnet_agg_data,h=8)
checkresiduals(nnnet_agg_data)
autoplot(forecast_nnet)
plot.ts(agg_month_data_ts)
lines(forecast_nnet$fitted,col='red')
##neural nets seems to give nice accuracy



###hybrid model takes ensembling of time series of models
hybrid_agg_data<-hybridModel(agg_month_data_ts)
accuracy(hybrid_agg_data)
forecast_hybrd<-forecast(hybrid_agg_data,h=16)
checkresiduals(forecast_hybrd)
plot.ts(agg_month_data_ts)
lines(forecast_hybrd$fitted,col="red")


###########################################################################################
##########################################################################################
##############################################################################
#2week forecasting


agg_month_data_ts<-ts(agg_2week_data$total_sales,start = decimal_date(ymd("2010-01-07")),frequency = 26)

plot.ts(agg_month_data_ts,col='blue')

plot(decompose(agg_month_data_ts))

plot(acf(agg_month_data_ts))
plot(pacf(agg_month_data_ts))
#these two plots tell if there a pattern exist in our data


#####arima model

arima_agg_data<-auto.arima(agg_month_data_ts)
accuracy(arima_agg_data)#checking the accuracy of the fitted model
checkresiduals(arima_agg_data)## residuals should not have any pattern, mean should be 0 not skewed

forecast_arima<-forecast.Arima(arima_agg_data,h=8)# forecasting for next 8months
autoplot(forecast_arima)

plot.ts(agg_month_data_ts)
lines(forecast_arima$fitted,col="red")#these two codes are to visualize how the fitted values resemble the plot


#####exponential smoothening 
holt_agg_data<-HoltWinters(agg_month_data_ts)
forecast_holt<-forecast.HoltWinters(holt_agg_data,h=8)
autoplot(forecast_holt)
checkresiduals(holt_agg_data)
sqrt((holt_agg_data$SSE)/nrow(barley_data))#accuracy of the model

plot.ts(agg_month_data_ts)
lines(forecast_holt$fitted,col="red")



##tbats model
tbats_agg_data<-tbats(agg_month_data_ts)
accuracy(tbats_agg_data)
forecast_tbats<-forecast.tbats(tbats_agg_data,h=8)
autoplot(forecast_tbats)

plot.ts(agg_month_data_ts)
lines(forecast_tbats$fitted,col="red")



###neural net model

nnnet_agg_data<-nnetar(agg_month_data_ts)
accuracy(nnnet_agg_data)
forecast_nnet<-forecast.nnetar(nnnet_agg_data,h=8)
checkresiduals(nnnet_agg_data)
autoplot(forecast_nnet)
plot.ts(agg_month_data_ts)
lines(forecast_nnet$fitted,col='red')
##neural nets seems to work perfectly


###hybrid model takes ensembling of time series of models
hybrid_agg_data<-hybridModel(agg_month_data_ts)
accuracy(hybrid_agg_data)
forecast_hybrd<-forecast(hybrid_agg_data,h=16)
checkresiduals(forecast_hybrd)
plot.ts(agg_month_data_ts)
lines(forecast_hybrd$fitted,col="red")














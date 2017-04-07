
corn_data<-fread("corn_tm.csv")
View(corn_data)
corn_data$week_date<-mdy(corn_data$week_date)
corn_data$sales<-corn_data$sales/1000
corn_data$sales<-round(corn_data$sales)

corn_data$year<-year(corn_data$week_date)
corn_data$month<-month(corn_data$week_date)

agg_month_data<-corn_data[,.(total_sales=sum(sales)),by=.(month,year)]
View(agg_month_data)
agg_max_month_data<-corn_data[,.(max_sales=max(sales)),by=.(month,year)]
View(agg_max_month_data)

###monthly forecasting
agg_month_data<-agg_month_data[241:312]
agg_month_ts<-ts(agg_month_data$total_sales,start =c(2010,1),frequency =12)
plot.ts(agg_month_ts)
plot(decompose(agg_month_ts))

arima_agg_data<-auto.arima(agg_month_ts)
forecast_arima<-forecast.Arima(arima_agg_data,h=8)
accuracy(arima_agg_data)
autoplot(forecast_arima)
plot.ts(agg_month_ts)
lines(forecast_arima$fitted,col="red")


holt_agg_data<-HoltWinters(agg_month_ts)
forecast_holt<-forecast.HoltWinters(holt_agg_data,h=8)
autoplot(forecast_holt)
sqrt((holt_agg_data$SSE)/nrow(agg_month_data))
plot.ts(agg_month_ts)
lines(forecast_holt$fitted,col="red")

nnetar_agg_data<-nnetar(agg_month_ts)
forecast_nnet<-forecast.nnetar(nnetar_agg_data,h=8)
autoplot(forecast_nnet)
accuracy(nnetar_agg_data)
plot.ts(agg_month_ts)
lines(forecast_nnet$fitted,col="red")




###
agg_max_month_data<-agg_max_month_data[241:312]
agg_max_month_ts<-ts(agg_max_month_data$max_sales,start=c(2010,1),frequency = 12)
plot.ts(agg_max_month_ts)
plot(decompose(agg_max_month_ts))


arima_max_data<-auto.arima(agg_max_month_ts)
forecast_arima<-forecast.Arima(arima_max_data,h=8)
accuracy(arima_max_data)
autoplot(forecast_arima)
plot.ts(agg_max_month_ts)
lines(forecast_arima$fitted,col="red")


holt_max_data<-HoltWinters(agg_max_month_ts)
forecast_holt<-forecast.HoltWinters(holt_max_data,h=8)
autoplot(forecast_holt)
plot.ts(agg_max_month_ts)
lines(forecast_holt$fitted,col='red')


nnetar_month_data<-nnetar(agg_max_month_ts)
forecast_nnet<-forecast.nnetar(nnetar_month_data,h=8)
accuracy(nnetar_month_data)
autoplot(forecast_nnet)
plot.ts(agg_max_month_ts)
lines(forecast_nnet$fitted,col="red")

tbats_month_data<-stl(agg_max_month_ts,s.window = "periodic")
forecast_tbats<-forecast.stl(tbats_month_data,h=8)
autoplot(forecast_tbats)
plot.ts(agg_max_month_ts)
accuracy(forecast_tbats)
lines(forecast_tbats$fitted,col="red")


####quartely forecasting now
agg_quarterly<-aggregate(agg_month_ts,nfrequency = 4)
plot.ts(agg_quarterly)
plot(decompose(agg_quarterly))


arima_quartely_data<-auto.arima(agg_quarterly)
forecast_quarter_arima<-forecast.Arima(arima_quartely_data,h=8)
autoplot(forecast_quarter_arima)
accuracy(arima_quartely_data)
plot.ts(agg_quarterly)
lines(forecast_quarter_arima$fitted,col='red')


holt_quarterly_data<-HoltWinters(agg_quarterly)
forecast_quarter_holt<-forecast.HoltWinters(holt_quarterly_data,h=8)
autoplot(forecast_quarter_holt)
plot.ts(agg_quarterly)
lines(forecast_quarter_holt$fitted,col="red")

nnetar_quarterly_data<-nnetar(agg_quarterly)
forecast_quarter_nnetar<-forecast.nnetar(nnetar_quarterly_data,h=8)
autoplot(forecast_quarter_nnetar)
plot.ts(agg_quarterly)
lines(forecast_quarter_nnetar$fitted,col="red")


####weekly forecasting





corn_weekly_data<-corn_data[1062:1441]
weekly_data_ts<-ts(corn_weekly_data$sales,start=decimal_date(ymd("2010-01-07")),frequency = 52)
plot.ts(weekly_data_ts)
plot(decompose(weekly_data_ts))

arima_weekly<-auto.arima(weekly_data_ts)
forecast_arima<-forecast.Arima(arima_weekly,h=8)
autoplot(forecast_arima)
plot.ts(weekly_data_ts)
lines(forecast_arima$fitted,col="red")

nnetar_weekly<-nnetar(weekly_data_ts)
forecast_nnetar<-forecast.nnetar(nnetar_weekly,h=8)
lines(forecast_nnetar$fitted,col="red")






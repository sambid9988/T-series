library(forecast)
library(forecastHybrid)
library(ggplot2)


data_time<-fread("soybeans.csv")

data_ts<-ts(data_time$sales,start =decimal_date(ymd("2012-01-05")),frequency = 52)

stl_df<-stl(data_ts,s.window = "periodic")

all_data<-cbind(data_time,data.frame(stl_df$time.series))

write.csv(all_data,"soybeans_decomposed.csv",row.names = F)

plot(stl_df,col='tomato',lwd=3)

###

model_holt<-HoltWinters(data_ts)
forecast_holt<-forecast.HoltWinters(model_holt,h=8)
mean(forecast_holt$residuals,na.rm = T)
plot(forecast_holt,col="orange",lwd=3)

write.csv(data.frame(forecast_holt),"soybeans_forecasted.csv",row.names = F)

model_msts<-msts(data_ts,seasonal.periods = c(7,365.25))
model_tbats<-tbats(model_msts)
accuracy(model_tbats)
mean(forecast_tbats$residuals)
forecast_tbats<-forecast.tbats(model_tbats,h=8)
plot(forecast_tbats,col="green",lwd=3)

write.csv(data.frame(forecast_tbats),"wheat_hard_red_winter_forecasted.csv",row.names = F)




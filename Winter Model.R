##Holt-Winter Exponential Smoothing
source("./defFunction.R")

rm(list = ls()) # clean environment variables
Epg <- read.csv("EPG.csv", header = F)
EpgTs <- ts(Epg, frequency = 12, start = c(2015,1))

#using xts plot
library(xts)
plot(as.xts(EpgTs), major.format = "%Y-%m")

#using normal plot
plot(EpgTs, xaxt = "n")
tsp = attributes(EpgTs)$tsp
dates = seq(as.Date("2015-01-01"), by = "month", along = EpgTs)
axis(1, at = seq(tsp[1], tsp[2], along = EpgTs), labels = format(dates, "%Y-%m"))

#decomposing time series data
plot(decompose(EpgTs), xaxt = "n")
axis(1, at = seq(tsp[1], tsp[2], along = EpgTs), labels = format(dates, "%Y-%m"))

# apply HoltWinter for historical data
WinterEpgTs<- HoltWinters(EpgTs, alpha = TRUE, beta = FALSE, gamma = FALSE)
WinterEpgTs$fitted # the strored forecasted data
plot(WinterEpgTs, xaxt = "n")
axis(1, at = seq(tsp[1], tsp[2], along = WinterEpgTs), labels = format(dates, "%Y-%m"))
WinterEpgTs$SSE

#forecast and plot
library(forecast)
WinterEpgTsForecast <- forecast.HoltWinters(WinterEpgTs, h=12)
plot.forecast(WinterEpgTsForecast, xaxt = "n")
a = seq(as.Date("2015-01-01"), by="month", length=36)
axis(1, at = decimal_date(a), labels = format(a, "%Y-%m"), cex.axis=0.6)
abline(v = decimal_date(a), col='grey', lwd=0.5)

#evaluating forecast error
acf(WinterEpgTsForecast$residuals[13:24,1],lag.max = 11) # show which lags touch significant bounds
Box.test(WinterEpgTsForecast$residuals[13:24,1], lag=11, type="Ljung-Box") # show evidence of autocorrelation

#plot redidual to check variance and mean
ResidualTs = ts(WinterEpgTsForecast$residuals[13:24,1],frequency = 12, start = c(2016,1))
plot.ts(ResidualTs, xaxt = "n") #show variance
tsp2 = attributes(ResidualTs)$tsp
dates2 = seq(as.Date("2016-01-01"), by = "month", along = ResidualTs)
axis(1, at = seq(tsp2[1], tsp2[2], along = ResidualTs), labels = format(dates2, "%Y-%m"))

plotForecastErrors(WinterEpgTsForecast$residuals[13:24,1]) # show mean in normal distribution graph

# save to csv
write.csv(WinterEpgTsForecast, file = "WinterEpgTsForecast.csv")

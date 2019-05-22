library(forecast)
fit <- auto.arima(WWWusage)
checkresiduals(fit)
autoplot(WWWusage)
WWWusage
fit <- ets(woolyrnq)
woolyrnq
ets(woolyrnq)
autoplot(ets(woolyrnq))
stlm(woolyrnq)
res <- cbind(Residuals = residuals(fit), Response.residuals = residuals(fit, type='response'))
autoplot(res, facets=T)
lynx
gghistogram(lynx)
ggseasonplot(USAccDeaths)
ggseasonplot(USAccDeaths, polar=T)

WWWusage %>% ets %>% forecast(h=20) -> fc
autoplot(WWWusage, series='Data') + 
  autolayer(fc, series='Forecast') +
  autolayer(fitted(fc), series='Fitted')

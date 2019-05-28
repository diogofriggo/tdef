y <- ts(c(123,39,78,52,110), start=2012)
library(tidyverse)
library(forecast)
#install.packages('fpp')
library(fpp)
melsyd[,'First.Class'] %>% autoplot() #syntax
melsyd %>% autoplot() + ggtitle("Economy class passengers: Melbourne-Sydney") +
  xlab("Year") +
  ylab("Thousands")

autoplot(a10) +
  ggtitle("Antidiabetic drug sales") +
  ylab("$ million") +
  xlab("Year")

a10 %>% ggseasonplot(year.labels=T, year.labels.left = T) + 
  ylab('$ million') +
  ggtitle("Seasonal plot: antidiabetic drug sales")

a10 %>% ggseasonplot(polar=T) + 
  ylab('$ million') +
  ggtitle("Seasonal plot: antidiabetic drug sales")

ggsubseriesplot(a10) +
  ylab('$ million') + 
  ggtitle("Seasonal plot: antidiabetic drug sales")
#install.packages('fpp2')
library(fpp2)
elecdemand
elecdemand %>% autoplot()
autoplot(elecdemand[,c('Demand','Temperature')], facets=T) + 
  xlab("Year: 2014") + ylab("") +
  ggtitle("Half-hourly electricity demand: Victoria, Australia")

rm(Temperature, Demand)
qplot(Temperature, Demand, data=as.data.frame(elecdemand))
qplot(Temperature, Demand, data=as_tibble(elecdemand))
ggplot(as_tibble(elecdemand)) + geom_point(aes(Temperature, Demand))

autoplot(visnights[,1:5],facets=T) + ylab("Number of visitor nights each quarter (millions)")

#install.packages('GGally')
GGally::ggpairs(as_tibble(visnights[,1:5]))

ausbeer %>% autoplot()
window(ausbeer, start=1992) %>% gglagplot()
window(ausbeer, start=1992) %>% ggAcf()
ausbeer %>% ggAcf()

aelec <- window(elec, start=1980)
autoplot(aelec) + xlab("Year") + ylab("GWh")

aelec %>% ggAcf(lag=48)

set.seed(30)
y <- ts(rnorm(50))
autoplot(y) + ggtitle("White noise")
ggAcf(y)

#meanf(y,h)
#naive(y,h)
#rwf(y,h)
#snaive(y, h)

beer2 <- window(ausbeer, start=1992)
autoplot(beer2) + 
  autolayer(meanf(beer2, h=11), series='Mean', PI=F) +
  autolayer(naive(beer2, h=11), series='Naïve', PI=F) +
  autolayer(snaive(beer2, h=11), series='Seasonal naïve', PI=F) + 
  ggtitle("Forecasts for quarterly beer production") +
  xlab("Year") + ylab("Megalitres") +
  guides(colour=guide_legend(title="Forecast"))

autoplot(goog200) + 
  autolayer(meanf(goog200, h=40), series='Mean', PI=F) +
  autolayer(rwf(goog200, h=40), series='Naïve', PI=F) +
  autolayer(rwf(goog200, h=40, drift=T), series='Drift', PI=F) + 
  ggtitle("Forecasts for quarterly beer production") +
  xlab("Year") + ylab("Megalitres") +
  ggtitle("Google stock (daily ending 6 Dec 2013)") +
  xlab("Day") + ylab("Closing Price (US$)") +
  guides(colour=guide_legend(title="Forecast"))

milk
dframe <- cbind(Monthly = milk, DailyAverage = milk/monthdays(milk))
dframe
autoplot(dframe, facet=T) + xlab("Years") + ylab("Pounds") +
  ggtitle("Milk production per cow")

lambda <- BoxCox.lambda(elec)
df <- cbind(BoxCox(elec, lambda), elec)
df
autoplot(df, facet=T)

fc <- rwf(eggs, drift=T, lambda=0, h=50, level=80)
fc2 <- rwf(eggs, drift=T, lambda=0, h=50, level=80, biasadj = T)
autoplot(eggs) + 
  autolayer(fc, series="Simple back transformation") + 
  #autolayer(fc2, series="Bias adjusted", PI=F) +
  autolayer(fc2, series="Bias adjusted", PI=F) + 
  guides(colour=guide_legend(title='Forecast'))

autoplot(goog200) + xlab("Day") + ylab("Closing Price (US$)") +
  ggtitle("Google Stock (daily ending 6 December 2013)")

res <- residuals(naive(goog200))
autoplot(res) + xlab("Day") + ylab("") +
  ggtitle("Residuals from naïve method")

gghistogram(res) + ggtitle("Histogram of residuals")
gghistogram(ts(res[res < 20])) + ggtitle("Histogram of residuals")

ggAcf(res) + ggtitle("ACF of residuals")
mean(res,na.rm = T)

Box.test(res, lag=10, fitdf=0) #is white noise, p > 0.05
Box.test(res, lag=10, fitdf=0, type='Lj') #is white noise, p > 0.05

checkresiduals(naive(goog200))

ausbeer
subset(ausbeer, start=length(ausbeer)-4*5)
subset(ausbeer, quarter=1)

ausbeer
beer2 <- window(ausbeer, start=1992, end=c(2007,4))
beerfit1 <- meanf(beer2, h=10)
beerfit2 <- rwf(beer2, h=10)
beerfit3 <- snaive(beer2, h=10)
autoplot(window(ausbeer, start=1992)) + 
  autolayer(beerfit1, series='Mean', PI=F) +
  autolayer(beerfit2, series='Naïve', PI=F) + 
  autolayer(beerfit3, series='Seasonal naïve', PI=F) + 
  xlab("Year") + ylab("Megalitres") +
  ggtitle("Forecasts for quarterly beer production") +
  guides(colour=guide_legend(title="Forecast"))
beer3 <- window(ausbeer, start=2008)
accuracy(beerfit1, beer3)
accuracy(beerfit2, beer3)
accuracy(beerfit3, beer3)

googfc1 <- meanf(goog200, h=40)
googfc2 <- rwf(goog200, h=40)
googfc3 <- rwf(goog200, drift=TRUE, h=40)
autoplot(subset(goog, end = 240)) +
  autolayer(googfc1, PI=FALSE, series="Mean") +
  autolayer(googfc2, PI=FALSE, series="Naïve") +
  autolayer(googfc3, PI=FALSE, series="Drift") +
  xlab("Day") + ylab("Closing Price (US$)") +
  ggtitle("Google stock price (daily ending 6 Dec 13)") +
  guides(colour=guide_legend(title="Forecast"))

googtest <- window(goog, start=201, end=240)
accuracy(googfc1, googtest)
accuracy(googfc2, googtest)
accuracy(googfc3, googtest)

goog200 %>% tsCV(rwf, drift=T, h=1) %>% .^2 %>% mean(na.rm=T) %>% sqrt()
goog200 %>% rwf(drift=T) %>% residuals() %>% .^2 %>% mean(na.rm=T) %>% sqrt()

goog200 %>% tsCV(naive, h=8) %>% .^2 %>% colMeans(na.rm=T) %>% 
  data.frame(x=1:8, y=.) %>% 
  ggplot(aes(x=x, y=y)) + geom_point()

goog200 %>% naive() %>% autoplot()
goog200 %>% naive(bootstrap=T) %>% autoplot()

window(ausbeer, start=2000) %>% forecast(h=4) %>% autoplot()

uschange[,c('Consumption', 'Income')] %>% autoplot() + ylab("% change") + xlab("Year")

uschange %>% as_tibble() %>% 
  ggplot(aes(x=Income, y=Consumption)) + 
  ylab("Consumption (quarterly % change)") +
  xlab("Income (quarterly % change)") +
  geom_point() +
  #geom_smooth(method="lm", se=T, level=0.4)
  geom_smooth(method="loess", se=T, level=0.9)

tslm(Consumption ~ Income, data=uschange)
uschange %>% autoplot(facet=T)
uschange %>% as.data.frame() %>% GGally::ggpairs()

fit.consMR <- tslm(Consumption ~ Income + Production + Unemployment + Savings, data=uschange)
summary(fit.consMR)

fit.consMR2 <- tslm(Consumption ~ Income, data=uschange)
summary(fit.consMR2)

autoplot(uschange[,'Consumption'], series='Data') + 
  autolayer(fitted(fit.consMR), series='Fitted') +
  xlab("Year") + ylab("") +
  ggtitle("Percent change in US consumption expenditure") +
  guides(colour=guide_legend(title=" "))

cbind(Data=uschange[,'Consumption'],
      Fitted=fitted(fit.consMR)) %>% 
  as.data.frame() %>% 
  ggplot(aes(x=Data, y=Fitted)) + 
  geom_point() + 
  ylab("Fitted (predicted values)") +
  xlab("Data (actual values)") +
  ggtitle("Percent change in US consumption expenditure") +
  geom_abline(intercept=0, slope=1)

fit.consMR %>% checkresiduals()

install.packages('gridExtra')
library(gridExtra)
df <- as.data.frame(uschange)
df[,"Residuals"] <- as.numeric(residuals(fit.consMR))
p1 <- ggplot(df, aes(x=Income, y=Residuals)) + geom_point()
p2 <- ggplot(df, aes(x=Production, y=Residuals)) + geom_point()
p3 <- ggplot(df, aes(x=Savings, y=Residuals)) + geom_point()
p4 <- ggplot(df, aes(x=Unemployment, y=Residuals)) + geom_point()
gridExtra::grid.arrange(p1, p2, p3, p4, nrow=2)

cbind(Fitted=fitted(fit.consMR), Residuals=residuals(fit.consMR)) %>% 
  as.data.frame() %>% 
  ggplot(aes(x=Fitted, y=Residuals)) + geom_point()

aussies <- window(ausair, end=2011)
fit <- tslm(aussies ~ guinearice)
summary(fit)
checkresiduals(fit)

beer2 <- window(ausbeer, start=1992)
autoplot(beer2) + xlab("Year") + ylab("Megalitres")
fit.beer <- tslm(beer2 ~ trend + season)
summary(fit.beer)

autoplot(beer2, series="Data") + 
  autolayer(fitted(fit.beer), series="Fitted") + 
  xlab("Year") + ylab("Megalitres") +
  ggtitle("Quarterly Beer Production")

cbind(Data=beer2, Fitted=fitted(fit.beer)) %>% 
  as.data.frame() %>% 
  ggplot(aes(x=Data, y=Fitted, colour=as.factor(cycle(beer2)))) + 
  geom_point() +  ylab("Fitted (predicted values)") + xlab("Data (actual values)") +
  ggtitle("Quarterly beer production") +
  scale_color_brewer(palette="Dark2", name="Quarter") + 
  geom_abline(intercept=0, slope=1)

fourier.beer <- tslm(beer2 ~ trend + fourier(beer2, K=2))
summary(fourier.beer)

autoplot(beer2, series="Data") + 
  autolayer(fitted(fourier.beer), series="Fitted") + 
  xlab("Year") + ylab("Megalitres") +
  ggtitle("Quarterly Beer Production")

CV(fit.consMR)

beer2 <- window(ausbeer, start=1992)
fit.beer <- tslm(beer2 ~ trend + season)
fcast <- forecast(fit.beer)
autoplot(fcast, series="forecast") + 
    autolayer(ausbeer, series="all") + 
  ggtitle("Forecasts of beer production using regression") +
  xlab("Year") + ylab("megalitres")

fit.consBest <- tslm(Consumption ~ Income + Savings + Unemployment, data = uschange)
h <- 4
newdata <- data.frame(
  Income = c(1, 1, 1, 1),
  Savings = c(0.5, 0.5, 0.5, 0.5),
  Unemployment = c(0, 0, 0, 0))
fcast.up <- forecast(fit.consBest, newdata = newdata)
newdata <- data.frame(
  Income = rep(-1, h),
  Savings = rep(-0.5, h),
  Unemployment = rep(0, h))
fcast.down <- forecast(fit.consBest, newdata = newdata)
  
autoplot(uschange[, 1]) +
  ylab("% change in US consumption") +
  autolayer(fcast.up, PI = TRUE, series = "increase") +
  autolayer(fcast.down, PI = TRUE, series = "decrease") +
  guides(colour = guide_legend(title = "Scenario"))

fit.cons <- tslm(Consumption ~ Income, data = uschange)
h <- 4
fcast.ave <- forecast(fit.cons, 
                      newdata = data.frame(Income = rep(mean(uschange[,"Income"]), h)))
fcast.up <- forecast(fit.cons,
                     newdata = data.frame(Income = rep(5, h)))

autoplot(uschange[, "Consumption"]) +
  ylab("% change in US consumption") +
  autolayer(fcast.ave, series = "Average increase", PI = TRUE) +
  autolayer(fcast.up, series = "Extreme increase", PI = TRUE) +
  guides(colour = guide_legend(title = "Scenario"))

h <- 10
fit.lin <- tslm(marathon ~ trend)
fcasts.lin <- forecast(fit.lin, h = h)
fit.exp <- tslm(marathon ~ trend, lambda = 0)
fcasts.exp <- forecast(fit.exp, h = h)

t <- time(marathon)
t.break1 <- 1940
t.break2 <- 1980
tb1 <- ts(pmax(0, t - t.break1), start = 1897)
tb2 <- ts(pmax(0, t - t.break2), start = 1897)

fit.pw <- tslm(marathon ~ t + tb1 + tb2)
t.new <- t[length(t)] + seq(h)
tb1.new <- tb1[length(tb1)] + seq(h)
tb2.new <- tb2[length(tb2)] + seq(h)

newdata <- cbind(t=t.new, tb1=tb1.new, tb2=tb2.new) %>%
  as.data.frame()
fcasts.pw <- forecast(fit.pw, newdata = newdata)

fit.spline <- tslm(marathon ~ t + I(t^2) + I(t^3) +
                     I(tb1^3) + I(tb2^3))
fcasts.spl <- forecast(fit.spline, newdata = newdata)

autoplot(marathon) +
  autolayer(fitted(fit.lin), series = "Linear") +
  autolayer(fitted(fit.exp), series = "Exponential") +
  autolayer(fitted(fit.pw), series = "Piecewise") +
  autolayer(fitted(fit.spline), series = "Cubic Spline") +
  autolayer(fcasts.pw, series="Piecewise") +
  autolayer(fcasts.lin, series="Linear", PI=FALSE) +
  autolayer(fcasts.exp, series="Exponential", PI=FALSE) +
  autolayer(fcasts.spl, series="Cubic Spline", PI=FALSE) +
  xlab("Year") + ylab("Winning times in minutes") +
  ggtitle("Boston Marathon") +
  guides(colour = guide_legend(title = " "))

marathon %>% splinef(lambda=0) %>% autoplot()
marathon %>% splinef(lambda=0) %>% checkresiduals()

plots <- list()
j <- 0
for (i in 3:10){
  if ((i %% 2) != 0){
    b <- c("Data", paste(i, "-MA"))
    v <- setNames(c("grey50", "red"), b)
    plot <- autoplot(elecsales, series="Data") + 
      autolayer(ma(elecsales, i), series=paste(i, "-MA")) + 
      xlab("Year") + ylab("GWh") + 
      ggtitle("Annual electricity sales: South Australia") + 
      scale_colour_manual(values=v, breaks=b)
    j <- j+1
    plots[[j]] <- plot
  }
}

do.call(gridExtra::grid.arrange,as.list(c(plots, nrow=2)))

ma4 = ma(beer2, order=4, centre=F)
ma2x4 = ma(beer2, order=4, centre=T)

autoplot(elecequip, series="Data") + 
  autolayer(ma(elecequip, 12), series="12-MA") + 
  ggtitle("Electrical equipment manufacturing (Euro area)") +
  scale_colour_manual(values=c("Data"="grey","12-MA"="red"), breaks=c("Data","12-MA"))

t <- elecequip %>% decompose(type="multiplicative")
t$x <- t$trend*t$random
t %>% autoplot() + xlab("Year") + ggtitle("Classical multiplicative decomposition of electrical equipment index")

install.packages("seasonal")
library(seasonal)
elecequip %>% seas(x11="") %>% 
  autoplot() + ggtitle("X11 decomposition of electrical equipment index")

fit <- elecequip %>% seas(x11="")
trendcycle(fit) %>% autoplot()
seasonal(fit) %>% autoplot()
remainder(fit) %>% autoplot()
seasadj(fit) %>% autoplot()

autoplot(elecequip, series="Data") +
  autolayer(trendcycle(fit), series="Trend") + 
  autolayer(seasadj(fit), series="Seasonally adjusted") + 
  xlab("Year") + ylab("New orders index") +
  ggtitle("Electrical equipment manufacturing (Euro area)") +
  scale_colour_manual(values=c("gray","blue","red"), breaks=c("Data","Seasonally adjusted","Trend"))

fit %>% seasonal() %>% ggsubseriesplot() + ylab("Seasonal")

elecequip %>% seas() %>%
  autoplot() +
  ggtitle("SEATS decomposition of electrical equipment index")

elecequip %>% stl(t.window=13, s.window="periodic", robust=T) %>% autoplot()
elecequip %>% mstl() %>% autoplot()
fit <- stl(elecequip, t.window=13, s.window="periodic", robust=T)
fit %>% seasadj() %>% naive() %>% autoplot() + ylab("New orders index") +
  ggtitle("Naive forecasts of seasonally adjusted data")
fit %>% forecast(method="naive") %>% autoplot()+ylab("New orders index")
elecequip %>% window(end=2008) %>% stlf() %>% autoplot(series="Forecast") + 
  autolayer(elecequip, series="Actual")
elecequip
forecast(fit,method="naive")

oildata <- window(oil, start=1996)
autoplot(oildata) + ylab("Oil (millions of tonnes)") + xlab("Year")
fc <- ses(oildata, h=5)
round(accuracy(fc),2)

autoplot(fc, series="Data") + 
  autolayer(fitted(fc), series="Fitted") + 
  ylab("Oil (millions of tonnes)") + xlab("Year")

air <- window(ausair, start=1990)
fc <- holt(air, h=5)
autoplot(fc, series="Data") + 
  autolayer(fitted(fc), series="Fitted") + 
  ylab("Oil (millions of tonnes)") + xlab("Year")

fc <- holt(air, h=15)
fc2 <- holt(air, damped=T, phi=0.9, h=15)
autoplot(air) + 
  autolayer(fc, series="Holt's method", PI=F) +
  autolayer(fc2, series="Damped Holt's method", PI=F) + 
  ggtitle("Forecasts from Holt's method") + 
  xlab("Year") + ylab("Air passengers in Australia (millions)") +
  guides(colour=guide_legend(title="Forecast"))

autoplot(livestock) + 
  xlab("Year") + ylab("Livestock, sheep in Asia (millions)")

e1 <- tsCV(livestock, ses, h=1)
e2 <- tsCV(livestock, holt, h=1)
e3 <- tsCV(livestock, holt, damped=T, h=1)

mean(e1^2, na.rm=T)
mean(e2^2, na.rm=T)
mean(e3^2, na.rm=T)

mean(abs(e1), na.rm=T)
mean(abs(e2), na.rm=T)
mean(abs(e3), na.rm=T)

fc <- holt(window(livestock, end=2000), damped=T)
fc[["model"]]

autoplot(fc, series="Fitted") +
  autolayer(livestock, series="Data", PI=F) +
  xlab("Year") + ylab("Livestock, sheep in Asia (millions)")

austourists

aust <- window(austourists, end=2008)
fit1 <- hw(aust, seasonal="additive", h=32, damped=F)
fit2 <- hw(aust, seasonal="multiplicative", h=32, damped=F)
autoplot(austourists) + 
  autolayer(fit1, series="HW additive forecasts", PI=F) +
  autolayer(fit2, series="HW multiplicative forecasts", PI=F) + 
  ylab("Visitor nights (millions)") +
  ggtitle("International visitors nights in Australia") +
  guides(colour=guide_legend(title="Forecast"))

autoplot(fit1[["Model"]])

l <- 120
fc <- hw(subset(hyndsight, end=length(hyndsight)-l), damped=F, seasonal="multiplicative", h=l)
autoplot(hyndsight) + 
  autolayer(fc, series="HW multi damped", PI=F) + 
  guides(colour=guide_legend(title="Daily forecasts"))

aust <- window(austourists, start=2005)
fit <- ets(aust)
summary(fit)
autoplot(fit)

cbind('Residuals' = residuals(fit), 'Forecast errors'= residuals(fit, type='response')) %>% 
  autoplot(facet=T) + xlab('Year') + ylab('')
austourists
aust <- window(austourists, end=2008)
fit <- ets(aust)
fit %>% forecast(h=35) %>% autoplot(series='Fitted') + 
  autolayer(austourists, series="Data") + 
  ylab("International visitor night in Australia (millions)")

Box.test(diff(goog200), lag=10, type='Ljung-Box')

gridExtra::grid.arrange(ggAcf(goog200), ggAcf(diff(goog200)))
a10
cbind("Sales ($million)" = a10, 
      "Monthly log sales" = log(a10),
      "Annual change in log sales" = diff(log(a10), 12)) %>% 
  autoplot(facet=T) + xlab("Year") + ylab("") + 
  ggtitle("Antidiabetic drug sales")

gridExtra::grid.arrange(
  ggAcf(a10), 
  ggAcf(log(a10)), 
  ggAcf(diff(log(a10), 12)),nrow=3)

lag = 1                 
Box.test(a10, lag=lag, type='Ljung-Box')
Box.test(log(a10), lag=lag, type='Ljung-Box')
Box.test(diff(log(a10), 12), lag=lag, type='Ljung-Box')

cbind("Billion Kwh" = usmelec,
      "Logs" = log(usmelec),
      "Seasonally\n differenced logs" = diff(log(usmelec), 12),
      "Doubly\n differenced logs" = diff(diff(log(usmelec), 12))) %>% 
  autoplot(facets=T) +
  xlab("Year") + ylab("") +
  ggtitle("Monthly US net electricity generation")

library(urca)
goog %>% ur.kpss() %>% summary()
goog %>% diff() %>% ur.kpss() %>% summary()
ndiffs(goog)
nsdiffs(elecsales)
nsdiffs(elecequip)
usmelec %>% nsdiffs()
usmelec %>% diff(lag=12) %>% nsdiffs()
usmelec %>% log() %>% nsdiffs()
usmelec %>% log() %>% diff(lag=12) %>% nsdiffs()

autoplot(uschange[,"Consumption"]) + 
  xlab("Year") + ylab("Quarterly percentage change")

fit <- auto.arima(uschange[,"Consumption"], seasonal = F)
fit %>% forecast(h=10) %>% autoplot(include=80)
length(uschange[,"Consumption"])
p1 <- ggAcf(uschange[,"Consumption"])
p2 <- ggPacf(uschange[,"Consumption"])
gridExtra::grid.arrange(p1, p2)
fit2 <- Arima(uschange[,"Consumption"], order=c(3,0,0))
fit3 <- auto.arima(uschange[,"Consumption"], seasonal = F, stepwise=F, approximation=F)
fit3
ggtsdisplay(uschange[,"Consumption"])

elecequip %>% stl(s.window='periodic') %>% seasadj() -> eeadj
eeadj %>% autoplot()
eeadj %>% diff() %>% ggtsdisplay()
fit <- Arima(eeadj, order=c(3,1,1))
checkresiduals()
residuals(fit) %>% ggtsdisplay()
Box.test(fit, lag=10, type='Ljung-Box')
autoplot(forecast(fit))
auto.arima(eeadj, approximation = F)
autoplot(fit)
#SEASONAL
autoplot(euretail) + ylab("Retail index") + xlab("Year")
euretail %>% diff(lag=4) %>% ggtsdisplay()
euretail %>% diff(lag=4) %>% diff() %>% ggtsdisplay()
euretail %>% Arima(order=c(0,1,1), seasonal=c(0,1,1)) %>% 
  residuals() %>% ggtsdisplay()
euretail %>% Arima(order=c(0,1,3), seasonal=c(0,1,1)) -> fit3
fit3 %>% residuals() %>% ggtsdisplay()
checkresiduals(fit3)
fit3 %>% forecast(h=12) %>% autoplot()
auto.arima(euretail)

lh02 <- log(h02)
cbind("H02 sales (million scripts)" = h02, "Log H02 sales"=lh02) %>% 
  autoplot(facets=T) + xlab("Year") + ylab("")
lh02 %>% diff(lag=12) %>% ggtsdisplay(xlab="Year", main="Seasonally differenced H02 scripts")
fit <- Arima(h02, order=c(3,0,1), seasonal=c(0,1,2), lambda=0)
checkresiduals(fit, lag=36)

h02 %>% 
  Arima(order=c(3,0,1), seasonal=c(0,1,2), lambda=0) %>% 
  forecast() %>% autoplot() +
  ylab("H02 sales (million scripts)") + xlab("Year") + 
  guides(colour=guide_legend(title="Data series"), 
         fill=guide_legend(title="Prediction interval"))

#update.packages()
install.packages('forecast')







ggplot.data <- c(data, forecast.mean)
type <- c(rep('measured', length(data)), rep('forecast', length(forecast.mean)))
time <- c(1:length(data), (length(data)-length(forecast.mean)+1):length(data))
nans <- rep(NA, length(data))
df <- tibble(time=time, speed=ggplot.data, type=type,
             lower80=c(nans, forecast.lower80), upper80=c(nans, forecast.upper80), 
             lower95=c(nans, forecast.lower95), upper95=c(nans, forecast.upper95))

df %>% ggplot(aes(x=time)) + 
  #geom_ribbon(aes(ymin=lower95, ymax=upper95), fill = '#C3C3F6') + 
  #geom_ribbon(aes(ymin=lower80, ymax=upper80), fill = '#7D7DEF') +
  geom_ribbon(aes(ymin=lower95, ymax=upper95, fill='95% level')) + 
  geom_ribbon(aes(ymin=lower80, ymax=upper80, fill='80% level')) +
  geom_line(aes(y=speed, colour=type), size=0.7)
#geom_line(aes(y=forecast.speed), color='white')
#green #00BA38
#red #F8766D
#95: #C3C3F6
#80: #7D7DEF

nans <- rep(NA, window.size-1)
df <- tibble(time=1:length(data), speed=data, forecast.speed=c(nans, forecast.mean), 
             lower80=c(nans, forecast.lower80), upper80=c(nans, forecast.upper80), 
             lower95=c(nans, forecast.lower95), upper95=c(nans, forecast.upper95))

df %>% ggplot(aes(x=time)) + 
  geom_ribbon(aes(ymin=lower95, ymax=upper95, colour = "bands"), fill = '#C3C3F6') + 
  geom_ribbon(aes(ymin=lower80, ymax=upper80, colour = "bands2"), fill = '#7D7DEF') +
  geom_line(aes(y=speed)) +
  geom_line(aes(y=forecast.speed), color='red')
#80: 7D7DEF
#95: C3C3F6




??urca

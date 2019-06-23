library(tidyverse)
library(forecast)
library(FinTS)
library(tseries)
library(rugarch)
#library(PerformanceAnalytics)
library(chron)
#install.packages('PerformanceAnalytics')
set.seed(321)
rnorm(500) %>% tsdisplay()
rnorm(500)^2 %>% tsdisplay()
rnorm(500) %>% ts() %>% autoplot()
rnorm(500) %>% checkresiduals()
rnorm(500) %>% ggAcf()
rnorm(500) %>% ggPacf()
#install.packages('FinTS')
data(m.intc7303)
str(m.intc7303)
head(m.intc7303)
plot(m.intc7303)
m.intc7303 %>% ts() %>% autoplot()
m.intc7303 %>% as.ts() %>% ggseasonplot()
m.intc7303 %>% as.ts() %>% ggtsdisplay()
ArchTest(log(1+as.numeric(m.intc7303)), lag=12)
m.intc7303^2 %>% as.ts() %>% ggtsdisplay()
ArchTest(log(1+as.numeric(m.intc7303)), lag=1, demean=T)
ArchTest(log(1+as.numeric(m.intc7303)), lag=2, demean=T)
ArchTest(log(1+as.numeric(m.intc7303)), lag=3, demean=T)
ArchTest(log(1+as.numeric(m.intc7303)), lag=4, demean=T)
m1 <- garch(m.intc7303, c(0,1))
summary(m1)
plot(m1)
m2 <- garch(m.intc7303, c(0,2))
summary(m2)
plot(m2)
usd<-read.csv("http://yunus.hacettepe.edu.tr/~iozkan/data/usd.csv", header=T, sep=";")
head(usd)
tail(usd)
usd.zoo=zoo(usd[,-1], order.by=as.Date(strptime(as.character(usd[,1]), "%d.%m.%Y")))
# usd contains weekends obs as NA. others are holidays.. Do not delete..
usd <- usd.zoo[!chron::is.weekend(time(usd.zoo))]
plot(usd, main="TL/USD Series", xlab="Date")
usd1 <- window(usd, start="2007-01-01")
# NA Treatment: Remove NA's from beginning and end points, approx in between
usd1 <- na.approx(na.trim(CalculateReturns(usd1), side="both"))

library(tidyverse)
library(lubridate)
library(rugarch)
library(forecast)
setwd('/home/diogo/Jupyter/tdef')
path <- '/home/diogo/Jupyter/tdef/Res025_ERA5.txt'
data <- read_table2(path, skip=9, comment="--") %>% 
  tail(-3) %>% 
  mutate(time=with_tz(ymd_hms(paste(Date, `Time(UTC)`)), tzone='Brazil/East')) %>% 
  mutate(hour=hour(time), day=date(time), month=month(time,label=TRUE, abbr=FALSE), year=year(time)) %>% 
  mutate(year_month = paste(year, month)) %>% 
  rename(speed=c_ws)
hourly.data <- data[(nrow(data)-365*24):nrow(data),]$speed
hourly.data %>% length()
ugarch_spec <- ugarchspec(variance.model = list(model='gjrGARCH', garchOrder = c(1,1)), 
                          mean.model = list(armaOrder = c(1,0), include.mean = T),
                          distribution.model =  "sstd")
ugarch_spec
#fit <- ugarchfit(spec = ugarch_spec, data = hourly.data)#, solver = 'hybrid')
#defaults: ugarchforecast(fitORspec, data = NULL, n.ahead = 10, n.roll = 0, out.sample = 0)
hourly.data %>% length() #169
#forecast <- ugarchforecast(fit, n.roll = 169, out.sample = 30000200)
#forecast %>% plot(which=1)

forecast.length <- 24*7

modelroll <- ugarchroll (
  spec=ugarch_spec, data=hourly.data, n.ahead = 1, forecast.length = forecast.length,
  n.start = NULL, refit.every = 50, refit.window = c("recursive"),
  window.size = NULL, solver = "hybrid", fit.control = list(),
  solver.control = list(), calculate.VaR = T, VaR.alpha = c(0.3, 0.7, 0.1, 0.90),
  cluster = NULL, keep.coef = F
)



#modelroll %>% plot(which=3)
#str(modelroll)

garch_plot <- function(hourly.data, modelroll){
  measured <- hourly.data[(length(hourly.data)-2*forecast.length):length(hourly.data)-1]
  
  forecast.lower80 <- modelroll@forecast$VaR[,'alpha(30%)']
  forecast.lower95 <- modelroll@forecast$VaR[,'alpha(10%)']
  forecast.mean <- modelroll@forecast$density[,'Mu']
  forecast.upper80 <- modelroll@forecast$VaR[,'alpha(70%)']
  forecast.upper95 <- modelroll@forecast$VaR[,'alpha(90%)']
  
  ggplot.data <- c(measured[1:length(measured)], forecast.mean)
  type <- c(rep('measured', length(measured)), rep('forecast', length(forecast.mean)))
  time <- c(1:length(measured), (length(measured)-length(forecast.mean)+1):length(measured))
  nans <- rep(NA, length(measured))
  
  df1 <- tibble(time=time, speed=ggplot.data, type=type,
                lower80=c(nans, forecast.lower80), upper80=c(nans, forecast.upper80), 
                lower95=c(nans, forecast.lower95), upper95=c(nans, forecast.upper95))
  df2 <- df1[(length(measured)+1):length(ggplot.data),]
  
  ggplot(data=df2, aes(x=time)) + 
    geom_ribbon(aes(ymin=lower95, ymax=upper95, fill='95% level'), alpha=1) + 
    geom_ribbon(aes(ymin=lower80, ymax=upper80, fill='80% level'), alpha=1) +
    geom_line(data=df1, aes(y=speed, colour=type), size=0.9) +
    scale_fill_manual(values=c('#7D7DEF', '#C3C3F6'), name="fill") +
    scale_color_manual(values = c('gold','black'))
  
  #ggsave('thesis/images/garch_first.png')
  #print(my_accuracy(myforecast, measured))
}

str(modelroll@forecast$VaR)

garch_plot(hourly.data, modelroll)

#x %>% ts() %>% autoplot()

#str(modelroll@forecast)

#TODO: make monthly garch 
#TODO: put garch plot into standard form
#TODO: make plot of financial data side-by-side with wind data
#TODO: make function to calculate forecast accuracy
#TODO: make ar, ma, arima prior to var arima and garch
#TODO: characterization: distribution, skewness, weibull test
#TODO: characterization: fourier transform
#TODO: 




era5 <- read_table2(path, skip=9, comment="--")
era5 %>% 
  tail(-3) %>% 
  mutate(stamp=with_tz(ymd_hms(paste(Date, `Time(UTC)`)), tzone='Brazil/East')) %>% 
  mutate(year=year(stamp), month=month(stamp,label=TRUE, abbr=FALSE)) %>% 
  group_by(year, month) %>% 
  summarize(speed = mean(c_ws, na.rm = T)) -> monthly.data
monthly.data <- ts(monthly.data$speed, start=c(2000,1), frequency=12)
monthly.data %>% autoplot()

ugarch_spec <- ugarchspec(variance.model = list(model='gjrGARCH', garchOrder = c(1,1)), 
                          mean.model = list(armaOrder = c(1,0), include.mean = T),
                          distribution.model =  "sstd")
ugarch_spec
#fit <- ugarchfit(spec = ugarch_spec, data = hourly.data)#, solver = 'hybrid')
#defaults: ugarchforecast(fitORspec, data = NULL, n.ahead = 10, n.roll = 0, out.sample = 0)
monthly.data %>% length() #169
#forecast <- ugarchforecast(fit, n.roll = 169, out.sample = 30000200)
#forecast %>% plot(which=1)

forecast.length <- 36

modelroll <- ugarchroll (
  spec=ugarch_spec, data=monthly.data, n.ahead = 1, forecast.length = forecast.length,
  n.start = NULL, refit.every = 4, refit.window = c("recursive"),
  window.size = NULL, solver = "hybrid", fit.control = list(),
  solver.control = list(), calculate.VaR = F, VaR.alpha = c(0.05, 0.01),
  cluster = NULL, keep.coef = F
)

#modelroll %>% plot(which=3)

str(modelroll)

measured <- monthly.data[(length(monthly.data)-4*forecast.length):length(monthly.data)-1]
myforecast <- modelroll@forecast$density[,'Mu']
myforecast <- c(rep(NA, length(measured)-length(myforecast)), myforecast)
x <- c(1:length(measured), 1:length(myforecast))
#y <- c(measured, forecast)
#color <- c(rep('measured', length(measured)), rep('forecast', length(forecast)))
df <- tibble(x1=1:length(measured),y1=measured,x2=1:length(myforecast),y2=myforecast)

ggplot(data=df) + 
  geom_line(aes(x=x1, y=y1, color='measured'), size=0.6) +
  geom_line(aes(x=x2, y=y2, color='forecast'), size=0.9)+
  #scale_color_manual(values = c('gold','black')) + 
  labs(x='Tempo', y='Velocidade (m/s)') + 
  ggtitle('title')

print(my_accuracy(myforecast, measured))

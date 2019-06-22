library(tidyverse)
library(lubridate)
library(forecast)
library(urca)
library(fpp)

setwd('/home/diogo/Jupyter/tdef')
path <- '/home/diogo/Jupyter/tdef/Res025_ERA5.txt'
era5 <- read_table2(path, skip=9, comment="--")
tail(era5,-3) %>% 
  mutate(stamp=with_tz(ymd_hms(paste(Date, `Time(UTC)`)), tzone='Brazil/East')) %>% 
  mutate(hour=hour(stamp), month=month(stamp,label=TRUE, abbr=FALSE)) %>% 
  group_by(month, hour) %>% 
  summarize(speed = mean(c_ws, na.rm = T)) %>% 
  rename(`Mês`=month, Hora=hour, Velocidade=speed) %>% 
  ggplot() + geom_smooth(aes(Hora, Velocidade, color=`Mês`), se = F) + scale_x_discrete(limits=0:23)
tail(era5)
nsdiffs(era5$c_ws)
nsdiffs(elecequip)
nsdiffs(usmelec)
era5$c_ws
era5['c_ws']
elecequip
usmelec
AirPassengers
euretail
h02
class(h02)
class(era5$c_ws)
?ts
?msts
#frequency is the number of observations per season
#if my season is a day then there are 24 observations per season
#if my season is a week then there are 24*7 observations per season
#if my season is a month then there are 24*30.44 observations per season
#if my season is a year then there are 24*365.25 observations per season
data <- tail(era5$c_ws, n=365*24*4)
data <- ts(data)
data %>% ggtsdisplay()
data %>% diff() %>% ggtsdisplay()
data %>%  diff() %>% ggtsdisplay(lag.max=100)
data %>%  diff(lag=365.25*24) %>% ggtsdisplay(lag.max=100)
#data <- msts(data, seasonal.periods=c(24, 168, 8766))
data <- diff(data, lag=365.25*24)
#BoxCox.lambda(data)
data <- BoxCox(data, lambda = 1.030897)
data
data %>% ggtsdisplay()
data %>% diff() %>% ggtsdisplay(lag.max=40)
data %>% diff(lag=365*24) %>% diff() %>% ggtsdisplay(lag.max=120)
data %>% diff(lag=365.25*24) %>% ggtsdisplay(lag.max=120)
nsdiffs(data)
#autoplot(taylor)
#taylor %>% tbats() %>% forecast() %>% autoplot()
c(24, 24*7, 365.25*24)

data  %>%
  Arima(order=c(5,1,2), seasonal=c(0,1,1)) %>%
  residuals() %>% ggtsdisplay()

data <- tail(era5$c_ws, n=365*24*3)
#data <- msts(data, seasonal.periods=c(24, 168, 8766))
data <- ts(data)
k = 10
train <- window(data, end=length(data)-k)
test <- window(data, start=length(data)-k)
other <- window(train, start=length(train)-100)
auto.arima(other)
other %>% Arima(order=c(1,0,2), seasonal=c(0,1,2)) %>% 
  forecast() %>% autoplot() + 
  autolayer(test, series='Observed') + 
  ggtitle("title") +
  xlab("Year") + ylab("Megalitres") +
  guides(colour=guide_legend(title="Forecast"))
  
attributes(result)
l <- length(result$fitted)
result$fitted <- result$fitted[l-20:l]
result$x <- result$x[l-20:l]
result %>% autoplot()
result$x



window.size <- 24*7
forecast.horizon <- 1
raw.data <- era5$c_ws
end <- length(raw.data)
start <- end-24*7*2
data <- raw.data[start:end]

forecast.lower80 <- c()
forecast.lower95 <- c()
forecast.mean <- c()
forecast.upper <- c()
forecast.upper80 <- c()
forecast.upper95 <- c()
p <- c()
d <- c()
q <- c()

for(k in 0:(length(data)-window.size)) {
  train <- data[(1+k):(window.size+k)]
  model <- auto.arima(train)
  print(c(k,length(data)-window.size,as.character(model)))
  result <- forecast(model, h=forecast.horizon)
  forecast.lower80 <- c(forecast.lower80, result$lower[1])
  forecast.lower95 <- c(forecast.lower95, result$lower[2])
  forecast.mean <- c(forecast.mean, result$mean[1])
  forecast.upper80 <- c(forecast.upper80, result$upper[1])
  forecast.upper95 <- c(forecast.upper95, result$upper[2])
  r <- str_match(as.character(model), "ARIMA\\((\\d),(\\d),(\\d)\\)")
  p <- c(p, r[2])
  d <- c(d, r[3])
  q <- c(q, r[4])
}

#measured.series <- ts(data)
#forecast.mean$lower <- forecast.lower
#forecast.mean$upper <- forecast.upper
result <- c()
result$x <-ts(data)
result$mean <- ts(forecast.mean, start=window.size)
result$lower <- ts.union('80%'=ts(forecast.lower80, start=window.size), '95%'=ts(forecast.lower95, start=window.size))
result$upper <- ts.union('80%'=ts(forecast.upper80, start=window.size), '95%'=ts(forecast.upper95, start=window.size))
result$level <- c(80, 95)
class(result) <- "forecast"
result %>%  autoplot(series='Forecast') + 
  autolayer(ts(data), series = 'Measured')


  
ggplot.data <- c(data, forecast.mean)
type <- c(rep('measured', length(data)), rep('forecast', length(forecast.mean)))
time <- c(1:length(data), (length(data)-length(forecast.mean)+1):length(data))
nans <- rep(NA, length(data))
df1 <- tibble(time=time, speed=ggplot.data, type=type,
             lower80=c(nans, forecast.lower80), upper80=c(nans, forecast.upper80), 
             lower95=c(nans, forecast.lower95), upper95=c(nans, forecast.upper95))
df2 <- df1[(length(data)+1):length(ggplot.data),]

ggplot(data=df2, aes(x=time)) + 
  #geom_ribbon(aes(ymin=lower95, ymax=upper95), fill = '#C3C3F6') + 
  #geom_ribbon(aes(ymin=lower80, ymax=upper80), fill = '#7D7DEF') +
  geom_ribbon(aes(ymin=lower95, ymax=upper95, fill='95% level'), alpha=1) + 
  geom_ribbon(aes(ymin=lower80, ymax=upper80, fill='80% level'), alpha=1) +
  geom_line(data=df1, aes(y=speed, colour=type), size=0.9) +
  scale_fill_manual(values=c('#7D7DEF', '#C3C3F6'), name="fill") +
  scale_color_manual(values = c('gold','black'))
  #scale_color_manual(values=c('#7D7DEF', '#C3C3F6'), name="other")
  #geom_line(aes(y=forecast.speed), color='white')

ggsave('thesis/images/var_result.png')

my_accuracy <- function(forecast, observed){
  accuracy(forecast, observed[(length(observed)-length(forecast)+1):length(observed)])
}

my_accuracy(forecast.mean, data)
#RMSE 	MAE 	MAPE 	MASE

data
forecast.mean

#green #00BA38
#red #F8766D
#95: #C3C3F6
#80: #7D7DEF
#dark green #02401B
library(RColorBrewer)
display.brewer.all(colorblindFriendly = TRUE)

#install.packages('wesanderson')
library(wesanderson)
names(wes_palettes)

df1 %>% ggplot(aes(x=time)) + 
  geom_line(aes(y=speed, colour=type), size=0.9)

autoplot(model)

#library(stringr)
str_locate("ARIMA(2,1,3)", "ARIMA(\d,\d,\d)")
str_extract("ARIMA(2,1,3)", "ARIMA(\d,\d,\d)")
regmatches("ARIMA(2,1,3)",regexpr("ARIMA(\\d,\\d,\\d)",txt))

tibble(time=1:length(p), p=p, d=d, q=q) %>% 
  ggplot(aes(as.integer(p), as.integer(q))) +
  geom_jitter(aes(colour=time), width=0.3, height=0.3, size=2) +
  #geom_jitter(aes(colour=time), width=0.3, height=0.3) + 
  #geom_path(aes(colour=time), position=position_jitter()) +
  #scale_color_gradientn(colours=c("navyblue", "blue", "darkmagenta", "darkorange1"))
  #scale_color_continuous(limits=c(1, 170), breaks=seq(1, 200, by=50)) +
  #scale_size_continuous(limits=c(170, 1), breaks=seq(200, 1, by=-50))
  #scale_color_gradientn(limits = c(1,170), colours=colours, breaks=b, labels=format(b))
  #scale_size_continuous(guide = FALSE) +
  scale_size(range = c(1, 5), guide=F) +
  scale_color_gradientn(colours = rainbow(5))+
  labs(x = 'ARIMA p', y='ARIMA q', color='Tempo (h)')

ggsave('thesis/images/var_arima.png')

test <- c()
for(k in 1:length(p)){
  test <- c(test, paste(p[k],d[k],q[k]))
}
unique(test)
autoplot(model)

subset <- window(data, start=length(data)-365*24)
subset %>% autoplot()
subset %>% ur.kpss() %>% summary()
subset %>% diff() %>% autoplot()
subset  %>% diff() %>% ur.kpss() %>% summary()
#null hypothesis: the data is stationary
#large p values suggest that data is stationary
#small p values suggest that data is not stationary
#since I found a value of 2.3 and my 1pct p-value is 0.739. I found a value much bigger than the 1pct value

era5 <- read_table2(path, skip=9, comment="--")
era5 %>% 
  tail(-3) %>% 
  mutate(stamp=with_tz(ymd_hms(paste(Date, `Time(UTC)`)), tzone='Brazil/East')) %>% 
  mutate(year=year(stamp), month=month(stamp,label=TRUE, abbr=FALSE)) %>% 
  group_by(year, month) %>% 
  summarize(speed = mean(c_ws, na.rm = T)) -> monthly.data
monthly.data <- ts(monthly.data$speed, start=c(2000,1), frequency=12)
monthly.data %>% autoplot()
monthly.data %>% nsdiffs()
monthly.data %>% diff(lag=12) %>% autoplot()
monthly.data %>% diff(lag=12) %>% nsdiffs()


#/home/diogo/Jupyter/tdef/thesis/images/diurnal.png











window.size <- 12
forecast.horizon <- 1
#raw.data <- era5$c_ws
#end <- length(raw.data)
#start <- end-24*7*2
#data <- raw.data[start:end]
data <- monthly.data

forecast.lower80 <- c()
forecast.lower95 <- c()
forecast.mean <- c()
forecast.upper <- c()
forecast.upper80 <- c()
forecast.upper95 <- c()
p <- c()
d <- c()
q <- c()

for(k in 0:(length(data)-window.size)) {
  train <- subset(data, start=(1+k), end=window.size+k)
  model <- auto.arima(train)
  print(c(k,length(data)-window.size,as.character(model)))
  result <- forecast(model, h=forecast.horizon)
  forecast.lower80 <- c(forecast.lower80, result$lower[1])
  forecast.lower95 <- c(forecast.lower95, result$lower[2])
  forecast.mean <- c(forecast.mean, result$mean[1])
  forecast.upper80 <- c(forecast.upper80, result$upper[1])
  forecast.upper95 <- c(forecast.upper95, result$upper[2])
  r <- str_match(as.character(model), "ARIMA\\((\\d),(\\d),(\\d)\\)")
  p <- c(p, r[2])
  d <- c(d, r[3])
  q <- c(q, r[4])
}

window(data, end=2004) %>% autoplot()
window(data, end=2004) %>% nsdiffs()
window(data, end=2004) %>% auto.arima()
subset(data, start=1, end=window.size)
data %>% ggseasonplot()
data %>% ggseasonplot(polar=T)











result <- c()
result$x <-ts(data)
result$mean <- ts(forecast.mean, start=window.size)
result$lower <- ts.union('80%'=ts(forecast.lower80, start=window.size), '95%'=ts(forecast.lower95, start=window.size))
result$upper <- ts.union('80%'=ts(forecast.upper80, start=window.size), '95%'=ts(forecast.upper95, start=window.size))
result$level <- c(80, 95)
class(result) <- "forecast"
result %>%  autoplot(series='Forecast') + 
  autolayer(ts(data), series = 'Measured')



ggplot.data <- c(data[1:length(data)], forecast.mean)
type <- c(rep('measured', length(data)), rep('forecast', length(forecast.mean)))
time <- c(1:length(data), (length(data)-length(forecast.mean)+1):length(data))
nans <- rep(NA, length(data))
df1 <- tibble(time=time, speed=ggplot.data, type=type,
              lower80=c(nans, forecast.lower80), upper80=c(nans, forecast.upper80), 
              lower95=c(nans, forecast.lower95), upper95=c(nans, forecast.upper95))
df2 <- df1[(length(data)+1):length(ggplot.data),]

ggplot(data=df2, aes(x=time)) + 
  #geom_ribbon(aes(ymin=lower95, ymax=upper95), fill = '#C3C3F6') + 
  #geom_ribbon(aes(ymin=lower80, ymax=upper80), fill = '#7D7DEF') +
  geom_ribbon(aes(ymin=lower95, ymax=upper95, fill='95% level'), alpha=1) + 
  geom_ribbon(aes(ymin=lower80, ymax=upper80, fill='80% level'), alpha=1) +
  geom_line(data=df1, aes(y=speed, colour=type), size=0.9) +
  scale_fill_manual(values=c('#7D7DEF', '#C3C3F6'), name="fill") +
  scale_color_manual(values = c('gold','black'))
#scale_color_manual(values=c('#7D7DEF', '#C3C3F6'), name="other")
#geom_line(aes(y=forecast.speed), color='white')
#green #00BA38
#red #F8766D
#95: #C3C3F6
#80: #7D7DEF
#dark green #02401B
library(RColorBrewer)
display.brewer.all(colorblindFriendly = TRUE)

#install.packages('wesanderson')
library(wesanderson)
names(wes_palettes)

df1 %>% ggplot(aes(x=time)) + 
  geom_line(aes(y=speed, colour=type), size=0.9)

auto.arima(data)

#ARIMA(2,0,1)(1,1,0)[12] 

train <- window(data, end=2014)
fit <- Arima(train, order=c(2,0,1), seasonal=c(1,1,0)) %>% forecast(h=96)
fit %>% autoplot()
data %>% autoplot() + 
  autolayer(fit$mean)
#args(Arima)

tail(era5,30*24) %>% 
  mutate(stamp=with_tz(ymd_hms(paste(Date, `Time(UTC)`)), tzone='Brazil/East')) %>% 
  mutate(hour=hour(stamp), month=month(stamp,label=TRUE, abbr=FALSE)) %>% 
  #group_by(month, hour) %>% 
  #summarize(speed = mean(c_ws, na.rm = T)) %>% 
  rename(`Mês`=month, Hora=hour, Velocidade=c_ws) %>% 
  ggplot() + geom_line(aes(stamp, Velocidade))

tail(era5,-3) %>% 
  mutate(stamp=with_tz(ymd_hms(paste(Date, `Time(UTC)`)), tzone='Brazil/East')) %>% 
  mutate(hour=hour(stamp), month=month(stamp,label=TRUE, abbr=FALSE)) %>% 
  #group_by(month, hour) %>% 
  #summarize(speed = mean(c_ws, na.rm = T)) %>% 
  rename(`Mês`=month, Hora=hour, Velocidade=c_ws) %>% 
  ggplot() + geom_line(aes(stamp, Velocidade))

x <- era5$c_ws
x <- x[(length(x)-24*7):length(x)] %>% ts()
model <- Arima(x, order=c(1,1,0))
model %>% forecast(h=12) %>% autoplot()
model %>% accuracy()
fARIMA110 <- function(x, h){forecast(Arima(x, order=c(1,1,0)), h=h)}

x %>% tsCV(fARIMA110, h=1) %>% mean(na.rm=T) %>% .^2 %>% sqrt()
x %>% fARIMA110(h=1) %>% residuals() %>% mean(na.rm=T) %>% .^2 %>% sqrt()

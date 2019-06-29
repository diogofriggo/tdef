library(tidyverse)
library(lubridate)
library(forecast)
library(FinTS)
library(tseries)
library(rugarch)
library(chron)

setwd('/home/diogo/Jupyter/tdef')
path <- '/home/diogo/Jupyter/tdef/Res025_ERA5.txt'

my_accuracy <- function(forecast, observed){
  accuracy(forecast, observed[(length(observed)-length(forecast)+1):length(observed)])
}

garch_plot <- function(data, modelroll, title, filename, multiplier, forecast_length){
  measured <- data[(length(data)-multiplier*forecast_length):(length(data)-1)]
  print(measured %>% length())
  forecast.lower80 <- modelroll@forecast$VaR[,'alpha(30%)']
  forecast.lower95 <- modelroll@forecast$VaR[,'alpha(10%)']
  forecast.mean <- modelroll@forecast$density[,'Mu']
  forecast.upper80 <- modelroll@forecast$VaR[,'alpha(70%)']
  forecast.upper95 <- modelroll@forecast$VaR[,'alpha(90%)']
  
  print(forecast.mean %>% length())
  print(forecast_length)
  
  ggplot.data <- c(measured[1:length(measured)], forecast.mean)
  type <- c(rep('measured', length(measured)), rep('forecast', length(forecast.mean)))
  time <- c(1:length(measured), (length(measured)-length(forecast.mean)+1):length(measured))
  nans <- rep(NA, length(measured))
  
  df1 <- tibble(time=time, speed=ggplot.data, type=type,
                lower80=c(nans, forecast.lower80), upper80=c(nans, forecast.upper80), 
                lower95=c(nans, forecast.lower95), upper95=c(nans, forecast.upper95))
  df2 <- df1[(length(measured)+1):length(ggplot.data),]
  
  # ggplot(data=df2, aes(x=time)) + 
  #   geom_ribbon(aes(ymin=lower95, ymax=upper95, fill='95% level'), alpha=1) + 
  #   geom_ribbon(aes(ymin=lower80, ymax=upper80, fill='80% level'), alpha=1) +
  #   geom_line(data=df1, aes(y=speed, colour=type), size=0.9) +
  #   scale_fill_manual(values=c('#7D7DEF', '#C3C3F6'), name="fill") +
  #   scale_color_manual(values = c('gold','black'))+
  #   labs(x='Tempo', y='Velocidade (m/s)') + 
  #   ggtitle(title)
  # 
  # ggsave(paste('thesis/images', filename, sep='/'))
  # print(my_accuracy(forecast.mean, measured))
  my_accuracy(forecast.mean, measured)
}

do_hourly_garch <- function(path, forecast_length){
  data <- read_table2(path, skip=9, comment="--") %>% 
    tail(-3) %>% 
    mutate(time=with_tz(ymd_hms(paste(Date, `Time(UTC)`)), tzone='Brazil/East')) %>% 
    mutate(hour=hour(time), day=date(time), month=month(time,label=TRUE, abbr=FALSE), year=year(time)) %>% 
    mutate(year_month = paste(year, month)) %>% 
    rename(speed=c_ws)
  hourly.data <- data[(nrow(data)-365*24):nrow(data),]$speed
  print(hourly.data %>% length())
  ugarch_spec <- ugarchspec(variance.model = list(model='gjrGARCH', garchOrder = c(1,1)), 
                            mean.model = list(armaOrder = c(1,0), include.mean = T),
                            distribution.model =  "sstd")
  modelroll <- ugarchroll (
    spec=ugarch_spec, data=hourly.data, n.ahead = 1, forecast_length = forecast_length,
    n.start = NULL, refit.every = 50, refit.window = c("recursive"),
    window.size = NULL, solver = "hybrid", fit.control = list(),
    solver.control = list(), calculate.VaR = T, VaR.alpha = c(0.3, 0.7, 0.1, 0.90),
    cluster = NULL, keep.coef = F
  )  
  list(hourly.data, modelroll)
}

#HOURLY GARCH

forecast_length <- 24*7
result <- do_hourly_garch(path, forecast_length)
hourly.data <- result[[1]]
modelroll <- result[[2]]
title <- paste('GARCH(1,1) com janela de 50 horas e passo de 1 hora')
garch_plot(hourly.data, modelroll, title, 'garch_first.png', 4, forecast_length)

root = '/home/diogo/Downloads/ERA5'
files <- list.files(path=root, pattern="*.txt", full.names=TRUE, recursive=FALSE)
lapply(files, function(x) {
  acc_list <- c()
  print(x)
  forecast_length <- 24*7
  result <- do_hourly_garch(path, forecast_length)
  hourly.data <- result[[1]]
  modelroll <- result[[2]]
  title <- paste('GARCH(1,1) com janela de 50 horas e passo de 1 hora')
  acc <- garch_plot(hourly.data, modelroll, title, 'garch_first.png', 4, forecast_length)
  acc_list <- c(acc_list, acc)
  acc_list
})

#MONTHLY GARCH

forecast_length <- 48
do_monthly_garch <- function(path, forecast_length){
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
  
  modelroll <- ugarchroll (
    spec=ugarch_spec, data=monthly.data, n.ahead = 1, forecast_length = forecast_length,
    n.start = NULL, refit.every = 4, refit.window = c("recursive"),
    window.size = NULL, solver = "hybrid", fit.control = list(),
    solver.control = list(), calculate.VaR = T, VaR.alpha = c(0.3, 0.7, 0.1, 0.90),
    cluster = NULL, keep.coef = F
  )
  c(monthly.data, modelroll)
}

result <- do_monthly_garch(path, forecast_length)
monthly.data <- result[0]
modelroll <- result[1]
title <- paste('GARCH(1,1) com janela de 4 meses e passo de 1 mês')
garch_plot(monthly.data, modelroll, title, 'garch_month.png', 4, forecast_length)
















measured <- monthly.data[(length(monthly.data)-4*forecast_length):length(monthly.data)-1]
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

ggplot(data=df2, aes(x=time)) + 
  geom_ribbon(aes(ymin=lower95, ymax=upper95, fill='95% level'), alpha=1) + 
  geom_ribbon(aes(ymin=lower80, ymax=upper80, fill='80% level'), alpha=1) +
  geom_line(data=df1, aes(y=speed, colour=type), size=0.9) +
  scale_fill_manual(values=c('#7D7DEF', '#C3C3F6'), name="fill") +
  scale_color_manual(values = c('gold','black'))+
  labs(x='Tempo', y='Velocidade (m/s)') + 
  ggtitle(title)

ggsave(paste('thesis/images', filename, sep='/'))
print(my_accuracy(myforecast, measured))


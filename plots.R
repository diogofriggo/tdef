library(forecast)
library(tidyverse)
library(lubridate)
library(tidyverse)
library(forecast)
library(fpp)
library(fpp2)
require(gridExtra)
library(fitdistrplus)
library(logspline)

setwd('/home/diogo/Jupyter/tdef')
path <- '/home/diogo/Jupyter/tdef/Res025_ERA5.txt'

data <- read_table2(path, skip=9, comment="--") %>% 
  tail(-3) %>% 
  mutate(time=with_tz(ymd_hms(paste(Date, `Time(UTC)`)), tzone='Brazil/East')) %>% 
  mutate(hour=hour(time), day=date(time), month=month(time,label=TRUE, abbr=FALSE), year=year(time)) %>% 
  mutate(year_month = paste(year, month)) %>% 
  rename(speed=c_ws)

data[(nrow(data)-365*24*2):nrow(data),] %>% 
  mutate(time=as_date(time)) %>% 
  ggplot() + geom_line(aes(time, speed)) + 
  scale_x_date(date_labels = "%b %y", date_breaks = "month") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(x = 'Tempo (hora)', y = 'Velocidade (m/s)') +
  ggtitle('Velocidade do vento em base horária')
ggsave('thesis/images/entire_series_hourly_basis.png')

data[(nrow(data)-365*24*2):nrow(data),] %>% 
  mutate(time=as_date(time), speed=c(0,diff(speed))) %>% 
  ggplot() + geom_line(aes(time, speed)) + 
  scale_x_date(date_labels = "%b %y", date_breaks = "month") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(x = 'Tempo (hora)', y = 'Velocidade (m/s)') +
  ggtitle('Velocidade do vento em base horária')
ggsave('thesis/images/entire_series_hourly_basis_seasonless.png')

data[(nrow(data)-365*24*2):nrow(data),] %>% 
  mutate(time=as_date(time), speed=c(rep(0,12),diff(speed, lag=12))) %>% 
  ggplot() + geom_line(aes(time, speed)) + 
  scale_x_date(date_labels = "%b %y", date_breaks = "month") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(x = 'Tempo (hora)', y = 'Velocidade (m/s)') +
  ggtitle('Velocidade do vento em base horária')
ggsave('thesis/images/entire_series_hourly_basis_seasonless_boxcox.png')

kpss.test(data[(nrow(data)-24*7*3):nrow(data),]$speed)
data[(nrow(data)-24*7*3):nrow(data),]$speed %>% ndiffs()

data[(nrow(data)-365*24*2):nrow(data),]$speed %>% diff(lag=12) %>% 
kpss.test(data[(nrow(data)-365*24*2):nrow(data),]$speed)
adf.test(data[(nrow(data)-365*24*2):nrow(data),]$speed, alternative = "stationary")

data[(nrow(data)-365*24*2):nrow(data),]$speed %>% ndiffs()
BoxCox.lambda(data[(nrow(data)-365*24*2):nrow(data),]$speed %>% ts())

data[(nrow(data)-365*24*2):nrow(data),]$speed %>% diff(order=2) %>% BoxCox(lambda = 1.18) %>% ts() %>% autoplot()
ggsave('thesis/images/boxcox.png')

png(file='thesis/images/long_memory.png')
data[(nrow(data)-365*24*2):nrow(data),]$speed %>% diff() %>% ts %>% ggtsdisplay()
dev.off()
data[(nrow(data)-365*24*2):nrow(data),]$speed %>% diff() %>% ts %>% Acf(lag.max=400) %>% autoplot()
ggsave('thesis/images/long_memory_lagmax.png')

week_data <- data[(nrow(data)-24*7*3):nrow(data),]$speed %>% diff() %>% ts()
plot1 <- data[(nrow(data)-24*7*3):nrow(data),]$speed %>% ts() %>% autoplot()
plot2 <- week_data %>% autoplot()
ggsave('thesis/images/last3weeks.png', arrangeGrob(plot1, plot2))

week_data %>% ggtsdisplay()
ggsave('thesis/images/last3weeks_acf.png', arrangeGrob(plot1, plot2))

#<WIND, MONEY PLOT>
data %>% 
  group_by(year, month, day) %>% 
  summarize(speed = mean(speed, na.rm = T)) -> daily.data

money <- goog[(length(goog)-120):length(goog)] %>% ts()
plot2 <- autoplot(money) +
  xlab("Dia") + ylab("Preço (US$)") +
  ggtitle("Ações da Google")

wind <- daily.data$speed
plot1 <- wind[(length(wind)-length(money)):length(wind)] %>% ts() %>% autoplot() +
  xlab("Dia") + ylab("Velocidade (m/s)") +
  ggtitle("Velocidade do Vento")

grid.arrange(plot1, plot2, ncol=2)
ggsave('thesis/images/wind_money.png', arrangeGrob(plot1, plot2, ncol=2))
#</WIND, MONEY PLOT>

#<WEIBULL>
wind <- data$speed
wind2 <- wind[(length(wind)-365*24):length(wind)]
data
data %>% 
  ggplot(aes(nw_ws)) + 
  geom_histogram(binwidth=0.3, fill="blue", alpha=.2, col='darkgrey') + 
  xlab('Velocidade (m/s)') + ylab('Frequência')
ggsave('weibull_histogram.png')

png('thesis/images/normal_overlay.png')
g = data$nw_ws
m<-mean(g)
std<-sqrt(var(g))
hist(g, density=20, breaks=20, prob=TRUE, 
     xlab="Velocidade (m/s)", ylab='Densidade', ylim=c(0, 0.2), 
     main='Curva normal superposta ao histograma de velocidades')
curve(dnorm(x, mean=m, sd=std), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")
dev.off() 

descdist(wind2, discrete = FALSE)
ggsave('thesis/images/cullen.png')

fit.weibull <- fitdist(wind2, "weibull")
fit.weibull
ks.test(wind2, "pweibull", scale=fit.weibull$estimate[2], shape=fit.weibull$estimate[1])
plot(fit.weibull)

fit.norm <- fitdist(wind2, "norm")
fit.norm
ks.test(wind2, "pnorm", mean=mean(wind2), sd=sd(wind2))
plot(fit.norm)

install.packages('OptInterim')
library(OptInterim)
weibull.plot(c(1,2,3,4))

#fit.weibull <- fitdistr(, "weibull")
#plot(fit.weibull)
#fit.norm <- fitdist(x, "norm")
#</WEIBULL>

data[(nrow(data)-365*24*2):nrow(data),] %>% 
  group_by(day) %>% 
  summarize(speed = mean(speed)) %>% 
  ggplot(aes(day, speed)) + geom_line() + 
  scale_x_date(date_labels = "%b %y", date_breaks = "month") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(x = 'Tempo (dia)', y = 'Velocidade (m/s)') +
  ggtitle('Velocidade do vento em base diária')
ggsave('thesis/images/entire_series_daily_basis.png')

data[(nrow(data)-365*24*4):nrow(data),] %>% 
  #mutate(day = as_date(day)) %>% 
  group_by(month = floor_date(day, unit = "month")) %>%
  summarize(speed = mean(speed, na.rm = T)) %>% 
  ggplot() + geom_line(aes(month, speed)) + 
  scale_x_date(date_labels = "%b %y", date_breaks = "2 months") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(x='Tempo (mês)', y='Velocidade (m/s)') + 
  ggtitle('Velocidade do vento em base mensal')
ggsave('thesis/images/entire_series_monthly_basis.png')

data %>% 
  filter(year < 2019) %>% 
  group_by(year) %>% 
  summarize(speed = mean(speed, na.rm = T)) %>% 
  ggplot() + geom_line(aes(year, speed)) + 
  scale_x_continuous(breaks = 2000:2018) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(x='Tempo (ano)', y='Velocidade (m/s)')
  ggtitle('Velocidade do vento em base anual')
ggsave('thesis/images/entire_series_yearly_basis.png')

#I want to evaluate point forecasts using any arima model
#data <- data$speed
#order <- c(1,0,0)
#test.size <- 24*7*2
#window.size <- 24*7

plot_model <- function(data, order, test.size, window.size){
  forecast.horizon <- 1
  data <- data[(length(data)-test.size):length(data)]
  
  forecast.lower80 <- c()
  forecast.lower95 <- c()
  forecast.mean <- c()
  forecast.upper80 <- c()
  forecast.upper95 <- c()
  
  for(k in 0:(length(data)-window.size)) {
    train <- data[(1+k):(window.size+k)]
    result <- forecast(Arima(ts(train), order=order), h=forecast.horizon)
    forecast.lower80 <- c(forecast.lower80, result$lower[1])
    forecast.lower95 <- c(forecast.lower95, result$lower[2])
    forecast.mean <- c(forecast.mean, result$mean[1])
    forecast.upper80 <- c(forecast.upper80, result$upper[1])
    forecast.upper95 <- c(forecast.upper95, result$upper[2])
  }
  ggplot.data <- c(data[1:length(data)], forecast.mean)
  type <- c(rep('measured', length(data)), rep('forecast', length(forecast.mean)))
  time <- c(1:length(data), (length(data)-length(forecast.mean)+1):length(data))
  nans <- rep(NA, length(data))
  df1 <- tibble(time=time, speed=ggplot.data, type=type,
                lower80=c(nans, forecast.lower80), upper80=c(nans, forecast.upper80), 
                lower95=c(nans, forecast.lower95), upper95=c(nans, forecast.upper95))
  df2 <- df1[(length(data)+1):length(ggplot.data),]
  
  ggplot(data=df2, aes(x=time)) + 
    geom_ribbon(aes(ymin=lower95, ymax=upper95, fill='95% level'), alpha=1) + 
    geom_ribbon(aes(ymin=lower80, ymax=upper80, fill='80% level'), alpha=1) +
    geom_line(data=df1, aes(y=speed, colour=type), size=0.9) +
    scale_fill_manual(values=c('#7D7DEF', '#C3C3F6'), name="fill") +
    scale_color_manual(values = c('gold','black'))
}

#auto.arima(ts(data$speed))
plot_model(data$speed, c(1,1,0), 24*7*2, 24*7)
ggsave('thesis/images/plot_model.png')

#seasonal plots

monthly.data <- data %>% 
  group_by(year, month) %>% 
  summarize(speed = mean(speed, na.rm = T))
  
monthly.data <- ts(monthly.data$speed, start=c(2000,1), frequency=12)
monthly.data %>% ggseasonplot()
ggsave('thesis/images/season_plot.png')
monthly.data %>% ggseasonplot(polar=T)
ggsave('thesis/images/season_plot_polar.png')

#12x24 plot
data %>% 
  group_by(month, hour) %>% 
  summarize(speed = mean(speed, na.rm = T)) %>% 
  ggplot() + 
  geom_smooth(aes(hour, speed, color=month), se = F) + 
  scale_x_discrete(limits=0:23) + 
  labs(x = 'Hora', y='Velocidade (m/s)', color='Mês') + 
  ggtitle('12x24')
ggsave('thesis/images/12x24plot.png')

#windroses

source('windrose.R')

data %>% 
  plot.windrose(spd = "speed", dir = "c_wd", spdmin=0) + 
  facet_wrap(~month) +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank(), axis.ticks.y = element_blank())
ggsave('thesis/images/windrose_monthly.png')

data %>% 
  plot.windrose(spd = "speed", dir = "c_wd", spdmin=0) + 
  theme(axis.text.y = element_blank(), axis.title.y = element_blank(), axis.ticks.y = element_blank())
ggsave('thesis/images/windrose.png')

#TODO: It'd be interesting to construct a table of error measures for various models
#models <- list()
#i <- 1
#for (p in 1:5){
#  for (d in c(0,1)){
    print(c('running arima', p, d, 0))
    models[[i]] <- Arima(ts.data, order=c(p,d,0))
    i <- i+1
  }
#}

#tsCV
h <- 1
x <- c()
y <- c()
color <- c()
max <- 15
for(k in 3:10){
  x <- c(x, 1:max)
  y <- c(y, rep(k-3,max))
  #color <- c(color, rep('janela', k), 'h=1', rep('outro', max-k-1))
  color <- c(color, rep('janela', k), rep('-', h-1), paste('h=',h), rep('-', max-k-h))
}
df <- tibble(x=x, y=y, color=color)
df %>% ggplot(aes(x,y,color=color)) + 
  geom_point(size=3) + 
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9")) + 
  labs(x = 'Tempo', y='Validação cruzada', color='Conjunto') + 
  ggtitle(paste('Validação cruzada com horizonte de previsão h=',3,sep=''))
ggsave(paste('thesis/images/crossh', h, '.png', sep=''))

#TODO TOMORROW: HOW DO NTH POINTS FORECAST PERFORM?
#TODO TOMORROW: RUN MANY ARIMA MODELS AND COMPARE RESULTS
#DO THE SAME PLOT_MODEL FOR GARCH
#plot_model monthly basis
#gráficos de tendencia + season + residuos
#TODO TOMORROW: various measures of how a model performs

#- - - - * * o o o o o o
#o o - - - - * * o o o o
#o o o o - - - - * * o o
#o o o o o o - - - - * *

my_accuracy <- function(forecast, observed){
  accuracy(forecast, observed[(length(observed)-length(forecast)+1):length(observed)])
}

plot_model <- function(data, order, window.size, test.size=NULL, forecast.horizon=1, singular='hora', plural='horas'){
  if(is.null(test.size)){
    test.size <- length(data)-window.size
  }
  data <- data[(length(data)-test.size):length(data)]
  #data[-(1:(length(data) %% forecast.horizon))] #this ensure data is a multiple of forecast.horizon
  
  forecast.lower80 <- c()
  forecast.lower95 <- c()
  forecast.mean <- c()
  forecast.upper80 <- c()
  forecast.upper95 <- c()
  
  for(k in seq(0, length(data)-window.size, forecast.horizon)) {
    #print(paste(length(data), 1+k, window.size+k, window.size+k+1, window.size+k+forecast.horizon))
    train <- data[(1+k):(window.size+k)]
    result <- forecast(Arima(ts(train), order=order), h=forecast.horizon)
    #print('everything is ok before this')
    forecast.lower80 <- c(forecast.lower80, result$lower[,1])
    forecast.lower95 <- c(forecast.lower95, result$lower[,2])
    forecast.mean <- c(forecast.mean, result$mean)
    forecast.upper80 <- c(forecast.upper80, result$upper[,1])
    forecast.upper95 <- c(forecast.upper95, result$upper[,2])
  }
  ggplot.data <- c(data[1:length(data)], forecast.mean)
  type <- c(rep('measured', length(data)), rep('forecast', length(forecast.mean)))
  time <- c(1:length(data), (length(data)-length(forecast.mean)+1):length(data))
  nans <- rep(NA, length(data))
  df1 <- tibble(time=time, speed=ggplot.data, type=type,
                lower80=c(nans, forecast.lower80), upper80=c(nans, forecast.upper80), 
                lower95=c(nans, forecast.lower95), upper95=c(nans, forecast.upper95))
  df2 <- df1[(length(data)+1):length(ggplot.data),]
  
  print(my_accuracy(forecast.mean, data))
  
  base <- if (forecast.horizon==1) singular else plural
  model.desc <- paste('ARIMA(', order[1], ',', order[2], ',', order[3], ')', sep='')
  ggplot(data=df2, aes(x=time)) + 
    geom_ribbon(aes(ymin=lower95, ymax=upper95, fill='95% level'), alpha=1) + 
    geom_ribbon(aes(ymin=lower80, ymax=upper80, fill='80% level'), alpha=1) +
    geom_line(data=df1, aes(y=speed, colour=type), size=0.9) +
    scale_fill_manual(values=c('#7D7DEF', '#C3C3F6'), name="fill") +
    scale_color_manual(values = c('gold','black')) + 
    labs(x='Tempo', y='Velocidade (m/s)') + 
    ggtitle(paste('Previsão de', test.size, plural, 'passo=', forecast.horizon, base, ',', model.desc, 'com janela', window.size, plural))
}

week_data <- data[(nrow(data)-24*7*3):nrow(data),]$speed %>% diff() %>% ts()
Arima(week_data, order=c(6,1,3)) %>% autoplot()
ggsave('thesis/images/conds.png')
Arima(week_data, order=c(1,1,1)) %>% autoplot()
Arima(week_data, order=c(1,1,1)) %>% autoplot()
Arima(week_data, order=c(2,1,2)) %>% autoplot()
Arima(week_data, order=c(2,1,3)) %>% autoplot()

p1 <- plot_model(data$speed, c(1,1,1), test.size=24*7*2, window.size=24*7, forecast.horizon=1)
p2 <- plot_model(data$speed, c(2,1,1), test.size=24*7*2, window.size=24*7, forecast.horizon=1)
ggsave('thesis/images/arima12.png', arrangeGrob(p1, p2))
p3 <- plot_model(data$speed, c(1,1,2), test.size=24*7*2, window.size=24*7, forecast.horizon=1)
p4 <- plot_model(data$speed, c(2,1,3), test.size=24*7*2, window.size=24*7, forecast.horizon=1)
ggsave('thesis/images/arima34.png', arrangeGrob(p3, p4))

#ME      RMSE      MAE     MPE     MAPE
#Test set 0.03306698 0.7544885 0.538832 3.06541 17.27658

monthly.data <- data %>% group_by(year, month) %>% 
  summarize(speed = mean(speed, na.rm = T))

plot_model(monthly.data$speed, c(1,1,0), window.size=12, forecast.horizon=1, singular='mês', plural='meses')

plot_model(monthly.data$speed, c(1,1,0), window.size=12, forecast.horizon=2, singular='mês', plural='meses')

#DON'T: do a function that uses the predicted value as if it was measured CHECK
#do a function like the above that work on a monthly basis CHECK
#do a function that uses garch DOING
#todo do cross validation manually (separate function) TODO


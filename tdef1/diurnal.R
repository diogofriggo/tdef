setwd('C:/Users/Diogo Friggo/Downloads')
library(tidyverse)
path <- 'C:/Users/Diogo Friggo/Downloads/nDani/250252 Cataventos Sento Se/04.WindAnalysis/02.WindApp 5.1.120.0/05.LongTerm/01.Inputs/SentoSe_ERA5.txt'
era5 <- read_table2(path, skip=9, comment="--")
library(lubridate)
tail(era5,-3) %>% 
  mutate(stamp=with_tz(ymd_hms(paste(Date, `Time(UTC)`)), tzone='Brazil/East')) %>% 
  mutate(hour=hour(stamp), month=month(stamp,label=TRUE, abbr=FALSE)) %>% 
  group_by(month, hour) %>% 
  summarize(speed = mean(c_ws, na.rm = T)) %>% 
  rename(`Mês`=month, Hora=hour, Velocidade=speed) %>% 
  ggplot() + geom_smooth(aes(Hora, Velocidade, color=`Mês`), se = F) + scale_x_discrete(limits=0:23)

ggsave('diurnal.png')
#grep("Brazil",OlsonNames(),value=TRUE)  

era5 %>%
  select(1:3) %>% 
  mutate(stamp=ymd_hms(paste(Date, `Time(UTC)`))) %>% 
  separate(Date, c('year', 'month', 'day')) %>% 
  filter(year==2017) %>%
  mutate(month=month(stamp,label=TRUE, abbr=FALSE)) %>% 
  group_by(month) %>%
  rename(`Mês`=month, Tempo=stamp, Velocidade=c_ws) %>%
  ggplot(aes(Tempo, Velocidade)) + 
    geom_line(size=0.2) + 
    geom_smooth() +
    theme(axis.text.x=element_blank()) + 
    facet_wrap(aes(`Mês`), scales = "free")

ggsave('stochastic_monthly.png')

#install.packages('Quandl')

# Quandl package must be installed
library(Quandl)

# Get your API key from quandl.com
quandl_api = "Dck1RApJVpV5jh2zX_T1"

# Add the key to the Quandl keychain
Quandl.api_key(quandl_api)

quandl_get <-
  function(sym, start_date = "2013-01-01") {
    require(devtools)
    require(Quandl)
    # create a vector with all lines
    tryCatch(Quandl(c(
      paste0("WIKI/", sym, ".8")),
      start_date = start_date,
      type = "zoo"
    ))
  }

era5 %>%
  select(1:3) %>% 
  #mutate(Tempo=ymd_hms(paste(Date, `Time(UTC)`))) %>% 
  #separate(Date, c('year', 'month', 'day')) %>% 
  filter(year(Date)==2017) %>%
  group_by(Date) %>% 
  summarise(Velocidade=mean(c_ws)) %>% 
  rename(Tempo=Date) %>%
  ggplot(aes(Tempo, Velocidade)) + 
    geom_line() + 
    geom_smooth()

ggsave('stochastic.png')

#install.packages('timetk')
library(timetk)

stock <- tk_tbl(tk_ts(quandl_get('GOOG'), start = c(2013,1), freq = 365), timetk_idx = TRUE)
stock %>% 
  rename(Valor=value) %>%
  mutate(Tempo=as_datetime(ymd(index))) %>% 
  ggplot(aes(Tempo, Valor)) + 
    geom_line() +
    geom_smooth()

ggsave('stock.png')
#> Warning: Non-numeric columns being dropped: date


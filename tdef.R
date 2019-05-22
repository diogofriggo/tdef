setwd('/home/diogo/Jupyter/tdef')
library(tidyverse)
path <- '/home/diogo/Jupyter/tdef/Res025_ERA5.txt'
era5 <- read_table2(path, skip=9, comment="--")
library(lubridate)
tail(era5,-3) %>% 
  mutate(stamp=with_tz(ymd_hms(paste(Date, `Time(UTC)`)), tzone='Brazil/East')) %>% 
  mutate(hour=hour(stamp), month=month(stamp,label=TRUE, abbr=FALSE)) %>% 
  group_by(month, hour) %>% 
  summarize(speed = mean(c_ws, na.rm = T)) %>% 
  rename(`Mês`=month, Hora=hour, Velocidade=speed) %>% 
  ggplot() + geom_smooth(aes(Hora, Velocidade, color=`Mês`), se = F) + scale_x_discrete(limits=0:23)
era5
library(forecast)
era5$c_ws %>% 
  tail(4*365) %>% 
  msts(seasonal.periods=4*365) %>%
  ggseasonplot(c_ws, polar=T)


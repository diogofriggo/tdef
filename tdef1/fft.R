setwd('C:/Users/Diogo Friggo/Downloads/TDEF/tex/images')
library(tidyverse)
library(lubridate)
path <- 'C:/Users/Diogo Friggo/Downloads/TDEF/code/SentoSe_ERA5.txt'
era5 <- read_table2(path, skip=9, comment="--")
tail(era5,-3) %>% 
  select(1:3) %>% 
  mutate(stamp=with_tz(ymd_hms(paste(Date, `Time(UTC)`)), tzone='Brazil/East')) %>% 
  mutate(x = 1:(nrow(era5)-3), fourier_transform = Re(fft(.$c_ws))) %>% 
  #do(tibble(x=1:(nrow(era5)-3), y = Re(fft(.$c_ws)))) %>% 
  filter(fourier_transform < max(fourier_transform)) %>% 
  ggplot() + geom_point(aes(x,fourier_transform/length(x)))


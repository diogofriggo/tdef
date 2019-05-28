setwd('C:/Users/Diogo Friggo/Google Drive/TDEF/tex/thesis/images')
library(tidyverse)
path <- 'C:/Users/Diogo Friggo/Google Drive/TDEF/code/SentoSe_ERA5.txt'
era5 <- read_table2(path, skip=9, comment="--")
library(lubridate)
era5 %>% 
  #select(4:ncol(.)) %>% 
  #select(-contains('color'))
  select(contains('_ws')) %>% 
  rename_(.dots=setNames(names(.), toupper(gsub("_ws", "", names(.))))) %>% 
  gather(1:ncol(.), key='Quadrante', value='Velocidade') %>% 
  ggplot() + 
    geom_freqpoly(aes(Velocidade, color=Quadrante), size=1, binwidth=0.2) + 
    ylab("Frequência")

ggsave('weibull_freqpoly.png')

era5 %>% 
  rename(Velocidade=c_ws) %>% 
  ggplot(aes(Velocidade)) + 
  #ggplot(aes(x=Velocidade, y=..count../sum(..count..))) + 
    #geom_histogram(binwidth=0.2, fill="blue", alpha=.2) + 
    geom_histogram(binwidth=0.3, fill="blue", alpha=.2, col='darkgrey') + 
    ylab("Frequência")

ggsave('weibull_histogram.png')



ggplot(diamonds, aes(carat)) +
  geom_density()

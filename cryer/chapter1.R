#install.packages('openssl')
library(tidyverse)

#RAINFALL

df <- read.csv('/home/diogo/Jupyter/tdef/cryer/datasets/larain.dat')
df$year <- (1877 + as.numeric(row.names(df)))
df$lagged_larain <- lag(df$larain)
df <- df[-1,]

ggplot(df, aes(x=year, y=larain)) + geom_line() + geom_point() +
  scale_x_continuous(breaks=df$year[c(TRUE, FALSE)]) +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  xlab('year') + ylab('rainfall (inches)')

ggplot(df, aes(x=lagged_larain, y=larain)) + geom_point() + geom_smooth(method='lm',formula=y~x)
  xlab('rainfall (inches) at t-1') + ylab('rainfall (inches) at t')

cor(df$larain, df$lagged_larain)

#COLOR

df <- read.csv('/home/diogo/Jupyter/tdef/cryer/datasets/color.dat')
df$time <- as.numeric(row.names(df))
df$lagged_color <- lag(df$color)
df <- df[-1,]
  
ggplot(df, aes(x=time, y=color)) + geom_line() + geom_point() +
  xlab('time') + ylab('color')

ggplot(df, aes(x=lagged_color, y=color)) + geom_point() + geom_smooth(method='lm',formula=y~x) +
  xlab('color_t-1') + ylab('color_t')

cor(df$color, df$lagged_color)

#HARE

df <- read.csv('/home/diogo/Jupyter/tdef/cryer/datasets/hare.dat')
df$y <- df$hare
df$x <- as.numeric(row.names(df))
df$lagged_y <- lag(df$y)
df <- df[-1,]

ggplot(df, aes(x=x, y=y)) + geom_line() + geom_point() +
  xlab('time') + ylab('abundance')

ggplot(df, aes(x=lagged_y, y=y)) + geom_point() + geom_smooth(method='lm',formula=y~x) +
  xlab('abundance at t-1') + ylab('abundance at t')

cor(df$y, df$lagged_y)

#TEMPDUB

df <- read.csv('/home/diogo/Jupyter/tdef/cryer/datasets/tempdub.dat')
df$y <- df$tempdub
df$x <- as.numeric(row.names(df))
df$lagged_y <- lag(df$y)
df <- df[-1,]

ggplot(df, aes(x=x, y=y)) + geom_line() + geom_point() +
  xlab('time') + ylab('temperature')

ggplot(df, aes(x=lagged_y, y=y)) + geom_point() + geom_smooth(method='lm',formula=y~x) +
  xlab('temperature at t-1') + ylab('temperature at t')

cor(df$y, df$lagged_y)

#OILFILTERS

require(lubridate)
start <- ymd('1984-7-1')
df <- read.csv('/home/diogo/Jupyter/tdef/cryer/datasets/oilfilters.dat')
df$y <- df$oilfilters
df$x <- seq(start, start %m+% months(nrow(df)-1), by="month")
df$lagged_y <- lag(df$y)
df <- df[-1,]

ggplot(df, aes(x,y)) + geom_line() + geom_point() +
  geom_text(aes(label=x %>% month(label=T) %>% toupper %>% substr(1, 1)), hjust=0, vjust=0) +
  scale_x_date(breaks=df$x[c(T,F)], labels=format(df$x[c(T,F)], f='%Y-%m')) +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  xlab('year-month') + ylab('oil sales')

ggplot(df, aes(x,y, color=month(x, label=T))) + geom_line() + geom_point() +
  geom_text(aes(label=x %>% month(label=T) %>% toupper %>% substr(1, 1)), hjust=0, vjust=0) +
  scale_x_date(breaks=df$x[c(T,F)], labels=format(df$x[c(T,F)], f='%Y-%m')) +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  xlab('year-month') + ylab('oil sales')

ggplot(df, aes(x=lagged_y, y=y)) + geom_point() + geom_smooth(method='lm',formula=y~x) +
  xlab('oil sales at t-1') + ylab('oil sales at t') +
  ggtitle(cor(df$y, df$lagged_y)) 

#EXERCISES

library(reshape2)
n<-12
l<-48
m <- matrix(runif(n*l), ncol=n)
colnames(m) <- paste('V', 1:n, sep='')
df <- m %>% as_tibble() %>% melt()
df$x <- rep(1:l,times=n)
ggplot(df, aes(x,value)) + geom_line() + geom_point() + facet_wrap(~variable)

m <- matrix(rchisq(n*l, 2, ncp = 0), ncol=n)
colnames(m) <- paste('V', 1:n, sep='')
df <- m %>% as_tibble() %>% melt()
df$x <- rep(1:l,times=n)
ggplot(df, aes(x,value)) + geom_line() + geom_point() + facet_wrap(~variable)

m <- matrix(rt(n*l, 5), ncol=n)
colnames(m) <- paste('V', 1:n, sep='')
df <- m %>% as_tibble() %>% melt()
df$x <- rep(1:l,times=n)
ggplot(df, aes(x,value)) + geom_line() + geom_point() + facet_wrap(~variable)


#install.packages('openssl')
library(tidyverse)
df <- read.csv('/home/diogo/Jupyter/tdef/cryer/datasets/larain.dat')
df$year <- (1877 + as.numeric(row.names(df)))

ggplot(df, aes(x=year, y=larain)) + geom_line() + geom_point() +
  scale_x_continuous(breaks=df$year[c(TRUE, FALSE)]) +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  xlab('year') + ylab('rainfall (inches)')

ggplot(df, aes(x=lag(larain)[c(-1)], y=larain[c(-1)])) + geom_point() +
  xlab('lagged rainfall (inches)') + ylab('rainfall (inches)')

df$larain
lag(df$larain)[c(-1)]
lead(df$larain)

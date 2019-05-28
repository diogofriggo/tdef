library(forecast)
library(tidyverse)
library(lubridate)

setwd('/home/diogo/Jupyter/tdef')
path <- '/home/diogo/Jupyter/tdef/Res025_ERA5.txt'

data <- read_table2(path, skip=9, comment="--") %>% 
  tail(-3) %>% 
  mutate(time=with_tz(ymd_hms(paste(Date, `Time(UTC)`)), tzone='Brazil/East')) %>% 
  mutate(hour=hour(time), day=date(time), month=month(time,label=TRUE, abbr=FALSE), year=year(time)) %>% 
  mutate(year_month = paste(year, month)) %>% 
  rename(speed=c_ws)

nff <- function(x = NULL, n = NULL, up = 10L, plot = TRUE, add = FALSE, main = NULL, ...){
  #The direct transformation
  #The first frequency is DC, the rest are duplicated
  dff = fft(x)
  #The time
  t = seq(from = 1, to = length(x))
  #Upsampled time
  nt = seq(from = 1, to = length(x)+1-1/up, by = 1/up)
  #New spectrum
  ndff = array(data = 0, dim = c(length(nt), 1L))
  ndff[1] = dff[1] #Always, it's the DC component
  if(n != 0){
    ndff[2:(n+1)] = dff[2:(n+1)] #The positive frequencies always come first
    #The negative ones are trickier
    ndff[length(ndff):(length(ndff) - n + 1)] = dff[length(x):(length(x) - n + 1)]
  }
  #The inverses
  indff = fft(ndff/length(x), inverse = TRUE)
  idff = fft(dff/length(x), inverse = TRUE)
  if(plot){
    if(!add){
      plot(x = t, y = x, pch = 16L, xlab = "Time", ylab = "Measurement",
           main = ifelse(is.null(main), paste(n, "harmonics"), main))
      print(idff)
      lines(y = Mod(idff), x = t, col = adjustcolor(1L, alpha = 0.5))
    }
    lines(y = Mod(indff), x = nt, ...)
  }
  ret = data.frame(time = nt, y = Mod(indff))
  return(ret)
}

x <- data$speed
x <- x[(length(x)-24*7*12):length(x)]
colors = rainbow(36L, alpha = 0.3)
#nff(x = x, n = 36L, up = 100L, col = colors[1])
for(i in 1:18){
  ad = ifelse(i == 1, FALSE, TRUE)
  nff(x = x, n = i, up = 10L, col = colors[i], add = ad, main = "All waves up to 18th harmonic")
}

#x2 <- diff(x) #detrended
x2 <- x[-(1:(length(x) %% 6))] #divisible by 12
#install.packages('GeneCycle')
library(GeneCycle)
f.data <- GeneCycle::periodogram(x2)
harmonics <- 1:50
plot(f.data$freq[harmonics]*length(x2), 
     f.data$spec[harmonics]/sum(f.data$spec), 
     xlab="Harmonics (Hz)", ylab="Amplitute Density", type="h")

stats::spectrum(x)

plot.frequency.spectrum <- function(X.k, xlimits=c(0,length(X.k))) {
  plot.data  <- cbind(0:(length(X.k)-1), Mod(X.k))
  
  # TODO: why this scaling is necessary?
  plot.data[2:length(X.k),2] <- 2*plot.data[2:length(X.k),2] 
  
  plot(plot.data, t="h", lwd=2, main="", 
       xlab="Frequency (Hz)", ylab="Strength", 
       xlim=xlimits, ylim=c(0,max(Mod(plot.data[,2]))))
}

X.k <- fft(x2)
plot.frequency.spectrum(X.k,xlimits=c(0,100))

#install.packages("TSA")
#library(TSA)
x <- data$speed
p <- periodogram(x)
plot(periodogram(x), type='h')

#OR
p <- spec.mtm(x, deltat = 0.1, nw = 6, k = 10, Ftest = TRUE)
x <-c()
y <-c()
window <- 2000
for (k in 2200:(length(p$spec)-window)){
  if((all(p$spec[k] >= p$spec[(k-window):(k+window)]))){
    x <- c(x, p$freq[k])
    y <- c(y, p$spec[k])
  }
}
length(x)
df <- tibble(x=x, y=y)
#install.packages('ggetho')
breaks <- c(1/24, 1/12, 1/8, 1/6, 1/4.8, 1/4)
labels <- c('1/24h', '1/12h', '1/8h', '1/6h', '1/4h48min ', '1/4h')
g <- ggplot(df, aes(x,y)) + 
  geom_point() + 
  scale_x_continuous(breaks=breaks, labels=labels) + 
  #theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = 'Frequência', y = 'Periodograma')

for (k in 1:nrow(df)){
  g <- g + geom_segment(x=df$x[k], y=0, xend=df$x[k], yend=df$y[k], size=5, lineend = 'round')
}
g

#install.packages("multitaper")
library(multitaper)
plot(s$freq, s$mtm$Ftest, type='l')
plot(s$freq, s$spec, type='l', log = 'y')

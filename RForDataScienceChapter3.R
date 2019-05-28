#install.packages('nycflights13')
library(nycflights13)
library(tidyverse)
#attr(flights, 'names') #get column names
?flights
View(flights) #view entire dataset
#***dplyr***
#pick rows (observations) by their values       : filter()
#reorder rows                                   : arrange()
#pick cols (variables) by their names           : select()
#create new cols with functions of existing cols: mutate()
#collapse many values down a single summary     : summarize()

#change the scope of each of the above to operate on a group-by-group basis instead of the entire dataset: group_by()

#first argument: tibble
#other arguments: what to do with the tibble using col names
#result: a new dataframe

filter(flights, month==1, day==1)
#jan1 <- filter(flights, month==1, day==1) #dplyr never mutates input
#(jan <- filter(flights, month==1, day==1)) #assign and display

c(sqrt(2)^2==2,near(sqrt(2)^2,2))
c(1/49 * 49 == 1,near(1/49 * 49, 1))

filter(flights, month==1 | month==12)
filter(flights, month %in% c(1,12))
#!(x&y) is the same as !x | !y
#find fligths that weren't delayed on arrival or departure
filter(flights, dep_delay <= 120, arr_delay <= 120)

c(NA > 5, 10 == NA, NA + 10, NA / 2, NA==NA, NA==NA)
x <- NA
y <- NA
x==y
is.na(x)
df <- tibble(x=c(1,NA,3))
filter(df, x>1)
filter(flights, arr_delay >= 120)
filter(flights, dest %in% c('IAH', 'HOU'))
?flights
airlines
filter(flights, carrier %in% c('UL', 'UA'))
filter(flights, month %in% c(7,8,9))
filter(flights, arr_delay >= 120, dep_delay <= 0)
filter(flights, arr_delay >= 60, air_time >= 30)
filter(flights, !between(dep_time, 601, 2359))
#unique(flights$dep_time) %>% sort() %>% tail(200)
for (column in attr(flights, 'names')){
  print(c(column,filter(flights, UQ(as.name(column)) %>% is.na()) %>% nrow()))
}
NA ^ 0
NA | T
F & NA
NA * 0
arrange(flights, year, month, day)
arrange(flights, desc(arr_delay))
df <- tibble(x=c(5,2,NA))
arrange(df, x)
arrange(df, desc(x))
flights %>% arrange(desc(is.na(dep_time)))


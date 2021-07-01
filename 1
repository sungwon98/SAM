# SAM

#install.packages("rmarkdown")
#install.packages("rmarkdown", dependencies = TRUE)
library(rmarkdown)

#install.packages("nycflights13")
library("nycflights13")
#install.packages("tidyverse")
library(tidyverse)

flights

library(magrittr)

flights %>% head

flights%>%dim

flights%>%summary

#filter(flights, month==1, day==1)%>%head(10)
flights %>% filter(month==1, day==1) %>% head(10)

(dec25 <- flights %>% filter(month == 12, day == 25))

flights %>% filter(month==11 | month==12) %>% head(5)

flights %>% filter(month==11 & day==12) %>% head(5)

flights %>% filter(!(month==11 | day==12)) %>% head(5)

######
#1
(flights %>% filter(dep_delay >= 120))[,"carrier"]

#2
(flights %>% filter(dep_delay <= 0 & arr_delay > 0))[,"carrier"]

#3
(flights %>% filter(dep_delay >= 60 & arr_delay <=30))[,"carrier"]

#4
#sum(is.na(flights[,c("arr_time", "sched_dep_time", "sched_arr_time",
#                     "dep_delay", "arr_delay", "hour", "minute",
#                     "carrier", "tailnum", "flight", "origin",
#                     "dest", "air_time", "distance", "time_hour")]))


X <- c("arr_time", "sched_dep_time", "sched_arr_time",
  "dep_delay", "arr_delay", "hour", "minute",
  "carrier", "tailnum", "flight", "origin",
  "dest", "air_time", "distance", "time_hour")

n = length(X)
num <- rep(NA, n)
for (i in 1:n) {
  num[i] = sum(is.na(flights[,X[i]]))
}

sum(is.na(flights[,"dep_time"]))
num
which(num==sum(is.na(flights[,"dep_time"])))
X[4]
flights[,c("dep_time","dep_delay")]
sum(is.na(flights[,"dep_time"])&is.na(flights[,"dep_delay"]))


#sol
flights.na = flights %>% filter(!is.na(dep_time))
sum(is.na(flights.na[,"dep_delay"]))



#####your turn 2####

flights$speed <- flights$distance/(flights$air_time/60)

max(flights$speed, na.rm=T)

((flights %>% arrange(desc(speed)))[,"carrier"])[1,]


flights %>% arrange(dep_delay)

flights %>% arrange(desc(dep_delay))

flights %>% arrange(year, month, desc(day))

#########

flights %>% select(year, month, day)

flights %>% select(year:day)

flights %>% select(-(year:day))

flights %>% select(starts_with("arr"))

flights %>% select(ends_with("time"))

flights %>% select(contains("dep"))

#### your turn 3

flights %>% select(starts_with(c("arr", "dep")))


######
flights %>% select(year:day, ends_with("delay"), distance, air_time) -> flights2
flights2 %>%
  mutate(gain = dep_delay-arr_delay,
         speed = distance/(air_time/60))



flights2 %>%
  transmute(gain = dep_delay-arr_delay,
            speed = distance/air_time*60)

#### your turn 4
flights3 <- flights %>% select(dep_time, sched_dep_time)
flights3 <- flights3 %>% filter(!is.na(flights3))

nchar(flights3$dep_time)
#hour
flights3$dep_hour = substr(flights3$dep_time, 1, nchar(flights3$dep_time)-2)
flights3$dep_hour
#minute
flights3$dep_min = substr(flights3$dep_time, nchar(flights3$dep_time)-1, nchar(flights3$dep_time))
flights3$dep_min

flights3


nchar(flights3$sched_dep_time)
#hour
flights3$sched_dep_hour = substr(flights3$sched_dep_time, 1, nchar(flights3$sched_dep_time)-2)
#minute
flights3$sched_dep_min = substr(flights3$sched_dep_time, nchar(flights3$sched_dep_time)-1, nchar(flights3$sched_dep_time))

flights3


#4-2
A = flights$arr_time - flights$dep_time
A = A %>% filter(!is.na(A))
n = length(A)
for (i in 1:n){
  if (A[i] < 0) {
    A[i] = A[i]+2400
  }
}
# can not solve

#4-3
flights$dep_time %>% head(10)
flights$sched_dep_time %>% head(10)
flights$dep_delay %>% head(10)
# dep_time - sched_dep_time = dep_delay

(flights$dep_time - flights$sched_dep_time) == flights$dep_delay


flights$arr_time %>% head(10)
flights$sched_arr_time %>% head(10)
flights$arr_delay %>% head(10)
# arr_time - sched_arr_time = arr_delay

##########
flights %>% summarise(delay=mean(dep_delay, na.rm=T))

flights %>%
  group_by(year, month, day) %>%
  summarise(delay=mean(dep_delay, na.rm=T))

flights %>%
  group_by(year, month, day) %>%
  summarise(delay=mean(dep_delay, na.rm=T), n = n())

###
#5-1
mean_carr <- flights %>% group_by(carrier) %>%
  summarise(mean_carr = mean(dep_delay,na.rm=TRUE))

var_carr <- flights %>% group_by(carrier) %>% 
  summarise(var_carr = var(dep_delay, na.rm=TRUE))

(mean_carr %>% arrange(desc(mean_carr)))[1,]
(var_carr %>% arrange(desc(var_carr)))[1,]

#5-2
mean_month <- flights %>% group_by(month) %>%
  summarise(mean_data = mean(dep_delay, na.rm=TRUE))
(mean_month %>% arrange(desc(mean_month)))

mean_day <- flights %>% group_by(day) %>%
  summarise(mean_day = mean(dep_delay, na.rm=TRUE))
(mean_day %>% arrange(desc(mean_day)))

mean_m_d <- flights %>% group_by(year, month, day) %>%
  summarise(mean_m_d = mean(dep_delay, na.rm=TRUE), n=n())
(mean_m_d %>% arrange(desc(mean_m_d)))

#5-3
var_carr2 = flights %>% group_by(month=12, carrier) %>%
  summarise(var_carr2 = var(dep_delay,na.rm = TRUE))
var_carr2 %>% arrange(var_carr2)

var_carr3 = flights %>% group_by(day=8, carrier) %>%
  summarise(var_carr3 = var(dep_delay,na.rm = TRUE))
var_carr3 %>% arrange(var_carr3)

var_carr4 = flights %>% group_by(month=3, day=8, carrier) %>%
  summarise(var_carr4 = var(dep_delay,na.rm = TRUE))
var_carr4 %>% arrange(var_carr4)

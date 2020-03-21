# 
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)

confirmed <- read_csv("~/Projekte/covid/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")

X <- confirmed %>% filter(`Country/Region`=='Germany')

xx <- X[5:length(X)]
tt <- mdy(names(xx))
xx <- unlist(xx)
n <- length(xx)
y <- tibble(time = tt,
            confirmed = xx,
            replication_rate = c(NA, confirmed[2:n]/confirmed[1:(n-1)]))
#plot(diff(xx) / xx[2:length(xx)])
p <- y %>% ggplot(aes(time, replication_rate, text = confirmed)) + geom_line()
q <- plotly::ggplotly(p, dynamicTicks = TRUE)
print(q)

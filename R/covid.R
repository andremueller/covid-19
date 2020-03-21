# 
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)

# confirmed <- read_csv("~/Projekte/covid/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")
# X <- confirmed %>% filter(`Country/Region`=='Germany')
# 
# xx <- X[5:length(X)]
# tt <- mdy(names(xx))
# xx <- unlist(xx)
# n <- length(xx)
# y <- tibble(time = tt,
#             confirmed = xx,
#             replication_rate = c(NA, confirmed[2:n]/confirmed[1:(n-1)]))
#plot(diff(xx) / xx[2:length(xx)])
y <- read_csv("../COVID-19/data/cases_time.csv",
              col_types = cols(
                Country_Region = col_character(),
                Last_Update = col_character(),
                Confirmed = col_double(),
                Deaths = col_double(),
                Recovered = col_double(),
                Active = col_double(),
                Delta_Confirmed = col_double(),
                Delta_Recovered = col_double()
              )) %>% mutate(Last_Update = mdy(Last_Update))

y1 <- y %>% 
  mutate(Replication_Rate = Delta_Confirmed / Confirmed)
  
p <- y1 %>% 
  filter(Country_Region == 'Germany') %>%
  ggplot(aes(Last_Update, Replication_Rate, text = Confirmed)) + geom_line()
q <- plotly::ggplotly(p, dynamicTicks = TRUE)
print(q)

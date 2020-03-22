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
events <- read_csv("events.csv") %>% mutate(Date = ymd(Date))

world_time <- y %>% group_by(Last_Update) %>% 
  summarize(Confirmed = sum(Confirmed), 
            Deaths = sum(Deaths),
            Recovered = sum(Recovered),
            Active = sum(Active),
            Mortality_Rate = Deaths/Confirmed * 100.0) %>%
  ungroup() %>%
  pivot_longer(-Last_Update)

p <- world_time %>% ggplot(aes(Last_Update, y=value, color=name)) +
  theme_bw() + geom_line() + ggtitle('Worldwide COVID-19')
#scale_x_log10() + annotation_logticks()
print(plotly::ggplotly(p, dynamicTicks = TRUE))

LOWER_LIMIT <- 10
y1 <- y %>%
  group_by(Country_Region) %>%
  mutate(Replication_Rate = (Confirmed - lag(Confirmed)) / lag(Confirmed)) %>%
  filter(Confirmed >= LOWER_LIMIT) %>%
  mutate(Day = row_number()) %>%
  ungroup()

Ev <- left_join(events, y1 %>% filter(Day==1)) %>% select(-Day) %>%
  rename(Outbreak = Last_Update) %>%
  mutate(Day = time_length(Date - Outbreak, 'days'))

y_current <- y1 %>% group_by(Country_Region) %>% slice(n()) %>% 
  ungroup() %>% arrange(desc(Confirmed)) %>% 
  mutate(Confirmed_per_Day = Confirmed / Day)
View(y_current)

total <- y %>% group_by(Country_Region) %>% slice(n()) %>% ungroup() %>%
  summarize(max(Last_Update), sum(Confirmed), sum(Deaths), sum(Recovered))
View(total)

sel <- c('Germany', 'China', 'Italy', 'France')
sel <- y_current$Country_Region[1:8]
p <- y1 %>% 
  filter(Country_Region %in% sel) %>%
  ggplot(aes(Last_Update, Replication_Rate, text = Confirmed, color = Country_Region)) + geom_line()
q <- plotly::ggplotly(p, dynamicTicks = TRUE)
print(q)
View(y %>% filter(Country_Region=='Germany'))

y2 <- y1 %>% filter(Country_Region %in% sel) %>% 
  mutate(info = paste0(Last_Update, " Confirmed = ", Confirmed))

# Active
p1 <- y2 %>%
  ggplot(aes(Day, Active, color = Country_Region, text = Confirmed)) + 
  theme_bw() +
  geom_line() + geom_line(aes(y=Deaths), linetype='dotted')  #+ scale_y_log10()
print(p1)
q1 <- plotly::ggplotly(p1, dynamicTicks = TRUE)
print(q1)

# Confirmed
p1 <- y2 %>%
  ggplot(aes(Day, Confirmed, color = Country_Region)) + 
  theme_bw() + 
  geom_line() + geom_line(aes(y=Deaths), linetype='dotted')  #+ scale_y_log10()
print(p1)
q1 <- plotly::ggplotly(p1, dynamicTicks = FALSE)
print(q1)

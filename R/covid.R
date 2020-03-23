# COVID-19 Plots
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)

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
updated <- max(y$Last_Update)
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

LOWER_LIMIT <- 100
y1 <- y %>%
  group_by(Country_Region) %>%
  mutate(Replication_Rate = 100 * (Confirmed - lag(Confirmed)) / lag(Confirmed)) %>%
  mutate(Death_Rate = 100 * (Deaths - lag(Deaths)) / lag(Deaths)) %>%
  mutate(Mortality_Rate = 100 * Deaths / Confirmed) %>%
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

# Select the top n countries
sel <- y_current$Country_Region[1:8]

# Replication rates
p <- y1 %>% 
  filter(Country_Region %in% sel) %>%
  ggplot(aes(Day, Replication_Rate, text = Confirmed, color = Country_Region,
             Confirmed = Confirmed, Deaths = Deaths, Last_Update = Last_Update)) + 
  geom_line() + geom_line(aes(y=Death_Rate), linetype='dotted') + theme_bw() +
  ylab('Replication Rate [%]')
q <- plotly::ggplotly(p, dynamicTicks = TRUE)
print(q)
View(y %>% filter(Country_Region=='Germany'))

y2 <- y1 %>% filter(Country_Region %in% sel)

# Confirmed Cases
p1 <- y2 %>%
  ggplot(aes(Day, Confirmed, color = Country_Region, 
             Last_Update = Last_Update, Deaths = Deaths)) + 
  ggtitle(sprintf("COVID-19 Confirmed Cases (%s)", updated)) +
  theme_bw() + geom_line() +
  scale_y_log10() + annotation_logticks(sides="l")
print(p1)
q1 <- plotly::ggplotly(p1, dynamicTicks = FALSE)
print(q1)

# Active cases
# p1 <- y2 %>%
#   ggplot(aes(Day, Active, color = Country_Region, 
#              Confirmed = Confirmed, Last_Update = Last_Update, Deaths = Deaths)) + 
#   ggtitle(sprintf("COVID-19 Active Cases (%s)", updated)) +
#   theme_bw() + geom_line() +
#   scale_y_log10() + annotation_logticks(sides="l")
# print(p1)
# q1 <- plotly::ggplotly(p1, dynamicTicks = FALSE)
# print(q1)

# Deaths
p1 <- y2 %>%
  ggplot(aes(Day, Deaths, color = Country_Region, 
             Confirmed = Confirmed, Last_Update = Last_Update)) + 
  ggtitle(sprintf("COVID-19 Deaths (%s)", updated)) +
  theme_bw() + geom_line() +
  scale_y_log10() + annotation_logticks(sides="l")
print(p1)
q1 <- plotly::ggplotly(p1, dynamicTicks = FALSE)
print(q1)

# Mortality Rate
p1 <- y2 %>%
  ggplot(aes(Day, Mortality_Rate, color = Country_Region, 
             Confirmed = Confirmed, Deaths = Deaths, Last_Update = Last_Update)) + 
  ggtitle(sprintf("COVID-19 Mortality Rate (%s)", updated)) +
  theme_bw() + geom_line() +
  scale_y_log10() + annotation_logticks(sides="l") +
  ylab("Mortality Rate [%]")
print(p1)
q1 <- plotly::ggplotly(p1, dynamicTicks = FALSE)
print(q1)

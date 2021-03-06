# COVID-19 Plots
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
requireNamespace("curl")

if (!is.null(dev.list())) {
  dev.off()
}

DO_CACHE <- TRUE

# load data from the John Hopkins university
load_john_hopkins_data <- function() {
  read_csv("../COVID-19/data/cases_time.csv",
           col_types = cols(
             Country_Region = col_character(),
             Last_Update = col_character(),
             Confirmed = col_double(),
             Deaths = col_double(),
             Recovered = col_double(),
             Active = col_double(),
             Delta_Confirmed = col_double(),
             Delta_Recovered = col_double()
           )) %>% mutate(Last_Update = mdy(Last_Update)) %>%
    select(-Active, -Recovered, -Delta_Recovered) %>%
    mutate(Delta_Deaths = c(0, diff(Deaths)))
}

# load data from European Centre for Disease Prevention and Control (ECDC)
# transform into the John Hopkins format
load_ecdc_data <- function(){
  data_url <- paste0("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-", 
                     format(Sys.time(), "%Y-%m-%d"), ".csv")
  if (DO_CACHE) {
    cache_file <- "covid19_ecdc_cache.csv"
    if (!file.exists(cache_file)) {
      curl::curl_download(data_url, cache_file)
    }
  } else {
    cache_file <- data_url
  }
  raw <- read_csv(cache_file)
  raw %>%
    mutate(DateRep = dmy(DateRep)) %>%
    rename(Country_Region = `Countries and territories`) %>%
    rename(Last_Update = DateRep) %>%
    rename(Delta_Confirmed = Cases) %>%
    rename(Delta_Deaths = Deaths) %>%
    group_by(Country_Region) %>%
    arrange(Last_Update) %>%
    mutate(Confirmed = cumsum(Delta_Confirmed)) %>%
    mutate(Deaths = cumsum(Delta_Deaths)) %>%
    ungroup() %>%
    select(Country_Region, Last_Update, Confirmed, Deaths, Delta_Confirmed, Delta_Deaths)
}


y <- load_ecdc_data()

updated <- max(y$Last_Update)
events <- read_csv("events.csv") %>% mutate(Date = ymd(Date))

world_time <- y %>% group_by(Last_Update) %>% 
  summarize(Confirmed = sum(Confirmed), 
            Deaths = sum(Deaths),
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
  summarize(max(Last_Update), sum(Confirmed), sum(Deaths))
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
  ylab("Mortality Rate [%]")
print(p1)
q1 <- plotly::ggplotly(p1, dynamicTicks = TRUE)
print(q1)

# Delta_Confirmed
p1 <- y2 %>%
  ggplot(aes(Day, Delta_Confirmed, color = Country_Region,
             Delta_Deaths = Delta_Deaths,
             Confirmed = Confirmed, Deaths = Deaths, Last_Update = Last_Update)) + 
  ggtitle(sprintf("COVID-19 Differential Cases (%s)", updated)) +
  geom_line(aes(y=Delta_Deaths), linetype = 'dotted') +
  theme_bw() + geom_line() +
  ylab("Number of Cases")
print(p1)
q1 <- plotly::ggplotly(p1, dynamicTicks = TRUE)
print(q1)

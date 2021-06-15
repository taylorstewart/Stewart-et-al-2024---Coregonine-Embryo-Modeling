#### CLEAR THE ENVIRONMENT FIRST ---------------------------------------------

rm(list = ls(all.names = TRUE))


#### LOAD PACKAGES -----------------------------------------------------------

library(dplyr)
library(data.table)
library(readxl)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(stringr)
library(lubridate)
library(lubridateExtras)


data <- read.csv("data/climate-simulations/weeklyChange-byDecade-byLatGroup.csv") %>% 
  filter(scenario == "rcp85", decade == 2030, lat.group == "mid") %>% 
  select(yday, perc.change) %>% 
  mutate(perc.change = (perc.change/7)/100)

temp.1 <- read_excel("data/LakeSuperior_APIS_Temp.xlsx", sheet = "2016")
temp.1 <- temp.1 %>%
  mutate(yday = yday(date),
         date = as.Date(ifelse(yday > 240, 
                               as.Date(yday, origin = "2020-12-31"), 
                               as.Date(yday, origin = "2021-12-31")), 
                        origin = "1970-01-01")) %>% 
  filter(yday > 276 | yday < 200)

temp.1.template <- left_join(temp.1, data) %>% 
  select(yday, perc.change) %>% 
  fill.na()

temp.1.start.temp <- temp.1 %>% arrange(date) %>% slice(1) %>% pull(temp.c)
temp.1.start.yday <- temp.1 %>% arrange(date) %>% slice(1) %>% pull(yday)


temp.2090 <- data.frame(yday = temp.1.start.yday, temp.c = temp.1.start.temp) %>% 
  right_join(temp.1.template) %>% 
  mutate(date = as.Date(ifelse(yday > 240, as.Date(yday, origin = "2020-12-31"), as.Date(yday, origin = "2021-12-31")), origin = "1970-01-01"))

fill_in <- function(x, p) {
  x <- ifelse(!is.na(x), x, lag(x, 1) * (1 + p))
  x
}

for(i in 1:nrow(temp.2090)) {
  temp.2090 <- temp.2090 %>%
    mutate(temp.c = fill_in(temp.c, perc.change))
}



ggplot(temp.2090, aes(x = date, y = temp.c)) +
  geom_line() +
  geom_line(data = temp.1) +
  geom_hline(yintercept = 4) +
  theme_few()


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


spawn <- read_excel("data/lake-geneva/lake-geneva-spawning.xlsx", sheet = "lake-geneva-spawning") %>% 
  filter(percent.abundance > 10, !is.na(temp.c)) %>% 
  group_by(year) %>% 
  arrange(date) %>%
  filter(row_number() %in% c(1, n())) %>% ungroup() %>% 
  mutate(spawn.group = rep(c("start", "end", "start", "end", "start", "end", "start", "end"))) %>% 
  group_by(spawn.group) %>% 
  summarize(temp.c = mean(temp.c))


test1 <- read.csv("/Users/taylor/Desktop/raw water export 1", header = TRUE)
test2 <- read.csv("/Users/taylor/Desktop/raw water export 2", header = TRUE)
test3 <- read.csv("/Users/taylor/Desktop/raw water export 3", header = TRUE)
test4 <- read.csv("/Users/taylor/Desktop/raw water export 4", header = TRUE)
test5 <- read.csv("/Users/taylor/Desktop/raw water export 5", header = TRUE)
test6 <- read.csv("/Users/taylor/Desktop/raw water export 6", header = TRUE)
test7 <- read.csv("/Users/taylor/Desktop/raw water export 7", header = TRUE)
test8 <- read.csv("/Users/taylor/Desktop/raw water export 8", header = TRUE)

#### CLEAR THE ENVIRONMENT FIRST ---------------------------------------------

rm(list = ls(all.names = TRUE))


#### LOAD PACKAGES -----------------------------------------------------------

library(dplyr)
library(data.table)


### CREATE A LIST OF FILES ----------------------------------------------------------------------

water.files <- list.files('raw-data/lake-superior/Temperature', pattern = "raw water", full.names = TRUE)


data <- do.call(rbind, lapply(water.files, function(i) {
  data <- read.csv(i, header = TRUE)
  
  test <- data %>% mutate(date = as.POSIXct(str_match(Submit.Date.Time, ", \\s*(.*?)\\s* -")[,2],
                                            format = "%m/%d/%Y"),
                          time = substr(Submit.Date.Time, nchar(.$Submit.Date.Time)-4, nchar(.$Submit.Date.Time)),
                          datetime = as.POSIXct(paste(date, time, sep = " "), format = "%Y-%m-%d %H:%M")) %>% 
    select(datetime, date, time, temp.c = Temperature)
})) %>% arrange(datetime) %>% 
  filter(!is.na(temp.c)) %>% 
  distinct(datetime, .keep_all = TRUE)

data.summary <- data %>% group_by(date) %>% 
  summarize(temp.c = mean(temp.c)) %>% 
  mutate(yday = yday(date),
         year = year(date),
         month = month(date),
         day = day(date)) %>% 
  filter(yday > 240 | yday < 190) %>% 
  mutate(year = ifelse(yday > 239, year+1, year),
         depth.m = 13) %>% 
  select(date, year, month, day, depth.m, temp.c)



write.csv(data.summary, "data/lake-superior-thunder-bay/lake-superior-thunder-bay-temperature.csv", row.names = FALSE)

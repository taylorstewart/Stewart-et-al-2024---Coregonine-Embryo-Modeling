library(dplyr)
library(readxl)

data <- read_excel("data/lake-geneva/lake-geneva-temperature-sonde-all.xlsx", sheet = "sonde") %>% 
  mutate(depth.bin = cut(depth.m, breaks = seq(-0.5, 309.5, 1)))

bin <- data.frame(depth.bin = unique(data$depth.bin),
                  mid.depth = seq(0, 309, 1))

data <- data %>% left_join(bin) %>% 
  group_by(date, mid.depth) %>% 
  summarize(temp.c = mean(temp.c)) %>% 
  filter(!is.na(temp.c)) %>% 
  select(date, depth.m = mid.depth, temp.c)

write.csv(data, "data/lake-geneva/lake-geneva-temperature-sonde-all.csv", row.names = FALSE)

data.6 <- data %>% filter(depth.m == 8)

data.6.interp <- data.frame(do.call(cbind, approx(x = data.6$date, y = data.6$temp.c, xout = seq(min(data.6$date), max(data.6$date), "day")))) %>% 
  mutate(date = as.POSIXct(x, origin = "1970-01-02")) %>% 
  select(date, temp.c = y)
write.csv(data.6.interp, "data/lake-geneva/lake-geneva-temperature-sonde-8m-interpolated.csv", row.names = FALSE)


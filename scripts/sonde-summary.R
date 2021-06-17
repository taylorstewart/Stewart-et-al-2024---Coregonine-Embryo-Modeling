library(dplyr)
library(readxl)

data <- read_excel("data/lake-annecy/lake-annecy-temperature-sonde-all.xlsx", sheet = "sonde") %>% 
  mutate(depth.bin = cut(depth.m, breaks = seq(-0.5, 69.5, 1)))

bin <- data.frame(depth.bin = levels(unique(data$depth.bin)),
                  mid.depth = seq(0, 69, 1))

data <- data %>% left_join(bin) %>% 
  group_by(date, mid.depth) %>% 
  summarize(temp.c = mean(temp.c)) %>% 
  filter(!is.na(temp.c)) %>% 
  select(date, depth.m = mid.depth, temp.c)

write.csv(data, "data/lake-annecy/lake-annecy-temperature-sonde-all.csv", row.names = FALSE)

depth <- 8
data.6 <- data %>% filter(depth.m == depth)

data.6.interp <- data.frame(do.call(cbind, approx(x = data.6$date, y = data.6$temp.c, xout = seq(min(data.6$date), max(data.6$date), "day")))) %>% 
  mutate(date = as.POSIXct(x, origin = "1970-01-02"),
         depth.m = depth) %>% 
  select(date, depth.m, temp.c = y)
write.csv(data.6.interp, "data/lake-annecy/lake-annecy-temperature-sonde-8m-interpolated.csv", row.names = FALSE)


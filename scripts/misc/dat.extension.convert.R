library(dplyr)
library(tidyr)
library(data.table)

data <- read.delim("/Users/taylor/Downloads/LowerLakeConstance_T.dat", header = TRUE, sep = ",") 

col.names <- colnames(data) %>% 
  gsub("X.", "", .)

colnames(data) <- col.names

data.date <- data %>% 
  mutate(datetime = as.Date("1981-01-01") + X) %>% 
  select(datetime, everything(), -X)

data.tidy <- data.date %>% pivot_longer(cols = 2:48, names_to = "depth_m", values_to = "temp_c")

write.csv(data.tidy, "data/lake-constance/lake-constance-lower-temperature-historical-model.csv", row.names = FALSE)



data2 <- fread("data/lake-constance/lake-constance-lower-temperature-historical-model.csv")

data.tidy.summary <- data2 %>% group_by(datetime, depth_m) %>% 
  summarize(temp_c = round(mean(temp_c), 3))

write.csv(data.tidy.summary, "data/lake-constance/lake-constance-lower-temperature-historical-model.csv", row.names = FALSE)


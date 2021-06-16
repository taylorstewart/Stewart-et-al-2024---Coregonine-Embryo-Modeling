#### CLEAR THE ENVIRONMENT FIRST ---------------------------------------------

rm(list = ls(all.names = TRUE))


#### LOAD PACKAGES -----------------------------------------------------------

library(ggplot2)
library(lubridate)
library(dplyr)
library(readxl)


# LOAD LARVAL LENGTH AND ABUNDANCE DATA -------------------------------------------------------

larvae <- read_excel("data/lake-superior-apostle-islands/lake-superior-apis-larval-lengths.xlsx", sheet = "data") %>% 
  filter(!is.na(LENGTH))


## Total length-at-hatch = 9.936 (Stewart et al., 2021)
## McCormick et al. (1971) - 3% decrease at 4°C
#dates <- data.frame(LENGTH = c(21, 20, seq(18, 14, -1), seq(13.5, 8, -0.5), 7),
#                    back.date = c(24, 22, 19, 18, 16, 14, 12, 10, 9, 8, 6, 5, 4, 2, 1, 0, 0, 0, 0, 0))

## Oyadomari & Auer, 2008 - 0.18 mm^-1
dates <- data.frame(LENGTH = c(21, 20, seq(18, 14, -1), seq(13.5, 8, -0.5), 7),
                    back.date = c(62, 56, 45, 40, 34, 28, 23, 20, 17, 14, 12, 9, 6, 3, 1, 0, 0, 0, 0, 0))

larvae.back <- left_join(larvae, dates) %>% 
  mutate(hatch.date = DATE-(back.date*86400),
         hatch.date.noYear = yday(hatch.date),
         hatch.date.noYear = as.Date(hatch.date.noYear, origin = "2020-12-31"))

larvae.back.summary <- larvae.back %>% group_by(YEAR) %>% 
  summarize(min.date = min(hatch.date),
            mean.date = mean(hatch.date),
            max.date = max(hatch.date),
            sigma.hatch = (max.date - mean.date) / 2.58,
            lower.dist = mean.date-sigma.hatch,
            upper.dist = mean.date+sigma.hatch)

write.csv(larvae.back.summary, "data/lake-superior-apostle-islands/lake-superior-apis-hatching.csv", row.names = FALSE)


ggplot(larvae.back, aes(x = hatch.date.noYear)) + 
  geom_histogram(aes(y = ..density..)) + 
  geom_density(col = "red") +
  labs(y = "Daily Percent Hatch", x = "Hatch Date") +
  scale_x_date(date_breaks = "5 days", date_labels =  "%b %d") +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap(~YEAR, ncol = 1)


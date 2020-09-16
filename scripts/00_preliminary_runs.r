
# the aim of this script is to do some preliminar test on the egg development model / daily cohorts


library(ggplot2)
library(lubridate)
library(ggthemes)
library(dplyr)



# 01 - temperature data -----------------------
# load temperature data for the Garda lake (Salmaso et al., 2003) as an example
# data frame with date, year and wt (= water temperature - upper layer [0-15 m] in this case)

load(here::here("data/wtemp.RData"))
wtemp$date <- as.Date(wtemp$date)
# ggplot(wtemp, aes(date, wt)) + geom_line() + theme_few() + ylab('Water temperature (°C)') + scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") + theme(axis.text.x=element_text(angle=90, hjust=1))





# 02 - define spawning characteristics---------
# spawning phenology is define as a normal function (Jones et al., 2003), but the first and last day of spawning must be provided, or they are obtained considering the start/stop temperature (should be given) as in Jones et al (2003), considering the first days with running average temperature reaching the critical thresholds

wtemp$t5 <- caTools::runmean(wtemp$wt, 5, align = 'right') # 5 days rolling average

startsp <- 12 # °C [MODEL PARAMETER]
stopsp <- 8.5 # °C [MODEL PARAMETER] # not sure this makes sense at all



# define spawning windows

# start with first season (Example)
dat01 <- subset(wtemp, date >= as_date('1996-10-01') & date <= as_date('1997-06-01'))
#ggplot(dat01, aes(date, wt)) + geom_line() + theme_few() + ylab('Water temperature (°C)') + scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") + theme(axis.text.x=element_text(angle=90, hjust=1))

mystartsp <- dat01$date[min(which(dat01$t5 < startsp))]
mystopsp <- dat01$date[min(which(dat01$t5 < stopsp))]

mu <- as_date((as.numeric(mystartsp) + as.numeric(mystopsp)) / 2) # center of spawning windows
sigm <- (as.numeric(mystopsp) - as.numeric(mu)) / 2.58 # Days
#ggplot(dat01, aes(date, wt)) + geom_line() + theme_few() + ylab('Water temperature (°C)') + scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") + theme(axis.text.x=element_text(angle=90, hjust=1)) + geom_vline(xintercept = mu, colour = "red") + geom_vline(xintercept = as_date(c(as.numeric(mu) - sigm, as.numeric(mu) + sigm)), colour = "blue", lty = 3) + ggtitle("Spawning window")



# 03 Population matrix ----------------------
# let's follow a population of N fertilized eggs

N <- 2000 # define how many embryos we want to follow

spawning <- data.frame(date = as_date(sort(floor(rnorm(N, as.numeric(mu), sigm)))))
# ggplot(spawning, aes(date)) + geom_histogram() + theme_few() 



# Temperature matrix (rows = days, cols = replicate T for the number of specimens)
T <- dat01[, rep(3, N)]


# create a population matrix
pop <- matrix(data = 0, ncol= N, nrow = nrow(dat01)) # colums = specimesn; rows = days



# set 1s in the pop matrix as indicator of spawning date
mymatch <- match(spawning$date , dat01$date)
ind <- lapply(1:N, FUN = function(x){c(mymatch[x], x)})

for(i in 1:length(ind)){
  pop[ind[[i]][1], ind[[i]][2]] <- 1
}


# base matrix for bookeeping Degree Days
pop_ddbase <- pop
for(j in 1:ncol(pop_ddbase)){
  pop_ddbase[,j] <- cumsum(pop_ddbase[ ,j])  # add all 1s after spawning
}

pop_ddbaseT <- pop_ddbase * T # temperature experience by each specimen

for(j in 1:ncol(pop_ddbase)){
  pop_ddbase[,j] <- cumsum(pop_ddbaseT[ ,j])  # potential degree days in the considered period (up to the end of the period)
}


#  age(days) of each egg in the considered period (up to the end of the period)
pop_days <- pop 
for(j in 1:ncol(pop_days)){
  pop_days[,j] <- cumsum(pop_days[ ,j])  
}
for(j in 1:ncol(pop_days)){ # repeat this chunk to sum up all days
  pop_days[,j] <- cumsum(pop_days[ ,j])  
}



# 04 Degree days Model -----------------------
# define the degreedays model
#As an example we use the model for C. lavaretus defined by Eckman (1987) after Luczynski & Kirklwska (1984)
#lnDRi= ln_a + ln_b * T +  ln_c * T^2     #Di = time for DS14 (50% hatch)

ln_a <- log(0.00573) # [MODEL PARAMETER] Eckmann (1987)
ln_b <- log(1.2893) # [MODEL PARAMETER] Eckmann (1987)
ln_c <- log(0.9925) # [MODEL PARAMETER] Eckmann (1987)


dummy <-  pop
for(j in 1:ncol(dummy)){
  dummy[,j] <- cumsum(dummy[ ,j])  
}


# compute the percent development per day
#plot(1:19, exp(ln_a + 1:19 * ln_b + (1:19) ^2 * ln_c), xlim = c(0, 20))

perc_day <- exp(ln_a + pop_ddbaseT * ln_b + pop_ddbaseT ^2 * ln_c)
perc_day <- as.matrix(perc_day * dummy)


# cumulative development
development <- perc_day
for(j in 1:ncol(development)){ 
  development[,j] <- cumsum(development[ ,j])  
}

for(j in 1:ncol(development)){ # set NAs after hatching
  development[(min(which(development[,j]>1)) + 1):nrow(development), j] <- NA
}


# create a hatching matrix
hatch <- matrix(NA, ncol= N, nrow = nrow(dat01))
for(j in 1:ncol(development)){
  hatch[min(which(development[,j]>1)), j]  <- 1
}



# update the other matrices
pop_days[which(is.na(development))] <- NA
pop_days[which(pop_days == 0)] <- NA


pop_dd <- pop_ddbase
pop_dd[which(is.na(development))] <- NA

perc_day[which(is.na(development))] <- NA

perc_day[which(perc_day == 0)] <- NA







# extract and plot some results

perc_daydf <- data.frame(date = dat01$date, data.frame(perc_day))
perc_daydfm <- reshape::melt(perc_daydf, id.vars = "date")
#ggplot(perc_daydfm, aes(date, value, group = variable)) + geom_line(alpha = 0.3) 




devdf <- data.frame(date = dat01$date, data.frame(development))
devdfm <- reshape::melt(devdf, id.vars = "date")
ggplot(devdfm, aes(date, value, group = variable)) + geom_line(alpha = 0.3) + scale_y_log10() + theme_few() + ggtitle("Cumulative development")


ndays <- data.frame(days = unlist(lapply(1:ncol(pop_days), FUN = function(x)max(pop_days[,x], na.rm = T))))
ggplot(ndays, aes(days)) + geom_histogram() + ggtitle("N° days for hatching") +theme_few()



ddays <- data.frame(dd = unlist(lapply(1:ncol(pop_dd), FUN = function(x)max(pop_dd[,x], na.rm = T))))
ggplot(ddays, aes(dd)) + geom_histogram() + theme_few() + ggtitle("Degree days") +xlab("D.D. (°C)")


hatchdf <- data.frame(date = dat01$date, data.frame(hatch))
hatchdfm <- reshape2::melt(hatchdf, id.vars = c("date"))
hatching_out <- group_by(hatchdfm, date) %>% summarise(n_hatch = sum(value, na.rm = T))

ggplot(spawning, aes(date)) + geom_histogram() + theme_few() + ggtitle("Spawning")

ggplot(hatching_out, aes(date, n_hatch)) + geom_col() + theme_few()  + ggtitle("Hatching")



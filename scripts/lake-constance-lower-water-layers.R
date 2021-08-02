library(rLakeAnalyzer)
library(dplyr)
library(data.table)
library(parallel)


temp <- fread("data/lake-constance/lake-constance-lower-temperature-historical-model.csv")


test <- do.call(rbind, lapply(unique(temp$datetime), function(i) {
  print(i)
  temp.filt <- temp %>% filter(datetime == as.Date("1988-11-26"))
  
  for(j in 0:10) {
    df1 <- try({
      wtr.layer(depth = temp.filt$depth_m, measure = temp.filt$temp_c, z0 = j, nseg = 4)
      }, silent = TRUE)
    if(class(df1) == "try-error") {
      j <- j+1
  } else break
  }
  
  segs <- as.data.frame(df1$segments) %>% 
    mutate(date = i,
           layer.name = c("surface", "cline_top", "cline_bottom", "bottom")) %>% 
    select(date, layer.name, layer.depth_m = segment_depth, layer.temp_c = segment_measure)
}))


write.csv(test, "data/lake-constance/lake-constance-lower-temperature-layers.csv", row.names = FALSE)


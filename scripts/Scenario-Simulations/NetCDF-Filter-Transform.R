#### CLEAR THE ENVIRONMENT FIRST -----------------------------------------------------------------

rm(list = ls(all.names = TRUE))


#### START TIME ----------------------------------------------------------------------------------

start <- Sys.time()


#### LOAD PACKAGES -------------------------------------------------------------------------------

library(tidync)
library(dplyr)
library(stringr)
library(parallel)


#### MAKE A LIST OF FILES ------------------------------------------------------------------------

climate.files <- list.files("/Volumes/home/Coregonine-Modeling-ClimateSimulations", pattern = "vic", full.names = TRUE)


#### LOOP ON EACH FILE ---------------------------------------------------------------------------
mclapply(climate.files, mc.cores = 7, function(i) {
  # EXTRACT VARIABLES FROM FILEPATH --------------------------------------------------------------
  impact.model <- str_match(i, "ClimateSimulations/(.*?)\\s*_gfdl")[2]
  climate.forcing <- str_match(i, "lake_\\s*(.*?)\\s*_ewembi")[2]
  climate.scenario <- str_match(i, "ewembi_\\s*(.*?)\\s*_2005soc")[2]
  decade <- as.numeric(str_match(i, "daily_\\s*(.*?)\\s*_")[2])
  
  
  # LOAD NETCDF FILE -----------------------------------------------------------------------------
  climate.output <- tidync(i)
  
  
  # FILTER BY EACH REGION/LAKE -------------------------------------------------------------------
  ## Thunder Bay
  #climate.output.TB <- climate.output %>% 
  #  hyper_filter(lon = lon > -89.5 & lon < -88.5,
  #               lat = lat > 48.2 & lat < 49.0)
  
  ## Apostle Islands
  climate.output.APIS <- climate.output %>% 
    hyper_filter(lon = lon > -91.5 & lon < -90.0,
                 lat = lat < 47.5 & lat > 46.5)
  
  ## Chaumont Bay
  #climate.output.CB <- climate.output %>% 
  #  hyper_filter(lon = lon > -76.9 & lon < -76.1,
  #               lat = lat < 44.5 & lat > 43.7)
  
  ## Lake Konnevesi
  #climate.output.LK <- climate.output %>% 
  #  hyper_filter(lon = lon > 26.0 & lon < 27.0,
  #               lat = lat > 62.0 & lat < 63.0)
  
  
  # TRANSFORM INTO DATAFRAME ---------------------------------------------------------------------
  ## Thunder Bay
  #climate.output.TB.df <- climate.output.TB %>% 
  #  hyper_tibble() %>% 
  #  dplyr::filter(!is.na(watertemp)) %>% 
  #  mutate(date = as.Date(time, origin = "1661-01-01"),
  #         watertemp = watertemp - 273.15) %>% 
  #  select(-time)
  
  ## Apostle Islands
  climate.output.APIS.df <- climate.output.APIS %>% 
    hyper_tibble() %>% 
    dplyr::filter(!is.na(watertemp)) %>% 
    mutate(date = as.Date(time, origin = "1661-01-01"),
           watertemp = watertemp - 273.15) %>% 
    select(-time)
  
  ## Chaumont Bay
  #climate.output.CB.df <- climate.output.CB %>% 
  #  hyper_tibble() %>% 
  #  dplyr::filter(!is.na(watertemp)) %>% 
  #  mutate(date = as.Date(time, origin = "1661-01-01"),
  #         watertemp = watertemp - 273.15) %>% 
  #  select(-time)
  
  ## Lake Konnevesi
  #climate.output.LK.df <- climate.output.LK %>% 
  #  hyper_tibble() %>% 
  #  dplyr::filter(!is.na(watertemp)) %>% 
  #  mutate(date = as.Date(time, origin = "1661-01-01"),
  #         watertemp = watertemp - 273.15) %>% 
  #  select(-time)
  
  
  # FILTER BY YEAR AND SAVE CSV ------------------------------------------------------------------
  lapply(seq(decade, decade+9, 1), function(j) {
    #climate.output.TB.df.filt <- climate.output.TB.df %>% filter(date >= paste0(j, "-01-01"), date <= paste0(j, "-12-31"))
    climate.output.APIS.df.filt <- climate.output.APIS.df %>% filter(date >= paste0(j, "-01-01"), date <= paste0(j, "-12-31"))
    #climate.output.CB.df.filt <- climate.output.CB.df %>% filter(date >= paste0(j, "-01-01"), date <= paste0(j, "-12-31"))
    #climate.output.LK.df.filt <- climate.output.LK.df %>% filter(date >= paste0(j, "-01-01"), date <= paste0(j, "-12-31"))
    
    #write.csv(climate.output.TB.df.filt, paste0("data/climate-simulations/lake-superior-thunder-bay/", impact.model, "-",
    #                                            climate.forcing, "-", climate.scenario, "-TB-", 
    #                                            j, ".csv"), row.names = FALSE)
    write.csv(climate.output.APIS.df.filt, paste0("data/climate-simulations/lake-superior-apostle-islands/", impact.model, "-",
                                                  climate.forcing, "-", climate.scenario, "-APIS-", 
                                                  j, ".csv"), row.names = FALSE)
    #write.csv(climate.output.CB.df.filt, paste0("data/climate-simulations/lake-ontario-chaumont-bay/", impact.model, "-", 
    #                                            climate.forcing, "-", climate.scenario, "-CB-", 
    #                                            j, ".csv"), row.names = FALSE)
    #write.csv(climate.output.LK.df.filt, paste0("data/climate-simulations/lake-konnevesi/", impact.model, "-", 
    #                                            climate.forcing, "-", climate.scenario, "-LK-", 
    #                                            j, ".csv"), row.names = FALSE)
  })
})


#### END TIME AND PRINT ELAPSED TIME -------------------------------------------------------------

end <- Sys.time()
end-start


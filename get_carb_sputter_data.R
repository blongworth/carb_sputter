# get data for carbonate sputtering

library(amstools)
library(tidyverse)
library(here)

# build nested list of wheels and positions to get desired data
wheels <- list("CFAMS121020" = c(15, 84, 81, 97, 99, 100, 101),
               "USAMS112321" = c(0:11),
               "USAMS112621" = c(0:22))

# function to get lines for positions from results file
readRawPos <- function(wheel, pos) {
  getRawWheel(wheel) %>% 
  filter(wheel_pos %in% pos)
}

readPos <- function(wheel, pos) {
  getWheel(wheel) %>% 
  filter(wheel_pos %in% pos)
}

map2_dfr(names(wheels), wheels, readRawPos) %>% 
  write_csv(here("data/raw_carb_sputter.csv"))
map2_dfr(names(wheels), wheels, readPos) %>% 
  write_csv(here("data/carb_sputter.csv"))

# All of the above won't work for early wheels since they aren't in snics_results! Ugh.
# set up file paths for wheels to read
dir <- "/mnt/shared"
wheels <- list("CFAMS/CFAMS Results/2010 Results/CFAMS010810R.XLS" = c(66:80), 
               "CFAMS/CFAMS Results/2010 Results/CFAMS073010R.XLS" = c(25:41), 
               "CFAMS/CFAMS Results/2010 Results/CFAMS122010R.XLS" = c(15:22), 
               "CFAMS/CFAMS Results/2011 Results/CFAMS012111R.XLS" = c(38:61), 
               "CFAMS/CFAMS Results/2011 Results/CFAMS050211R.XLS" = c(42:73),
               "USAMS/Results/USAMS042321R.txt" = c(39:58))

paths <- file.path(dir, names(wheels))  

# function to get lines for positions from results file
readPos <- function(file, pos) {
  read_tsv(file, skip = 4, comment = "=") %>% # readResfile doesn't work with these paths?
    filter(Pos %in% pos) %>% 
    mutate(wheel = str_extract(file, "\\w{5}\\d{6}")) %>% 
    rename(Run.Completion.Time = 'Run Completion Time',
           X13.12he = '13/12he',
           X14.12he = '14/12he',
           Sample.Name = 'Sample Name')
}


# read data for wheels and put into a dataframe
# map works like: for file in paths, readResfile(file)
data <- map2_dfr(paths, wheels, readPos)  %>% 
    mungeResfile()

# write data to a file
write_csv(data, here("data/old_raw_carb_sputter.csv"))

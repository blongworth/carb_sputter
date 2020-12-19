# get data for carbonate sputtering

library(amstools)
library(tidyverse)
library(here)

# build nested list of wheels and positions to get desired data
wheels <- list("CFAMS121020" = c(15, 84, 81, 97, 99, 100, 101))

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
dir <- "/mnt/shared/CFAMS/CFAMS Results"
wheels <- list("2010 Results/CFAMS010810" = c(66:80), 
               "2010 Results/CFAMS073010" = c(25:41), 
               "2010 Results/CFAMS122010" = c(15:22), 
               "2011 Results/CFAMS012111" = c(38:61), 
               "2011 Results/CFAMS050211" = c(42:73)) 
paths <- file.path(dir, paste0(names(wheels), "R.XLS"))  

# function to get lines for positions from results file
readPos <- function(file, pos) {
  read_tsv(file, skip = 4, comment = "=") %>% # readResfile doesn't work with these paths?
    filter(Pos %in% pos) %>% 
    mutate(wheel = str_extract(file, "CFAMS\\d{6}")) %>% 
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

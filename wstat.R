#!/usr/bin/r
# quick wheel status from cmd line



#load lobraries
suppressPackageStartupMessages(library(stats))
suppressPackageStartupMessages(library(dplyr))

#source functions
source("./wheelfuncs.R")

#directories
usamspath <- "/mnt/shared/USAMS/Results"

#load most recent file

#Get and order wheelnames by system
details <- file.info(list.files(path = usamspath, pattern = "*AMS*.*",
                                full.names=TRUE))
details <- details[with(details, order(as.POSIXct(mtime))), ]
wheels <- rownames(details)
wheel <- wheels[1]
data <- readCFWheel(wheel)

#last run
print(select(tail(data), ts, Pos, Meas, Sample.Name, he12C, cor1412he))

#current stats

sum  <- data %>% filter(Num == "S") %>% 
  select(X14.12he, cor1412he) %>% 
  summarise_each(funs(mean, sd, rsd)) 

print(sum)

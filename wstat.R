#!/usr/bin/r
# quick wheel status from cmd line


#load libraries
suppressPackageStartupMessages(library(stats))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(amstools))

#directories
usamspath <- "/mnt/shared/USAMS/Results"

#Get and order wheelnames by system
details <- file.info(list.files(path = usamspath, pattern = "*AMS*.*",
                                full.names=TRUE))
details <- details[with(details, order(as.POSIXct(mtime))), ]
wheels <- rownames(details)
wheel <- tail(wheels, 1)
data <- mungeResfile(readResfile(wheel))

#last run
cat("Last 5 Runs:\n")
print(select(tail(data), ts, Pos, Meas, Sample.Name, he12C, cor1412he))

#current stats

sum  <- data %>% filter(Num == "S") %>% 
  select(X14.12he, cor1412he) %>% 
  summarise_each(funs(mean, sd, rsd)) 

cat("\nStandards Summary:\n")
print(sum)


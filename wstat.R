#!/usr/bin/r
# quick wheel status from cmd line


#load libraries
suppressPackageStartupMessages(library(stats))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(amstools))
suppressPackageStartupMessages(library(lubridate))

options(digits = 4)

#directories
usamspath <- "/mnt/shared/USAMS/Results"

#Get and order wheelnames by system
details <- file.info(list.files(path = usamspath, pattern = "*AMS*.*",
                                full.names=TRUE))
details <- details[with(details, order(as.POSIXct(mtime))), ]
wheels <- rownames(details)
wheel <- tail(wheels, 1)
z <- mungeResfile(readResfile(wheel))


# Runs completed
runsdone <- nrow(z)
#last run

lastrun <- z[runsdone,]
lt <- lastrun$Run.Completion.Time
lp <- lastrun$Pos
lr <- paste("Last run was position", lp, "at", lt)

# load wheelfile for time calculation
wheelfile <- paste("/mnt/shared/USAMS/Wheels", 
                   gsub('R', '', basename(wheel)), sep = "/")
wheel <- read.delim(wheelfile)

# Calculate total runs in wheel
runs <- sum(wheel$Repeats)

# time per run so far
wheeltime <- difftime(lastrun$ts[1], z$ts[1], units = 'secs')
runtime <- wheeltime / runsdone
r <- runs - runsdone  # runs remaining
t <- r * runtime #seconds remaining
h <- seconds_to_period(t)
h$second <- round(h$second)


if (r <= 0) {
  rl <- paste("Run finished:", lt)
  re <- ""
} else {
  rl <- paste(r, "runs to go, which will take about", h)
  re <- paste("The run should end around", Sys.time() + t)
}

cat(rl)
cat("\n")
cat(re)
cat("\n\n")
#last run
cat("Last 5 Runs:\n")
print(select(tail(z), ts, Pos, Meas, Name = Sample.Name, he12C, cor1412he))

#current stats

sum  <- z %>% filter(Num == "S",
                     grepl("OX-I", Sample.Name),
                     cor1412he > 9) %>% 
  select(raw1412 = X14.12he, cor1412 = cor1412he) %>% 
  summarise_each(funs(mean, sd, rsd)) 

cat("\nStandards Summary:\n")
print(sum)

Meas <- z$Meas
lag.Meas <- c(tail(Meas, -1), NA)
z <- z[-(1:which(Meas != lag.Meas & Meas != lag.Meas -1)),]

sum  <- z %>% filter(Num == "S",
                     grepl("OX-I", Sample.Name),
                     cor1412he > 9) %>% 
  select(raw1412 = X14.12he, cor1412 = cor1412he) %>% 
  summarise_each(funs(mean, sd, rsd)) 

cat("\nSummary, last group only:\n")
print(sum)

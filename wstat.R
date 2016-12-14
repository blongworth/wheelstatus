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
z <- mungeResfile(readResfile(wheel))

# wheel finish time

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
      
      r <- runs - runsdone  # runs remaining
      t <- r * 190 #seconds remaining
      h <- seconds_to_period(t)
      
      if (r <= 0) {
        rl <- paste("Run finished:", lt)
        re <- ""
      } else {
        rl <- paste(r, "runs to go, which will take about", h)
        re <- paste("The run should end around", Sys.time() + t)
      }
      cat(rl)
      cat(re)
      cat("\n\n")
#last run
cat("Last 5 Runs:\n")
print(select(tail(z), ts, Pos, Meas, Sample.Name, he12C, cor1412he))

#current stats

sum  <- z %>% filter(Num == "S",
		       	grepl("OX-I", Sample.Name),
		       	cor1412he > 9) %>% 
  select(X14.12he, cor1412he) %>% 
  summarise_each(funs(mean, sd, rsd)) 

cat("\nStandards Summary:\n")
print(sum)


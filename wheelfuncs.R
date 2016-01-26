# functions for loading and formatting snics wheels

#Massage wheel data
mungeCFWheel = function (z) {
  z %>% mutate(ts = as.POSIXct(strptime(
                  Run.Completion.Time, format = "%a %b %d %H:%M:%S %y")), 
               Pos = as.factor(Pos),
               ce = 1/sqrt(CntTotGT), #Add counting error
               cor1412he = X14.12he/X13.12he^2 * 1E9, #Add corrected 14/12
               X14.12he = X14.12he * 1E12, #Convert ratio to 1E12
               he12C = he12C * 1E6, #Convert current to uA
               le12C = le12C * 1E6)
}

#Function to read a single file into a data frame
readCFWheel = function (file) {
  z <- read.delim(file, skip = 4, comment.char = "=")
  mungeCFWheel(z)
}

#relative sd
rsd <- function(x) {sd(x)/mean(x)}


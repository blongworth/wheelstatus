# NOSAMS Wheel Status Web app

# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)
#library(plotly)
library(dplyr)
library(RColorBrewer)
library(lubridate)

usamspath = "/mnt/shared/USAMS/Results"
cfamspath = "/mnt/shared/CFAMS/CFAMS Results"

##Define functions

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

#Function for formatting table
format_num <- function(col) {
  if (is.numeric(col))
    sprintf('%1.4f', col)
  else
    col
}


#Main reactive shiny server logic
shinyServer(function(input, output, clientData, session) {
        
  observe({
    
    #Get and order wheelnames by system
    details <- file.info(list.files(path = input$system, pattern = "*AMS*.*", full.names=TRUE))
    details <- details[with(details, order(as.POSIXct(mtime))), ]
    wheels <- basename(rownames(details))
    
    # Change values for input$wheelSelect
    updateSelectInput(session, "wheelSelect",
                      choices = wheels,
                      selected = tail(wheels, n = 1))
    
  })
  
  wheelData <- reactive({
    
    wheelfile <- paste(input$system, input$wheelSelect, sep = "/")
    
    #Check that we've selected a valid file
    validate(need(file.exists(wheelfile), message=FALSE))
    
    #Code to reload wheel when file changes
    file <- reactiveFileReader(5000, session, wheelfile, read.delim, skip = 4, comment.char = "=")
    z <- file()
    mungeCFWheel(z)
    
  })

  subData <- reactive({
    
    z <- wheelData()
    
    #Subset based on input
    if (input$type == 1) {
      filter(z, Num == "S")
    } else if (input$type == 2) {
      filter(z, Num == "B")
    } else {
      z
    }
    
  })  
  
  colScale <- reactive({
    
    z <- wheelData()
    
    #Create a custom color scale
    myColors <- brewer.pal(length(levels(z$Num)),"Set1")
    names(myColors) <- levels(z$Num)
    scale_colour_manual(name = "Num",values = myColors)
    
  })
  
  #run statistics
  output$stdData <- renderUI({ 
    
    z <- wheelData()
    
    sum <- z %>% filter(Num == "S") %>% 
      select(X14.12he, cor1412he) %>% 
      summarise_each(funs(mean, sd, rsd)) 
    
    s <- sprintf("Mean of Standards is %.3f SD %.3f (RSD %.3f)", 
                 sum$X14.12he_mean, sum$X14.12he_sd, sum$X14.12he_rsd)
    c <- sprintf("Mean of 13C corrected standards is %.3f SD %.3f (RSD %.3f)", 
                 sum$cor1412he_mean, sum$cor1412he_sd, sum$cor1412he_rsd)
    
    #last run
    lt <- tail(z$Run.Completion.Time, n = 1)
    lp <- tail(z$Pos, n = 1)
    lr <- paste("Last run was position", lp, "at", lt)
    
    #skip time calculations for cfams
    if (input$system == "/mnt/shared/USAMS/Results") {
    
      #time remaining
      l <- tail(z, n = 1)
      #Need to calculate runs based on runlist for CFAMS
      r <- 401 - 40 * (l$Meas - 1) - as.numeric(l$Pos) #runs remaining
      t <- r * 180 #seconds remaining
      h <- seconds_to_period(t)
      
      if (r <= 0) {
        rl <- paste("Run finished:", lt)
        re <- ""
      } else {
        rl <- paste(r, "runs to go, which will take about", h)
        re <- paste("The run should end around", Sys.time() + t)
      }
      
    } else {
      rl <- ""
      re <- ""
    }
    
    HTML(paste(s, c, lr, rl, re, sep = '<br/>'))
    
  })
  
  output$ratPlot <- renderPlot({
    
    if (input$box == 1) {
      #try position_dodge to add points to boxplot
      ggplot(subData(), aes(factor(Pos), X14.12he, color = Num)) + 
        geom_boxplot() + 
        colScale() + ggtitle("Ratio Boxplot") +
        ylab(expression(paste("Raw 14/12C (x", 10^{-12},")"))) +
        theme(axis.title.x = element_blank()) + 
        #theme(legend.position="none") +
        theme(axis.title.y = element_text(size=16), 
              axis.text.y  = element_text(size=12)) 
    } else {
      if (input$type == 1) {
        ggplot(subData(), aes(ts, X14.12he, color=as.factor(Pos))) + 
        geom_point(size=3.5) + 
        ggtitle("14/12 Ratio") +
        ylab(expression(paste("Raw 14/12C (x", 10^{-12},")"))) +
        theme(axis.title.x = element_blank()) + #theme(legend.position="none") +
        theme(axis.title.y = element_text(size=16), 
              axis.text.y  = element_text(size=12))
      } else {
        ggplot(subData(), aes(ts, X14.12he, color = Num)) + 
          geom_point(size=3.5) + 
          colScale() + ggtitle("14/12 Ratio") +
          ylab(expression(paste("Raw 14/12C (x", 10^{-12},")"))) +
          theme(axis.title.x = element_blank()) + #theme(legend.position="none") +
          theme(axis.title.y = element_text(size=16), 
                axis.text.y  = element_text(size=12))
      }  
    }  
    
  })
  
  output$rat13Plot <- renderPlot({
    
    #14/12 corrected by 13/12
    
    if (input$box == 1) {
      #try position_dodge to add points to boxplot
      ggplot(subData(), aes(Pos, cor1412he, color = Num)) + geom_boxplot() + 
        colScale() + ggtitle("13/12C corrected 14/12 Ratio") +
        ylab(expression(paste("14/12C (x", 10^{-12},")"))) +
        theme(axis.title.x = element_blank()) + #theme(legend.position="none") +
        theme(axis.title.y = element_text(size=16), axis.text.y  = element_text(size=12)) 
    } else {
      if (input$type == 1) {
        ggplot(subData(), aes(ts, cor1412he, color = as.factor(Pos))) + geom_point(size=3.5) + 
          ggtitle("13/12C corrected 14/12 Ratio") +
          ylab(expression(paste("14/12C (x", 10^{-12},")"))) +
          theme(axis.title.x = element_blank()) + #theme(legend.position="none") +
          theme(axis.title.y = element_text(size=16), axis.text.y  = element_text(size=12))
      } else {
        ggplot(subData(), aes(ts, cor1412he, color = Num)) + geom_point(size=3.5) + 
          colScale() + ggtitle("13/12C corrected 14/12 Ratio") +
          ylab(expression(paste("14/12C (x", 10^{-12},")"))) +
          theme(axis.title.x = element_blank()) + #theme(legend.position="none") +
          theme(axis.title.y = element_text(size=16), axis.text.y  = element_text(size=12))
      }
    }  
    
  })
  
  output$curPlot <- renderPlot({
      
    if (input$box == 1) {
      #try position_dodge to add points to boxplot
      ggplot(subData(), aes(factor(Pos), he12C, color = Num)) + geom_boxplot() + 
        colScale() + ggtitle("12C Boxplot") +
        ylab(expression(paste("He 12C (", mu,"A)"))) +
        theme(axis.title.x = element_blank()) + #theme(legend.position="none") +
        theme(axis.title.y = element_text(size=16), axis.text.y  = element_text(size=12))
      
    } else {
      ggplot(subData(), aes(ts, he12C, color = Num)) + geom_point(size=3.5) + 
        colScale() + ggtitle("12C Currents") +
        ylab(expression(paste("He 12C (", mu,"A)"))) +
        theme(axis.title.x = element_blank()) + #theme(legend.position="none") +
        theme(axis.title.y = element_text(size=16), axis.text.y  = element_text(size=12))
    }  
    
  })
  
  output$curratPlot <- renderPlot({
    
      ggplot(subData(), aes(he12C, cor1412he, color = Num)) + geom_point(size=3.5) + 
        colScale() + ggtitle("Ratio vs Current") +
        xlab(expression(paste("He 12C (", mu,"A)"))) +
        ylab(expression(paste("13C corrected 14/12C (x", 10^{-12},")"))) +
        theme(axis.title.x = element_text(size=16), axis.text.x  = element_text(size=12)) +
        theme(axis.title.y = element_text(size=16), axis.text.y  = element_text(size=12))
     
    
  })
  
  tableData <- reactive({

    subData() %>% mutate(Timestamp = strftime(ts,"%b-%d %H:%M:%S"),
                         X13.12he = X13.12he*100,
                         `LE12C (uA)` = format_num(le12C),
                         `HE12C (uA)` = format_num(he12C),
                         `HE13/12C (x10E-2)` = format_num(X13.12he),
                         `HE14/12C (x10E-12)` = format_num(X14.12he)) %>%
                  select(Timestamp, Position = Pos, Measurement = Meas,
                         Name = Sample.Name, `LE12C (uA)`,
                         `HE12C (uA)`, `HE13/12C (x10E-2)`,
                         `HE14/12C (x10E-12)`) 
      
  })
  
  
  output$table <- renderDataTable(tableData())
    
  
})


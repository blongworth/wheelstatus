
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
#system = 1

##Define functions

#Function to read a single file into a data frame

readCFWheel = function (file) {
  z <- read.delim(file, skip = 4, comment.char = "=")
  #Fix timestamp
  z$ts = as.POSIXct(strptime(z$Run.Completion.Time, format = "%a %b %d %H:%M:%S %y"))
  #Add counting error
  z$ce = 1/sqrt(z$CntTotGT)
  #Add corrected 14/12
  z$cor1412he <- z$X14.12he/z$X13.12he^2 * 1E9
  
  #Convert ratio to 1E12
  z$X14.12he = z$X14.12he * 1E12
  #Convert current to uA
  z$he12C = z$he12C * 1E6
  z$le12C = z$le12C * 1E6
  z
}

mungeCFWheel = function (z) {
  
  #Fix timestamp
  z$ts = as.POSIXct(strptime(z$Run.Completion.Time, format = "%a %b %d %H:%M:%S %y"))
  #Add counting error
  z$ce = 1/sqrt(z$CntTotGT)
  #Add corrected 14/12
  z$cor1412he <- z$X14.12he/z$X13.12he^2 * 1E9
  
  #Convert ratio to 1E12
  z$X14.12he = z$X14.12he * 1E12
  #Convert current to uA
  z$he12C = z$he12C * 1E6
  z$le12C = z$le12C * 1E6
  z
}

#Function for formatting table
format_num <- function(col) {
  if (is.numeric(col))
    sprintf('%1.4f', col)
  else
    col
}

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
      z[z$Num == "S",]
    } else if (input$type == 2) {
      z[z$Num == "B",]
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
    
    #raw ratio
    s.m <- mean(z[z$Num == "S",15]) * 10E-13
    s.s <- sd(z[z$Num == "S",15]) * 10E-13
    s.rs <- s.s/s.m
    s <- sprintf("Mean of Standards is %.3e SD %.3e (RSD %.4f)", s.m, s.s, s.rs)
    
    #13C norm ratio
    c.m <- mean(z[z$Num == "S",18])
    c.s <- sd(z[z$Num == "S",18])
    c.rs <- c.s/c.m
    c <- sprintf("Mean of 13C corrected Standards is %.3e SD %.3e (RSD %.4f)", c.m, c.s, c.rs)
    
    #last run
    lt <- tail(z$Run.Completion.Time, n = 1)
    lr <- paste("Last run was ", lt)
    
    #skip time calculations for cfams
    if (input$system == "/mnt/shared/USAMS/Results") {
    
      #time remaining
      l <- tail(z, n = 1)
      #Need to calculate runs based on runlist for CFAMS
      r <- 401 - 40 * (l$Meas - 1) - l$Pos #runs remaining
      t <- r * 180 #seconds remaining
      h <- seconds_to_period(t)
      
      if (r <= 0) {
        rl <- "Run is finished"
        re <- ""
      } else {
        rl <- paste(r, "runs to go, which will take about", h)
        re <- paste("The run should end around", Sys.time() + t)
      }
      
    } else {
      rl <- "Run is finished"
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
      ggplot(subData(), aes(ts, X14.12he, color = Num)) + 
        geom_point(size=3.5) + 
        colScale() + ggtitle("14/12 Ratio") +
        ylab(expression(paste("Raw 14/12C (x", 10^{-12},")"))) +
        theme(axis.title.x = element_blank()) + #theme(legend.position="none") +
        theme(axis.title.y = element_text(size=16), 
              axis.text.y  = element_text(size=12))
      #qplot(ts, X14.12he, color=as.factor(Pos), size = 4, data=z)
    }  
    
  })
  
  output$rat13Plot <- renderPlot({
    
    #14/12 corrected by 13/12
    
    if (input$box == 1) {
      #try position_dodge to add points to boxplot
      ggplot(subData(), aes(factor(Pos), cor1412he, color = Num)) + geom_boxplot() + 
        colScale() + ggtitle("13/12C corrected 14/12 Ratio") +
        ylab(expression(paste("14/12C (x", 10^{-12},")"))) +
        theme(axis.title.x = element_blank()) + #theme(legend.position="none") +
        theme(axis.title.y = element_text(size=16), axis.text.y  = element_text(size=12)) 
      
      
    } else {
      ggplot(subData(), aes(ts, cor1412he, color = Num)) + geom_point(size=3.5) + 
        colScale() + ggtitle("13/12C corrected 14/12 Ratio") +
        ylab(expression(paste("14/12C (x", 10^{-12},")"))) +
        theme(axis.title.x = element_blank()) + #theme(legend.position="none") +
        theme(axis.title.y = element_text(size=16), axis.text.y  = element_text(size=12))
      #qplot(ts, X14.12he, color=as.factor(Pos), size = 4, data=z)
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
    z <- subData()
    
    #Format for table output
    z$Timestamp <- strftime(z$ts,"%b-%d %H:%M:%S")
    z0 <- select(z, Timestamp, Pos, Meas, Sample.Name)
    
    # Apply the function to each column, and convert the list output back to a data frame
    z1 <- z %>% 
      select(le12C, he12C, X13.12he, X14.12he) %>% 
      mutate(X13.12he = X13.12he*100) %>%
      mutate_each(funs(format_num)) 
    
    t <- bind_cols(z0,z1)
    colnames(t) <- c("Timestamp", "Position", "Measurement", "Name",
                     "LE 12C (uA)", "HE 12C (uA)", "HE 13/12C (x10E-2)",
                     "HE 14/12C (x10E-12)") 
    
    return(t)
    
  })
  
  
  output$table <- renderDataTable(tableData())
    

  
})


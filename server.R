
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)


usamspath = "H:/USAMS/Results"
cfamspath = "H:/CFAMS/CFAMS Results"
#system = 1

#Define functions

#Function to read a single file into a data frame

readCFWheel = function (file) {
  z <- read.delim(file, skip = 4, comment.char = "=")
  #Fix timestamp
  z$ts = as.POSIXct(strptime(z$Run.Completion.Time, format = "%a %b %d %H:%M:%S %y"))
  #Add counting error
  z$ce = 1/sqrt(z$CntTotGT)
  z
}

shinyServer(function(input, output, clientData, session) {
  
  observe({
    
    #get wheels based on system
    if (input$system == 1) {
      wheelpath = usamspath
    } else if (input$system == 2) {
      wheelpath = cfamspath
    } else {
      #Stop
    }
    
    wheels <- list.files(path = wheelpath, pattern = "*AMS*.*")
    
    # Change values for input$wheelSelect
    updateSelectInput(session, "wheelSelect",
                      choices = wheels,
                      selected = tail(wheels, n = 1))
  
  })

  wheelData <- reactive({
    #get wheels based on system
    if (input$system == 1) {
      wheelpath = usamspath
    } else if (input$system == 2) {
      wheelpath = cfamspath
    } else {
      #Stop
    }
    
    input$reload
    
    file <- paste(wheelpath, input$wheelSelect, sep = "/")
    readCFWheel(file)
          
  })
  
  output$stdMean <- renderText({ 
    
    z <- wheelData()
    m <- mean(z[z$Num == "S",15])
    s <- sd(z[z$Num == "S",15])
    sprintf("Mean of Standards is %.3e SD %.3e", m, s)
    
  })
  
  output$lastRun <- renderText({ 
    
    z <- wheelData()
    l <- tail(z$Run.Completion.Time, n = 1)
    
    paste("Last run was ", l)
    
  })
  
  output$endTime <- renderText({ 
    
    z <- wheelData()
    l <- tail(z, n = 1)
    r <- 401 - 40 * (l$Meas - 1) - l$Pos
    t <- r * 180
    h <- t / 3600
    
    sprintf("%d runs to go, which will take about %.1f hours", r, h)
    
  })
  
  output$ratPlot <- renderPlot({
    
    z <- wheelData()
    if (input$type == 1) {
      z <- z[z$Num == "S",]
    } else if (input$type == 2) {
      z <- z[z$Num == "B",]
    }
    
    if (input$box == 1) {
      #try position_dodge to add points to boxplot
      ggplot(z, aes(factor(Pos), X14.12he, color = Num)) + geom_boxplot()
  
    } else {
      ggplot(z, aes(ts, X14.12he, color = Num)) + geom_point(size=4)
      #qplot(ts, X14.12he, color=as.factor(Pos), size = 4, data=z)
    }  
  
  })
  
 
  output$curPlot <- renderPlot({
    
    z <- wheelData()
    if (input$type == 1) {
      z <- z[z$Num == "S",]
    } else if (input$type == 2) {
      z <- z[z$Num == "B",]
    }
    
    if (input$box == 1) {
      #try position_dodge to add points to boxplot
      ggplot(z, aes(factor(Pos), he12C, color = Num)) + geom_boxplot()
      
    } else {
      ggplot(z, aes(ts, he12C, color = Num)) + geom_point(size=4)
    }  
    
  })
  
 
})


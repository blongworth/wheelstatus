
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)
#library(plotly)
library(dplyr)


usamspath = "/mnt/shared/USAMS/Results"
cfamspath = "/mnt/shared/CFAMS/CFAMS Results"
#system = 1

#Define functions

#Function to read a single file into a data frame

readCFWheel = function (file) {
  z <- read.delim(file, skip = 4, comment.char = "=")
  #Fix timestamp
  z$ts = as.POSIXct(strptime(z$Run.Completion.Time, format = "%a %b %d %H:%M:%S %y"))
  #Add counting error
  z$ce = 1/sqrt(z$CntTotGT)
  #Convert ratio to 1E12
  z$X14.12he = z$X14.12he * 1E12
  #Convert current to uA
  z$he12C = z$he12C * 1E6
  z$le12C = z$le12C * 1E6
  z
}

format_num <- function(col) {
  if (is.numeric(col))
    sprintf('%1.4f', col)
  else
    col
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
    m <- mean(z[z$Num == "S",15]) * 10E-13
    s <- sd(z[z$Num == "S",15]) * 10E-13
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
      ggplot(z, aes(factor(Pos), X14.12he, color = Num)) + geom_boxplot() + geom_boxplot() + 
        ylab(expression(paste("Raw 14/12C (x", 10^{-12},")"))) +
        theme(axis.title.x = element_blank()) + theme(legend.position="none") +
        theme(axis.title.y = element_text(size=16), axis.text.y  = element_text(size=12))
  
    } else {
      ggplot(z, aes(ts, X14.12he, color = Num)) + geom_point(size=3.5) + 
        ylab(expression(paste("Raw 14/12C (x", 10^{-12},")"))) +
        theme(axis.title.x = element_blank()) + theme(legend.position="none") +
        theme(axis.title.y = element_text(size=16), axis.text.y  = element_text(size=12))
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
      ggplot(z, aes(factor(Pos), he12C, color = Num)) + geom_boxplot() + 
        ylab(expression(paste("He 12C (", mu,"A)"))) +
        theme(axis.title.x = element_blank()) + theme(legend.position="none") +
        theme(axis.title.y = element_text(size=16), axis.text.y  = element_text(size=12))
      
    } else {
      ggplot(z, aes(ts, he12C, color = Num)) + geom_point(size=3.5) + 
        ylab(expression(paste("He 12C (", mu,"A)"))) +
        theme(axis.title.x = element_blank()) + theme(legend.position="none") +
        theme(axis.title.y = element_text(size=16), axis.text.y  = element_text(size=12))
    }  
    
  })
  
  # Filter data based on selections
  output$table <- renderDataTable({
    
    z <- wheelData()
    if (input$type == 1) {
      z <- z[z$Num == "S",]
    } else if (input$type == 2) {
      z <- z[z$Num == "B",]
    }
    
    z$Timestamp <- strftime(z$ts,"%b-%d %H:%M:%S")
    z0 <- select(z, Timestamp, Pos, Meas, Sample.Name)
    # Apply the function to each column, and convert the list output back to a data frame
    z1 <- z %>% select(le12C, he12C, X13.12he, X14.12he) %>% mutate_each(funs(format_num))
    z <- bind_cols(z0,z1)
    z
    }#, options = list(LengthMenu = c(25, 40, 401), pageLength = 40, orderClasses = TRUE,
    #              autoWidth = TRUE
                  #columns = list(list(width = "30px", width = "15px",
                  #                     width = "15px", width = "30px"))
  #)
  )
})


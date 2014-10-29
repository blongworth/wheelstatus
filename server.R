
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)
library(plotly)

usamspath = "H:/USAMS/Results"
cfamspath = "H:/CFAMS/Results"
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


#Get available wheels

getWheels <- function(system) {
  
  if (system == 1) {
    wheelpath = usamspath
  } else if (system == 2) {
    wheelpath = cfamspath
  } else {
    #Stop
  }
  
  list.files(path = wheelpath, pattern = "*AMS*.*")
  
}

shinyServer(function(input, output, clientData, session) {
  
  observe({
    
    #get wheels based on system
    wheels <- getWheels(input$system)
    
    # Change values for input$wheelSelect
    updateSelectInput(session, "wheelSelect",
                      choices = wheels)
  })

  output$ratPlot <- renderPlot({
    file <- paste(wheelpath, input$wheelSelect, sep = "/")
    z <- readCFWheel(file)
    
    if (input$type == 1) {
      z <- z[z$Num == "S",]
    } else if (input$type == 2) {
      z <- z[z$Num == "B",]
    }
    
    qplot(ts, X14.12he, color=as.factor(Pos), data=z)
  })
  
  output$curPlot <- renderPlot({
    file <- paste(wheelpath, input$wheelSelect, sep = "/")
    z <- readCFWheel(file)
    if (input$type == 1) {
      z <- z[z$Num == "S",]
    } else if (input$type == 2) {
      z <- z[z$Num == "B",]
    }
    
    qplot(ts, he12C, color=as.factor(Pos), data=z)
  })
  
 
})


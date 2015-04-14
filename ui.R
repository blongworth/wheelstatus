
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
usamspath = "/mnt/shared/USAMS/Results"
cfamspath = "/mnt/shared/CFAMS/CFAMS Results"


shinyUI(
  fluidPage(
  
    # Application title
    titlePanel("NOSAMS Wheel Status"),
  
    # Sidebar with a slider input for number of bins
    sidebarLayout(
      sidebarPanel(
        
        radioButtons("system", label = h3("System"),
                     choices = list("USAMS" = usamspath, "CFAMS" = cfamspath),
                     selected = usamspath),
        selectInput("wheelSelect",
                    label = h3("Wheel"),
                    c("label 1" = "option1")),
                    
        radioButtons("type", label = h3("Sample type"),
                     choices = list("Standards" = 1, "Blanks" = 2, "All samples" = 3),
                     selected = 1),
        
        checkboxInput("box", label = "Boxplot?", value = FALSE),
        
        actionButton("reload", "Refresh Data")
        
      ),
        
  
      # Show a plot of the generated distribution
      mainPanel(
                
        textOutput("stdMean"),
        textOutput("stdnMean"),
        textOutput("lastRun"),
        textOutput("endTime"),
        plotOutput("ratPlot"),
        plotOutput("rat13Plot"),
        plotOutput("curPlot"),
        plotOutput("curratPlot")
      )
    ),
    
    fluidRow(
      dataTableOutput(outputId="table")
    )
  
  )
  
)


# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)


shinyUI(fluidPage(

  # Application title
  titlePanel("NOSAMS Wheel Status"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      
      radioButtons("system", label = h3("System"),
                   choices = list("USAMS" = 1, "CFAMS" = 2),
                   selected = 1),
      selectInput("wheelSelect",
                  label = h3("Wheel"),
                  c("label 1" = "option1",
                    "label 2" = "option2")),
                  
      radioButtons("type", label = h3("Sample type"),
                   choices = list("Standards" = 1, "Blanks" = 2, "All samples" = 3),
                   selected = 1),
      
      checkboxInput("box", label = "Boxplot?", value = FALSE),
      
      actionButton("reload", "Refresh Data")
      
    ),
    
#     sidebarPanel(
#       textOutput("stdMean")
#     ),
      
      

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("ratPlot"),
      
      plotOutput("curPlot"),
      
      textOutput("stdMean"),
      
      textOutput("lastRun"),
      
      textOutput("endTime")
    )
  )
))

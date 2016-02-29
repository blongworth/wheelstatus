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

    # Sidebar with inputs for selecting wheel and display style
    sidebarLayout(
      sidebarPanel(
        radioButtons("system", label = h3("System"),
                     choices = list("USAMS" = usamspath, "CFAMS" = cfamspath),
                     selected = usamspath),
        uiOutput("wheelNames"),
        #selectInput("wheelSelect",
        #            label = h3("Wheel"),
        #            c("label 1" = "option1")),
        radioButtons("type", label = h3("Sample type"),
                     choices = list("Standards" = 1, "Blanks" = 2, "All samples" = 3),
                     selected = 1),
        checkboxInput("box", label = "Boxplot?", value = FALSE)
      ),

      # Stats and Plots
      mainPanel(
        htmlOutput("stdData"),
        plotOutput("ratPlot"),
        plotOutput("rat13Plot"),
        plotOutput("curPlot"),
        plotOutput("curratPlot")
      )
    ),

    fluidRow(
      #Data table
      dataTableOutput(outputId="table")
    )
  )
)

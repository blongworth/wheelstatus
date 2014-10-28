
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

#Get available wheels

usamspath = "H:/USAMS/Results"
cfamspath = "H:/CFAMS/Results"
system = 1

if (system == 1) {
  wheelpath = usamspath
} else if (system == 2) {
  wheelpath = cfamspath
} else {
  #Stop
}

wheels = list.files(path = wheelpath, pattern = "*AMS*.*")

shinyUI(fluidPage(

  # Application title
  titlePanel("NOSAMS Wheel Status"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      
      selectInput("wheel",
                  label = h3("Wheel"),
                  choices = wheels
                  ),
      radioButtons("type", label = h3("Sample type"),
                   choices = list("Standards" = 1, "Blanks" = 2, "All samples" = 3),
                   selected = 1)
      
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("ratPlot"),
      
      plotOutput("curPlot"),
      
      htmlOutput("plotly") 
    )
  )
))

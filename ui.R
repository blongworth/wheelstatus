# Wheelstatus shiny webapp UI
#
# TODO: Fix group choice

library(shiny)
library(DT, warn.conflicts = FALSE)

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
        selectInput("wheelSelect",
                    label = h3("Wheel"),""),
        radioButtons("type", label = h3("Sample type"),
                     choices = list("Standards" = 1, "Blanks" = 2, "All samples" = 3),
                     selected = 1),
        checkboxInput("box", label = "Boxplot?", value = FALSE),
        checkboxInput("oxi", label = "Use only OX-I primaries?", value = FALSE),
        checkboxInput("group", label = "Last group only?", value = FALSE)
      ),
      

      # Stats and Plots
      mainPanel(
        htmlOutput("stdData"),
        plotOutput("Plot", height = "1000px")
      )
    ),

    fluidRow(
      #Data table
      dataTableOutput(outputId="table")
    )
  )
)

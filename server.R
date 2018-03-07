# NOSAMS Wheel Status Web app

# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(readxl)
library(shiny)
library(ggplot2)
library(dplyr, warn.conflicts = FALSE)
library(RColorBrewer)
library(lubridate, warn.conflicts = FALSE)
library(amstools, warn.conflicts = FALSE)
library(gridExtra, warn.conflicts = FALSE)
usamspath = "/mnt/shared/USAMS/Results"
cfamspath = "/mnt/shared/CFAMS/CFAMS Results"

##Define functions

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
    
    # If no file is selected, don't do anything
    validate(need(input$wheelSelect, message = FALSE))
    
    wheelfile <- paste(input$system, input$wheelSelect, sep = "/")
    
    #Check that we've selected a valid file
    validate(need(file.exists(wheelfile), message=FALSE))
    
    #Code to reload wheel when file changes
    file <- reactiveFileReader(5000, session, wheelfile, read.delim, skip = 4, comment.char = "=")
    z <- mungeResfile(file())
    
    # Last group only?
    if (input$group) {
	    Meas <- z$Meas
	    lag.Meas <- c(tail(Meas, -1), NA)
	    z[-(1:which(Meas != lag.Meas & Meas != lag.Meas -1)),]
    } else {
	    z
    }
  })

  subData <- reactive({
    
    z <- wheelData()
    
    #Subset based on input
    if (input$type == 1) {
      if (input$oxi) {
        z <- filter(z, grepl("OX-I[^I]", Sample.Name))
      }
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
    
    sum <- z %>% filter(Num == "S",
                        X14.12he > .98)
    if (input$oxi) {
      sum <- filter(sum, grepl("OX-I[^I]", Sample.Name))
    }
    sum <- sum %>%  
      select(X14.12he, cor1412he) %>% 
      summarise_all(funs(mean, sd, rsd)) 
    
    
    s <- sprintf("Mean of Standards is %.3f SD %.3f (RSD %.3f)", 
                 sum$X14.12he_mean, sum$X14.12he_sd, sum$X14.12he_rsd)
    c <- sprintf("Mean of 13C corrected standards is %.3f SD %.3f (RSD %.3f)", 
                 sum$cor1412he_mean, sum$cor1412he_sd, sum$cor1412he_rsd)
    
    # Runs completed
    runsdone <- nrow(z)
    
    #last run
    lastrun <- z[runsdone,]
    lt <- lastrun$Run.Completion.Time
    lp <- lastrun$Pos
    lr <- paste("Last run was position", lp, "at", lt)
    
    #still running?
    lasttime <- difftime(Sys.time(),lastrun$ts, units = c("secs"))
    if( lasttime < 240) {
      status <- paste('<h3 style="color:green">Run active</h3>')
    } else {
      status <- paste('<h3 style="color:red">Run stopped</h3>')
    }
    
    # load wheelfiles
    if (input$system == "/mnt/shared/USAMS/Results") {
      # load wheelfile for time calculation
      wheelfile <- paste("/mnt/shared/USAMS/Wheels", 
                         gsub('R.*\\.txt', '.txt', input$wheelSelect), sep = "/")
      wheel <- read.delim(wheelfile)
    } else {
      # load wheelfile for time calculation
      wheelfile <- paste("/mnt/shared/CFAMS/CFAMS Wheels", 
                         gsub('R.*\\.XLS', '.xlsx', input$wheelSelect), sep = "/")
      wheel <- read_excel(wheelfile)
    }
      
    # Calculate total runs in wheel
    runs <- sum(wheel$Repeats)
    
    # time per run so far
    wheeltime <- difftime(lastrun$ts[1], z$ts[1], units = 'secs')
    runtime <- wheeltime / runsdone
    r <- runs - runsdone  # runs remaining
    t <- r * runtime #seconds remaining
    h <- seconds_to_period(t)
    h$second <- round(h$second)
    
    if (r <= 0) {
      rl <- paste("Run finished:", lt)
      re <- ""
    } else {
      rl <- paste(r, "runs to go, which will take about", h)
      re <- paste("The run should end around", Sys.time() + t)
    }
    
    # Output as html
    HTML(paste(status, s, c, lr, rl, re, sep = '<br/>'))
    
  })
  
  output$Plot <- renderPlot({

    # If no file is selected, don't do anything
    validate(need(nrow(subData()) > 1, ''))

    # Get data and add error bits
        data <- subData()

    if (input$box == 1) {
      #try position_dodge to add points to boxplot
      g1 <- ggplot(data, aes(factor(Pos), X14.12he, color = Num)) + 
        geom_boxplot() + 
        colScale() + ggtitle("Ratio Boxplot") +
        ylab(expression(paste("Raw 14/12C (x", 10^{-12},")"))) +
        theme(axis.title.x = element_blank()) + 
        theme(axis.title.y = element_text(size=16), 
              axis.text.y  = element_text(size=12)) 
      
      g2 <- ggplot(data, aes(Pos, cor1412he, color = Num)) + geom_boxplot() + 
        colScale() + ggtitle("13/12C corrected 14/12 Ratio") +
        ylab(expression(paste("14/12C (x", 10^{-12},")"))) +
        theme(axis.title.x = element_blank()) + #theme(legend.position="none") +
        theme(axis.title.y = element_text(size=16), axis.text.y  = element_text(size=12)) 
      
      g3 <- ggplot(data, aes(factor(Pos), he12C, color = Num)) + geom_boxplot() + 
        colScale() + ggtitle("12C Boxplot") +
        ylab(expression(paste("He 12C (", mu,"A)"))) +
        theme(axis.title.x = element_blank()) + #theme(legend.position="none") +
        theme(axis.title.y = element_text(size=16), axis.text.y  = element_text(size=12))
      plots <- list(g1, g2, g3)
    } else {
      
      if (input$type == 1) {

        #add errors
        m14 <- mean(data$X14.12he)
        s14 <- sd(data$X14.12he)
        pce14 <- data$ce*data$X14.12he
        m1413 <- mean(data$cor1412he)
        s1413 <- sd(data$cor1412he)
        pce1413 <- data$ce*data$cor1412he
        
        g1 <- ggplot(data, aes(ts, X14.12he, color=Pos)) + 
          geom_hline(yintercept = m14) +
          geom_hline(yintercept = m14 + s14, color = "grey") +
          geom_hline(yintercept = m14 - s14, color = "grey") +
          # TODO: use geom_area?
          geom_linerange(aes(ymin=X14.12he - pce14, ymax = X14.12he + pce14)) +
          geom_point(size=3) + 
          ggtitle("14/12 Ratio") +
          ylab(expression(paste("Raw 14/12C (x", 10^{-12},")"))) +
          theme(axis.title.x = element_blank()) + 
          theme(axis.title.y = element_text(size=16), 
                axis.text.y  = element_text(size=12))
        
        g2 <- ggplot(data, aes(ts, cor1412he, color=Pos)) + 
          geom_hline(yintercept = m1413) +
          geom_hline(yintercept = m1413 + s1413, color = "grey") +
          geom_hline(yintercept = m1413 - s1413, color = "grey") +
          # TODO: use geom_area?
          geom_linerange(aes(ymin=cor1412he - pce1413, ymax = cor1412he + pce1413 )) +
          geom_point(size=3) + 
          ggtitle("13/12C corrected 14/12 Ratio") +
          ylab(expression(paste("14/12C (x", 10^{-12},")"))) +
          theme(axis.title.x = element_blank()) + #theme(legend.position="none") +
          theme(axis.title.y = element_text(size=16), axis.text.y  = element_text(size=12))
        
        g3 <- ggplot(data, aes(ts, he12C, color = Pos)) + geom_point(size=3) + 
          ggtitle("12C Currents") +
          ylab(expression(paste("He 12C (", mu,"A)"))) +
          theme(axis.title.x = element_blank()) + #theme(legend.position="none") +
          theme(axis.title.y = element_text(size=16), axis.text.y  = element_text(size=12))
        
        g4 <- ggplot(data, aes(he12C, cor1412he, color = Num)) + geom_point(size=3) + 
          colScale() + ggtitle("Ratio vs Current") +
          xlab(expression(paste("He 12C (", mu,"A)"))) +
          ylab(expression(paste("13C corrected 14/12C (x", 10^{-12},")"))) +
          theme(axis.title.x = element_text(size=16), axis.text.x  = element_text(size=12)) +
          theme(axis.title.y = element_text(size=16), axis.text.y  = element_text(size=12))
       plots <- list(g1, g2, g3, g4) 
      } else {

        g1 <- ggplot(data, aes(ts, X14.12he, color = Num)) + 
          geom_point(size=3) + 
          colScale() + ggtitle("14/12 Ratio") +
          ylab(expression(paste("Raw 14/12C (x", 10^{-12},")"))) +
          theme(axis.title.x = element_blank()) + 
          theme(axis.title.y = element_text(size=16), 
                axis.text.y  = element_text(size=12))

        g2 <- ggplot(data, aes(ts, cor1412he, color = Num)) + geom_point(size=3) + 
          colScale() + ggtitle("13/12C corrected 14/12 Ratio") +
          ylab(expression(paste("14/12C (x", 10^{-12},")"))) +
          theme(axis.title.x = element_blank()) + #theme(legend.position="none") +
          theme(axis.title.y = element_text(size=16), axis.text.y  = element_text(size=12))

        g3 <- ggplot(data, aes(ts, he12C, color = Num)) + geom_point(size=3) + 
          colScale() + ggtitle("12C Currents") +
          ylab(expression(paste("He 12C (", mu,"A)"))) +
          theme(axis.title.x = element_blank()) + #theme(legend.position="none") +
          theme(axis.title.y = element_text(size=16), axis.text.y  = element_text(size=12))
      plots <- list(g1, g2, g3)
      }  
    }  
    marrangeGrob(plots, nrow=length(plots), ncol=1, top = NULL)
  })
  
  
  output$table <- renderDataTable({
    subData() %>% mutate(Timestamp = strftime(ts,"%b-%d %H:%M:%S"),
                         X13.12he = X13.12he*100,
                         `LE12C (uA)` = format_num(le12C),
                         `HE12C (uA)` = format_num(he12C),
                         `HE13/12C (x10E-2)` = format_num(X13.12he),
                         `HE14/12C (x10E-12)` = format_num(X14.12he)) %>%
                  select(Timestamp, Pos, Meas,
                         Name = Sample.Name, `LE12C (uA)`,
                         `HE12C (uA)`, `HE13/12C (x10E-2)`,
                         `HE14/12C (x10E-12)`) 
  }, options = list(scrollY = '400px', scrollCollapse = TRUE, paging = FALSE))
})


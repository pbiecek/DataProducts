
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)
library(dplyr)
library(ZPD)
library(ggplot2)

shinyServer(function(input, output) {
  
  output$distPlot <- renderPlot({
    
    # generate and plot an rnorm distribution with the requested
    # number of observations
    data <- as.data.frame(t(szkola_uczniowie(input$gmina, input$rok, input$typ, input$sprrdz, input$sprcz)))
    ggplot(data, aes(x=rownames(data), y=V1))
    
  })
  
})

load("bigData.RData")
load("smallData.RData")
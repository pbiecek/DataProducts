
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)

shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Proof of concept"),
  
  # Sidebar with a slider input for number of observations
  sidebarPanel(
    numericInput("rok", "Rok:", 1990, min = 1990),
    textInput("gmina", "Gmina:"),
    textInput("typ", "Typ szkoły:"),
    textInput("sprrdz", "Rodzaj sprawdzianu:"),
    textInput("sprcz", "Część sprawdzianu:")
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("distPlot")
  )
))

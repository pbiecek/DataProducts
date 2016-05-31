
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)
library(dplyr)

shinyUI(pageWithSidebar(
  
  headerPanel("Trzecia iteracja"),
    
  sidebarPanel(
      uiOutput("egz_wybor"),
      uiOutput("arkusz_wybor"),
      uiOutput("egz_wybor_poprz"),
      selectInput("poziom", label = "Grupuj po:", list(
        "kryteriach oceny" = "kry",
        "pytaniach" = "pyt",
        "wiązkach pytań" = "wia"
        )
      )
    ),
  mainPanel(
    tabsetPanel(
      id = "tabset",
      tabPanel(
        "Wpływ wcześniejszych etapów edukacji",
        value = "poprzednie",
        plotOutput("poprz_plot", click="poprz_click"),
        plotOutput("poprz_plot_jedno")
      )
    )
  )
))
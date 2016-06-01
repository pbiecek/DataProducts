
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)
library(dplyr)
library(shinyBS)


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
      ),
      actionButton("help_but", "Help")
    ),
  mainPanel(
    tabsetPanel(
      id = "tabset",
      tabPanel(
        "Wpływ wcześniejszych etapów edukacji",
        value = "poprzednie",
        plotOutput("poprz_plot", click="poprz_click"),
        htmlOutput("link_do_arkusza"),
        htmlOutput("link_do_klucza"),
        plotOutput("poprz_plot_jedno"),
        htmlOutput("arkusze_zawierajace")
      )
    ),
    bsModal("help_modal", "Interfejs użytkownika", "help_but", tags$div("Some helpful info!"))
  )
))

# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)
library(dplyr)
library(shinyBS)

modalHelp <- function(name, header = "Help", content = "Pomoc niedostępna.") {
  tags$html(actionButton(name, "Help"),
            bsModal(paste(c("window",name), collapse="_"), header, name, tags$div(content)))
}

shinyUI(pageWithSidebar(
  
  headerPanel("Trzecia iteracja"),
    
  sidebarPanel(
      uiOutput("egz_wybor"),
      uiOutput("arkusz_wybor"),
      uiOutput("egz_wybor_poprz"),
      selectInput("poziom", label = "Grupuj po:", list(
        "wiązkach pytań" = "wia",
        "pytaniach" = "pyt",
        "kryteriach oceny" = "kry"
        )
      ),
      modalHelp("help_but", "Interfejs użytkownika", "Some helpful informations!")
    ),
  mainPanel(
    tabsetPanel(
      id = "tabset",
      tabPanel(
        "Wpływ wcześniejszych etapów edukacji",
        value = "poprzednie",
        plotOutput("poprz_plot", click="poprz_click"),
        modalHelp("help_but_main", "Wykresy"),
        htmlOutput("link_do_arkusza"),
        htmlOutput("link_do_klucza"),
        plotOutput("poprz_plot_jedno"),
        htmlOutput("arkusze_zawierajace")
      )
    ))
))
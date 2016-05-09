library(shiny)

source("input.R")

nazwyPrzedmiotow <- c("1000-211bWPI", "1000-214bWWW", "1000-214bJAO", "1000-214bSIK")
  
shinyUI(fluidPage(
  titlePanel("Zależność rozkładu ocen z przedmiotu B od przedmiotu A"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "przedmiot1", 
                  label = "Wybierz przedmiot A",
                  choices = nazwyPrzedmiotow,
                  selected = "1000-211bWPI"
                  ),
      selectInput(inputId = "przedmiot2", 
                  label = "Wybierz przedmiot B",
                  choices = nazwyPrzedmiotow,
                  selected = "1000-214bWWW"
      )
    ),
    mainPanel(
      plotOutput("corDiagram"),
      dataTableOutput("corTable")
    )
  )
))

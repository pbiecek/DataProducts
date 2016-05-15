library(shiny)

source("input.R")

nazwyPrzedmiotow <- get_subjects_codes() 

shinyUI(fluidPage(
  titlePanel("Zależność rozkładu ocen z przedmiotu B od przedmiotu A"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "przedmiot1", 
                  label = "Wybierz przedmiot A",
                  choices = nazwyPrzedmiotow,
                  selected = "1000-211aWPI"
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

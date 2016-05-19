library(shiny)

source("input.R")

nazwyPrzedmiotow <- get_subjects_codes()

shinyUI(fluidPage(
  titlePanel("Przedmioty skorelowane"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "przedmiot", 
                  label = "Wybierz przedmiot",
                  choices = nazwyPrzedmiotow,
                  selected = "1000-214bJAO"
      )
    ),
    mainPanel(
      textOutput("headerNegative"),
      plotOutput("corDiagramNegative"),
      textOutput("headerPositive"),
      plotOutput("corDiagramPositive")
    )
  )
))

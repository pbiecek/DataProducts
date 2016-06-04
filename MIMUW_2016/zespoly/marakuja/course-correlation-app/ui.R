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
      ),
      numericInput("min-common", "Minimalna liczba wspólnych studentów", 20,
                   min = 1, max = 200)
    ),
    mainPanel(
      textOutput("headerNegative"),
      plotOutput("corDiagramNegative"),
      dataTableOutput("tableNegative"),
      textOutput("headerPositive"),
      plotOutput("corDiagramPositive"),
      dataTableOutput("tablePositive")
    )
  )
))

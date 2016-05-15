library(shiny)

source("input.R")

nazwyPrzedmiotow <- get_subjects_codes()

shinyUI(fluidPage(
  titlePanel("Wpływ przedmiotów na MIMie na wybrany"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "przedmiot", 
                  label = "Wybierz przedmiot",
                  choices = nazwyPrzedmiotow,
                  selected = "1000-213bASD"
      )
    ),
    mainPanel(
      tabsetPanel(
          tabPanel("Sprzyjające zdanie",
                   textOutput("headerPositive"),
                   plotOutput("corDiagramPositive")),
          tabPanel("Niesprzyjające niezdanie",
                   textOutput("headerNegative"),
                   plotOutput("corDiagramNegative"))
      )
    )
  )
))

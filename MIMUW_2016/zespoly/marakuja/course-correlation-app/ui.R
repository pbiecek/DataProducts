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
                  selected = "1000-213aBAD"
      )
    ),
    mainPanel(
      tabsetPanel(
          tabPanel("Sprzyjające zdanie", plotOutput("corDiagramPositive")),
          tabPanel("Niesprzyjające niezdanie", plotOutput("corDiagramNegative"))
      )
    )
  )
))

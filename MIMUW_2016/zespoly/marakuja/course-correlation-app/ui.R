library(shiny)

source("courses.R")


nazwyPrzedmiotow <- courses_vector

shinyUI(fluidPage(
  titlePanel("Porównanie przedmiotów"),
  tabsetPanel(
    tabPanel("Wybierz jeden przedmiot",
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
    ),
    tabPanel("Wybierz dwa przedmioty",
      sidebarLayout(
        sidebarPanel(
          selectInput(inputId = "przedmiot_a",
                      label = "Wybierz przedmiot A",
                      choices = nazwyPrzedmiotow,
                      selected = "1000-224bJNP2"),
          selectInput(inputId = "przedmiot_b",
                      label = "Wybierz przedmiot B",
                      choices = nazwyPrzedmiotow,
                      selected = "1000-214bJAO")
        ),
        mainPanel(
          plotOutput("corDiagramTwoCourses")
        )
      )
    )
  )
))

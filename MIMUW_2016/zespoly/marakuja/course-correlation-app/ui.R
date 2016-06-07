library(shiny)

source("courses.R")


nazwyPrzedmiotow <- courses_vector

shinyUI(fluidPage(
  titlePanel("Porównanie przedmiotów"),
  tabsetPanel(
    tabPanel("Znajdź skorelowane przedmioty",
      sidebarLayout(
        sidebarPanel(
          selectInput(inputId = "przedmiot",
                      label = "Wybierz przedmiot B",
                      choices = nazwyPrzedmiotow,
                      selected = "1000-214bJAO"
          ),
          selectInput(inputId = "min-grade",
                      label = "Minimalna ocena z przedmiotu B",
                      choices = c(2, 3, 3.5, 4, 4.5, 5),
                      selected = 5),
          
          numericInput("min-common", "Minimalna liczba wspólnych studentów", 20,
                       min = 1)
        ),
        mainPanel(
          h3(textOutput("headerNegative")),
          plotOutput("corDiagramNegative"),
          dataTableOutput("tableNegative"),
          h3(textOutput("headerPositive")),
          plotOutput("corDiagramPositive"),
          dataTableOutput("tablePositive")
        )
      )
    ),
    tabPanel("Porównaj dwa przedmioty",
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
          h3(textOutput("headerTwoCourses")),
          plotOutput("corDiagramTwoCourses"),
          textOutput("legendTwoCourses"),
          tableOutput("tableTwoCourses")
        )
      )
    )
  )
))

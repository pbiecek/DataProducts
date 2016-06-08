library(shiny)

source("courses.R")


nazwyPrzedmiotow <- courses_vector

shinyUI(fluidPage(
  titlePanel("Porównanie przedmiotów z informatyki, MIMUW"),
  tabsetPanel(
    tabPanel("Znajdź skorelowane przedmioty",
      sidebarLayout(
        sidebarPanel(
          em("Zdanie lub niezdanie których przedmiotów najbardziej koreluje
             z uzyskaniem oceny przynajmniej X z przedmiotu B?"),
          hr(),
          selectInput(inputId = "przedmiot",
                      label = "Wybierz przedmiot B",
                      choices = nazwyPrzedmiotow,
                      selected = "1000-214bJAO"
          ),
          selectInput(inputId = "min-grade",
                      label = "Ocena X",
                      choices = c(2, 3, 3.5, 4, 4.5, 5),
                      selected = 4),
          
          numericInput("min-common", "Minimalna liczba studentów z oceną co najmniej graniczną", 10,
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
          em("Jak zdanie bądź niezdanie przedmiotu A koreluje z uzyskaniem oceny
             przynajmniej X z przedmiotu B?"),
          hr(),
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
          htmlOutput("textSummary"),
          textOutput("legendTwoCourses"),
          tableOutput("tableTwoCourses")
        )
      )
    )
  )
))

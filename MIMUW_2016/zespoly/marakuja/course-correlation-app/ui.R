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
             z uzyskaniem przynajmniej oceny X z przedmiotu B?"),
          hr(),
          selectInput(inputId = "przedmiot",
                      label = "Przedmiot B",
                      choices = nazwyPrzedmiotow,
                      selected = "1000-214bJAO"
          ),
          selectInput(inputId = "min-grade",
                      label = "Ocena X",
                      choices = c(2, 3, 3.5, 4, 4.5, 5),
                      selected = 4),
          
          numericInput("min-common", "Minimalna liczba studentów z oceną co najmniej X",
                       10, min = 1)
        ),
        mainPanel(
          h3(textOutput("headerNegative")),
          plotOutput("corDiagramNegative"),
          dataTableOutput("tableNegative"),
          tags$style(type="text/css", '#tableNegative tfoot {display:none;}'),
          h3(textOutput("headerPositive")),
          plotOutput("corDiagramPositive"),
          dataTableOutput("tablePositive"),
          tags$style(type="text/css", '#tablePositive tfoot {display:none;}')
        )
      )
    ),
    tabPanel("Porównaj dwa przedmioty",
      sidebarLayout(
        sidebarPanel(
          em("Jak zdanie bądź niezdanie przedmiotu A koreluje z uzyskaniem poszczególnych
             ocen z przedmiotu B?"),
          hr(),
          selectInput(inputId = "przedmiot_a",
                      label = "Przedmiot A",
                      choices = nazwyPrzedmiotow,
                      selected = "1000-224bJNP2"),
          selectInput(inputId = "przedmiot_b",
                      label = "Przedmiot B",
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

library(shiny)
library(PogromcyDanych)

source("input.R")

nazwyPrzedmiotow <- get_course_names(usos_dump)[
  grep("^1000", get_course_names(usos_dump))]
  
shinyUI(fluidPage(
  titlePanel("Porównanie występowania średnich ocen z wybranych grup przedmiotów"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "przedmiot1", 
                  label = "Wybierz grupę 1 do analizy",
                  choices = nazwyPrzedmiotow,
                  selected = c("1000-211aWPI", "1000-211bPM"),
                  multiple = TRUE
                  ),
      selectInput(inputId = "przedmiot2", 
                  label = "Wybierz grupę 2 do analizy",
                  choices = nazwyPrzedmiotow,
                  selected = c("1000-214bWWW"),
                  multiple = TRUE
      )
    ),
    mainPanel(
      plotOutput("corDiagram"),
      dataTableOutput("corTable")
    )
  )
))

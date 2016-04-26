library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  titlePanel("Wyniki egzaminów maturalnych a dane społeczno-ekonomiczne"),
  
  sidebarPanel(
    selectInput(inputId = "rodzajWskaznika",
                label = "Wybierz wskaźnik do porównania",
                choices = c("Wydatki gminy na licealistę przez 3 lata" = "wydatki",
                            "Liczba komputerów na licealistę" = "komputery"),
                selected = "wydatki"),
  
      htmlOutput("listaGmin"),
  
      selectInput(inputId = "przedmiot",
                  label = "Wybierz przedmiot",
                  choices = c("Język polski podstawowy" = "polski"),
                  selected = "polski"),
    
      sliderInput("rok",
                  "Wybierz rok egzaminu:",
                  sep = "",
                  min = 2012,
                  max = 2014,
                  value = 2012)
  ),
    
  mainPanel(
    tabsetPanel(
      tabPanel("Wykres", 
               h3("Porównianie wyników z matury do wybranego wskaźnika"), 
               plotOutput("wykres")),
      tabPanel("Opis",
               h3("Opis porównania"),
               verbatimTextOutput("opis")
      )    
    )
  )
  
))

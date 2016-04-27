library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  titlePanel("Wyniki egzaminów maturalnych a dane społeczno-ekonomiczne"),
  
  sidebarPanel(
    width = 3,
    selectInput(inputId = "rodzajWskaznika",
                label = "Wybierz wskaźnik do porównania",
                choices = c("Wydatki gminy na licealistę przez 3 lata" = "wydatki",
                            "Liczba komputerów na licealistę" = "komputery",
                            "Liczba mieszkańców na 1 bibliotekę" = "biblioteki",
                            "Liczba czytelników bibliotek na 1000 mieszkańców" = "czytelnicy",
                            "Liczba wypożyczeń na 1 czytelnika" = "wypozyczenia",
                            "Bezrobocie" = "bezrobocie"),
                selected = "bezrobocie"),
  
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
    width = 9,
    tabsetPanel(
      tabPanel("Wykres porówujący", 
               h3("Porównanie wyników z matury do wybranego wskaźnika"), 
               plotOutput("wykres")),
      tabPanel("Wykres dla wskaźnika", 
                 h3("Wartość wskaźnika w porównaniu do innych gmin "), 
                 plotOutput("wykres2")),
      tabPanel("Opis",
               h3("Opis porównania"),
               verbatimTextOutput("opis")
      )    
    )
  )
  
))

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  titlePanel("Wyniki egzaminów maturalnych a dane społeczno-ekonomiczne"),
  
  mainPanel(
    
    htmlOutput("listaGmin"),
    
    selectInput(inputId = "przedmiot",
                label = "Wybierz przedmiot",
                choices = c("Język polski podstawowy" = "polski"),
                selected = "polski"),
    
     h3("Porównanie wyników z matury względem liczby bezrobotnych na 1000 osób w wieku produkcyjnym."), 
     
    plotOutput("wykres_bezrobotni"),
    sliderInput("rok_bezrobotni",
                "Wybierz rok egzaminu:",
                sep = "",
                min = 2012,
                max = 2014,
                value = 2012),
    
    plotOutput("wykres_bezrobotni2"),
    sliderInput("rok_bezrobotni2",
                "Wybierz rok egzaminu:",
                sep = "",
                min = 2012,
                max = 2014,
                value = 2012),
    
    h3("Porównanie wyników z matury względem liczby mieszkańców na 1 bibliotekę."),
    
    plotOutput("wykres_biblioteki"),
    sliderInput("rok_biblioteki",
                "Wybierz rok egzaminu:",
                sep = "",
                min = 2012,
                max = 2014,
                value = 2012),
    
    plotOutput("wykres_biblioteki2"),
    sliderInput("rok_biblioteki2",
                "Wybierz rok egzaminu:",
                sep = "",
                min = 2012,
                max = 2014,
                value = 2012),
    
    h3("Porównanie wyników z matury względem liczby czytelników na 1000 mieszkańców."),
    
    plotOutput("wykres_czytelnicy"),
    sliderInput("rok_czytelnicy",
                "Wybierz rok egzaminu:",
                sep = "",
                min = 2012,
                max = 2014,
                value = 2012),
    
    plotOutput("wykres_czytelnicy2"),
    sliderInput("rok_czytelnicy2",
                "Wybierz rok egzaminu:",
                sep = "",
                min = 2012,
                max = 2014,
                value = 2012),
    
    h3("Porównanie wyników z matury względem liczby wypożyczonych książek na 1 czytelnika."), 
    
    plotOutput("wykres_wypozyczenia"),
    sliderInput("rok_wypozyczenia",
                "Wybierz rok egzaminu:",
                sep = "",
                min = 2012,
                max = 2014,
                value = 2012),
    
    plotOutput("wykres_wypozyczenia2"),
    sliderInput("rok_wypozyczenia2",
                "Wybierz rok egzaminu:",
                sep = "",
                min = 2012,
                max = 2014,
                value = 2012),
    
    
    h3("Porównanie wyników z matury względem sumy trzyletnich wydatków na ucznia"),
    
    plotOutput("wykres_wydatki"),
    sliderInput("rok_wydatki",
                "Wybierz rok egzaminu:",
                sep = "",
                min = 2012,
                max = 2014,
                value = 2012),
    
    plotOutput("wykres_wydatki2"),
    sliderInput("rok_wydatki2",
                "Wybierz rok egzaminu:",
                sep = "",
                min = 2012,
                max = 2014,
                value = 2012)
   
      )    
    )
  )
  
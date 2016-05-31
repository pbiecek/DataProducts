library(shiny)
library(plotly)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "header.css")
  ),
  
  tags$div(class="header",
           tags$div(class="header_title",
                    tags$p("JAK POSZŁA MATURA?")),
           tags$div(class="slider",
                    sliderInput("rok", NULL,  sep = "",
                                min = 2012,
                                max = 2014,
                                value = 2012,
                                width='150px')    
           ),
           tags$div(class="select",
                    selectInput(inputId = "przedmiot",
                                label = NULL,
                                width = '200px',
                                choices = c("Język polski podstawowy" = "polski"),
                                selected = "polski"))),
  
  tags$div(class="con",
           
           tags$p(class="subtitle", 
                  "Aplikacja umożliwia zestawienie wyników matur w danej gminie z czynnikami społeczno-ekonomicznymi
              oraz porównanie sytuacji wyników tego zestawienia z innymi gminami w Polsce."),
           
           mainPanel(
             
             tags$div(
              tags$p(class="select_gmina", "Wybierz gminę"),
              htmlOutput("listaGmin")),
             
             tags$p(class="subtitle", 
                    "Poniżej znajdują się wykresy, w których wyniki wybranej matury zestawione są z różnymi czynnikami, 
                    których opisy podane są w tytułach wykresu."),
             
             h3("Liczba bezrobotnych na 1000 osób w wieku produkcyjnym."),
             h5("Im większa wartość, tym większe bezrobocie w gminie."),
             tags$p(class="subtitle", "Poniższy wykres zestawia wyniki matur z liczbą bezrobotnych
                    przypadających na 1000 mieszkańców w danych gminach i pozwala porównać tę relację względem innych
                    gmin w Polsce. "),
             plotlyOutput("wykres_bezrobotni"),
             
             tags$p(class="subtitle", "Poniżej możemy sprawdzić na jakim poziomie jest bezrobocie w wybranej 
                    gminie w porównaniu z innymi polskimi gminami."),
             plotlyOutput("wykres_bezrobotni2"),
             textOutput("liczba_bezrobotni"),
             
             h3("Liczba mieszkańców na 1 bibliotekę."),
             h5("Im mniejsza wartość, tym większe zagęszczenie bibliotek w gminie."),
             
             tags$p(class="subtitle", "Poniższy wykres zestawia wyniki matur z liczbą mieszkańców 
                    przypadających na jedną bibliotekę w danych gminach i pozwala porównać tę relację względem innych
                    gmin w Polsce."),
             plotlyOutput("wykres_biblioteki"),
             
             tags$p(class="subtitle", "Poniżej możemy sprawdzić na jakim poziomie jest ilość bibliotek względem
                    liczby mieszkańców wybranej gminy w porównaniu z innymi polskimi gminami."),
             plotlyOutput("wykres_biblioteki2"),
             textOutput("liczba_biblioteki"),
             
             h3("Liczba czytelników na 1000 mieszkańców."),
             h5("Im większa wartość, tym większe czytelnictwo w gminie."),
             
             tags$p(class="subtitle", "Poniższy wykres zestawia wyniki matur z liczbą czytelników
                    przypadających na 1000 mieszkańców w danych gminach i pozwala porównać tę relację względem innych
                    gmin w Polsce."),
             plotlyOutput("wykres_czytelnicy"),
             
             tags$p(class="subtitle", "Poniżej możemy sprawdzić na jakim poziomie jest liczba czytelników względem
                    liczby mieszkańców wybranej gminy w porównaniu z innymi polskimi gminami."),
             plotlyOutput("wykres_czytelnicy2"),
             textOutput("liczba_czytelnicy"),
             
             h3("Liczba wypożyczonych książek na 1 czytelnika."),
             h5("Im większa wartość, tym czytelnicy czytają więcej książek."),
             
             tags$p(class="subtitle", "Poniższy wykres zestawia wyniki matur z liczbą przeczytanych książek
                    na 1 czytelnika w danych gminach i pozwala porównać tę relację względem innych
                    gmin w Polsce."),
             plotlyOutput("wykres_wypozyczenia"),
             
             tags$p(class="subtitle", "Poniżej możemy sprawdzić na jakim poziomie jest liczba przeczytanych książek
                    na 1 czytelnika w wybranej gminie w porównaniu z innymi polskimi gminami."),
             plotlyOutput("wykres_wypozyczenia2"),
             textOutput("liczba_wypozyczenia"),
             
             h3("Suma trzyletnich wydatków na ucznia"),
             h5("Im większa wartość, tym więcej wydano pieniędzy na jednego ucznia."),
             
             tags$p(class="subtitle", "Poniższy wykres zestawia wyniki matur z sumą trzyletnich wydatków
                    na 1 ucznia w danych gminach i pozwala porównać tę relację względem innych
                    gmin w Polsce."),
             plotlyOutput("wykres_wydatki"),
             
             tags$p(class="subtitle", "Poniżej możemy sprawdzić na jakim poziomie jest suma trzyletnich wydatków
                    na 1 ucznia w wybranej gminie w porównaniu z innymi polskimi gminami."),
             plotlyOutput("wykres_wydatki2"),
             textOutput("liczba_wydatki"),
             plotlyOutput("wykres_matury")
             
           )    
        )
    )
  )
  
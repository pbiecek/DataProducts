library(shiny)
library(dplyr)
library(ggplot2)

shinyServer(function(input, output) {
  
  wybranyWskaznik <- reactive({
    if (input$rodzajWskaznika == "wydatki")
      wskaznik <- arrange(polski_srednia_wydatki, nazwa_gminy)
  })
   
  output$listaGmin <- renderUI({ 
    wskaznik <- wybranyWskaznik()
    selectInput("gmina", "Wybierz gminę", wskaznik$nazwa_gminy)
  })
  
  output$wykres = renderPlot({
    wskaznik <- wybranyWskaznik()
    
    if (input$rodzajWskaznika == "wydatki") {
      wskaznikWybranyRok <- filter(wskaznik, rok == input$rok)
      zaznaczonaGmina <- subset(wskaznikWybranyRok, nazwa_gminy == input$gmina)
      wykres <- ggplot(wskaznikWybranyRok, aes(suma_na_licealiste, sredni_wynik_matury)) + 
                xlab("Suma trzyletnich wydatków na jednego ucznia") +
                ylab("Średni wynik matury") +
                geom_point()
    }
    
    wykres <- wykres + geom_point(data = zaznaczonaGmina, colour = "red", size = 5) +
              geom_text(data = zaznaczonaGmina, label = input$gmina, vjust = 2, colour = "red", size = 5)
    
    wykres
  })
  
  output$opis = renderText({
    if (input$rodzajWskaznika == "wydatki") {
      "TU pojawi się jakiś opis"
    }
  })
  
})

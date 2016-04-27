library(shiny)
library(dplyr)
library(ggplot2)

shinyServer(function(input, output) {
  
  wybranyWskaznik <- reactive({
    if (input$rodzajWskaznika == "wydatki")
      wskaznik <- arrange(polski_srednia_wydatki, nazwa_gminy)
    
    if (input$rodzajWskaznika == "bezrobocie")
      wskaznik <- arrange(polski_srednia_bezrobocie, nazwa_gminy)
  })
   
  output$listaGmin <- renderUI({ 
    wskaznik <- wybranyWskaznik()
 
    selectInput("gmina", "Wybierz gminę", wskaznik$nazwa_gminy)
  })
  
  output$wykres = renderPlot({
    wskaznik <- wybranyWskaznik()
    
    wskaznikWybranyRok <- filter(wskaznik, rok == input$rok)
    zaznaczonaGmina <- subset(wskaznikWybranyRok, nazwa_gminy == input$gmina)
    
    
    if (input$rodzajWskaznika == "wydatki") {
      wykres <- ggplot(wskaznikWybranyRok, aes(suma_na_licealiste, sredni_wynik_matury)) +
        ylab("Średni wynik matury") +
        xlab("Suma trzyletnich wydatków na jednego ucznia") +
        geom_point(colour = "lightblue")
    }
    
    if (input$rodzajWskaznika == "bezrobocie") {
      wykres <- ggplot(wskaznikWybranyRok, aes(Wartosc, sredni_wynik_matury)) +
        ylab("Średni wynik matury") +
        xlab("Liczba bezrobotnych na 1000 osób w wieku produkcyjnym") +
        geom_point(colour = "lightblue")
    }
    
    wykres <- wykres + geom_point(data = zaznaczonaGmina, colour = "#D55E00", size = 5) +
              geom_text(data = zaznaczonaGmina, label = input$gmina, vjust = 2, colour = "#D55E00", size = 7)
    
    wykres
  })
  
  output$opis = renderText({
    if (input$rodzajWskaznika == "wydatki") {
      "TU pojawi się jakiś opis"
    }
  })
  
  output$wykres2 = renderPlot({
    wskaznik <- wybranyWskaznik()
    
    if (input$rodzajWskaznika == "bezrobocie") {
      wskaznikWybranyRok <- filter(bezrobocie_bez_dupl, Rok == input$rok)
      zaznaczonaGmina <- subset(wskaznikWybranyRok, Nazwa == input$gmina)
      
      base <- ggplot(wskaznikWybranyRok, aes(Wartosc)) +
        geom_area(aes(y = ..count..), stat = "bin", binwidth = 5, colour = "lightblue", fill = "lightblue")
      
      pl <- base +
        geom_vline(xintercept = zaznaczonaGmina$Wartosc, linetype = "dashed", color = "#D55E00", size = 0.8) +
        labs(x = "Liczba bezrobotnych na 1000 osób w wieku produkcyjnym", y = "Liczba gmin") +
        geom_text(aes(zaznaczonaGmina$Wartosc, mean(range(ggplot_build(base)$data[[1]]$count)), 
          label = zaznaczonaGmina$Wartosc), colour = "black", angle = 90, vjust = 1, nudge_x = 1)
    }
    pl
  })
  
})

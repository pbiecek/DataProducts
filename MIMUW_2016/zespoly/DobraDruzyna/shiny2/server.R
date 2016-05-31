library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)

shinyServer(function(input, output) { 
  
  output$listaGmin <- renderUI({ 
    wskaznik <- arrange(polski_srednia_wydatki, nazwa_gminy)
    
    selectInput("gmina", NULL, wskaznik$nazwa_gminy, selected = NULL, width='100%')
  })
  
  output$wykres_wydatki = renderPlotly({
    wskaznik <- arrange(polski_srednia_wydatki, nazwa_gminy)
    
    wskaznikWybranyRok <- filter(wskaznik, rok == input$rok)
    
    zaznaczonaGmina <- subset(wskaznikWybranyRok, nazwa_gminy == input$gmina)
    
    wykres <- plot_ly(wskaznikWybranyRok, x = suma_na_licealiste, y = sredni_wynik_matury, text = paste("gmina: ", nazwa_gminy),
                   mode = "markers", marker = list(color = "lightblue"))
    
    wykres <- layout(wykres, 
                     xaxis = list(title = "Suma trzyletnich wydatków na ucznia"),
                     yaxis = list(title = "Średni wynik z matury"),
                     annotations = list(x = zaznaczonaGmina$suma_na_licealiste, y = zaznaczonaGmina$sredni_wynik_matury, text = "Twoja gmina", showarrow = T))
    
    wykres
  })
  
  output$wykres_wydatki2 = renderPlotly({
    wykres <- plot_ly(filter(polski_srednia_wydatki, nazwa_gminy==zaznaczonaGmina$nazwa_gminy), x = rok, y = suma_na_licealiste)
    wykres <- layout(wykres,
                     yaxis = list(title = "Suma trzyletnich wydatków na ucznia"))
    wykres
  })

  output$liczba_wydatki = renderText({
    wskaznikWybranyRok <- filter(polski_srednia_wydatki, rok == input$rok)
    zaznaczonaGmina <- subset(wskaznikWybranyRok, nazwa_gminy == input$gmina)
    gminy_wiecej <- filter(wskaznikWybranyRok, suma_na_licealiste > zaznaczonaGmina$suma_na_licealiste)
    
    nrow(gminy_wiecej)/(nrow(wskaznikWybranyRok) - 1) * 100
    
  })
  
  output$wykres_bezrobotni = renderPlotly({
    wskaznik <- arrange(polski_srednia_bezrobocie, nazwa_gminy)
    
    wskaznikWybranyRok <- filter(wskaznik, rok == input$rok)
    zaznaczonaGmina <- subset(wskaznikWybranyRok, nazwa_gminy == input$gmina)
    
    wykres <- plot_ly(wskaznikWybranyRok, x = Wartosc, y = sredni_wynik_matury, text = paste("gmina: ", nazwa_gminy),
                      mode = "markers", marker = list(color = "lightblue"))
    
    wykres <- layout(wykres, 
                     xaxis = list(title = "Liczba bezrobotnych na 1000 osób w wieku produkcyjnym"),
                     yaxis = list(title = "Średni wynik z matury"),
                     annotations = list(x = zaznaczonaGmina$Wartosc, y = zaznaczonaGmina$sredni_wynik_matury,
                                        text = "Twoja gmina", showarrow = T))
    
    wykres
  })
  
  output$wykres_bezrobotni2 = renderPlotly({
    wykres <- plot_ly(filter(bezrobocie_bez_dupl, Nazwa==zaznaczonaGmina$nazwa_gminy), x = Rok, y = Wartosc)
    wykres <- layout(wykres,
                     yaxis = list(title = "Bezrobocie"))
    wykres
  })
  
  output$liczba_bezrobotni = renderText({
    wskaznikWybranyRok <- filter(polski_srednia_bezrobocie, rok == input$rok)
    zaznaczonaGmina <- subset(wskaznikWybranyRok, nazwa_gminy == input$gmina)
    gminy_wiecej = filter(wskaznikWybranyRok, Wartosc > zaznaczonaGmina$Wartosc)
    
    nrow(gminy_wiecej)/(nrow(wskaznikWybranyRok) - 1) * 100
  })
  
  output$wykres_biblioteki = renderPlotly({
    wskaznik <- arrange(polski_srednia_biblioteki, nazwa_gminy)
    
    wskaznikWybranyRok <- filter(wskaznik, rok == input$rok)
    zaznaczonaGmina <- subset(wskaznikWybranyRok, nazwa_gminy == input$gmina)

    wykres <- plot_ly(wskaznikWybranyRok, x = Wartosc, y = sredni_wynik_matury, text = paste("gmina: ", nazwa_gminy),
                      mode = "markers", marker = list(color = "lightblue"))
    
    wykres <- layout(wykres, 
                     xaxis = list(title = "Liczba mieszkańców na 1 bibliotekę"),
                     yaxis = list(title = "Średni wynik z matury"),
                     annotations = list(x = zaznaczonaGmina$Wartosc, y = zaznaczonaGmina$sredni_wynik_matury,
                                        text = "Twoja gmina", showarrow = T))
    

    wykres
  })
  
  output$wykres_biblioteki2 = renderPlotly({
    plot_ly(filter(ludnosc_na_biblioteke_bez_dupl, Nazwa==zaznaczonaGmina$nazwa_gminy), x = Rok, y = Wartosc)
    wykres <- layout(wykres,
                     yaxis = list(title = "Liczba mieszkańców na 1 bibliotekę"))
    wykres
  })
  
  output$liczba_biblioteki = renderText({
    wskaznikWybranyRok <- filter(polski_srednia_biblioteki, rok == input$rok)
    zaznaczonaGmina <- subset(wskaznikWybranyRok, nazwa_gminy == input$gmina)
    gminy_wiecej = filter(wskaznikWybranyRok, Wartosc > zaznaczonaGmina$Wartosc)
    
    nrow(gminy_wiecej)/(nrow(wskaznikWybranyRok) - 1) * 100
  })
  
  output$wykres_czytelnicy = renderPlotly({
    wskaznik <- arrange(polski_srednia_czytelnicy, nazwa_gminy)
    
    wskaznikWybranyRok <- filter(wskaznik, rok == input$rok)
    zaznaczonaGmina <- subset(wskaznikWybranyRok, nazwa_gminy == input$gmina)
    
  
    wykres <- plot_ly(wskaznikWybranyRok, x = Wartosc, y = sredni_wynik_matury, text = paste("gmina: ", nazwa_gminy),
                      mode = "markers", marker = list(color = "lightblue"))
    
    wykres <- layout(wykres, 
                     xaxis = list(title = "Liczba czytelników bibliotek na 1000 mieszkańców"),
                     yaxis = list(title = "Średni wynik z matury"),
                     annotations = list(x = zaznaczonaGmina$Wartosc, y = zaznaczonaGmina$sredni_wynik_matury,
                                        text = "Twoja gmina", showarrow = T))
    
    wykres
  })
  
  output$wykres_czytelnicy2 = renderPlotly({
    wykres <- plot_ly(filter(czytelnicy, Nazwa==zaznaczonaGmina$nazwa_gminy), x = Rok, y = Wartosc)
    wykres <- layout(wykres,
                     yaxis = list(title = "Liczba czytelników bibliotek na 1000 mieszkańców"))
    wykres
  })
  
  output$liczba_czytelnicy = renderText({
    wskaznikWybranyRok <- filter(polski_srednia_czytelnicy, rok == input$rok)
    zaznaczonaGmina <- subset(wskaznikWybranyRok, nazwa_gminy == input$gmina)
    gminy_wiecej = filter(wskaznikWybranyRok, Wartosc > zaznaczonaGmina$Wartosc)
    
    nrow(gminy_wiecej)/(nrow(wskaznikWybranyRok) - 1) * 100
  })
  
  output$wykres_wypozyczenia = renderPlotly({
    wskaznik <- arrange(polski_srednia_wypozyczenia, nazwa_gminy)
    
    wskaznikWybranyRok <- filter(wskaznik, rok == input$rok)
    zaznaczonaGmina <- subset(wskaznikWybranyRok, nazwa_gminy == input$gmina)
   
    wykres <- plot_ly(wskaznikWybranyRok, x = Wartosc, y = sredni_wynik_matury, text = paste("gmina: ", nazwa_gminy),
                      mode = "markers", marker = list(color = "lightblue"))
    
    wykres <- layout(wykres, 
                     xaxis = list(title = "Liczba wypożyczeń na 1 czytelnika"),
                     yaxis = list(title = "Średni wynik z matury"),
                     annotations = list(x = zaznaczonaGmina$Wartosc, y = zaznaczonaGmina$sredni_wynik_matury,
                                        text = "Twoja gmina", showarrow = T))
    
    wykres
  })
  
  output$wykres_wypozyczenia2 = renderPlotly({
    wykres <- plot_ly(filter(wypozyczenia, Nazwa==zaznaczonaGmina$nazwa_gminy), x = Rok, y = Wartosc)
    wykres <- layout(wykres,
                     yaxis = list(title = "Liczba wypożyczeń na 1 czytelnika"))
    wykres
  })
  
  output$liczba_wypozyczenia = renderText({
    wskaznikWybranyRok <- filter(polski_srednia_wypozyczenia, rok == input$rok)
    zaznaczonaGmina <- subset(wskaznikWybranyRok, nazwa_gminy == input$gmina)
    gminy_wiecej = filter(wskaznikWybranyRok, Wartosc > zaznaczonaGmina$Wartosc)

    nrow(gminy_wiecej)/(nrow(wskaznikWybranyRok) - 1) * 100
  })
  
  output$wykres_matury = renderPlotly({
    wykres <- plot_ly(filter(polski_srednia, gmina_szkoly==zaznaczonaGmina$nazwa_gminy), x = rok, y = sredni_wynik_matury)
    wykres <- layout(wykres,
                     yaxis = list(title = "Średni wynik matury"))
    wykres
  })

})
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(RColorBrewer)
library(JakPoszlaMatura)

shinyServer(function(input, output) { 
  
  output$listaGmin <- renderUI({ 
    wskaznik <- arrange(polski, gmina_szkoly)
    
    teryty <- wskaznik$teryt
    nazwy <- wskaznik$gmina_szkoly
    names(teryty) <- nazwy
    
    selectInput("gmina", NULL, teryty, width='100%')
  })
  
  output$wykres_wydatki = renderPlotly({
      wskaznik <- reactive({
          data <- merge(polski, wydatki_na_licea)
          merge(x = data, y = ewd, by = c("rok", "teryt"))
      })
      
      wskaznikWybranyRok <- filter(wskaznik(), rok == input$rok)
      
      zaznaczonaGmina <- subset(wskaznikWybranyRok, teryt == input$gmina)
      
      wykres <- plot_ly(wskaznikWybranyRok, x = Wartosc, y = srednia_gminy, text = paste("gmina: ", gmina_szkoly),
      mode = "markers", color = EWD, colors = "RdYlGn")
      
      wykres <- layout(wykres,
        xaxis = list(title = "Suma trzyletnich wydatków na ucznia"),
        yaxis = list(title = "Średni wynik z matury"),
        annotations = list(x = zaznaczonaGmina$Wartosc, y = zaznaczonaGmina$srednia_gminy,
        text = "Wybrana gmina", showarrow = T))
      
      wykres
  })
  
  output$wykres_wydatki2 = renderPlotly({
      wskaznik <- merge(x = wydatki_na_licea, y = ewd, by = c("rok", "teryt"))
      
      wykres <- plot_ly(filter(wskaznik, teryt==input$gmina), x = rok, y = Wartosc,
      mode = "markers+lines", color = EWD, colors = "RdYlGn",
      marker = list(size = 18),
      line = list(color = "grey"))
      
      wykres <- layout(wykres,
      yaxis = list(title = "Suma trzyletnich wydatków na ucznia."))
      
      wykres
  })
  
  output$liczba_wydatki = renderText({
    wskaznikWybranyRok <- filter(polski_srednia_wydatki, rok == input$rok)
    zaznaczonaGmina <- subset(wskaznikWybranyRok, nazwa_gminy == input$gmina)
    gminy_wiecej <- filter(wskaznikWybranyRok, suma_na_licealiste > zaznaczonaGmina$suma_na_licealiste)
    
    nrow(gminy_wiecej)/(nrow(wskaznikWybranyRok) - 1) * 100
    
  })
  
  output$wykres_bezrobotni = renderPlotly({
    wskaznik <- reactive({
      data <- merge(polski, bezrobocie)
      merge(x = data, y = ewd, by = c("rok", "teryt"))
    })
    
    wskaznikWybranyRok <- filter(wskaznik(), rok == input$rok)
    zaznaczonaGmina <- subset(wskaznikWybranyRok, teryt == input$gmina)
    
    wykres <- plot_ly(wskaznikWybranyRok, x = Wartosc, y = srednia_gminy, text = paste("gmina: ", gmina_szkoly),
                      mode = "markers", color = EWD, colors = "RdYlGn")
    
    wykres <- layout(wykres, 
                     xaxis = list(title = "Liczba bezrobotnych na 1000 osób w wieku produkcyjnym"),
                     yaxis = list(title = "Średni wynik z matury"),
                     annotations = list(x = zaznaczonaGmina$Wartosc, y = zaznaczonaGmina$srednia_gminy,
                                        text = "Wybrana gmina", showarrow = T))
    
    wykres
  })
  
  output$wykres_bezrobotni2 = renderPlotly({
    wskaznik <- merge(x = bezrobocie, y = ewd, by = c("rok", "teryt"))
    
    wykres <- plot_ly(filter(wskaznik, teryt==input$gmina), x = rok, y = Wartosc, 
                      mode = "markers+lines", color = EWD, colors = "RdYlGn",
                      marker = list(size = 18),
                      line = list(color = "grey"))
                      
    wykres <- layout(wykres,
                     yaxis = list(title = "Bezrobocie"))
    
    wykres
  })
  
  output$liczba_bezrobocie = renderText({
    wskaznikWybranyRok <- filter(bezrobocie, rok == input$rok)
    zaznaczonaGmina <- subset(wskaznikWybranyRok, teryt == input$gmina)
    gminy_wiecej = filter(wskaznikWybranyRok, Wartosc < zaznaczonaGmina$Wartosc)
    
    round(nrow(gminy_wiecej)/(nrow(wskaznikWybranyRok) - 1) * 100, 0)
  })
  
  output$srednia_bezrobocie = renderText({
    wskaznikWybranyRok <- filter(bezrobocie, rok == input$rok)
    zaznaczonaGmina <- subset(wskaznikWybranyRok, teryt == input$gmina)
    
    zaznaczonaGmina$Wartosc
  })
  
  output$wykres_biblioteki = renderPlotly({
    wskaznik <- reactive({
      data <- merge(polski, biblioteki)
      merge(x = data, y = ewd, by = c("rok", "teryt"))
    })
    
    wskaznikWybranyRok <- filter(wskaznik(), rok == input$rok)
    zaznaczonaGmina <- subset(wskaznikWybranyRok, teryt == input$gmina)

    wykres <- plot_ly(wskaznikWybranyRok, x = Wartosc, y = srednia_gminy, text = paste("gmina: ", gmina_szkoly),
                      mode = "markers", color = EWD, colors = "RdYlGn")
    
    wykres <- layout(wykres, 
                     xaxis = list(title = "Liczba mieszkańców na 1 bibliotekę"),
                     yaxis = list(title = "Średni wynik z matury"),
                     annotations = list(x = zaznaczonaGmina$Wartosc, y = zaznaczonaGmina$srednia_gminy,
                                        text = "Twoja gmina", showarrow = T))
    

    wykres
  })
  
  output$wykres_biblioteki2 = renderPlotly({
    wskaznik <- merge(x = biblioteki, y = ewd, by = c("rok", "teryt"))
    
    wykres <- plot_ly(filter(wskaznik, teryt==input$gmina), x = rok, y = Wartosc, 
                      mode = "markers+lines", color = EWD, colors = "RdYlGn",
                      marker = list(size = 18),
                      line = list(color = "grey"))
    
    wykres <- layout(wykres,
                     yaxis = list(title = "Liczba mieszkańców na bibliotekę"))
    
    wykres
  })
  
  output$liczba_biblioteki = renderText({
    wskaznikWybranyRok <- filter(biblioteki, rok == input$rok)
    zaznaczonaGmina <- subset(wskaznikWybranyRok, teryt == input$gmina)
    gminy_wiecej = filter(wskaznikWybranyRok, Wartosc < zaznaczonaGmina$Wartosc)
    
    round(nrow(gminy_wiecej)/(nrow(wskaznikWybranyRok) - 1) * 100, 0)
  })
  
  output$srednia_biblioteki = renderText({
    wskaznikWybranyRok <- filter(biblioteki, rok == input$rok)
    zaznaczonaGmina <- subset(wskaznikWybranyRok, teryt == input$gmina)
    
    zaznaczonaGmina$Wartosc
  })
  
  output$wykres_czytelnicy = renderPlotly({
    wskaznik <- reactive({
      data <- merge(polski, czytelnicy)
      merge(x = data, y = ewd, by = c("rok", "teryt"))
    })
    
    wskaznikWybranyRok <- filter(wskaznik(), rok == input$rok)
    zaznaczonaGmina <- subset(wskaznikWybranyRok, teryt == input$gmina)
    
  
    wykres <- plot_ly(wskaznikWybranyRok, x = Wartosc, y = srednia_gminy, text = paste("gmina: ", gmina_szkoly),
                      mode = "markers", color = EWD, colors = "RdYlGn")
    
    wykres <- layout(wykres, 
                     xaxis = list(title = "Liczba czytelników bibliotek na 1000 mieszkańców"),
                     yaxis = list(title = "Średni wynik z matury"),
                     annotations = list(x = zaznaczonaGmina$Wartosc, y = zaznaczonaGmina$srednia_gminy,
                                        text = "Twoja gmina", showarrow = T))
    
    wykres
  })
  
  output$wykres_czytelnicy2 = renderPlotly({
    wskaznik <- merge(x = czytelnicy, y = ewd, by = c("rok", "teryt"))
    
    wykres <- plot_ly(filter(wskaznik, teryt==input$gmina), x = rok, y = Wartosc, 
                      mode = "markers+lines", color = EWD, colors = "RdYlGn",
                      marker = list(size = 18),
                      line = list(color = "grey"))
    
    wykres <- layout(wykres,
                     yaxis = list(title = "Liczba czytelników na 1000 mieszkańców"))
    
    wykres
  })
  
  output$liczba_czytelnicy = renderText({
    wskaznikWybranyRok <- filter(czytelnicy, rok == input$rok)
    zaznaczonaGmina <- subset(wskaznikWybranyRok, teryt == input$gmina)
    gminy_wiecej = filter(wskaznikWybranyRok, Wartosc > zaznaczonaGmina$Wartosc)
    
    round(nrow(gminy_wiecej)/(nrow(wskaznikWybranyRok) - 1) * 100, 0)
  })
  
  output$srednia_biblioteki = renderText({
    wskaznikWybranyRok <- filter(czytelnicy, rok == input$rok)
    zaznaczonaGmina <- subset(wskaznikWybranyRok, teryt == input$gmina)
    
    zaznaczonaGmina$Wartosc
  })
  
  output$wykres_wypozyczenia = renderPlotly({
    wskaznik <- reactive({
      data <- merge(polski, wypozyczenia)
      merge(x = data, y = ewd, by = c("rok", "teryt"))
    })
    
    wskaznikWybranyRok <- filter(wskaznik(), rok == input$rok)
    zaznaczonaGmina <- subset(wskaznikWybranyRok, teryt == input$gmina)
   
    wykres <- plot_ly(wskaznikWybranyRok, x = Wartosc, y = srednia_gminy, text = paste("gmina: ", gmina_szkoly),
                      mode = "markers", color = EWD, colors = "RdYlGn")
    
    wykres <- layout(wykres, 
                     xaxis = list(title = "Liczba wypożyczeń na 1 czytelnika"),
                     yaxis = list(title = "Średni wynik z matury"),
                     annotations = list(x = zaznaczonaGmina$Wartosc, y = zaznaczonaGmina$srednia_gminy,
                                        text = "Twoja gmina", showarrow = T))
    
    wykres
  })
  
  output$wykres_wypozyczenia2 = renderPlotly({
    wskaznik <- merge(x = wypozyczenia, y = ewd, by = c("rok", "teryt"))
    
    wykres <- plot_ly(filter(wskaznik, teryt==input$gmina), x = rok, y = Wartosc, 
                      mode = "markers+lines", color = EWD, colors = "RdYlGn",
                      marker = list(size = 18),
                      line = list(color = "grey"))
    
    wykres <- layout(wykres,
                     yaxis = list(title = "Liczba wypożyczeń na 1 czytelnika"))
    
    wykres
  })
  
  output$liczba_wypozyczenia = renderText({
    wskaznikWybranyRok <- filter(wypozyczenia, rok == input$rok)
    zaznaczonaGmina <- subset(wskaznikWybranyRok, teryt == input$gmina)
    gminy_wiecej = filter(wskaznikWybranyRok, Wartosc > zaznaczonaGmina$Wartosc)

    round(nrow(gminy_wiecej)/(nrow(wskaznikWybranyRok) - 1) * 100, 0)
  })
  
  output$srednia_wypozyczenia = renderText({
    wskaznikWybranyRok <- filter(wypozyczenia, rok == input$rok)
    zaznaczonaGmina <- subset(wskaznikWybranyRok, teryt == input$gmina)
    
    zaznaczonaGmina$Wartosc
  })
  
  output$srednia_matury = renderText({
    wskaznikWybranyRok <- filter(polski, rok == input$rok)
    zaznaczonaGmina <- subset(wskaznikWybranyRok, teryt == input$gmina)

    round(zaznaczonaGmina$srednia_gminy, 0)
  })
  
  output$liczba_matury = renderText({
    wskaznikWybranyRok <- filter(polski, rok == input$rok)
    zaznaczonaGmina <- subset(wskaznikWybranyRok, teryt == input$gmina)
    gminy_wiecej = filter(wskaznikWybranyRok, srednia_gminy > zaznaczonaGmina$srednia_gminy)
    
    round(nrow(gminy_wiecej)/(nrow(wskaznikWybranyRok) - 1) * 100, 0)
  })
  
  output$srednia_ewd = renderText({
    wskaznikWybranyRok <- filter(ewd, rok == input$rok)
    zaznaczonaGmina <- subset(wskaznikWybranyRok, teryt == input$gmina)
    
    zaznaczonaGmina$EWD
  })
  
  output$wykres_matury = renderPlotly({
    wskaznik <- merge(x = polski, y = ewd, by = c("rok", "teryt"))
    
    wykres <- plot_ly(filter(wskaznik, teryt==input$gmina), x = rok, y = srednia_gminy, 
                      mode = "markers+lines", color = EWD, colors = "RdYlGn",
                      marker = list(size = 18),
                      line = list(color = "grey"))
    
    wykres <- layout(wykres,
                     yaxis = list(title = "Średni wynik matury"))
    
    wykres
  })
})
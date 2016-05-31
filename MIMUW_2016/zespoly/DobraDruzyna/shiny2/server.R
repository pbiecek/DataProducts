library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)

shinyServer(function(input, output) { 
  
  output$listaGmin <- renderUI({ 
    wskaznik <- arrange(polski_srednia_wydatki, nazwa_gminy)
    
    selectInput("gmina", NULL, wskaznik$nazwa_gminy, selected = "Radom", width='100%')
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
  
  output$wykres_wydatki2 = renderPlot({
    wskaznik <- arrange(polski_srednia_wydatki, nazwa_gminy)
    wskaznikWybranyRok <- filter(wskaznik, rok == input$rok)
    zaznaczonaGmina <- subset(wskaznikWybranyRok, nazwa_gminy == input$gmina)
    
    baza <- ggplot(wskaznikWybranyRok, aes(suma_na_licealiste)) +
      geom_area(aes(y = ..count..), stat = "bin", binwidth = 10000, colour = "#87B4FF", fill = "#87B4FF")
    
    
    
    wykres <- baza +
      geom_vline(xintercept = zaznaczonaGmina$suma_na_licealiste, linetype = "dashed", color = "#D55E00", size = 0.8) +
      labs(x = "Suma wydatków na 1 licealistę", y = "Liczba gmin") +
      geom_text(aes(zaznaczonaGmina$suma_na_licealiste, mean(range(ggplot_build(baza)$data[[1]]$count)),
                    label = zaznaczonaGmina$suma_na_licealiste), colour = "black", angle = 90, vjust = 1, nudge_x = 1)
  
      wykres
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
  
  output$wykres_bezrobotni2 = renderPlot({
    wskaznik <- arrange(polski_srednia_bezrobocie, nazwa_gminy)
    wskaznikWybranyRok <- filter(bezrobocie_bez_dupl, Rok == input$rok)
    zaznaczonaGmina <- subset(wskaznikWybranyRok, Nazwa == input$gmina)
    
    baza <- ggplot(wskaznikWybranyRok, aes(Wartosc)) +
      geom_area(aes(y = ..count..), stat = "bin", binwidth = 5, colour = "#87B4FF", 
                fill = "#87B4FF")
    
    wykres<- baza +
      geom_vline(xintercept = zaznaczonaGmina$Wartosc, linetype = "dashed", color = "#D55E00", size = 0.8) +
      labs(x = "Liczba bezrobotnych na 1000 osób w wieku produkcyjnym", y = "Liczba gmin") +
      geom_text(aes(zaznaczonaGmina$Wartosc, mean(range(ggplot_build(baza)$data[[1]]$count)), 
                    label = zaznaczonaGmina$Wartosc), colour = "black", angle = 90, vjust = 1, nudge_x = 1)
  
    wykres
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
  
  output$wykres_biblioteki2 = renderPlot({
    wskaznik <- arrange(polski_srednia_biblioteki, nazwa_gminy)
    wskaznikWybranyRok <- filter(ludnosc_na_biblioteke_bez_dupl, Rok == input$rok)
    zaznaczonaGmina <- subset(wskaznikWybranyRok, Nazwa == input$gmina)
    
    baza <- ggplot(wskaznikWybranyRok, aes(Wartosc)) +
      geom_area(aes(y = ..count..), stat = "bin", binwidth = 500, colour = "#87B4FF", 
                fill = "#87B4FF")
    
    wykres<- baza +
      geom_vline(xintercept = zaznaczonaGmina$Wartosc, linetype = "dashed", color = "#D55E00", size = 0.8) +
      labs(x = "Liczba mieszkańców na 1 bibliotekę", y = "Liczba gmin") +
      geom_text(aes(zaznaczonaGmina$Wartosc, mean(range(ggplot_build(baza)$data[[1]]$count)),
                    label = zaznaczonaGmina$Wartosc), colour = "black", angle = 90, vjust = 1,
                nudge_x = 200, size = 5)
    
    wykres
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
  
  output$wykres_czytelnicy2 = renderPlot({
    wskaznik <- arrange(polski_srednia_czytelnicy, nazwa_gminy)
    wskaznikWybranyRok <- filter(czytelnicy_bez_dupl, Rok == input$rok)
    zaznaczonaGmina <- subset(wskaznikWybranyRok, Nazwa == input$gmina)
    
    baza <- ggplot(wskaznikWybranyRok, aes(Wartosc)) +
      geom_area(aes(y = ..count..), stat = "bin", binwidth = 15, colour = "#87B4FF",
                fill = "#87B4FF")
    
    wykres<- baza +
      geom_vline(xintercept = zaznaczonaGmina$Wartosc, linetype = "dashed", color = "#D55E00",
                 size = 0.6) +
      labs(x = "Liczba czytelników na 1000", y = "Liczba gmin") +
      geom_text(aes(zaznaczonaGmina$Wartosc, mean(range(ggplot_build(baza)$data[[1]]$count)), 
                    label = zaznaczonaGmina$Wartosc), colour = "black", angle = 90, vjust = 1, 
                nudge_x = 5, size = 5)
    
    wykres
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
  
  output$wykres_wypozyczenia2 = renderPlot({
    wskaznik <- arrange(polski_srednia_wypozyczenia, nazwa_gminy)
    
    wskaznikWybranyRok <- filter(wypozyczenia_bez_dupl, Rok == input$rok)
    zaznaczonaGmina <- subset(wskaznikWybranyRok, Nazwa == input$gmina)
    
    baza <- ggplot(wskaznikWybranyRok, aes(Wartosc)) +
      geom_area(aes(y = ..count..), stat = "bin", binwidth = 1.5, colour = "#87B4FF", 
                fill = "#87B4FF")
    
    wykres<- baza +
      geom_vline(xintercept = zaznaczonaGmina$Wartosc, linetype = "dashed", color = "#D55E00", size = 0.6) +
      labs(x = "Liczba wypożyczonych książek na 1 czytelnika", y = "Liczba gmin") +
      geom_text(aes(zaznaczonaGmina$Wartosc, mean(range(ggplot_build(baza)$data[[1]]$count)), 
                    label = zaznaczonaGmina$Wartosc), colour = "black", angle = 90, vjust = 1,
                nudge_x = 1, size = 5)
    
    wykres
    })

})
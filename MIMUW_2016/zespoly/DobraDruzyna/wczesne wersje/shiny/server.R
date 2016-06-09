library(shiny)
library(dplyr)
library(ggplot2)

shinyServer(function(input, output) {
  
  wybranyWskaznik <- reactive({
    if (input$rodzajWskaznika == "wydatki")
      wskaznik <- arrange(polski_srednia_wydatki, nazwa_gminy)
    
    else if (input$rodzajWskaznika == "bezrobocie")
      wskaznik <- arrange(polski_srednia_bezrobocie, nazwa_gminy)
    
    else if (input$rodzajWskaznika == "biblioteki")
      wskaznik <- arrange(polski_srednia_biblioteki, nazwa_gminy)
    
    else if (input$rodzajWskaznika == "czytelnicy")
      wskaznik <- arrange(polski_srednia_czytelnicy, nazwa_gminy)
    
    else if (input$rodzajWskaznika == "wypozyczenia")
      wskaznik <- arrange(polski_srednia_wypozyczenia, nazwa_gminy)
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
        xlab("Suma trzyletnich wydatków na jednego ucznia")
    }
    
    else if (input$rodzajWskaznika == "bezrobocie") {
      wykres <- ggplot(wskaznikWybranyRok, aes(Wartosc, sredni_wynik_matury)) +
        xlab("Liczba bezrobotnych na 1000 osób w wieku produkcyjnym")
    }
    
    else if (input$rodzajWskaznika == "biblioteki") {
      wykres <- ggplot(wskaznikWybranyRok, aes(Wartosc, sredni_wynik_matury)) +
        xlab("Liczba mieszkańców na 1 bibliotekę")
    }
    
    else if (input$rodzajWskaznika == "czytelnicy") {
      wykres <- ggplot(wskaznikWybranyRok, aes(Wartosc, sredni_wynik_matury)) +
        xlab("Liczba czytelników na 1000 mieszkańców")
    }
    
    else if (input$rodzajWskaznika == "wypozyczenia") {
      wykres <- ggplot(wskaznikWybranyRok, aes(Wartosc, sredni_wynik_matury)) +
        xlab("Liczba wypożyczonych książek na 1 czytelnika")
    }
    
    wykres <- wykres + geom_point(colour = "lightblue") +
      geom_point(data = zaznaczonaGmina, colour = "#D55E00", size = 5) +
      geom_text(data = zaznaczonaGmina, label = input$gmina, vjust = 2,
                colour = "#D55E00", size = 7) +
      ylab("Średni wynik matury")
    
    wykres
  })
  
  output$opis = renderText({
    if (input$rodzajWskaznika == "wydatki") {
      "TU pojawi się jakiś opis"
    }
  })
  
  output$wykres2 = renderPlot({
    wskaznik <- wybranyWskaznik()
    
   if (input$rodzajWskaznika == "wydatki") {
      wskaznikWybranyRok <- filter(wskaznik, rok == input$rok)
      zaznaczonaGmina <- subset(wskaznikWybranyRok, nazwa_gminy == input$gmina)
      
      baza <- ggplot(wskaznikWybranyRok, aes(suma_na_licealiste)) +
        geom_area(aes(y = ..count..), stat = "bin", binwidth = 10000, colour = "lightblue", fill = "lightblue")
      
      
      
      wykres <- baza +
        geom_vline(xintercept = zaznaczonaGmina$suma_na_licealiste, linetype = "dashed", color = "#D55E00", size = 0.8) +
        labs(x = "Suma wydatków na 1 licealistę", y = "Liczba gmin") +
        geom_text(aes(zaznaczonaGmina$suma_na_licealiste, mean(range(ggplot_build(baza)$data[[1]]$count)),
                      label = zaznaczonaGmina$suma_na_licealiste), colour = "black", angle = 90, vjust = 1, nudge_x = 1)
    }
    
    else if (input$rodzajWskaznika == "bezrobocie") {
      wskaznikWybranyRok <- filter(bezrobocie_bez_dupl, Rok == input$rok)
      zaznaczonaGmina <- subset(wskaznikWybranyRok, Nazwa == input$gmina)
      
      baza <- ggplot(wskaznikWybranyRok, aes(Wartosc)) +
        geom_area(aes(y = ..count..), stat = "bin", binwidth = 5, colour = "lightblue", 
                  fill = "lightblue")
      
      wykres<- baza +
        geom_vline(xintercept = zaznaczonaGmina$Wartosc, linetype = "dashed", color = "#D55E00", size = 0.8) +
        labs(x = "Liczba bezrobotnych na 1000 osób w wieku produkcyjnym", y = "Liczba gmin") +
        geom_text(aes(zaznaczonaGmina$Wartosc, mean(range(ggplot_build(baza)$data[[1]]$count)), 
          label = zaznaczonaGmina$Wartosc), colour = "black", angle = 90, vjust = 1, nudge_x = 1)
    }
    
    else if (input$rodzajWskaznika == "biblioteki") {
      wskaznikWybranyRok <- filter(ludnosc_na_biblioteke_bez_dupl, Rok == input$rok)
      zaznaczonaGmina <- subset(wskaznikWybranyRok, Nazwa == input$gmina)
      
      baza <- ggplot(wskaznikWybranyRok, aes(Wartosc)) +
        geom_area(aes(y = ..count..), stat = "bin", binwidth = 500, colour = "lightblue", 
                  fill = "lightblue")
      
      wykres<- baza +
        geom_vline(xintercept = zaznaczonaGmina$Wartosc, linetype = "dashed", color = "#D55E00", size = 0.8) +
        labs(x = "Liczba mieszkańców na 1 bibliotekę", y = "Liczba gmin") +
        geom_text(aes(zaznaczonaGmina$Wartosc, mean(range(ggplot_build(baza)$data[[1]]$count)),
                    label = zaznaczonaGmina$Wartosc), colour = "black", angle = 90, vjust = 1,
                    nudge_x = 200, size = 5)
    }
    
    else if (input$rodzajWskaznika == "czytelnicy") {
      wskaznikWybranyRok <- filter(czytelnicy_bez_dupl, Rok == input$rok)
      zaznaczonaGmina <- subset(wskaznikWybranyRok, Nazwa == input$gmina)
      
      baza <- ggplot(wskaznikWybranyRok, aes(Wartosc)) +
        geom_area(aes(y = ..count..), stat = "bin", binwidth = 15, colour = "lightblue",
                  fill = "lightblue")
      
      wykres<- baza +
        geom_vline(xintercept = zaznaczonaGmina$Wartosc, linetype = "dashed", color = "#D55E00",
                   size = 0.6) +
        labs(x = "Liczba czytelników na 1000", y = "Liczba gmin") +
        geom_text(aes(zaznaczonaGmina$Wartosc, mean(range(ggplot_build(baza)$data[[1]]$count)), 
                      label = zaznaczonaGmina$Wartosc), colour = "black", angle = 90, vjust = 1, 
                      nudge_x = 5, size = 5)
    }
    
    else if (input$rodzajWskaznika == "wypozyczenia") {
      wskaznikWybranyRok <- filter(wypozyczenia_bez_dupl, Rok == input$rok)
      zaznaczonaGmina <- subset(wskaznikWybranyRok, Nazwa == input$gmina)
      
      baza <- ggplot(wskaznikWybranyRok, aes(Wartosc)) +
        geom_area(aes(y = ..count..), stat = "bin", binwidth = 1.5, colour = "lightblue", 
                  fill = "lightblue")
      
      wykres<- baza +
        geom_vline(xintercept = zaznaczonaGmina$Wartosc, linetype = "dashed", color = "#D55E00", size = 0.6) +
        labs(x = "Liczba wypożyczonych książek na 1 czytelnika", y = "Liczba gmin") +
        geom_text(aes(zaznaczonaGmina$Wartosc, mean(range(ggplot_build(baza)$data[[1]]$count)), 
                      label = zaznaczonaGmina$Wartosc), colour = "black", angle = 90, vjust = 1,
                      nudge_x = 1, size = 5)
    }
    
    wykres
  })
  
})

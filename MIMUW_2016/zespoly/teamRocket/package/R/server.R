library(ggplot2)
library(shiny)
library(dplyr)

pobierz.nazwy.gmin <- function(ramka) {
  sort(ramka$gmina_szkoly)
}

pobierz.nazwy.szkol <- function(ramka, gmina) {
  matury.dla.gmin <- matury %>% filter(gmina_szkoly == gmina)
  sort(matury.dla.gmin$nazwa_szkoly)
}

pobierz.id.szkoly <- function(ramka, szkola) {
  wiersz <- ramka %>% filter(nazwa_szkoly == szkola) %>% head(1)
  wiersz$id_szkoly
}

pobierz.opisy.wyznacznikow <- function(wyznaczniki) {
  for (w in wyznaczniki)
    opisy <- c(opisy, w[[2]])
  opisy
}

pobierz.wyznacznik <- function(wyznaczniki, opis) {
  for (w in wyznaczniki)
    if (w[[2]] == opis)
      return(w[[1]])
}

#'Serwer aplikacji shiny
maturiser.server <- function(matury, wyznaczniki) {

  # do testow
  #wyznaczniki <- list(
  #  c("m_inf_p", "Informatyka - podstawa"),
  #  c("m_inf_r", "Informatyka - rozszerzenie")
  #)

  ilosc.wyznacznikow <- length(wyznaczniki)
  
  shinyServer(function(input, output) {
    output$gmina <- renderUI({
      gminy <- pobierz.nazwy.gmin(matury)
      selectInput(inputId = "gmina",
                  label = "Wybierz gminę",
                  choices = gminy,
                  selected = "")
    })
  
    output$szkola <- renderUI({
      szkoly <- pobierz.nazwy.szkol(matury, input$gmina)
      selectInput(inputId = "szkola", 
                  label = "Wybierz szkołę",
                  choices = szkoly,
                  selected = "")
    })
    
    output$wyznacznik <- renderUI({
      opisy.wyznacznikow <- pobierz.opisy.wyznacznikow(wyznaczniki)
      selectInput(inputId = "wyznacznik", 
                  label = "Wybierz wyznacznik",
                  choices = opisy.wyznacznikow,
                  width = "100%",
                  selected = "")
    })
    
    output$wykres <- renderPlot(
      maturiser:::generuj.wykres(matury,
                                 pobierz.wyznacznik(wyznaczniki, input$wyznacznik),
                                 pobierz.id.szkoly(matury, input$szkola),
                                 input$wyznacznik)
    )
  
    # output$wykresy <- renderUI({
    #   lista.wykresow <- lapply(1:ilosc.wyznacznikow, function(i) {
    #     nazwa.wykresu <- paste("wykres", i, sep="")
    #     plotOutput(nazwa.wykresu)
    #   })
    #   
    #   # Convert the list to a tagList - this is necessary for the list of items
    #   # to display properly.
    #   do.call(tagList, lista.wykresow)
    # })
    # 
    # # Iterujemy po liĹ›cie wyznacznikĂłw, generujÄ…c wykres dla kaĹĽdego z nich
    # for(i in 1:ilosc.wyznacznikow) {
    #   local({
    #     tmp.i <- i
    #     nazwa.wykresu <- paste("wykres", tmp.i, sep="")
    #     
    #     output[[nazwa.wykresu]] <- renderPlot({
    #       maturiser:::generuj.wykres(matury,
    #                                  wyznaczniki[[tmp.i]][[1]],
    #                                  pobierz.id.szkoly(matury, input$szkola),
    #                                  wyznaczniki[[tmp.i]][[2]])
    #     })
    #   })
    # }
  }
)}

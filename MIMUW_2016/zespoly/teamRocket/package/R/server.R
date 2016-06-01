#library(ggplot2)
#library(shiny)
#library(dplyr)

pobierz.nazwy.gmin <- function(ramka) {
  sort(ramka$gmina_szkoly)
}

pobierz.szkoly <- function(ramka) {
  ramka %>% dplyr::select(id_szkoly, nazwa_szkoly, gmina_szkoly) %>%
    dplyr::distinct(id_szkoly) %>% dplyr::arrange(nazwa_szkoly)
}

szkoly.z.gminy <- function(lista.szkol, gmina) {
  lista.szkol %>%
    sort(dplyr::filter(nazwa_gminy == gmina)$nazwa_szkoly)
}

pobierz.id.szkoly <- function(lista.szkol, szkola) {
  wiersz <- ramka %>% dplyr::filter(nazwa_szkoly == szkola) %>% utils::head(1)
  wiersz$id_szkoly
}

pobierz.opisy.wyznacznikow <- function(wyznaczniki) {
  opisy <- NULL
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
  ilosc.wyznacznikow <- length(wyznaczniki)
  gminy <- pobierz.nazwy.gmin(matury)
  szkoly <- pobierz.szkoly(matury)
  opisy.wyznacznikow <- pobierz.opisy.wyznacznikow(wyznaczniki)
  
  shiny::shinyServer(function(input, output) {

    output$gmina <- shiny::renderUI({
      shiny::selectInput(inputId = "gmina",
                  label = "Wybierz gminę",
                  choices = gminy,
                  selected = dplyr::first(gminy))
    })
  
    if(is.null(input$gmina))
      input$gmina <- dplyr::first(gminy)
      
    szk.gm <- szkoly.z.gminy(szkoly, input$gmina)
    output$szkola <- shiny::renderUI({
      shiny::selectInput(inputId = "szkola", 
                  label = "Wybierz szkołę",
                  choices = szk.gm,
                  selected = dplyr::first(szk.gm))
    })
    
    output$wyznacznik <- shiny::renderUI({
      shiny::selectInput(inputId = "wyznacznik", 
                  label = "Wybierz wyznacznik",
                  choices = opisy.wyznacznikow,
                  width = "100%",
                  selected = dplyr::first(opisy.wyznacznikow))
    })
    if(is.null(input$wyznacznik))
      input$wyznacznik <- dplyr::first(opisy.wyznacznikow)
    output$wykres <- shiny::renderPlot(
      generuj.wykres(matury,
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

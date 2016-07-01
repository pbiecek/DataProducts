#library(ggplot2)
#library(shiny)
#library(dplyr)

pobierz.nazwy.gmin <- function(ramka) {
  sort(ramka$gmina_szkoly)
}

pobierz.szkoly <- function(ramka) {
  ramka %>% dplyr::select(id_szkoly, nazwa_szkoly, gmina_szkoly) %>%
    dplyr::distinct(id_szkoly, .keep_all = TRUE) %>% dplyr::arrange(nazwa_szkoly)
}

szkoly.z.gminy <- function(lista.szkol, gmina) {
  dane <- dplyr::filter(lista.szkol, gmina_szkoly == gmina)
  szkoly <- dane$id_szkoly
  names(szkoly) <- dane$nazwa_szkoly
  return(szkoly)
  # na razie brak sortowania szkol
  # sort(dplyr::filter(lista.szkol, gmina_szkoly == gmina)$nazwa_szkoly)
}

pobierz.opisy.wyznacznikow <- function(wyznaczniki) {
  opisy <- c()
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
#'@param matury tabela zawierająca wyniki matur i wyliczonych wyznaczników
#'@param wyznaczniki lista par postaci ("nazwa_kolumny", "opis_wyznacznika")
#'Wymienione wyznaczniki zostaną przedstawione w aplikacji
maturiser.server <- function(matury, wyznaczniki) {
  # Dzięki temu zabiegowi dane te będą dla serwera "stałymi globalnymi"
  gminy <- pobierz.nazwy.gmin(matury)
  szkoly <- pobierz.szkoly(matury)
  opisy.wyznacznikow <- pobierz.opisy.wyznacznikow(wyznaczniki)
  
  shiny::shinyServer(function(input, output) {

    ##############################################################################
    # ZAKLADKA 1
    ##############################################################################
    output$gmina <- shiny::renderUI({
      shiny::selectInput(inputId = "gmina",
                  label = "Wybierz gminę",
                  choices = gminy)
    })
   
    output$szkola <- shiny::renderUI({
      szk.gm <- szkoly.z.gminy(szkoly, input$gmina)
      shiny::selectInput(inputId = "szkola", 
                  label = "Wybierz szkołę",
                  choices = szk.gm)
    })
    
    output$wyznacznik <- shiny::renderUI({
        shiny::selectInput(inputId = "wyznacznik", 
                  label = "Wybierz wyznacznik",
                  choices = opisy.wyznacznikow,
                  width = "100%")
    })
    
    output$wykres <- shiny::renderPlot({
      if (!is.null(input$szkola) & input$szkola != "")
        generuj.wykres(matury,
                     pobierz.wyznacznik(wyznaczniki, input$wyznacznik),
                     input$szkola,
                     input$wyznacznik)
    })
    
    output$instrukcja <- shiny::renderText({
      if (is.null(input$szkola) | input$szkola == "")
        '<p style="color:green; font-size:16px;">Obsługa aplikacji jest niezwykle prosta!</p><br/><p style="color:green; font-size:16px;">W panelu bocznym wybierz <b>gminę</b> oraz interesującą Cię <b>szkołę</b>. Następnie wybierz <b>wyznacznik</b> - kryterium, ze względu na które chcesz porównać wyniki maturalne uczniów wybranej placówki. Możliwe jest filtrowanie gmin/szkół/wyznaczników poprzez wpisywanie części nazw w polach wyboru.</p><br/><p style="color:green; font-size:16px;">Przed Twoimi oczami ukaże się wykres przedstawiający liczbę uczniów, dla których wyznacznik ma określoną wartość.</p>'
    })
    
    
    ##############################################################################
    # ZAKLADKA 2
    ##############################################################################
    output$gmina1 <- shiny::renderUI({
      shiny::selectInput(inputId = "gmina1",
                         label = "Wybierz gminę",
                         choices = gminy)
    })
    
    output$szkola1 <- shiny::renderUI({
      szk.gm <- szkoly.z.gminy(szkoly, input$gmina1)
      shiny::selectInput(inputId = "szkola1", 
                         label = "Wybierz szkołę",
                         choices = szk.gm)
    })
    
    output$gmina2 <- shiny::renderUI({
      shiny::selectInput(inputId = "gmina2",
                         label = "Wybierz gminę",
                         choices = gminy)
    })
    
    output$szkola2 <- shiny::renderUI({
      szk.gm <- szkoly.z.gminy(szkoly, input$gmina2)
      shiny::selectInput(inputId = "szkola2", 
                         label = "Wybierz szkołę",
                         choices = szk.gm)
    })
    
    
    output$wyznacznik2 <- shiny::renderUI({
      shiny::selectInput(inputId = "wyznacznik2", 
                         label = "Wybierz wyznacznik",
                         choices = opisy.wyznacznikow,
                         width = "100%")
    })
    
    output$wykres2 <- shiny::renderPlot({
      if (!is.null(input$szkola1) & input$szkola1 != "")
        if (!is.null(input$szkola2) & input$szkola2 != "")
          generuj.wykres.porownaj(matury,
                       pobierz.wyznacznik(wyznaczniki, input$wyznacznik2),
                       input$szkola1,
                       input$szkola2,
                       input$wyznacznik2)
    })
    
    output$instrukcja2 <- shiny::renderText({
      if (is.null(input$szkola1) | input$szkola1 == "" | is.null(input$szkola2) | input$szkola2 == "")
        '<p style="color:green; font-size:16px;">Obsługa aplikacji jest niezwykle prosta!</p><br/><p style="color:green; font-size:16px;">W panelu bocznym wybierz dwie placówki, które chcesz porównać. W celu wskazania placówki najpierw wybierz <b>gminę</b>, potem interesującą Cię <b>szkołę</b>. Następnie ustaw <b>wyznacznik</b> - kryterium, ze względu na które chcesz porównać wyniki maturalne uczniów wybranych placówek. Możliwe jest filtrowanie gmin/szkół/wyznaczników poprzez wpisywanie części nazw w polach wyboru.</p><br/><p style="color:green; font-size:16px;">Przed Twoimi oczami ukaże się wykres przedstawiający liczbę uczniów, dla których wyznacznik ma określona wartość.</p>'
    })
  })
}


# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)
library(dplyr)
library(ZPD)
library(ggplot2)
library(scales)
library(tidyr)
library(corrplot)

source("./pobierz_pytania.R")

load("./data.RData")

egzaminy <<- dane@zapisane_testy
egzaminy_poprz <<- dane@wyniki_po_egz %>%
  select(rok, rodzaj_egzaminu, czesc_egzaminu, rodzaj_poprzedni, czesc_poprzedni) %>%
  distinct()
egzaminy_poprz$nr <<- 1:nrow(egzaminy_poprz)

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

numer_kryterium <- function(nr_kryterium, nr_pytania) {
  if(is.na(nr_kryterium) || nr_kryterium == "")
    nr_pytania
  else
    paste(nr_pytania, nr_kryterium, sep=":")
}

dane_poprzedni <- function(nr, poziom = "kry", nr_arkusza) {
  egz_p = egzaminy_poprz[nr,]
  
  wyniki_egz = dane@wyniki_po_egz %>%
    inner_join(egz_p) %>%
    inner_join(dane@kryteria, by = c("id_kryterium" = "id"))
  
  if (length(nr_arkusza) != 0 && nr_arkusza != 0) {
    ark = dane@typy_testow[nr_arkusza,]
    wyniki_egz = wyniki_egz %>%
      inner_join(dane@kryteria_testy, by = c("id_kryterium" = "id")) %>%
      inner_join(ark, by = c("id_testu" = "id"))
      return(dane_poprzedni_arkusz(wyniki_egz, poziom))
  } else {
    return(dane_poprzedni_wszystko(wyniki_egz, poziom))
  }
}

dane_poprzedni_wszystko <- function(wyniki_egz, poziom) {
  if (poziom == "pyt") {
    wyniki_egz = wyniki_egz %>%
      group_by(poprzedni_wynik, id_pytania, liczba) %>%
      summarise_each(funs(sum), wynik, max_punktow) %>%
      rename(id = id_pytania)
    n_poziom <- "id pytania"
  } else if (poziom == "wia") {
    wyniki_egz = wyniki_egz %>%
      group_by(poprzedni_wynik, id_wiazki, liczba) %>%
      summarise_each(funs(sum), wynik, max_punktow) %>%
      rename(id = id_wiazki)
    n_poziom <- "id wiązki"
  } else {
    wyniki_egz = wyniki_egz %>%
      rename(id = id_kryterium)
    n_poziom <- "id kryterium"
  }
  
  wyniki_egz$wynik = wyniki_egz$wynik / wyniki_egz$max_punktow
  wyniki_egz$id_factor = factor(wyniki_egz$id)
  return(list(wyniki_egz, n_poziom))
}

dane_poprzedni_arkusz <- function(wyniki_egz, poziom) {
  wyniki_egz <- wyniki_egz %>%
    arrange(numer_pytania)
  
  if (poziom == "kry") {
    wyniki_egz = wyniki_egz %>%
      rename(id = id_kryterium)
    n_poziom <- "numer kryterium"
    wyniki_egz$id_factor = apply(wyniki_egz, 1, function(x) {
        numer_kryterium(x["numer_kryterium"], x["numer_pytania"])
      }) %>%
      as.character()
  } else if (poziom == "pyt") {
    wyniki_egz = wyniki_egz %>%
      group_by(poprzedni_wynik, id_pytania, liczba, numer_pytania) %>%
      summarise_each(funs(sum), wynik, max_punktow) %>%
      rename(id = id_pytania)
    n_poziom <- "numer pytania"
    wyniki_egz$id_factor = as.character(wyniki_egz$numer_pytania)
  } else if (poziom == "wia") { # FIXME - jak dobrze zrobic numery wiazek?
    wyniki_egz = wyniki_egz %>%
      group_by(poprzedni_wynik, id_wiazki, liczba) %>%
      summarise_each(funs(sum), wynik, max_punktow) %>%
      rename(id = id_wiazki)
    n_poziom <- "numer wiązki"
    wyniki_egz$id_factor = as.character(wyniki_egz$id)
  }
  
  wyniki_egz$wynik = wyniki_egz$wynik / wyniki_egz$max_punktow
  nry = wyniki_egz %>% select(id_factor) %>% distinct()
  wyniki_egz$id_factor <- factor(wyniki_egz$id_factor, levels=gtools::mixedsort(nry$id_factor))
  return(list(wyniki_egz, n_poziom))
}

dane_poprzedni_jedno <- function(poprzednie, p_id) {
  return(poprzednie %>% filter(id == p_id))
}

rysuj_wykres_poprzedni <- function(dane, nazwa_x) {
  ggplot(dane, aes(x = id_factor, y = wynik)) +
    geom_point(aes(color = poprzedni_wynik, size = liczba)) +
    labs(x = nazwa_x, y = "wynik", color = "Poprzedni wynik", size = "Liczba uczniów z poprzednim wynikiem") +
    scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}

rysuj_wykres_poprzedni_jedno <- function(dane, nazwa) {
  ggplot(dane, aes(x = poprzedni_wynik, y = wynik)) +
  geom_point(aes(size = liczba)) +
  labs(x = "poprzedni", y = "wynik", title = nazwa, size = "Liczba uczniów z poprzednim wynikiem") +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  
}

arkusze_zawierajace <- function(n_id, poziom)
{
  if (poziom == "kry") {
    arkusze <- dane@kryteria %>%
      filter(id == n_id)
  } else if (poziom == "pyt") {
    arkusze <- dane@kryteria %>%
      filter(id_pytania == n_id)
  } else {
    arkusze <- dane@kryteria %>%
      filter(id_wiazki == n_id)
  }
  arkusze <- arkusze %>%
    inner_join(dane@kryteria_testy) %>%
    inner_join(dane@typy_testow, by = c("id_testu" = "id")) %>%
    distinct(id_testu)
  View(arkusze)
  wynik <- arkusze %>% select(arkusz) %>% rename(arkusze = arkusz)
  
  if (poziom == "kry") {
    wynik$numery = apply(arkusze, 1, function(x){
        numer_kryterium(x["numer_kryterium"], x["numer_pytania"])
      })
  } else if (poziom == "pyt") {
    wynik$numery = arkusze$numer_pytania
  } else {
    wynik$numery = arkusze$id_wiazki
  }
  return(wynik)
}

arkusz_linia <- function(arkusz, czesc) {
  paste(arkusz, " [", czesc, "]", "<br/>", sep = "")
}

tekst_arkuszy <- function(dane) {
  linie <- apply(dane, 1, function(x) {
    arkusz_linia(x["arkusze"], x["numery"])
    }) %>% as.data.frame()
  r <- apply(linie, 2, paste, collapse="")
  r <-paste("Element występuje w testach:<br/>", r)
  renderUI(HTML(r))
}

generuj_link <- function(arkusz, klucz) {
  return(paste(c("http://zpd.ibe.edu.pl/pobierzTresc.php?arkusz=", arkusz, "&typ=pdf&klucz=",klucz)
               , collapse = ''))
}

shinyServer(function(input, output) {
  
  # dane do wykresu poprzednich kryteriow
  poprzedni_data <- data.frame()
  # poziom tych danych (kryteria, pytania, wiązki)
  poprzedni_poziom <- character(0)
  # dane do wykresu pojedynczego elementu
  poprzedni_data_jedno <- data.frame()
  
  output$egz_wybor <- renderUI({
    nazwy_egzaminow <- as.list(1:nrow(egzaminy))
    names(nazwy_egzaminow) <- apply(egzaminy, 1, (function(x) paste(x, collapse=" ")))
    selectInput("egzamin_nr", label = "Egzamin:",
                choices = nazwy_egzaminow)
  })
  
  output$egz_wybor_poprz <- renderUI({
    egz_o <- egzaminy[input$egzamin_nr,]
    egz_p <- egzaminy_poprz %>% inner_join(egz_o)
      
    poprz_nazwy <- as.list(egz_p$nr)
    egz_p <- egz_p %>% select(rodzaj_poprzedni, czesc_poprzedni)
    names(poprz_nazwy) <- apply(egz_p, 1, (function(x) paste(x, collapse=" ")))
    
    selectInput("nr_poprzedni", label = "Poprzedni egzamin:",
                choices = poprz_nazwy)
  })
  
  output$arkusz_wybor <- renderUI({
    egz_o <- egzaminy[input$egzamin_nr,]
    ark <- dane@typy_testow
    ark$nr <- 1:nrow(ark) 
    ark <- ark %>% inner_join(egz_o)
    
    ark_nazwy <- c(0, as.list(ark$nr))
    names(ark_nazwy) <- c("Wszystkie", as.list(ark$arkusz))
    selectInput("nr_arkusza", label = "Arkusz:",
                choices = ark_nazwy)
  })
  
  output$help_button <- renderUI({
    tags$button("Click me", on_click=)
  })
  
  observeEvent(list(input$nr_poprzedni, input$poziom, input$nr_arkusza), {
    d <- dane_poprzedni(input$nr_poprzedni, input$poziom, input$nr_arkusza)
    poprzedni_data <<- d[[1]]
    poprzedni_poziom <<- d[[2]]
    output$poprz_plot <- renderPlot(rysuj_wykres_poprzedni(d[[1]], d[[2]]))
    if(!is.null(input$nr_arkusza) && input$nr_arkusza != 0) {
      output$link_do_arkusza <- renderUI(
        tags$a(href = generuj_link(dane@typy_testow[input$nr_arkusza,]$arkusz, 0), "Test")
      )
    } else {
      output$link_do_arkusza <- renderUI(tags$div(""))
    }
    if(!is.null(input$nr_arkusza) && input$nr_arkusza != 0) {
      output$link_do_klucza <- renderUI(
        tags$a(href = generuj_link(dane@typy_testow[input$nr_arkusza,]$arkusz, 1), "Klucz")
      )
    } else {
      output$link_do_klucza <- renderUI(tags$div(""))
    }
  })
  
  observeEvent(input$poprz_click, {
    dane <- nearPoints(poprzedni_data, xvar="id_factor", yvar="wynik", input$poprz_click, threshold = 15, maxpoints = 1)
    if (nrow(dane) == 0)
      return()
    
    poprzedni_data_jedno <<- dane_poprzedni_jedno(poprzedni_data, dane$id)
    output$poprz_plot_jedno <- renderPlot(rysuj_wykres_poprzedni_jedno(poprzedni_data_jedno, poprzedni_poziom))
    output$arkusze_zawierajace <- tekst_arkuszy(arkusze_zawierajace(dane$id, input$poziom))
    })
})

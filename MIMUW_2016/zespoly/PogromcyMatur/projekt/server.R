
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

dane_poprzedni <- function(nr, poziom = "kry") {
  egz_p = egzaminy_poprz[nr,]
  
  wyniki_egz = dane@wyniki_po_egz %>%
    inner_join(egz_p)
  if (poziom == "pyt") {
    wyniki_egz = wyniki_egz %>%
      inner_join(dane@kryteria, by = c("id_kryterium" = "id")) %>%
      group_by(poprzedni_wynik, id_pytania, liczba) %>%
      summarise_each(funs(sum), wynik, max_punktow) %>%
      rename(id = id_pytania)
    n_poziom <- "id pytania"
  } else if (poziom == "wia") {
    wyniki_egz = wyniki_egz %>%
      inner_join(dane@kryteria, by = c("id_kryterium" = "id")) %>%
      group_by(poprzedni_wynik, id_wiazki, liczba) %>%
      summarise_each(funs(sum), wynik, max_punktow) %>%
      rename(id = id_wiazki)
    n_poziom <- "id wiązki"
  } else {
    wyniki_egz = wyniki_egz %>%
      inner_join(dane@kryteria, by = c("id_kryterium" = "id")) %>%
      rename(id = id_kryterium)
    n_poziom <- "id kryterium"
  }
  
  wyniki_egz$wynik = wyniki_egz$wynik / wyniki_egz$max_punktow
  wyniki_egz$id_factor = factor(wyniki_egz$id)
  return(list(wyniki_egz, n_poziom))
}

dane_poprzedni_jedno <- function(poprzednie, p_id) {
  return(poprzednie %>% filter(id == p_id))
}

rysuj_wykres_poprzedni <- function(dane, nazwa_x) {
  ggplot(dane, aes(x = id_factor, y = wynik)) +
    geom_point(aes(color = poprzedni_wynik, size = liczba)) +
    labs(x = nazwa_x, y = "poprzedni wynik") +
    ylim(0, 1)
}

rysuj_wykres_poprzedni_jedno <- function(dane, nazwa) {
  ggplot(dane, aes(x = poprzedni_wynik, y = wynik)) +
  geom_point(aes(size = liczba)) +
  labs(x = "poprzedni wynik", y = "wynik", title = nazwa) +
  ylim(0, 1)
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
  
  observeEvent(list(input$nr_poprzedni, input$poziom), {
    d <- dane_poprzedni(input$nr_poprzedni, input$poziom)
    poprzedni_data <<- d[[1]]
    poprzedni_poziom <<- input$poziom
    output$poprz_plot <- renderPlot(rysuj_wykres_poprzedni(d[[1]], d[[2]]))
  })
  
  observeEvent(input$poprz_click, {
    dane <- nearPoints(poprzedni_data, xvar="id_factor", yvar="wynik", input$poprz_click, threshold = 15, maxpoints = 1)
    if (nrow(dane) == 0)
      return()
    
    poprzedni_data_jedno <<- dane_poprzedni_jedno(poprzedni_data, dane$id)
    output$poprz_plot_jedno <- renderPlot(rysuj_wykres_poprzedni_jedno(poprzedni_data_jedno, poprzedni_poziom))
    })
})

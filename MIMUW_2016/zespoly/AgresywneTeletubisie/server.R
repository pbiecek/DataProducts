## Dependencies

library(shiny)
library(dplyr)
library(maptools)
library(RColorBrewer)
library(classInt)

## Load cached data

## This is what's in Dane_z_bazy.RData
# conn <- polacz()
# 
# if (!exists('wartosci_wskaznikow')) {
#   wartosci_wskaznikow <- pobierz_wartosci_wskaznikow(conn) %>% collect
# }
# if (!exists('wskazniki')) {
#   wskazniki <- pobierz_wskazniki(conn) %>% collect
# }
# if (!exists('szkoly')) {
#   szkoly <- pobierz_szkoly(conn) %>% collect
# }
# 
# rozlacz()

baza_danych <- './Dane_z_bazy.RData'

## Dane GUS 

dane_powiatowe <- read.csv("dane_powiatowe_2015.csv", sep=";", dec=",", fileEncoding = 'Windows-1252', header=TRUE)

## Check if we have data frames we need. If not - load them from cached RData file

lista <- c("szkoly","wartosci_wskaznikow","wskazniki")
if (!all(sapply(lista,exists))) {
  load(baza_danych)
}

## ?? Hacky solution ??
## Fix values in wartosci_wskaznikow so they match wskazniki

wartosci_wskaznikow['wskaznik'] <- wartosci_wskaznikow %>%
  select(wskaznik) %>%
  sapply(function(x) gsub('paou_m_mat_var','paou_m_mat_war', x))

# dane geograficzne jednostek administracyjnych
# TODO: powinniśmy to pobrać raz, żeby nie zamulać ładowania aplikacji

lista_shapes <- c("pl", "woj", "pow", 'gmn', 'srodki_powiatow', 'srodki_gmin', 'srodki_wojewodztw')

## URL for shapes data ftp://91.223.135.109/prg/jednostki_administracyjne.zip

geo_shapes <- './Jednostki_administracyjne.RData'

if (!all(sapply(lista,exists))) {
  load(geo_shapes)
}

# Code used to generate above
# pl <- readShapePoly("panstwo/panstwo.shp")
# woj <- readShapePoly("wojewodztwa/wojewodztwa.shp")
# pow <- readShapePoly("powiaty/powiaty.shp")
# gmn <- readShapePoly("gminy/gminy.shp")
# srodki_powiatow <- coordinates(pow)
# srodki_wojewodztw <- coordinates(woj)
# srodki_gmin <- coordinates(gmn)

# testowe dane do generowania mapy prezentującej stopę bezrobocia 
# TODO: zamienić na generowanie mapy przedstawiają wyniki egzaminów

powiatowe_wskazniki <- wartosci_wskaznikow %>% filter(poziom_agregacji=='powiat')
powiatowe_wskazniki['jpt_kod_je'] <- powiatowe_wskazniki %>% select(teryt_jst) %>%
  unlist(use.names=FALSE) %>%
  gsub('(\\d{2}$)','',.,perl=TRUE) %>% 
  as.integer

powiatowe_wskazniki_light <- powiatowe_wskazniki %>% select(wskaznik,rok,srednia,jpt_kod_je) %>%
  inner_join(wskazniki) %>%
  distinct(wskaznik,srednia) %>%
  select(typ_szkoly,czesc_egzaminu,rok,srednia,jpt_kod_je)

## Remove as we will not use it further

rm(powiatowe_wskazniki)


## Create voievodship list

wojewodztwa <- szkoly %>% select(wojewodztwo_szkoly) %>% unique %>% na.omit %>% unlist(use.names=FALSE)

## Create school type list. Remove undefined schools from the choice list (only 6 schools, shouldn't be an issue)

rodzaj_szkoly <- szkoly %>% filter(typ_szkoly!='ZZ') %>% select(typ_szkoly) %>% distinct %>% unlist

## Create names for list of examination

names(rodzaj_szkoly) <- rodzaj_szkoly


## Make a list assigning exam type to each school type

typy_egzaminow <- lapply(rodzaj_szkoly,
                         function(x) wskazniki %>%
                           filter(typ_szkoly==x) %>%
                           select(rodzaj_egzaminu) %>%
                           na.omit %>%
                           distinct %>%
                           unlist(use.names=FALSE))


## Create names for UI
## ?? Seems like reusing it like that is dirty and should be redone ??

lista_egzaminow <- lapply(rodzaj_szkoly,
                          function(x) wskazniki %>%
                            filter(typ_szkoly == x) %>%
                            select(czesc_egzaminu) %>%
                            na.omit %>%
                            distinct)

names(rodzaj_szkoly) <- c('gimnazjum', 'szkoła podstawowa', 'liceum ogólnokształcące', 'technikum',
                          'liceum profilowane', 'liceum uzupełniające', 'technikum uzupełniające')

## Create names for maps

rodzaj_egzaminu <- rodzaj_szkoly
names(rodzaj_egzaminu) <- c('egzaminu gimnazjalnego', 'sprawdzianu szóstoklasisty', 'matury', 'matury', 'matury', 'matury', 'matury')

## Helper functions for creating lists for input choices

lista_powiatow <- function(w){
  szkoly %>% filter(wojewodztwo_szkoly == w) %>%
    select(powiat_szkoly) %>% distinct %>% unlist(use.names=FALSE)
}  

lista_gmin <- function(w,p){
  szkoly %>% filter(wojewodztwo_szkoly == w, powiat_szkoly == p) %>%
    select(gmina_szkoly) %>% distinct %>% unlist(use.names=FALSE)
}

## ?? Rewrite this so we can return list of schools at any aggregate level and
## only require the type of school to be specified. In such case give schools from all of Poland
## ?? Some schools have empty string as a name - it results in id of school being given as a name
## should do something about it...

lista_szkol <- function(w,p,g,r){
  l_s <- szkoly %>% filter(wojewodztwo_szkoly==w,powiat_szkoly==p,gmina_szkoly==g,typ_szkoly==r) %>%
    select(nazwa_szkoly,id_szkoly) %>% distinct
  lista_return <- l_s['id_szkoly'] %>% unlist(use.names=FALSE)
  names(lista_return) <- l_s['nazwa_szkoly'] %>% unlist(use.names=FALSE)
  lista_return
}

## Generuje liste szkol pos sredniej z wybranego egzaminu z wybranego zakresu lat
## (wyniki z poszczegolnych lat tez sa usredniane)

lista_po_sredniej <- function(id, rodzaj, czesc, lata){
  
  rw <- ifelse(rodzaj == 'sprawdzian', 'pwe', 'ewd')
  
  wybrane_wskazniki <- wskazniki %>%
    filter(rodzaj_wsk == rw, rodzaj_egzaminu == rodzaj, czesc_egzaminu == czesc)
  
  wybrane_szkoly <- szkoly %>%
    filter(id_szkoly %in% id, rok %in% lata)
  
  wybrane_dane <- wartosci_wskaznikow %>%
    inner_join(wybrane_szkoly) %>%
    inner_join(wybrane_wskazniki) %>%
    select(nazwa_szkoly, miejscowosc, czesc_egzaminu, srednia) %>%
    group_by(nazwa_szkoly) %>%
    mutate(srednia=mean(srednia)) %>%
    distinct %>%
    ungroup %>%
    arrange(desc(srednia))
}

## Map creating function
## !! Works only for primary and secondary schools now. Not for high school types

mapa_powiatow <- function(szkola, rok_egzaminu, egzamin=NULL){
  dane_do_mapy <- powiatowe_wskazniki_light %>% filter(typ_szkoly==szkola, rok==rok_egzaminu)
  if(is.null(egzamin)){
    dane_do_mapy <- dane_do_mapy %>%
      group_by(jpt_kod_je, rok) %>%
      mutate(srednia=mean(srednia)) %>%
      ungroup %>%
      select(srednia, jpt_kod_je) %>%
      distinct
  }
  
  nazwa_egzaminu <- rodzaj_egzaminu[rodzaj_egzaminu %in% szkola] %>% names
  tytul_mapy <- paste0('Wyniki ',nazwa_egzaminu,' ',rok_egzaminu,'r.')
  
  przedzialy <- 5
  kolory <- brewer.pal(przedzialy, "BrBG")
  klasy <- classIntervals(dane_do_mapy['srednia'] %>% unlist,
                          n=5, style = "sd", dataPrecision = 0)
  tabela.kolorow <- findColours(klasy, kolory)
  
  plot(pow, lwd = 0.5 , col = tabela.kolorow)
  plot(woj, lwd = 2.5, add = TRUE)
  legend("bottomleft", legend = names(attr(tabela.kolorow, "table")),
         fill = attr(tabela.kolorow, "palette"), cex = 1, bty = "n")
  title(main = tytul_mapy)
}

# Server logic

shinyServer(function(input, output) {
  
  ## Create input lists for UI 
  
  output$wybierz_wojewodztwo <- renderUI({
    selectInput('wojewodztwo', label = ('Województwo'), choices = c(Wybierz = '',wojewodztwa))
  })
  
  output$wybierz_powiat <- renderUI({
    ## Do not run if previous input is not done - stops erros
    if (is.null(input$wojewodztwo) || is.na(input$wojewodztwo) || input$wojewodztwo == '') {
      return()
    }
    else {
      powiaty <- lista_powiatow(input$wojewodztwo)
      selectInput('powiat', label = ('Powiat'), choices = c(Wybierz = '', powiaty))
    }
  })
  
  output$wybierz_gmine <- renderUI({
    ## Do not run if previous input is not done - stops errors
    if (is.null(input$powiat) || is.na(input$powiat) || input$powiat == '') {
      return()
    }
    else { 
      gminy <- lista_gmin(input$wojewodztwo, input$powiat)
      selectInput('gmina', label = ('Gmina'), choices = c(Wybierz = '', gminy))
    }
  })
  
  output$wybierz_rodzaj <- renderUI({
    selectInput('rodzaj', label = ('Rodzaj szkoły'), choices = c(Wybierz = '', rodzaj_szkoly))
  })
  
  output$wybierz_szkole <- renderUI({
    ## Do not run if previous input is not done - stops errors
    if (is.null(input$gmina) || is.na(input$gmina) || input$gmina == '' || is.null(input$rodzaj) ||
        is.na(input$rodzaj) || input$rodzaj == '')
    {
      return()
    }
    else {
      lista_szkol <- lista_szkol(input$wojewodztwo, input$powiat, input$gmina, input$rodzaj)
      selectInput('szkola', label = ('Szkoła'), choices = c(Wybierz = '', lista_szkol))
    }
  })
  
  output$wybierz_czesc_egzaminu <- renderUI({
   
   ## Do not run if previous input is not done - stops errors
    
   if (is.null(input$rodzaj) || is.na(input$rodzaj) || input$rodzaj == '') {
     return()
   }
   else {
      lista_czesci <- lista_egzaminow[[input$rodzaj]]
      selectInput('czesc', label = ('Czesc egzaminu'), choices = c(Wybierz = '', lista_czesci))
    }
  })
  
  output$wybierz_lata <- renderUI({
    ## Different time ranges for different data sets - should probably restrict values after choice of data set
    ## PWE is 2002 - 2013 EWD is 2006 - 2015
    ## !! Temporarily lock top value to 2013
    sliderInput("wybierz_lata", "Wybierz lata", min = 2002, max = 2013, value = c(2002,2013),
                ticks = TRUE, sep = '')
    
  })
  
  output$mapa <- renderPlot({
    if (is.null(input$rodzaj) || is.na(input$rodzaj) || input$rodzaj == '') {
      return()
    }
    else mapa_powiatow(input$rodzaj, max(input$wybierz_lata), input$czesc)
  })
  
  output$wyniki_egzaminu <- renderDataTable({ 
    if (is.null(input$rodzaj) || input$rodzaj == "") {
      return()
    }
    else {
    lista_po_sredniej(input$szkola, typy_egzaminow[[input$rodzaj]], input$czesc, input$wybierz_lata)
    }
  })
})

# library(plyr)
# library(maptools)
# library(RColorBrewer)
# library(classInt)

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

# baza_danych <- './Dane_z_bazy.RData'

# wskazniki_light <- wskazniki %>% distinct(wskaznik,rok_do) %>% select(-id_skali,-skalowanie,-okres)
# wartosci_wskaznikow_all <- wartosci_wskaznikow %>% inner_join(wskazniki_light)
# all_data <- wartosci_wskaznikow_all %>% left_join(szkoly)

## Dane GUS
## dane_powiatowe <- read.csv("dane_powiatowe_2015.csv", sep=";", dec=",", fileEncoding = 'Windows-1252', header=TRUE)

# ## Check if we have data frames we need. If not - load them from cached RData file
#
# lista <- c("szkoly","wartosci_wskaznikow","wskazniki")
# if (!all(sapply(lista,exists))) {
#   load(baza_danych)
# }

# dane geograficzne jednostek administracyjnych
# TODO: powinniśmy to pobrać raz, żeby nie zamulać ładowania aplikacji

## lista_shapes <- c("pl", "woj", "pow", 'gmn', 'srodki_powiatow', 'srodki_gmin', 'srodki_wojewodztw')

## URL for shapes data ftp://91.223.135.109/prg/jednostki_administracyjne.zip

## geo_shapes <- './Jednostki_administracyjne.RData'

## if (!all(sapply(lista_shapes,exists))) {
##  load(geo_shapes)
## }

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

# powiatowe_wskazniki <- wartosci_wskaznikow %>% filter(poziom_agregacji=='powiat')
# powiatowe_wskazniki['jpt_kod_je'] <- powiatowe_wskazniki %>% select(teryt_jst) %>%
#   unlist(use.names=FALSE) %>%
#   gsub('(\\d{2}$)','',.,perl=TRUE) %>%
#   as.integer
#
# powiatowe_wskazniki_light <- powiatowe_wskazniki %>% select(wskaznik,rok,srednia,jpt_kod_je) %>%
#   inner_join(wskazniki) %>%
#   distinct(wskaznik,srednia) %>%
#   select(typ_szkoly,czesc_egzaminu,rok,srednia,jpt_kod_je)

## Remove as we will not use it further

# rm(powiatowe_wskazniki)


# generowanie listy promieni

## promienie <- c(1, 5, 10, 20, 30, 50, 100, 200, 300)

## Create voievodship list

# wojewodztwa <- szkoly %>% select(wojewodztwo_szkoly) %>% unique %>% na.omit %>% unlist(use.names=FALSE)

## Helper functions for creating lists for input choices

# lista_powiatow <- function(w){
#   szkoly %>% filter(wojewodztwo_szkoly == w) %>%
#     select(powiat_szkoly) %>% distinct %>% unlist(use.names=FALSE)
# }
#
# lista_gmin <- function(w,p){
#   szkoly %>% filter(wojewodztwo_szkoly == w, powiat_szkoly == p) %>%
#     select(gmina_szkoly) %>% distinct %>% unlist(use.names=FALSE)
# }

## ?? Rewrite this so we can return list of schools at any aggregate level and
## only require the type of school to be specified. In such case give schools from all of Poland
## ?? Some schools have empty string as a name - it results in id of school being given as a name
## should do something about it...

# lista_szkol <- function(rodzaj_szkoly,wojewodztwo,powiat,gmina){
#
#   l_s <- szkoly %>% filter(typ_szkoly==rodzaj_szkoly)
#
#   if(wojewodztwo!='') l_s <- l_s %>% filter(wojewodztwo_szkoly==wojewodztwo)
#   if(powiat!='') l_s <- l_s %>% filter(powiat_szkoly==powiat)
#   if(gmina!='') l_s <- l_s %>% filter(gmina_szkoly==gmina)
#
#   l_s <- l_s %>% select(nazwa_szkoly,id_szkoly) %>% distinct
#
#   lista_return <- l_s['id_szkoly'] %>% unlist(use.names=FALSE)
#   names(lista_return) <- l_s['nazwa_szkoly'] %>% unlist(use.names=FALSE)
#   lista_return
# }

## Generuje liste szkol po sredniej z wybranego egzaminu z wybranego zakresu lat
## (wyniki z poszczegolnych lat tez sa usredniane) ## SREDNIA NIE DZIALA group by nie grupuje WTF

# lista_po_sredniej <- function(wojewodztwo, powiat, gmina, rodzaj, czesc, lata){
#
#   id <- lista_szkol(rodzaj,wojewodztwo,powiat,gmina)
#
#   all_data %>%
#      filter(id_szkoly %in% id, czesc_egzaminu == czesc, rok %in% lata[1]:lata[2])  %>%
#      group_by(id_szkoly) %>%
#      mutate(srednia=mean(srednia)) %>%
#      ungroup %>%
#      distinct(id_szkoly,srednia) %>%
#      select(-id_szkoly) %>% select(nazwa_szkoly,czesc_egzaminu,srednia) %>%
#      arrange(desc(srednia))
#
# }

## Map creating function
## !! Works only for primary and secondary schools now. Not for high school types

# mapa_powiatow <- function(szkola, rok_egzaminu, egzamin=NULL){
#   dane_do_mapy <- powiatowe_wskazniki_light %>% filter(typ_szkoly==szkola, rok==rok_egzaminu)
#   if(is.null(egzamin)){
#     dane_do_mapy <- dane_do_mapy %>%
#       group_by(jpt_kod_je, rok) %>%
#       mutate(srednia=mean(srednia)) %>%
#       ungroup %>%
#       select(srednia, jpt_kod_je) %>%
#       distinct
#   }
#
#   nazwa_egzaminu <- rodzaj_egzaminu[rodzaj_egzaminu %in% szkola] %>% names
#   tytul_mapy <- paste0('Wyniki ',nazwa_egzaminu,' ',rok_egzaminu,'r.')
#
#   przedzialy <- 5
#   kolory <- brewer.pal(przedzialy, "BrBG")
#   klasy <- classIntervals(dane_do_mapy['srednia'] %>% unlist,
#                           n=5, style = "sd", dataPrecision = 0)
#   tabela.kolorow <- findColours(klasy, kolory)
#
#   plot(pow, lwd = 0.5 , col = tabela.kolorow)
#   plot(woj, lwd = 2.5, add = TRUE)
#   legend("bottomleft", legend = names(attr(tabela.kolorow, "table")),
#          fill = attr(tabela.kolorow, "palette"), cex = 1, bty = "n")
#   title(main = tytul_mapy)
# }

#
# ## Create input lists for UI
#
# output$wybierz_wojewodztwo <- renderUI({
#   selectInput('wojewodztwo', label = ('Województwo'), choices = c(wojewodztwa))
# })
#
# output$wybierz_powiat <- renderUI({
#   ## Do not run if previous input is not done - stops erros
#   if (is.null(input$wojewodztwo) || is.na(input$wojewodztwo) || input$wojewodztwo == '') {
#     return()
#   }
#   else {
#     powiaty <- lista_powiatow(input$wojewodztwo)
#     selectInput('powiat', label = ('Powiat'), choices = c(powiaty))
#   }
# })
#
# output$wybierz_gmine <- renderUI({
#   ## Do not run if previous input is not done - stops errors
#   if (is.null(input$powiat) || is.na(input$powiat) || input$powiat == '') {
#     return()
#   }
#   else {
#     gminy <- lista_gmin(input$wojewodztwo, input$powiat)
#     selectInput('gmina', label = ('Gmina'), choices = c(gminy))
#   }
# })
#
# output$wybierz_rodzaj <- renderUI({
#   selectInput('rodzaj', label = ('Rodzaj szkoły'), choices = c(Wybierz = '', rodzaj_szkoly))
# })
#
# output$wybierz_szkole <- renderUI({
#   ## Do not run if previous input is not done - stops errors
#   if ( is.null(input$rodzaj) || is.na(input$rodzaj) || input$rodzaj == '')
#   {
#     return()
#   }
#   else {
#     lista_szkol <- lista_szkol(input$rodzaj, input$wojewodztwo, input$powiat, input$gmina)
#     selectInput('szkola', label = ('Szkoła'), choices = c(Wybierz = '', lista_szkol))
#   }
# })
#
# output$wybierz_czesc_egzaminu <- renderUI({
#
#  ## Do not run if previous input is not done - stops errors
#
#  if (is.null(input$rodzaj) || is.na(input$rodzaj) || input$rodzaj == '') {
#    return()
#  }
#  else {
#     lista_czesci <- lista_egzaminow[[input$rodzaj]]
#     selectInput('czesc', label = ('Czesc egzaminu'), choices = c(lista_czesci))
#   }
# })
#
# output$wybierz_lata <- renderUI({
#   ## Different time ranges for different data sets - should probably restrict values after choice of data set
#   ## PWE is 2002 - 2013 EWD is 2006 - 2015
#   ## !! Temporarily lock top value to 2013
#   sliderInput("wybierz_lata", "Wybierz lata", min = 2002, max = 2013, value = c(2002,2013),
#               ticks = TRUE, sep = '')
#
# })

#   output$wybierz_promien <- renderUI({
#     promienie_stringi <- paste(promienie, "km", sep=" ")
#     selectInput('wybierz_promien', label = ('Zasięg wyszukiwania'), choices = c(append("Cała Polska", promienie_stringi)))
#   })

# output$wyniki_egzaminu <- renderDataTable({
#   if (is.null(input$rodzaj) || input$rodzaj == "") {
#     return()
#   }
#   else {
#   lista_po_sredniej(input$wojewodztwo, input$powiat, input$gmina, input$rodzaj, input$czesc, input$wybierz_lata)
#   }
# })

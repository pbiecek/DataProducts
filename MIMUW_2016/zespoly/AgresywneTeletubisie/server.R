minimum_lon <- all_data_geo %>% filter(!is.na(lat)) %>% distinct(id_szkoly) %>% select(lon) %>% min
maximum_lon <- all_data_geo %>% filter(!is.na(lat)) %>% distinct(id_szkoly) %>% select(lon) %>% max
minimum_lat <- all_data_geo %>% filter(!is.na(lat)) %>% distinct(id_szkoly) %>% select(lat) %>% min
maximum_lat <- all_data_geo %>% filter(!is.na(lat)) %>% distinct(id_szkoly) %>% select(lat) %>% max
markers_palette <- colorNumeric('RdYlGn', domain = NULL)

shinyServer(function(input, output, session) {
  rodzaj_szkoly <- all_data_geo %>% filter(typ_szkoly!='ZZ') %>% select(typ_szkoly) %>% distinct %>% unlist
  
  ## Create names for list of examination
  
  names(rodzaj_szkoly) <- rodzaj_szkoly
  
  ## Make a list assigning exam type to each school type
  
  typy_egzaminow <- lapply(rodzaj_szkoly,
                           function(x) all_data_geo %>%
                             filter(typ_szkoly==x) %>%
                             select(rodzaj_egzaminu) %>%
                             na.omit %>%
                             distinct %>%
                             unlist(use.names=FALSE))
  
  
  ## Create names for UI
  ## ?? Seems like reusing it like that is dirty and should be redone ??
  
  lista_egzaminow <- lapply(rodzaj_szkoly,
                            function(x) all_data_geo %>%
                              filter(typ_szkoly == x) %>%
                              select(skrot) %>%
                              na.omit %>%
                              distinct)
  
  names(rodzaj_szkoly) <- c('gimnazjum', 'technikum uzupełniające', 'liceum ogólnokształcące', 'technikum',
                            'szkoła podstawowa')
  
  listaCzesci <- reactive(lista_egzaminow[[input$rodzaj]] %>% unlist(use.names=F) %>% as.list())
  
  observeEvent(input$rodzaj, updateCheckboxGroupInput(session, 'czesc', choices = listaCzesci(), selected = listaCzesci()))
  
  output$wagi_czesci <- renderMenu({
    sidebarMenu(
      menuItem("Czesci egzaminu", icon = icon("bar-chart-o"),
               checkboxGroupInput('czesc', label = NULL, choices = lista_egzaminow[['gimn.']] %>% unlist(use.names=F) %>% as.list(),
                                  selected = lista_egzaminow[['gimn.']] %>% unlist(use.names=F) %>% as.list())
      ),
      menuItem("Waga czesci egzaminu", icon = icon("bar-chart-o"),
               lapply(listaCzesci(), function(nazwa_czesci) {
                 numericInput(inputId = paste0('waga_',nazwa_czesci), value=100, label = nazwa_czesci, min = 0 , max = 100, step = 5)
               })
    ))
  })
  
  output$mapa <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      fitBounds(minimum_lon, minimum_lat, maximum_lon, maximum_lat) %>%
      addLegend(position = 'bottomright', title = 'Legenda: kolor odzwierciedla wyniki z wybranych egzaminów w danej szkole.',
                bins = 3, 
                colors = rev(markers_palette(c(1:3))),
                labels = c('Lepiej', rep('',1), 'Gorzej'))
    })
  
  observe({
      if(nrow(filteredData())==0) { leafletProxy("mapa") %>% clearShapes() %>% clearMarkerClusters()} 
      else{
      ilosc_szkol <- schools_in_bounds() %>% nrow
      ilosc_szkol_pl <- filteredData() %>% nrow
      miejsce_szkoly_pl <- function(id) { which(filteredData()$id_szkoly %in% id) }
      miejsce_szkoly <- function(id) { which(schools_in_bounds()$id_szkoly == id) } 
      leafletProxy(map = 'mapa', data=schools_in_bounds()) %>%
          clearShapes() %>%
          clearMarkerClusters() %>% 
          addCircleMarkers(lng = ~lon, lat = ~lat,
                           clusterOptions = markerClusterOptions(disableClusteringAtZoom = 8),
                           popup=~paste0('<STRONG><BOLD>', nazwa_szkoly, '<BR></BOLD></STRONG>',
                                         'BR',
                                         'Miejsce szkoly na pokazanym obszarze: ', miejsce_szkoly(id_szkoly), '/', ilosc_szkol,
                                         '<BR>',
                                         'Miejsce szkoly w Polsce: ', miejsce_szkoly_pl(id_szkoly), '/', ilosc_szkol_pl),
                           fillColor = ~markers_palette(srednia), fillOpacity = 1, radius = 7,
                           stroke = TRUE, color='black', opacity = 1, weight = 1)
      }
    })
  
  filteredData <- reactive({
    rodzaj_szkoly <- input$rodzaj
    lata <- input$wybierz_lata
    wybrana_czesc <- input$czesc
    
    first <- all_data_geo %>% filter(nazwa_szkoly != '', !is.na(lon), !is.na(lat), rok %in% seq(lata[1], lata[2], by = 1),
                            typ_szkoly==rodzaj_szkoly, skrot %in% wybrana_czesc) %>% 
      group_by(id_szkoly) %>%
      mutate(srednia=mean(srednia)) %>%
      ungroup %>%
      distinct(id_szkoly)
    
    second <- all_data_geo %>%
       filter(!is.na(lat), rok %in% seq(lata[1], lata[2], by = 1),
              typ_szkoly==rodzaj_szkoly, skrot %in% wybrana_czesc) %>% 
       group_by(id_szkoly, skrot) %>%
       summarise(srednia=mean(srednia)) %>%
       spread(., skrot, srednia)
    
    right_join(first,second) %>% arrange(desc(srednia))
    
  })
  
  output$teshting <- DT::renderDataTable({
    DT::datatable(filteredData(), options=list(scrollX=TRUE))
  })
  
  schools_in_bounds <- reactive({
      lata <- input$wybierz_lata
      if ( is.null(input$mapa_bounds) ){ return(all_data_geo[FALSE,]) }
    
      if ( is.null(input$rodzaj) || is.na(input$rodzaj) || input$rodzaj == ''){
          rodzaj_szkoly <- all_data_geo %>% dplyr::filter(typ_szkoly!='ZZ') %>% select(typ_szkoly) %>% distinct %>% unlist 
          names(rodzaj_szkoly) <- c('gimnazjum', 'technikum uzupełniające', 'liceum ogólnokształcące', 'technikum',
                                  'szkoła podstawowa')
        }
      else rodzaj_szkoly <- input$rodzaj
      
      bounds <- input$mapa_bounds
      lat_rng <- range(bounds$north, bounds$south)
      lon_rng <- range(bounds$east, bounds$west)
      filteredData() %>% dplyr::filter(lat >= lat_rng[1], lat <= lat_rng[2], lon >= lon_rng[1], lon <= lon_rng[2])
      
    })
      
  output$schools_in_view <- DT::renderDataTable({
    
    dane_do_tabeli <- schools_in_bounds() %>% select(-id_szkoly,-rok,-(lu:czesc_egzaminu),-(adres:lat))
    dane_do_tabeli <- dane_do_tabeli[,c(2,1,seq(3,length(colnames(dane_do_tabeli))))] %>% arrange(desc(srednia)) %>% rename(Ogolnie=srednia)
    
    normalit15<-function(m){
      round(4*((m - min(m, na.rm = T))/(max(m, na.rm = T)-min(m, na.rm = T)))+1, digits = 2)
    }
    
   dane_do_tabeli <- dane_do_tabeli %>% ungroup %>% mutate_each(funs(normalit15),-1)
    DT::datatable(dane_do_tabeli, options=list(scrollX=TRUE))
    
  })
  
})

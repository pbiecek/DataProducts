load("../ZPD_iter1.dat")
load("../ZPD_szkoly.dat")
library(dplyr)
library(sqldf)
library(tidyr)
library(ggplot2)
library(rCharts)
library(ca)

# ~~~~~~~~~~~~~~~~~~~~~~UWAGA~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Zmiana przelicznika. Zamiast punktow trzeba liczyc procenty - na razie tylko dla mimuw
mimuw_classify <- filter(data.wawa, p_mat>15 & p_pl>15 & r_mat>0)
mimuw_classify <- mimuw_classify %>% rowwise() %>% mutate(wynik = 
                                                            0.1 * max(0, 0.6*p_pl*10/7, r_pl*10/4, na.rm = TRUE) + 
                                                            0.1 * max(0, 0.6*p_mat*2, r_mat*2, na.rm = TRUE) +
                                                            0.1 * max(0, 0.6*p_ang*2, r_ang*2, na.rm = TRUE) + 
                                                            0.5 * max(0, r_inf*2, r_mat*2, na.rm = TRUE) + 
                                                            0.2 * max(0, r_mat*2, r_pl*10/4, r_ang*2, r_fiz*10/6, r_bio*10/6, r_chem*10/6, r_inf*2, na.rm = TRUE)
)
mini_classify <- filter(data.wawa, p_mat>15 & p_pl>15 & r_mat>0 & (r_bio > 0 | r_fiz > 0 | r_chem>0 | r_inf > 0))
mini_classify <- mini_classify %>% rowwise() %>% mutate(wynik = 
                                                          max(0, 0.5*p_mat*2, r_mat*2, na.rm = TRUE)*4/9 +
                                                          0.25 * max(0, 0.5*p_ang*2,r_ang*2, na.rm = TRUE)/9 + 
                                                          max(0, r_fiz*10/6, 0.5*r_bio*10/6, r_fiz*10/6, 0.75*r_chem*10/6, r_inf*2, na.rm = TRUE)*4/9
)
uj_classify <- filter(data.wawa, p_mat>15 & p_pl>15 & sum(c(r_inf>0,r_mat>0, r_bio>0,r_fiz>0,r_chem>0), na.rm = TRUE)>1)
uj_classify <- uj_classify %>% rowwise() %>% mutate(wynik = 
                                                        max(max(0, 0.5*p_mat*2, r_mat*2, na.rm = TRUE), 
                                                            max(0, 0.5*p_inf*2, r_inf*2, na.rm = TRUE),
                                                            max(0, 0.5*p_bio*10/6, r_bio*10/6, na.rm = TRUE),
                                                            max(0, 0.5*p_fiz*10/6, r_fiz*10/6, na.rm = TRUE),
                                                            max(0, 0.5*p_chem*10/6, r_chem*10/6, na.rm = TRUE),
                                                            na.rm = TRUE)/2 + 
                                                        
                                                        sort(
                                                          c(
                                                            max(0, 0.5*p_mat*2, r_mat*2, na.rm = TRUE), 
                                                            max(0, 0.5*p_inf*2, r_inf*2, na.rm = TRUE),
                                                            max(0, 0.5*p_bio*10/6, r_bio*10/6, na.rm = TRUE),
                                                            max(0, 0.5*p_fiz*10/6, r_fiz*10/6, na.rm = TRUE),
                                                            max(0, 0.5*p_chem*10/6, r_chem*10/6, na.rm = TRUE)
                                                          ),
                                                          ,partial=4)[4]/2
)

uksford_classify <- filter(data.wawa, p_mat>15 & p_pl>15 & r_mat>0)
uksford_classify <- uksford_classify %>% rowwise() %>% mutate(wynik = 
                                                                max(0, 0.4*p_mat*2, 0.8*r_mat*2, na.rm = TRUE) +
                                                                max(0, 0.1*p_ang*2, 0.2*r_ang*2, na.rm = TRUE)
)




get_classify <- function(wybranaUczelnia, wybranyKierunek) {
  if (wybranaUczelnia == 'UW') {
    classify <- mimuw_classify
    if (wybranyKierunek == 'Matematyka')
      prog <- 72
    else if (wybranyKierunek == 'Informatyka')
      prog <- 85
    else
      prog <- 0
  } else if (wybranaUczelnia == 'PW') {
    classify <- mini_classify
    if (wybranyKierunek == 'Matematyka')
      prog <- 70
    else if (wybranyKierunek == 'Informatyka')
      prog <- 80
    else
      prog <- 0
  } else if (wybranaUczelnia == 'UJ') {
    classify <- uj_classify
    if (wybranyKierunek == 'Matematyka')
      prog <- 70
    else if (wybranyKierunek == 'Informatyka')
      prog <- 80
    else
      prog <- 0
  } else if (wybranaUczelnia == 'UKSW') {
    classify <- uksford_classify
    if (wybranyKierunek == 'Matematyka')
      prog <- 70
    else if (wybranyKierunek == 'Informatyka')
      prog <- 80
    else
      prog <- 0
  }
  return(classify)
}

get_prog <- function(wybranaUczelnia, wybranyKierunek) {
  if (wybranaUczelnia == 'UW') {
    classify <- mimuw_classify
    if (wybranyKierunek == 'Matematyka')
      prog <- 72
    else if (wybranyKierunek == 'Informatyka')
      prog <- 85
    else
      prog <- 0
  } else if (wybranaUczelnia == 'PW') {
    classify <- mini_classify
    if (wybranyKierunek == 'Matematyka')
      prog <- 70
    else if (wybranyKierunek == 'Informatyka')
      prog <- 80
    else
      prog <- 0
  } else if (wybranaUczelnia == 'UJ') {
    classify <- uj_classify
    if (wybranyKierunek == 'Matematyka')
      prog <- 70
    else if (wybranyKierunek == 'Informatyka')
      prog <- 80
    else
      prog <- 0
  } else if (wybranaUczelnia == 'UKSW') {
    classify <- uksford_classify
    if (wybranyKierunek == 'Matematyka')
      prog <- 70
    else if (wybranyKierunek == 'Informatyka')
      prog <- 80
    else
      prog <- 0
  }
  return(prog)
}



scores_aggregate <- function(res){
  if(is.na(res)) return("NAN")
  if(res > 40) return("81%-100%")
  if(res > 30) return("61%-80%")
  if(res > 20) return("41%-60%")
  if(res > 10) return("21%-40%")
  return("1%-20%")
}

get_table_for_results_of_school <- function(name, tbl, subjects){
  tbl <- filter(tbl, id_szkoly == name)
  len <- dim(tbl)[2]
  wid <- dim(tbl)[1]
  tbl <- tbl[,2:(len-1)]
  final <- as.data.frame(lapply(tbl,FUN = function(x) {sapply(x,FUN=scores_aggregate)}))
  gathered <- gather(final, key = przedmiot, value = wynik)
  gathered <- filter(gathered, wynik != "NAN")
  gathered <- filter(gathered, przedmiot %in% subjects)
  tab <- table(gathered$wynik, gathered$przedmiot)
  return(tab)
}


get_school_id <- function(school) {
  school_row <- szkoly %>% filter(nazwa_szkoly == school) %>% head(1)
  return(school_row$id_szkoly)
}



shinyServer(function(input, output) { 
  ###########################################################################
  # TYTUL
  ###########################################################################
  output$tytul <- renderText({
    "</br><h4>Procent uczniow danej szkoly dostajacych sie na wybrany kierunek</h4></br>"
  })
  
  ###########################################################################
  #  TABELA Z DANYMI (ILE PROCENT UCZNIOW Z DANEJ SZKOLY SIE DOSTAJE)
  ###########################################################################
  output$ranking <- renderDataTable({
    classify <- get_classify(input$wybranaUczelnia, input$wybranyKierunek)
    prog <- get_prog(input$wybranaUczelnia, input$wybranyKierunek)
    
    # Lista 300 najlepszych studentow ogolnie + wynik 300 studenta
    best_300 <- classify %>% arrange(desc(wynik)) %>% head(300)
    wynik_300 <- best_300[300,]$wynik
    
    # Wyliczanie top 5 szkol
    top <- classify %>% group_by(id_szkoly) %>%
      summarise(all_count = n(), top_300_count = sum(wynik >= wynik_300), procent = 100*top_300_count/all_count) %>%
      arrange(desc(procent)) %>% filter(all_count > 10) %>% head(input$ileSzkol) %>%
      merge(filter(szkoly, rok==2015), by = "id_szkoly") %>% merge(classify, by = "id_szkoly")
    
    # Zamiana id_szkoly z typu numeric na character
    top$id_szkoly <- as.character(top$id_szkoly)
    
    procent <- top %>% group_by(id_szkoly, nazwa_szkoly) %>% summarise(procent = 100*mean(wynik > prog))
    procent
  })

  ###########################################################################
  #  WYKRES SLUPKOWY (ILE PROCENT UCZNIOW Z DANEJ SZKOLY SIE DOSTAJE)
  ###########################################################################
  output$wykresSlupkowy <- renderPlot({
    classify <- get_classify(input$wybranaUczelnia, input$wybranyKierunek)
    prog <- get_prog(input$wybranaUczelnia, input$wybranyKierunek)
    
    # Lista 300 najlepszych studentow ogolnie + wynik 300 studenta
    best_300 <- classify %>% arrange(desc(wynik)) %>% head(300)
    wynik_300 <- best_300[300,]$wynik
    
    # Wyliczanie top 5 szkol
    top <- classify %>% group_by(id_szkoly) %>%
      summarise(all_count = n(), top_300_count = sum(wynik >= wynik_300), procent = 100*top_300_count/all_count) %>%
      arrange(desc(procent)) %>% filter(all_count > 10) %>% head(input$ileSzkol) %>%
      merge(filter(szkoly, rok==2015), by = "id_szkoly") %>% merge(classify, by = "id_szkoly")
    
    # Zamiana id_szkoly z typu numeric na character
    top$id_szkoly <- as.character(top$id_szkoly)
    
    procent <- top %>% group_by(nazwa_szkoly, id_szkoly) %>% summarise(procent = 100*mean(wynik > prog))
    slupkowy <- ggplot(procent, aes(x=id_szkoly, y=procent)) + geom_bar(stat='identity', fill='blue', width=0.5)
    slupkowy
  })
  
  ###########################################################################
  # TYTUL
  ###########################################################################
  output$tytulDwa <- renderText({
    "</br><h4>Rozklad wynikow w procesie rekrutacyjnym uczniow danej szkoly</h4></br>"
  })
  
  ###########################################################################
  #  WYKRES SKRZYPCOWY (ROZKLAD WYNIKOW REKRUTACJI UCZNIOW Z DANEJ SZKOLY)
  ###########################################################################
  output$wykresSrzypcowy <- renderPlot({
    classify <- get_classify(input$wybranaUczelnia, input$wybranyKierunek)
    prog <- get_prog(input$wybranaUczelnia, input$wybranyKierunek)
    
    # Lista 300 najlepszych studentow ogolnie + wynik 300 studenta
    best_300 <- classify %>% arrange(desc(wynik)) %>% head(300)
    wynik_300 <- best_300[300,]$wynik
    
    # Wyliczanie top 5 szkol
    top <- classify %>% group_by(id_szkoly) %>%
      summarise(all_count = n(), top_300_count = sum(wynik >= wynik_300), procent = 100*top_300_count/all_count) %>%
      arrange(desc(procent)) %>% filter(all_count > 10) %>% head(input$ileSzkol) %>%
      merge(filter(szkoly, rok==2015), by = "id_szkoly") %>% merge(classify, by = "id_szkoly")
    
    # Zamiana id_szkoly z typu numeric na character
    top$id_szkoly <- as.character(top$id_szkoly)
    
    
    skrzypcowy <- ggplot(top, aes(x = id_szkoly, y = wynik, group=id_szkoly, fill=nazwa_szkoly)) +
      theme(axis.title.x=element_blank(), axis.text.x=element_blank(),
            axis.ticks.x=element_blank(), legend.text=element_text(size=7.6),
            legend.position='right') +
      geom_violin() +
      expand_limits(y=c(15, 100))
    
    if (input$wybranyKierunek == 'Matematyka' && input$prog) {
      skrzypcowy <- skrzypcowy + geom_hline(aes(yintercept=prog), size=0.8)
    } else if (input$wybranyKierunek == 'Informatyka'&& input$prog) {
      skrzypcowy <- skrzypcowy + geom_hline(aes(yintercept=prog), size=0.8)
    }
    skrzypcowy
  })
  
  ###########################################################################
  # TYTUL
  ###########################################################################
  output$tytul2 <- renderText({
    "</br><h4>Ilosc uczniow uzyskujacy wynik z matury na okreslonym poziomie</h4></br>"
  })
  
  ###########################################################################
  # LISTA SZKOL
  ###########################################################################
  output$wyborSzkoly <- renderUI({
    # id szkol z Warszawy
    id_szkol_warszawa <- unique(data.wawa['id_szkoly'])
    
    # id szkol z Warszawy jako wektor
    id_szkol_warszawa_vec <- id_szkol_warszawa %>% select(id_szkoly) %>% sapply(as.character) %>% as.vector

    # id szkoly + nazwa szkoly (Warszawa)
    szkoly_warszawa <- szkoly %>% filter(id_szkoly %in% id_szkol_warszawa_vec) %>% select(nazwa_szkoly) %>% unique()
    
    selectInput(inputId = "szkola2",
                label = "Szkola",
                choices = szkoly_warszawa,
                selected = "I")
  })
  
  ###########################################################################
  # LISTA PRZEDMIOTOW
  ###########################################################################
  output$wyborPrzedmiotow <- renderUI({
    checkboxGroupInput("przedmioty2",
                       label = "Przedmioty",
                       choices = names(data.wawa)[2:15])
  })
  
  ###########################################################################
  #  TABELA Z DANYMI (WYNIKI MATUR)
  ###########################################################################
  output$ranking2 = renderDataTable({
    szkola <- get_school_id(input$szkola2)
    przedmioty <- input$przedmioty2
    if (length(przedmioty) > 0) {
      curr_tbl <- get_table_for_results_of_school(szkola, data.wawa, przedmioty)
      curr_tbl <- curr_tbl %>% as.data.frame.matrix() %>% add_rownames("Wynik")
      curr_tbl
    }
  })

  ###########################################################################
  #  WYKRES TENDENCJI WYKINOW Z MATUR DLA SZKOLY
  ###########################################################################
  output$wykresCA2 = renderPlot({
    szkola <- get_school_id(input$szkola2)
    przedmioty <- input$przedmioty2
    if (length(przedmioty) > 2) {
      curr_tbl <- get_table_for_results_of_school(szkola, data.wawa, przedmioty)
      wykres <- plot(ca(curr_tbl), arrows = c(TRUE, FALSE))
      wykres
    }
  })
})

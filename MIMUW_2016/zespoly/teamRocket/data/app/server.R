load("../ZPD_iter1.dat")
load("../ZPD_szkoly.dat")
library(dplyr)
library(sqldf)
library(tidyr)
library(ggplot2)
library(rCharts)

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


shinyServer(function(input, output) {
  output$wykresSlupkowy = renderPrint({
    if (input$wybranaUczelnia == 'UW') {
      classify <- mimuw_classify
      if (input$wybranyKierunek == 'Matematyka')
        prog <- 72
      else if (input$wybranyKierunek == 'Informatyka')
        prog <- 85
      else
        prog <- 0
    } else if (input$wybranaUczelnia == 'PW') {
      classify <- mini_classify
      if (input$wybranyKierunek == 'Matematyka')
        prog <- 70
      else if (input$wybranyKierunek == 'Informatyka')
        prog <- 80
      else
        prog <- 0
    } else if (input$wybranaUczelnia == 'UJ') {
      classify <- uj_classify
      if (input$wybranyKierunek == 'Matematyka')
        prog <- 70
      else if (input$wybranyKierunek == 'Informatyka')
        prog <- 80
      else
        prog <- 0
    } else if (input$wybranaUczelnia == 'UKSW') {
      classify <- uksford_classify
      if (input$wybranyKierunek == 'Matematyka')
        prog <- 70
      else if (input$wybranyKierunek == 'Informatyka')
        prog <- 80
      else
        prog <- 0
    }
    
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
    
    procent <- top %>% group_by(nazwa_szkoly) %>% summarise(procent = mean(wynik > prog))
    procent
#     
#     slupkowy <- ggplot(procent) +
#       theme(axis.title.x=element_blank(), axis.text.x=element_blank(),
#             axis.ticks.x=element_blank(), legend.text=element_text(size=7.6),
#             legend.position='right') +
#       geom_bar() +
#       expand_limits(y=c(15, 100))
#     slupkowy
  })
  
  output$wykresSrzypcowy = renderPlot({
    if (input$wybranaUczelnia == 'UW') {
      classify <- mimuw_classify
      if (input$wybranyKierunek == 'Matematyka')
        prog <- 72
      else if (input$wybranyKierunek == 'Informatyka')
        prog <- 85
      else
        prog <- 0
    } else if (input$wybranaUczelnia == 'PW') {
      classify <- mini_classify
      if (input$wybranyKierunek == 'Matematyka')
        prog <- 70
      else if (input$wybranyKierunek == 'Informatyka')
        prog <- 80
      else
        prog <- 0
    } else if (input$wybranaUczelnia == 'UJ') {
      classify <- uj_classify
      if (input$wybranyKierunek == 'Matematyka')
        prog <- 70
      else if (input$wybranyKierunek == 'Informatyka')
        prog <- 80
      else
        prog <- 0
    } else if (input$wybranaUczelnia == 'UKSW') {
      classify <- uksford_classify
      if (input$wybranyKierunek == 'Matematyka')
        prog <- 70
      else if (input$wybranyKierunek == 'Informatyka')
        prog <- 80
      else
        prog <- 0
    }
    
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
  
  output$ranking = renderPrint({
    
  })
})
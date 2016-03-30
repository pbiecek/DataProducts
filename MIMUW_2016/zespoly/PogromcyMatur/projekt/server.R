
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)
library(dplyr)
library(ZPD)
library(ggplot2)

shinyServer(function(input, output) {
  
  output$distPlot <- renderPlot({
    
    # generate and plot an rnorm distribution with the requested
    # number of observations
    data <- as.data.frame(t(szkola_uczniowie(input$gmina, input$rok, input$typ, input$sprrdz, input$sprcz)))
    ggplot(data, aes(x=rownames(data), y=V1))
    
  })
  
})

src = polacz()

szkola_uczniowie <- function(gmina, w_rok, typ, egz_rodz, egz_czesc) {
  
  szkoly = pobierz_szkoly(src) %>%
    filter(gmina_szkoly == gmina, rok == w_rok, typ_szkoly == typ) %>%
    select(id_szkoly, gmina_szkoly, nazwa_szkoly, rok, wojewodztwo_szkoly, typ_szkoly)
  
  szkoly_f = szkoly %>%
      select(id_szkoly, rok, wojewodztwo_szkoly)
  
  ut = pobierz_dane_uczniowie_testy(src) %>%
    inner_join(szkoly_f)
  
  uczniowie = pobierz_uczniow(src) %>%
    inner_join(ut) %>%
    select(id_szkoly, id_cke, id_testu, id_obserwacji)
  
  dane = pobierz_wyniki_egzaminu(src, egz_rodz, egz_czesc, w_rok, TRUE) %>%
    inner_join(szkoly_f) %>%
    inner_join(uczniowie) %>%
    select(-id_szkoly, -rok, -wojewodztwo_szkoly, -id_testu, -id_obserwacji, -id_cke) %>%
    collect() %>%
    summarise_each(funs(mean(., na.rm = TRUE)))
  return(dane)
}

pobierz_gminy <- function() {
  dane = pobierz_szkoly(src) %>%
    select(gmina_szkoly, wojewodztwo_szkoly) %>%
    distinct() %>%
    arrange(wojewodztwo_szkoly, gmina_szkoly) %>%
    collect()
  return(dane)
}  

wyniki_usrednij <- function(wyniki) {
  res = wyniki %>%
    group_by(id_testu)
  return(res)
}
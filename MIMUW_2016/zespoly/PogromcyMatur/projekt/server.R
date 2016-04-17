
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
  tmp = as.list(1:7)
  names(tmp) = unique(wyniki_szkol$id_kryterium)
  kryt = as.integer(lapply(wyniki_szkol$id_kryterium, function(x) tmp[[x]]))
  kolory = c('red', 'purple', 'blue')
  output$kryteriaPlot <- renderPlot({
    plot(
      x = kryt,
      y = wyniki_szkol$sredni_wynik,
      col = kolory[wyniki_szkol$id], type="p",
      ylab = 'Ilość punktów',
      main = 'Porównanie średnich wyników szkół za poszczególne kryteria',
      xlab = 'Numer kryterium',
      )
  })
  
})

load("test/bigData.RData")
load("test/smallData.RData")

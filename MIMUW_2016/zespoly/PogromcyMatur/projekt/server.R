
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

load("test/bigData.RData")
load("test/smallData.RData")

# Generuje wykres wyników szkół po kryteriach z testu "id_testu_wyniki" porządkowanych
# po "id_testu_ocena". Kwantyle to lista kwantyli (dwuelementowych list początek, koniec).
# Przykładowo pojedynczy kwantyl to (0.5, 0.7).
getplot <- function(id_testu_wyniki, id_testu_ocena, kwantyle) {

  # Wyciągnij najpierw dane o czkołach, których dotyczą te testy
  dane = oceny_szkol %>%
         filter(id_testu == id_testu_ocena) %>%
         arrange(sredni_wynik) %>%
         select(-id_testu)
  szkoly = dane %>% select(id)
  wyniki = wyniki_szkol %>%
           filter(id_testu == id_testu_wyniki) %>%
           inner_join(szkoly) %>%
           select(-id_testu)
  dane_wykresu = NULL

  # Dla każdego kwantyla wyciągnij i uśrednij dane z wyniki_szkol
  for (i in 1:length(kwantyle)) {
    kwantyl = dane %>%
              filter(sredni_wynik >= quantile(dane$sredni_wynik, kwantyle[[i]][[1]]),
                     sredni_wynik <= quantile(dane$sredni_wynik, kwantyle[[i]][[2]])) %>%
              select(id)
    wynikk = wyniki %>%
             inner_join(kwantyl) %>%
             group_by(id_kryterium) %>%
             summarise(suma_wynik = mean(sredni_wynik)) %>%
             select(id_kryterium, suma_wynik)
    wynikk$kwantyl = paste(as.character(kwantyle[[i]][[1]]), ":", as.character(kwantyle[[i]][[2]]))
    dane_wykresu <- rbind(dane_wykresu, wynikk)
  }

  #Przeskaluj dane na procenty
  dane_z_kryt = dane_wykresu %>%
                 inner_join(kryteria, by = c("id_kryterium" = "id"))
  dane_wykresu$suma_wynik = dane_z_kryt$suma_wynik / dane_z_kryt$max_punktow

  #Wykres
  plot = ggplot(dane_wykresu, aes(x = id_kryterium, y = suma_wynik, fill = factor(kwantyl))) + geom_bar(stat="identity", position = "dodge") +
  scale_fill_discrete(name = "Kwantyl") + ylab("Średni wynik (%)") + xlab("Id kryterium") +
  scale_y_continuous(labels = scales::percent)
  return(plot)
}

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

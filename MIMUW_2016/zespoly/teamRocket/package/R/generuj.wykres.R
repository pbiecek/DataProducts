# Tworzenie nowej tabeli na zależność wynik procentowy uzyskany przez procent uczniow.
generuj.tabele.pp <- function(dane, nazwa_kolumny) {
  d <- data.frame(wynik_procentowy=numeric(0), procent_osob=numeric(0))
  for(i in 0:100)
  {
    procent <- 100 * nrow(dane[dane$nazwa_kolumny == i,]) / nrow(dane) 
    d[nrow(d)+1, ] <- c(i, procent)
  }
  return(d)
}


# Generuj wykres liniowy na podstawie otrzymanych danych.
generuj.wykres <- function(dane, nazwa_kolumny, szkola_id, opis) {
  dane_szkola <- filter(dane, id_szkoly==szkola_id)
  dane_gmina <- filter(dane_licz, gmina_szkoly==dane_szkola[1,]$gmina_szkoly)
  
  # Tworzenie nowych tabel na zależność wynik procentowy uzyskany przez procent uczniow.
  dane_procentowe <- generuj_tabele_pp(dane, nazwa_kolumny)
  dane_procentowe_szkola <- generuj_tabele_pp(dane_szkola, nazwa_kolumny)
  dane_procentowe_gmina <- generuj_tabele_pp(dane_gmina, nazwa_kolumny)
  
  wykres <- ggplot() +
            geom_line(data=dane_procentowe_szkola, aes(x=wynik_procentowy, y=procent_osob), colour="red") +
            geom_line(data=dane_procentowe_gmina, aes(x=wynik_procentowy, y=procent_osob), colour="#BEE4A9") +
            geom_line(data=dane_procentowe, aes(x=wynik_procentowy, y=procent_osob), colour="gray")
            
  return(wykres)
}
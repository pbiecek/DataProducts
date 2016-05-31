# Tworzenie nowej tabeli na zależność wynik procentowy uzyskany przez procent uczniow.
generuj_tabele_pp <- function(dane, nazwa_kolumny) {
  x <- hist(dane[[nazwa_kolumny]], plot=FALSE)
  d <- data.frame(wynik_procentowy = x$mids, procent_osob = 100 * x$counts / nrow(dane))
  return(d)
}

# Generuj dowolny wykres.
generuj_wykres <- function(dane, nazwa_kolumny, szkola_id, opis) {
  dane_szkola <- dplyr::filter(dane, id_szkoly==szkola_id)
  dane_gmina <- dplyr::filter(dane, gmina_szkoly==dane_szkola[1,]$gmina_szkoly)
  
  # Tworzenie nowej tabeli na zależność wynik procentowy uzyskany przez procent uczniow.
  dane_procentowe <- generuj_tabele_pp(dane, nazwa_kolumny)
  dane_procentowe_szkola <- generuj_tabele_pp(dane_szkola, nazwa_kolumny)
  dane_procentowe_gmina <- generuj_tabele_pp(dane_gmina, nazwa_kolumny)
  
  wykres <- ggplot() +
            geom_smooth(se=FALSE, data=dane_procentowe_szkola, aes(x=wynik_procentowy, y=procent_osob), colour="red", size=1.5) +
            geom_smooth(se=FALSE, data=dane_procentowe_gmina, aes(x=wynik_procentowy, y=procent_osob), colour="#9F9F8F", size=1.2) +
            geom_smooth(se=FALSE, data=dane_procentowe, aes(x=wynik_procentowy, y=procent_osob), colour="#2F87EB", size=1.2) +
            labs(title = opis) + xlab('Wynik procentowy') + ylab('Liczba uczniów z danym wynikiem do ogólnej liczby uczniów') + 
            theme(legend.position = "bottom",
                  panel.background = element_rect(fill = "white"),
                  panel.grid.major = element_line(colour = "#D1D1C1"))
            
  return(wykres)
}
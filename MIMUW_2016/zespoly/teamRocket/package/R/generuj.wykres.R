# Tworzenie nowej tabeli na zależność wynik procentowy uzyskany przez procent uczniow.
generuj_tabele_pp <- function(dane, nazwa_kolumny) {
  x <- hist(dane[[nazwa_kolumny]], breaks=100, plot=FALSE)
  d <- data_frame(wynik_procentowy = x$mids, procent_osob = 100 * x$counts / nrow(dane))
  return(d)
}

# Generuj wykres liniowy.
generuj_wykres <- function(dane, nazwa_kolumny, szkola_id, opis) {
  dane_szkola <- filter(dane, id_szkoly==szkola_id)
  dane_gmina <- filter(dane_licz, gmina_szkoly==dane_szkola[1,]$gmina_szkoly)
  
  # Tworzenie nowej tabeli na zależność wynik procentowy uzyskany przez procent uczniow.
  dane_procentowe <- generuj_tabele_pp(dane, nazwa_kolumny)
  dane_procentowe_szkola <- generuj_tabele_pp(dane_szkola, nazwa_kolumny)
  dane_procentowe_gmina <- generuj_tabele_pp(dane_gmina, nazwa_kolumny)
  
  wykres <- ggplot() +
            geom_line(data=dane_procentowe_szkola, aes(x=wynik_procentowy, y=procent_osob), colour="red") +
            geom_line(data=dane_procentowe_gmina, aes(x=wynik_procentowy, y=procent_osob), colour="#BEE4A9") +
            geom_line(data=dane_procentowe, aes(x=wynik_procentowy, y=procent_osob), colour="gray")
            
  return(wykres)
}
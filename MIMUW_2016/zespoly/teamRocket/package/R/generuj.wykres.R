# Tworzenie nowej tabeli na zależność wynik procentowy uzyskany przez procent uczniow.
generuj_tabele_pp <- function(dane, nazwa_kolumny) {
  x <- graphics::hist(dane[[nazwa_kolumny]], plot=FALSE)
  d <- data.frame(wynik_procentowy = x$mids, procent_osob = 100 * x$counts / nrow(dane))
  return(d)
}

#' Generuj wykresy porównawcze dla konkretnego współczynnika
#' @param dane tabela z której generować wykres
#' @param nazwa.kolumny nazwa kolumny 
generuj.wykres <- function(dane, nazwa.kolumny, szkola_id, opis) {
  dane_szkola <- dplyr::filter(dane, id_szkoly==szkola_id)
  dane_gmina <- dplyr::filter(dane, gmina_szkoly==dplyr::first(dane_szkola$gmina_szkoly))
  
  # Tworzenie nowej tabeli na zależność wynik procentowy uzyskany przez procent uczniow.
  # Zawartość tabel wygładzana
  dane_procentowe <- as.data.frame(zoo::rollmean(generuj_tabele_pp(dane, nazwa.kolumny), 3))
  dane_procentowe_szkola <- as.data.frame(zoo::rollmean(generuj_tabele_pp(dane_szkola, nazwa.kolumny), 3))
  dane_procentowe_gmina <- as.data.frame(zoo::rollmean(generuj_tabele_pp(dane_gmina, nazwa.kolumny), 3))
  
  spp = 0.3
  wykres <- ggplot2::ggplot() +
    ggplot2::geom_smooth(se=FALSE, data=dane_procentowe, ggplot2::aes(x=wynik_procentowy, y=procent_osob,colour="#2F87EB"), span = spp, size=1.2) +
    ggplot2::geom_smooth(se=FALSE, data=dane_procentowe_gmina, ggplot2::aes(x=wynik_procentowy, y=procent_osob, colour="#9F9F8F"), span = spp,  size=1.2) +
    ggplot2::geom_smooth(se=FALSE, data=dane_procentowe_szkola, ggplot2::aes(x=wynik_procentowy, y=procent_osob, colour="red"), span = spp, size=1.5) +
    ggplot2::labs(title = opis) +
    ggplot2::xlab('Wynik procentowy') +
    ggplot2::ylab('Relatywny procent uczniów') + 
    ggplot2::theme(legend.position = "right",
                  panel.background = ggplot2::element_rect(fill = "white"),
                  panel.grid.major = ggplot2::element_line(colour = "#D1D1C1"),
                  legend.key = ggplot2::element_rect(fill = "white")) +
    ggplot2::scale_color_identity(name="Legenda", guide = "legend", labels = c("kraj", "gmina", "szkoła"))
            
  return(wykres)
}
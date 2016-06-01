#' Generuj wykresy porównawcze dla konkretnego współczynnika
#' @param dane tabela z której generować wykres
#' @param nazwa.kolumny nazwa kolumny 
#' @export
generuj.wykres <- function(dane, nazwa.kolumny, szkola_id, opis) {
  # Odfiltrowywanie danych
  dane_szkola <- dplyr::filter(dane, id_szkoly==szkola_id)
  dane_gmina <- dplyr::filter(dane, gmina_szkoly==dplyr::first(dane_szkola$gmina_szkoly))
  
  wykres <- ggplot2::ggplot() +
    ggplot2::geom_freqpoly(data=dane, ggplot2::aes_string(x=nazwa.kolumny, "..density..", colour='"#9F9F8F"'), size = 2, bins = 10) +
    ggplot2::geom_freqpoly(data=dane_gmina, ggplot2::aes_string(x=nazwa.kolumny, "..density..", colour='"#2F87EB"'), size = 1, bins = 10) +
    ggplot2::geom_freqpoly(data=dane_szkola, ggplot2::aes_string(x=nazwa.kolumny, "..density..", colour='"red"'), bins = 10) +
    ggplot2::labs(title = opis) +
    ggplot2::xlab('Wartość wyznacznika') +
    ggplot2::ylab('Częstotliwość') + 
    ggplot2::theme(legend.position = "right",
                  panel.background = ggplot2::element_rect(fill = "white"),
                  panel.grid.major = ggplot2::element_line(colour = "#D1D1C1"),
                  legend.key = ggplot2::element_rect(fill = "white")) +
    ggplot2::scale_colour_identity(name="Legenda", guide = "legend", labels = c("#9F9F8F" = "kraj", "#2F87EB" = "gmina", "red" = "szkoła"))
            
  return(wykres)
}
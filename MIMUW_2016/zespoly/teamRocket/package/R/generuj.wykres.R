#' Generuj wykresy porównawcze dla konkretnego współczynnika
#' 
#' Dla współczynnika o wartościach liczbowych generuje wykres liniowy gęstości
#' Dla współczynnika o wartościach innych (tekst) generuje słupkowy wykres gęstościowy
#' 
#' @param dane tabela z której generować wykres
#' @param nazwa.kolumny nazwa kolumny
#' @param szkola_id id szkoły dla której ma być wygenerowany zestaw danych w ramach tego wykresu
#' @param opis nazwa dla danych zawartych w wyszczególnionej kolumnie - tytuł wykresu
#' @examples {
#' generuj.wykres(matura.2015, "m_mat_r", 24063, "Matura rozszerzona z matematyki")
#' }
#' @export
generuj.wykres <- function(dane, nazwa.kolumny, szkola_id, opis) {
  # Czy kolumna typu liczbowego?
  czy.liczbowe <- is.numeric(dane[[nazwa.kolumny]])
  # Różne wykresy zależnie od typu kolumny
  wykres <- if(czy.liczbowe){
    # Odfiltrowywanie danych
    dane_kraj <- dane
    dane_szkoly <- dplyr::filter_(dane, paste0("id_szkoly==", szkola_id))
    dane_gmina <- dplyr::filter_(dane, paste0("gmina_szkoly=='", dplyr::first(dane_szkoly$gmina_szkoly), "'"))
    # Dla numerycznych kolumn generuj wykresy "funkcji gęstości"
    ggplot2::ggplot() +
    ggplot2::geom_freqpoly(data=dane_kraj, ggplot2::aes_string(x=nazwa.kolumny, "..density..", colour='"#9F9F8F"'), size = 2, bins = 10) +
    ggplot2::geom_freqpoly(data=dane_gmina, ggplot2::aes_string(x=nazwa.kolumny, "..density..", colour='"#2F87EB"'), size = 1, bins = 10) +
    ggplot2::geom_freqpoly(data=dane_szkoly, ggplot2::aes_string(x=nazwa.kolumny, "..density..", colour='"red"'), bins = 10)+
    ggplot2::labs(title = opis) +
    ggplot2::xlab('Wartość wyznacznika') +
    ggplot2::ylab('Częstotliwość') + 
    ggplot2::theme(legend.position = "right",
                   panel.background = ggplot2::element_rect(fill = "white"),
                   panel.grid.major = ggplot2::element_line(colour = "#D1D1C1"),
                   legend.key = ggplot2::element_rect(fill = "white")) +
    ggplot2::scale_colour_identity(name="Legenda", guide = "legend", labels = c("#9F9F8F" = "kraj", "#2F87EB" = "gmina", "red" = "szkoła"))
  } else {
    # Odfiltrowywanie danych
    dane_kraj <- dplyr::filter_(dane, paste0("!is.na(", nazwa.kolumny, ")"))
    dane_szkoly <- dplyr::filter_(dane, paste0("id_szkoly==", szkola_id, " & !is.na(", nazwa.kolumny,")"))
    dane_gmina <- dplyr::filter_(dane, paste0("gmina_szkoly=='", dplyr::first(dane_szkoly$gmina_szkoly), "' & !is.na(", nazwa.kolumny, ")"))
    # Dla nie-numerycznych kolumn generuj stackowane wykresy słupkowe
    ggplot2::ggplot() +
    ggplot2::geom_bar(data=dane_kraj, ggplot2::aes_string(fill=nazwa.kolumny, y="100*..count../sum(..count..)", x='"Kraj"'),position = "stack", na.rm = TRUE) +
    ggplot2::geom_bar(data=dane_gmina, ggplot2::aes_string(fill=nazwa.kolumny, y="100*..count../sum(..count..)", x='"Gmina"'),position = "stack", na.rm = TRUE) +
    ggplot2::geom_bar(data=dane_szkoly, ggplot2::aes_string(fill=nazwa.kolumny, y="100*..count../sum(..count..)", x='"Szkoła"'),position = "stack", na.rm = TRUE)+
    ggplot2::labs(title = opis) +
    ggplot2::xlab('') +
    ggplot2::ylab('Częstotliwość [%]') + 
    ggplot2::theme(legend.position = "right",
                   axis.text = ggplot2::element_text(size = 15),
                   panel.background = ggplot2::element_rect(fill = "white"),
                   panel.grid.major = ggplot2::element_line(colour = "#D1D1C1"),
                   legend.key = ggplot2::element_rect(fill = "white"))+
    ggplot2::guides(fill=ggplot2::guide_legend(title="Wartość wyznacznika"))
  }
  return(wykres)
}

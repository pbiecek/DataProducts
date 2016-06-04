#' Generuj wykresy porównawcze dla konkretnego współczynnika dla dwóch szkół
#' 
#' Dla współczynnika o wartościach liczbowych generuje wykres liniowy gęstości
#' Dla współczynnika o wartościach innych (tekst) generuje słupkowy wykres gęstościowy
#' 
#' @param dane tabela z której generować wykres
#' @param nazwa.kolumny nazwa kolumny
#' @param szkola1_id id pierwszej szkoły dla której ma być wygenerowany zestaw danych w ramach tego wykresu
#' @param szkola2_id id drugiej szkoły dla której ma być wygenerowany zestaw danych w ramach tego wykresu
#' @param opis nazwa dla danych zawartych w wyszczególnionej kolumnie - tytuł wykresu
#' @examples {
#' generuj.wykres(matura.2015, "m_mat_r", 24063, 24064, "Matura rozszerzona z matematyki")
#' }
#' @export
generuj.wykres <- function(dane, nazwa.kolumny, szkola1_id, szkola2_id, opis) {
  # Odfiltrowywanie danych
  dane_szkola1 <- dplyr::filter_(dane, paste0("id_szkoly==", szkola1_id, " & !is.na(", nazwa.kolumny,")"))
  dane_szkola2 <- dplyr::filter_(dane, paste0("id_szkoly==", szkola2_id, " & !is.na(", nazwa.kolumny,")"))
  # Czy kolumna typu liczbowego?
  czy.liczbowe <- is.numeric(dane[[nazwa.kolumny]])
  # Różne wykresy zależnie od typu kolumny
  wykres <- if(czy.liczbowe){
    # Dla numerycznych kolumn generuj wykresy "funkcji gęstości"
    ggplot2::ggplot() +
      ggplot2::geom_freqpoly(data=dane_szkoly1, ggplot2::aes_string(x=nazwa.kolumny, "..density..", colour='"red"'), bins = 10) +
      ggplot2::geom_freqpoly(data=dane_szkoly2, ggplot2::aes_string(x=nazwa.kolumny, "..density..", colour='"blue"'), bins = 10) +
      ggplot2::labs(title = opis) +
      ggplot2::xlab('Wartość wyznacznika') +
      ggplot2::ylab('Częstotliwość') + 
      ggplot2::theme(legend.position = "right",
                     panel.background = ggplot2::element_rect(fill = "white"),
                     panel.grid.major = ggplot2::element_line(colour = "#D1D1C1"),
                     legend.key = ggplot2::element_rect(fill = "white")) +
      ggplot2::scale_colour_identity(name="Legenda", guide = "legend", labels = c("red" = szkola1_id, "blue" = szkola2_id))
  } else {
    # Dla nie-numerycznych kolumn generuj stackowane wykresy słupkowe
    ggplot2::ggplot() +
      ggplot2::geom_bar(data=dane_szkoly1, ggplot2::aes_string(fill=nazwa.kolumny, y="100*..count../sum(..count..)", x=szkola1_id),position = "stack", na.rm = TRUE)+
      ggplot2::geom_bar(data=dane_szkoly2, ggplot2::aes_string(fill=nazwa.kolumny, y="100*..count../sum(..count..)", x=szkola2_id),position = "stack", na.rm = TRUE)+
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
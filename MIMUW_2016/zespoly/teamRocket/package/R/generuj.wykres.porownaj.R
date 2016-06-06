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
generuj.wykres.porownaj <- function(dane, nazwa.kolumny, szkola1_id, szkola2_id, opis) {
  # Czy kolumna typu liczbowego?
  czy.liczbowe <- is.numeric(dane[[nazwa.kolumny]])
  # Różne wykresy zależnie od typu kolumny
  wykres <- if(czy.liczbowe){
    # Odfiltrowywanie danych
    dane_szkola1 <- dplyr::filter_(dane, paste0("id_szkoly==", szkola1_id))
    dane_szkola2 <- dplyr::filter_(dane, paste0("id_szkoly==", szkola2_id))
    # Odzyskaj nazwę szkoły
    nazwa.szkola.1 <- dplyr::first(dane_szkola1$nazwa_szkoly) %>% strwrap(30) %>% paste(collapse="\n") %>% paste0("\n")
    nazwa.szkola.2 <- dplyr::first(dane_szkola2$nazwa_szkoly) %>% strwrap(30) %>% paste(collapse="\n") %>% paste0("\n")
    # Dla numerycznych kolumn generuj wykresy "funkcji gęstości"
    ggplot2::ggplot() +
      ggplot2::geom_freqpoly(data=dane_szkola1, ggplot2::aes_string(x=nazwa.kolumny, "..density..", colour='"red"'), bins = 10) +
      ggplot2::geom_freqpoly(data=dane_szkola2, ggplot2::aes_string(x=nazwa.kolumny, "..density..", colour='"blue"'), bins = 10) +
      ggplot2::labs(title = opis) +
      ggplot2::xlab('Wartość wyznacznika') +
      ggplot2::ylab('Częstotliwość') + 
      ggplot2::theme(legend.position = "right",
                     panel.background = ggplot2::element_rect(fill = "white"),
                     panel.grid.major = ggplot2::element_line(colour = "#D1D1C1"),
                     legend.key = ggplot2::element_rect(fill = "white")) +
      ggplot2::guides(colour = ggplot2::guide_legend(nrow=2, ncol=1))+
      ggplot2::scale_colour_identity(name="", guide = "legend", labels = c("red" = nazwa.szkola.1, "blue" = nazwa.szkola.2))
  } else {
    # Odfiltrowywanie danych
    dane_szkola1 <- dplyr::filter_(dane, paste0("id_szkoly==", szkola1_id, " & !is.na(", nazwa.kolumny,")"))
    dane_szkola2 <- dplyr::filter_(dane, paste0("id_szkoly==", szkola2_id, " & !is.na(", nazwa.kolumny,")"))
    # Odzyskaj nazwę szkoły
    nazwa.szkola.1 <- dplyr::first(dane_szkola1$nazwa_szkoly) %>% strwrap(30) %>% paste(collapse="\n") %>% paste0("\n")
    nazwa.szkola.2 <- dplyr::first(dane_szkola2$nazwa_szkoly) %>% strwrap(30) %>% paste(collapse="\n") %>% paste0("\n")
    # Dla nie-numerycznych kolumn generuj stackowane wykresy słupkowe
    ggplot2::ggplot() +
      ggplot2::geom_bar(data=dane_szkola1, ggplot2::aes_string(fill=nazwa.kolumny, y="100*..count../sum(..count..)", x="nazwa.szkola.1"),position = "stack", na.rm = TRUE)+
      ggplot2::geom_bar(data=dane_szkola2, ggplot2::aes_string(fill=nazwa.kolumny, y="100*..count../sum(..count..)", x="nazwa.szkola.2"),position = "stack", na.rm = TRUE)+
      ggplot2::labs(title = opis) +
      ggplot2::xlab('') +
      ggplot2::ylab('Częstotliwość [%]') + 
      ggplot2::theme(legend.position = "right",
                     panel.background = ggplot2::element_rect(fill = "white"),
                     panel.grid.major = ggplot2::element_line(colour = "#D1D1C1"),
                     legend.key = ggplot2::element_rect(fill = "white"))+
      ggplot2::guides(fill=ggplot2::guide_legend(title="Wartość wyznacznika"))
  }
  return(wykres)
}
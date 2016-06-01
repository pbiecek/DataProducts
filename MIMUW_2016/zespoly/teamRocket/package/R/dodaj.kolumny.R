#' Dodaj nowe kolumny do data.frame'a
#' @param ramka data.frame do którego należy dodać nowe kolumny
#' @param lista.funkcji lista zawierająca pary postaci
#' ("nazwa_nowej_kolumny", "formuła_wyliczana_dla_każdego_wiersza")
#' @return ramka wraz z dodanymi nowymi kolumnami
#' @usage
#' x <- dplyr::data_frame(a=1:5, b=a+7)
#' l <- list(
#'   c("col_1", "a + b"),
#'   c("col_2_name", ~pmax(a*a, a*b - a*a))
#' )
#' y <- dodaj.kolumny(ramka = x, lista.funkcji = l)
dodaj.kolumny <- function(ramka, lista.funkcji){
  Reduce(function(df, funkcja)
    dplyr::mutate_(df, .dots=stats::setNames(funkcja[2], funkcja[1])),
    lista.funkcji, ramka)
}

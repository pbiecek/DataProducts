#'Uruchom aplikację shiny dla konkretynych danych i wyznaczników
#'@param dane tabela zawierajaca wyniki matur i wartości
#'wyznaczników dla poszczególnych uczniów. dla każdego ucznia jest
#'też id jego szkoły, skrót nazwy gminy i nazwa szkoły
#'@param lista.wyznaczników lista wyznaczników które mają być
#'przedstawione na wykresach. typu c("nazwa.kolumny", "opis.wyznacznika")
#'gdzie nazwa.kolumny to kolumna w tabeli dane
uruchom <- function(dane, lista.wyznacznikow){
  shiny::shinyApp(ui = maturiser.ui,
                  server = maturiser.server(dane, lista.wyznacznikow))
}

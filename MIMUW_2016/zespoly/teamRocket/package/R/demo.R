#' Wyświetl przykładową aplikację wykonaną przy pomocy
#' frameworku maturiser
#' @export
demo <- function() {
  utils::data(matura.2015)
  #lista dodatkowych wyznaczników do dodania do danych
  kolumny <- list(
    c(uw.matematyka$skrot, uw.matematyka$formula),
    c(uw.informatyka$skrot, uw.informatyka$formula),
    c(uw.prawo$skrot, uw.prawo$formula),
    c(uw.dziennikarstwo$skrot, uw.dziennikarstwo$formula),
    c(zdawalnosc$skrot, zdawalnosc$formula)
  )
  
  # lista kolumn dla których mają być prezentowane wykresy
  fajne.kolumny <- list(
    c("m_pol_p", "Polski podstawowy"),
    c("m_mat_p", "Matematyka podstawowa"),
    c("m_mat_r", "Matematyka rozszerzona"),
    c("m_fiz_r", "Fizyka rozszerzona"),
    c("m_wos_r", "WOS rozszerzony"),
    c(uw.matematyka$skrot, uw.matematyka$opis),
    c(uw.informatyka$skrot, uw.informatyka$opis),
    c(uw.prawo$skrot, uw.prawo$opis),
    c(uw.dziennikarstwo$skrot, uw.dziennikarstwo$opis),
    c(zdawalnosc$skrot, zdawalnosc$opis)

  )
  # dodaj kolumny na wyznaczniki
  ramka <- dodaj.kolumny(matura.2015, kolumny) 
  # uruchom aplikację
  uruchom(ramka, fajne.kolumny)
}

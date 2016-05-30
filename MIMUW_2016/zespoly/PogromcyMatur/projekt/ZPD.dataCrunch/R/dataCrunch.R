.przeladuj_regiony <- function(ctxt) {
  src = polacz()
  reg = pobierz_szkoly(src) %>%
    select(gmina_szkoly, powiat_szkoly, wojewodztwo_szkoly) %>%
    distinct() %>%
    rename(gmina = gmina_szkoly, powiat = powiat_szkoly, wojewodztwo = wojewodztwo_szkoly) %>%
    collect() %>%
    as.data.frame()
  ctxt@regiony <- reg
  return(ctxt)
}

.zaladuj_nowe_szkoly <- function(ctxt, p_rok) {
  src = polacz()
  nowe = pobierz_szkoly(src) %>%
    filter(rok == p_rok) %>%
    select(id_szkoly, gmina_szkoly, powiat_szkoly, wojewodztwo_szkoly, nazwa_szkoly, typ_szkoly, rok) %>%
    rename(id = id_szkoly, gmina = gmina_szkoly, powiat = powiat_szkoly, wojewodztwo = wojewodztwo_szkoly,
           nazwa = nazwa_szkoly, typ = typ_szkoly) %>%
    collect() %>%
    as.data.frame()
  ctxt@szkoly <- rbind(ctxt@szkoly, nowe)
  ctxt@szkoly <- ctxt@szkoly %>% distinct()
  return(ctxt)
}

.zaladuj_testy <- function(ctxt) {
  src = polacz()
  nowe = pobierz_testy(src) %>%
    filter(czy_egzamin == TRUE, dane_ewd == TRUE) %>%
    filter(rodzaj_egzaminu != "matura poprawkowa") %>% # TODO - jakos uwzgledniac
    select(id_testu, rodzaj_egzaminu, czesc_egzaminu, rok, arkusz) %>%
    rename(id = id_testu) %>%
    collect() %>%
    as.data.frame()
  ctxt@typy_testow <- rbind(ctxt@typy_testow, nowe)
  ctxt@typy_testow <- ctxt@typy_testow %>% unique()
  return(ctxt)
}

.przeladuj_kryteria <- function(ctxt) {
  src = polacz()
  kryt = pobierz_kryteria_oceny(src) %>%
    select(kryterium, l_punktow,
           numer_pytania, numer_kryterium, id_pytania, id_wiazki, tresc_pytania, tresc_wiazki, id_testu) %>%
    rename(id = kryterium, max_punktow = l_punktow) %>%
    collect() %>%
    distinct() %>%
    as.data.frame()
  ctxt@kryteria <- kryt
  return(ctxt)
}

.zaladuj_normy <- function(ctxt) {
  src = polacz()
  skalowania <- pobierz_skale(src) %>%
    filter(rodzaj_skali == 'zrównywanie', posiada_normy) %>%
    select(id_skali, id_testu, skalowanie, opis_skalowania)
  
  ctxt@normy <- pobierz_normy(src) %>%
    inner_join(skalowania) %>%
    select(id_testu, wartosc_zr, wartosc) %>%
    collect()
  return(ctxt)
}

.zaladuj_uczniow <- function(ctxt) {
  src = polacz()
  ctxt@uczniowie <- pobierz_uczniow(src) %>%
    select(id_obserwacji, plec) %>%
    as.data.frame()
  return(ctxt)
}

#' Pobiera dane pomocnicze niezależne od roku.
#' 
#' Część przetwarzanych danych pełni funkcję pomocniczą w przetwarzaniu
#' wyników egzaminów. Ta funkcja wczytuje je.
#' 
#' Dane pobierane w tej funkcji mogą zmieniać się co roku. Wywołanie jej
#' zastąpi poprzednie dane w dazie danych - aby zaktualizować dane, wystarczy
#' więc wywołać tę funkcję raz.
#' @param ctxt Obiekt klasy DataCrunch, w którym mamy umieścić dane.
#' @return Obiekt DataCrunch z umieszczonymi nowymi danymi.
#' @export
zaladuj_male_dane <- function (ctxt) {
  ctxt = ctxt %>%
    .przeladuj_regiony() %>%
    .przeladuj_kryteria() %>%
    .zaladuj_normy() %>%
    .zaladuj_ostatnie_przystapienia() %>%
    .zaladuj_uczniow() %>%
    .zaladuj_nowe_testy()
  return(ctxt)
}

.zaladuj_ostatnie_przystapienia <- function(ctxt) {
  # Nie ma odczuwalnej roznicy miedzy sciaganiem calych danych i robieniem selecta, wiec sciagamy cale dane
  src = polacz()
  
  przystapienia = filtruj_przystapienia(src, FALSE, 'matura', NULL, TRUE) %>% 
    select(id_obserwacji, rodzaj_egzaminu, rok) %>% collect()
  ctxt@ostatnie_przystapienia <- przystapienia
  
  przystapienia = filtruj_przystapienia(src, FALSE, 'egzamin gimnazjalny', NULL, TRUE) %>%
    select(id_obserwacji, rodzaj_egzaminu, rok) %>% collect()
  ctxt@ostatnie_przystapienia <- ctxt@ostatnie_przystapienia %>% rbind(przystapienia)
  
  przystapienia = filtruj_przystapienia(src, FALSE, 'sprawdzian', NULL, TRUE) %>%
    select(id_obserwacji, rodzaj_egzaminu, rok) %>% collect()
  ctxt@ostatnie_przystapienia <- ctxt@ostatnie_przystapienia %>% rbind(przystapienia)
  
  return(ctxt)
}

.dodaj_sumaryczne_oceny_uczniow <- function(ctxt, src, dane, row) {
  # Sumaryczne wyniki uczniow, zrownane
  sumy = rowSums(dane %>% select(starts_with("k_")))
  nowe_oceny_uczniow <- dane %>%
    mutate(wynik = sumy) %>%
    select(id_obserwacji, id_testu, id_szkoly, wynik) %>%
    inner_join(ctxt@normy, by = c("id_testu" = "id_testu", "wynik" = "wartosc")) %>%
    select(-wynik) %>%
    rename(wynik=wartosc_zr) %>%
    mutate(rodzaj_egzaminu = row$rodzaj_egzaminu, czesc_egzaminu = row$czesc_egzaminu, rok = row$rok)
  
  ctxt@oceny_uczniow <- ctxt@oceny_uczniow %>% rbind(as.data.frame(nowe_oceny_uczniow))
  return(ctxt)
}

.dodaj_oceny_po_poprzednich <- function(ctxt, src, dane, row) {
  uczniowie <- dane %>%
    select(id_obserwacji) %>%
    distinct()
  
  poprzednie_oceny = ctxt@oceny_uczniow %>%
    inner_join(uczniowie) %>%
    filter(rodzaj_egzaminu != row$rodzaj_egzaminu) %>% # Rozważaj tylko scisle wczesniejsze egzaminy
    inner_join(ctxt@ostatnie_przystapienia)
  
  poprzednie_egzaminy = poprzednie_oceny %>%
    select(rodzaj_egzaminu, czesc_egzaminu) %>%
    distinct()
  
  # Posortuj uczniow po poprzednich pisanych egzaminach
  for (j in 1:nrow(poprzednie_egzaminy)) {
    egz = poprzednie_egzaminy[j,]
    
    szer = poprzednie_oceny %>%
      inner_join(egz) %>%
      rename(rodzaj_poprzedni=rodzaj_egzaminu, czesc_poprzedni=czesc_egzaminu) %>%
      select(id_obserwacji, rodzaj_poprzedni, czesc_poprzedni, wynik) %>%
      inner_join(dane) %>%
      arrange(wynik) %>%
      rename(poprzedni_wynik = wynik)
    
    szer = szer %>%
      group_by(poprzedni_wynik, rodzaj_poprzedni, czesc_poprzedni) %>%
      mutate(liczba = n()) %>%
      mutate_each(funs(mean(., na.rm = TRUE)),
                  starts_with("k_")) %>%
      select(poprzedni_wynik, rodzaj_poprzedni, czesc_poprzedni, starts_with("k_"), liczba) %>%
      distinct() %>%
      gather(id_kryterium, wynik, starts_with("k_"), na.rm = TRUE) %>%
      mutate(rodzaj_egzaminu = row$rodzaj_egzaminu, czesc_egzaminu = row$czesc_egzaminu, rok = row$rok)
    
    ctxt@wyniki_po_egz <- ctxt@wyniki_po_egz %>% rbind(as.data.frame(szer))
  }
  return(ctxt)
}

.dodaj_oceny_po_plci <- function(ctxt, dane, row) {
  nowe = ctxt@uczniowie %>%
    select(id_obserwacji, plec) %>%
    filter(!is.na(plec)) %>% # Niektorzy uczniowie nie maja informacji o plci
    inner_join(dane)
  nowe = nowe %>%
    group_by(plec) %>%
    summarise_each(funs(mean(., na.rm = TRUE)),
                   starts_with("k_"))
  nowe = nowe %>%
    gather(id_kryterium, wynik, starts_with("k_"), na.rm = TRUE)
  nowe = nowe %>%
    mutate(rodzaj_egzaminu = row$rodzaj_egzaminu, czesc_egzaminu = row$czesc_egzaminu, rok = row$rok)
  ctxt@wyniki_po_plci <- ctxt@wyniki_po_plci %>% rbind(as.data.frame(nowe))
  return(ctxt)
}

#' Pozwala umieścić w danych wyniki danego egzaminu.
#' 
#' UWAGA: dane, które w bazie już się znajdują zostaną zignorowane. Jeśli chcesz wprowadzić
#' je ponownie, użyj funkcji usun_wyniki.
#' UWAGA: zalecane jest chronologiczne wprowadzanie danych.
#' Patrz vignette(package="ZPD.dataCrunch", topic="wyniki_po_egz).
#' @param ctxt Obiekt klasy DataCrunch, w którym mamy umieścić dane.
#' @param p_rok Rok egzaminu.
#' @param t_rodzaj Rodzaj egzaminu.
#' @param t_czesc Część egzaminu.
#' @param l_dane Dane pobrane przez pakiet ZPD funkcją pobierz_wyniki_egzaminu(..., czy_ewd = TRUE).
#' 
#' @return Obiekt danych DataCrunch z dodanymi danymi, jeśli dane zostały poprawnie pobrane
#' i wprowadzone do bazy. Niezmieniony obiekt, jeśli dane egzaminu już zostały wprowadzone.
#' @export
zaladuj_nowe_wyniki <- function(ctxt, p_rok, t_rodzaj, t_czesc, l_dane) {
  
  src = polacz()
  
  row = data.frame()
  row$rok = p_rok
  rok$rodzaj_egzaminu = t_rodzaj
  rok$czesc_egzaminu = t_czesc
    
  if (nrow(inner_join(row, ctxt@zapisane_testy)) > 0) # Mamy juz ten test
    return(ctxt)
  
    ctxt = ctxt %>%
      .dodaj_sumaryczne_oceny_uczniow(src, l_dane, row) %>%
      .dodaj_oceny_po_poprzednich(src, l_dane, row) %>%
      .dodaj_oceny_po_plci(l_dane, row)
    ctxt@zapisane_testy <- ctxt@zapisane_testy %>% rbind(as.data.frame(row))
  return(ctxt)
}

#' Usuwa dane z bazy z danego roku.
#' @param p_rok Rok, który chcemy usunąć.
#' @param ctxt Obiekt klasy DataCrunch, z którego mamy usunąć dane.
#' @return Obiekt DataCrunch z usuniętymi danymi.
#' @export
usun_wyniki <- function(ctxt, p_rok)
{
  ctxt@wyniki_po_egz <- ctxt@wyniki_po_egz %>% filter(rok != p_rok)
  ctxt@wyniki_po_plci <- ctxt@wyniki_po_plci %>% filter(rok != p_rok)
  ctxt@oceny_uczniow <- ctxt@oceny_uczniow %>% filter(rok != p_rok)
  ctxt@zapisane_testy <- ctxt@zapisane_testy %>% filter(rok != p_rok)
  return(ctxt)
}
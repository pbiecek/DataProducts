library(ZPD)
library(tidyr)

#load("bigData.RData")
#load("smallData.RData")

zapisz_male_dane <- function() {
  save(regiony, szkoly, typy_testow, kryteria, file="smallData.RData")
}

zapisz_duze_dane <- function() {
  save(oceny_szkol, wyniki_szkol, oceny_uczniow, ostatnie_przystapienia, file="bigData.RData")
}

przeladuj_regiony <- function() {
  src = polacz()
  reg = pobierz_szkoly(src) %>%
        select(gmina_szkoly, powiat_szkoly, wojewodztwo_szkoly) %>%
        distinct() %>%
        rename(gmina = gmina_szkoly, powiat = powiat_szkoly, wojewodztwo = wojewodztwo_szkoly) %>%
        collect() %>%
        as.data.frame()
  regiony <<- reg
}

zaladuj_nowe_szkoly <- function(p_rok) {
  src = polacz()
  nowe = pobierz_szkoly(src) %>%
         filter(rok == p_rok) %>%
         filter(!dla_doroslych, is.na(artystyczna), (is.na(specjalna) | !specjalna)) %>% #Odfiltruj bardzo nietypowe szkoly
         select(id_szkoly, gmina_szkoly, powiat_szkoly, wojewodztwo_szkoly, nazwa_szkoly, typ_szkoly, rok) %>%
         rename(id = id_szkoly, gmina = gmina_szkoly, powiat = powiat_szkoly, wojewodztwo = wojewodztwo_szkoly,
                nazwa = nazwa_szkoly, typ = typ_szkoly) %>%
         collect() %>%
         as.data.frame()
  szkoly <<- rbind(szkoly, nowe)
}

zaladuj_nowe_testy <- function(p_rok) {
  src = polacz()
  nowe = pobierz_testy(src) %>%
         filter(rok == p_rok, czy_egzamin == TRUE, dane_ewd == TRUE) %>%
         filter(rodzaj_egzaminu != "matura poprawkowa") %>% # TODO - jakos uwzgledniac    
         select(id_testu, rodzaj_egzaminu, czesc_egzaminu, rok) %>%
         rename(id = id_testu) %>%
         collect() %>%
         as.data.frame()
  typy_testow <<- rbind(typy_testow, nowe)
}

przeladuj_kryteria <- function() {
  src = polacz()
  kryt = pobierz_kryteria_oceny(src) %>%
         select(kryterium, l_punktow,
                numer_pytania, numer_kryterium, id_pytania, id_wiazki, tresc_pytania, tresc_wiazki) %>%
         rename(id = kryterium, max_punktow = l_punktow) %>%
         collect() %>%
         distinct() %>%
         as.data.frame()
  kryteria <<- kryt
}

zaladuj_male_dane <- function () {
  przeladuj_regiony()
  przeladuj_kryteria()
}

zaladuj_male_dane_rok <- function(rok) {
  zaladuj_nowe_szkoly(rok)
  zaladuj_nowe_testy(rok)
}

zaladuj_ostatnie_przystapienia <- function() {
  # Nie ma odczuwalnej roznicy miedzy sciaganiem calych danych i robieniem selecta, wiec sciagamy cale dane
  src = polacz()
  ostatnie_przystapienia <<- NULL
  
  przystapienia = filtruj_przystapienia(src, FALSE, 'matura', NULL, TRUE) %>% collect()
  ostatnie_przystapienia <<- ostatnie_przystapienia %>% rbind(przystapienia)
  
  przystapienia = filtruj_przystapienia(src, FALSE, 'egzamin gimnazjalny', NULL, TRUE) %>% collect()
  ostatnie_przystapienia <<- ostatnie_przystapienia %>% rbind(przystapienia)
  
  przystapienia = filtruj_przystapienia(src, FALSE, 'sprawdzian', NULL, TRUE) %>% collect()
  ostatnie_przystapienia <<- ostatnie_przystapienia %>% rbind(przystapienia)
  
  ostatnie_przystapienia <<- ostatnie_przystapienia %>% select(id_obserwacji, rodzaj_egzaminu, rok)
}

zaladuj_nowe_wyniki_szkol <- function(p_rok, t_rodzaj = NULL) {
  src = polacz()

  #Testy z podanego roku
  testy = pobierz_testy(src) %>%
          filter(rok == p_rok, czy_egzamin == TRUE, dane_ewd == TRUE) %>%
          filter(rodzaj_egzaminu != "matura poprawkowa") %>% # TODO - jakos uwzgledniac
          select(id_testu, rodzaj_egzaminu, czesc_egzaminu, rok)
  
  #Typowe szkoly
  t_szkoly = szkoly %>%
    filter(rok == p_rok) %>%
    select(id) # Gwarantowany distinct
  
  
  # Dla kazdego typu egzaminu pobierz dane oddzielnie, by zmiescic sie w pamieci
  testy_iter = testy %>%
               collect() %>%
               select(rodzaj_egzaminu, czesc_egzaminu, rok) %>%
               distinct()
  if (!is.null(t_rodzaj)) {
    testy_iter = testy_iter %>% filter(rodzaj_egzaminu == t_rodzaj)
  }
  # Wyniki szkol z poszczegolnych testow
  nowe_wyniki_szkol = NULL
  # Uczniowie, na podstawie ktorych obliczymy oceny szkol z poprzednich testow
  nnowi_uczniowie = NULL
  
  for (i in 1:nrow(testy_iter)) {
    row = testy_iter[i,]
    
    # Wyniki egzaminu
    dane = pobierz_wyniki_egzaminu(src, row$rodzaj_egzaminu, row$czesc_egzaminu, row$rok, TRUE) %>%
      collect() %>%
      select(-rok) %>%
      inner_join(t_szkoly, by = c("id_szkoly" = "id"))
    
    # id testow pasujacych do tego egzaminu
    ntesty = testy %>%
      filter(rodzaj_egzaminu == row$rodzaj_egzaminu, czesc_egzaminu == row$czesc_egzaminu, rok == row$rok) %>%
      select(id_testu)
   
    # Wyniki szkol z egzaminu
    wyniki = dane %>%
      group_by(id_szkoly) %>%
      summarise_each(funs(mean),
                     -id_szkoly, -id_obserwacji, -id_testu) %>%
      gather(id_kryterium, wynik, -id_szkoly) %>%
      rename(id = id_szkoly)
    
    nowe_wyniki_szkol = rbind(nowe_wyniki_szkol, wyniki)
    
    # Sumaryczne wyniki uczniow
    sumy = rowSums(dane %>% select(starts_with("k_")))
    nowe_oceny_uczniow = dane %>%
      mutate(wynik = sumy) %>%
      select(id_obserwacji, id_testu, id_szkoly, wynik) %>%
      mutate(rodzaj_egzaminu = row$rodzaj_egzaminu, czesc_egzaminu = row$czesc_egzaminu, rok = row$rok)
    
    oceny_uczniow <<- oceny_uczniow %>% rbind(nowe_oceny_uczniow)
    
    # Uczniowie, ktorzy pisali te egzaminy
    nowi_uczniowie = pobierz_dane_uczniowie_testy(src) %>%
      inner_join(ntesty) %>%
      inner_join(testy) %>%
      select(id_obserwacji, id_szkoly, rodzaj_egzaminu) %>%
      collect() %>%
      rename(obecny_egzamin = rodzaj_egzaminu)
    
    nnowi_uczniowie = rbind(nnowi_uczniowie, nowi_uczniowie)
  }
  
  
  nnowi_uczniowie = nnowi_uczniowie %>% distinct()
  
  # Wyfiltruj szkoly, dla których mamy bardzo malo uczniow w tym roczniku
  miarodajne_szkoly = nnowi_uczniowie %>%
    group_by(id_szkoly) %>%
    summarise(liczba = n()) %>%
    filter(liczba >= 30) %>%
    select(id_szkoly)
  
  nowe_wyniki_szkol = nowe_wyniki_szkol %>%
    inner_join(miarodajne_szkoly, by = c("id" = "id_szkoly"))

  # Dla nowych uczniow polacz ich ze wszystkimi egzaminami, jakie wczesniej pisali
  nowe_oceny = oceny_uczniow %>%
    inner_join(nnowi_uczniowie) %>%
    inner_join(miarodajne_szkoly) %>%
    filter(obecny_egzamin != rodzaj_egzaminu) %>% # Rozważaj tylko scisle wczesniejsze egzaminy
    inner_join(ostatnie_przystapienia) %>%
    select(id_obserwacji, id_szkoly, id_testu, rodzaj_egzaminu, czesc_egzaminu, rok, wynik)
  
  # Pobierz skale zrownujace
  skalowania = pobierz_skale(src) %>%
    filter(rodzaj_skali == 'zrównywanie', posiada_normy) %>%
    select(id_skali, id_testu, skalowanie, opis_skalowania)
  normy = pobierz_normy(src) %>%
    inner_join(skalowania) %>%
    select(id_testu, wartosc_zr, wartosc) %>%
    collect()
  
  # Zaaplikuj skale zrownujace i usrednij wyniki po typach egzaminow w ramach szkol
  nowe_oceny = nowe_oceny %>%
    inner_join(normy, by = c("id_testu" = "id_testu", "wynik" = "wartosc"), copy = TRUE) %>%
    select(id_szkoly, id_obserwacji,
           rodzaj_egzaminu, czesc_egzaminu,
           wartosc_zr) %>%
    group_by(id_szkoly, rodzaj_egzaminu, czesc_egzaminu) %>%
    summarise(sredni_wynik = mean(wartosc_zr))
  nowe_oceny = nowe_oceny %>%
    mutate(rok = p_rok)
  # W nowe_oceny mamy wyniki poprzednich egzaminow rocznika, który pisal te egzaminy, unormowane i usrednione po szkolach
  
  oceny_szkol <<- rbind(oceny_szkol, nowe_oceny)
  wyniki_szkol <<- rbind(wyniki_szkol, nowe_wyniki_szkol)
}

zaladuj_duze_dane <- function() {
  zaladuj_ostatnie_przystapienia()
}

zaladuj_duze_dane_rok <- function(rok) {
  zaladuj_nowe_wyniki_szkol(rok)
}

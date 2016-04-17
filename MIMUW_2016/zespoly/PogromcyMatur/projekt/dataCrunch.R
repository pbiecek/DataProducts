library(ZPD)

load("bigData.RData")
load("smallData.RData")

zapisz_male_dane <- function() {
  save(regiony, szkoly, typy_testow, kryteria, file="smallData.RData")
}

zapisz_duze_dane() <- function() {
  save(oceny_szkol, wyniki_szkol, uczniowie_szkoly, oceny_uczniow, file="bigData.RData")
}

przeladuj_regiony <- function() {
  src = polacz()
  reg = pobierz_szkoly(src) %>%
        select(gmina_szkoly, powiat_szkoly, wojewodztwo_szkoly) %>%
        distinct(select(gmina_szkoly, powiat_szkoly, wojewodztwo_szkoly)) %>%
        rename(gmina = gmina_szkoly, powiat = powiat_szkoly, wojewodztwo = wojewodztwo_szkoly) %>%
        collect() %>%
        as.data.frame()
  regiony <<- reg
}

zaladuj_nowe_szkoly <- function(rok) {
  src = polacz()
  nowe = pobierz_szkoly(src) %>%
         filter(rok == rok) %>%
         select(id_szkoly, gmina_szkoly, powiat_szkoly, wojewodztwo_szkoly, nazwa_szkoly, typ_szkoly, rok) %>%
         rename(id = id_szkoly, gmina = gmina_szkoly, powiat = powiat_szkoly, wojewodztwo = wojewodztwo_szkoly,
                nazwa = nazwa_szkoly, typ = typ_szkoly) %>%
         collect() %>%
         as.data.frame()
  szkoly <<- rbind(szkoly, nowe)
}

zaladuj_nowe_testy <- function(rok) {
  src = polacz()
  nowe = pobierz_testy(src) %>%
         filter(rok == rok) %>%
         select(id_testu, rodzaj_egzaminu, czesc_egzaminu, rok) %>%
         rename(id = id_testu) %>%
         collect() %>%
         as.data.frame()
  typy_testow <<- rbind(typy_testow, nowe)
}

przeladuj_kryteria <- function(rok) {
  src = polacz()
  kryt = pobierz_kryteria_oceny(src) %>%
         select(kryterium, numer_pytania, id_wiazki, numer_kryterium, id_testu, l_punktow) %>%
         rename(id = kryterium, max_punktow = l_punktow) %>%
         collect() %>%
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

zaladuj_nowe_wyniki_szkol <- function(rok) {
  src = polacz()

  testy = pobierz_testy(src) %>%
          filter(rok == rok) %>%
          select(id_testu)
  dane = pobierz_odpowiedzi() %>%
        join(testy) %>%
        filter(grepl("k", kryterium)) %>% # odfiltruj tylko prawdziwe kryteria
        select(id_szkoly, id_testu, id_obserwacji, kryterium, ocena) %>%
        collect()

  wyniki = dane %>%
           group_by(id_szkoly, id_testu, kryterium) %>%
           summarise(sredni_wynik = avg(ocena)) %>%
           rename(id = id_szkoly, id_kryterium = kryterium) %>%
           as.data.frame()
  wyniki_szkol <<- rbind(wyniki_szkol, wyniki)

  uczszk = pobierz_dane_uczniowie_testy(src) %>%
           join(testy) %>%
           select(id_obserwacji, id_szkoly) %>%
           distinct(select(id_obserwacji, id_szkoly)) %>%
           collect()

  uczniowie_szkoly <<- rbind(uczniowie_szkoly, uczszk) %>%
                       distinct(id_obserwacji, id_szkoly)

  oceny = dane %>%
          zsumuj_punkty() %>%
          select(id_obserwacji, id_testu, wynik)

  oceny_uczniow <<- rbind(oceny_uczniow, oceny)

  nowe_wyniki = join(oceny_uczniow, uczszk) %>%
          group_by(id_testu, id_szkoly) %>%
          summarise(sredni_wynik = avg(wynik)) %>%
          rename(id = id_szkoly) %>%
          as.data.frame()

  oceny_szkol <<- rbind(oceny_szkol, nowe_wyniki) %>%
                  distinct(select(id_szkoly, id_testu))
}

zaladuj_duze_dane_rok <- function(rok) {
  zaladuj_nowe_wyniki_szkol(rok)
}

library(ZPD)

src = polacz()

rocznik <- function (rok){
  uczniowie = pobierz_uczniow(src) %>%
    filter(rocznik == rok)
  ut = pobierz_dane_uczniowie_testy(src) %>%
    join(uczniowie) %>%
    select(id_szkoly, id_obserwacji) %>%
    unique(id_szkoly, id_obserwacji) %>%
  return(ut)
}

oceny_szkol_po_tescie <- function (id_testu, rok) {
  wyniki = pobierz_wyniki_testu(src, id_testu) %>%
    zsumuj_punkty(TRUE)
  uczniowie = rocznik(rok)
  os = join(uczniowie, wyniki) %>%
    select(id_szkoly, wynik) %>%
    group_by(id_szkoly) %>%
    summarize(id_szkoly, wynik_szkoly = mean(wynik))
  return(os)
}

wyniki_szkol_z_testu <- function (id_testu) {
  wyniki = pobierz_odpowiedzi(src) %>%
    filter(id_testu = id_testu) %>%
    select(id_szkoly, kryterium, ocena) %>%
    group_by(id_szkoly, kryterium) %>%
    summarize(id_szkoly,kryterium, wynik_szkoly=mean(wynik))
  return(wyniki)
}
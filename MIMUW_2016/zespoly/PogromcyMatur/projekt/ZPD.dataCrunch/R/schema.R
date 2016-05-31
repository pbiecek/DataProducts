DataCrunch <- setClass(
  "DataCrunch",
  
  slots = c(
    typy_testow = "data.frame",
    zapisane_testy = "data.frame",
    kryteria = "data.frame",
    kryteria_testy = "data.frame",
    normy = "data.frame",
    wyniki_po_egz = "data.frame",
    wyniki_po_plci = "data.frame",
    oceny_uczniow = "data.frame",
    ostatnie_przystapienia = "data.frame",
    uczniowie = "data.frame"
  ),
  
  prototype = list(
    typy_testow = data.frame(),
    zapisane_testy = data.frame(),
    kryteria = data.frame(),
    kryteria_testy = data.frame(),
    normy = data.frame(),
    wyniki_po_egz = data.frame(),
    wyniki_po_plci = data.frame(),
    oceny_uczniow = data.frame(),
    ostatnie_przystapienia = data.frame(),
    uczniowie = data.frame()
  )
)

#' Tworzy nową grupę pustych danych.
#' @export
puste_dane <- function() {

  typy_testow <- data.frame()
  typy_testow$id <- integer(0)
  typy_testow$rodzaj_egzaminu <- character(0)
  typy_testow$czesc_egzaminu <- character(0)
  typy_testow$rok <- integer(0)
  typy_testow$arkusz <- character(0)
  
  zapisane_testy <- data.frame()
  zapisane_testy$rok <- integer(0)
  zapisane_testy$rodzaj_egzaminu <- character(0)
  zapisane_testy$czesc_egzaminu <- character(0)

  kryteria <- data.frame()
  kryteria$id <- character(0)
  kryteria$id_pytania <- character(0)
  kryteria$id_wiazki <- character(0)
  kryteria$tresc_pytania <- character(0)
  kryteria$tresc_wiazki <- character(0)
  kryteria$numer_kryterium <- character(0)
  kryteria$max_punktow <- integer(0)

  kryteria_testy <- data.frame()
  kryteria_testy$id <- character(0)
  kryteria_testy$numer_pytania <- character(0)
  kryteria_testy$id_testu <- integer(0)
  
  wyniki_po_egz <- data.frame()
  wyniki_po_egz$rok <- numeric(0)
  wyniki_po_egz$poprzedni_wynik <- numeric(0)
  wyniki_po_egz$liczba <- integer(0)
  wyniki_po_egz$rodzaj_egzaminu <- character(0)
  wyniki_po_egz$czesc_egzaminu <- character(0)
  wyniki_po_egz$rodzaj_poprzedni <- character(0)
  wyniki_po_egz$czesc_poprzedni <- character(0)
  wyniki_po_egz$id_kryterium <- numeric(0)
  wyniki_po_egz$wynik <- numeric(0)

  wyniki_po_plci <- data.frame()
  wyniki_po_plci$rok <- numeric(0)
  wyniki_po_plci$rodzaj_egzaminu <- character(0)
  wyniki_po_plci$czesc_egzaminu <- character(0)
  wyniki_po_plci$plec <- character(0)
  wyniki_po_plci$id_kryterium <- numeric(0)
  wyniki_po_plci$wynik <- numeric(0)

  # Tabele pomocnicze
  oceny_uczniow <- data.frame()
  oceny_uczniow$id_testu <- integer(0)
  oceny_uczniow$id_szkoly <- integer(0)
  oceny_uczniow$rodzaj_egzaminu <- character(0)
  oceny_uczniow$czesc_egzaminu <- character(0)
  oceny_uczniow$id_obserwacji <- integer(0)
  oceny_uczniow$wynik <- numeric(0)

  ostatnie_przystapienia <- data.frame()
  ostatnie_przystapienia$id_testu <- integer(0)
  ostatnie_przystapienia$rodzaj_egzaminu <- character(0)
  ostatnie_przystapienia$czesc_egzaminu <- character(0)
  ostatnie_przystapienia$rok <- integer(0)
  
  normy <- data.frame()
  normy$id_testu <- integer(0)
  normy$wartosc_zr <- numeric(0)
  normy$wartosc <- numeric(0)
  
  ostatnie_przystapienia <- data.frame()
  ostatnie_przystapienia$id_obserwacji <- integer(0)
  ostatnie_przystapienia$rodzaj_egzaminu <- character(0)
  ostatnie_przystapienia$rok <- integer(0)
  
  uczniowie <- data.frame()
  uczniowie$id_obserwacji <- integer(0)
  uczniowie$plec <- character(0)
  
  c = DataCrunch()
  c@typy_testow= typy_testow
  c@zapisane_testy = zapisane_testy
  c@kryteria = kryteria
  c@kryteria_testy = kryteria_testy
  c@normy = normy
  c@wyniki_po_egz = wyniki_po_egz
  c@wyniki_po_plci = wyniki_po_plci
  c@oceny_uczniow = oceny_uczniow
  c@ostatnie_przystapienia = ostatnie_przystapienia
  c@uczniowie = uczniowie
  
  return(c)
}

init <<- function() {

  regiony <<- data.frame()
  regiony$wojewodztwo <<- character(0)
  regiony$powiat <<- character(0)
  regiony$gmina <<- character(0)

  szkoly <<- data.frame()
  szkoly$id <<- integer(0)
  szkoly$rok <<- integer(0)
  szkoly$gmina <<- character(0)
  szkoly$powiat <<- character(0)
  szkoly$wojewodztwo <<- character(0)
  szkoly$nazwa <<- character(0)
  szkoly$typ <<- character(0)

  typy_testow <<- data.frame()
  typy_testow$id <<- integer(0)
  typy_testow$rodzaj_egzaminu <<- character(0)
  typy_testow$czesc_egzaminu <<- character(0)
  typy_testow$rok <<- integer(0)

  oceny_szkol <<- data.frame()
  oceny_szkol$id <<- integer(0)
  oceny_szkol$id_testu <<- integer(0)
  oceny_szkol$sredni_wynik <<- numeric(0)

  wyniki_szkol <<- data.frame()
  wyniki_szkol$id <<- integer(0)
  wyniki_szkol$id_testu <<- integer(0)
  wyniki_szkol$sredni_wynik <<- numeric(0)
  wyniki_szkol$id_kryterium <<- character(0)

  kryteria <<- data.frame()
  kryteria$id <<- character(0)
  kryteria$numer_pytania <<- character(0)
  kryteria$id_wiazki <<- integer(0)
  kryteria$numer_kryterium <<- character(0)
  kryteria$id_testu <<- integer(0)
  kryteria$max_punktow <<- integer(0)
}

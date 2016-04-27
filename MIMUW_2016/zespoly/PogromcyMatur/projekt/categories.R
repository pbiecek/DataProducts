library(ZPD)

src = polacz()

regiony <- function() {
  ret = pobierz_szkoly(src) %>%
    select(wojewodztwo_szkoly, powiat_szkoly, gmina_szkoly) %>%
    unique(wojewodztwo_szkoly, powiat_szkoly, gmina_szkoly) %>%
    collect()
  return(ret)
}

typy_testow <- function() {
  ret = pobierz_testy(src) %>%
    select(id_testu, rodzaj_egzaminu, rok, czesc_egzaminu) %>%
    collect()
  return(ret)
}

regiony_szkoly <- function() {
  ret = pobierz_szkoly(src) %>%
    select(id_szkoly, gmina_szkoly, powiat_szkoly, wojewodztwo_szkoly, rok) %>%
    collect()
  return(ret)
}
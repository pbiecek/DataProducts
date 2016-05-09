# Ustaw 'curret working directory' na lokalizację pliku - pozwala na zapisywanie/odczytywanie danych do/z pliku
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

library(dplyr)
library(ggplot2)

generate_single <- function(ido_cke, czesc, szk) {
  if(czesc[1] == "r") {
    czesc_d <- paste("rozsz_", czesc[2], sep='')
  } else {
    czesc_d <- paste("pods_", czesc[2], sep='')
  }
  czesc_s <-paste(czesc, sep="", collapse="_")

  load(file = paste("../raw_data/ZPD", czesc_d, "2015.dat", sep="_"))
  dt <-get(paste("mt", czesc_d, "2015", sep="_"))
  l_kol <- ncol(dt)
  #print(dt)
  dt %>%
    inner_join(szk, by = "id_szkoly") %>%
    mutate_(.dots=setNames(
      list(lazyeval::interp(~(rowSums(.[5 : l_kol ], na.rm = TRUE)))),
           czesc_s)) %>%
    select_("id_obserwacji", czesc_s, "id_szkoly") -> wyn
  inner_join(wyn, ido_cke, by="id_obserwacji") %>%
    select_("id_cke", czesc_s, "id_szkoly") %>%
    rename(id_szkoly_new = id_szkoly)
}

generate_all <- function(ucz, szk, czesci) {
  Reduce(function(a, b) {
    rename(a, id_szkoly_old = id_szkoly) %>%
      full_join(generate_single(ucz, b, szk),
                                  by = "id_cke") %>%
      mutate(id_szkoly = ifelse(is.na(id_szkoly_old), id_szkoly_new, id_szkoly_old)) %>%
      select(-id_szkoly_old, -id_szkoly_new)
    },
         czesci, data.frame(id_cke = character(0), id_szkoly = integer(0)))
}


szkoly %>%
  filter(powiat_szkoly == "Warszawa", rok==2015) %>%
  select(id_szkoly, nazwa_szkoly) -> szkoly_wawa_2015

# Jak użyć?
# zasource'uj ten plik
# wywoływanie:
# lista.matur = list(c("p", "mat"), c("r", "mat")) ## <-- p - podstawa, r - rozszerzenie
# generate_all(ucz, szkoly_wawa_2015, lista.matur) ## <-- lista może być dłuższa

generuj.1.iter <- function() {
  load(file = "../raw_data/ZPD_ucz.dat")
  load(file = "../raw_data/ZPD_szkoly.dat")
  lista.matur = list(
    c("p", "mat"),
    c("p", "pl"),
    c("p", "ang"),
    c("p", "fiz"),
    c("p", "bio"),
    c("p", "chem"),
    c("p", "inf"),
    c("r", "mat"),
    c("r", "pl"),
    c("r", "ang"),
    c("r", "fiz"),
    c("r", "bio"),
    c("r", "chem"),
    c("r", "inf")
  )
  data.wawa <- generate_all(ucz, szkoly_wawa_2015, lista.matur)
  save(data.wawa, file = "ZPD_iter1.dat")
}

# demo użycia z ggplot
rysuj.matma <- function() {
  load("ZPD_iter1.dat")
  ggplot(data.wawa, aes(x= r_mat, y = p_mat)) + geom_density2d()
}
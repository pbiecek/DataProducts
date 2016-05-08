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
  czesc_s <-paste(czesc[1], czesc[2], sep="_", collapse="")

  load(file = paste("../raw_data/ZPD", czesc_d, "2015.dat", sep="_"))
  dt <-get(paste("mt", czesc_d, "2015", sep="_"))
  l_kol <- ncol(dt)
  mx_sum <- strtoi(czesc[3]) / 100
  #print(mx_sum)
  dt %>%
    inner_join(szk, by = "id_szkoly") %>%
    mutate_(.dots=setNames(
      list(~(rowSums(.[5 : l_kol ], na.rm = TRUE)/mx_sum)),
           czesc_s)) %>%
    select_("id_obserwacji", czesc_s, "id_szkoly") -> wyn
  inner_join(wyn, ido_cke, by="id_obserwacji") %>%
    select_("id_cke", czesc_s, "id_szkoly") %>%
    rename(id_szkoly_new = id_szkoly) -> wyn
  print(paste("Generated ", czesc[1], czesc[2]))
  return(wyn)
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


# Jak użyć?
# zasource'uj ten plik
# wywoływanie:
# lista.matur = list(c("p", "mat"), c("r", "mat")) ## <-- p - podstawa, r - rozszerzenie
# generate_all(ucz, szkoly_wawa_2015, lista.matur) ## <-- lista może być dłuższa

generuj.3.iter <- function() {
  load(file = "../raw_data/ZPD_ucz.dat")
  load(file = "../raw_data/ZPD_szkoly.dat")
  szkoly %>%
    filter(rok==2015) %>%
    filter(typ_szkoly %in% c("LO", "LOU","LP", "T", "TU", "ZZ")) %>%
    select(id_szkoly, nazwa_szkoly) -> szkoly.2015
  
  #lista dobrana mniej-więcej eksperymentalnie. potrzebne konkretniejsze dane :P
  lista.matur = list(
    c("p", "mat", 50),
    c("p", "pl", 70),
    c("p", "ang", 50),
    c("p", "fiz", 50),
    c("p", "bio", 50),
    c("p", "chem", 50),
    c("p", "inf", 60),
    c("r", "mat", 50),
    c("r", "pl", 40),
    c("r", "ang", 50),
    c("r", "fiz", 60),
    c("r", "bio", 47),
    c("r", "chem", 48),
    c("r", "inf", 50)
  )
  data.all.pl <- generate_all(ucz, szkoly.2015, lista.matur)
  save(data.all.pl, file = "ZPD_iter3.dat")
  return(data.all.pl)
}
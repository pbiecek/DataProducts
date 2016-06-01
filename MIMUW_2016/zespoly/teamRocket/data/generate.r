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
  czesc_s <-paste("m", czesc[2], czesc[1], sep="_", collapse="")
  cat(paste0("Working on ", czesc_s, "\n"))
  load(file = paste("../raw_data/ZPD", czesc_d, "2015.dat", sep="_"))
  dt <-get(paste("mt", czesc_d, "2015", sep="_"))
  l_kol <- ncol(dt)
  mx_sum <- strtoi(czesc[3]) / 100
  dt %>%
    inner_join(szk, by = "id_szkoly") %>%
    mutate_(.dots=setNames(
      list(~(rowSums(.[grepl("^[pk]_[0-9]+$", names(.))], na.rm = TRUE))), czesc_s)) %>%
    mutate_(.dots=setNames(paste(czesc_s, "* 100 / max(",czesc_s,")"),
                           czesc_s)) %>%
    inner_join(ido_cke, by="id_obserwacji") %>%
    select_("id_cke", "id_szkoly", "nazwa_szkoly", "gmina_szkoly",czesc_s ) -> wyn
  cat(paste("Generated ", czesc_s,"\n"))
  return(wyn)
}

generate_all <- function(ucz, szk, czesci) {
  Reduce(function(a, b) {
      full_join(a, generate_single(ucz, b, szk), by = NULL)
    },
         czesci[-1], generate_single(ucz, first(czesci), szk))
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
    select(id_szkoly, nazwa_szkoly, gmina_szkoly) -> szkoly.2015
  
  #lista dobrana mniej-więcej eksperymentalnie. potrzebne konkretniejsze dane :P
  lista.matur = list(
    c("p", "mat"),
    c("p", "pl"),
    c("p", "ang"),
    c("p", "fiz"),
    c("p", "bio"),
    c("p", "chem"),
    c("p", "inf"),
    c("p", "wos"),
    c("p", "his"),
    c("p", "geo"),
    c("r", "mat"),
    c("r", "pl"),
    c("r", "ang"),
    c("r", "fiz"),
    c("r", "bio"),
    c("r", "chem"),
    c("r", "inf"),
    c("r", "wos"),
    c("r", "his"),
    c("r", "geo")
  )
  data.all.pl <- generate_all(ucz, szkoly.2015, lista.matur) %>%
    rename(m_che_p = m_chem_p, m_pol_p = m_pl_p, m_che_r = m_chem_r, m_pol_r = m_pl_r)
  #save(data.all.pl, file = "ZPD_iter3.dat")
  return(data.all.pl)
}
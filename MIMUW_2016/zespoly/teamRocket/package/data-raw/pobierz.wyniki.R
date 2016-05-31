# Ustaw 'curret working directory' na lokalizację pliku
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

require(dplyr)

nazwy.czesc <- list(
  "pol" = "j. polski",
  "mat" = "matematyka",
  "ang" = "j. angielski",
  "fiz" = "fizyka",
  "bio" = "biologia",
  "che" = "chemia",
  "inf" = "informatyka",
  "geo" = "geografia",
  "his" = "historia",
  "wos" = "WOS"
)
nazwy.poziom <- list(
  "p" = "podstawowa",
  "r" = "rozszerzona"
)


pobierz.pojedynczy <- function(src, ucz, czesc = "err1", poziom = "err2", szk, ktory.rok, ktory=-1, razem=0) {
  nazwa.czesci <- paste(nazwy.czesc[czesc], nazwy.poziom[poziom])
  nazwa.kolumny <- paste("m", czesc, poziom, sep="_")
  cat(sprintf("[%d/%d] Pobieranie '%s' jako kolumna '%s'.\n",
              ktory, razem, nazwa.czesci, nazwa.kolumny))
  ZPD::pobierz_wyniki_egzaminu(src, rodzajEgzaminu = "matura",
                                     czescEgzaminu = nazwa.czesci, rokEgzaminu = ktory.rok,
                                     czyEwd = TRUE) %>%
    dplyr::inner_join(szk, by = "id_szkoly") %>%
    dplyr::inner_join(ucz, by = "id_obserwacji") %>%
    collect() %>%
    mutate_(.dots=setNames(
      list(~(rowSums(.[grepl("^[pk]_[0-9]+$", names(.))], na.rm = TRUE))), nazwa.kolumny)) %>%
    mutate_(.dots=setNames(paste(nazwa.kolumny, "* 100 / max(",nazwa.kolumny,")"),
                           nazwa.kolumny)) %>%
    dplyr::select_("gmina_szkoly","id_cke", "id_szkoly", nazwa.kolumny)
}


#' Pobierz wyniki z konkretnego roku
pobierz.wyniki <-function(ktory.rok){
  if(!requireNamespace("ZPD", quietly = TRUE)){
    stop("Pakiet ZPD jest konieczny do pobrania danych. Zainstaluj go z [github: zozlak/ZPD].",
         call. = FALSE)
  }
  
  # uchwyt danych
  src <- ZPD::polacz()
  #lista uczniów - jeszcze nie pobrana
  ZPD::pobierz_uczniow(src) %>%
    dplyr::select(id_cke, id_obserwacji) %>%
    dplyr::filter(rocznik == ktory.rok-18 |
                  rocznik == ktory.rok-19 |
                  rocznik == ktory.rok-20) -> uczniowie
  # lista szkół - jeszcze nie pobrana
  ZPD::pobierz_szkoly(src) %>%
    dplyr::filter(typ_szkoly %in%
                    c("LO", "LOU","LP", "T", "TU", "ZZ")) %>% #Tylko szkoły średnie/zawodowe
    dplyr::filter(rok == ktory.rok) %>%
    dplyr::select(id_szkoly, nazwa_szkoly, gmina_szkoly, powiat_szkoly, miejscowosc) -> szkoly
  
  # lista.matur = list("wos", "his", "mat", "pol", "ang", "fiz","bio", "che", "inf", "mat", "geo")
  lista.matur = list("inf", "his") # do szybkich testów!
  lista.egz = expand.grid(czesc = lista.matur, poziom = list("p", "r"))
  # pobierz wszystkie konieczne wyniki
  wyniki = Map(function(x, y, n) pobierz.pojedynczy(src,uczniowie, czesc = x, poziom = y,
                                                    szkoly, ktory.rok, n, length(lista.egz$poziom)),
               lista.egz$czesc, lista.egz$poziom, 1:length(lista.egz$poziom))
  cat("Trwa złączanie. Czekaj...\n")
  wynik = Reduce(function(reszta, nowy){
    dplyr::full_join(reszta, nowy, by=c("id_cke", "id_szkoly", "gmina_szkoly"))
    }, wyniki[-1], dplyr::first(wyniki))
  
  nazwa.ramki = paste0("matura.", ktory.rok)
  assign(nazwa.ramki, wynik)
  do.call(save, list(nazwa.ramki, file=paste0("../data/matura.", ktory.rok, ".rda"), compress = TRUE))
}

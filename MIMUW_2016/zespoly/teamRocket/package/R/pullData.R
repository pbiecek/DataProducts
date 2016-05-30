#' Download data for a specified year
#' 
#' Execution can take up to a few hours!
#' @param year year for which to download data
pullData <-function(year){
  if(!requireNamespace("devtools", quietly = TRUE)){
    stop("Package devtools needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if(!requireNamespace("ZPD", quietly = TRUE)){
    stop("Package ZPD needed for this function to work. Please install it. (github: zozlak/ZPD)",
         call. = FALSE)
  }
  
  #@param czesc c(short_part_name, "p" or "r")
  gen_single <- function(src, ido_cke, czesc, szk, year) {
    part_names <- list(
      "pl" = "j. polski",
      "mat" = "matematyka",
      "ang" = "j. angielski",
      "fiz" = "fizyka",
      "bio" = "biologia",
      "chem" = "chemia",
      "inf" = "informatyka"
    )
    level_names <- list(
      "p" = "podstawowa",
      "r" = "rozszerzona"
    )
    
    part_name <- paste(part_names[czesc[1]], level_names[czesc[2]])
    col_name <- paste(czesc[2], czesc[1], sep="_")
    dt <- ZPD::pobierz_wyniki_egzaminu(src, rodzajEgzaminu = "matura",
                                       czescEgzaminu = part_name, rokEgzaminu = year,
                                       czyEwd = TRUE) %>%
      dplyr::inner_join(szk, by = "id_szkoly") %>%
      dplyr::inner_join(ido_cke, by="id_obserwacji") %>%
      collect() %>%
      mutate_(.dots=setNames(
        list(~(rowSums(.[grepl("^[pk]_[0-9]+$", names(.))], na.rm = TRUE))),
        col_name)) %>%
      mutate_(.dots=setNames(paste(col_name, "* 100 / max(",col_name,")"), col_name)) %>%
      dplyr::select_("id_cke", "id_szkoly", col_name)
  }
  
  src <- ZPD::polacz()
  ZPD::pobierz_uczniow(src) %>%
    dplyr::select(id_cke, id_obserwacji) %>%
    dplyr::filter(rocznik == year-18 |
                  rocznik == year-19 |
                  rocznik == year-20) -> ucz
  ZPD::pobierz_szkoly(src) %>%
    dplyr::filter(typ_szkoly %in%
                    c("LO", "LOU","LP", "T", "TU", "ZZ")) %>% #Tylko szkoły średnie/zawodowe
    dplyr::filter(rok == year) %>%
    dplyr::select(id_szkoly, nazwa_szkoly, gmina_szkoly, powiat_szkoly, miejscowosc) -> szkoly
  
  #gen_single(src, ucz, c("inf", "r"), szkoly, year)
  # TODO: looping with gen_single over list of c(part, lvl)  
}


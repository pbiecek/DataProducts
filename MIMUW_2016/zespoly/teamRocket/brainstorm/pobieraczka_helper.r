# Ustaw 'curret working directory' na lokalizację pliku - pozwala na zapisywanie/odczytywanie danych do/z pliku
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

library(ZPD)

src <- polacz()
lvl <- c("podstawowa", "rozszerzona")
lvl_d <- c("pods", "rozsz")
tem <- c("historia")
tem_d <- c("his")
czesc_egz <-levels(interaction(tem, lvl, sep=" "))
czesc_egz_d <-levels(interaction(lvl_d, tem_d, sep="_"))

download <- function(czesc, czesc_d) {
  src <- polacz()
  x <- pobierz_wyniki_egzaminu(src,
                               rodzajEgzaminu = "matura",
                               czescEgzaminu = czesc,
                               rokEgzaminu = 2015, TRUE)
  x <- collect(x)
  rozlacz(src)
  print(paste("Pobrano", czesc, sep=" "))
  assign(paste("mt", czesc_d, "2015", sep="_"), x)
  save(list=paste("mt", czesc_d, "2015", sep="_"), file = paste("ZPD", czesc_d, "2015.dat", sep="_"))
  print(paste("Zapisano", czesc, sep=" "))
}

mapply(download, czesc = czesc_egz, czesc_d = czesc_egz_d)
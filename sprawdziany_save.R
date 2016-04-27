library(ZPD)
src <- polacz()
sprawdziany <- list()

for (rok in as.character(2002, 2013, 2014, 2015)) {
  dane <- pobierz_wyniki_egzaminu(src, "sprawdzian", "", as.numeric(rok), FALSE)
  sprawdziany[rok] <- dane %>% collect()
}

for (rok in as.character(2003:2012)) {
  dane <- pobierz_wyniki_egzaminu(src, "sprawdzian", "", as.numeric(rok), TRUE)
  sprawdziany[rok] <- dane %>% collect()
}

save(sprawdziany, file <- "sprawdziany.RData")

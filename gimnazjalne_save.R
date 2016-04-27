library(ZPD)
src <- polacz()
gimnazjalne <- list()

for (rok in as.character(2002:2004)) {
  dane <- pobierz_wyniki_egzaminu(src, "egzamin gimnazjalny", "matematyczno-przyrodnicza", as.numeric(rok), FALSE)
  gimnazjalne_mat_przyr[rok] <- dane %>% collect()
}

for (rok in as.character(2005:2015)) {
  dane <- pobierz_wyniki_egzaminu(src, "egzamin gimnazjalny", "matematyczno-przyrodnicza", as.numeric(rok), TRUE)
  gimnazjalne_mat_przyr[rok] <- dane %>% collect()
}

save(gimnazjalne_mat_przyr, file <- "gim_mat_przyr.RData")

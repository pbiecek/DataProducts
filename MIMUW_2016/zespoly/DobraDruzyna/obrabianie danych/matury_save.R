library(ZPD)
src <- polacz()
matury <- list()

for (rok in as.character(2010:2015)) {
  dane <- pobierz_wyniki_egzaminu(src, "matura", "matematyka podstawowa", as.numeric(rok), TRUE)
  matury_mat_p[rok] <- dane %>% collect()
}

save(matury_mat_p, file <- "matury_mat_p.RData")
  
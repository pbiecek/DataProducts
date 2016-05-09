# Ustaw 'curret working directory' na lokalizację pliku - pozwala na zapisywanie/odczytywanie danych do/z pliku
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

library(ZPD)
library(rCharts)

czy_pobrac <- "nie"

if (czy_pobrac == "tak") {
  src <- polacz()
  ucz <-pobierz_uczniow(src)
  mt_pods_pl_2015 <- pobierz_wyniki_egzaminu(src, rodzajEgzaminu = "matura",czescEgzaminu = "j. polski podstawowa", 2015, TRUE)
  mt_pods_mat_2015 <- pobierz_wyniki_egzaminu(src, rodzajEgzaminu = "matura", czescEgzaminu = "matematyka podstawowa", 2015, TRUE)
  mt_rozsz_mat_2015 <- pobierz_wyniki_egzaminu(src, rodzajEgzaminu = "matura", czescEgzaminu = "matematyka rozszerzona", 2015, TRUE)
  mt_rozsz_pl_2015 <- pobierz_wyniki_egzaminu(src, rodzajEgzaminu = "matura", czescEgzaminu = "j. polski rozszerzona", 2015, TRUE)
  mt_pods_pl_2015 <- collect(mt_pods_pl_2015)
  mt_pods_mat_2015 <- collect(mt_pods_mat_2015)
  mt_rozsz_pl_2015 <- collect(mt_rozsz_pl_2015)
  mt_rozsz_mat_2015 <- collect(mt_rozsz_mat_2015)
  ucz <- collect(ucz)
  save(ucz, file = "ZPD_ucz.dat")
  save(mt_pods_pl_2015, file = "ZPD_pods_pl_2015.dat")
  save(mt_pods_mat_2015, file = "ZPD_pods_mat_2015.dat")
  save(mt_rozsz_pl_2015, file = "ZPD_rozsz_pl_2015.dat")
  save(mt_rozsz_mat_2015, file = "ZPD_rozsz_mat_2015.dat")
  
  print("Pobrano")
}

load(file = "ZPD_ucz.dat")
load(file = "ZPD_pods_mat_2015.dat")
load(file = "ZPD_pods_pl_2015.dat")
load(file = "ZPD_rozsz_mat_2015.dat")
load(file = "ZPD_rozsz_pl_2015.dat")

mt_pods_pl_2015 %>%
  mutate(suma_ppl = rowSums(.[5:67], na.rm = TRUE)) %>%
  select(id_obserwacji, suma_ppl) -> sumy_ppl_2015

mt_rozsz_pl_2015 %>%
  mutate(suma_rpl = rowSums(.[5:43], na.rm = TRUE)) %>%
  select(id_obserwacji, suma_rpl) -> sumy_rpl_2015

mt_pods_mat_2015 %>%
  mutate(suma_pmat = rowSums(.[5:67], na.rm = TRUE)) %>%
  select(id_obserwacji, suma_pmat) -> sumy_pmat_2015

mt_rozsz_mat_2015 %>%
  mutate(suma_rmat = rowSums(.[5:31], na.rm = TRUE)) %>%
  select(id_obserwacji, suma_rmat) -> sumy_rmat_2015

inner_join(sumy_pmat_2015, ucz, by = "id_obserwacji") %>%
  select(suma_pmat, id_cke) -> sumy_pmat_2015

inner_join(sumy_ppl_2015, ucz, by = "id_obserwacji") %>%
  select(suma_ppl, id_cke) -> sumy_ppl_2015

inner_join(sumy_rmat_2015, ucz, by = "id_obserwacji") %>%
  select(suma_rmat, id_cke) -> sumy_rmat_2015

inner_join(sumy_rpl_2015, ucz, by = "id_obserwacji") %>%
  select(suma_rpl, id_cke) -> sumy_rpl_2015

inner_join(sumy_ppl_2015, sumy_pmat_2015, by= "id_cke") %>%
  select(suma_pmat, suma_ppl) -> korelacja_pm_pp

inner_join(sumy_rmat_2015, sumy_pmat_2015, by= "id_cke") %>%
  select(suma_rmat, suma_pmat) -> korelacja_rm_pm

inner_join(sumy_rpl_2015, sumy_ppl_2015, by= "id_cke") %>%
  select(suma_rpl, suma_ppl) -> korelacja_rp_pp

inner_join(sumy_rmat_2015, sumy_ppl_2015, by= "id_cke") %>%
  select(suma_rmat, suma_ppl) -> korelacja_rm_pp

inner_join(sumy_rpl_2015, sumy_pmat_2015, by= "id_cke") %>%
  select(suma_rpl, suma_pmat) -> korelacja_rp_pm

inner_join(sumy_rpl_2015, sumy_rmat_2015, by= "id_cke") %>%
  select(suma_rpl, suma_rmat) -> korelacja_rp_rm

#smoothScatter(korelacja_pm_pp, nbin = 300,
#              xlab = "Wynik matury z matematyki podst",
#              ylab = "Wyniki matury z j. polskiego podst")

#smoothScatter(korelacja_rm_pm, nbin = 300,
#              xlab = "Wynik matury z matematyki rozsz",
#              ylab = "Wyniki matury z matematyki podst")

#smoothScatter(korelacja_rp_pp, nbin = 300,
#              xlab = "Wynik matury z j. polskiego rozsz",
#              ylab = "Wyniki matury z j. polskiego podst")

#smoothScatter(korelacja_rm_pp, nbin = 300,
#              xlab = "Wynik matury z matematyki rozsz",
#              ylab = "Wyniki matury z j. polskiego podst")

#smoothScatter(korelacja_rp_pm, nbin = 300,
#              xlab = "Wynik matury z j. polskiego rozsz",
#              ylab = "Wyniki matury z matematyki podst")

smoothScatter(korelacja_rp_rm, nbin = 300,
              xlab = "Wynik matury z j. polskiego rozsz",
              ylab = "Wyniki matury z matematyki rozsz")

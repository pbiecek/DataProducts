# Ustaw 'curret working directory' na lokalizację pliku - pozwala na zapisywanie/odczytywanie danych do/z pliku
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

library(dplyr)

load(file="../raw_data/ZPD_gm_mat_2012.dat")
load(file="../raw_data/ZPD_gm_pl_2012.dat")
load(file="../raw_data/ZPD_gm_his_2012.dat")
load(file="../raw_data/ZPD_gm_prz_2012.dat")
load(file="../raw_data/ZPD_ucz.dat")
load(file="../raw_data/ZPD_szkoly.dat")
load(file="ZPD_iter3.dat")


gm_mat_2012 %>%
  mutate(gm_mat = rowSums(.[5 : 27], na.rm = TRUE)*(100/30)) %>%
  select(gm_mat, id_obserwacji) -> sum_gm_mat_2012

gm_pl_2012 %>%
  mutate(gm_pl = rowSums(.[5 : 31], na.rm = TRUE)*(100/32)) %>%
  select(gm_pl, id_obserwacji) -> sum_gm_pl_2012

gm_his_2012 %>%
  mutate(gm_his = rowSums(.[5 : 37], na.rm = TRUE)*(100/33)) %>%
  select(gm_his, id_obserwacji) -> sum_gm_his_2012

gm_prz_2012 %>%
  mutate(gm_prz = rowSums(.[5 : 30], na.rm = TRUE)*(100/26)) %>%
  select(gm_prz, id_obserwacji) -> sum_gm_prz_2012

inner_join(x = ucz, y = sum_gm_mat_2012, by = NULL) %>%
  select(id_cke, gm_mat) -> proc_gm_mat_2012

inner_join(x = ucz, y = sum_gm_pl_2012, by = NULL) %>%
  select(id_cke, gm_pl) -> proc_gm_pl_2012

inner_join(x = ucz, y = sum_gm_his_2012, by = NULL) %>%
  select(id_cke, gm_his) -> proc_gm_his_2012

inner_join(x = ucz, y = sum_gm_prz_2012, by = NULL) %>%
  select(id_cke, gm_prz) -> proc_gm_prz_2012

szkoly %>%
  filter(rok==2015) %>%
  filter(typ_szkoly %in% c("LO", "LOU","LP", "T", "TU", "ZZ")) %>%
  select(id_szkoly, nazwa_szkoly) -> szkoly.2015

inner_join(data.all.pl, proc_gm_mat_2012, by= NULL) %>%
  inner_join(proc_gm_pl_2012, by= NULL) %>%
  inner_join(proc_gm_his_2012, by= NULL) %>%
  inner_join(proc_gm_prz_2012, by= NULL) %>%
  inner_join(szkoly.2015, by=NULL) -> data.2015
save(data.2015, file="ZPD_iter3_5.dat")

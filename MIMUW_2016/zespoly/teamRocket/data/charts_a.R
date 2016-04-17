this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

load("ZPD_iter1.dat")
load("ZPD_szkoly.dat")
library(dplyr)
library(sqldf)
library(tidyr)
library(ggplot2)
library(rCharts)

# ~~~~~~~~~~~~~~~~~~~~~~UWAGA~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Zmiana przelicznika. Zamiast punktow trzeba liczyc procenty - na razie tylko dla mimuw
mimuw_classify<-filter(data.wawa, p_mat>15 & p_pl>15 & r_mat>0)
mimuw_classify <- mimuw_classify %>% rowwise() %>% mutate(mimuw_wynik = 
                                                            0.1 * max(0.6*p_pl*10/7, r_pl*10/4, na.rm = TRUE) + 
                                                            0.1 * max(0.6*p_mat*2, r_mat*2, na.rm = TRUE) +
                                                            0.1 * max(0.6*p_ang*2, r_ang*2, na.rm = TRUE) + 
                                                            0.5 * max(r_inf*2, r_mat*2, na.rm = TRUE) + 
                                                            0.2 * max(r_mat*2, r_pl*10/4, r_ang*2, r_fiz*10/6, r_bio*10/6, r_chem*10/6, r_inf*2, na.rm = TRUE)
)
mini_classify <- filter(data.wawa, p_mat>15 & p_pl>15 & r_mat>0 & (r_bio > 0 | r_fiz > 0 | r_chem>0 | r_inf > 0))
mini_classify <- mini_classify %>% rowwise() %>% mutate(mini_wynik = 
                                                          max(0.5*p_mat, r_mat, na.rm = TRUE) +
                                                          0.25 * max(0.5*p_ang,r_ang, na.rm = TRUE) + 
                                                          max(r_fiz, 0.5*r_bio, r_fiz, 0.75*r_chem, r_inf, na.rm = TRUE)
)
uj_classify <- filter(data.wawa, p_mat>15 & p_pl>15 & sum(c(r_inf>0,r_mat>0, r_bio>0,r_fiz>0,r_chem>0), na.rm = TRUE)>1)
uj_classify <- mini_classify %>% rowwise() %>% mutate(uj_wynik = 
                                                        max(max(0.5*p_mat, r_mat, na.rm = TRUE), 
                                                            max(0.5*p_inf, r_inf, na.rm = TRUE),
                                                            max(0.5*p_bio, r_bio, na.rm = TRUE),
                                                            max(0.5*p_fiz, r_fiz, na.rm = TRUE),
                                                            max(0.5*p_chem, r_chem, na.rm = TRUE),
                                                            na.rm = TRUE) + 
                                                        
                                                        sort(
                                                          c(
                                                            max(0.5*p_mat, r_mat, na.rm = TRUE), 
                                                            max(0.5*p_inf, r_inf, na.rm = TRUE),
                                                            max(0.5*p_bio, r_bio, na.rm = TRUE),
                                                            max(0.5*p_fiz, r_fiz, na.rm = TRUE),
                                                            max(0.5*p_chem, r_chem, na.rm = TRUE)
                                                          ),
                                                          ,partial=4)[4]
)

uksford_classify<-filter(data.wawa, p_mat>15 & p_pl>15 & r_mat>0)
uksford_classify <- uksford_classify %>% rowwise() %>% mutate(uksford_wynik = 
                                                                max(0.4*p_mat, 0.8*r_mat, na.rm = TRUE) +
                                                                max(0.1*p_ang, 0.2*r_ang, na.rm = TRUE)
)

# Lista 300 najlepszych studentow ogolnie + wynik 300 studenta
mimuw_best_300 <- mimuw_classify %>% arrange(desc(mimuw_wynik)) %>% head(300)
mimuw_wynik_300 <- mimuw_best_300[300,]$mimuw_wynik

# Wyliczanie top 5 szkol
mimuw_top5 <- mimuw_classify %>% group_by(id_szkoly) %>%
  summarise(all_count = n(), top_300_count = sum(mimuw_wynik >= mimuw_wynik_300), procent = 100*top_300_count/all_count) %>%
  arrange(desc(procent)) %>% filter(all_count > 10) %>% head(5) %>%
  merge(filter(szkoly, rok==2015), by = "id_szkoly") %>% merge(mimuw_classify, by = "id_szkoly")

# Zamiana id_szkoly z typu numeric na character
mimuw_top5$id_szkoly <- as.character(mimuw_top5$id_szkoly)

# Rozklad wynikow poszczegolnych matur dla 5 najlepszych szkol
ggplot(mimuw_top5, aes(x = id_szkoly, y = mimuw_wynik, group=id_szkoly, fill=nazwa_szkoly)) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  geom_violin() + geom_hline(aes(yintercept=85), size=0.8) +
  geom_hline(aes(yintercept=72), linetype=2, size=0.8) +
  expand_limits(y=c(15, 100))

# Zestawienie podstawowej matury z polskiego do ogolnego wyniku na mimuw dla uczniow w 5 najlepszych szkolach
# + statystyka
#ggplot(mimuw_best_avg, aes(x = mimuw_wynik, y = p_pl, color = id_szkoly)) +
#  stat_density2d(h=c(10,10), color="grey") + geom_point(size = 1.3)

# Zestawienie podstawowej matury z polskiego do ogolnego wyniku na mimuw dla 300 najlepszych uczniow
# ggplot(mimuw_best_300, aes(x=mimuw_wynik, y=p_pl)) + geom_point() + stat_smooth()

# Zestawienie rozszerzonej matury z angielskiego do ogolnego wyniku na mimuw dla 300 najlepszyh uczniow
# ggplot(mimuw_best_300, aes(x=mimuw_wynik, y=r_ang)) + geom_point()

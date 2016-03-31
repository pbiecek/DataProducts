this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

load("ZPD_iter1.dat")
library(dplyr)
library(sqldf)
library(tidyr)
library(ggplot2)
library(rCharts)

mimuw_classify<-filter(data.wawa, p_mat>15 & p_pl>15 & r_mat>0)
mimuw_classify <- mimuw_classify %>% rowwise() %>% mutate(mimuw_wynik = 
                                                            0.1 * max(0.6*p_pl, r_pl, na.rm = TRUE) + 
                                                            0.1 * max(0.6*p_mat, r_mat, na.rm = TRUE) +
                                                            0.1 * max(0.6*p_ang,r_ang, na.rm = TRUE) + 
                                                            0.5 * max(r_inf,r_mat, na.rm = TRUE) + 
                                                            0.2 * max(r_mat,r_pl, r_ang, r_fiz, r_bio, r_fiz, r_chem, r_inf, na.rm = TRUE)
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


mimuw_best_150 <- mimuw_classify %>% arrange(desc(mimuw_wynik)) %>% head(150)

# Zestawienie podstawowej matury z polskiego do ogolnego wyniku na mimuw dla 150 najlepszych uczniow
ggplot(mimuw_best_150, aes(x=mimuw_wynik, y=p_pl)) + geom_point() + stat_smooth()

# Zestawienie rozszerzonej matury z angielskiego do ogolnego wyniku na mimuw dla 150 najlepszyh uczniow
# ggplot(mimuw_best_150, aes(x=mimuw_wynik, y=r_ang)) + geom_point()

mimuw_best_avg <- mimuw_classify %>% group_by(id_szkoly) %>%
                  summarise(median_score = median(mimuw_wynik)) %>%
                  arrange(desc(median_score)) %>%
                  head(5) %>% merge(mimuw_classify, by = "id_szkoly")

# Zamiana id_szkoly z typu numeric na character
mimuw_best_avg$id_szkoly <- as.character(mimuw_best_avg$id_szkoly)

# Rozklad wynikow poszczegolnych matur dla 5 najlepszych szkol
# Problem, jak dla jednej szkoly mamy tylko jednego ucznia - wtedy wykres violin sie nie pokazuje
# ggplot(mimuw_best_avg, aes(x = id_szkoly, y = mimuw_wynik, group=id_szkoly, color=id_szkoly)) +
#  geom_violin() + geom_jitter(height = 0)

# Zestawienie podstawowej matury z polskiego do ogolnego wyniku na mimuw dla uczniow w 5 najlepszych szkolach
# + statystyka
#ggplot(mimuw_best_avg, aes(x = mimuw_wynik, y = p_pl, color = id_szkoly)) +
#  stat_density2d(h=c(10,10), color="grey") + geom_point(size = 1.3)



# Zestawienie rozszerzonej matury z ang do rozszerzonej matury z mat dla uczniow w 5 najlepszych szkolach
# TODO na jednym wykresie powinny byc czerwone kropki=uczniowie danej szkoly, szare=uczniowie innych szkol 
#ggplot(na.omit(mimuw_best_avg), aes(x = r_ang, y = r_mat)) +
#  stat_ellipse(color="red4")+
#  geom_point(data = mimuw_best_avg[,-5],size = 1,color = "grey") +
#  geom_point(size = 2, color = "red") + 
#  facet_wrap(~id_szkoly)

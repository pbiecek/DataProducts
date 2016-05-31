#' String zawierający wszystkie matury podstawowe dostępne w serwisie
all_rozsz <- "m_bio_r, m_che_r, m_fiz_r, m_geo_r, m_his_r, m_inf_r, m_ang_r, m_pol_r, m_mat_r, m_wos_r"

#' String zawierający wszystkie matury podstawowe dostępne w serwisie
all_podst <- "m_bio_p, m_che_p, m_fiz_p, m_geo_p, m_his_p, m_inf_p, m_ang_p, m_pol_p, m_mat_p, m_wos_p"

#' Krotka zawierająca opis funkcji wyliczjącej punkty przy rekrutacji na matematyke UW
uw.matematyka <- list(skrot = "UW_mat", formula = 
                paste("pmax(m_pol_p*0.6, m_pol_r) * 0.1 + pmax(m_mat_p * 0.6 * 0.1, m_mat_r * 0.6)",
                " pmax(m_ang_p * 0.6, m_ang_r) * 0.1 + pmax(", all_rozsz, ") * 0.2"),
                opis = paste("Wynik jaki uczniowie szkoły otrzymaliby podczas rekrutacji na matematykę na MIMUW.",
                "Nie uwzględnia wyników z innych języków obcych niz angielski")
                )

#' Krotka zawierająca opis funkcji wyliczjącej punkty przy rekrutacji na informatykę UW
uw.informatyka <- list(skrot = "UW_inf", formula = 
                    paste("pmax(m_pol_p*0.6, m_pol_r) * 0.1 + pmax(m_mat_p * 0.6, m_mat_r) * 0.1 + 0.5 * m_mat_r",
                          " pmax(m_ang_p * 0.6, m_ang_r) * 0.1 + pmax(", all_rozsz, ") * 0.2"),
                  opis = paste("Wynik jaki uczniowie szkoły otrzymaliby podczas rekrutacji na informatykę na MIMUW.",
                               "Nie uwzględnia wyników z innych języków obcych niz angielski")
)

#' Krotka zawierająca opis funkcji wyliczjącej czy osoba zdała maturę
zdawalnosc <- list(skrot = "Zdawalność", formula = "ifelse(m_mat_p => 30 &  m_pol_p => 30 m_ang_p >= 30, 1, 0)",
                   opis = paste("Uczniowie którzy zdali maturę (oznaczeni jako 1) i ci którzy nie zdali (oznaczeni jako 0)",
                                "Nie uwzględnia wyników z innych języków obcych niz angielski"))

#' Krotka zawierająca opis funkcji wyliczjącej punkty przy rekrutacji na dziennikarstwo UW
uw.dziennikarstwo <- list(skrot = "UW_dzien", formula = paste("pmax(m_pol_p * 0.6, m_pol_r) * 0.3 + ",
                  "pmax(m_mat_p * 0.6, m_mat_r) * 0.05 + pmax(m_ang_p * 0.6, m_ang_r) * 0.2 + ",
                  "pmax(0.6 * pmax(m_hist_p, m_wos_p, m_geo_p, m_inf_p), pmax(m_hist_r, m_wos_r, m_geo_r, m_inf_r)) * 0.45"),
                  opis = paste("Wynik jaki uczniowie szkoły otrzymaliby podczas rekrutacji na Dziennikarstwo UW.",
                  "Nie uwzględnia wyników z innych języków obcych niz angielski"))

#' Krotka zawierająca opis funkcji wyliczjącej punkty przy rekrutacji na PRAWO UW
uw.prawo <- list(skrot = "UW_prawo", formula = paste("pmax(m_pol_p * 0.6, m_pol_r) * 0.15 +",
                  "pmax(m_mat_p * 0.6, m_mat_r) * 0.05 + pmax(m_ang_p * 0.6, m_ang_r) * 0.1",
                  " + (m_hist_r + m_wos_r + m_geo_r - pmin(m_hist_r, m_wos_r, m_geo_r)) * 0.35"),
              opis = "Wynik jaki uczniowie szkoły otrzymaliby podczas rekrutacji na Dziennikarstwo UW.",
              "Nie uwzględnia wyników z innych języków obcych niz angielski ani wyników z języka łacińskiego i filozofii")


#' String zawierający wszystkie matury podstawowe dostępne w serwisie
all_rozsz <- "m_bio_r, m_che_r, m_fiz_r, m_geo_r, m_his_r, m_inf_r, m_ang_r, m_pol_r, m_mat_r, m_wos_r"

#' String zawierający wszystkie matury podstawowe dostępne w serwisie
all_podst <- "m_bio_p, m_che_p, m_fiz_p, m_geo_p, m_his_p, m_inf_p, m_ang_p, m_pol_p, m_mat_p, m_wos_p"

#' Krotka zawierająca opis funkcji wyliczającej punkty przy rekrutacji na matematykę na UW
#' @export
uw.matematyka <- list(skrot = "uw_mat", formula = 
                paste("pmax(m_pol_p*0.6, m_pol_r, na.rm = TRUE) * 0.1 + pmax(m_mat_p * 0.6 * 0.1, m_mat_r * 0.6, na.rm = TRUE) +",
                " pmax(m_ang_p * 0.6, m_ang_r, na.rm = TRUE) * 0.1 + pmax(", all_rozsz, ", na.rm = TRUE) * 0.2"),
                opis = paste("Wynik jaki uczniowie otrzymaliby podczas rekrutacji na matematykę na MIMUW."))

#' Krotka zawierająca opis funkcji wyliczającej punkty przy rekrutacji na informatykę na UW
#' @export
uw.informatyka <- list(skrot = "uw_inf", formula = 
                    paste("pmax(m_pol_p*0.6, m_pol_r, na.rm = TRUE) * 0.1 + pmax(m_mat_p * 0.6, m_mat_r, na.rm = TRUE) * 0.1 + 0.5 * pmax(m_mat_r,m_inf_r, na.rm = TRUE) +",
                          " pmax(m_ang_p * 0.6, m_ang_r, na.rm = TRUE) * 0.1 + pmax(", all_rozsz, ", na.rm = TRUE) * 0.2"),
                  opis = paste("Wynik jaki uczniowie otrzymaliby podczas rekrutacji na informatykę na MIMUW."))

#' Krotka zawierająca opis funkcji wyliczającej czy osoba zdała maturę
#' @export
zdawalnosc <- list(skrot = "zdawalnosc", formula = "ifelse(m_mat_p >= 30 &  m_pol_p >= 30 & m_ang_p >= 30, 1, 0)",
                   opis = paste("Zdawalność matury [0 = nie zdał, 1 = zdał]"))

#' Krotka zawierająca opis funkcji wyliczającej punkty przy rekrutacji na dziennikarstwo na UW
#' @export
uw.dziennikarstwo <- list(skrot = "uw_dzien", formula = paste("pmax(m_pol_p * 0.6, m_pol_r, na.rm = TRUE) * 0.3 + ",
                  "pmax(m_mat_p * 0.6, m_mat_r, na.rm = TRUE) * 0.05 + pmax(m_ang_p * 0.6, m_ang_r, na.rm = TRUE) * 0.2 + ",
                  "pmax(0.6 * pmax(m_his_p, m_wos_p, m_geo_p, m_inf_p, na.rm = TRUE), pmax(m_his_r, m_wos_r, m_geo_r, m_inf_r, na.rm = TRUE), na.rm = TRUE) * 0.45"),
                  opis = paste("Wynik jaki uczniowie otrzymaliby podczas rekrutacji na dziennikarstwo UW."))

#' Krotka zawierająca opis funkcji wyliczającej punkty przy rekrutacji na prawo na UW
#' @export
uw.prawo <- list(skrot = "uw_prawo", formula = paste("pmax(m_pol_p * 0.6, m_pol_r, na.rm = TRUE) * 0.15 +",
                  "pmax(m_mat_p * 0.6, m_mat_r, na.rm = TRUE) * 0.05 + pmax(m_ang_p * 0.6, m_ang_r, na.rm = TRUE) * 0.1",
                  " + (pmax(0, m_his_r, na.rm = TRUE) + pmax(0, m_wos_r, na.rm = TRUE) + pmax(0, m_geo_r, na.rm = TRUE) -",
                  "ifelse(!anyNA(c(m_his_r, m_wos_r, m_geo_r)), pmin(m_his_r, m_wos_r, m_geo_r), 0)) * 0.35"),
              opis = paste("Wynik jaki uczniowie otrzymaliby podczas rekrutacji na prawo UW."))

# from USOSweb, https://usosweb.mimuw.edu.pl/kontroler.php?_action=home/grupy&rok=2014
# var courselist = $("td.strong span.note").map(function(n, x) {return x.innerHTML});
# console.log(courselist.map(function(x, n) {return '"' + n + '"'}).get().join(',\n\t'))

courses_vector <- c(
  "1000-214bWWW",
  "1000-214bIOP",
  "1000-224bJNP2",
  "1000-214bJAO",
  "1000-214bPSZ",
  "1000-214bSIK",
  "1000-213bASD",
  "1000-213bBAD",
  "1000-00HM1-OG",
  "1000-223bJNP1",
  "1000-213bRPS",
  "1000-213bSOP",
  "1000-212bAM2",
  "1000-212bAKS",
  "1000-00HM2-OG",
  "1000-222bIPP",
  "1000-212bMD",
  "1000-212bPO",
  "1000-211bAM1",
  "1000-211bGAL",
  "1000-211bPM",
  "1000-211bWPF",
  "1000-215bBSK",
  "1000-225bJNP3",
  "1000-216bJPP",
  "1000-215bMNU",
  "1000-2PRAKZ",
  "1000-215bSWP",
  "1000-2L5ZPP",
  "1000-2L5ZP1",
  "1000-2L5ZP2"
)

# https://usosweb.mimuw.edu.pl/kontroler.php?_action=actionx%3Akatalog2%2Fprzedmioty%2FszukajPrzedmiotu%28method%3Arej%3Brej_kod%3A1000-2016%3Bcallback%3Ag_5ffc7cdf%3Bcp_showDescriptions%3A0%3Bcp_showGroupsColumn%3A0%3Bcp_cdydsDisplayLevel%3A2%3Bf_tylkoWRejestracji%3A%3BkierujNaPlanyGrupy%3A%3Bf_grupa%3A1000-OBIER-STALE%29&f_modified=1&f_grupa=1000-OBIER
# https://usosweb.mimuw.edu.pl/kontroler.php?_action=actionx%3Akatalog2%2Fprzedmioty%2FszukajPrzedmiotu%28method%3Arej%3Brej_kod%3A1000-2016l%3Bcallback%3Ag_5ffc7cdf%3Bcp_showDescriptions%3A0%3Bcp_showGroupsColumn%3A0%3Bcp_cdydsDisplayLevel%3A2%3Bf_tylkoWRejestracji%3A%3BkierujNaPlanyGrupy%3A%3Bf_grupa%3A1000-OBIER-STALE%29&f_modified=1&f_grupa=1000-OBIER
# $("#layout-c22a > span > table.wrnav > tbody > tr > td:nth-child(1)").text()

courses_vector <- append(courses_vector, strsplit("1000-2M16AN
1000-2M00TL
1000-2M12DNA
1000-2N09ALT
1000-2M12AGO
1000-2M13AR
1000-2M00GO
1000-2M15ZTA
1000-2N09KDW
1000-2M12KI1
1000-2M16NMW
1000-2M09OTW
1000-2M05ZP
1000-2M08PMK
1000-2M11PWA
1000-2N00PLO
1000-2M13PDD
1000-2M11RW
1000-2M01SP
1000-2M10SR
1000-2M00SW
1000-2N03TI
1000-2M10TKI
1000-2M11WK
1000-2M15WAS
1000-2M07MD
1000-2N09ZBD
1000-2M08ZPI
1000-2M12TGK
1000-2N00ALG
1000-2M11AGP
1000-2M03DM
1000-2M13DZD
1000-2M09GOB
1000-2M16GSN
1000-2M09OTA
1000-2M09ICK
1000-2M12KOK
1000-2M12KI2
1000-2M10PLO
1000-2M15PJN
1000-2M09SI
1000-2M13SAD1
1000-2M13SAD2
1000-2N09SUS
1000-2N00SID
1000-2M13TAU
1000-2M14TKP
1000-2M00TO
1000-2M14TGS
1000-2M16TIM
1000-2N09WWK
1000-2N09WSS
1000-2N03BO
1000-2M11WWI
1000-2M01XM
1000-2M12ZMI", "\\s+")[[1]])

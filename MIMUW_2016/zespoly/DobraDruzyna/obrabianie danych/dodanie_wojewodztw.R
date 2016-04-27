# $biblioteki to tabela z GUSu z kodem teryt (z już obciętą ostatnią cyfrą).
# Skrypt dodaje do tabeli kolumnę z kodem województw.

woj <- trunc(biblioteki["Kod"]/10000)
biblioteki_woj <- cbind(biblioteki, Woj=woj)
colnames(biblioteki_woj)[19] <- "Województwo"

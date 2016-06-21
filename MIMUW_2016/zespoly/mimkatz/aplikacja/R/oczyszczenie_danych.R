library(plyr)

#sciezki
path_read <- "/home/marcin/jnp2/proj/data/oceny_clean.txt"
path_write <- "/home/marcin/jnp2/proj/data/oceny.csv"

#wczytaj
raw_data <- read.csv(path_read, header=TRUE, sep=";")
colnames(raw_data) <- c("PRZ_KOD", "PRZ_NAZWA", "CYKL_DYD", "TERMIN", "OCENA", "OSOBA")

#przedmioty z MIM
raw_data <- raw_data[grepl("^1000-*", raw_data$PRZ_KOD), ]

#podzial na terminy zaliczenia
raw_data_1 <- raw_data[grepl("1", raw_data$TERMIN), ]
raw_data_2 <- raw_data[grepl("2", raw_data$TERMIN), ]
raw_data_3 <- raw_data[grepl("3", raw_data$TERMIN), ]

#polaczenie osob majacych ocene z drugiego terminu
tmp <- join(raw_data_1, raw_data_2, type = "left", by = c("OSOBA", "CYKL_DYD", "PRZ_KOD", "PRZ_NAZWA"))

#zastapienie ostatecznej oceny, ocena z drugiego terminu
data <- tmp[,c(1, 2, 3, 5, 6)]
colnames(data) <- c("PRZ_KOD", "PRZ_NAZWA", "CYKL_DYD", "OCENA", "OSOBA")
whichRows <- which(!is.na(tmp[, 8]))
data[whichRows, 4] <- tmp[whichRows, 8]

#polaczenie osob majacych ocene z trzeciego terminu
tmp <- join(data, raw_data_3, type = "left", by = c("OSOBA", "CYKL_DYD", "PRZ_KOD", "PRZ_NAZWA"))

#zastapienie ostatecznej oceny, ocena z trzeciego terminu
data <- tmp[,c(1, 2, 3, 4, 5)]
colnames(data) <- c("PRZ_KOD", "PRZ_NAZWA", "CYKL_DYD", "OCENA", "OSOBA")
whichRows <- which(!is.na(tmp[, 7]))
data[whichRows, 4] <- tmp[whichRows, 7]

#usuniecie smieci
remove(whichRows)
remove(tmp)
remove(raw_data)
remove(raw_data_1)
remove(raw_data_2)
remove(raw_data_3)

#data zawiera 5 kolumn, w OCENA jest ostateczna ocena jaka student otrzymal z danego przedmiotu

#wybierz istotne roczniki
data <- data[grepl("2008L|2008Z|2009Z|2009L|2010Z|2010L|2011Z|2011L|2012Z|2012L|2013Z|2013L|2014Z|2014L|2015Z", data$CYKL_DYD), ]

#usun biale znaki
data[,1] <- trimws(data[,1])
data[,2] <- trimws(data[,2])
data[,3] <- trimws(data[,3])
data[,4] <- trimws(data[,4])
data[,5] <- trimws(data[,5])

#ta sama nazwa, inny przedmiot
data[grepl("1000-212bMD", data$PRZ_KOD), ]$PRZ_NAZWA <- "Matematyka dyskretna - inf"
data[grepl("1000-134MAD", data$PRZ_KOD), ]$PRZ_NAZWA <- "Matematyka dyskretna - mat"

data[grepl("1000-213bBAD", data$PRZ_KOD), ]$PRZ_NAZWA <- "Bazy danych - inf"
data[grepl("1000-134BAD", data$PRZ_KOD), ]$PRZ_NAZWA <- "Bazy danych - mat"
data[grepl("1000-715BDU", data$PRZ_KOD), ]$PRZ_NAZWA <- "Bazy danych - bio"

#jakies stare pojedyncze kody
data <- data[data$PRZ_KOD != "1000-2D97BD1S",]
data <- data[data$PRZ_KOD != "1000-2D97DB1S",]
data <- data[data$PRZ_KOD != "1000-4b13BAD",]
data <- data[data$PRZ_KOD != "1000-4b13ASD",]
data <- data[data$PRZ_KOD != "1000-4b13SOP",]
data <- data[data$PRZ_KOD != "1000-4b12MD",]
data <- data[data$PRZ_KOD != "1000-711MAD",]
data <- data[data$PRZ_KOD != "1000-713MDA",]

#zapisz
write.csv(data, path_write, row.names=FALSE)

#wyczysc workspace
remove(data)

library(dplyr)

biblioteki <- biblioteki_dane
biblioteki[1] <- trunc(biblioteki[1]/10)

biblioteki_clean <- filter(biblioteki, !grepl("miasto|obszar wiejski", Nazwa))
saveRDS(biblioteki_clean, file = "biblioteki_wyczyszczone")

generuj_url_pytania <- function(id_pytania) {
  return(paste(c("http://zpd.ibe.edu.pl/pobierzTresc.php?id_pytania=", id_pytania, "&typ=html&klucz=0")
                    , collapse = ''))
}


generuj_url_wiazki <- function(id_pytania) {
  return(paste(c("http://zpd.ibe.edu.pl/pobierzTresc.php?id_wiazki=", id_pytania, "&typ=html&klucz=0")
               , collapse = ''))
}


generuj_url_arkusza <- function(nazwa_arkusza) {
  return(paste(c("http://zpd.ibe.edu.pl/pobierzTresc.php?arkusz="
                 ,nazwa_arkusza, "&typ=pdf&klucz=0"), collapse = ''))
}


pobierz_pytanie <- function(id_pytania) {
  library(curl)
  h <- new_handle()
  
  con <- curl(generuj_url_pytania(id_pytania), handle = h)
  open(con)
  
  cache <- HTML(paste(readLines(con), separator='\n', collapse=''))
  close(con)
  return(cache)
}


pobierz_wiazke <- function(id_wiazki) {
  library(curl)
  h <- new_handle()
  
  con <- curl(generuj_url_wiazki(id_wiazki), handle = h)
  open(con)
  
  cache <- HTML(paste(readLines(con), separator='\n', collapse=''))
  close(con)
  return(cache)
}
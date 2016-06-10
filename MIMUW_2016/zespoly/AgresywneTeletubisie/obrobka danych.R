library(ZPD)

## Sciaganie danych z bazy
baza_danych <- './Dane_z_bazy.RData'
load(baza_danych)
conn <- polacz()

if (!exists('wartosci_wskaznikow')) {
  wartosci_wskaznikow <- pobierz_wartosci_wskaznikow(conn) %>% collect
}
if (!exists('wskazniki')) {
  wskazniki <- pobierz_wskazniki(conn) %>% collect
}
if (!exists('szkoly')) {
  szkoly <- pobierz_szkoly(conn) %>% collect
}

rozlacz()

load('id_szkoly_geo.RData')

szkoly <- szkoly %>%
  filter(id_szkoly > 0) %>%
  group_by(id_szkoly) %>%
  arrange(id_szkoly, desc(adres)) %>%
  mutate(adres=adres[1]) %>%
  arrange(desc(miejscowosc)) %>%
  mutate(miejscowosc=miejscowosc[1]) %>%
  arrange(desc(gmina_szkoly)) %>%
  mutate(gmina_szkoly=gmina_szkoly[1]) %>%
  arrange(desc(wojewodztwo_szkoly)) %>%
  mutate(wojewodztwo_szkoly=wojewodztwo_szkoly[1]) %>%
  arrange(desc(nazwa_szkoly)) %>%
  mutate(nazwa_szkoly = nazwa_szkoly[1])

all_data <- wartosci_wskaznikow %>% inner_join(wskazniki) %>% distinct(id_szkoly,skrot,rok) %>% left_join(szkoly)
all_data_geo <- left_join(all_data, id_szkoly_geo)

all_data_geo <- all_data_geo %>%
  mutate(skrot=gsub('gimnazjum ', '', .$skrot)) %>%
  arrange(id_szkoly,rok,skrot,desc(rodzaj_wsk)) %>%
  distinct(id_szkoly, rok, skrot) %>%
  select(id_szkoly,rok,srednia,lu,skrot,rodzaj_wsk,typ_szkoly:czesc_egzaminu,nazwa_szkoly:wielkosc_miejscowosci,teryt_szkoly:lat)

all_data_geo$skrot <- all_data_geo$skrot %>%
  gsub('.*polski','jezyk polski', .) %>%
  gsub('matura ','', .) %>% gsub('mat.-przyr.', 'matematyczno-przyrodniczy', .) %>%
  gsub('hist. i WOS', 'historia i WOS', .) %>%
  gsub('przedm. przyr.', 'przedmioty przyrodnicze', .) %>%
  gsub('j. ang.', 'jezyk angielski', .)

lista_do_geo <- szkoly %>%
  mutate(adres_do_geo=paste(ifelse(nchar(adres) < 5, pna, adres), miejscowosc, 'PL', sep=', ')) %>%
  mutate(adres_do_geo2=paste(gmina_szkoly, wojewodztwo_szkoly, 'PL', sep=', ')) %>%
  mutate(adres_do_geo=ifelse(adres_do_geo!=', , PL', adres_do_geo, adres_do_geo2)) %>%
  distinct(id_szkoly) %>%
  filter(!is.na(id_szkoly)) %>%
  select(id_szkoly,adres_do_geo)

## Geocoding

geoCode <- function(address,verbose=FALSE) {
  
  url <- function(address, return.call = "json", sensor = "false") {
    root <- "http://maps.google.com/maps/api/geocode/"
    u <- paste(root, return.call, "?address=", address, "&sensor=", sensor,"&region=pl" , sep = "")
    return(URLencode(u))
  }
  
  if(verbose) cat(address,"\n")
  u <- url(address)
  doc <- getURL(u)
  x <- fromJSON(doc,simplify = FALSE)
  if(x$status=="OK") {
    lat <- x$results[[1]]$geometry$location$lat
    lng <- x$results[[1]]$geometry$location$lng
    return(data.frame(lon=lng, lat=lat))
    Sys.sleep(2)
  } else {
    return(data.frame(lon=NA,lat=NA))
  }
}

geocoded_data <- lista_do_geocode %>% ungroup %>%
    select(adres_do_geo) %>%
    apply(1, geoCode) %>% do.call(rbind, .) 


save(all_data_geo,file='all_data_geo.RData')


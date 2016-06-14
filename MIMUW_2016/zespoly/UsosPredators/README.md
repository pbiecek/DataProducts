## Nazwa zespołu

**UsosPredators**

## Skład

Tomasz Knopik: <tk359778@students.mimuw.edu.pl>

Michał Graczykowski: <mg359194@students.mimuw.edu.pl>

Janusz Marcinkiewicz: <jm360338@students.mimuw.edu.pl>

## Role

* Kontakt na zewnątrz: Tomasz
* Specyfikacja wymagań: Tomasz
* Opieka nad kodem: Michał
* Opieka nad częścią analityczną: Michał
* Opieka nad częścią wizualną: Janusz
* Opieka nad dokumentacją końcową: Janusz

## Temat

USOS: Określenie wyników wykładowcy lub ćwiczeniowca poprzez przedstawienie statystyk na temat: zdawalności, ilości warunków, ilości odpadnieć, rozkładu ocen i tendencji w poszczególnych latach.

## Dokumentacja

### Instrukcja instalacji

Instalujemy bibliotekę devtools:
```
install.packages("devtools")
```

Instalujemy:
```
devtools::install_github("pbiecek/DataProducts/MIMUW_2016/zespoly/UsosPredators/usosstats")
```

### Instrukcja uruchomienia

Aplikację uruchamiamy poleceniem:
```
usosstats::run()
```

### Aktualizacja danych

Jeśli chcemy zaktualizować dane aktualnie już wykorzystywane w programie, należy podmienić plik odpowiedniego wskaźnika w katalogu data, dbając o to, aby kolumny w nowej wersji danych miały dokładnie te same nazwy oraz reprezentowały te same wartości.

### Obsługa innych przedmiotów

Jeśli chcemy obsługiwać inne przedmioty niż zostały wybrane w aplikacji należy zmodyfikować plik server.R uaktualniając zmienną subjectNames poprzez dopisanie pary wartości (nazwa przedmiotu, kod przedmiotu).

### Aktualizacja pakietu

Instrukcja jak tworzyć oraz modyfikować istniejące pakiety znajduje się tutaj:

https://pbiecek.gitbooks.io/przewodnik/content/Programowanie/pakiety/po_co.html

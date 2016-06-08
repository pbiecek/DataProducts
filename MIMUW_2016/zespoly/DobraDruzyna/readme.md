## Nazwa zespołu

**Dobra Drużyna**

## Skład

Marta Rożek

Paweł Poskrobko

Michał Banaszkiewicz

## Role

* Kontakt na zewnątrz: Michał
* Specyfikacja wymagań: Marta
* Opieka nad kodem: Michał
* Opieka nad częścią analityczną: Paweł
* Opieka nad częścią wizualną: Marta
* Opieka nad dokumentacją końcową: Paweł

## Temat

Porównanie wyników poszczególnych przedmiotów maturalnych w różnych gminach z uwzględnieniem czynników społecznych i ekonomicznych.

## Dokumentacja

### Instrukcja instalacji

Aplikacja dostarczona jest w formie pakietu.

#### Instalacja "po kolei"

Klonujemy repo:
```
git clone https://github.com/mihal277/DataProducts/
```

W R Studio zmieniamy folder roboczy na odpowiedni:
```
setwd("/home/nazwa_uzytkownika/DataProducts/MIMUW_2016/zespoly/DobraDruzyna/pakiet")
```

Instalujemy bibliotekę devtools:
```
install.packages("devtools")
```

Budujemy pakiet:
```
devtools::build("JakPoszlaMatura")
```

Instalujemy pakiet:
```
devtools::install("JakPoszlaMatura")
```

#### Instalacja "na skróty"

Instalujemy bibliotekę devtools:
```
install.packages("devtools")
```

Instalujemy:
```
devtools::install_github(repo = "mihal277/DataProducts", subdir="/MIMUW_2016/zespoly/DobraDruzyna/pakiet/JakPoszlaMatura")
```

### Instrukcja uruchomienia

Aplikację uruchamiamy poleceniem

```
JakPoszlaMatura::uruchom()
```

### Instrukcja korzystania

Aplikacja przeznaczona jest dla wszystkich zainteresowanych możliwością analizy wyników edukacyjnych na poziomie poszczególnych gmin oraz porównania tych wyników ze wskaźnikami społeczno-ekenomicznymi (np. poziom bezrobocia, liczba bibliotek itp.).

Aplikacja jest niezwykle prosta w użyciu.
Po uruchomieniu naszym oczom ukazuje się główny pasek nawigacyjny u góry, pod którym znajduje się pojedyncza strona z serią wykresów. Każdy wykres jest opisany.

Na pasku nawigacyjnym mamy możliwość wybrać interesujący nas rok oraz przedmiot (w tym momencie do wyboru są lata 2012-2014 oraz matura podstawowa z języka polskiego).
Tuż poniżej możemy wybrać interesującą nas gminę.

### Aktualizacja danych

Jeśli chcemy zaktualizować dane aktualnie już wykorzystywane w programie, należy podmienić plik odpowiedniego wskaźnika w katalogu data, dbając o to, aby kolumny w nowej wersji danych miały dokładnie te same nazwy oraz reprezentowały te same wartości.

### Dodanie nowych danych

Aby dodać nowy wskaźnik porównawczy do aplikacji należy:
1. Dodać plik .RData do katalogu data.
2. Zdefiniować nowe wykresy w pliku server.r jako zmienne output$nazwa_wykresu.
3. W pliku ui.r dodać w contencie strony opis nowych wykresów oraz wywołać je przy użyciu plotlyOutput, bądź innym odpowiednim do stworzonego wykresu.




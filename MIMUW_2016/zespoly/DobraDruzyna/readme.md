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
setwd('/home/nazwa_uzytkownika/DataProducts/MIMUW_2016/zespoly/DobraDruzyna/shiny2')
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
devtools::install_github(repo = "mihal277/DataProducts", subdir="/MIMUW_2016/zespoly/DobraDruzyna/shiny2/JakPoszlaMatura")
```

### Instrukcja uruchomienia

Aplikację uruchamiamy poleceniem

```
JakPoszlaMatura::uruchom()
```




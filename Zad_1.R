#1. Napisz funkcję sprawdzająca czy 1 liczba jest podzielna przez druga użyj - %% 


zad1 <-function(x,y){
  ifelse(x%%y==0, TRUE, FALSE)
}

zad1(4,7)


#2. Pociąg z Lublina do Warszawy przejechał połowę drogi ze średnią prędkością 120 km/h. 
#Drugą połowę przejechał ze średnią prędkością 90 km/h. 
#Jaka była średnia prędkość pociągu. 


zad2=2/((1/120)+(1/90))

zad2


#3. Utwórz funkcję obliczającą współczynnik korelacji r Pearsona dla 2 wektorów o tej samej długości. 
#Wczytaj dane plik dane.csv i oblicz współczynnik dla wagi i wzrostu. W komentarzu napisz co oznacza wynik.

library(readr)
dane <- read_delim("dane.csv", ";", escape_double = FALSE, 
                   trim_ws = TRUE)

zad3 <-function(x,y){
  cor(x, y,  method = "pearson")
}

zad3(dane$wzrost,dane$waga)


#4. Napisz funkcję zwracającą ramke danych z danych podanych przez użytkownika  
#stworzDataFrame <- function(ile=1) 
#W pierwszym wierszu użytkownik podaje nazwy kolumn. w kolejnych wierszach zawartość wierszy ramki danych 
#( tyle wierszy ile podaliśmy w argumencie ile. ile=1 oznacza, że gdy użytkownik nie poda żadnej wartości jako parametr, domyślna wartością będzie 1)

stworzDataFrame <- function(nazwa,wartosc,ile=1){
wartosci=c()
  for (i in 1:ile){
    wartosci <- c(wartosci, wartosc)
  }
df=data.frame(column1 = wartosci)

names(df)[names(df) == "column1"] <- nazwa

df
}
  
test=stworzDataFrame(nazwa='testowa_kolumna',wartosc='test12334',33)


#5 Napisz funkcję , która pobiera sciezkeKatalogu, nazweKolumny, jakaFunkcje, DlaIluPlikow i liczy:  
#mean, median,min,max w zależności od podanej nazwy funkcji w argumencie, z katologu który podaliśmy i z tylu plików ilu podaliśmy dla wybranej nazwy kolumny.  
#UWAGA: w podanych plikach R pobierając komórki nazwane liczbami 

#R wstawi przed nazwy X. Funkcję przetestuj dla katalogu smogKrakow.zip.  
#Wykonując obliczenia pomiń brakujące wartości. 

liczZplikow <- function(sciezka,nazwaKolumny,jakaFunkcja="mean",DlaIluPlikow=1){  
  


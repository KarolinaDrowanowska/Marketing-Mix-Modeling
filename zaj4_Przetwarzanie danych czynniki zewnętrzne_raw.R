library(tidyverse)
library(readxl)
library(lubridate)

# 
# zad 1) Stwórz data frame ze zmienną Date, która przyjmuje wartości tygodniowe dat od 2021-01-04 do 2022-05-23. Możesz zastosować funkcję seq().

 ---------------------------------------------------------------
Date <- 

data.df <- 

# 2) Stwórz 3 zmienne:
# XMAS – która przyjmuje wartość 1, dla 25tego grudnia każdego roku
# MAY – która przyjmuje wartość 1 dla 1szego maja każdego roku
# Y2022 – która przyjmuje wartości 1 dla wszystkich dat w 2022 roku.


---------------------------------------------------------------
data.zad2.df <- data.df %>%
    mutate(XMAS = ,
           MAY = ,
           Y2022 = )


# Pogoda ------------------------------------------------------------------

# 3) Na podstawie dziennych wartości temperatury z pliku pogoda.xlsx stwórz dane o częstotliwości tygodniowej w podziale na miejscowości (station.name) dla okresu od 2018-12-31 do 2022-05-23.


weather.raw.df <- 

zad.3.df <- weather.raw.df %>%
    mutate(Date =as.Date(Date)) %>%
    filter() %>%
    mutate(Date.weekly = ) %>%
    group_by( , ) %>%
    summarize(TEMP_MEAN = mean()) %>%
    ungroup()

# Tip: do stworzenia daty tygodniowej z dziennej możesz zastosować funkcję floor_date() lub cut().
# 4) Połącz dane pogodowe ze słownikiem województw (zakładka słownik) i policz średnią temperaturę per tydzień i województwo

voi.df <- 

zad.4.df <- zad.3.df %>%
    left_join() %>%
    group_by(, ) %>%
    summarize(TEMP_MEAN_VOI = mean()) %>%
    ungroup()

# 5) Złącz dane z punktu 4 z wagami określającymi populację województw stosując wagi z zakładki wagi. Następnie policz średnią ważoną  temperaturę 


weights.df <- 

zad.5.df <- zad.4.df %>%
    left_join() %>%
    mutate(TEMP_X_WEIGHT =  * ) %>%
    group_by() %>%
    summarize(TEMP_WEIGHTED = sum()) %>%
    ungroup()


# 6) Przypisz numer tygodnia do daty tygodniowej – numer tygodnia musi być między 1 a 52 w zakładce nrtygodnia. Następnie połącz plik z dataframe z punktu 5.


week.number.df <- 

zad.6.df <- zad.5.df %>%
    left_join(  , by = c("Date.weekly" = "Date"))

# 7) Policz cykl temperatury poprzez policzenie średniej temperatury na podstawie numeru tygodnia.


zad.7.df <- zad.6.df %>%
    group_by() %>%
    summarise(TEMP_NORM = mean())



# 8) Stwórz finalny dataframe z wartościami cyklu temperatury dla okresu od 2018-12-31 do 2022-05-23, łącząc dataframy z punktów 6 i 7.


final.cykl.df <- week.number.df %>%
    left_join()





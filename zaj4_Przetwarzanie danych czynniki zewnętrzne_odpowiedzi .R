library(tidyverse)
library(readxl)
library(lubridate)

# 
# zad 1) Stwórz data frame ze zmienną Date, która przyjmuje wartości tygodniowe dat od 2021-01-04 do 2022-05-23. Możesz zastosować funkcję seq().

 ---------------------------------------------------------------

Date <- seq(from = as.Date("2021-01-04"), to = as.Date("2022-05-23"), by = 7)
# 

data.df <- as.data.frame(Date) 

# 2) Stwórz 3 zmienne:
# XMAS – która przyjmuje wartość 1, dla 25tego grudnia każdego roku
# MAY – która przyjmuje wartość 1 dla 1szego maja każdego roku
# Y2022 – która przyjmuje wartości 1 dla wszystkich dat w 2022 roku.


---------------------------------------------------------------
    
data.zad2.df <- data.df %>%
    mutate(XMAS = ifelse(Date == "2021-12-20", 1, 0),
           MAY = ifelse ( Date =="2021-04-26"| Date == "2022-04-25", 1,0 ),
           Y2022 = ifelse(Date >= "2022-01-03", 1,0))



    
# Pogoda ------------------------------------------------------------------

# 3) Na podstawie dziennych wartości temperatury z pliku pogoda.xlsx stwórz dane o częstotliwości tygodniowej w podziale na miejscowości (station.name) dla okresu od 2018-12-31 do 2022-05-23.


weather.raw.df <- read_excel("pogoda.xlsx", sheet = "pogoda")

weather.processed.df <- weather.raw.df %>%
    mutate(Date =as.Date(Date)) %>%
    filter(Date >= "2018-12-31", Date <= "2022-05-23") %>%
    mutate(Date.weekly = floor_date(Date, "weekly", week_start = 1 )) %>%
    group_by(Date.weekly, station.name) %>%
    summarize(TEMP_MEAN = mean(TEMP)) %>%
    ungroup()

# Tip: do stworzenia daty tygodniowej z dziennej możesz zastosować funkcję floor_date() lub cut().
# 4) Połącz dane pogodowe ze słownikiem województw (zakładka słownik) i policz średnią temperaturę per tydzień i województwo

voi.df <- read_excel("pogoda.xlsx", sheet = "słownik")

weather.voi.df <- weather.processed.df %>%
    left_join(voi.df) %>%
    group_by(Date.weekly, voi) %>%
    summarize(TEMP_MEAN_VOI = mean(TEMP_MEAN)) %>%
    ungroup()

# 5) Złącz dane z punktu 4 z wagami określającymi populację województw stosując wagi z zakładki wagi. Następnie policz średnią ważoną  temperaturę 


weights.df <- read_excel("pogoda.xlsx", sheet = "wagi")

weather.voi.weight.df <- weather.voi.df %>%
    left_join(weights.df) %>%
    mutate(TEMP_X_WEIGHT = TEMP_MEAN_VOI * weight) %>%
    group_by(Date.weekly) %>%
    summarize(TEMP_WEIGHTED = sum(TEMP_X_WEIGHT)) %>%
    ungroup()


# 6) Przypisz numer tygodnia do daty tygodniowej – numer tygodnia musi być między 1 a 52 w zakładce nrtygodnia. Następnie połącz plik z dataframe z punktu 5.


week.number.df <- read_excel("pogoda.xlsx", sheet = "nrtygodnia")

zad.6.df <- weather.voi.weight.df %>%
    left_join(week.number.df, by = c("Date.weekly" = "Date"))

# 7) Policz cykl temperatury poprzez policzenie średniej temperatury na podstawie numeru tygodnia.


zad.7.df <- zad.6.df %>%
    group_by(week.no) %>%
    summarise(TEMP_NORM = mean(TEMP_WEIGHTED))



# 8) Stwórz finalny dataframe z wartościami cyklu temperatury dla okresu od 2018-12-31 do 2022-05-23, łącząc dataframy z punktów 6 i 7.


final.cykl.df <- week.number.df %>%
    left_join(zad.7.df)





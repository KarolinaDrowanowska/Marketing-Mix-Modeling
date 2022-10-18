# install.packages("tidyverse")
# install.packages("readxl")
# install.packages("plotly")


library(tidyverse)
library(readxl)
library(plotly)



# Wczytanie danych do środowiska - zadania --------------------------------

# 1) Proszę wczytać plik: example_data.xlsx za pomocą odpowiedniej funkcji

zad.1.df <- 

# 2) Proszę wczytać zakładkę 2 („example_data”) z pliku example_data.xlsx 

zad.2.df <- 


# 3) Proszę wczytać zakładkę 2 („example_data”) z pliku example_data.xlsx  pomijając pierwsze 3 wiersze


zad.3.df <- 


# 4)Proszę wczytać zakładkę 2 („example_data”) z pliku example_data.xlsx pomijając pierwsze 3 wiersze i pozostawiając tylko 4 wiersze


zad.4.df <- 


# 5) Proszę sprawdzić strukturę pliku z pkt 3

head(zad.3.df)
str(zad.3.df)


# Przykład tibble vs dataframe --------------------------------------------


zad.3.df

zad.3.data.frame.df <- as.data.frame(zad.3.df)

zad.3.data.frame.df

# wczytanie danych read.csv2 vs read_csv2

check.dataframe.df <- read.csv2("example_dictionary.csv")
str(check.dataframe.df)
check.dataframe.df

check.tibble.df <- read_csv2("example_dictionary.csv")
str(check.tibble.df)
check.tibble.df



# Przykład tworzenie uporządkowanego zbioru danych ------------------------


iris

iris.long.df <- iris %>%
    mutate(row.number = row_number()) %>%
    pivot_longer(cols = Sepal.Length : Petal.Width,
                 names_to = "Description",
                 values_to = "value")

iris.wide.df <- pivot_wider(iris.long.df, 
                            names_from = "Description",
                            values_from = "value")



# 6) Proszę stworzyć uporządkowany plik (bazując na pliku z poprzedniego zadania nr 3) 

zad.6.df <-           

# 7) Proszę stworzyć dane w formacie long na podstawie stworzonego w zadaniu 6 df.

zad.7.df <-  


# Przykład wykorzystania wszystkich czasowników ---------------------------

# sekwencja i pipe

iris.new.df <- iris %>%
    select(Species , Sepal.Length, Sepal.Width ) %>%
    mutate(sum = Sepal.Length + Sepal.Width) %>%
    filter(Sepal.Length >= 4.5) %>%
    arrange(Species) %>%
    rename(Sepal.sum = sum)  %>%
    group_by(Species) %>%
    summarise(Sepal.Length.sum = sum(Sepal.Length),
              Sepal.Width.mean = mean(Sepal.Width),
              Sepal.sum.max = max(Sepal.sum))


# 8) Ze zbioru danych z zadania 6 i korzystając z pipe’ów stwórz data frame, na podstawie którego można 
# odpowiedzieć na poniższe pytanie:
# Dla Brand3, jaka była średnia koszt w PLN oraz maksymalny koszt w EUR pomiędzy datami: 2017-08-12 i 2019-08-31



zad.8.df <- zad.6.df %>%
    mutate(Date = as.Date(Date)) %>%
    select() %>%
    filter() %>%
    mutate() %>%
    group_by() %>%
    summarise () %>%
    ungroup()

# Przykład joinów w R -----------------------------------------------------


dictionary.df <- read_csv2("example_dictionary.csv")

data.tidy.dict.left.df <- zad.6.df %>%
    left_join(dictionary.df, by = c("Brand", "Film Code", "Ratecard Duration")) 




# 9) Ze zbioru danych z zadania 6 i korzystając z pipe’ów  stwórz data frame, na podstawie którego stworzysz liniowy wykres przedstawiający sumę dziennych wydatków mediowych dla wszystkich Brandów.


zad.6.df %>% 
    group_by( ) %>% 
    summarise() %>% 
    ungroup() %>% 
    ggplot(.) +           
    geom_line(aes(),
              color = 'blue') +
    theme_minimal() + # way nicer plot's theme
    xlab("") +
    ylab("") +
    ggtitle("")



# Jak stworzyć wykres z wykorzystaniem plotly? ----------------------------


# Ze zbioru danych z zadania 6 i korzystając z pipe’ów  stwórz data frame, na podstawie którego stworzysz liniowy wykres przedstawiający sumę dziennych wydatków mediowych dla wszystkich Brandów (na lewej osi) oraz sumę TRP dla wszystkich brandów (na prawej osi). Do zadania wykorzystaj pakiet plotly.


zad.6.df %>% 
    group_by() %>% 
    summarise() %>% 
    ungroup()  %>%
    plot_ly(., type = "", x = ~, y = ~, mode = 'lines', name = '') %>%
    add_trace(x = ~, y = ~, mode = 'lines',yaxis = "y2", name = '') %>%
    layout(title = "",
           yaxis2 = list(overlaying = "y", side = "right"))




# Przykład funkcji lm() w R -----------------------------------------------

# Data frame do użycia w modelu: suma kosztów, TRPsów, Stworzenie zmiennej trend przyjmujacej wartosci od 1 do ostatniego numeru wiersza 

data.to.model.df <- zad.6.df %>%
    group_by(Date) %>% 
    summarise(Cost_total =  sum(Cost, na.rm = T),
              TRP_total = sum(TRP, na.rm = T)) %>%
    mutate(Trend = row_number()) %>%
    ungroup()


linear_model <- lm( ~  + , data=)

# view diagnostic plot
plot(linear_model)

summary(linear_model)

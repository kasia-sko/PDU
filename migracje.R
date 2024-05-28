migracje <- read.csv("migracje.csv", fileEncoding = "Windows-1250")
migracje
nazwyKrajow <- read.csv("countries.csv")
head(nazwyKrajow,20)
nazwyKrajow$pl
srednie_migracje <- migracje %>%
  group_by(Nazwa) %>%
  summarise(srednia_emigracja = mean(c(EMI16, EMI17, EMI18, EMI19,EMI20, EMI21, EMI22), na.rm = TRUE), 
            srednie_saldo = mean(c(SALDO16,SALDO17, SALDO18, SALDO19, SALDO20, SALDO21, SALDO22), na.rm = TRUE))%>% as.data.frame()
srednie_migracje
migracje_pogodowe <- srednie_migracje %>%
  inner_join(pogodaIndeks, by = c("Nazwa" = "country_name"))
head(migracje_pogodowe)

#przedzialy temperatury
migracje_pogodowe$mean_temp_category <- cut(migracje_pogodowe$mean_temp_year, 
                                            breaks = c(-10, -5, 0, 5, 10, 15, 20, 25, 30, 35, 40),
                                            labels = c("-10- -5", "-5-0", "0-5", "5-10", "10-15", "15-20", "20-25", "25-30", "30-35", "35-40"))

# wczytujemy populacje
populacja <- read.csv("populacja.csv", row.names = NULL)
colnames(populacja)[1] <- "country_name"
populacja <- populacja[,c("country_name", "X2022")]
migracje_pogodowe <- populacja %>%
  inner_join(migracje_pogodowe, by = c("country_name" = "Nazwa")) %>%
  mutate(emigracje_per_1000 = srednia_emigracja / X2022 * 1000)
migracje_pogodowe
colnames(migracje_pogodowe)

# wykres na 1000 ludzi
ggplot(migracje_pogodowe, aes(x = mean_temp_category, y = emigracje_per_1000)) +
  geom_bar(stat = "identity", fill = "#4d2f02") +
  labs(title = "ZALEŻNOŚĆ LICZBY EMIGRANTÓW NA 1000 MIESZKAŃCÓW OD TEMPERATURY W KRAJU DOCELOWYM",
       x = "średnia roczna temperatura",
       y = "Liczba emigrantów na 1000 mieszkańców")+
  theme(plot.title = element_text(hjust = 0.5))

# wykres ilosci emigrujacych ludzi od temperatury - histogram
migracje_pogodowe11 <- migracje_pogodowe %>%
  group_by(mean_temp_category) %>%
  summarise(ilosc_ludzi_emi = sum(srednia_emigracja), ilosc_ludzi_saldo = sum(srednie_saldo)) %>% as.data.frame()

ggplot(migracje_pogodowe11, aes(x = mean_temp_category, y = ilosc_ludzi_emi)) +
  geom_bar(stat = "identity", fill = "#4d2f02") +
  labs(title = "ZALEŻNOŚĆ LICZBY EMIGRANTÓW OD TEMPERATURY W KRAJU DOCELOWYM",
       x = "Średnia roczna temperatura",
       y = "Liczba emigrantów") +
  theme(plot.title = element_text(hjust = 0.5))

# saldo temp
ggplot(migracje_pogodowe11, aes(x = mean_temp_category, y = ilosc_ludzi_saldo)) +
  geom_bar(stat = "identity") +
  labs(title = "ZALEŻNOŚĆ ILOŚCI EMIGRANTÓW OD TEMPERATURY W KRAJU DOCELOWYM",
       x = "średnia temperatura w ciągu roku",
       y = "Saldo") +
  theme_minimal()


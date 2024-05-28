library(ggplot2)
#install.packages("plotly")
library(plotly)
ggplot(pogodaIndeks, aes(x = LadderScore, y = mean_temp_year, color=Continents, shape=Continents) ) +  geom_point()
ggplot(pogodaIndeks, aes(x = LadderScore, y = year_mean_sunshine_duration, color=Continents, shape=Continents) ) +  geom_point()
ggplot(pogodaIndeks, aes(x = LadderScore, y = maks_aplitude, color=Continents, shape=Continents) ) +  geom_point()
ggplot(pogodaIndeks, aes(x = LadderScore, y = max_max_temp, color=Continents, shape=Continents) ) +  geom_point()
ggplot(migracje_pogodowe, aes(x = srednia_emigracja, y = mean_temp_year, color=Continents, shape=Continents) ) +  geom_point()

# ok wykresy
# mean temp
# krzywa naj dopasowania

ggplot(pogodaIndeks, aes(y = LadderScore, x = mean_temp_year, color = Continents, shape = Continents)) +
  geom_point() +
  geom_smooth(aes(group = 1), method = "lm", se = FALSE, color = "black", lwd = 0.2) +
  labs(title = "ZALEŻNOŚĆ INDEKSU SZCZĘŚCIA OD TEMPERATURY",
       y = "Indeks szczęścia",
       x = "Srednia roczna temperatura")+
  theme(plot.title = element_text(hjust = 0.5))

correlation <- cor(pogodaIndeks$mean_temp_year,pogodaIndeks$LadderScore)
correlation
?cor

# bez krzywej
ggplot(pogodaIndeks, aes(y = LadderScore, x = mean_temp_year, color = Continents, shape = Continents)) +
  geom_point() +
  labs(title = "ZALEŻNOŚĆ TEMPERATURY OD INDEKSU SZCZĘŚCIA",
       y = "Indeks szczęścia",
       x = "Średnia roczna temperatura") +
  theme(plot.title = element_text(hjust = 0.5))


# słońce
# krzywa naj dopasowania
ggplot(pogodaIndeks, aes(x = LadderScore, y = year_mean_sunshine_duration, color = Continents, shape = Continents)) +
  geom_point() +
  geom_smooth(aes(group = 1), method = "lm", se = FALSE, color = "black", lwd = 0.2) +
  labs(title = "ZALEŻNOŚĆ GODZIN SŁONECZNYCH OD INDEKSU SZCZĘŚCIA",
       x = "Indeks szczęścia",
       y = "Średnia ilość godzin słonecznych")+
  theme(plot.title = element_text(hjust = 0.5))
correlation <- cor(pogodaIndeks$LadderScore, pogodaIndeks$mean_temp_year)
correlation
?cor

# bez krzywej
ggplot(pogodaIndeks, aes(x = LadderScore, y = year_mean_sunshine_duration, color = Continents, shape = Continents)) +
  geom_point() +
  labs(title = "ZALEŻNOŚĆ GODZIN SŁONECZNYCH OD INDEKSU SZCZĘŚCIA",
       x = "Indeks szczęścia",
       y = "Średnia ilość godzin słonecznych")+
  theme(plot.title = element_text(hjust = 0.5))





#log migracje
ggplot(migracje_pogodowe, aes(y = srednia_emigracja, x = mean_temp_year, color = Continents, shape = Continents)) + 
  geom_point() +
  scale_y_log10() +
  labs(title = "ZALEŻNOŚĆ ŚREDNIEJ LICZBY EMIGRANTÓW OD TEMPERATURY",
       y = "Średnia liczba emigrantów (skala logarytmiczna)",
       x = "Średnia roczna temperatura",
       color = "Continents",
       shape = "Continents") + 
  theme(plot.title = element_text(hjust = 0.5))

correlation <- cor(migracje_pogodowe$srednia_emigracja, migracje_pogodowe$mean_temp_year)
correlation


# wykres ilosci emigrujacych ludzi od temperatury
migracje_pogodowe$mean_temp_category <- cut(migracje_pogodowe$mean_temp_year, 
                                            breaks = c(-10, -5, 0, 5, 10, 15, 20, 25, 30, 35, 40),
                                            labels = c("-10- -5", "-5-0", "0-5", "5-10", "10-15", "15-20", "20-25", "25-30", "30-35", "35-40"))
migracje_pogodowe <- migracje_pogodowe %>%
  group_by(mean_temp_category) %>%
  summarise(ilosc_ludzi_emi = sum(srednia_emigracja), ilosc_ludzi_saldo = sum(srednie_saldo)) %>% as.data.frame()

ggplot(migracje_pogodowe, aes(x = mean_temp_category, y = ilosc_ludzi_emi)) +
  geom_bar(stat = "identity") +
  labs(title = "ZALEŻNOŚĆ ILOŚCI EMIGRANTÓW OD TEMPERATURY W KRAJU DOCELOWYM",
       x = "średnia temperatura w ciągu roku",
       y = "Ilość emigrantów") +
  theme_minimal()

# saldo temp
ggplot(migracje_pogodowe, aes(x = mean_temp_category, y = ilosc_ludzi_saldo)) +
  geom_bar(stat = "identity") +
  labs(title = "ZALEŻNOŚĆ ILOŚCI EMIGRANTÓW OD TEMPERATURY W KRAJU DOCELOWYM",
       x = "średnia temperatura w ciągu roku",
       y = "Saldo") +
  theme_minimal()


#ok!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!11
p2 <- plot_ly(data = pogodaIndeks, 
              y = ~mean_temp_year, 
              x = ~LadderScore, 
              type = 'scatter', 
              mode = 'markers', 
              color = ~Continents,
              text = ~paste('Kraj:', country_name, '<br>LadderScore:', round(LadderScore, digits = 2), '<br>Mean Temp Year:', round(mean_temp_year, digits = 2)), 
              hoverinfo = 'text') %>%
  layout(
    yaxis = list(title = 'Mean Temp Year'),
    xaxis = list(title = 'LadderScore'),
    title = 'Scatter Plot of LadderScore vs Mean Temp Year by Continents',
    width = 750,
    height = 590
  ) %>%
  config(
    displayModeBar = TRUE, 
    responsive = TRUE
  )

# Display the plot
p2

link <- api_create(p2, filename = "moj_wykres", sharing = "public")

# Wyświetlenie linku
print(link)


library(htmlwidgets)
saveWidget(p2, file = "moj_wykres.html")

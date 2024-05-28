library(dplyr)
library(ggplot2)
library(config)
library(plotly)
turystyka <- read.csv("turystyka.csv", row.names = NULL)
turystyka <- turystyka[ ,c("CountryName", "X2014", "X2015", "X2016", "X2017", "X2018", "X2019")]
turystyka <- turystyka %>%
  group_by(CountryName) %>%
  summarise(mean_tourists_sum = mean(c(X2014, X2015,X2016, X2017, X2018, X2019), na.rm = TRUE)) %>%
  as.data.frame()
turystyka <- na.omit(turystyka)
turysci <- turystyka %>%
   inner_join(pogodaIndeks, by = c("CountryName" = "country_name"))
turysci
?cor

#########################################################################################
test <- pogodaIndeks %>%
  mutate(temprain = mean_temp_year + year_mean_sunshine_duration)
test
plot_ly(data = test, 
        y = ~temprain, 
        x = ~LadderScore, 
        type = 'scatter', 
        mode = 'markers', 
        color = ~Continents,
        text = ~paste('Kraj:', country_name, '<br>LadderScore:', round(LadderScore, digits = 2), '<br>temprain:', round(temprain, digits = 2)), 
        hoverinfo = 'text') %>%
  layout(
    yaxis = list(title = 'test'),
    xaxis = list(title = 'LadderScore'),
    title = 'Scatter Plot of LadderScore vs Mean Temp Year by Continents',
    width = 750,
    height = 590
  ) %>%
  config(
    displayModeBar = TRUE, 
    responsive = TRUE
  )
correlation <- cor(test$LadderScore, test$temprain)
correlation 

#########################################################################
ggplot(turysci, aes(x = mean_tourists_sum, y = mean_temp_year, color = Continents, shape = Continents)) +
  geom_point() +
  scale_x_log10() +
  geom_smooth(aes(group = 1), method = "lm", se = FALSE, color = "black", lwd = 0.2) +
  labs(title = "Scatter Plot with One Best Fit Line",
       x = "Mean tourists sum in a year",
       y = "Mean Temp Year")
correlation <- cor(turysci$mean_tourists_sum, turysci$mean_temp_year, method = "spearman")
correlation 
correlation1 <- cor(turysci$mean_tourists_sum, turysci$mean_temp_year)
correlation1


ggplot(turysci, aes(x = mean_tourists_sum, y = year_mean_sunshine_duration, color = Continents, shape = Continents)) +
  geom_point() +
  scale_x_log10() +
  geom_smooth(aes(group = 1), method = "lm", se = FALSE, color = "black", lwd = 0.2) +
  labs(title = "Scatter Plot with One Best Fit Line",
       x = "Mean tourists sum in a year",
       y = "Mean sunshine")
correlation <- cor(turysci$mean_tourists_sum, turysci$year_mean_sunshine_duration, method = "spearman")
correlation 
correlation1 <- cor(turysci$mean_tourists_sum, turysci$year_mean_sunshine_duration)
correlation1


plot_ly(data = turysci, 
        y = ~mean_temp_year, 
        x = ~mean_tourists_sum, 
        type = 'scatter', 
        mode = 'markers', 
        color = ~Continents,
        text = ~paste('Kraj:', CountryName, '<br>Mean tourists sum in a year:',mean_tourists_sum , '<br>Mean temp:', round(mean_temp_year, digits = 2)), 
        hoverinfo = 'text') %>%
  layout(
    yaxis = list(title = 'Mean Temp Year'),
    xaxis = list(title = 'tourist', type = "log"),
    title = 'Scatter Plot of tourists vs Mean Temp Year by Continents',
    width = 750,
    height = 590
  ) %>%
  config(
    displayModeBar = TRUE, 
    responsive = TRUE
  )


########################
# wykres bąbelkowy
p <- ggplot(turysci, aes(x = LadderScore, y = mean_temp_year, size = mean_tourists_sum, text = CountryName)) +
  geom_point(alpha = 0.5, color = "blue") +
  scale_size_continuous(range = c(3, 15), name = "Tourists") +
  labs(
    title = "ZALEŻNOŚĆ ILOŚCI TURYSTÓW OD INDEKTU SZCZĘŚCIA I TEMPERATURY",
    x = "Indeks szczęścia",
    y = "Średnia roczna temperatura"
  ) +
  theme_minimal()

ggplotly(p, tooltip = "text")



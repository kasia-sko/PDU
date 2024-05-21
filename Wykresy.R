library(ggplot2)
install.packages("plotly")
library(plotly)
ggplot(pogodaIndeks, aes(x = LadderScore, y = mean_temp_year, color=Continents, shape=Continents) ) +  geom_point()
ggplot(pogodaIndeks, aes(x = LadderScore, y = year_mean_sunshine_duration, color=Continents, shape=Continents) ) +  geom_point()
ggplot(pogodaIndeks, aes(x = LadderScore, y = maks_aplitude, color=Continents, shape=Continents) ) +  geom_point()
ggplot(pogodaIndeks, aes(x = LadderScore, y = max_max_temp, color=Continents, shape=Continents) ) +  geom_point()
ggplot(migracje_pogodowe, aes(x = srednia_emigracja, y = mean_temp_year, color=Continents, shape=Continents) ) +  geom_point()

# krzywa naj dopasowania
ggplot(pogodaIndeks, aes(x = LadderScore, y = mean_temp_year, color = Continents, shape = Continents)) +
  geom_point() +
  geom_smooth(aes(group = 1), method = "lm", se = FALSE, color = "black", lwd = 0.2) +
  labs(title = "Scatter Plot with One Best Fit Line",
       x = "LadderScore",
       y = "Mean Temp Year")
correlation <- cor(pogodaIndeks$LadderScore, pogodaIndeks$mean_temp_year)
correlation
?cor


ggplot(migracje_pogodowe, aes(x = srednia_emigracja, y = mean_temp_year, color = Continents, shape = Continents)) + 
  geom_point() +
  scale_x_log10() +
  labs(x = "Srednia Emigracja (log scale)", y = "Mean Temperature Year", color = "Continents", shape = "Continents") +
  theme_minimal()



# wykres ilosci emigrujacych ludzi od temperatury
migracje_pogodowe$mean_temp_category <- cut(migracje_pogodowe$mean_temp_year, 
                                            breaks = c(-10, -5, 0, 5, 10, 15, 20, 25, 30, 35, 40),
                                            labels = c("-10- -5", "-5-0", "0-5", "5-10", "10-15", "15-20", "20-25", "25-30", "30-35", "35-40"))
migracje_pogodowe11 <- migracje_pogodowe %>%
  group_by(mean_temp_category) %>%
  summarise(ilosc_ludzi = sum(srednia_emigracja)) %>% as.data.frame()

ggplot(migracje_pogodowe11, aes(x = mean_temp_category, y = ilosc_ludzi)) +
  geom_bar(stat = "identity") +
  labs(title = "Barplot of People Count by Mean Temperature Category",
       x = "Mean Temperature Category (°C)",
       y = "People Count") +
  theme_minimal()



# 
# p2 <- nPlot(LadderScore ~ mean_temp_year, group = "Continents", data = pogodaIndeks, type = "scatterChart")
# p2$xAxis(axisLabel = 'LadderScore')
# p2$yAxis(axisLabel = 'mean temp')
# p2$chart(tooltipContent = "#! function(key, x, y, e){ 
#   return 'Kraj: ' + e.point.country_name
# } !#")
# p2$set(width = 750, height = 590)

# plot <- plot_ly(pogodaIndeks, x = ~mean_temp_year, y = ~LadderScore, type = 'scatter', mode = 'markers', 
#                 color = ~Continents, symbol = ~Continents) %>%
#   layout(
#     xaxis = list(type = "log", title = "Mean Temperature Year (log scale)"),
#     yaxis = list(type = "log", title = "Ladder Score (log scale)"),
#     title = "Interactive Scatter Plot with Logarithmic Scales"
#   )
# 
# # Display the plot
# plot


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

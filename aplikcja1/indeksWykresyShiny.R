library(shiny)
library(plotly)
library(ggplot2)

pogodaIndeksShiny <- read.csv("C:\\Users\\Kasia\\Desktop\\PDU\\PracaDomowa2\\aplikcja1\\pogodaIndeks")
pogodaIndeksShiny

choices <- setNames(names(pogodaIndeksShiny), 
                    c("X", "Id kraju", "Nazwa kraju", 
                      "Średnia roczna temperatura", "Średnia temperatura w najcieplejszym miesiącu", "Średnia temperatura w najzminiejszym miesiącu", 
                      "Największa różnica temperatur", "Maksymalna temperatura", "Minimalna temperatura", 
                      "Roczna suma opadów", "Średnia długość dnia", "Średnia ilość godzin słonecznych", 
                      "Współczynnik szczęścia", "Kontynent"))
filtered_choices <- choices[c(4,5,6,7,8,9,10,12)]

ui <- fluidPage(
  tags$head(tags$style(HTML("
    #title {
      text-align: center;
    }
  "))),
  div(id = "title", titlePanel("ZALEŻNOŚĆ INDEKSU SZCZĘŚCIA OD POGODY")),
  br(),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "yvar",
        label = "Wybierz wartość na osi Y:",
        #choices = names(pogodaIndeksShiny),
        choices = filtered_choices, #numery ktore chcemy: 
        selected = "mean_temp_year"
      ),
      #textOutput("correlation_text")
      htmlOutput("correlation_text")
    ),
    
    mainPanel(
      plotlyOutput("wykres")
    )
  )
)

server <- function(input, output) {
  output$wykres <- renderPlotly({
    yvar_label <- names(filtered_choices)[filtered_choices == input$yvar]
    p2 <- plot_ly(data = pogodaIndeksShiny, 
                  x = as.formula(paste0("~`", input$yvar, "`")), 
                  y = ~LadderScore, 
                  type = 'scatter', 
                  mode = 'markers', 
                  color = ~Continents,
                  text = ~paste('Kraj:', country_name, '<br>LadderScore:', round(LadderScore, digits = 2), '<br>', input$yvar, ':', round(get(input$yvar), digits = 2)), 
                  hoverinfo = 'text') %>%
      layout(
        xaxis = list(title = yvar_label),
        yaxis = list(title = 'Indeks szczęścia'),
        width = 750,
        height = 590
      ) %>%
      config(
        displayModeBar = TRUE, 
        responsive = TRUE
      )
    
    p2  # Zwrócenie wykresu
  })
  
  output$correlation_text <- renderText({
    yvar_data <- pogodaIndeksShiny[[input$yvar]]
    cor_value <- cor(pogodaIndeksShiny$LadderScore, yvar_data, use = "complete.obs")
    #paste("Współczynnik korelacji między LadderScore a", input$yvar, "wynosi:", round(cor_value, 2))
    
    cor_strength <- ifelse(abs(cor_value) < 0.2, "nikła",
                           ifelse(abs(cor_value) < 0.4, "niska",
                                  ifelse(abs(cor_value) < 0.6, "umiarkowana",
                                         ifelse(abs(cor_value) < 0.8, "wysoka", "bardzo wysoka"))))
    
    HTML(paste("<div style='font-size: 18px; font-weight: bold; color: #2C3E50;'>",
               "Współczynnik korelacji wynosi:", round(cor_value, 2),  
               "<br> Siła korelacji: <span style='color:#E74C3C;'>", cor_strength, "</span>",
               "</div>"))
  })
}

shinyApp(ui, server)

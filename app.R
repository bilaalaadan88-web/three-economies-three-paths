library(shiny)
library(tidyverse)
library(DT)

data_raw <- read_csv("inflation interest unemployment.csv")

data_clean <- data_raw %>%
     select(
          country,
          year,
          inflation = `Inflation, consumer prices (annual %)`,
          interest_rate = `Real interest rate (%)`,
          unemployment = `Unemployment, total (% of total labor force) (modeled ILO estimate)`
     ) %>%
     filter(country %in% c("United States", "India", "Japan"))

ui <- fluidPage(
     titlePanel("Three Economies, Three Paths: USA, India & Japan"),
     
     sidebarLayout(
          sidebarPanel(
               selectizeInput(
                    "countries", "Select countries:",
                    choices = c("United States", "India", "Japan"),
                    selected = c("United States", "India", "Japan"),
                    multiple = TRUE
               ),
               sliderInput(
                    "year_range", "Year range:",
                    min = 1991, max = 2021,
                    value = c(1991, 2021), sep = ""
               ),
               width = 3
          ),
          
          mainPanel(
               fluidRow(
                    column(6, plotOutput("inflation_trend", height = "350px")),
                    column(6, plotOutput("interest_trend", height = "350px"))
               ),
               fluidRow(
                    column(6, plotOutput("unemployment_trend", height = "350px")),
                    column(6, plotOutput("phillips_curve", height = "350px"))
               ),
               fluidRow(
                    column(6, plotOutput("volatility_bar", height = "350px")),
                    column(6, br())
               ),
               fluidRow(
                    column(12, h4("Summary Table"), DTOutput("summary_table"))
               ),
               width = 9
          )
     )
)

server <- function(input, output) {
     
     filtered_data <- reactive({
          data_clean %>%
               filter(country %in% input$countries,
                      year >= input$year_range[1],
                      year <= input$year_range[2])
     })
     
     # Chart 1: Inflation trend
     output$inflation_trend <- renderPlot({
          ggplot(filtered_data(), aes(x = year, y = inflation, color = country)) +
               geom_line(linewidth = 1) +
               labs(title = "Inflation Trend", y = "Inflation (annual %)", x = NULL) +
               theme_minimal()
     })
     
     # Chart 2: Interest rate trend
     output$interest_trend <- renderPlot({
          filtered_data() %>%
               filter(!is.na(interest_rate)) %>%
               ggplot(aes(x = year, y = interest_rate, color = country)) +
               geom_line(linewidth = 1) +
               labs(title = "Real Interest Rate Trend", y = "Real interest rate (%)", x = NULL) +
               theme_minimal()
     })
     
     # Chart 3: Unemployment trend
     output$unemployment_trend <- renderPlot({
          filtered_data() %>%
               filter(!is.na(unemployment)) %>%
               ggplot(aes(x = year, y = unemployment, color = country)) +
               geom_line(linewidth = 1) +
               labs(title = "Unemployment Trend", y = "Unemployment (%)", x = NULL) +
               theme_minimal()
     })
     
     # Chart 4: Phillips Curve (unemployment vs inflation)
     output$phillips_curve <- renderPlot({
          filtered_data() %>%
               filter(!is.na(unemployment), !is.na(inflation)) %>%
               ggplot(aes(x = unemployment, y = inflation, color = country)) +
               geom_point(size = 2, alpha = 0.7) +
               labs(title = "Phillips Curve: Unemployment vs Inflation",
                    x = "Unemployment (%)", y = "Inflation (%)") +
               theme_minimal()
     })
     
     # Chart 5: Inflation volatility comparison (standard deviation)
     output$volatility_bar <- renderPlot({
          filtered_data() %>%
               group_by(country) %>%
               summarise(volatility = sd(inflation, na.rm = TRUE)) %>%
               ggplot(aes(x = reorder(country, volatility), y = volatility, fill = country)) +
               geom_col(show.legend = FALSE) +
               coord_flip() +
               labs(title = "Inflation Volatility (Std. Deviation)",
                    x = NULL, y = "Standard Deviation (%)") +
               theme_minimal()
     })
     
     # Table: Summary statistics
     output$summary_table <- renderDT({
          filtered_data() %>%
               group_by(country) %>%
               summarise(
                    Avg_Inflation = round(mean(inflation, na.rm = TRUE), 2),
                    Max_Inflation = round(max(inflation, na.rm = TRUE), 2),
                    Min_Inflation = round(min(inflation, na.rm = TRUE), 2),
                    Avg_Interest_Rate = round(mean(interest_rate, na.rm = TRUE), 2),
                    Avg_Unemployment = round(mean(unemployment, na.rm = TRUE), 2),
                    Inflation_Volatility = round(sd(inflation, na.rm = TRUE), 2)
               ) %>%
               datatable(options = list(pageLength = 10))
     })
}

shinyApp(ui = ui, server = server)



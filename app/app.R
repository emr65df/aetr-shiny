library(shiny)
library(bslib)
library(readr)
library(dplyr)
library(ggplot2)
library(DT)

#data
source <- "https://raw.githubusercontent.com/acep-uaf/aetr-web-book-2024/main/data/working/prices/weighted_prices.csv"
weighted_prices <- read_csv(url(source))

# Define UI for application that draws a histogram
ui <- page_sidebar(
  title = "Prices of Electricity",
  sidebar = sidebar(
    selectInput(inputId = "acep_energy_region",
                label = "Select Region",
                choices = c("Coastal","Railbelt","Rural Remote"),
                selected = "Coastal")
  ),
  card(
    plotOutput(outputId = "line_plot", hover = "plot_hover"),
    dataTableOutput(outputId = "hover_table")
  )#,
  # card(dataTableOutput(outputId = "hover_table")
  # )
)


# Define server logic required to draw a histogram
server <- function(input, output) {

  regional_subset <- reactive({
    weighted_prices %>%
      filter(acep_energy_region == input$acep_energy_region)
  })

  output$line_plot <- renderPlot({
    ggplot(regional_subset(), aes(x = year, y = weighted_price, colour = sector)) +
      geom_line()
})

  output$hover_table <- renderDataTable({
    nearPoints(regional_subset(), input$plot_hover)
  })
}
# Run the application
shinyApp(ui = ui, server = server)

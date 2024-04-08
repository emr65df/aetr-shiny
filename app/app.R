library(shiny)
library(bslib)
# library(readr)
# library(dplyr)
# library(ggplot2)

#data
# source <- "https://raw.githubusercontent.com/acep-uaf/aetr-web-book-2024/main/data/working/prices/weighted_prices.csv"
# weighted_prices <- read_csv(url(source))

# Define UI for application that draws a histogram
ui <- page_sidebar(
  title = "Prices of Electricity",
  sidebar = sidebar(
    selectInput(inputId = "acep_energy_region",
                label = "Select Region",
                choices = c("Coastal","Railbelt","Rural Remote"),
                selected = "Coastal")

  )

)


# Define server logic required to draw a histogram
server <- function(input, output) {

}
# Run the application
shinyApp(ui = ui, server = server)

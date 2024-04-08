#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(bslib)
library(readr)

source <- "https://raw.githubusercontent.com/acep-uaf/aetr-web-book-2024/main/data/working/prices/weighted_prices.csv"
weighted_prices <- read_csv(url(source))
# Define UI for application that draws a histogram
ui <- page_sidebar(

)

# Define server logic required to draw a histogram
server <- function(input, output) {

}

# Run the application
shinyApp(ui = ui, server = server)

library(shiny)
library(bslib)
library(readr)
library(dplyr)
library(ggplot2)
#library(DT)
library(ggiraph)
library(kableExtra)
library(purrr)

#data
source <- "https://raw.githubusercontent.com/acep-uaf/aetr-web-book-2024/main/data/working/prices/weighted_prices.csv"
weighted_prices <- read_csv(url(source))

make_table <- function(use_year, use_acep_energy_region, use_sector) {
  weighted_prices %>%
    dplyr::filter(year == use_year & acep_energy_region == use_acep_energy_region & sector == use_sector) %>%
    select(year, weighted_price) %>%
    kableExtra::kbl(digits = 3, row.names = FALSE)
}

df <- weighted_prices %>%
  dplyr::rowwise() %>%
  dplyr::mutate(table = make_table(year, acep_energy_region, sector)) #%>%



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
    girafeOutput(outputId = "plot")
  )#,
  # card(dataTableOutput(outputId = "hover_table")
  # )
)


# Define server logic required to draw a histogram
server <- function(input, output) {

  regional_subset <- reactive({
    df %>%
      filter(acep_energy_region == input$acep_energy_region)
  })

  output$plot <- renderGirafe({
    object <- ggplot(regional_subset(), aes(x = year, y = weighted_price, colour = sector)) +
      geom_point_interactive(aes(tooltip = table)) +
      geom_line(alpha = 0.3) +
      scale_x_continuous(breaks = seq(min(df$year), max(df$year), by = 1)) +
      scale_y_continuous(limits = c(10,70), breaks = seq(10,70, by = 10)) +
      theme_bw()

    girafe(ggobj = object,
           options = list(
             opts_tooltip(opacity = .7,
                          offx = 10, offy = -50,
                          delay_mouseover = 50,
                          delay_mouseout = 50)
           ))
})

  # output$hover_table <- renderDataTable({
  #   nearPoints(regional_subset(), input$plot_hover)
  # }, rownames = FALSE)
}
# Run the application
shinyApp(ui = ui, server = server)

# library(shiny)
# library(dplyr)
# library(ggplot2)
# library(ggiraph)
#
#
# ui <- shinyUI(fluidPage(
#
#
#   titlePanel("Shiny & ggiraph"),
#
#
#   sidebarLayout(
#     sidebarPanel(
#       selectInput("species",
#                   "Select species:",
#                   selected = "setosa",
#                   choices = unique(levels(iris$Species))
#       )
#     ),
#
#
#     mainPanel(
#       girafeOutput("plotIris")
#     )
#   )
# ))
#
#
# server <- shinyServer(function(input, output) {
#   filterIris <- reactive({
#     filter(iris, Species == input$species)
#   })
#
  # output$plotIris <- renderGirafe({
  #   gg <- ggplot(filterIris(), aes(x = Sepal.Length, y = Petal.Length))
  #   gg <- gg + geom_point_interactive(
  #     aes(tooltip = filterIris()$Sepal.Length), size = 2)
  #   girafe(code = print(gg))
  # })
#
# })
#
#
# shinyApp(ui = ui, server = server)

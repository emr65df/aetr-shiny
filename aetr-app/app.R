library(shiny)
library(bslib)
library(readr)
library(dplyr)
library(ggplot2)
library(ggiraph)
library(kableExtra)
source("helpers.R")

#access data
source <- "https://raw.githubusercontent.com/acep-uaf/aetr-web-book-2024/main/data/working/prices/weighted_prices.csv"
weighted_prices <- read_csv(url(source))

#filter data < 2020
weighted_prices_filtered <- weighted_prices %>%
  filter(year <= 2019)

#add table to each row for tooltip to work
df <- weighted_prices_filtered %>%
  dplyr::rowwise() %>%
  dplyr::mutate(table = make_table(.,year, acep_energy_region, sector)) #%>%

# Define UI for application that draws a histogram
ui <- page_sidebar(
  theme = bs_theme(preset = "vapor"),
  title = "Prices of Electricity",
  sidebar = sidebar(
    selectInput(inputId = "acep_energy_region",
                label = "Select Region",
                choices = c("Coastal","Railbelt","Rural Remote"),
                selected = "Coastal"),
    # p("Download Data Below"),
    # radioButtons(inputId = "filetype",
    #              label = "Select filetype:",
    #              choices = c("csv", "tsv"),
    #              selected = "csv"),
    div(style = "position: absolute; bottom: 10px; right: 8%;",
        tags$img(height = 65, width = 215, src = "acep-logo.png")),
    #tags$a("2024 Alaska Electricity", tags$br(), "Trends Report", href = "https://acep-uaf.github.io/aetr-web-book-2024/")
    ),
  #   downloadButton("download_data", "All Regions")
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
      labs(title = paste(input$acep_energy_region, "Trends in Price of Electricity"),
           subtitle = "hover over points for details") +
      ylab("Cents per\nKilowatt-hour") +
      #theme_bw() +
      theme(axis.title.y = element_text(angle=0, size = 7, colour = "grey45"),
            axis.title.x = element_text(size = 7, colour = "grey45", hjust = 1),
            axis.text.x = element_text(colour = "white"),
            axis.text.y = element_text(colour = "white"),
            plot.title = element_text(face = "bold", colour = "white"),
            plot.subtitle = element_text(size = 7, colour = "white"),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
            panel.background = element_blank(),  # Make panel background transparent
            plot.background = element_rect(fill = "#30115e"),
            legend.background = element_blank(),
            legend.text = element_text(colour = "white"),
            legend.title = element_text(colour = "white")
            )

    girafe(ggobj = object,
           options = list(
             opts_tooltip(opacity = .9,
                          offx = 10, offy = -100,
                          delay_mouseover = 50,
                          delay_mouseout = 50)
           ),
           width_svg = 8, height_svg = 4)
})

  # output$hover_table <- renderDataTable({
  #   nearPoints(regional_subset(), input$plot_hover)
  # }, rownames = FALSE)
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("weighted_prices.", input$filetype)
    },
    content = function(file) {
      if(input$filetype == "csv"){
        write_csv(weighted_prices, file)
      }
      if(input$filetype == "tsv"){
        write_tsv(weighted_prices, file)
      }
    }
  )
}
# Run the application
shinyApp(ui = ui, server = server)

library(shiny)
library(bslib)
library(readr)
library(dplyr)
library(ggplot2)
library(plotly)
library(kableExtra)
source("helpers.R")


#installed capacity data
source_capacity <- "https://raw.githubusercontent.com/acep-uaf/aetr-web-book-2024/main/data/working/capacity/capacity_long.csv"
capacity <- read_csv(url(source_capacity))
#filter data prime_mover != NA
capacity_filtered <- capacity %>%
  filter(!is.na(prime_mover)) %>%
  mutate(across(where(is.numeric), round, 2))

#generation
source_generation <- "https://raw.githubusercontent.com/acep-uaf/aetr-web-book-2024/main/data/working/generation/net_generation_long.csv"
generation <- read_csv(url(source_generation))

#consumption
source_consumption <- "https://raw.githubusercontent.com/acep-uaf/aetr-web-book-2024/main/data/working/consumption/consumption_long.csv"
consumption <- read_csv(url(source_consumption))
#filter data <= 2019
consumption_filtered <- consumption %>%
  filter(year <= 2019) %>%
  mutate(across(where(is.numeric), round, 2))

#prices data
source_weighted_prices <- "https://raw.githubusercontent.com/acep-uaf/aetr-web-book-2024/main/data/working/prices/weighted_prices.csv"
weighted_prices <- read_csv(url(source_weighted_prices))
#filter data <= 2019
weighted_prices_filtered <- weighted_prices %>%
  filter(year <= 2019) %>%
  mutate(across(where(is.numeric), round, 2))

# Define UI for application that draws a histogram
ui <- page_navbar(
  theme = bs_theme(preset = "vapor"),
  title = "2024 Alaska Electricity Trends Report",
  id = "nav",
    sidebar = sidebar(
      selectInput(inputId = "acep_energy_region",
                  label = "Select Region",
                  choices = c("Statewide", "Coastal","Railbelt","Rural Remote"),
                  selected = "Statewide"),
      div(style = "position: absolute; bottom: 10px; right: 8%;",
        tags$img(height = 65, width = 215, src = "acep-logo.png"))
    ),
    #tags$a("2024 Alaska Electricity", tags$br(), "Trends Report", href = "https://acep-uaf.github.io/aetr-web-book-2024/")
  #   downloadButton("download_data", "All Regions")
    nav_panel("About", div(p("The purpose of this application is to showcase the 2024 Alaska
                              Electricity Trends Report graphs in a different format. Shiny Applications
                              are incredibly flexible, customizable, and allow for much more interactivity than
                              what is demonstrated here. Click some tabs, stay awhile, and hover around to explore the details of each plot.\n
                              The link below will take you to the web book."),
                           tags$a("2024 Alaska Electricity", tags$br(), "Trends Report", href = "https://acep-uaf.github.io/aetr-web-book-2024/"),
                           ),
              div(style = "position: absolute; bottom: 10px; right: 8%;",
                  p("Created by Emily Richmond"))),
    # Panel with plot ----
    nav_panel("Installed Capacity", plotlyOutput(outputId = "ic_plot")),

    # Panel with summary ----
    nav_panel("Net/Gross Generation", plotlyOutput(outputId = "ng_plot")),

    # Panel with table ----
    nav_panel("Consumption and Sales", plotlyOutput(outputId = "cs_plot")),
    nav_panel("Price of Electricity", plotlyOutput(outputId = "pe_plot"))
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  # output$about <- renderText({p("The purpose of this application to showcase the 2024 Alaska
  #                   Electricity Trends Report in a different format. Shiny Applications
  #                   are incredibly flexible and customizable.")
  # })

plot_theme <- theme(axis.title.y = element_text(angle=0, size = 7, colour = "white"),
                    axis.title.x = element_text(size = 7, colour = "white", hjust = 1),
                    axis.text.x = element_text(colour = "white"),
                    axis.text.y = element_text(colour = "white"),
                    plot.title = element_text(face = "bold", colour = "white"),
                    plot.subtitle = element_text(size = 7, colour = "white"),
                    panel.grid.minor = element_blank(),
                    panel.grid.major = element_blank(),
                    panel.background = element_rect(fill = "transparent"),
                    plot.background = element_rect(fill = "#30115e", colour = "#30115e"),
                    legend.background = element_blank(),
                    legend.text = element_text(colour = "white"),
                    legend.title = element_text(colour = "white"))

  ic_subset <- reactive({
    if (c("Statewide") %in% input$acep_energy_region)
      capacity_filtered %>%
      group_by(year, prime_mover) %>%
      summarize(capacity = sum(capacity)) %>%
      arrange(capacity, .by_group = TRUE) %>%
      mutate(prime_mover = factor(prime_mover, levels = unique(prime_mover)))
    else
      capacity_filtered %>%
      filter(acep_region == input$acep_energy_region) %>%
      group_by(year, prime_mover) %>%
      summarize(capacity = sum(capacity))  %>%
      arrange(capacity, .by_group = TRUE) %>%
      mutate(prime_mover = factor(prime_mover, levels = unique(prime_mover)))
  })

  output$ic_plot <- renderPlotly({
    object_ic <-
      p <- ggplot(ic_subset(), aes(x = year, y = capacity, fill = prime_mover)) +
        geom_area(stat = "identity", position = "stack") +
      scale_x_continuous(breaks = seq(min(ic_subset()$year), max(ic_subset()$year), by = 1)) +
      scale_y_continuous(limits = c(0,3500), breaks = seq(0,3500, by = 500)) +
      labs(title = paste(input$acep_energy_region, "Trends in Installed Capacity"),
           subtitle = "hover over points for details") +
      ylab("Capacity (mW)") +
        plot_theme

    ggplotly(p) %>%
      layout(hovermode = "x unified")
  })

  ng_subset <- reactive({
    if (c("Statewide") %in% input$acep_energy_region)
    generation %>%
      group_by(year, acep_region) %>%
      summarize(generation = sum(generation, na.rm = T)/1000) %>%
      mutate(across(where(is.numeric), round, 2))
    else
    generation %>%
      filter(acep_region == input$acep_energy_region &
             fuel_type %in% switch(input$acep_energy_region,
                                 "Coastal" = c("Oil", "Hydro", "Wind"),
                                 "Railbelt" = c("Coal", "Oil", "Gas", "Hydro", "Wind", "Solar"),
                                 "Rural Remote" = c("Oil", "Gas", "Hydro", "Wind", "Solar", "Other"))) %>%
      group_by(year, fuel_type) %>%
      summarise(generation = sum(generation, na.rm = T)/1000) %>%
      arrange(generation, .by_group = TRUE) %>%
      mutate(fuel_type = factor(fuel_type, levels = unique(fuel_type))) %>%
      mutate(across(where(is.numeric), round, 2))
  })

  output$ng_plot <- renderPlotly({
        p <- ggplot(ng_subset(), aes(x = year, y = generation, fill = switch(input$acep_energy_region,
                                                                               "Statewide" = ng_subset()$acep_region,
                                                                               "Coastal" = ng_subset()$fuel_type,
                                                                               "Railbelt" = ng_subset()$fuel_type,
                                                                               "Rural Remote" = ng_subset()$fuel_type),
                                     text = paste("generation:", generation))) +
            geom_col(position = "stack") +
            scale_x_continuous(breaks = seq(min(ng_subset()$year), max(ng_subset()$year), by = 1)) +
            scale_y_continuous(limits = c(0,7000), breaks = seq(0,7000, by = 1000)) +
            labs(title = paste(input$acep_energy_region, "Trends in Generation"),
                 subtitle = "hover over points for details") +
            ylab("Generation (GWh)") +
            plot_theme

        ggplotly(p, tooltip = "text") %>%
          layout(legend = list(title = ""),
                 hovermode = "x unified")
    })


  cs_subset <- reactive({
    if (c("Statewide") %in% input$acep_energy_region)
      consumption_filtered %>%
      filter(class %in% c("Residential", "Commercial", "Other")) %>%
      group_by(year, class) %>%
      summarise(
        "revenue" = sum(revenue, na.rm = TRUE),
        "sales" = sum(sales, na.rm = TRUE),
        "customers" = sum(customers, na.rm = TRUE)) %>%
      mutate(sales_per_capita = round((sales/customers),  2))
    else
      consumption_filtered %>%
      filter(class %in% c("Residential", "Commercial", "Other"), acep_region == input$acep_energy_region) %>%
      group_by(year, class) %>%
      summarise(
        "revenue" = sum(revenue, na.rm = TRUE)/1000,
        "sales" = sum(sales, na.rm = TRUE)/1000,
        "customers" = sum(customers, na.rm = TRUE))
  })

  output$cs_plot <- renderPlotly({
    p <- ggplot(cs_subset(), aes(x = year, y = switch(input$acep_energy_region,
                                                      "Statewide" = sales_per_capita,
                                                      "Coastal" = sales,
                                                      "Rural Remote" = sales,
                                                      "Railbelt" = sales), fill = class,
                                 text = switch(input$acep_energy_region,
                                               "Statewide" = paste("Sales per Capita (MWh):", sales_per_capita),
                                               "Coastal" = paste("Sales (GWh):", sales),
                                               "Rural Remote" = paste("Sales (GWh):", sales),
                                               "Railbelt" = paste("Sales (GWh):", sales)))) +
      geom_col(position = "stack") +
      scale_x_continuous(breaks = seq(min(cs_subset()$year), max(cs_subset()$year), by = 1)) +
      scale_y_continuous(limits = switch(input$acep_energy_region,
                                         "Statewide" = c(0,400),
                                         "Coastal" = c(0,5000),
                                         "Rural Remote" = c(0,5000),
                                         "Railbelt" = c(0,5000))) +
      labs(title = switch(input$acep_energy_region,
                          "Statewide" = paste(input$acep_energy_region, "Trends in Sales per Capita by Class"),
                          "Coastal" = paste(input$acep_energy_region, "Trends in Sales by Class"),
                          "Rural Remote" = paste(input$acep_energy_region, "Trends in Sales by Class"),
                          "Railbelt" = paste(input$acep_energy_region, "Trends in Sales by Class")),
           subtitle = "hover over points for details") +
      ylab(switch(input$acep_energy_region,
                  "Statewide" = "Sales per Capita (MWh)",
                  "Coastal" = "Sales (GWh)",
                  "Rural Remote" = "Sales (GWh)",
                  "Railbelt" = "Sales (GWh)")) +
      plot_theme

    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(title = ""),
             hovermode = "x unified")
  })

  pe_subset <- reactive({
    if (c("Statewide") %in% input$acep_energy_region)
      weighted_prices_filtered %>%
      group_by(year, sector) %>%
      summarize(weighted_price = mean(weighted_price))
    else
    weighted_prices_filtered %>%
      filter(acep_energy_region == input$acep_energy_region)
  })

  output$pe_plot <- renderPlotly({
    object_pe <- ggplotly(
      ggplot(pe_subset(), aes(x = year, y = weighted_price, colour = sector)) +
      #geom_point_interactive(aes(tooltip = table)) +
      geom_point() +
      geom_line(alpha = 0.3) +
      scale_x_continuous(breaks = seq(min(pe_subset()$year), max(pe_subset()$year), by = 1)) +
      scale_y_continuous(limits = c(10,70), breaks = seq(10,70, by = 10)) +
      labs(title = paste(input$acep_energy_region, "Trends in Price of Electricity"),
           subtitle = "hover over points for details") +
      ylab("Cents per\nKilowatt-hour") +
        plot_theme
)
    # girafe(ggobj = object_pe,
    #        options = list(
    #          opts_tooltip(opacity = 0.7,
    #                       use_stroke = TRUE,
    #                       offx = 10, offy = -100,
    #                       delay_mouseover = 50,
    #                       delay_mouseout = 50)
    #        ),
    #        width_svg = 8, height_svg = 4)
})



  # output$hover_table <- renderDataTable({
  #   nearPoints(pe_subset(), input$plot_hover)
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


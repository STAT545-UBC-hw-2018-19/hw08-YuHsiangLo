# Load necessary packages
library(shiny)
library(tidyverse)
library(plotly)
library(scales)
library(DT)
library(stringr)

bcl <- read.csv("data/bcl-data.csv", header = TRUE, sep = ",")

# Some data cleaning, since I don't use Sweetness in this app
bcl <- bcl %>%
  mutate(Type = str_to_title(Type),
         Subtype = str_to_title(Subtype),
         Country = str_to_title(Country),
         Name = str_to_title(Name)) %>%
  select(-Sweetness)

ui <- fluidPage(

  titlePanel(title = "Drink Decider", windowTitle = "Drink Decider"),

  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "priceInput", label = "Price", min = 0, max = 100,
                  value = c(10, 30), pre = "$"),

      sliderInput(inputId = "alcoholInput", label = "Alcohol content",
                  min = 2, max = 80, value = c(5, 15), post = "%"),

      checkboxGroupInput(inputId = "typeInput", label = "Product type",
                         choices = sort(unique(bcl$Type)),
                         selected = c("Beer", "Wine")),

      uiOutput("countrySelectOutput"),

      tags$b("Check the following box if you want fate to decide
             what you should drink"),
      tags$i("(Well, you still get to decide the price, alcohol content,
                type, and country)"),

      checkboxInput(inputId = "choiceFilter", label = "Let fate decide!",
                    value = FALSE),

      span("Data source:",
           tags$a("OpenDataBC",
                  href = "http://www.opendatabc.ca/dataset/bc-liquor-store-product-price-list-current-prices")),
      br(),

      span("Reference:",
           tags$a("Dean Attali",
                  href = "https://github.com/daattali/shiny-server/blob/master/bcl/app.R"))
    ),

    mainPanel(
      tags$h3(textOutput("numText")),
      tags$i("Hover on a point to see more information."),
      br(), br(),
      plotlyOutput("plot"),
      br(), br(),
      dataTableOutput("table")

      # For having different tabs for plot and table
      #tabsetPanel(
      #  tabPanel(title = "Plot", tags$h3(textOutput("numText")),
      #           tags$i("Hover on a point to see more information."),
      #           plotlyOutput("plot")),
      #  tabPanel(title = "Table", dataTableOutput("table"))
      #)
    )
  )
)

server <- function(input, output) {
  bcl_filtered <- reactive({
    if (is.null(input$typeInput) || is.null(input$countryInput)) {
      return(NULL)
    }

    filtered <- bcl %>%
      filter(Price >= input$priceInput[1],
             Price <= input$priceInput[2],
             Alcohol_Content >= input$alcoholInput[1],
             Alcohol_Content <= input$alcoholInput[2],
             Type %in% input$typeInput)

    if (!("All" %in% input$countryInput)) {
      filtered <- filtered %>%
        filter(Country %in% input$countryInput)
    }

    if (nrow(filtered) == 0) {
      return(NULL)
    }

    if (input$choiceFilter) {
      filtered <- sample_n(filtered, size = 1)
    }

    return(filtered)
  })

  output$numText <- renderText({
    num <- nrow(bcl_filtered())
    if (is.null(num)) {
      num <- 0
    }
    paste("We have", num, "drink(s) for you!")
  })

  output$countrySelectOutput <- renderUI({
    selectInput(inputId = "countryInput", label = "Country",
                choices = sort(c("All", unique(as.character(bcl$Country)))),
                selected = "All", multiple = TRUE)
  })

  output$plot <- renderPlotly({
    if (is.null(bcl_filtered())) {
      return(NULL)
    }

    g <- bcl_filtered() %>%
      ggplot(aes(x = Price, y = Alcohol_Content, color = Type, group = Country)) +
      theme_bw() +
      geom_point() +
      scale_x_continuous(labels = dollar_format(prefix = "$")) +
      scale_y_continuous(labels = percent_format(suffix = "%", scale = 1)) +
      labs(x = "Price ($)", y = "Alcohol content (%)") +
      theme(legend.title = element_blank()) +
      scale_color_manual(values = c("Wine" = "#F8766D",
                                    "Spirits" = "#7CAE00",
                                    "Beer" = "#00BFC4",
                                    "Refreshment" = "#C77CFF"))
    ggplotly(g)
  })

  output$table <- renderDataTable({
    bcl_filtered()
  })
}

shinyApp(ui = ui, server = server)
# Install required packages
options(repos = c(CRAN = "https://cran.rstudio.com/"))

install.packages(c("shiny", "shinythemes", "httr", "jsonlite", "ggplot2", "dplyr", "DT"))

library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(DT)

# Define UI
ui <- fluidPage(
    theme = shinytheme("flatly"), # Apply Flatly theme for a modern look
    tags$head(
        tags$style(HTML("
            body {
                background-color: #f7f7f7; /* Light gray background */
                color: #333333; /* Dark gray text for readability */
                font-family: 'Arial', sans-serif; /* Professional font */
            }
            .navbar-default {
                background-color: #007bff; /* Flatly default blue for navbar */
                border-color: #0056b3;
            }
            .navbar-default .navbar-brand,
            .navbar-default .navbar-nav > li > a {
                color: white; /* White navbar text */
            }
            .navbar-default .navbar-brand:hover,
            .navbar-default .navbar-nav > li > a:hover {
                color: #f8f9fa; /* Slightly lighter text on hover */
            }
            table.dataTable {
                background-color: #ffffff; /* White table background */
                color: #333333; /* Dark text for table */
            }
            table.dataTable th {
                background-color: #e9ecef; /* Light gray header background */
                color: #333333; /* Dark text for headers */
            }
            table.dataTable tbody tr:hover {
                background-color: #f1f1f1; /* Light hover effect */
            }
            .form-control {
                background-color: #ffffff; /* White input fields */
                color: #333333; /* Dark input text */
                border: 1px solid #ced4da; /* Light gray borders */
            }
            .form-control:focus {
                border-color: #80bdff; /* Blue focus border */
                box-shadow: 0 0 5px rgba(0, 123, 255, 0.5); /* Blue focus glow */
            }
            .btn-primary {
                background-color: #007bff; /* Flatly blue for buttons */
                border-color: #0056b3; /* Slightly darker border */
            }
            .btn-primary:hover {
                background-color: #0056b3; /* Darker blue on hover */
            }
            h1, h2, h3, h4, h5, h6, p, label {
                color: #333333; /* Ensure all headings and text are dark gray */
            }
        "))
    ),
    titlePanel("Enhanced COVID-19 Analysis Dashboard"),
    sidebarLayout(
        sidebarPanel(
            h4("Filter Options"),
            selectInput("country", "Select a Country:", choices = NULL),
            selectInput("comparison_country", "Select Comparison Country:", choices = NULL),
            dateRangeInput("date_range", "Select Date Range:",
                           start = Sys.Date() - 30, end = Sys.Date()),
            actionButton("refresh", "Refresh Data", class = "btn btn-primary")
        ),
        mainPanel(
            tabsetPanel(
                tabPanel("Summary", DTOutput("summary_table")),
                tabPanel("Total Cases Trend", plotOutput("cases_trend_plot")),
                tabPanel("Total Deaths Trend", plotOutput("deaths_trend_plot")),
                tabPanel("Vaccination Trends", plotOutput("vaccination_trend_plot")),
                tabPanel("Country Comparison", plotOutput("comparison_plot")),
                tabPanel("Data Table", DTOutput("filtered_table"))
            )
        )
    )
)

# Define server
server <- function(input, output, session) {
    # Reactive data loading
    covid_data <- reactive({
        tryCatch({
            file_path <- "owid-covid-data.csv"  # Ensure the file name is correct
            if (!file.exists(file_path)) {
                stop("Local data file not found!")
            }
            data <- read.csv(file_path)
            data$date <- as.Date(data$date)
            data
        }, error = function(e) {
            showNotification("Error loading local data.", type = "error")
            NULL
        })
    })

    # Populate country dropdowns
    observe({
        choices <- unique(covid_data()$location)
        updateSelectInput(session, "country", choices = choices)
        updateSelectInput(session, "comparison_country", choices = choices)
    })

    # Filtered data based on user input
    filtered_data <- reactive({
        req(covid_data(), input$country, input$date_range)
        covid_data() %>%
            filter(location == input$country & date >= input$date_range[1] & date <= input$date_range[2])
    })

    comparison_data <- reactive({
        req(covid_data(), input$comparison_country, input$date_range)
        covid_data() %>%
            filter(location == input$comparison_country & date >= input$date_range[1] & date <= input$date_range[2])
    })

    # Render summary table
    output$summary_table <- renderDT({
        data <- filtered_data()
        if (is.null(data) || nrow(data) == 0) {
            datatable(data.frame(Message = "No data available"))
        } else {
            summary <- data %>%
                summarise(
                    Total_Cases = max(total_cases, na.rm = TRUE),
                    Total_Deaths = max(total_deaths, na.rm = TRUE),
                    Total_Vaccinations = max(total_vaccinations, na.rm = TRUE)
                )
            datatable(summary, options = list(pageLength = 5))
        }
    })

    # Render total cases trend plot
    output$cases_trend_plot <- renderPlot({
        data <- filtered_data()
        if (is.null(data) || nrow(data) == 0) return(NULL)
        ggplot(data, aes(x = date, y = total_cases)) +
            geom_line(color = "#007bff", size = 1) +
            labs(title = paste("Total Cases in", input$country),
                 x = "Date", y = "Total Cases") +
            theme_minimal()
    })

    # Render total deaths trend plot
    output$deaths_trend_plot <- renderPlot({
        data <- filtered_data()
        if (is.null(data) || nrow(data) == 0) return(NULL)
        ggplot(data, aes(x = date, y = total_deaths)) +
            geom_line(color = "#dc3545", size = 1) +
            labs(title = paste("Total Deaths in", input$country),
                 x = "Date", y = "Total Deaths") +
            theme_minimal()
    })

    # Render vaccination trends plot
    output$vaccination_trend_plot <- renderPlot({
        data <- filtered_data()
        if (is.null(data) || nrow(data) == 0) return(NULL)
        ggplot(data, aes(x = date, y = total_vaccinations)) +
            geom_line(color = "#28a745", size = 1) +
            labs(title = paste("Vaccination Trends in", input$country),
                 x = "Date", y = "Total Vaccinations") +
            theme_minimal()
    })

    # Render comparison plot
    output$comparison_plot <- renderPlot({
        data1 <- filtered_data()
        data2 <- comparison_data()
        if (is.null(data1) || is.null(data2) || nrow(data1) == 0 || nrow(data2) == 0) return(NULL)
        combined <- rbind(
            data1 %>% mutate(Country = input$country),
            data2 %>% mutate(Country = input$comparison_country)
        )
        ggplot(combined, aes(x = date, y = total_cases, color = Country)) +
            geom_line(size = 1) +
            labs(title = "Country Comparison: Total Cases",
                 x = "Date", y = "Total Cases") +
            theme_minimal()
    })

    # Render filtered data table
    output$filtered_table <- renderDT({
        data <- filtered_data()
        if (is.null(data) || nrow(data) == 0) {
            datatable(data.frame(Message = "No data available"))
        } else {
            datatable(data, options = list(pageLength = 10))
        }
    })
}

# Run the application
shinyApp(ui = ui, server = server)

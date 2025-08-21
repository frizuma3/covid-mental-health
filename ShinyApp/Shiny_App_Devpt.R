# Load Libraries
library(shiny)         # For building interactive web apps in R
library(DT)            # For interactive tables (DataTables)
library(tidyverse)     # For data wrangling and manipulation (includes dplyr, ggplot2, etc.)
library(plotly)        # For interactive plotting
library(rmarkdown)     # For dynamic report generation (HTML/PDF/Word)
library(shinyWidgets)  # For enhanced UI widgets (like pickerInput)
library(scales)        # For number formatting in plots (e.g., comma formatting)

# DATA INPUTS 
# Paths to the cleaned datasets used in the dashboard
agesex_path <- "mhia_agesex_dataset.csv"
datatrends_path <- "mhia_datatrends_dataset.csv"
diagnosistrends_path <- "mhia_diagnosistrends_dataset.csv"

# List of dataset names and their paths for selection in UI
dataset_paths <- list(
  "Age-Sex Dataset" = agesex_path,
  "Data Trends Dataset" = datatrends_path,
  "Diagnosis Trends Dataset" = diagnosistrends_path
)

# Helper function: Remove any rows where ALL numeric columns are NA
clean_dataset <- function(data) {
  data %>% filter(if_any(where(is.numeric), ~!is.na(.)))
}

# Helper function: Generate summary statistics for a dataframe
summarize_dataset <- function(data) summary(data)

# UI DEFINITION 
ui <- fluidPage(
  titlePanel("Mental Health Datasets Explorer (Scotland)"),
  sidebarLayout(
    sidebarPanel(
      # Dropdown to select which dataset to view
      selectInput("dataset", "Select Dataset", choices = names(dataset_paths)),
      # Dynamically generated filters for all columns, incl. "year"
      uiOutput("dynamic_filters"),
      hr(),
      # Dropdown to pick which column to use as X axis in bar plot
      selectInput("xvar", "Bar Plot X-Axis", choices = NULL),
      hr(),
      # Download buttons for table as HTML/PDF/Word
      downloadButton("download_html", "Download Table (HTML)"),
      downloadButton("download_pdf", "Download Table (PDF)"),
      downloadButton("download_word", "Download Table (Word)")
    ),
    mainPanel(
      tabsetPanel(
        # Table view tab: shows data and the sum of "value"
        tabPanel("Data Table",
                 DTOutput("datatable"),
                 htmlOutput("totalsum")
        ),
        # Tab for summary statistics of the filtered dataset
        tabPanel("Summary Statistics", verbatimTextOutput("summarystats")),
        # Tab for the interactive plot
        tabPanel("Plot", plotlyOutput("plot_out"))
      )
    )
  )
)

# SERVER LOGIC
server <- function(input, output, session) {
  # Load and clean the dataset selected by user
  dataset_raw <- reactive({
    read_csv(
      dataset_paths[[input$dataset]], 
      col_types = cols(year = col_character()),  # Ensure year always read as character
      show_col_types = FALSE
    ) %>% clean_dataset()
  })
  
  # Dynamically generate filter UI for all categorical/integer columns
  output$dynamic_filters <- renderUI({
    df <- dataset_raw()
    ui_list <- list()
    # Special case: if region/hospital columns are present, add custom linkage filter
    if ("hbtreat_name" %in% names(df) && "hospital_name" %in% names(df)) {
      ui_list <- append(ui_list, list(
        pickerInput("hbtreat_name", "Filter by NHS Region", 
                    choices = c("All", unique(df$hbtreat_name)), selected = "All", 
                    options = pickerOptions(liveSearch = TRUE)),
        uiOutput("hospital_filter_ui")  # UI for hospital filtered by region
      ))
    }
    # Identify columns to generate filters for (categorical/factor/integer)
    filter_cols <- names(df)[sapply(df, function(x) is.character(x) || is.factor(x) || is.integer(x))]
    # Remove special columns (handled separately)
    filter_cols <- setdiff(filter_cols, c("hbtreat_name", "hospital_name", "year_ending"))
    # Ensure year always appears as the first filter if present
    if ("year" %in% filter_cols) {
      ui_list <- append(ui_list, list(
        pickerInput("filter_year", "Filter by year",
                    choices = c("All", sort(unique(as.character(df$year)))), 
                    selected = "All", options = pickerOptions(liveSearch = TRUE))
      ))
      filter_cols <- setdiff(filter_cols, "year")
    }
    # Generate filters for all other columns
    for (col in filter_cols) {
      ui_list <- append(ui_list, list(
        pickerInput(paste0("filter_", col), paste("Filter by", col),
                    choices = c("All", unique(as.character(df[[col]]))),
                    selected = "All", options = pickerOptions(liveSearch = TRUE))
      ))
    }
    tagList(ui_list)  # Return all filter UI elements as a list
  })
  
  # Dynamic hospital filter that depends on NHS Region selection
  output$hospital_filter_ui <- renderUI({
    df <- dataset_raw()
    req("hbtreat_name" %in% names(df))  # Only show if NHS region exists
    sel_region <- input$hbtreat_name
    # If a region is selected, show only hospitals in that region
    if (!is.null(sel_region) && sel_region != "All") {
      hosps <- unique(df$hospital_name[df$hbtreat_name == sel_region])
    } else {
      hosps <- unique(df$hospital_name)
    }
    pickerInput("hospital_name", "Filter by Hospital",
                choices = c("All", hosps), selected = "All", 
                options = pickerOptions(liveSearch = TRUE))
  })
  
  # Filter dataset according to all selected filters
  filtered_data <- reactive({
    df <- dataset_raw()
    # Filter by region, hospital, and year (special UI)
    if ("hbtreat_name" %in% names(df) && !is.null(input$hbtreat_name) && input$hbtreat_name != "All") {
      df <- df %>% filter(hbtreat_name == input$hbtreat_name)
    }
    if ("hospital_name" %in% names(df) && !is.null(input$hospital_name) && input$hospital_name != "All") {
      df <- df %>% filter(hospital_name == input$hospital_name)
    }
    if ("year" %in% names(df) && !is.null(input$filter_year) && input$filter_year != "All") {
      df <- df %>% filter(as.character(year) == input$filter_year)
    }
    # Apply all other filters from dynamic UI
    filter_cols <- names(df)[sapply(df, function(x) is.character(x) || is.factor(x) || is.integer(x))]
    filter_cols <- setdiff(filter_cols, c("hbtreat_name", "hospital_name", "year_ending", "year"))
    for (col in filter_cols) {
      sel <- input[[paste0("filter_", col)]]
      if (!is.null(sel) && sel != "All") {
        df <- df %>% filter(as.character(.data[[col]]) == sel)
      }
    }
    df
  })
  
  # Automatically update the bar plot X-axis dropdown based on filtered data
  observe({
    df <- filtered_data()
    char_choices <- setdiff(
      names(df)[sapply(df, function(x) is.character(x) || is.factor(x) || is.integer(x))],
      "value"
    )
    updateSelectInput(session, "xvar", choices = char_choices, selected = char_choices[1])
  })
  
  # Render filtered data table using DT
  output$datatable <- renderDT({
    datatable(filtered_data(), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Show the sum of the "value" column under the table
  output$totalsum <- renderUI({
    df <- filtered_data()
    if (!("value" %in% names(df)) || nrow(df) == 0) {
      HTML("<em>No values to sum.</em>")
    } else {
      v <- sum(df$value, na.rm = TRUE)
      HTML(paste0("<b>Total sum of <code>value</code>: </b>", format(v, big.mark = ",")))
    }
  })
  
  # Show summary statistics of the filtered data
  output$summarystats <- renderPrint({
    df <- filtered_data()
    if (nrow(df) == 0) {
      cat("No data to summarize.")
    } else {
      print(summarize_dataset(df))
    }
  })
  
  # Interactive bar plot: sum of value by user-selected X-axis column
  output$plot_out <- renderPlotly({
    df <- filtered_data()
    x <- input$xvar           # X axis column chosen by user
    y <- "value"              # Always plot sum of "value"
    req(x, y, nrow(df) > 0, y %in% names(df))
    # Build bar plot: sum value by x
    p <- df %>%
      group_by(.data[[x]]) %>%
      summarise(yval = sum(.data[[y]], na.rm = TRUE)) %>%
      ggplot(aes(x = .data[[x]], y = yval, fill = .data[[x]])) +
      geom_col(show.legend = FALSE) +
      scale_y_continuous(labels = comma) +  # Format Y with comma separator
      labs(title = paste("Bar Plot of", y, "by", x), x = x, y = y) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p)  # Return interactive plotly plot
  })
  
  # DOWNLOAD HANDLERS: Output filtered table and sum as HTML, PDF, or Word
  output$download_html <- downloadHandler(
    filename = function() paste0("table-", Sys.Date(), ".html"),
    content = function(file) {
      df <- filtered_data()
      tempReport <- tempfile(fileext = ".Rmd")
      # RMarkdown file for HTML download
      writeLines(
        c(
          "---", "output: html_document", "---", "",
          "```{r mytable, echo=FALSE}", "DT::datatable(df)", "```",
          "```{r mysum, echo=FALSE}", "if ('value' %in% names(df)) cat('Total sum of value:', sum(df$value, na.rm=TRUE)) else cat('No value column to sum.')", "```"
        ), tempReport
      )
      env <- new.env(parent = globalenv()) # Create new environment to hold df
      env$df <- df
      rmarkdown::render(tempReport, output_file = file, envir = env)
    }
  )
  output$download_pdf <- downloadHandler(
    filename = function() paste0("table-", Sys.Date(), ".pdf"),
    content = function(file) {
      df <- filtered_data()
      tempReport <- tempfile(fileext = ".Rmd")
      # RMarkdown file for PDF download
      writeLines(
        c(
          "---", "output: pdf_document", "---", "",
          "```{r mytable, echo=FALSE, results='asis'}", "knitr::kable(df)", "```",
          "```{r mysum, echo=FALSE}", "if ('value' %in% names(df)) cat('Total sum of value:', sum(df$value, na.rm=TRUE)) else cat('No value column to sum.')", "```"
        ), tempReport
      )
      env <- new.env(parent = globalenv())
      env$df <- df
      rmarkdown::render(tempReport, output_file = file, envir = env)
    }
  )
  output$download_word <- downloadHandler(
    filename = function() paste0("table-", Sys.Date(), ".docx"),
    content = function(file) {
      df <- filtered_data()
      tempReport <- tempfile(fileext = ".Rmd")
      # RMarkdown file for Word download
      writeLines(
        c(
          "---", "output: word_document", "---", "",
          "```{r mytable, echo=FALSE, results='asis'}", "knitr::kable(df)", "```",
          "```{r mysum, echo=FALSE}", "if ('value' %in% names(df)) cat('Total sum of value:', sum(df$value, na.rm=TRUE)) else cat('No value column to sum.')", "```"
        ), tempReport
      )
      env <- new.env(parent = globalenv())
      env$df <- df
      rmarkdown::render(tempReport, output_file = file, envir = env)
    }
  )
}

# Launch the app
shinyApp(ui, server)

library(shiny)
library(DT)
library(dplyr)
library(readr)
library(shinyjs)
library(htmltools)
library(plotly)

# Load predictions data
# Will be created by merge_predictions.py
df_path <- "data/predictions_for_app.csv"

if (file.exists(df_path)) {
  preview_df <- read.csv(df_path, stringsAsFactors = FALSE)
} else {
  # Sample data for testing
  preview_df <- data.frame(
    PMID = c(12345678, 87654321),
    Title = c("Sample paper about autophosphorylation", "Another paper without mechanism"),
    Abstract = c("This paper discusses autophosphorylation mechanisms in kinases...", 
                 "This paper discusses other topics not related to autoregulation..."),
    `Autoregulatory.Type` = c("autophosphorylation", "none"),
    `Term.Probability` = c(0.92, 0.0),
    `Has.Mechanism` = c(TRUE, FALSE),
    `Stage1.Confidence` = c(0.98, 0.95),
    check.names = TRUE
  )
}

# Clean column names
colnames(preview_df) <- gsub("\\.", " ", colnames(preview_df))

# Ensure required columns exist
required_cols <- c(
  "PMID",
  "Title", 
  "Abstract",
  "Autoregulatory Type",
  "Term Probability",
  "Has Mechanism",
  "Stage1 Confidence"
)

for (col in required_cols) {
  if (!(col %in% colnames(preview_df))) {
    preview_df[[col]] <- NA
  }
}

# Select only required columns
df <- preview_df[, required_cols]

# Format confidence scores as percentages
safe_percent_column <- function(x) {
  num <- suppressWarnings(as.numeric(x))
  percent <- ifelse(!is.na(num), paste0(round(num * 100, 2), "%"), NA)
  return(percent)
}

df$`Term Probability` <- safe_percent_column(df$`Term Probability`)
df$`Stage1 Confidence` <- safe_percent_column(df$`Stage1 Confidence`)

# Replace 'none' or NA in Autoregulatory Type with readable text
df$`Autoregulatory Type` <- ifelse(
  is.na(df$`Autoregulatory Type`) | 
    trimws(df$`Autoregulatory Type`) == "" |
    df$`Autoregulatory Type` == "none",
  "non-autoregulatory",
  df$`Autoregulatory Type`
)

# Header UI reused across all tabs
header_ui <- div(
  class = "header-section",
  div(class = "app-title", img(src = "logo.png"), "SOORENA"),
  div(class = "header-logos",
      img(src = "logoHelsinki.png"),
      img(src = "logoUBC.png"),
      img(src = "logoJafariLab.png")
  )
)

# Define UI
ui <- navbarPage(
  title = NULL,
  id = "main_nav",
  
  # Tab: Search and Main App Interface
  tabPanel(
    title = "Search",
    fluidPage(
      useShinyjs(),
      tags$head(
        tags$style(HTML("
          body {
            background-color: #f9f9f9;
          }
          .header-section {
            background-size: 50% auto;
            background-position: center;
            background-repeat: no-repeat;
            background-color: #ffffff;
            display: flex;
            align-items: center;
            justify-content: space-between;
            margin-top: 20px;
            padding: 20px 30px;
            border-radius: 8px;
            box-shadow: 0 2px 5px rgba(0,0,0,0.1);
          }
          .app-title {
            font-size: 48px;
            font-weight: bold;
            color: #2c3e50;
            margin: 0;
            display: flex;
            align-items: center;
          }
          .app-title img {
            height: 120px;
            margin-right: 20px;
          }
          .header-logos {
            display: flex;
            gap: 20px;
          }
          .header-logos img {
            height: 120px;
          }
          .filter-panel {
            background-color: #ffffff;
            padding: 20px;
            border-radius: 8px;
            margin: 20px 30px;
            box-shadow: 0 2px 5px rgba(0,0,0,0.1);
          }
          .btn-warning {
            margin-top: 10px;
          }
          .dataTables_wrapper {
            background-color: #ffffff;
            padding: 10px;
            border-radius: 8px;
            box-shadow: 0 2px 5px rgba(0,0,0,0.1);
            margin: 0 30px 20px 30px;
          }
          .stats-panel {
            background-color: #ffffff;
            padding: 20px;
            border-radius: 8px;
            margin: 20px 30px;
            box-shadow: 0 2px 5px rgba(0,0,0,0.1);
          }
          .stat-box {
            text-align: center;
            padding: 15px;
            background-color: #ecf0f1;
            border-radius: 5px;
            margin: 5px;
          }
          .stat-number {
            font-size: 32px;
            font-weight: bold;
            color: #2c3e50;
          }
          .stat-label {
            font-size: 14px;
            color: #7f8c8d;
            margin-top: 5px;
          }
        "))
      ),
      
      # Header section
      header_ui,
      
      # Statistics Panel
      div(
        class = "stats-panel",
        h4("Dataset Overview"),
        fluidRow(
          column(3, div(class = "stat-box",
                       div(class = "stat-number", textOutput("total_papers")),
                       div(class = "stat-label", "Total Papers"))),
          column(3, div(class = "stat-box",
                       div(class = "stat-number", textOutput("with_mechanism")),
                       div(class = "stat-label", "With Mechanisms"))),
          column(3, div(class = "stat-box",
                       div(class = "stat-number", textOutput("without_mechanism")),
                       div(class = "stat-label", "Without Mechanisms"))),
          column(3, div(class = "stat-box",
                       div(class = "stat-number", textOutput("most_common_type")),
                       div(class = "stat-label", "Most Common Type")))
        )
      ),
      
      # Filter Panel
      div(
        class = "filter-panel",
        h4("Search & Filter Options"),
        
        fluidRow(
          column(6,
                 textInput("search_text", 
                          label = "Search in Title/Abstract:",
                          placeholder = "Enter keywords...")),
          column(6,
                 textInput("search_pmid",
                          label = "Search by PMID:",
                          placeholder = "Enter PMID..."))
        ),
        
        fluidRow(
          column(4,
                 selectInput("filter_mechanism_type",
                            label = "Mechanism Type:",
                            choices = c("All", sort(unique(df$`Autoregulatory Type`))),
                            selected = "All")),
          column(4,
                 selectInput("filter_has_mechanism",
                            label = "Has Mechanism:",
                            choices = c("All", "Yes", "No"),
                            selected = "All")),
          column(4,
                 sliderInput("filter_confidence",
                            label = "Min Stage 2 Confidence:",
                            min = 0, max = 100, value = 0, step = 5,
                            post = "%"))
        ),
        
        actionButton("reset_filters", "Reset All Filters", class = "btn btn-warning")
      ),
      
      # Download button
      div(
        style = "margin: 0 30px 10px 30px;",
        downloadButton("download_filtered", "Download Filtered Results", 
                      class = "btn btn-primary")
      ),
      
      # Data Table
      div(
        class = "dataTables_wrapper",
        DTOutput("main_table")
      )
    )
  ),
  
  # Tab: About
  tabPanel(
    title = "About",
    fluidPage(
      header_ui,
      div(
        class = "filter-panel",
        h3("About SOORENA"),
        p("SOORENA (Self-lOOp containing or autoREgulatory Nodes in biological network Analysis) 
          is a text-mining-based approach to extract and catalog information about self-loops 
          in molecular biology."),
        
        h4("Two-Stage Classification Pipeline"),
        tags$ul(
          tags$li(strong("Stage 1:"), " Binary classification to detect if a paper describes 
                  an autoregulatory mechanism (96.85% accuracy)"),
          tags$li(strong("Stage 2:"), " Multi-class classification to identify the specific 
                  mechanism type among 7 categories (98.01% accuracy)")
        ),
        
        h4("Mechanism Types"),
        tags$ul(
          tags$li(strong("Autophosphorylation:"), " Self-phosphorylation of proteins"),
          tags$li(strong("Autoregulation:"), " Self-regulation of gene expression"),
          tags$li(strong("Autocatalytic:"), " Self-catalytic reactions"),
          tags$li(strong("Autoinhibition:"), " Self-inhibitory mechanisms"),
          tags$li(strong("Autoubiquitination:"), " Self-ubiquitination processes"),
          tags$li(strong("Autolysis:"), " Self-degradation"),
          tags$li(strong("Autoinducer:"), " Self-inducing molecules")
        ),
        
        h4("Model Information"),
        p("Base model: PubMedBERT (microsoft/BiomedNLP-PubMedBERT-base-uncased-abstract-fulltext)"),
        p("Training data: ~1,300 manually curated papers from PubMed"),
        
        h4("Contact"),
        p("For questions or feedback, please contact the Jafari Lab at the University of Helsinki.")
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Summary statistics
  output$total_papers <- renderText({
    format(nrow(df), big.mark = ",")
  })
  
  output$with_mechanism <- renderText({
    format(sum(df$`Has Mechanism`, na.rm = TRUE), big.mark = ",")
  })
  
  output$without_mechanism <- renderText({
    format(sum(!df$`Has Mechanism`, na.rm = TRUE), big.mark = ",")
  })
  
  output$most_common_type <- renderText({
    mech_df <- df[df$`Has Mechanism` & df$`Autoregulatory Type` != "non-autoregulatory", ]
    if (nrow(mech_df) > 0) {
      top_type <- names(sort(table(mech_df$`Autoregulatory Type`), decreasing = TRUE))[1]
      # Shorten if too long
      if (nchar(top_type) > 15) {
        paste0(substr(top_type, 1, 12), "...")
      } else {
        top_type
      }
    } else {
      "N/A"
    }
  })
  
  # Filtered data reactive
  filtered_data <- reactive({
    data <- df
    
    # Text search
    if (!is.null(input$search_text) && input$search_text != "") {
      search_term <- tolower(input$search_text)
      data <- data[
        grepl(search_term, tolower(data$Title)) | 
          grepl(search_term, tolower(data$Abstract)),
      ]
    }
    
    # PMID search
    if (!is.null(input$search_pmid) && input$search_pmid != "") {
      data <- data[grepl(input$search_pmid, as.character(data$PMID)), ]
    }
    
    # Mechanism type filter
    if (!is.null(input$filter_mechanism_type) && input$filter_mechanism_type != "All") {
      data <- data[data$`Autoregulatory Type` == input$filter_mechanism_type, ]
    }
    
    # Has mechanism filter
    if (!is.null(input$filter_has_mechanism)) {
      if (input$filter_has_mechanism == "Yes") {
        data <- data[data$`Has Mechanism`, ]
      } else if (input$filter_has_mechanism == "No") {
        data <- data[!data$`Has Mechanism`, ]
      }
    }
    
    # Confidence filter
    if (!is.null(input$filter_confidence) && input$filter_confidence > 0) {
      # Extract numeric value from percentage string
      data$conf_numeric <- as.numeric(gsub("%", "", data$`Term Probability`))
      data <- data[!is.na(data$conf_numeric) & data$conf_numeric >= input$filter_confidence, ]
      data$conf_numeric <- NULL
    }
    
    data
  })
  
  # Main table
  output$main_table <- renderDT({
    display_df <- filtered_data() %>%
      select(PMID, Title, `Autoregulatory Type`, `Term Probability`, `Stage1 Confidence`)
    
    datatable(
      display_df,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        dom = 'Bfrtip',
        columnDefs = list(
          list(width = '100px', targets = 0),
          list(width = '500px', targets = 1),
          list(width = '180px', targets = 2),
          list(width = '120px', targets = 3),
          list(width = '120px', targets = 4)
        )
      ),
      rownames = FALSE,
      selection = 'single',
      filter = 'top'
    ) %>%
      formatStyle('Autoregulatory Type',
                 backgroundColor = styleEqual(
                   c('autophosphorylation', 'autoregulation', 'autocatalytic',
                     'autoinhibition', 'autoubiquitination', 'autolysis',
                     'autoinducer', 'non-autoregulatory'),
                   c('#e8f4f8', '#d4edda', '#fff3cd', '#f8d7da',
                     '#e7d4f8', '#fde2e4', '#cfe2ff', '#f8f9fa')
                 ))
  })
  
  # Reset filters
  observeEvent(input$reset_filters, {
    updateTextInput(session, "search_text", value = "")
    updateTextInput(session, "search_pmid", value = "")
    updateSelectInput(session, "filter_mechanism_type", selected = "All")
    updateSelectInput(session, "filter_has_mechanism", selected = "All")
    updateSliderInput(session, "filter_confidence", value = 0)
  })
  
  # Download handler
  output$download_filtered <- downloadHandler(
    filename = function() {
      paste0("soorena_filtered_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
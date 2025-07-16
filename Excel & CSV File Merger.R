library(shiny)
library(readxl)
library(writexl)
library(dplyr)
library(readr)
library(shinydashboard)
library(shinyjs)
library(DT)
library(tools)
library(data.table) # Added for more powerful data handling

# UI Component with Brite Neobrutalist theme
ui <- fluidPage(
  # Set the page title that appears in browser tab
  tags$head(
    # Set the page title
    tags$title("File Merger Pro"),
    
    tags$style(HTML('
      @import url("https://fonts.googleapis.com/css2?family=Space+Grotesk:wght@400;600;700&family=VT323&display=swap");
      
      body {
        background-color: #ffe4e1;
        color: #111111;
        font-family: "Space Grotesk", sans-serif;
      }
      
      /* Neobrutalist style container */
      .neo-container {
        border: 4px solid #000000;
        border-radius: 0px;
        box-shadow: 8px 8px 0px #000000;
        background-color: #ffffff;
        margin-bottom: 30px;
        position: relative;
        padding: 20px;
      }
      
      /* Header style */
      .neo-header {
        background-color: #ff6b6b;
        border: 4px solid #000000;
        padding: 15px;
        color: #000000;
        font-weight: bold;
        margin-bottom: 20px;
        box-shadow: 6px 6px 0px #000000;
        text-align: center;
      }
      
      /* Button styles */
      .neo-btn {
        background-color: #4cc9f0;
        border: 3px solid #000000;
        border-radius: 0px;
        color: #000000;
        font-weight: bold;
        padding: 12px 20px;
        text-transform: uppercase;
        cursor: pointer;
        box-shadow: 4px 4px 0px #000000;
        transition: transform 0.1s, box-shadow 0.1s;
        font-family: "Space Grotesk", sans-serif;
      }
      
      .neo-btn:hover {
        transform: translate(-2px, -2px);
        box-shadow: 6px 6px 0px #000000;
      }
      
      .neo-btn:active {
        transform: translate(2px, 2px);
        box-shadow: 2px 2px 0px #000000;
      }
      
      /* Success button */
      .neo-btn-success {
        background-color: #52ff00;
      }
      
      /* Primary button */
      .neo-btn-primary {
        background-color: #4cc9f0;
      }
      
      /* Form control styles */
      .neo-input {
        border: 3px solid #000000;
        border-radius: 0px;
        padding: 12px;
        background-color: #ffffff;
        box-shadow: 4px 4px 0px #000000;
      }
      
      .neo-input:focus {
        outline: none;
        border-color: #ff6b6b;
      }
      
      /* File input styling */
      .neo-file-input {
        border: 3px dashed #ff6b6b;
        background-color: #fff4f2;
        padding: 25px;
        text-align: center;
        cursor: pointer;
        color: #000000;
        font-weight: bold;
        position: relative;
        box-shadow: 4px 4px 0px #000000;
      }
      
      .neo-file-input:hover {
        background-color: #ffe4e1;
      }
      
      /* Badge styles */
      .neo-badge {
        display: inline-block;
        padding: 5px 10px;
        font-weight: bold;
        border: 2px solid #000000;
        box-shadow: 2px 2px 0px #000000;
      }
      
      .neo-badge-excel {
        background-color: #00ff80;
        color: #000000;
      }
      
      .neo-badge-csv {
        background-color: #ffdd00;
        color: #000000;
      }
      
      /* Tab styling */
      .nav-tabs {
        border-bottom: 3px solid #000000;
      }
      
      .nav-tabs > li > a {
        border: 3px solid #000000;
        border-bottom-color: transparent;
        background-color: #dedeff;
        margin-right: 5px;
        border-radius: 0;
        color: #000000;
        font-weight: bold;
        padding: 10px 15px;
      }
      
      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:focus,
      .nav-tabs > li.active > a:hover {
        border: 3px solid #000000;
        border-bottom-color: transparent;
        background-color: #ff6b6b;
        color: #000000;
      }
      
      /* Table styling */
      .dataTable {
        border: 3px solid #000000 !important;
      }
      
      .dataTable th {
        background-color: #4cc9f0 !important;
        color: #000000 !important;
        font-weight: bold !important;
        border: 2px solid #000000 !important;
      }
      
      .dataTable td {
        border: 1px solid #000000 !important;
      }
      
      /* Help section styling */
      .neo-help-section {
        border: 3px solid #000000;
        padding: 15px;
        background-color: #ffffd9;
        box-shadow: 4px 4px 0px #000000;
      }
      
      .neo-help-title {
        background-color: #fd9bff;
        padding: 5px 10px;
        margin-top: 10px;
        margin-bottom: 10px;
        border: 2px solid #000000;
        display: inline-block;
      }
      
      /* Fixed header */
      .app-header {
        position: fixed;
        top: 0;
        left: 0;
        right: 0;
        z-index: 1050;
        margin-bottom: 0 !important;
        background-color: #ffd166 !important;
        box-shadow: 0 4px 0px #000000;
        width: 100%;
        border-bottom: 4px solid #000000;
      }
      
      body {
        padding-top: 125px !important;
      }
      
      /* Fix main content positioning */
      .tab-content {
        position: relative;
        z-index: 1;
        margin-top: 20px;
      }
      
      /* Progress bar styling */
      .neo-progress {
        height: 20px;
        border: 2px solid #000000;
        background-color: #ffffff;
        position: relative;
        box-shadow: 2px 2px 0px #000000;
      }
      
      .neo-progress-bar {
        background-color: #ff6b6b;
        height: 100%;
        text-align: center;
        color: #000000;
        font-weight: bold;
        position: absolute;
        left: 0;
        top: 0;
        transition: width 0.3s ease;
      }
      
      /* Radio buttons */
      .neo-radio {
        margin-right: 15px;
      }
      
      .neo-radio input[type="radio"] {
        margin-right: 5px;
      }
      
      .neo-radio span {
        font-weight: bold;
      }
      
      /* Checkbox styling */
      input[type="checkbox"] {
        width: 20px;
        height: 20px;
        vertical-align: middle;
        margin-right: 8px;
      }
      
      /* Numeric input styling */
      .neo-numeric {
        width: 80px;
        text-align: center;
        font-weight: bold;
        border: 3px solid #000000;
        border-radius: 0;
        box-shadow: 3px 3px 0px #000000;
      }
      
      /* Success/Error message styling */
      .neo-alert {
        padding: 15px;
        margin: 20px 0;
        border: 3px solid #000000;
        box-shadow: 4px 4px 0px #000000;
        font-weight: bold;
        text-align: center;
        border-radius: 0;
      }
      
      .neo-alert-success {
        background-color: #52ff00;
        color: #000000;
      }
      
      .neo-alert-error {
        background-color: #ff6b6b;
        color: #ffffff;
      }
    '))
  ),
  
  useShinyjs(),
  
  # App title with logo/icon
  tags$div(
    class = "app-header",
    tags$div(style = "display: flex; align-items: center; justify-content: center; padding: 10px 0;",
             # App name
             tags$div(
               tags$h1("FILE MERGER PRO", style = "color: #000000; margin: 0; text-align: center; font-weight: 700; font-size: 32px; text-shadow: 2px 2px 0px #ffffff;"),
               tags$p("MERGE EXCEL AND CSV FILES LIKE A BOSS!", style = "text-align: center; color: #000000; margin: 5px 0 0 0; font-weight: bold;")
             )
    )
  ),
  
  # Main tab navigation - horizontal tabs on top
  tabsetPanel(
    id = "mainTabs",
    type = "tabs",
    selected = "upload",
    
    # First tab - Upload & Options combined
    tabPanel(
      title = "UPLOAD & CONFIGURE", 
      value = "upload",
      icon = icon("upload"),
      
      fluidRow(
        column(12,
               div(
                 class = "neo-container",
                 div(class = "neo-header", 
                     tags$h3("UPLOAD FILES & SET OPTIONS", style = "margin: 0;font-weight: 700; font-size: 32px; text-shadow: 2px 2px 0px #ffffff;"),
                     tags$span(id = "uploadStatus", class = "neo-badge", style = "margin-top: 5px;", "READY TO ROCK")),
                 
                 fluidRow(
                   # Left column - Upload Files
                   column(6,
                          div(class = "neo-file-input",
                              tags$label(
                                "DROP YOUR FILES HERE OR CLICK TO BROWSE",
                                tags$br(),
                                tags$i(class = "fa fa-cloud-upload", style = "font-size: 2.5em; margin: 15px 0;"),
                                tags$br(),
                                "EXCEL OR CSV ONLY!",
                                div(
                                  fileInput("files", NULL, multiple = TRUE, accept = c(".csv", ".xls", ".xlsx"))
                                )
                              )
                          ),
                          tags$p(class = "help-block", "SUPPORTED FORMATS: CSV, XLSX, XLS", style = "font-weight: bold; margin-top: 10px;"),
                          uiOutput("uploadedFilesInfo")
                   ),
                   
                   # Right column - Options
                   column(6,
                          div(style = "background-color: #ffffd9; border: 3px solid #000000; padding: 15px; box-shadow: 5px 5px 0px #000000;",
                              tags$h4("OUTPUT FORMAT", style = "color: #000000; margin-top: 0; border-bottom: 2px solid #000000; padding-bottom: 5px;"),
                              
                              div(style = "display: flex; margin-bottom: 15px;",
                                  div(class = "neo-radio",
                                      radioButtons("outputFormat", NULL,
                                                   choices = list("CSV" = "csv", "EXCEL" = "excel"),
                                                   selected = "csv",
                                                   inline = TRUE)
                                  )
                              ),
                              
                              tags$hr(style = "border-color: #000000; border-width: 2px;"),
                              
                              # Replace checkbox with numeric input for header row
                              div(style = "margin-bottom: 15px;",
                                  tags$label("HEADER ROW NUMBER:", style = "font-weight: bold; margin-right: 10px;"),
                                  div(class = "neo-numeric",
                                      numericInput("headerRowNum", NULL, value = 1, min = 1, max = 100, step = 1)
                                  )
                              ),
                              
                              checkboxInput("preview", "PREVIEW MERGED DATA BEFORE DOWNLOAD", value = TRUE),
                              checkboxInput("removeEmptyRows", "REMOVE EMPTY ROWS", value = TRUE),
                              checkboxInput("trimWhitespace", "TRIM WHITESPACE FROM TEXT", value = TRUE),
                              checkboxInput("addFilenameColumn", "ADD FILENAME AS A COLUMN", value = FALSE),
                              checkboxInput("cleanSpecialChars", "CONVERT SPECIAL CHARACTERS TO 0", value = TRUE),
                              
                              tags$hr(style = "border-color: #000000; border-width: 2px;"),
                              
                              div(style = "text-align: center; margin-top: 20px;",
                                  actionButton("mergeBtn", "MERGE FILES!", 
                                               class = "neo-btn neo-btn-success",
                                               icon = icon("layer-group"),
                                               style = "padding: 12px 20px; font-size: 16px;"),
                                  hidden(
                                    div(id = "downloadArea", style = "margin-top: 15px;",
                                        downloadButton("downloadBtn", "DOWNLOAD MERGED", 
                                                       class = "neo-btn neo-btn-primary",
                                                       style = "padding: 12px 20px; font-size: 16px;")
                                    )
                                  )
                              )
                          )
                   )
                 )
               )
        )
      ),
      
      # Status messages area
      fluidRow(
        column(12,
               hidden(
                 div(id = "statusMessages",
                     uiOutput("statusMessage")
                 )
               )
        )
      ),
      
      # Data preview section
      fluidRow(
        column(12,
               hidden(
                 div(id = "previewArea",
                     div(
                       class = "neo-container",
                       div(class = "neo-header", 
                           tags$h3("DATA PREVIEW", style = "margin: 0;"),
                           tags$span(id = "dataStats", class = "neo-badge", style = "margin-top: 5px;")),
                       div(style = "padding: 10px;", DTOutput("dataPreview"))
                     )
                 )
               )
        )
      )
    ),
    
    # Second tab - Processing Status
    tabPanel(
      title = "PROCESSING LOG", 
      value = "processing",
      icon = icon("terminal"),
      
      fluidRow(
        column(12,
               div(
                 class = "neo-container",
                 div(class = "neo-header", tags$h3("PROCESSING STATUS", style = "margin: 0;")),
                 
                 div(id = "processingMsg", style = "padding: 20px;",
                     tags$div(class = "text-center",
                              tags$p(id = "processingTitle", "READY TO PROCESS FILES!", 
                                     style = "font-size: 18px; margin-bottom: 15px; font-weight: bold;"), 
                              tags$div(class = "neo-progress",
                                       tags$div(id = "progressBar", class = "neo-progress-bar", style = "width: 0%", "0%")
                              ),
                              tags$p(id = "progressText", "Upload files and click 'MERGE FILES!' to start", style = "margin-top: 5px; font-weight: bold;")
                     )
                 ),
                 
                 div(style = "background-color: #000000; padding: 15px; color: #52ff00; font-family: 'VT323', monospace; font-size: 18px; margin: 20px;",
                     verbatimTextOutput("processingStatus")
                 )
               )
        )
      ),
      
      # File details table
      fluidRow(
        column(12,
               div(
                 class = "neo-container",
                 div(class = "neo-header", tags$h3("FILE DETAILS", style = "margin: 0;")),
                 div(style = "padding: 10px;", DTOutput("fileDetailsTable"))
               )
        )
      )
    ),
    
    # Third tab - Help
    tabPanel(
      title = "HELP", 
      value = "help",
      icon = icon("question-circle"),
      
      fluidRow(
        column(12,
               div(
                 class = "neo-container",
                 div(class = "neo-header", tags$h3("HOW TO USE THIS APP", style = "margin: 0;")),
                 
                 div(class = "neo-help-section",
                     tags$div(
                       tags$h4(class = "neo-help-title", "STEP 1: UPLOAD FILES"),
                       tags$p("Click the 'Browse' button or drag and drop your CSV or Excel files into the upload area."),
                       tags$p("You can select multiple files at once to be merged."),
                       
                       tags$h4(class = "neo-help-title", "STEP 2: CONFIGURE OPTIONS"),
                       tags$p("Choose your desired output format (CSV or Excel)."),
                       tags$p("Set which row number contains your headers using the numeric input."),
                       tags$p("Choose if you want to preview data before downloading."),
                       tags$p("Enable 'Remove empty rows' to clean up your data."),
                       tags$p("Enable 'Trim whitespace' to remove leading/trailing spaces from text fields."),
                       tags$p("Enable 'Add filename as a column' to include the source filename for each row."),
                       tags$p("Enable 'Clean special characters' to automatically convert special characters to '0' (recommended for problematic files)."),
                       
                       tags$h4(class = "neo-help-title", "STEP 3: PROCESS FILES"),
                       tags$p("Click the 'MERGE FILES!' button to start the processing."),
                       tags$p("You can monitor the progress in the 'PROCESSING LOG' tab."),
                       
                       tags$h4(class = "neo-help-title", "STEP 4: DOWNLOAD RESULT"),
                       tags$p("Once processing is complete, click the 'DOWNLOAD MERGED' button to save your merged file."),
                       
                       tags$h4(class = "neo-help-title", "TIPS & TRICKS"),
                       tags$ul(
                         tags$li("For best results, ensure all files have similar column structures."),
                         tags$li("The app will attempt to handle inconsistent data types by converting to text."),
                         tags$li("Very large files may take longer to process. Be patient!"),
                         tags$li("Setting the correct header row number is essential! Default is row 1."),
                         tags$li("Using 'Add filename as a column' helps track the origin of each row in the merged data."),
                         tags$li("If you encounter errors with special characters, enable 'Clean special characters' option.")
                       )
                     )
                 )
               )
        )
      )
    ),
    
    # About tab
    tabPanel(
      title = "ABOUT", 
      value = "about",
      icon = icon("info-circle"),
      
      fluidRow(
        column(12,
               div(
                 class = "neo-container",
                 div(class = "neo-header", tags$h3("ABOUT FILE MERGER PRO", style = "margin: 0;")),
                 
                 div(style = "text-align: center; padding: 20px;",
                     tags$img(src = "https://icon-icons.com/icons2/37/PNG/512/merge_files_55864.png", height = "100px", style = "margin-bottom: 20px;"),
                     tags$h3("FILE MERGER PRO", style = "color: #000000; font-weight: bold;"),
                     tags$div(style = "display: inline-block; background-color: #ff6b6b; padding: 5px 15px; border: 2px solid #000000; box-shadow: 3px 3px 0px #000000;",
                              tags$p("VERSION 2.3.0", style = "color: #000000; margin: 0; font-weight: bold;")),
                     tags$p("A powerful tool for merging Excel and CSV files with Neobrutalist style!", style = "margin-top: 15px;"),
                     tags$hr(style = "border-color: #000000; width: 50%; border-width: 2px;"),
                     tags$p("© 2025 Keyur Makwana", style = "color: #000000;")
                 )
               )
        )
      )
    )
  )
)

# Server Component with enhanced updates to handle header row number
server <- function(input, output, session) {
  
  # Initialize reactive values
  values <- reactiveValues(
    merged_data = NULL,
    processing_log = NULL,
    file_details = NULL,
    progress_value = 0,
    uploaded_files = 0,
    processing_complete = FALSE,
    processing_success = FALSE
  )
  
  # Display uploaded files info
  output$uploadedFilesInfo <- renderUI({
    req(input$files)
    file_count <- nrow(input$files)
    
    values$uploaded_files <- file_count
    
    if (file_count > 0) {
      # Create a list of uploaded files
      display_limit <- 10
      files_to_display <- head(input$files$name, display_limit)
      remaining_count <- file_count - display_limit
      
      file_items <- lapply(seq_along(files_to_display), function(i) {
        file_name <- files_to_display[i]
        file_ext <- tolower(tools::file_ext(file_name))
        
        # Create a badge based on file type
        if (file_ext == "csv") {
          badge_class <- "neo-badge-csv"
          badge_text <- "CSV"
        } else if (file_ext %in% c("xls", "xlsx")) {
          badge_class <- "neo-badge-excel"
          badge_text <- toupper(file_ext)
        } else {
          badge_class <- ""
          badge_text <- toupper(file_ext)
        }
        
        tags$li(
          style = "margin-bottom: 10px; list-style-type: none;",
          tags$span(class = paste("neo-badge", badge_class), badge_text),
          tags$span(style = "margin-left: 10px; font-weight: bold;", file_name)
        )
      })
      
      # Add note if files are truncated
      if (remaining_count > 0) {
        file_items[[length(file_items) + 1]] <- tags$li(
          style = "margin-top: 10px; list-style-type: none; font-style: italic; font-weight: bold;",
          paste0("... and ", remaining_count, " more files")
        )
      }
      
      # Update upload status
      shinyjs::html("uploadStatus", paste(file_count, "FILES READY"))
      
      tags$div(
        style = "margin-top: 15px; background-color: #f0f8ff; padding: 15px; border: 3px solid #000000; box-shadow: 5px 5px 0 #000000;",
        tags$h5(style = "margin-top: 0; color: #000000; font-weight: bold; border-bottom: 2px solid #000000; padding-bottom: 5px;", 
                tags$i(class = "fa fa-file-text", style = "margin-right: 5px;"), 
                "UPLOADED FILES"),
        tags$ul(
          style = "padding-left: 5px; margin-bottom: 0;",
          file_items
        )
      )
    } else {
      # Update upload status
      shinyjs::html("uploadStatus", "READY TO ROCK")
      
      NULL
    }
  })
  
  # Add File Details table output
  output$fileDetailsTable <- renderDT({
    req(values$file_details)
    datatable(values$file_details, 
              options = list(
                scrollX = TRUE, 
                pageLength = 10,
                dom = 'ftp',
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#4cc9f0', 'color': '#000000', 'font-weight': 'bold', 'border': '2px solid #000000'});",
                  "}"
                )
              ),
              class = 'cell-border stripe hover')
  })
  
  # Function to clean special characters
  clean_special_chars <- function(df) {
    if (input$cleanSpecialChars) {
      char_cols <- sapply(df, is.character)
      
      # Convert special characters to 0 in character columns
      df[char_cols] <- lapply(df[char_cols], function(x) {
        # Replace any non-alphanumeric characters (except spaces and dots) with "0"
        gsub("[^[:alnum:] \\.]", "0", x, perl = TRUE)
      })
    }
    
    return(df)
  }
  
  # Function to read files with header row number option
  read_file <- function(file_path, file_name) {
    file_ext <- tolower(file_ext(file_path))
    
    # Get the header row number from input
    header_row_num <- input$headerRowNum
    
    if (file_ext == "csv") {
      tryCatch({
        # Use fread from data.table with specific header row
        if (header_row_num == 1) {
          # Standard reading if header is in first row
          data <- as.data.frame(fread(file_path, header = TRUE))
        } else {
          # Skip rows and set header
          data <- as.data.frame(fread(file_path, skip = header_row_num - 1, header = TRUE))
        }
        
        # Clean special characters if option is enabled
        data <- clean_special_chars(data)
        
        # Add filename column if option is selected
        if (input$addFilenameColumn) {
          data$Source_File <- file_name
        }
        
        return(list(success = TRUE, data = data, message = paste("Successfully read CSV file:", basename(file_path), "with header at row", header_row_num)))
      }, error = function(e) {
        # Fallback to readr if fread fails
        tryCatch({
          if (header_row_num == 1) {
            data <- read_csv(file_path, col_names = TRUE, show_col_types = FALSE)
          } else {
            data <- read_csv(file_path, skip = header_row_num - 1, col_names = TRUE, show_col_types = FALSE)
          }
          
          # Clean special characters if option is enabled
          data <- clean_special_chars(data)
          
          # Add filename column if option is selected
          if (input$addFilenameColumn) {
            data$Source_File <- file_name
          }
          
          return(list(success = TRUE, data = data, message = paste("Successfully read CSV file:", basename(file_path), "with header at row", header_row_num)))
        }, error = function(e2) {
          return(list(success = FALSE, data = NULL, message = paste("Error reading CSV file:", basename(file_path), "-", e2$message)))
        })
      })
    } else if (file_ext %in% c("xls", "xlsx")) {
      tryCatch({
        # For Excel, we need to adjust how we read based on header row
        if (header_row_num == 1) {
          data <- read_excel(file_path, col_names = TRUE)
        } else {
          # For Excel, we read without headers first
          data_raw <- read_excel(file_path, col_names = FALSE)
          
          # Extract headers from the specified row
          headers <- unlist(data_raw[header_row_num, ])
          
          # Use data from rows after the header
          data <- data_raw[(header_row_num + 1):nrow(data_raw), ]
          
          # Set column names
          colnames(data) <- headers
        }
        
        # Standardize column names - replace spaces and special chars with underscores
        names(data) <- make.names(names(data), unique = TRUE)
        
        # Clean special characters if option is enabled
        data <- clean_special_chars(data)
        
        # Add filename column if option is selected
        if (input$addFilenameColumn) {
          data$Source_File <- file_name
        }
        
        return(list(success = TRUE, data = data, message = paste("Successfully read Excel file:", basename(file_path), "with header at row", header_row_num)))
      }, error = function(e) {
        return(list(success = FALSE, data = NULL, message = paste("Error reading Excel file:", basename(file_path), "-", e$message)))
      })
    } else {
      return(list(success = FALSE, data = NULL, message = paste("Unsupported file format:", file_ext)))
    }
  }
  
  # Update progress bar with improved functionality
  updateProgress <- function(value) {
    values$progress_value <- value
    
    # Update progress bar width and text
    shinyjs::runjs(paste0('
      $("#progressBar").css("width", "', value, '%");
      $("#progressBar").text("', value, '%");
    '))
    
    # Update progress text
    if (value == 0) {
      shinyjs::html("progressText", "Ready to start processing...")
    } else if (value == 100) {
      if (values$processing_success) {
        shinyjs::html("progressText", "✅ Processing completed successfully!")
      } else {
        shinyjs::html("progressText", "❌ Processing completed with errors!")
      }
    } else {
      shinyjs::html("progressText", paste0("Processing files... ", value, "% complete"))
    }
  }
  
  # Main merge function with enhanced error handling
  observeEvent(input$mergeBtn, {
    req(input$files)
    
    # Reset values
    values$merged_data <- NULL
    values$processing_log <- ""
    values$file_details <- NULL
    values$processing_complete <- FALSE
    values$processing_success <- FALSE
    
    # Show processing area and hide download button
    shinyjs::show("statusMessages")
    shinyjs::hide("downloadArea")
    shinyjs::hide("previewArea")
    
    # Update processing title
    shinyjs::html("processingTitle", "PROCESSING FILES...")
    
    # Initialize progress
    updateProgress(0)
    
    # Start processing
    tryCatch({
      file_count <- nrow(input$files)
      all_data <- list()
      file_details <- data.frame(
        File_Name = character(),
        File_Type = character(),
        Rows = numeric(),
        Columns = numeric(),
        Status = character(),
        stringsAsFactors = FALSE
      )
      
      processing_log <- "=== FILE MERGER PRO - PROCESSING LOG ===\n"
      processing_log <- paste0(processing_log, "Started at: ", Sys.time(), "\n")
      processing_log <- paste0(processing_log, "Files to process: ", file_count, "\n")
      processing_log <- paste0(processing_log, "Header row number: ", input$headerRowNum, "\n")
      processing_log <- paste0(processing_log, "Output format: ", toupper(input$outputFormat), "\n")
      processing_log <- paste0(processing_log, "Remove empty rows: ", input$removeEmptyRows, "\n")
      processing_log <- paste0(processing_log, "Trim whitespace: ", input$trimWhitespace, "\n")
      processing_log <- paste0(processing_log, "Add filename column: ", input$addFilenameColumn, "\n")
      processing_log <- paste0(processing_log, "Clean special characters: ", input$cleanSpecialChars, "\n")
      processing_log <- paste0(processing_log, "\n--- PROCESSING FILES ---\n")
      
      values$processing_log <- processing_log
      
      # Process each file
      for (i in 1:file_count) {
        file_info <- input$files[i, ]
        
        # Update progress
        progress_percent <- round((i - 1) / file_count * 100)
        updateProgress(progress_percent)
        
        processing_log <- paste0(processing_log, "\n[", i, "/", file_count, "] Processing: ", file_info$name, "\n")
        
        # Read file
        result <- read_file(file_info$datapath, file_info$name)
        
        if (result$success) {
          data <- result$data
          
          # Remove empty rows if option is selected
          if (input$removeEmptyRows) {
            original_rows <- nrow(data)
            data <- data[rowSums(is.na(data)) != ncol(data), ]
            removed_rows <- original_rows - nrow(data)
            if (removed_rows > 0) {
              processing_log <- paste0(processing_log, "   Removed ", removed_rows, " empty rows\n")
            }
          }
          
          # Trim whitespace if option is selected
          if (input$trimWhitespace) {
            char_cols <- sapply(data, is.character)
            data[char_cols] <- lapply(data[char_cols], trimws)
            processing_log <- paste0(processing_log, "   Trimmed whitespace from text columns\n")
          }
          
          # Convert all columns to character to avoid binding issues
          data <- data %>% mutate_all(as.character)
          
          all_data[[i]] <- data
          
          # Add to file details
          file_details <- rbind(file_details, data.frame(
            File_Name = file_info$name,
            File_Type = toupper(tools::file_ext(file_info$name)),
            Rows = nrow(data),
            Columns = ncol(data),
            Status = "✅ SUCCESS",
            stringsAsFactors = FALSE
          ))
          
          processing_log <- paste0(processing_log, "   ✅ SUCCESS - ", nrow(data), " rows, ", ncol(data), " columns\n")
          
        } else {
          # Add error to file details
          file_details <- rbind(file_details, data.frame(
            File_Name = file_info$name,
            File_Type = toupper(tools::file_ext(file_info$name)),
            Rows = 0,
            Columns = 0,
            Status = "❌ ERROR",
            stringsAsFactors = FALSE
          ))
          
          processing_log <- paste0(processing_log, "   ❌ ERROR - ", result$message, "\n")
        }
        
        values$processing_log <- processing_log
      }
      
      # Update progress to 50% for merging phase
      updateProgress(50)
      
      # Merge all successfully read data
      successful_data <- all_data[!sapply(all_data, is.null)]
      
      if (length(successful_data) > 0) {
        processing_log <- paste0(processing_log, "\n--- MERGING DATA ---\n")
        processing_log <- paste0(processing_log, "Merging ", length(successful_data), " successful files...\n")
        
        # Use bind_rows for better handling of different column structures
        merged_data <- bind_rows(successful_data, .id = NULL)
        
        processing_log <- paste0(processing_log, "✅ Merge completed - ", nrow(merged_data), " total rows, ", ncol(merged_data), " columns\n")
        
        values$merged_data <- merged_data
        values$processing_success <- TRUE
        
        # Update progress to 100%
        updateProgress(100)
        
        processing_log <- paste0(processing_log, "\n=== PROCESSING COMPLETE ===\n")
        processing_log <- paste0(processing_log, "Completed at: ", Sys.time(), "\n")
        processing_log <- paste0(processing_log, "Total rows in merged data: ", nrow(merged_data), "\n")
        processing_log <- paste0(processing_log, "Total columns in merged data: ", ncol(merged_data), "\n")
        
        values$processing_log <- processing_log
        values$file_details <- file_details
        values$processing_complete <- TRUE
        
        # Show success message
        output$statusMessage <- renderUI({
          tags$div(
            class = "neo-alert neo-alert-success",
            tags$strong("🎉 SUCCESS!"),
            tags$br(),
            paste0("Successfully merged ", length(successful_data), " files with ", 
                   nrow(merged_data), " total rows and ", ncol(merged_data), " columns!")
          )
        })
        
        # Show download button
        shinyjs::show("downloadArea")
        
        # Show preview if enabled
        if (input$preview) {
          shinyjs::show("previewArea")
          
          # Update data stats
          shinyjs::html("dataStats", paste0(nrow(merged_data), " ROWS • ", ncol(merged_data), " COLUMNS"))
        }
        
      } else {
        # No successful files
        values$processing_success <- FALSE
        updateProgress(100)
        
        processing_log <- paste0(processing_log, "\n❌ NO FILES WERE SUCCESSFULLY PROCESSED\n")
        processing_log <- paste0(processing_log, "Completed at: ", Sys.time(), "\n")
        
        values$processing_log <- processing_log
        values$file_details <- file_details
        values$processing_complete <- TRUE
        
        # Show error message
        output$statusMessage <- renderUI({
          tags$div(
            class = "neo-alert neo-alert-error",
            tags$strong("❌ ERROR!"),
            tags$br(),
            "No files were successfully processed. Please check the processing log for details."
          )
        })
      }
      
    }, error = function(e) {
      # Handle unexpected errors
      values$processing_success <- FALSE
      updateProgress(100)
      
      error_log <- paste0(values$processing_log, "\n❌ UNEXPECTED ERROR: ", e$message, "\n")
      error_log <- paste0(error_log, "Completed at: ", Sys.time(), "\n")
      
      values$processing_log <- error_log
      values$processing_complete <- TRUE
      
      # Show error message
      output$statusMessage <- renderUI({
        tags$div(
          class = "neo-alert neo-alert-error",
          tags$strong("❌ UNEXPECTED ERROR!"),
          tags$br(),
          e$message
        )
      })
    })
  })
  
  # Data preview output
  output$dataPreview <- renderDT({
    req(values$merged_data)
    
    # Show first 1000 rows for performance
    preview_data <- head(values$merged_data, 1000)
    
    datatable(preview_data, 
              options = list(
                scrollX = TRUE, 
                pageLength = 10,
                dom = 'frtip',
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#4cc9f0', 'color': '#000000', 'font-weight': 'bold', 'border': '2px solid #000000'});",
                  "}"
                )
              ),
              class = 'cell-border stripe hover') %>%
      formatStyle(columns = 1:ncol(preview_data), 
                  border = '1px solid #000000')
  })
  
  # Processing status output
  output$processingStatus <- renderText({
    if (is.null(values$processing_log)) {
      "Ready to process files...\nUpload files and click 'MERGE FILES!' to start."
    } else {
      values$processing_log
    }
  })
  
  # Download handler
  output$downloadBtn <- downloadHandler(
    filename = function() {
      timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      if (input$outputFormat == "csv") {
        paste0("merged_data_", timestamp, ".csv")
      } else {
        paste0("merged_data_", timestamp, ".xlsx")
      }
    },
    content = function(file) {
      req(values$merged_data)
      
      if (input$outputFormat == "csv") {
        write_csv(values$merged_data, file)
      } else {
        write_xlsx(values$merged_data, file)
      }
    }
  )
  
  # Auto-switch to processing tab when merge starts
  observeEvent(input$mergeBtn, {
    updateTabsetPanel(session, "mainTabs", selected = "processing")
  })
  
  # Session cleanup
  session$onSessionEnded(function() {
    # Clean up any temporary files or resources
    # Note: We don't need to explicitly set reactive values to NULL
    # as they will be garbage collected when the session ends
  })
}

# Run the application
shinyApp(ui = ui, server = server)
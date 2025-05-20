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
    # Add favicon/icon for the browser tab
    tags$link(rel = "firstdraft", href = "favicon.ico"),
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
    '))
  ),
  
  useShinyjs(),
  
  # App title with logo/icon
  tags$div(
    class = "app-header",
    tags$div(style = "display: flex; align-items: center; justify-content: center; padding: 10px 0;",
             # App icon/logo
             tags$img(src = "https://icon-icons.com/icons2/37/PNG/512/merge_files_55864.png", height = "50px", style = "margin-right: 15px;"),
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
                     tags$h3("UPLOAD FILES & SET OPTIONS", style = "margin: 0;"),
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
                 
                 hidden(
                   div(id = "processingMsg", style = "padding: 20px;",
                       tags$div(class = "text-center",
                                tags$p("PROCESSING FILES... HANG TIGHT!", 
                                       style = "font-size: 18px; margin-bottom: 15px; font-weight: bold;"), 
                                tags$div(class = "neo-progress",
                                         tags$div(id = "progressBar", class = "neo-progress-bar", style = "width: 0%")
                                ),
                                tags$p(id = "progressText", "0%", style = "margin-top: 5px; font-weight: bold;")
                       )
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
                         tags$li("Using 'Add filename as a column' helps track the origin of each row in the merged data.")
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
                              tags$p("VERSION 2.0.0", style = "color: #000000; margin: 0; font-weight: bold;")),
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
    uploaded_files = 0
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
          
          # If we had to skip rows, we need to handle the skipped data separately if needed
          # Here we simply continue with the data starting from the header row
        }
        
        # Add filename column if option is selected
        if (input$addFilenameColumn) {
          data$Source_File <- file_name
        }
        
        return(list(success = TRUE, data = data, message = paste("Successfully read CSV file:", basename(file_path), "with header at row", header_row_num)))
      }, error = function(e) {
        # Fallback to readr if fread fails
        tryCatch({
          if (header_row_num == 1) {
            data <- read_csv(file_path, col_names = TRUE)
          } else {
            data <- read_csv(file_path, skip = header_row_num - 1, col_names = TRUE)
          }
          
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
  
  # Update progress bar
  observe({
    invalidateLater(100)
    if(values$progress_value > 0 && values$progress_value < 100) {
      shinyjs::html("progressText", paste0(values$progress_value, "%"))
      shinyjs::runjs(paste0('$("#progressBar").css("width", "', values$progress_value, '%");'))
    }
  })
  
  # Process files when merge button is clicked
  observeEvent(input$mergeBtn, {
    # Validate file input
    req(input$files)
    if (length(input$files$datapath) < 1) {
      values$processing_log <- "ERROR: NO FILES UPLOADED! WHAT ARE YOU DOING?!"
      return()
    }
    
    # Validate header row number
    if (is.na(input$headerRowNum) || input$headerRowNum < 1) {
      values$processing_log <- "ERROR: HEADER ROW NUMBER MUST BE AT LEAST 1!"
      return()
    }
    
    # Switch to processing tab automatically
    updateTabsetPanel(session, "mainTabs", selected = "processing")
    
    # Reset progress
    values$progress_value <- 0
    
    # Show processing message
    shinyjs::show("processingMsg")
    shinyjs::hide("downloadArea")
    shinyjs::hide("previewArea")
    
    values$processing_log <- "🚀 STARTING FILE PROCESSING... LET'S GO!\n"
    
    # Show if filename column is being added
    if (input$addFilenameColumn) {
      values$processing_log <- paste0(values$processing_log, "ℹ️ WILL ADD 'Source_File' COLUMN WITH FILENAMES\n")
    }
    
    values$processing_log <- paste0(values$processing_log, "ℹ️ USING ROW ", input$headerRowNum, " AS HEADER ROW\n")
    
    # Process all files
    all_data_frames <- list()
    file_details <- data.frame(
      File = character(),
      Type = character(),
      Rows = integer(),
      Columns = integer(),
      Status = character(),
      stringsAsFactors = FALSE
    )
    
    total_files <- nrow(input$files)
    
    for (i in 1:nrow(input$files)) {
      file_path <- input$files$datapath[i]
      file_name <- input$files$name[i]
      file_type <- tools::file_ext(file_name)
      
      # Update progress
      values$progress_value <- round((i / total_files) * 100)
      
      # Read file
      values$processing_log <- paste0(values$processing_log, "📄 PROCESSING FILE ", i, "/", total_files, ": ", file_name, "\n")
      result <- read_file(file_path, file_name)
      
      if (result$success) {
        all_data_frames[[i]] <- result$data
        file_details <- rbind(file_details, data.frame(
          File = file_name,
          Type = toupper(file_type),
          Rows = nrow(result$data),
          Columns = ncol(result$data),
          Status = "SUCCESS",
          stringsAsFactors = FALSE
        ))
        values$processing_log <- paste0(values$processing_log, "✅ ", result$message, "\n")
      } else {
        file_details <- rbind(file_details, data.frame(
          File = file_name,
          Type = toupper(file_type),
          Rows = NA,
          Columns = NA,
          Status = "FAILED",
          stringsAsFactors = FALSE
        ))
        values$processing_log <- paste0(values$processing_log, "❌ ", result$message, "\n")
      }
    }
    
    # Store file details
    values$file_details <- file_details
    
    # Check if we have any successful files
    if (length(all_data_frames) == 0) {
      values$processing_log <- paste0(values$processing_log, "❌ NO FILES WERE SUCCESSFULLY PROCESSED. NOTHING TO MERGE.\n")
      values$progress_value <- 100
      return()
    }
    
    values$processing_log <- paste0(values$processing_log, "\n🔄 MERGING ", length(all_data_frames), " FILES...\n")
    
    # Merge all data frames
    tryCatch({
      merged_data <- bind_rows(all_data_frames)
      
      # Data cleaning options
      if (input$removeEmptyRows) {
        # Remove rows where all values are NA
        values$processing_log <- paste0(values$processing_log, "🧹 REMOVING EMPTY ROWS...\n")
        original_rows <- nrow(merged_data)
        merged_data <- merged_data[rowSums(is.na(merged_data)) != ncol(merged_data), ]
        rows_removed <- original_rows - nrow(merged_data)
        values$processing_log <- paste0(values$processing_log, "ℹ️ REMOVED ", rows_removed, " EMPTY ROWS\n")
      }
      
      if (input$trimWhitespace) {
        # Trim whitespace from character columns
        values$processing_log <- paste0(values$processing_log, "✂️ TRIMMING WHITESPACE FROM TEXT COLUMNS...\n")
        char_cols <- sapply(merged_data, is.character)
        merged_data[, char_cols] <- lapply(merged_data[, char_cols, drop = FALSE], trimws)
      }
      
      # Save the merged data
      values$merged_data <- merged_data
      
      # Update processing log
      values$processing_log <- paste0(values$processing_log, "✅ MERGE COMPLETE! ", nrow(merged_data), " ROWS AND ", ncol(merged_data), " COLUMNS IN FINAL DATA\n")
      values$processing_log <- paste0(values$processing_log, "🎉 READY FOR DOWNLOAD!\n")
      
      # Show download button
      shinyjs::show("downloadArea")
      
      # Show preview if selected
      if (input$preview) {
        shinyjs::show("previewArea")
        # Update data stats
        shinyjs::html("dataStats", paste0(nrow(merged_data), " ROWS × ", ncol(merged_data), " COLUMNS"))
      }
      
      # Update progress bar to 100%
      values$progress_value <- 100
      
    }, error = function(e) {
      values$processing_log <- paste0(values$processing_log, "❌ ERROR DURING MERGE: ", e$message, "\n")
      values$progress_value <- 100
    })
  })
  
  # Download handler
  output$downloadBtn <- downloadHandler(
    filename = function() {
      # Create a timestamp-based filename
      timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      
      if (input$outputFormat == "csv") {
        paste0("merged_data_", timestamp, ".csv")
      } else {
        paste0("merged_data_", timestamp, ".xlsx")
      }
    },
    content = function(file) {
      # Get the merged data
      data <- values$merged_data
      
      if (is.null(data)) {
        return(NULL)
      }
      
      # Export based on format
      if (input$outputFormat == "csv") {
        write_csv(data, file)
      } else {
        write_xlsx(data, file)
      }
    }
  )
  
  # Data preview output
  output$dataPreview <- renderDT({
    req(values$merged_data)
    
    # Limit preview to first 1000 rows for performance
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
      formatStyle(names(preview_data), backgroundColor = 'white', color = 'black')
  })
  
  # Processing log output
  output$processingStatus <- renderText({
    req(values$processing_log)
    return(values$processing_log)
  })
  
  # React to tab change
  observeEvent(input$mainTabs, {
    # If switching to processing tab and files are uploaded
    if (input$mainTabs == "processing" && values$uploaded_files > 0) {
      # Show the processing log
      shinyjs::show("processingStatus")
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
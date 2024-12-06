library(shiny)
library(httr)
library(readxl)

# Ensure the 'www' directory exists and is correct
rmarkdowns_path <- normalizePath(paste0(getwd(),"/www"), mustWork = TRUE)
# 
# # Add resource path for the RMarkdown HTML files
addResourcePath("www", rmarkdowns_path)

# Define UI
ui <- fluidPage(
  titlePanel("Ona Data Downloader"),
  sidebarLayout(
    sidebarPanel(
      textInput("username", "Ona Username", value = "ctradmin"),
      passwordInput("password", "Ona Password", value = "31S62L"),
      textInput("step1_form_id", "STEP 1 Form ID", value = "713225"),
      textInput("step3_form_id", "STEP 3 Form ID", value = "713235"),
      actionButton("download_data", "Download Data")
    ),
    mainPanel(
      verbatimTextOutput("status"),
      downloadButton("download_data1", "Download STEP 1 CSV"),
      downloadButton("download_data3", "Download STEP 3 CSV"),
      uiOutput('rmd_output')
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Use renderUI to dynamically render the Rmd output
  output$rmd_output <- renderUI({
    # Render the Rmd file and return its content
    rmarkdown::render(paste0(getwd(),'/STEPS_Report.Rmd'), output_format = "html_document", output_file = "STEPS_Report.html")

    p(HTML("A summary of the STEPS survey monitoring report is accessible through this A <strong><a href='/www/STEPS_Report.html' target='_blank'>link</a></strong>."))
    
  })
 
  # Function to download and read Excel data from Ona
  download_excel_data <- function(form_id, username, password, include_labels = FALSE) {
    base_url <- "https://api.ona.io/api/v1/data/"
    options <- ifelse(include_labels, "?include_labels=true&remove_group_name=true", "?remove_group_name=true")
    url <- paste0(base_url, form_id, ".xlsx", options)
    
    response <- GET(url, authenticate(username, password, type = "basic"))
    if (status_code(response) == 200) {
      temp_file <- tempfile(fileext = ".xlsx")
      writeBin(response$content, temp_file)
      data <- read_excel(temp_file)
      file.remove(temp_file)
      return(data)
    } else {
      stop("Failed to download data: HTTP status ", status_code(response))
    }
  }
  
  # Reactive values to store data
  data <- reactiveValues(data1 = NULL, data3 = NULL)
  
  # Trigger data download with progress tracking
  observeEvent(input$download_data, {
    tryCatch({
      # Validate inputs
      if (input$username == "" || input$password == "" || input$step1_form_id == "" || input$step3_form_id == "") {
        stop("Please fill in all fields.")
      }
      
      # Show progress bar
      withProgress(message = "Downloading data", value = 0, {
        
        # STEP 1: Download and process STEP 1 data
        incProgress(0.3, detail = "Downloading STEP 1 data...")
        data$data1 <- download_excel_data(input$step1_form_id, input$username, input$password)
        colnames(data$data1) <- tolower(colnames(data$data1))
        names(data$data1) <- gsub("^_", "", names(data$data1))
        incProgress(0.4, detail = "STEP 1 data downloaded and processed.")
        
        # STEP 2: Download and process STEP 3 data
        incProgress(0.3, detail = "Downloading STEP 3 data...")
        data$data3 <- download_excel_data(input$step3_form_id, input$username, input$password)
        colnames(data$data3) <- tolower(colnames(data$data3))
        names(data$data3) <- gsub("^_", "", names(data$data3))
        incProgress(0.8, detail = "STEP 3 data downloaded and processed.")
        
        # Save CSV files
        write.csv(data$data1, "data1.csv", row.names = FALSE)
        write.csv(data$data3, "data3.csv", row.names = FALSE)
        incProgress(1, detail = "Saving files.")
        
      })
      
      # Update status message
      output$status <- renderText("Data downloaded and processed successfully.")
    }, error = function(e) {
      output$status <- renderText(paste("Error:", e$message))
    })
  })
  
  # Provide download handlers
  output$download_data1 <- downloadHandler(
    filename = function() { "data1.csv" },
    content = function(file) {
      file.copy("data1.csv", file)
    }
  )
  
  output$download_data3 <- downloadHandler(
    filename = function() { "data3.csv" },
    content = function(file) {
      file.copy("data3.csv", file)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

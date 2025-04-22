# Load required packages
if (!require(shiny)) install.packages("shiny", dependencies = TRUE)
if (!require(qcc)) install.packages("qcc", dependencies = TRUE)

library(shiny)
library(qcc)

# Define UI
ui <- fluidPage(
  titlePanel("EWMA Control Chart Generator"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File", accept = ".csv"),
      numericInput("lambda", "EWMA λ (0.01 to 1)", value = 0.2, min = 0.01, max = 1, step = 0.01),
      actionButton("generate", "Generate EWMA Chart"),
      br(),
      downloadButton("downloadReport", "Download CSV Report")
    ),
    
    mainPanel(
      plotOutput("ewmaChart"),
      verbatimTextOutput("summary")
    )
  )
)

# Define Server
server <- function(input, output) {
  
  # Reactive expression to read and process the uploaded CSV file
  processedData <- reactive({
    req(input$file)
    
    data_raw <- read.csv(input$file$datapath, header = FALSE)
    
    if (all(data_raw[[1]] == seq_len(nrow(data_raw)))) {
      data_raw <- data_raw[, -1]
    }
    
    # Transpose the data (swap rows and columns)
    data_matrix <- t(as.matrix(sapply(data_raw, as.numeric)))
    subgroup_means <- rowMeans(data_matrix, na.rm = TRUE)
    subgroup_means <- na.omit(subgroup_means)
    
    return(subgroup_means)
  })
  
  # Store EWMA chart result for CSV report
  ewma_result <- reactiveVal(NULL)
  
  observeEvent(input$generate, {
    output$ewmaChart <- renderPlot({
      subgroup_means <- processedData()
      lambda_val <- input$lambda
      
      if (length(subgroup_means) < 2) {
        showNotification("Not enough data points to generate EWMA chart.", type = "error")
        return(NULL)
      }
      
      if (lambda_val <= 0 || lambda_val > 1) {
        showNotification("Lambda must be between 0 and 1.", type = "error")
        return(NULL)
      }
      
      center_val <- mean(subgroup_means)
      std_dev_val <- sd(subgroup_means)
      
      # Ensure we have valid standard deviation (avoid division by zero)
      if (std_dev_val == 0) {
        showNotification("Standard deviation is zero. Unable to generate EWMA chart.", type = "error")
        return(NULL)
      }
      
      # Try to generate the EWMA chart and catch potential errors
      tryCatch({
        ewma_chart <- ewma(
          data = subgroup_means,
          center = center_val,
          std.dev = std_dev_val,
          lambda = lambda_val,
          nsigmas = 3,
          plot = FALSE
        )
        
        ewma_result(ewma_chart)
        
        # Plot with legend
        plot(ewma_chart, 
             main = "EWMA Control Chart",
             xlab = "Subgroup Index", 
             ylab = "EWMA Value")
        legend("topright", 
               legend = c("EWMA", "Center", "UCL", "LCL"),
               col = c("black", "blue", "red", "red"), 
               lty = c(1, 2, 2, 2), 
               bty = "n")
      }, error = function(e) {
        showNotification(paste("Error generating EWMA chart:", e$message), type = "error")
      })
    })
    
    output$summary <- renderPrint({
      subgroup_means <- processedData()
      
      # Manual 3-sigma Calculations
      lambda_val <- input$lambda
      center_val <- mean(subgroup_means)
      std_dev_val <- sd(subgroup_means)
      
      # 3-sigma calculation
      sigma_z <- std_dev_val * sqrt(lambda_val / (2 - lambda_val))
      ucl_manual <- center_val + 3 * sigma_z
      lcl_manual <- center_val - 3 * sigma_z
      
      cat("EWMA Chart Summary:\n")
      cat("------------------------------\n")
      cat("Number of Subgroups:", length(subgroup_means), "\n")
      cat("Lambda (λ):", input$lambda, "\n")
      cat("Center (Mean):", center_val, "\n")
      cat("Standard Deviation:", std_dev_val, "\n\n")
      
      cat("Manual 3-Sigma Calculations:\n")
      cat("  Sigma (σ_z):", round(sigma_z, 4), "\n")
      cat("  UCL (Manual 3-Sigma):", round(ucl_manual, 4), "\n")
      cat("  LCL (Manual 3-Sigma):", round(lcl_manual, 4), "\n")
    })
  })
  
  # CSV Download Handler
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste("ewma_report_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      result <- ewma_result()
      if (!is.null(result)) {
        data_out <- data.frame(
          Index = seq_along(result$statistics),
          EWMA = result$statistics,
          Center = result$center,
          UCL = result$limits[,2],
          LCL = result$limits[,1]
        )
        write.csv(data_out, file, row.names = FALSE)
      }
    }
  )
}

# Run the Shiny App
shinyApp(ui = ui, server = server)

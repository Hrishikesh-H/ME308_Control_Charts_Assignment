# Load required packages
if (!require(shiny)) install.packages("shiny", dependencies = TRUE)
if (!require(qcc)) install.packages("qcc", dependencies = TRUE)

library(shiny)
library(qcc)

# Define UI
ui <- fluidPage(
  titlePanel("S-chart Control Chart Generator"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File", accept = ".csv"),
      actionButton("generate", "Generate S-chart"),
      br(),
      downloadButton("downloadReport", "Download CSV Report")
    ),
    
    mainPanel(
      plotOutput("sChart"),
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
    
    # Convert the rows as subgroup values and columns as samples (transpose the matrix)
    data_matrix <- t(as.matrix(sapply(data_raw, as.numeric)))
    
    # Return the transposed data as a format compatible with qcc
    return(list(data_matrix = data_matrix, data_raw = data_raw))
  })
  
  # Store S-chart result for CSV report
  sChart_obj <- reactiveVal(NULL)
  
  observeEvent(input$generate, {
    processed_data <- processedData()
    data_matrix <- processed_data$data_matrix
    
    # Generate S-chart
    s_chart <- qcc(data = data_matrix, type = "S")
    
    sChart_obj(s_chart)  # Store the result
    
    # Plot S-chart
    output$sChart <- renderPlot({
      plot(s_chart, 
           main = "S-chart Control Chart",
           xlab = "Subgroup Index", 
           ylab = "Standard Deviation (S)")
      
      # Add legend for clarity
      legend("topright", 
             legend = c("S-chart", "UCL", "LCL"),
             col = c("black", "red", "red"), 
             lty = c(1, 2, 2), 
             bty = "n")
    })
    
    # Summary output for S-chart
    output$summary <- renderPrint({
      s_chart <- sChart_obj()
      data_raw <- processed_data$data_raw
      n_samples <- ncol(data_matrix)  # Number of columns (samples)
      n_subgroups <- nrow(data_matrix)  # Number of rows (subgroups)
      
      # Manual calculation for center (S̄) and std deviation (S)
      manual_center <- mean(s_chart$statistics)
      manual_sd <- sd(s_chart$statistics)
      
      # Manual calculation for control limits using A2
      A2 <- 0.577  # A2 constant for sample sizes of 2 (if n > 2, change this constant accordingly)
      manual_UCL_A2 <- manual_center + A2 * manual_center
      manual_LCL_A2 <- manual_center - A2 * manual_center
      
      # Manual calculation for control limits using 3-sigma method
      manual_UCL_3sigma <- manual_center + 3 * manual_sd
      manual_LCL_3sigma <- manual_center - 3 * manual_sd
      
      # Print Summary Information for qcc
      cat("S‑Chart Summary (from qcc):\n")
      cat("  Number of Subgroups (Samples):    ", s_chart$n.subgroups, "\n")
      cat("  Sample Size (Per Subgroup):       ", s_chart$sizes, "\n")
      cat("  Center (S̄) from qcc:             ", round(s_chart$center, 4), "\n")
      cat("  Standard Deviation (S) from qcc:  ", round(s_chart$std.dev, 4), "\n")
      cat("  UCL (Upper Control Limit) from qcc:", round(s_chart$limits[2], 4), "\n")
      cat("  LCL (Lower Control Limit) from qcc:", round(s_chart$limits[1], 4), "\n")
      
      # Manual calculations for control limits and center
      cat("\nManual Calculation of Parameters:\n")
      cat("  Center (S̄) from manual calculation: ", round(manual_center, 4), "\n")
      cat("  Standard Deviation (S) from manual calculation: ", round(manual_sd, 4), "\n")
      cat("  UCL (Upper Control Limit) from manual calculation using A2: ", round(manual_UCL_A2, 4), "\n")
      cat("  LCL (Lower Control Limit) from manual calculation using A2: ", round(manual_LCL_A2, 4), "\n")
      cat("  UCL (Upper Control Limit) from manual calculation using 3-sigma: ", round(manual_UCL_3sigma, 4), "\n")
      cat("  LCL (Lower Control Limit) from manual calculation using 3-sigma: ", round(manual_LCL_3sigma, 4), "\n")
      
      # Print Manual calculation for sample size and number of subgroups
      cat("\nManual Calculation Summary:\n")
      cat("  Number of Subgroups (Manual):    ", n_subgroups, "\n")
      cat("  Sample Size (Manual):            ", n_samples, "\n")
    })
  })
  
  # CSV Download Handler
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste("s_chart_report_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      s_chart <- sChart_obj()
      if (!is.null(s_chart)) {
        # Create a report with S-chart statistics
        data_out <- data.frame(
          Index = seq_along(s_chart$statistics),
          S = s_chart$statistics,
          Center = s_chart$center,
          UCL = s_chart$limits[,2],
          LCL = s_chart$limits[,1]
        )
        write.csv(data_out, file, row.names = FALSE)
      }
    }
  )
}

# Run the Shiny App
shinyApp(ui = ui, server = server)

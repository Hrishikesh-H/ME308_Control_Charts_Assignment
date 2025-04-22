# Load required packages
if (!require(shiny)) install.packages("shiny", dependencies = TRUE)
if (!require(qcc)) install.packages("qcc", dependencies = TRUE)

library(shiny)
library(qcc)

# Define D3 and D4 constants based on subgroup size (n)
D3_values <- c(0, 0, 0, 0, 0, 0, 0.08, 0.14, 0.18, 0.21, 
               0.24, 0.27, 0.29, 0.31, 0.33, 0.35, 0.36, 0.37, 0.38, 0.39, 
               0.40, 0.41, 0.42, 0.43, 0.44, 0.45, 0.46, 0.47, 0.48, 0.49, 
               0.50)

D4_values <- c(
  0,
  3.2665,  # n = 2
  2.5746,  # n = 3
  2.2821,  # n = 4
  2.1145,  # n = 5
  2.0038,  # n = 6
  1.9243,  # n = 7
  1.8638,  # n = 8
  1.8160,  # n = 9
  1.7770,  # n = 10
  1.7444,  # n = 11
  1.7167,  # n = 12
  1.6928,  # n = 13
  1.6719,  # n = 14
  1.6534,  # n = 15
  1.6370,  # n = 16
  1.6221,  # n = 17
  1.6087,  # n = 18
  1.5965,  # n = 19
  1.5853,  # n = 20
  1.5750,  # n = 21
  1.5655,  # n = 22
  1.5566,  # n = 23
  1.5484,  # n = 24
  1.5407   # n = 25
)


# Define UI
ui <- fluidPage(
  titlePanel("R Chart Generator"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File", accept = ".csv"),
      actionButton("generate", "Generate R Chart"),
      br(),
      downloadButton("downloadReport", "Download CSV Report")
    ),
    
    mainPanel(
      plotOutput("rChart"),
      verbatimTextOutput("summary")
    )
  )
)

# Define Server
server <- function(input, output) {
  
  # Reactive expression to read and process the uploaded CSV file
  formattedData <- reactive({
    req(input$file)
    data_raw <- read.csv(input$file$datapath, header = FALSE)
    data_matrix <- as.matrix(sapply(data_raw, as.numeric))
    data_t <- t(data_matrix)  # Transpose to make columns = subgroups
    return(data_t)
  })
  
  r_chart_obj <- reactiveVal(NULL)
  manual_vals <- reactiveVal(NULL)
  
  observeEvent(input$generate, {
    data <- formattedData()
    subgroup_size <- ncol(data)
    num_subgroups <- nrow(data)
    
    # Calculate Ranges
    ranges <- apply(data, 2, function(x) max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
    R_bar <- mean(ranges, na.rm = TRUE)
    R_sd <- sd(ranges, na.rm = TRUE)
    
    
    # Manual 3-sigma limits using R̄ and the calculated standard deviation of the ranges
    ucl_3sigma <- R_bar + 3 * R_sd
    lcl_3sigma <- max(0, R_bar - 3 * R_sd)
    
    # Manual classical limits using D3 and D4 constants
    D3 <- D3_values[subgroup_size]  # Get D3 from predefined table
    D4 <- D4_values[subgroup_size]  # Get D4 from predefined table 
    
    ucl_d <- R_bar * D4
    lcl_d <- R_bar * D3
    
    # Store manual results
    manual_vals(list(
      R_bar = R_bar,
      R_sd = R_sd,
      ucl_3sigma = ucl_3sigma,
      lcl_3sigma = lcl_3sigma,
      ucl_d = ucl_d,
      lcl_d = lcl_d,
      subgroup_size = subgroup_size,
      num_subgroups = num_subgroups
    ))
    
    # QCC R chart
    qc <- qcc(data, type = "R", plot = FALSE)
    r_chart_obj(qc)
    
    output$rChart <- renderPlot({
      plot(qc, main = "R Control Chart")
    })
  })
  
  output$summary <- renderPrint({
    req(r_chart_obj(), manual_vals())
    qc <- r_chart_obj()
    mv <- manual_vals()
    
    cat("R Chart Summary\n")
    cat("------------------------------\n")
    cat("Number of Subgroups (Samples):       ", mv$num_subgroups, "\n")
    cat("Sample Size (Per Subgroup):          ", mv$subgroup_size, "\n\n")
    
    cat("QCC Calculations:\n")
    cat("  Center (R̄):                        ", round(qc$center, 4), "\n")
    cat("  Within-group SD (qcc):             ", round(qc$std.dev, 4), "\n")
    cat("  UCL (qcc):                         ", round(qc$limits[2], 4), "\n")
    cat("  LCL (qcc):                         ", round(qc$limits[1], 4), "\n\n")
    
    cat("Manual Calculations:\n")
    cat("  Mean of Ranges (R̄):               ", round(mv$R_bar, 4), "\n")
    cat("  SD of Ranges:                      ", round(mv$R_sd, 4), "\n")
    cat("  3-Sigma UCL:                       ", round(mv$ucl_3sigma, 4), "\n")
    cat("  3-Sigma LCL:                       ", round(mv$lcl_3sigma, 4), "\n")
    cat("  UCL (Using D4):                    ", round(mv$ucl_d, 4), "\n")
    cat("  LCL (Using D3):                    ", round(mv$lcl_d, 4), "\n")
  })
  
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste("r_chart_report_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      qc <- r_chart_obj()
      if (!is.null(qc)) {
        data_out <- data.frame(
          Index = seq_along(qc$statistics),
          Range = qc$statistics,
          Center = qc$center,
          UCL = qc$limits[,2],
          LCL = qc$limits[,1]
        )
        write.csv(data_out, file, row.names = FALSE)
      }
    }
  )
}

# Run the Shiny app
shinyApp(ui = ui, server = server)

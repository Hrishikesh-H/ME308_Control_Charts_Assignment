# Load required packages
if (!require(shiny)) install.packages("shiny", dependencies = TRUE)
if (!require(qcc)) install.packages("qcc", dependencies = TRUE)

library(shiny)
library(qcc)

# Define UI
ui <- fluidPage(
  titlePanel("X-bar Control Chart Generator"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File", accept = ".csv"),
      actionButton("generate", "Generate X-bar Chart"),
      br(),
      downloadButton("downloadReport", "Download CSV Report")
    ),
    
    mainPanel(
      plotOutput("xbarChart"),
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
    
    # Convert the rows as sample values and columns as samples
    data_matrix <- as.matrix(sapply(data_raw, as.numeric))
    
    # Treat columns as samples and rows as sample values (each column is a sample)
    # Subgroup means are the means of each row across all samples (columns)
    subgroup_means <- colMeans(data_matrix, na.rm = TRUE)
    subgroup_means <- na.omit(subgroup_means)
    
    return(subgroup_means)
  })
  
  # Store X-bar chart result for CSV report
  xbar_result <- reactiveVal(NULL)
  
  observeEvent(input$generate, {
    output$xbarChart <- renderPlot({
      subgroup_means <- processedData()
      
      # X-bar chart settings
      center_val <- mean(subgroup_means)
      std_dev_val <- sd(subgroup_means)
      
      # Generate X-bar chart
      xbar_chart <- qcc(
        data = subgroup_means,
        type = "xbar",
        center = center_val,
        std.dev = std_dev_val,
        nsigmas = 3
      )
      
      xbar_result(xbar_chart)
      
      # Plot with legend
      plot(xbar_chart, 
           main = "X-bar Control Chart",
           xlab = "Subgroup Index", 
           ylab = "X-bar Value")
      legend("topright", 
             legend = c("X-bar", "Center", "UCL", "LCL"),
             col = c("black", "blue", "red", "red"), 
             lty = c(1, 2, 2, 2), 
             bty = "n")
    })
    
    output$summary <- renderPrint({
      subgroup_means <- processedData()
      cat("X-bar Chart Summary:\n")
      cat("Number of Subgroups:", length(subgroup_means), "\n")
      cat("Center (Mean):", mean(subgroup_means), "\n")
      cat("Standard Deviation:", sd(subgroup_means), "\n")
    })
  })
  
  # CSV Download Handler
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste("xbar_report_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      result <- xbar_result()
      if (!is.null(result)) {
        data_out <- data.frame(
          Index = seq_along(result$statistics),
          Xbar = result$statistics,
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

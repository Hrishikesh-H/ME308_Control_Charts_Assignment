library(shiny)
library(knitr)

ui <- fluidPage(
  titlePanel("Shewhart Moving Average (X̄) Control Chart"),
  sidebarLayout(
    sidebarPanel(
      fileInput("csv", "Upload CSV (no header)", accept = ".csv"),
      numericInput("window", "Moving Average Window (# subgroups):", value = 5, min = 1, step = 1),
      numericInput("k", "Control limit multiplier (k·σ):", value = 3, min = 1, step = 0.5),
      downloadButton("downloadData", "Download Data as CSV")
    ),
    mainPanel(
      plotOutput("ma_plot"),
      verbatimTextOutput("summary")
    )
  )
)

server <- function(input, output) {
  # Reactive: read and validate CSV
  data_mat <- reactive({
    req(input$csv)
    df <- read.table(input$csv$datapath, header = FALSE, sep = ",")
    mat <- as.matrix(df)
    mode(mat) <- "numeric"
    validate(
      need(nrow(mat) >= 1, "CSV must have at least 1 row."),
      need(ncol(mat) >= 2, "CSV must have at least 2 columns." )
    )
    mat
  })
  
  # Reactive: compute stats
  stats <- reactive({
    mat <- data_mat()
    n_obs <- nrow(mat)  # Number of observations (rows)
    n_sub <- ncol(mat)  # Number of subgroups (columns)
    
    # Calculate column means for moving averages
    xbar <- colMeans(mat)
    sigma <- sqrt(mean(apply(mat, 2, var)))  # Standard deviation of the columns
    center <- mean(xbar)
    w <- min(max(floor(input$window), 1), n_sub)
    k <- input$k
    
    ma <- numeric(n_sub)  # Moving averages
    upper <- numeric(n_sub)  # Upper control limits
    lower <- numeric(n_sub)  # Lower control limits
    
    # Transpose calculations (apply window on columns for moving average)
    for(i in seq_len(n_sub)) {
      idx <- seq(max(1, i - w + 1), i)
      ma[i] <- mean(xbar[idx])  # Moving average of the mean values across columns
      se <- sigma / sqrt(n_obs * length(idx))  # Standard error for moving average calculation
      upper[i] <- center + k * se  # Upper control limit
      lower[i] <- center - k * se  # Lower control limit
    }
    
    oc <- which(ma < lower | ma > upper)  # Out-of-control points
    
    # Return all parameters for plotting
    list(ma = ma, upper = upper, lower = lower,
         center = center, sigma = sigma,
         n_sub = n_sub, n_obs = n_obs, w = w, k = k,
         oc = oc, xbar = xbar)
  })
  
  # Plot output with detailed legend
  output$ma_plot <- renderPlot({
    s <- stats()
    idx <- seq_len(s$n_sub)  # Index for subgroups (columns)
    
    # Ensure all values are printed in plot
    plot(idx, s$ma, type = 'o', pch = 16,
         ylim = range(c(s$lower, s$upper, s$ma), na.rm = TRUE),  # Ensure no NAs are involved
         xlab = "Subgroup", ylab = "Moving Avg of X̄",
         main = paste0("MA Chart (window = ", s$w, ", k = ", s$k, ")"))
    
    lines(idx, s$upper, lty = 2, col = "red")
    lines(idx, s$lower, lty = 2, col = "red")
    abline(h = s$center, col = 'blue', lwd = 2)
    
    legend("topright", 
           legend = c(
             "Moving Average (MA)",
             paste("Center =", round(s$center, 4)),
             paste("σ =", round(s$sigma, 4)),
             paste("Window =", s$w),
             paste("k =", s$k),
             "Control Limits (±k·σ)"
           ),
           lty = c(1, NA, NA, NA, NA, 2),
           pch = c(16, NA, NA, NA, NA, NA),
           col = c("black", "blue", "blue", "black", "black", "red"),
           bty = 'n', cex = 0.9)
  })
  
  # Text summary
  output$summary <- renderPrint({
    s <- stats()
    cat("Subgroups:", s$n_sub, "\n")
    cat("Obs per subgroup:", s$n_obs, "\n")
    cat(sprintf("Center: %.4f, σ: %.4f\n", s$center, s$sigma))
    cat(sprintf("Window: %d, k: %.1f\n", s$w, s$k))
    if(length(s$oc)) cat("Out-of-control at:", paste(s$oc, collapse = ", "))
    else cat("All points in control.")
  })
  
  # Download data as CSV
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("ma_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      s <- stats()
      # Create a data frame with all the parameters to be downloaded
      df <- data.frame(
        Subgroup = seq_len(s$n_sub),
        Xbar = s$xbar,
        Moving_Avg = s$ma,
        Upper_Limit = s$upper,
        Lower_Limit = s$lower,
        Control_Center = s$center,
        Sigma = s$sigma
      )
      write.csv(df, file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)

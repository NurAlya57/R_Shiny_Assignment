library(shiny)
library(readxl)

ui <- fluidPage(
  titlePanel("Claim Paid Summary"),
  
  sidebarLayout(
    sidebarPanel(
      width = 4,
      
      # to upload data
      h4("Attach files (if any)"),
      fileInput(
        inputId = "file_upload",
        label = "Upload Claims Triangle (CSV or Excel):",
        accept = c(".csv", ".xlsx", ".xls")
      ),
      helpText("Note: Uploaded file should be a 3x3 matrix with cumulative loss years in rows."),
      
      hr(),#make a line to separate.
      h4("Enter Claims Manually"),
      helpText("Note: Please make sure that the value insert is not cumulative."),
      
      # manual inputs
      h5("Accident Year 2017:"),
      helpText("Claim Paid for 2017, 2018, 2019"),
      splitLayout(
<<<<<<< HEAD
        numericInput("c11", NULL, 524792), numericInput("c12", NULL, 743057),
        numericInput("c13", NULL, 745282)
=======
        numericInput("c11", NULL, 524792), numericInput("c12", NULL, 218265),
        numericInput("c13", NULL, 2225)
>>>>>>> a6dc1bdb2e789a771b4fabe1a375ba59c75a03c5
      ),
      
      h5("Accident Year 2018:"),
      helpText("Claim Paid for 2018, 2019"),
      splitLayout(
<<<<<<< HEAD
        numericInput("c21", NULL, 798502), numericInput("c22", NULL,995659),
=======
        numericInput("c21", NULL, 798502), numericInput("c22", NULL,197157),
>>>>>>> a6dc1bdb2e789a771b4fabe1a375ba59c75a03c5
      ),
      
      h5("Accident Year 2019:"),
      helpText("Claim Paid for 2019"),
      splitLayout(
        numericInput("c31", NULL, 917636)
      ),
      
      hr(),
      h4("Step 2: Parameters"),
      numericInput("tail_factor", "Tail Factor:", value = 1.1, min = 1.0, step = 0.01),
      
      actionButton("calc_btn", "Calculate", class = "btn-danger")
    ),
    
    mainPanel(
      width = 8,
      tabsetPanel(
        tabPanel("Cumulative Claim Paid ($)", 
                 h4("Cumulative Claim Paid ($)"),
                 tableOutput("completed_table")),
        
        tabPanel("Ultimate Cumulative, 2020 ($)", 
                 h4("Ultimate Cumulative, 2020 ($)"),
                 tableOutput("summary_table")),
        
        tabPanel("Cumulative Graph", 
                 h4("Cumulative Graph"),
                 plotOutput("dev_plot"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  # make sure user click the action button, baru display result
  results <- eventReactive(input$calc_btn, {
    
    tri <- matrix(NA, nrow = 3, ncol = 3)
    
    # check file
    if (!is.null(input$file_upload)) {
      filepath <- input$file_upload$datapath
      ext <- tools::file_ext(filepath)
      
      #read file
      if (ext == "csv") {
        data <- read.csv(filepath, row.names = 1)
      } else {
        data <- as.data.frame(read_excel(filepath))
        rownames(data) <- data[, 1]
        data <- data[, -1] 
      }
      
      tri <- as.matrix(data)
      
    } else {
      # if no file attached
      #input must be in incremental not cumulative
<<<<<<< HEAD
      tri[1, ] <- c(input$c11, input$c12, input$c13)
      tri[2, 1:2] <- c(input$c21, input$c22)
=======
      tri[1, ] <- c(input$c11, input$c11+input$c12, input$c11+input$c12+input$c13)
      tri[2, 1:2] <- c(input$c21, input$c21+input$c22)
>>>>>>> a6dc1bdb2e789a771b4fabe1a375ba59c75a03c5
      tri[3, 1] <- input$c31
      
      rownames(tri) <- c("2017", "2018", "2019")
      colnames(tri) <- c("Dev 1", "Dev 2", "Dev 3")
    }
    
    
    #calculate development factor (cumulative)
    f1 <- (tri[1, 2] + tri[2, 2]) / (tri[1, 1] + tri[2, 1])
    f2 <- (tri[1, 3]) / (tri[1, 2] )
    
    f <- c(f1, f2)
    
    #lower right triangle
    completed <- tri
    completed[2, 3] <- completed[2, 2] * f[2]
    completed[3, 2] <- completed[3, 1] * f[1]
    completed[3, 3] <- completed[3, 2] * f[2]
    
    #multiplier
    tail_f <- input$tail_factor
    cdf3 <- tail_f
    cdf2 <- cdf3 * f[2]
    cdf1 <- cdf2 * f[1]
    
    latest_claims <- c(tri[1, 3], tri[2, 2], tri[3, 1])
    selected_cdfs <- c(cdf3, cdf2, cdf1)
    ultimates <- latest_claims * selected_cdfs
    
    summary_df <- data.frame(
      Loss_Year = rownames(tri),
      Total_Claim = round(latest_claims, 2),
      Multiplier = round(selected_cdfs, 4),
      Ultimate_Claim = round(ultimates, 2)
    )
    
    list(
      triangle = tri,
      completed = round(completed, 2),
      summary = summary_df
    )
  })
  
  output$completed_table <- renderTable({
    req(results())
    results()$completed
  }, rownames = TRUE)
  
  output$summary_table <- renderTable({
    req(results())
    results()$summary
  })
  
  output$dev_plot <- renderPlot({
    req(results())
    
    #data include the ultimate
    tri_comp <- results()$completed
    summary_df <- results()$summary
    full_data <- cbind(tri_comp, Ultimate = summary_df$Ultimate_Claim)
    
    #2017 plot
    plot(1:4, full_data[1, ], type = "b", col = "red", pch = 16, lwd = 2, lty = 1,
         ylim = c(0, max(full_data, na.rm = TRUE) * 1.2),
         xlab = "Development Stage", ylab = "Cumulative Paid Claim ($)",
         xaxt = "n", main = "Cumulative Claims Development")
    
    axis(1, at = 1:4, labels = c("Dev 1", "Dev 2", "Dev 3", "Ultimate"))
    
    #2018 plot
    lines(1:3, full_data[2, 1:3], type = "b", col = "green", pch = 16, lwd = 2, lty = 1)
    lines(3:4, full_data[2, 3:4], type = "b", col = "green", pch = 17, lwd = 2, lty = 2)
    
    #2019 plot
    lines(1:2, full_data[3, 1:2], type = "b", col = "purple", pch = 16, lwd = 2, lty = 1)
    lines(2:4, full_data[3, 2:4], type = "b", col = "purple", pch = 17, lwd = 2, lty = 2)
    
    #reference
    legend("bottomright", 
           legend = c(rownames(full_data), "Actual Data", "Ultimate"),
           col = c("red", "green", "purple", "black", "black"), 
           lty = c(1, 1, 1, 1, 2), 
           pch = c(16, 16, 16, 16, 17),
           bg = "white")
  })
}

<<<<<<< HEAD
shinyApp(ui, server)
library(shiny)
library(readxl)

ui <- fluidPage(
  titlePanel("Claim Paid Summary"),
  
  sidebarLayout(
    sidebarPanel(
      width = 4,
      
      # to upload data
      h4("Attach files (if any)"),
      fileInput(
        inputId = "file_upload",
        label = "Upload Claims Triangle (CSV or Excel):",
        accept = c(".csv", ".xlsx", ".xls")
      ),
      helpText("Note: Uploaded file should be a 3x3 matrix with cumulative loss years in rows."),
      
      hr(),#make a line to separate.
      h4("Enter Claims Manually"),
      helpText("Note: Please make sure that the value insert is cumulative."),
      
      # manual inputs
      h5("Accident Year 2017:"),
      helpText("Claim Paid for 2017, 2018, 2019"),
      splitLayout(
        numericInput("c11", NULL, 524792), numericInput("c12", NULL, 743057),
        numericInput("c13", NULL, 745282)
      ),
      
      h5("Accident Year 2018:"),
      helpText("Claim Paid for 2018, 2019"),
      splitLayout(
        numericInput("c21", NULL, 798502), numericInput("c22", NULL, 995659),
      ),
      
      h5("Accident Year 2019:"),
      helpText("Claim Paid for 2019"),
      splitLayout(
        numericInput("c31", NULL, 917636)
      ),
      
      hr(),
      h4("Step 2: Parameters"),
      numericInput("tail_factor", "Tail Factor:", value = 1.1, min = 1.0, step = 0.01),
      
      actionButton("calc_btn", "Calculate", class = "btn-danger")
    ),
    
    mainPanel(
      width = 8,
      tabsetPanel(
        tabPanel("Cumulative Claim Paid ($)", 
                 h4("Cumulative Claim Paid ($)"),
                 tableOutput("completed_table")),
        
        tabPanel("Ultimate Cumulative, 2020 ($)", 
                 h4("Ultimate Cumulative, 2020 ($)"),
                 tableOutput("summary_table")),
        
        tabPanel("Cumulative Graph", 
                 h4("Cumulative Graph"),
                 plotOutput("dev_plot"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  # make sure user click the action button, baru display result
  results <- eventReactive(input$calc_btn, {
    
    tri <- matrix(NA, nrow = 3, ncol = 3)
    
    # check file
    if (!is.null(input$file_upload)) {
      filepath <- input$file_upload$datapath
      ext <- tools::file_ext(filepath)
      
      #read file
      if (ext == "csv") {
        data <- read.csv(filepath, row.names = 1)
      } else {
        data <- as.data.frame(read_excel(filepath))
        rownames(data) <- data[, 1]
        data <- data[, -1] 
      }
      
      tri <- as.matrix(data)
      
    } else {
      # if no file attached
      #input must be in incremental not cumulative
      tri[1, ] <- c(input$c11, input$c12, input$c13)
      tri[2, 1:2] <- c(input$c21, input$c22)
      tri[3, 1] <- input$c31
      
      rownames(tri) <- c("2017", "2018", "2019")
      colnames(tri) <- c("Dev 1", "Dev 2", "Dev 3")
    }
    
    
    #calculate development factor (cumulative)
    f1 <- (tri[1, 2] + tri[2, 2]) / (tri[1, 1] + tri[2, 1])
    f2 <- (tri[1, 3]) / (tri[1, 2] )
    
    f <- c(f1, f2)
    
    #lower right triangle
    completed <- tri
    completed[2, 3] <- completed[2, 2] * f[2]
    completed[3, 2] <- completed[3, 1] * f[1]
    completed[3, 3] <- completed[3, 2] * f[2]
    
    #multiplier
    tail_f <- input$tail_factor
    cdf3 <- tail_f
    cdf2 <- cdf3 * f[2]
    cdf1 <- cdf2 * f[1]
    
    latest_claims <- c(tri[1, 3], tri[2, 2], tri[3, 1])
    selected_cdfs <- c(cdf3, cdf2, cdf1)
    ultimates <- latest_claims * selected_cdfs
    
    summary_df <- data.frame(
      Loss_Year = rownames(tri),
      Total_Claim = round(latest_claims, 2),
      Multiplier = round(selected_cdfs, 4),
      Ultimate_Claim = round(ultimates, 2)
    )
    
    list(
      triangle = tri,
      completed = round(completed, 2),
      summary = summary_df
    )
  })
  
  output$completed_table <- renderTable({
    req(results())
    results()$completed
  }, rownames = TRUE)
  
  output$summary_table <- renderTable({
    req(results())
    results()$summary
  })
  
  output$dev_plot <- renderPlot({
    req(results())
    
    #data include the ultimate
    tri_comp <- results()$completed
    summary_df <- results()$summary
    full_data <- cbind(tri_comp, Ultimate = summary_df$Ultimate_Claim)
    
    #2017 plot
    plot(1:4, full_data[1, ], type = "b", col = "red", pch = 16, lwd = 2, lty = 1,
         ylim = c(0, max(full_data, na.rm = TRUE) * 1.2),
         xlab = "Development Stage", ylab = "Cumulative Paid Claim ($)",
         xaxt = "n", main = "Cumulative Claims Development")
    
    axis(1, at = 1:4, labels = c("Dev 1", "Dev 2", "Dev 3", "Ultimate"))
    
    #2018 plot
    lines(1:3, full_data[2, 1:3], type = "b", col = "green", pch = 16, lwd = 2, lty = 1)
    lines(3:4, full_data[2, 3:4], type = "b", col = "green", pch = 17, lwd = 2, lty = 2)
    
    #2019 plot
    lines(1:2, full_data[3, 1:2], type = "b", col = "purple", pch = 16, lwd = 2, lty = 1)
    lines(2:4, full_data[3, 2:4], type = "b", col = "purple", pch = 17, lwd = 2, lty = 2)
    
    #reference
    legend("bottomright", 
           legend = c(rownames(full_data), "Actual Data", "Ultimate"),
           col = c("red", "green", "purple", "black", "black"), 
           lty = c(1, 1, 1, 1, 2), 
           pch = c(16, 16, 16, 16, 17),
           bg = "white")
  })
}

shinyApp(ui, server)
rsconnect::writenManifest()
=======
shinyApp(ui, server)
>>>>>>> a6dc1bdb2e789a771b4fabe1a375ba59c75a03c5

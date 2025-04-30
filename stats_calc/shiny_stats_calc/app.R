library(shiny)
#
#
# FUNCTION DEFINITIONS
#
#
# function to pause and wait for user input


# function to calculate and display descriptive statistics for a selected variable
descriptive_stats = function(data, variable) {
  # print the name of the variable being analyzed
  cat("Descriptive statistics of", variable, "\n")
  cat("NOTE: missing values are ignored. \n\n")
  column = data[[variable]] # extract the specific column from the dataset
  
  # if the column is numeric, calculate various statistics
  if (is.numeric(column)) {
    #calculate and print mean
    cat("Mean:  ", mean(column, na.rm=TRUE), "\n")
    
    #calculate and print median
    cat("Median:  ", median(column, na.rm=TRUE), "\n")
    
    #calculate and print quartiles
    cat("Quantiles (0th to 4th):  ")
    cat(quantile(column, na.rm=TRUE), sep = " ")
    cat("\n")
    
    #calculate and print interquartile range (IQR)
    cat("IQR: ", IQR(column, na.rm=TRUE), "\n")
    
    # calculate and print sample variance (n-1 degrees of freedom)
    cat("Sample Variance (n-1): ", var(column, na.rm=TRUE), "\n")
    
    # calculate and print sample standard deviation (n-1 degrees of freedom)
    cat("Sample Standard Deviation (n-1): ", sd(column, na.rm=TRUE), "\n")
  } else {
    # if the column is not numeric, display frequency table
    cat("Frequency table: ")
    print(sort(table(column), decreasing = TRUE))
  }
}

ui =
  fluidPage(
    titlePanel("Stats Calculator"),
    
    sidebarLayout(sidebarPanel(
      selectInput(
        "alpha",
        "Significance level (α or alpha): ",
        list(0.001, 0.01, 0.05, 0.10),
        selected = 0.05
      ),
      radioButtons("response", "Response variable", "N/A"),
    ),
    mainPanel(
      
      fileInput("file", h3(".csv file input"), accept = "text/csv"),
      column(6, verbatimTextOutput("validation")),
      
      column(6, verbatimTextOutput("descriptive")),
      
      column(9, verbatimTextOutput("normality_text")),
      column(4, plotOutput("box_plot")),
      column(4, plotOutput("q.q_plot")),
      
      column(9, verbatimTextOutput("subset_model")),
      column(4, plotOutput("q.q_res")),
      column(4, plotOutput("outlier")),)
    )
  )

server = function(input, output) {
  observe({
    if (!is.null(input$file)) {
      csv = read.csv(input$file$datapath, stringsAsFactors = TRUE)
      updateRadioButtons(inputId = "response", choices = names(csv))
      
      output$validation = renderPrint({
        str(csv)
      })
      
      # function to calculate and display descriptive statistics for a selected variable
      output$descriptive = renderPrint({
        descriptive_stats(csv, input$response)
      })
      
      # check normality for the selected response variable
      output$normality_text = renderPrint({
        alpha = as.numeric(input$alpha)
        column = csv[[input$response]] # extract the selected column
        
        # if the column is numeric, proceed with plotting and testing
        if (is.numeric(column)) {
          cat("Normality tests for", input$response, "\n")
          # perform Shapiro-Wilk test for normality
          shapiro_results = shapiro.test(column)
          # print the p-value
          print(shapiro_results)
          # interpret the p value results
          cat("At a(n)",
              100 * alpha,
              "% level of significance, the response variable ")
          if (shapiro_results$p.value < alpha) {
            cat(
              "is not normally distributed because the p-value is lower than ",
              100 * alpha,
              "%.\n\n"
            )
          } else {
            cat(
              "might be normally distributed because the p-value is greater than or equal to",
              100 * alpha,
              "%.\n\n"
            )
          }
          cat(
            "If the response is normal, the following box-and-whisker plot should be somewhat symmetrical.",
            "If the response is normal, the following Q-Q plot should show a straight line.",
            sep = "\n"
          )
        }
        
        output$box_plot = renderPlot({
          # if the column is numeric, proceed with plotting and testing
          if (is.numeric(column)) {
            # create and display a box-and-whisker plot for the selected variable
            boxplot(column)
            title("Box Plot")
          }
        })
        
        output$q.q_plot = renderPlot({
          # if the column is numeric, proceed with plotting and testing
          if (is.numeric(column)) {
            # create and display a Q-Q plot to visually assess normality
            qqnorm(column)
            qqline(column)
          }
        })
      })
      
      output$subset_model = renderPrint({
        if (is.numeric(csv[[input$response]])) {
          # fit an empty model
          empty_model = lm(as.formula(paste(input$response, " ~ 1")), csv)
          
          # fit a full model with all predictors and their interactions
          full_model = lm(as.formula(paste(input$response, " ~ .^2")), csv)
          
          # perform regression to find the best model
          cat("Subset linear regression model (stepwise regression, best AIC): ")
          subset_model = step(
            empty_model,
            scope = formula(full_model),
            direction = "both",
            trace = 0 # suppress detailed tracing output
          )
          
          # display the summary and ANOVA for the selected model
          print(
            summary(
              subset_model,
              correlation = T,
              symbolic.cor = F,
              show.signif.stars = T
            )
          )
          print(anova(subset_model))
          
          cat(
            "\nThe residuals should be normally distributed by following the line in the following Q-Q plot.",
            "In the following Residuals vs Leverage plot: ",
            "  * Leverage points (outliers) are indicated by being past the vertical line.",
            "  * Influential points (very bad outliers) are indicated by being within the Cook's distances",
            sep = "\n"
          )
          leverage = hatvalues(subset_model)
          leverage_points = csv[leverage > 2 * mean(leverage), ]
          if (nrow(leverage_points) >= 1) {
            cat("Leverage points: \n")
            print(leverage_points)
          }
          
          distances = cooks.distance(subset_model)
          influential_points = csv[distances > min(1, qf(0.5, sum(leverage), nrow(csv) - sum(leverage))), ]
          if (nrow(influential_points) >= 1) {
            cat("Influential points: \n")
            print(influential_points)
          }
          
          
          output$q.q_res = renderPlot({
            plot(subset_model, 2)
          })
          
          output$outlier = renderPlot({
            plot(subset_model, 5, cook.levels = c(1, qf(
              0.5, sum(leverage), nrow(csv) - sum(leverage)
            )))
            abline(v = 2 * mean(leverage))
          })
        }
      })
    }
  })
}

shinyApp(ui, server)

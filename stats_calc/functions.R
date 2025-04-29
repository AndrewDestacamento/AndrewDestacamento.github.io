#
#
# FUNCTION DEFINITIONS
#
#
# function to pause and wait for user input
pause = function(message = "") {
  invisible(readline(paste(message, "\n Press <Return> to continue.")))
}

# function to check normality of a selected variable
normality = function(data, variable) {
  column = data[[variable]] # extract the selected column
  
  # if the column is numeric, proceed with plotting and testing
  if (is.numeric(column)) {
    # create and display a box-and-whisker plot for the selected variable
    cat("Box-and-whisker plot of  ", variable, ":\n")
    boxplot(column)
    pause("If the response is normal, the box-and-whisker plot should be somewhat symmetrical.")
    
    # create and display a Q-Q plot to visually assess normality
    cat("Q-Q plot of  ", variable, ":\n")
    qqnorm(column)
    pause("If the response is normal, the Q-Q plot should show a straight line.")
    
    # perform Shapiro-Wilk test for normality
    shapiro_results = shapiro.test(column)
    # print the p-value
    cat("Shapiro-Wilk normality test: p-value = ",
        shapiro_results$p.value,
        "\n")
    # interpret the p value results
    cat("At a(n) ", 100*alpha, "% level of significance, the response variable ")
    if (shapiro_results$p.value < alpha) {
      cat("is not normally distributed because the p-value is lower than ", 100*alpha, "%.")
    } else {
      cat("might be normally distributed because the p-value is greater than or equal to ", 100*alpha, "%.")
    }
    pause()
    
    remove(shapiro_results) # remove the temporary variable used for Shapiro-Wilk results
  }
  remove(column) # remove the temporary column variable
}

# function to calculate and display descriptive statistics for a selected variable
descriptive_stats = function(data, variable) {
  # print the name of the variable being analyzed
  cat("Descriptive statistics of ", variable, "\n")
  column = data[[variable]] # extract the specific column from the dataset
  
  # if the column is numeric, calculate various statistics
  if (is.numeric(column)) {
    #calculate and print mean
    cat("Mean:  ", mean(column), "\n")
    
    #calculate and print median
    cat("Median:  ", median(column), "\n")
    
    #calculate and print quartiles
    cat("Quantiles (0th to 4th):  ")
    cat(quantile(column), sep = " ")
    cat("\n")
    
    #calculate and print interquartile range (IQR)
    cat("IQR: ", IQR(column), "\n")
    
    # calculate and print sample variance (n-1 degrees of freedom)
    cat("Sample Variance (n-1): ", var(column), "\n")
    
    # calculate and print sample standard deviation (n-1 degrees of freedom)
    cat("Sample Standard Deviation (n-1): ", sd(column), "\n")
  } else {
    # if the column is not numeric, display frequency table
    cat("Frequency table: ")
    print(sort(table(column), decreasing = TRUE))
  }
  pause() # pause after displaying statistics to allow user to review the results
  remove(column) # remove temporary column variable
}

modeling = function(data, variable) {
  # fit an empty model
  cat("Empty linear regression model (just y-intercept): ")
  empty_model = lm(as.formula(paste(variable, " ~ 1")), data)
  # display summary and ANOVA for the empty model
  print(summary(empty_model))
  print(anova(empty_model))
  pause() # pause before moving on to the full model
  
  # fit a full model with all predictors and their interactions
  cat("Full linear regression model (y-intercept and all other variables): ")
  full_model = lm(as.formula(paste(variable, " ~ .^2")), data)
  # display summary and ANOVA for the full model
  print(summary(full_model))
  print(anova(full_model))
  pause() # pause before moving on to model selection
  
  
  # perform regression to find the best model
  cat("Subset linear regression model (stepwise regression, best AIC): ")
  subset_model = step(
    empty_model,
    scope = formula(full_model),
    direction = "both",
    trace = 0 # suppress detailed tracing output
  )
  # display the summary and ANOVA for the selected model
  print(summary(
    subset_model,
    correlation = T,
    symbolic.cor = F,
    show.signif.stars = T
  ))
  print(anova(subset_model))
  pause() # pause after showing final model results
  
  plot(subset_model, 2)
  pause("Q-Q plot of the residuals. The residuals should be normally distributed by following the line.")
  plot(subset_model, 5, cook.levels = c(1, qf(0.5, sum(hatvalues(subset_model)), nrow(data) - sum(hatvalues(subset_model))))) 
  abline(v = 2 * mean(hatvalues(subset_model)))
  pause("Residuals vs Leverage plot. Leverage points (outliers) are past the vertical line, and influential points (very bad outliers) are within Cook's distances.")
}
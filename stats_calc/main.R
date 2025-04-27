# function to pause and wait for user input
pause = function(message = "") {
  invisible(readline(paste(message, " Press <Return> to continue.")))
}

# function to check normality of a selected variable
normality = function(data, position) {
  column = data[[position]] # extract the selected column

  # if the column is numeric, proceed with plotting and testing
  if (is.numeric(column)) { 
    # create and display a box-and-whisker plot for the selected variable
    cat("Box-and-whisker plot of  ", names(data[position]), ":\n")
    boxplot(column)
    pause("If the response is normal, the box-and-whisker plot should be somewhat symmetrical.")
    # create and display a Q-Q plot to visually assess normality
    cat("Q-Q plot of  ", names(data[position]), ":\n")
    qqnorm(column)
    pause("If the response is normal, the Q-Q plot should show a straight line.")
    # perform Shapiro-Wilk test for normality 
    shapiro_results = shapiro.test(column)
    # print the p-value
    cat("Shapiro-Wilk normality test: p-value = ",
        shapiro_results$p.value,
        "\n")
    # interpret the p value results
    if (shapiro_results$p.value < 0.05) {
      pause(
        "At a 5% level of significance, the response variable is not normally distributed because the p-value is lower than 5%."
      )
    } else {
      pause(
        "At a 5% level of significance, the response variable might be normally distributed because the p-value is greater than or equal to 5%."
      )
    }
    remove(shapiro_results) # remove the temporary variable used for Shapiro-Wilk results
  }
  remove(column) # remove the temporary column variable
}

# function to calculate and display descriptive statistics for a selected variable
descriptive_stats = function(data, position) {
  # print the name of the variable being analyzed
  cat("Descriptive statistics of ", names(data[position]), "\n")
  column = data[[position]] # extract the specific column from the dataset

  # if the column is numeric, calculate various statistics 
  if (is.numeric(column)) {
    #calculate and print mean
    cat("Mean:  ", mean(column), "\n")
    #calculate and print median
    cat("Median:  ", median(column), "\n")
    #calculate and print quantiles 
    cat("Quantiles (0th to 4th):  ")
    for (quantile in quantile(column)) {
      cat(quantile, "  ")
    }
    cat("\n")
    #calculate and print interquartile range (IQR)
    cat("IQR: ", IQR(column), "\n")
    # calculate and print sample variance (n-1 degrees of freedom)
    cat("Sample Variance (n-1): ", var(column), "\n")
    # calculate and print sample standard deviation (n-1 degrees of freedom)
    cat("Sample Standard Deviation (n-1): ", sd(column), "\n")
  } else {
    # if the column is not numeric, display frequency table
    print(sort(table(column), decreasing = TRUE))
  }
  pause() # pause after displaying statistics to allow user to review the results 
  remove(column) # remove temporary column variable
}

# load the dataset from CSV file
data = read.csv("examples/Baseball data.csv", stringsAsFactors = TRUE)
# print the structure of the loaded data set 
print(str(data))

if (length(data) == 1) {
  # if the dataset contains only one column, run descriptive stats directly
  descriptive_stats(data, 1)
} else if (length(data) > 1) {
  # if the dataset contains more than one column, ask the user to choose a response variable
  response = as.integer(readline("Starting from 1, enter the position of the response variable: "))
  # run descriptive statistics for the selected response variable 
  descriptive_stats(data, response)
  # check normality for the selected response variable 
  normality(data, response)
  
  # fit an empty model 
  empty_model = lm(as.formula(paste(names(data)[response], " ~ 1")), data)
  # display summary and ANOVA for the empty model
  print(summary(empty_model))
  print(anova(empty_model))
  pause() # pause before moving on to the full model

  # fit a full model with all predictors and their interactions
  full_model = lm(as.formula(paste(names(data)[response], " ~ .^2")), data)
  # display summary and ANOVA for the full model
  print(summary(full_model))
  print(anova(full_model))
  pause() # pause before moving on to model selection 


  # perform regression to find the best model
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

  # clean up temporary variables
  remove(response, empty_model, full_model, subset_model)
}
# remove the defined functions and dataset to clean up memory 
remove(pause, normality, descriptive_stats, data)

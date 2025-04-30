library(shiny)
library(bslib)
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
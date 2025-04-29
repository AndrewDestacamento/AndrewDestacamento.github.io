alpha = 0.05
source("functions.R")
#
#
# ENTRY POINT
#
#
cat("List of options: ",
    "1. Distribution value lookup",
    "2. Dataset upload (more options)",
    "",
    "0. Exit.",
    cat("9. Change significance level (currently ", 100*alpha, "%)"),
    sep="\n")
choice = as.integer(readline("Enter your option: "))
if (option == 1) {
  
}

# load the dataset from CSV file
file_path = readline("Enter path to dataset (relative or absolute): ")
cat("Entered path: ", file_path, "\n")
data = read.csv(file_path, stringsAsFactors = TRUE)

# print the structure of the loaded data set
str(data) # Prints to output directly

if (length(data) == 1) {
  # if the dataset contains only one column, run descriptive stats directly
  descriptive_stats(data, names(data)[1])
  
} else if (length(data) > 1) {
  # if the dataset contains more than one column, ask the user to choose a response variable
  response = readline("From the above, enter the name of the response variable: ")
  
  # run descriptive statistics for the selected response variable
  descriptive_stats(data, response)
  
  # check normality for the selected response variable
  normality(data, response)
  
  # show estimated linear regressions
  modeling(data, response)
}
# remove the defined functions and dataset to clean up memory
# variables from functions are removed when the function is removed
remove(pause, normality, descriptive_stats, modeling, file_path, data, response)

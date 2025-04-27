pause = function(message = "") {
  invisible(readline(paste(message, " Press <Return> to continue.")))
}

normality = function(data, position) {
  column = data[[position]]
  
  if (is.numeric(column)) {
    cat("Box-and-whisker plot of  ", names(data[position]), ":\n")
    boxplot(column)
    pause("If the response is normal, the box-and-whisker plot should be somewhat symmetrical.")
    
    cat("Q-Q plot of  ", names(data[position]), ":\n")
    qqnorm(column)
    pause("If the response is normal, the Q-Q plot should show a straight line.")
    
    shapiro_results = shapiro.test(column)
    cat("Shapiro-Wilk normality test: p-value = ",
        shapiro_results$p.value,
        "\n")
    if (shapiro_results$p.value < 0.05) {
      pause(
        "At a 5% level of significance, the response variable is not normally distributed because the p-value is lower than 5%."
      )
    } else {
      pause(
        "At a 5% level of significance, the response variable might be normally distributed because the p-value is greater than or equal to 5%."
      )
    }
    remove(shapiro_results)
  }
  remove(column)
}

descriptive_stats = function(data, position) {
  cat("Descriptive statistics of ", names(data[position]), "\n")
  column = data[[position]]
  
  if (is.numeric(column)) {
    cat("Mean:  ", mean(column), "\n")
    cat("Median:  ", median(column), "\n")
    
    cat("Quantiles (0th to 4th):  ")
    for (quantile in quantile(column)) {
      cat(quantile, "  ")
    }
    cat("\n")
    
    cat("IQR: ", IQR(column), "\n")
    cat("Sample Variance (n-1): ", var(column), "\n")
    cat("Sample Standard Deviation (n-1): ", sd(column), "\n")
  } else {
    print(sort(table(column), decreasing = TRUE))
  }
  pause()
  remove(column)
}

data = read.csv("examples/Baseball data.csv", stringsAsFactors = TRUE)
print(str(data))

if (length(data) == 1) {
  descriptive_stats(data, 1)
} else if (length(data) > 1) {
  response = as.integer(readline("Starting from 1, enter the position of the response variable: "))
  descriptive_stats(data, response)
  normality(data, response)
  
  empty_model = lm(as.formula(paste(names(data)[response], " ~ 1")), data)
  print(summary(
    empty_model,
  ))
  print(anova(empty_model))
  pause()
  
  full_model = lm(as.formula(paste(names(data)[response], " ~ .^2")), data)
  print(summary(
    full_model
  ))
  print(anova(full_model))
  pause()
  
  subset_model = step(
    empty_model,
    scope = formula(full_model),
    direction = "both",
    trace = 0
  )
  print(summary(
    subset_model,
    correlation = T,
    symbolic.cor = F,
    show.signif.stars = T
  ))
  print(anova(subset_model))
  pause()
  
  remove(response, empty_model, full_model, subset_model)
}

remove(pause, normality, descriptive_stats, data)
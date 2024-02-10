if (!require(car)) {
  install.packages("car")
}
if (!require(rstatix)) {
  install.packages("rstatix")
}
library(rstatix)
library(car)

# Clear the output file initially
cat("", file = output_file)

# Function to print and save results to a file
print_and_save <-
  function(message) {
    cat(message, "\n\n", file = output_file, append = TRUE)
  }

# Function to print and save table to a file
print_and_save_table <- function(data, caption) {
  # Convert the data to a string
  data_string <- capture.output(print(data))
  
  write(caption, file = output_file, append = TRUE, sep = "\n")
  
  # Save to file
  if (file.exists(output_file)) {
    write(data_string, file = output_file, append = TRUE, sep = "\n")
  } else {
    write(data_string, file = output_file, sep = "\n")
  }
  
  cat(data_string, "\n\n")
}

# Function to perform and summarise Shapiro-Wilk test
perform_shapiro_test <- function(data, variable_name) {
  result <- shapiro.test(data)
  message <- paste(
    "Shapiro-Wilk normality test for",
    variable_name,
    "\n",
    "W =",
    round(result$statistic, 5),
    "p-value =",
    round(result$p.value, 5)
  )
  print_and_save(message)
}

# Function to perform and summarise Levene's test
perform_levene_test <- function(data, formula) {
  result <- leveneTest(formula, data = data)
  
  # Extract F value and p-value from the first row
  f_value <- result$`F value`[1]
  p_value <- result$`Pr(>F)`[1]
  
  message <- paste(
    "Levene's Test for Homogeneity of Variance\n",
    "F value:",
    round(f_value, 5),
    "p-value:",
    round(p_value, 5)
  )
  print_and_save(message)
}

# Function to perform and summarise the independent samples t-test
perform_t_test <- function(data, formula, variable_name) {
  result <- t.test(formula, data = data, var.equal = TRUE)
  message <- paste(
    "Two Sample t-test for",
    variable_name,
    "\nt =",
    round(result$statistic, 5),
    "df =",
    result$parameter,
    "p-value =",
    round(result$p.value, 5),
    "\n95% CI:",
    toString(round(result$conf.int, 5)),
    "\nMean Estimates:",
    toString(round(result$estimate, 5))
  )
  print_and_save(message)
}

# Calculate effect size (Cohen's d) for the independent samples T-test
cohens_d <- function(x, y, variable_name) {
  nx <- length(x)
  ny <- length(y)
  pooled_sd <- sqrt(((nx - 1) * sd(x)^2 + (ny - 1) * sd(y)^2) / (nx + ny - 2))
  d_value <- (mean(x) - mean(y)) / pooled_sd
  # Print and save the Cohen's d result
  message <- paste(
    "Cohen's d Effect Size for ",
    variable_name,
    "- d_value =",
    d_value
  )
  print_and_save(message)
}

perform_paired_t_test <- function(data, formula, variable_name) {
  result <- t.test(formula, data = data, paired = TRUE)
  message <- paste(
    "Paired Samples t-test for",
    variable_name,
    "\nt =",
    round(result$statistic, 5),
    "df =",
    result$parameter,
    "p-value =",
    round(result$p.value, 5),
    "\n95% CI:",
    toString(round(result$conf.int, 5)),
    "\nMean Estimates:",
    toString(round(result$estimate, 5))
  )
  print_and_save(message)
}

perform_cohens_d_paired <- function(before, after, variable_name) {
  # Calculate the differences
  differences <- after - before
  
  # Calculate the mean and standard deviation of the differences
  mean_diff <- mean(differences)
  sd_diff <- sd(differences)
  
  # Calculate Cohen's d
  d_value <- mean_diff / sd_diff
  
  # Prepare and print the message
  message <- paste(
    "Cohen's d for paired samples (",
    variable_name,
    ") - d_value =",
    round(d_value, 5)
  )
  print_and_save(message)
}

perform_MANOVA_test <- function(data, formula, variable_name) {
  manova_results <- manova(formula, data = data)
  manova_summary <- capture.output(print(summary(manova_results)))
  message <- paste(
    "MANOVA Test for ",
    variable_name,
    "- Summary\n",
    paste(manova_summary, collapse = "\n")
  )
  print_and_save(message)
}

# Non-Parametric alternative to independent samples T-Test
perform_wilcox_rank_sum_test <- function(data, formula, variable_name) {
  wilcox_rank_sum_result <- wilcox.test(formula, data = data, exact = FALSE, correct = TRUE)
  message <- paste(
    "Wilcoxon Rank Sum for ",
    variable_name,
    "- p-value =",
    round(wilcox_rank_sum_result$p.value, 5),
    "\n(approximation due to ties)"
  )
  print_and_save(message)
}

# The Non-parametric alternative to the paired sample T-test
perform_wilcoxon_signed_rank_test <- function(response1, response2, variable_name) {
  result <- wilcox.test(response1, response2, paired = TRUE)

  # Check for number of ties and zeroes
  num_ties <- sum(duplicated(response1 - response2))
  num_zeroes <- sum(response1 - response2 == 0)

  message <- paste(
    "Wilcoxon Signed-Rank Test for", variable_name, "- p-value =", result$p.value,
    "\nNumber of ties in the data:", num_ties,
    "\nNumber of zero differences:", num_zeroes
  )
  print_and_save(message)
}

# Function to calculate Wilcoxon effect size (r)
calculate_wilcoxon_effect_size <- function(data, formula) {
  # Calculate the Wilcoxon effect size
  effect_size_result <- wilcox_effsize(
    data = data,
    formula = formula,
    paired = TRUE
  )
  
  # Extract the effect size
  effect_size <- effect_size_result$effsize
  
  # Print and save the effect size
  print_and_save(paste("Wilcoxon effect size (r): ", effect_size))
}

# Function to calculate descriptive statistics
calculate_stats <- function(data, variable_name, condition_name) {
  mean_val <- mean(data, na.rm = TRUE)
  median_val <- median(data, na.rm = TRUE)
  iqr_val <- IQR(data, na.rm = TRUE)
  sd_val <- sd(data, na.rm = TRUE)
  
  result <- paste("\nDescriptive Statistics for", condition_name, "(" , variable_name, "):\n",
                  "Mean: ", mean_val, "\n",
                  "Median: ", median_val, "\n",
                  "IQR: ", iqr_val, "\n",
                  "Standard Deviation: ", sd_val)
  print_and_save(result)
}
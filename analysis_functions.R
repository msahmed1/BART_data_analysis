# Clear the output file initially
cat("", file = output_file)

# Function to print and save results to a file
print_and_save <-
  function(message) {
    cat(message, "\n\n", file = output_file, append = TRUE)
    cat(message, "\n\n")
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

# The Non-parametric alternative to the paired sample T-test
perform_wilcoxon_signed_rank_test <- function(response1, response2, variable_name) {
  result <- wilcox.test(response1, response2, paired = TRUE)
  message <- paste(
    "Wilcoxon Signed-Rank Test for ",
    variable_name,
    "- p-value =",
    result$p.value
  )
  print_and_save(message)
}

# Function to calculate Vargha and Delaney's A
calculate_vd_a <- function(data, group_column, value_column) {
  # Rank the data
  ranked_data <- rank(data[[value_column]])
  data$ranked_data <- ranked_data
  
  # Calculate the sum of ranks for one group
  sum_ranks_group1 <- sum(data$ranked_data[data[[group_column]] == 1])
  
  # Number of observations in each group
  n1 <- sum(data[[group_column]] == 1)
  n2 <- sum(data[[group_column]] == 0)
  
  # Calculate Vargha and Delaney's A
  A <- (sum_ranks_group1 - (n1*(n1+1)/2)) / (n1*n2)
  print_and_save(paste("Vargha and Delaney's A: ", A))
}

# Function to calculate Common Language Effect Size (CLES)
calculate_cles <- function(data, formula, group_column) {
  # Perform the Wilcoxon rank sum test
  wilcox_test <- wilcox.test(formula, data = data)
  
  # Extract group sizes
  n1 <- sum(data[[group_column]] == unique(data[[group_column]])[1])
  n2 <- sum(data[[group_column]] == unique(data[[group_column]])[2])
  
  # Calculate U statistic and CLES
  if (n1 > 0 && n2 > 0) {
    U <- wilcox_test$statistic
    CLES <- U / (n1 * n2)
    print_and_save(paste("CLES: ", CLES))
  } else {
    print_and_save("Invalid group sizes for CLES calculation.")
  }
}

perform_wilcoxon_signed_rank_cohens_d <- function(response1, response2, variable_name) {
  test_result <- wilcox.test(response1, response2, paired = TRUE)
  N <- length(na.omit(c(response1, response2))) # Total number of observations without NA
  Z <- qnorm(test_result$p.value / 2)
  d_value <- abs(Z / sqrt(N))
  
  message <- paste(
    "Cohen's d Effect Size for ",
    variable_name,
    "- d_value =",
    d_value
  )
  print_and_save(message)
}

# The non-parametric alternative to the one-way ANOVA
perform_kruskal_wallis_test <- function(data, formula, variable_name) {
  result <- kruskal.test(formula, data = data)
  message <- paste(
    "Kruskal-Wallis Test for ",
    variable_name,
    "- p-value =",
    result$p.value
  )
  print_and_save(message)
}

# Function to calculate descriptive statistics
calculate_stats <- function(data, variable_name, condition_name) {
  mean_val <- mean(data, na.rm = TRUE)
  median_val <- median(data, na.rm = TRUE)
  iqr_val <- IQR(data, na.rm = TRUE)
  sd_val <- sd(data, na.rm = TRUE)
  
  result <- paste("Descriptive Statistics for", condition_name, "(" , variable_name, "):\n",
                  "Mean: ", mean_val, "\n",
                  "Median: ", median_val, "\n",
                  "IQR: ", iqr_val, "\n",
                  "Standard Deviation: ", sd_val)
  print_and_save(result)
}
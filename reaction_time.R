setwd(getSrcDirectory(function() {})[1])

if (!require(DBI)) {
  install.packages("DBI")
}
if (!require(RSQLite)) {
  install.packages("RSQLite")
}
if (!require(tidyr)) {
  install.packages("tidyr")
}
library(DBI)
library(RSQLite)
library(tidyr)

# File to save output
output_file <- "reaction_time_results.txt"

source("./analysis_functions.R")

# Set up connection to the SQLite database
db_path <- "./participant.db"
conn <- dbConnect(RSQLite::SQLite(), dbname = db_path)

################################################################################
print_and_save(
  "#### Is there a difference in median reaction time between robot responses? ####"
)
################################################################################

reaction_time_query <- "
  SELECT
    balloon_inflate.player_id,
    players.customise_first AS study_cond,
    robot_response,
    time_to_decide AS reaction_time
  FROM
    balloon_inflate
  JOIN
    players ON balloon_inflate.player_id = players.player_id
  WHERE
    players.testing = 0
    AND players.game_completed = 1
    AND game_round = 2
    AND balloon_inflate.balloon_id > 5
    AND help_requested = 1
"

# Fetch reaction time data
reaction_time_data <- dbGetQuery(conn, reaction_time_query)


# Calculate median reaction time for each group
median_time_data <- aggregate(reaction_time ~ player_id + study_cond + robot_response,
  data = reaction_time_data,
  FUN = median
)
# Rename the aggregated column reaction_time to "median_reaction_time"
names(median_time_data)[4] <- "median_reaction_time"

# Create a table where robot_response = 1 i.e. exclude all robot_response = 0
reaction_time_for_inflate_request <- subset(median_time_data, robot_response == 1)

# For Inflate Request - Non-customise study condition
rt_non_cust_inflate_requested <- subset(reaction_time_for_inflate_request, study_cond == 0)
mean_inflate_0 <- mean(rt_non_cust_inflate_requested$median_reaction_time)
sd_inflate_0 <- sd(rt_non_cust_inflate_requested$median_reaction_time)
rt_non_cust_inflate_requested_filtered <- subset(rt_non_cust_inflate_requested, median_reaction_time >= (mean_inflate_0 - 2 * sd_inflate_0) & median_reaction_time <= (mean_inflate_0 + 2 * sd_inflate_0))

# For inflate Request - Customise study condition
rt_cust_inflate_requested <- subset(reaction_time_for_inflate_request, study_cond == 1)
mean_inflate_1 <- mean(rt_cust_inflate_requested$median_reaction_time)
sd_inflate_1 <- sd(rt_cust_inflate_requested$median_reaction_time)
rt_cust_inflate_requested_filtered <- subset(rt_cust_inflate_requested, median_reaction_time >= (mean_inflate_1 - 2 * sd_inflate_1) & median_reaction_time <= (mean_inflate_1 + 2 * sd_inflate_1))

# Create a table where robot_response = 0 i.e. exclude all robot_response = 1
reaction_time_for_collect_request <- subset(median_time_data, robot_response == 0)

# For Collect Request - Non-customise study condition
rt_non_cust_collect_requested <- subset(reaction_time_for_collect_request, study_cond == 0)
mean_collect_0 <- mean(rt_non_cust_collect_requested$median_reaction_time)
sd_collect_0 <- sd(rt_non_cust_collect_requested$median_reaction_time)
rt_non_cust_collect_requested_filtered <- subset(rt_non_cust_collect_requested, median_reaction_time >= (mean_collect_0 - 2 * sd_collect_0) & median_reaction_time <= (mean_collect_0 + 2 * sd_collect_0))

# For Collect Request - Customise study condition
rt_cust_collect_requested <- subset(reaction_time_for_collect_request, study_cond == 1)
mean_collect_1 <- mean(rt_cust_collect_requested$median_reaction_time)
sd_collect_1 <- sd(rt_cust_collect_requested$median_reaction_time)
rt_cust_collect_requested_filtered <- subset(rt_cust_collect_requested, median_reaction_time >= (mean_collect_1 - 2 * sd_collect_1) & median_reaction_time <= (mean_collect_1 + 2 * sd_collect_1))

# Find excluded participants for each study conditions
excluded_rt_cust_inflate_requested <- setdiff(rt_cust_inflate_requested$player_id, rt_cust_inflate_requested_filtered$player_id)
excluded_rt_cust_collect_requested <- setdiff(rt_cust_collect_requested$player_id, rt_cust_collect_requested_filtered$player_id)

excluded_rt_non_cust_inflate_requested <- setdiff(rt_non_cust_inflate_requested$player_id, rt_non_cust_inflate_requested_filtered$player_id)
excluded_rt_non_cust_collect_requested <- setdiff(rt_non_cust_collect_requested$player_id, rt_non_cust_collect_requested_filtered$player_id)

# Combine excluded participant lists for each set of conditions
all_excluded_ppt_cust_cond <- unique(c(excluded_rt_cust_inflate_requested, excluded_rt_cust_collect_requested))
all_excluded_ppt_non_cust_cond <- unique(c(excluded_rt_non_cust_inflate_requested, excluded_rt_non_cust_collect_requested))

# Exclude these participants from respective datasets
rt_cust_inflate_requested_filtered <- rt_cust_inflate_requested_filtered[!rt_cust_inflate_requested_filtered$player_id %in% all_excluded_ppt_cust_cond, ]
rt_cust_collect_requested_filtered <- rt_cust_collect_requested_filtered[!rt_cust_collect_requested_filtered$player_id %in% all_excluded_ppt_cust_cond, ]

rt_non_cust_inflate_requested_filtered <- rt_non_cust_inflate_requested_filtered[!rt_non_cust_inflate_requested_filtered$player_id %in% all_excluded_ppt_non_cust_cond, ]
rt_non_cust_collect_requested_filtered <- rt_non_cust_collect_requested_filtered[!rt_non_cust_collect_requested_filtered$player_id %in% all_excluded_ppt_non_cust_cond, ]

# Test for Normality and Homogeneity
perform_shapiro_test(
  rt_non_cust_inflate_requested_filtered$median_reaction_time,
  "Shapiro-Wilk for Inflate Request, Study Condition 0"
)

perform_shapiro_test(
  rt_cust_inflate_requested_filtered$median_reaction_time,
  "Shapiro-Wilk for Inflate Request, Study Condition 1"
)

inflate_request_combined <- rbind(rt_non_cust_inflate_requested_filtered, rt_cust_inflate_requested_filtered)

inflate_request_combined$study_cond <- as.factor(inflate_request_combined$study_cond)

# Levene's Test for Inflate Request
perform_levene_test(
  data = inflate_request_combined,
  formula = median_reaction_time ~ study_cond
)

perform_shapiro_test(
  rt_cust_collect_requested_filtered$median_reaction_time,
  "Shapiro-Wilk for Collect Request, Study Condition 1"
)

perform_shapiro_test(
  rt_non_cust_collect_requested_filtered$median_reaction_time,
  "Shapiro-Wilk for Collect Request, Study Condition 0"
)

qqPlot(rt_cust_collect_requested_filtered$median_reaction_time)

collect_request_combined <- rbind(rt_non_cust_collect_requested_filtered, rt_cust_collect_requested_filtered)

collect_request_combined$study_cond <- as.factor(collect_request_combined$study_cond)

# Levene's Test for Inflate Request
perform_levene_test(
  data = collect_request_combined,
  formula = median_reaction_time ~ study_cond
)

combined_filtered_reaction_time_data <- rbind(inflate_request_combined, collect_request_combined)

combined_filtered_reaction_time_data$robot_response <- as.factor(combined_filtered_reaction_time_data$robot_response)

print_and_save(
  "################################ Parametric test ################################" 
)

# Reshape data into wide format
wide_data <- pivot_wider(
  data = combined_filtered_reaction_time_data,
  names_from = robot_response,
  values_from = median_reaction_time,
  names_prefix = "response_"
)

# Convert study_cond to a factor
wide_data$study_cond <- as.factor(wide_data$study_cond)

# Homogeneity of covariances assumption
# Perform Box's M test
box_m_result <- box_m(wide_data[, c("response_1", "response_0")], wide_data$study_cond)

# Check the results
print(box_m_result)

# Perform the repeated measures ANOVA
res.aov <- anova_test(
  data = combined_filtered_reaction_time_data, # your long-format dataset
  dv = median_reaction_time, # dependent variable
  wid = player_id, # subject identifier
  between = study_cond, # between-subjects factor
  within = robot_response # within-subjects factor
)

# Get the ANOVA table
anova_table <- get_anova_table(res.aov)
print_and_save_table(anova_table, "\nTable 1: Mixed ANOVA ")

pwc <- combined_filtered_reaction_time_data %>%
  group_by(robot_response) %>%
  pairwise_t_test(median_reaction_time ~ study_cond, p.adjust.method = "bonferroni")
print_and_save_table(pwc, "\nTable 2: Pairwise Comparisons by Robot Response")

pwc2 <- combined_filtered_reaction_time_data %>%
  group_by(study_cond) %>%
  pairwise_t_test(
    median_reaction_time ~ robot_response,
    paired = TRUE,
    p.adjust.method = "bonferroni"
  ) %>%
  select(-df, -statistic, -p) # Remove details
print_and_save_table(pwc2, "\nTable 3: Pairwise Comparisons by Study Condition")

print_and_save(
  "\n############################ Descriptive statistics ############################" 
)

# Calculate and save statistics for each condition
calculate_stats(rt_cust_collect_requested_filtered$median_reaction_time, "Collect Request", "Customisation Condition")
calculate_stats(rt_non_cust_collect_requested_filtered$median_reaction_time, "Collect Request", "Non-customisation Condition")

# Count the number of participants after filtering for inflate request
num_participants_inflate <- nrow(inflate_request_combined)
print_and_save(paste("Number of participants remaining for inflate request (Robot Response 1):", num_participants_inflate))

# Count the number of participants after filtering for collect request
num_participants_collect <- nrow(collect_request_combined)
print_and_save(paste("Number of participants remaining for collect request (Robot Response 0):", num_participants_collect))

# Ensure the data is in the correct format and add descriptive labels
combined_filtered_reaction_time_data$study_cond_label <- ifelse(combined_filtered_reaction_time_data$study_cond == 0, "Non-Customise", "Customise")
combined_filtered_reaction_time_data$robot_response_label <- ifelse(combined_filtered_reaction_time_data$robot_response == 0, "Collect", "Inflate")

# Combine the labels for a comprehensive y-axis label
combined_filtered_reaction_time_data$combined_label <- paste(combined_filtered_reaction_time_data$robot_response_label, combined_filtered_reaction_time_data$study_cond_label, sep = " - ")

# Create the plot
ggplot(combined_filtered_reaction_time_data, aes(x = median_reaction_time, y = combined_label, fill = study_cond_label)) +
  geom_density_ridges(alpha = 0.4, scale = 0.6) +
  geom_boxplot(aes(y = combined_label, x = median_reaction_time), width = 0.2, alpha = 0.3) +
  labs(
    title = "Median Reaction Time by Study Condition and Robot Response",
    x = "Median Reaction Time (seconds)",
    y = "Condition"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "right"
  ) +
  coord_flip()

# Close the database connection
dbDisconnect(conn)
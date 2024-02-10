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
if (!require(ggplot2)) {
  install.packages("ggplot2")
}
if (!require(dplyr)) {
  install.packages("dplyr")
}
library(DBI)
library(RSQLite)
library(tidyr)
library(ggplot2)
library(dplyr)

# File to save output
output_file <- "compliance_behaviour_results.txt"

source("./analysis_functions.R")

# Set up connection to the SQLite database
db_path <- "./participant.db"
conn <- dbConnect(RSQLite::SQLite(), dbname = db_path)

################################################################################
print_and_save(
  "######### Is there a difference in compliance between study conditions? #########"
)
################################################################################

compliance_query <- "
  SELECT
    balloon_inflate.player_id,
    players.customise_first AS study_cond,
    robot_response,
    (CASE
      WHEN (robot_response = 1 AND inflated_after_help_request > 0)
        OR (robot_response = 0 AND inflated_after_help_request = 0)
      THEN 1 ELSE 0 END) AS compliance
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

# Fetch compliance data
compliance_data <- dbGetQuery(conn, compliance_query)

# Sum the values in the compliance column for each unique combination of player_id, study_cond and robot_response
total_compliance <- aggregate(compliance ~ player_id + study_cond + robot_response,
                              data = compliance_data,
                              FUN = sum
)
# The resulting column with summed values is automatically named 'compliance', but it represents total compliance, re-name it to "total_compliance"
names(total_compliance)[4] <- "total_compliance"

# Add a new column 'count_response' with a default value of 0
compliance_data$count_response <- 0
# Count the number of rows for each unique combination of player_id, study_cond and robot_response
count_responses <- aggregate(count_response ~ player_id + study_cond + robot_response,
                             data = compliance_data,
                             FUN = length
)

# Merge total_compliance and count_responses tables
normalised_compliance_data <- merge(total_compliance, count_responses,
                                    by = c("player_id", "study_cond", "robot_response")
)

# Calculate normalised compliance and store it in the "normalised_compliance" column
normalised_compliance_data$normalised_compliance <- normalised_compliance_data$total_compliance / normalised_compliance_data$count_response

# Subset the data for when robot_response is 0 (collect request)
collect_request_data <- subset(normalised_compliance_data, robot_response == 0)

# Subset the data for when robot_response is 1 (inflate request) i.e. don't include data for robot_response == 0
inflate_request_data <- subset(normalised_compliance_data, robot_response == 1)
collect_request_data <- subset(normalised_compliance_data, robot_response == 0)

combined_overall_game_data <- rbind(inflate_request_data, collect_request_data)

combined_overall_game_data$study_cond <- as.factor(combined_overall_game_data$study_cond)


inflate_non_cust_data <- subset(inflate_request_data, study_cond == 0)
inflate_cust_data <- subset(inflate_request_data, study_cond == 1)

# Combine the data frames and add a column to distinguish the datasets
inflate_non_cust_data$dataset <- 'Non-Customise'
inflate_cust_data$dataset <- 'Customise'
combined_data <- rbind(inflate_non_cust_data, inflate_cust_data)

# Convert the 'dataset' column to a factor for color coding
combined_data$dataset <- as.factor(combined_data$dataset)

# Create the Q-Q plot
ggplot(combined_data, aes(sample = normalised_compliance, colour = dataset)) +
  stat_qq(size = 4) +
  geom_qq_line() +
  labs(x = "Theoretical Quantiles",
       y = "Sample Quantiles",
       colour = "Study Condition") +
  theme_minimal() +
  scale_colour_manual(values = c("Non-Customise" = "blue", "Customise" = "red")) +

perform_shapiro_test(
  collect_request_data$normalised_compliance,
  "normalised_compliance_data for collect req (Robot Response 0)"
)

collect_request_data_study_0 <- subset(collect_request_data, study_cond == 0)
perform_shapiro_test(
  collect_request_data_study_0$normalised_compliance,
  "collect_request_data for colect req (Robot Response 0) in non-custum cond. (study_cond = 0)"
)

collect_request_data_study_1 <- subset(collect_request_data, study_cond == 1)
perform_shapiro_test(
  collect_request_data_study_1$normalised_compliance,
  "collect_request_data for collect req (Robot Response 0) in custum cond. (study_cond = 1)"
)

collect_non_cust_data <- subset(collect_request_data, study_cond == 0)
collect_cust_data <- subset(collect_request_data, study_cond == 1)

# Combine the data frames and add a column to distinguish the datasets
collect_non_cust_data$dataset <- 'Non-Custom'
collect_cust_data$dataset <- 'Custom'
combined_data <- rbind(collect_non_cust_data, collect_cust_data)

# Convert the 'dataset' column to a factor for color coding
combined_data$dataset <- as.factor(combined_data$dataset)

# Create the Q-Q plot
ggplot(combined_data, aes(sample = normalised_compliance, colour = dataset)) +
  stat_qq(size = 4) +
  geom_qq_line() +
  labs(x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal() +
  scale_colour_manual(values = c("Non-Custom" = "blue", "Custom" = "red"))

collect_request_data$study_cond <- as.factor(collect_request_data$study_cond)

perform_wilcoxon_signed_rank_test(
  collect_request_data$normalised_compliance,
  inflate_request_data$normalised_compliance,
  "normalised Compliance based on robot request regardless of study condition"
)

calculate_wilcoxon_effect_size(
  data = combined_overall_game_data,
  formula = normalised_compliance ~ robot_response
)

# Ensure the data is in the correct format and add descriptive labels
combined_overall_game_data <- combined_overall_game_data %>%
mutate(study_cond = as.factor(ifelse(study_cond == 0, "Non-Customise", "Customise")),
       robot_response = as.factor(ifelse(robot_response == 0, "Collect", "Inflate")))

# Calculate means and SEM
grouped_data <- combined_overall_game_data %>%
  group_by(study_cond, robot_response) %>%
  summarise(mean = mean(normalised_compliance),
            sem = sd(normalised_compliance) / sqrt(n()), .groups = 'drop')

# Set the position for dodge (adjust width as needed)
dodge <- position_dodge(width = 0.8)

# Create the bar plot with error bars
ggplot(grouped_data, aes(x = robot_response, y = mean, fill = study_cond)) +
  geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem, group = study_cond),
                width = 0.25, position = dodge) +
  labs(x = "Robot Response",
       y = "Normalised Compliance",
       fill = "Study Condition") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(labels = c("Collect", "Inflate"))
  
################################################################################
print_and_save(
  "\n\n### Is there a difference in inflammation behaviours after receiving a suggestion? ###"
)
################################################################################

avf_inflates_after_receiving_suggestion_query <- "
  SELECT
    balloon_inflate.player_id,
    players.customise_first AS study_cond,
    robot_response,
    AVG(total_inflates - total_inflates_before_help_request) AS avg_inflate_after_help
  FROM
    balloon_inflate
  JOIN
    players ON balloon_inflate.player_id = players.player_id
  WHERE
    players.testing = 0
    AND players.game_completed = 1
    AND game_round = 2
    AND help_requested = 1
    AND balloon_inflate.balloon_id > 5
  GROUP BY
    balloon_inflate.player_id,
    players.customise_first,
    robot_response
  "

# Fetch data
avg_inflates_after_suggestion <- dbGetQuery(conn, avf_inflates_after_receiving_suggestion_query)

# Create a table where robot_response = 1 i.e. exclude all robot_response = 0
avg_inflates_for_inflate_request <- subset(avg_inflates_after_suggestion, robot_response == 1)

# For Inflate Request - Non-customise study condition
non_cust_inflate_requested <- subset(avg_inflates_for_inflate_request, study_cond == 0)
mean_non_cust_inflate_requested <- mean(non_cust_inflate_requested$avg_inflate_after_help)
sd_non_cust_inflate_requested <- sd(non_cust_inflate_requested$avg_inflate_after_help)
non_cust_inflate_requested_filtered <- subset(non_cust_inflate_requested, avg_inflate_after_help >= (mean_non_cust_inflate_requested - 2 * sd_non_cust_inflate_requested) & avg_inflate_after_help <= (mean_non_cust_inflate_requested + 2 * sd_non_cust_inflate_requested))

# For inflate Request - Customise study condition
cust_inflate_requested <- subset(avg_inflates_for_inflate_request, study_cond == 1)
mean_cust_inflate_requested <- mean(cust_inflate_requested$avg_inflate_after_help)
sd_cust_inflate_requested <- sd(cust_inflate_requested$avg_inflate_after_help)
cust_inflate_requested_filtered <- subset(cust_inflate_requested, avg_inflate_after_help >= (mean_cust_inflate_requested - 2 * sd_cust_inflate_requested) & avg_inflate_after_help <= (mean_cust_inflate_requested + 2 * sd_cust_inflate_requested))

# Create a table where robot_response = 0 i.e. exclude all robot_response = 1
avg_inflates_for_collect_request <- subset(avg_inflates_after_suggestion, robot_response == 0)

# For Collect Request - Non-customise study condition
non_cust_collect_requested <- subset(avg_inflates_for_collect_request, study_cond == 0)
mean_non_cust_collect_requested <- mean(non_cust_collect_requested$avg_inflate_after_help)
sd_non_cust_collect_requested <- sd(non_cust_collect_requested$avg_inflate_after_help)
non_cust_collect_requested_filtered <- subset(non_cust_collect_requested, avg_inflate_after_help >= (mean_non_cust_collect_requested - 2 * sd_non_cust_collect_requested) & avg_inflate_after_help <= (mean_non_cust_collect_requested + 2 * sd_non_cust_collect_requested))

# For Collect Request - Customise study condition
cust_collect_requested <- subset(avg_inflates_for_collect_request, study_cond == 1)
mean_cust_collect_requested <- mean(cust_collect_requested$avg_inflate_after_help)
sd_cust_collect_requested <- sd(cust_collect_requested$avg_inflate_after_help)
cust_collect_requested_filtered <- subset(cust_collect_requested, avg_inflate_after_help >= (mean_cust_collect_requested - 2 * sd_cust_collect_requested) & avg_inflate_after_help <= (mean_cust_collect_requested + 2 * sd_cust_collect_requested))

# Find excluded participants for each study conditions
excluded_cust_inflate_requested <- setdiff(non_cust_inflate_requested$player_id, non_cust_inflate_requested_filtered$player_id)
excluded_cust_collect_requested <- setdiff(cust_inflate_requested$player_id, cust_inflate_requested_filtered$player_id)

excluded_non_cust_inflate_requested <- setdiff(non_cust_collect_requested$player_id, non_cust_collect_requested_filtered$player_id)
excluded_non_cust_collect_requested <- setdiff(cust_collect_requested$player_id, cust_collect_requested_filtered$player_id)

# Combine excluded participant lists for each set of conditions
all_excluded_ppt_cust_cond <- unique(c(excluded_cust_inflate_requested, excluded_cust_collect_requested))
all_excluded_ppt_non_cust_cond <- unique(c(excluded_non_cust_inflate_requested, excluded_non_cust_collect_requested))

all_excluded_ppt <- rbind(all_excluded_ppt_cust_cond, all_excluded_ppt_non_cust_cond)

# Exclude these participants from respective datasets
cust_inflate_requested_filtered <- cust_inflate_requested_filtered[!cust_inflate_requested_filtered$player_id %in% all_excluded_ppt, ]
cust_collect_requested_filtered <- cust_collect_requested_filtered[!cust_collect_requested_filtered$player_id %in% all_excluded_ppt, ]

non_cust_inflate_requested_filtered <- non_cust_inflate_requested_filtered[!non_cust_inflate_requested_filtered$player_id %in% all_excluded_ppt, ]
non_cust_collect_requested_filtered <- non_cust_collect_requested_filtered[!non_cust_collect_requested_filtered$player_id %in% all_excluded_ppt, ]

# Test for Normality and Homogeneity
perform_shapiro_test(
  non_cust_inflate_requested_filtered$avg_inflate_after_help,
  "Shapiro-Wilk for Inflate Request, Study Condition 0"
)

# qqPlot(non_cust_inflate_requested_filtered$avg_inflate_after_help)

perform_shapiro_test(
  cust_inflate_requested_filtered$avg_inflate_after_help,
  "Shapiro-Wilk for Inflate Request, Study Condition 1"
)

inflate_request_combined <- rbind(non_cust_inflate_requested_filtered, cust_inflate_requested_filtered)

inflate_request_combined$study_cond <- as.factor(inflate_request_combined$study_cond)

perform_shapiro_test(
  cust_collect_requested_filtered$avg_inflate_after_help,
  "Shapiro-Wilk for Collect Request, Study Condition 1"
)

perform_shapiro_test(
  non_cust_collect_requested_filtered$avg_inflate_after_help,
  "Shapiro-Wilk for Collect Request, Study Condition 0"
)

# qqPlot(non_cust_collect_requested_filtered$avg_inflate_after_help)

collect_request_combined <- rbind(non_cust_collect_requested_filtered, cust_collect_requested_filtered)

collect_request_combined$study_cond <- as.factor(collect_request_combined$study_cond)

# Levene's Test for Inflate Request
perform_levene_test(
  data = collect_request_combined,
  formula = avg_inflate_after_help ~ study_cond
)

combined_filtered_inflation_data <- rbind(inflate_request_combined, collect_request_combined)

combined_filtered_inflation_data$robot_response <- as.factor(combined_filtered_inflation_data$robot_response)

# collect_request_combined
# generate a raincloud plot without displaing the data points
ggplot(collect_request_combined, aes(x = avg_inflate_after_help, y = factor(study_cond), fill = factor(study_cond))) +
  geom_density_ridges(alpha = 0.4, scale = 0.6) +
  geom_boxplot(aes(y = factor(study_cond), x = avg_inflate_after_help), width = 0.2, alpha = 0.3) +
  geom_jitter(height=0.1, size=1, alpha=0.5, color="black") +
  labs(
    x = "avg inflates after help requested",
    y = "Study Condition"
  ) +
  theme_minimal() +
  theme(legend.position = "none", plot.margin = margin(t = 0.5, r = 0.1, b = 0.1, l = 0.5, unit = "cm")) +
  coord_flip() +
  scale_y_discrete(expand = c(0, 0), labels = c("non-custom", "custom"))


# Add two new column to convert numeric codes to descriptive labels
combined_filtered_inflation_data <- combined_filtered_inflation_data %>%
  mutate(study_cond_label = ifelse(study_cond == 0, "Non-Customise", "Customise"),
         robot_response_label = ifelse(robot_response == 0, "Collect", "Inflate"))

# Calculate means and SEM
grouped_data <- combined_filtered_inflation_data %>%
  group_by(study_cond_label, robot_response_label) %>%
  summarise(mean = mean(avg_inflate_after_help),
            sem = sd(avg_inflate_after_help) / sqrt(n()), .groups = 'drop')

# Set the position for dodge (adjust width as needed)
dodge <- position_dodge(width = 0.8)

# Create the bar plot with error bars
ggplot(grouped_data, aes(x = robot_response_label, y = mean, fill = study_cond_label)) +
  geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem, group = study_cond_label),
                width = 0.25, position = dodge) +
  labs(x = "Robot Response",
       y = "Average Inflates After Help",
       fill = "Study Condition") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(labels = c("Collect", "Inflate"))

print_and_save(
  "################################ Parametric test ################################" 
)

# Perform the repeated measures ANOVA
res.aov <- anova_test(
  data = combined_filtered_inflation_data, # your long-format dataset
  dv = avg_inflate_after_help, # dependent variable
  wid = player_id, # subject identifier
  between = study_cond, # between-subjects factor
  within = robot_response # within-subjects factor
)

# Get the ANOVA table
anova_table <- get_anova_table(res.aov)
print_and_save_table(anova_table, "\nTable 1: Mixed ANOVA ")

pwc_robot_response <- combined_filtered_inflation_data %>%
  group_by(robot_response) %>%
  pairwise_t_test(avg_inflate_after_help ~ study_cond, p.adjust.method = "bonferroni")
print_and_save_table(pwc_robot_response, "\nTable 2: Pairwise Comparisons by Robot Response")

pwc_study_cond <- combined_filtered_inflation_data %>%
  group_by(study_cond) %>%
  pairwise_t_test(
    avg_inflate_after_help ~ robot_response,
    paired = TRUE,
    p.adjust.method = "bonferroni"
  ) %>%
  select(-df, -statistic, -p) # Remove details
print_and_save_table(pwc_study_cond, "\nTable 3: Pairwise Comparisons by Study Condition")

print_and_save(
  "\n############################## Non-parametric test ##############################" 
)

# I should do a shapiro wilks test between study conds

# Wilcoxon Test for difference in inflates between study conditions (collect requests)
perform_wilcox_rank_sum_test(
  data = combined_filtered_inflation_data,
  formula = avg_inflate_after_help ~ study_cond,
  "inflation behaviour between Study Conditions"
)

# Wilcoxon Test for difference in inflates between study conditions (inflate requests)
perform_wilcox_rank_sum_test(
  data = inflate_request_combined,
  formula = avg_inflate_after_help ~ study_cond,
  "inflation behaviour after help when the robot requests a inflate"
)

# Wilcoxon Test for difference in inflates between study conditions (collect requests)
perform_wilcox_rank_sum_test(
  data = collect_request_combined,
  formula = avg_inflate_after_help ~ study_cond,
  "inflation behaviour after help when the robot requests a collect"
)

print_and_save(
  "For the game played with the robot, is there a difference in how much the participant inflated the
  balloon after they received a inflates or collects request from the robot, regardless of the study condition"
)

# Wilcoxon Signed-Rank Test
perform_wilcoxon_signed_rank_test(
  inflate_request_combined$avg_inflate_after_help,
  collect_request_combined$avg_inflate_after_help,
  "inflation behaviour for based on the request made by the robot, regardless of study condition"
)

calculate_wilcoxon_effect_size(
  data = combined_filtered_inflation_data,
  formula = avg_inflate_after_help ~ robot_response
)

print_and_save(
  "############################ Descriptive statistics ############################" 
)

# Calculate and save statistics for each robot response
calculate_stats(inflate_request_combined$avg_inflate_after_help, "avg inflate", "Inflate request")
calculate_stats(collect_request_combined$avg_inflate_after_help, "avg inflate", "Collect request")

# Count the number of participants after filtering for inflate request
num_participants_inflate <- nrow(inflate_request_combined)
print_and_save(paste("Number of participants remaining for inflate request (Robot Response 1):", num_participants_inflate))

# Count the number of participants after filtering for collect request
num_participants_collect <- nrow(collect_request_combined)
print_and_save(paste("Number of participants remaining for collect request (Robot Response 0):", num_participants_collect))

# Close the database connection
dbDisconnect(conn)
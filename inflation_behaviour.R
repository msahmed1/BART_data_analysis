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
if (!require(rstatix)) {
  install.packages("rstatix")
}
library(DBI)
library(RSQLite)
library(tidyr)
library(rstatix)

# File to save output
output_file <- "inflation_behaviour_results.txt"

source("./analysis_functions.R")

# Set up connection to the SQLite database
db_path <- "./participant.db"
conn <- dbConnect(RSQLite::SQLite(), dbname = db_path)

################################################################################
print_and_save(
  "### Is there a difference in inflammation behaviours after receiving a suggestion? ###"
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

qqPlot(non_cust_inflate_requested_filtered$avg_inflate_after_help)

perform_shapiro_test(
  cust_inflate_requested_filtered$avg_inflate_after_help,
  "Shapiro-Wilk for Inflate Request, Study Condition 1"
)

inflate_request_combined <- rbind(non_cust_inflate_requested_filtered, cust_inflate_requested_filtered)

inflate_request_combined$study_cond <- as.factor(inflate_request_combined$study_cond)

# Levene's Test for Inflate Request
perform_levene_test(
  data = inflate_request_combined,
  formula = avg_inflate_after_help ~ study_cond
)

perform_shapiro_test(
  cust_collect_requested_filtered$avg_inflate_after_help,
  "Shapiro-Wilk for Collect Request, Study Condition 1"
)

perform_shapiro_test(
  non_cust_collect_requested_filtered$avg_inflate_after_help,
  "Shapiro-Wilk for Collect Request, Study Condition 0"
)

qqPlot(non_cust_collect_requested_filtered$avg_inflate_after_help)

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
    title = "Avg inflates for Inflate request by robot",
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
  labs(title = "Average Inflates by Study Condition and Robot Response",
       x = "Robot Response",
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
  \nballoon after they received a inflates or collects request from the robot, regardless of the study condition"
)

# Wilcoxon Signed-Rank Test
perform_mann_whitney(
  inflate_request_combined$avg_inflate_after_help,
  collect_request_combined$avg_inflate_after_help,
  "inflation behaviour for based on the request made by the robot, regardless of study condition"
)

# Find the effect size
perform_cliffs_delta(
  inflate_request_combined$avg_inflate_after_help,
  collect_request_combined$avg_inflate_after_help,
  "Effect size for the inflation behaviour for inflate vs. collect, when playing with robot"
)

print_and_save(
  "############################ Descriptive statistics ############################" 
)

# Calculate and save statistics for each condition
calculate_stats(cust_collect_requested_filtered$avg_inflate_after_help, "Collect Request", "Customisation Condition")
calculate_stats(non_cust_collect_requested_filtered$avg_inflate_after_help, "Collect Request", "Non-customisation Condition")

# Calculate and save statistics for each condition
calculate_stats(cust_inflate_requested_filtered$avg_inflate_after_help, "Inflate Request", "Customisation Condition")
calculate_stats(non_cust_inflate_requested_filtered$avg_inflate_after_help, "Inflate Request", "Non-customisation Condition")

# Count the number of participants after filtering for inflate request
num_participants_inflate <- nrow(inflate_request_combined)
print_and_save(paste("Number of participants remaining for inflate request (Robot Response 1):", num_participants_inflate))

# Count the number of participants after filtering for collect request
num_participants_collect <- nrow(collect_request_combined)
print_and_save(paste("Number of participants remaining for collect request (Robot Response 0):", num_participants_collect))


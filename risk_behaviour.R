# Set the working directory and include required packages
setwd(getSrcDirectory(function() {})[1])

if (!require(DBI)) {
  install.packages("DBI")
}
if (!require(RSQLite)) {
  install.packages("RSQLite")
}
if (!require(car)) {
  install.packages("car")
}
if (!require(ggplot2)) {
  install.packages("ggplot2")
}
if (!require(ggridges)) {
  install.packages("ggridges")
}
library(DBI)
library(RSQLite)
library(ggplot2)

# File to save output
output_file <- "risk_behaviour_results.txt"

source("./analysis_functions.R")

# Set up connection to the SQLite database
db_path <- "./participant.db"
conn <- dbConnect(RSQLite::SQLite(), dbname = db_path)

################################################################################
print_and_save(
  "######### Is there a difference in risk taking behaviour from baseline? ########"
)
################################################################################

average_inflates_query <- "
  SELECT
    balloon_inflate.player_id,
    game_round,
    players.customise_first AS study_cond,
    AVG(total_inflates) AS average_balloon_inflate_per_game
  FROM
    balloon_inflate
  JOIN
    players ON balloon_inflate.player_id = players.player_id
  WHERE
    players.testing = 0
    AND players.game_completed = 1
    AND balloon_inflate.balloon_id > 5
    AND total_inflates <= balloon_limit
  GROUP BY
    balloon_inflate.player_id,
    game_round
"

# Fetch averate_inflate data
inflates_data <- dbGetQuery(conn, average_inflates_query)

game_one <- subset(inflates_data, game_round == 1)
mean_game_one <- mean(game_one$average_balloon_inflate_per_game)
sd_game_one <- sd(game_one$average_balloon_inflate_per_game)
game_one_filtered <- subset(game_one, average_balloon_inflate_per_game >= (mean_game_one - 2 * sd_game_one) & average_balloon_inflate_per_game <= (mean_game_one + 2 * sd_game_one))
perform_shapiro_test(
  game_one_filtered$average_balloon_inflate_per_game,
  "average_balloon_inflate_per_game (Game Round 1)"
)

qqPlot(game_one_filtered$average_balloon_inflate_per_game)

game_two <- subset(inflates_data, game_round == 2)
mean_game_two <- mean(game_two$average_balloon_inflate_per_game)
sd_game_two <- sd(game_two$average_balloon_inflate_per_game)
game_two_filtered <- subset(game_two, average_balloon_inflate_per_game >= (mean_game_two - 2 * sd_game_two) & average_balloon_inflate_per_game <= (mean_game_two + 2 * sd_game_two))
perform_shapiro_test(
  game_two_filtered$average_balloon_inflate_per_game,
  "average_balloon_inflate_per_game (Game Round 2)"
)

qqPlot(game_two_filtered$average_balloon_inflate_per_game)

# Find excluded participants for each study conditions
excluded_game_one_ppt <- setdiff(game_one$player_id, game_one_filtered$player_id)
excluded_game_two_ppt <- setdiff(game_two$player_id, game_two_filtered$player_id)

# Combine excluded participant lists for each set of conditions
all_excluded_from_both_games_ppt <- unique(c(excluded_game_one_ppt, excluded_game_two_ppt))

# Exclude these participants from respective datasets
game_one_filtered <- game_one_filtered[!game_one_filtered$player_id %in% all_excluded_from_both_games_ppt, ]
game_two_filtered <- game_two_filtered[!game_two_filtered$player_id %in% all_excluded_from_both_games_ppt, ]

combined_overall_game_data <- rbind(game_one_filtered, game_two_filtered)

combined_overall_game_data$game_round <- as.factor(combined_overall_game_data$game_round)

# Levene's Test for Homogeneity of Variances
perform_levene_test(
  data = combined_overall_game_data,
  formula = average_balloon_inflate_per_game ~ game_round
)

print_and_save(
  "\n############################## Non-parametric test ##############################" 
)

perform_paired_t_test(
  data = combined_overall_game_data,
  formula = average_balloon_inflate_per_game ~ game_round,
  "Average inflate per game"
)

# Find the effect size
perform_cohens_d_paired(
  game_one_filtered$average_balloon_inflate_per_game,
  game_two_filtered$average_balloon_inflate_per_game,
  "Overall risk taking behaviour"
)

print_and_save(
  "############################ Descriptive statistics ############################" 
)

calculate_stats(game_one_filtered$average_balloon_inflate_per_game, "total inflates for game 1", "Game round 1")
calculate_stats(game_two_filtered$average_balloon_inflate_per_game, "total inflates for game 2", "Game round 2")

ggplot(combined_overall_game_data, aes(x = average_balloon_inflate_per_game, y = factor(game_round), fill = factor(game_round))) +
  geom_density_ridges(alpha = 0.4, scale = 0.5) +
  geom_boxplot(aes(y = factor(game_round), x = average_balloon_inflate_per_game), width = 0.2, alpha = 0.3) +
  labs(
    x = "Average Inflates per game",
    y = "Game round"
  ) +
  theme_minimal() +
  theme(legend.position = "none", plot.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "cm")) +
  coord_flip() +
  scale_y_discrete(expand = c(0, 0.1), labels = c("Baseline", "w/ Robot"))

# Calculate mean and SEM for each game round
game_stats <- combined_overall_game_data %>%
  group_by(game_round) %>%
  summarise(
    mean = mean(average_balloon_inflate_per_game),
    sem = sd(average_balloon_inflate_per_game) / sqrt(n()), .groups = "drop"
  )

# Convert game_round to a factor with meaningful labels
game_stats$game_round <- factor(game_stats$game_round, levels = c(1, 2), labels = c("Baseline", "w/ Robot"))

# Create the bar plot with error bars
ggplot(game_stats, aes(x = game_round, y = mean, fill = game_round)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width = .2, position = position_dodge(.9)) +
  labs(
    title = "Risk Behaviour: Average Inflates Per Game",
    x = "Game Round",
    y = "Average Inflates",
    fill = "Game Round"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

################################################################################
print_and_save(
  "\n\n## Is there a difference in overall average inflates between study conditions? ##"
)
################################################################################

print_and_save(
  "for the game played with the robot, is there a difference in average inflates between the two study conditions?"
)

overall_avg_query <- "
  SELECT
    balloon_inflate.player_id,
    players.customise_first AS study_cond,
    AVG(total_inflates - total_inflates_before_help_request) AS overall_avg_inflate_after_help
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
    players.customise_first
"

overall_avg_data <- dbGetQuery(conn, overall_avg_query)

# Convert 'study_cond' to a factor so the statistical tests know what the categories (study conditions) are
overall_avg_data$study_cond <-
  as.factor(overall_avg_data$study_cond)

customise <- subset(overall_avg_data, study_cond == 1)
mean_custom <- mean(customise$overall_avg_inflate_after_help)
sd_custum <- sd(customise$overall_avg_inflate_after_help)
customise_filtered <- subset(customise, overall_avg_inflate_after_help >= (mean_custom - 2 * sd_custum) & overall_avg_inflate_after_help <= (mean_custom + 2 * sd_custum))
perform_shapiro_test(
  customise_filtered$overall_avg_inflate_after_help,
  "overall_avg_inflate_after_help for customised condition"
)

qqPlot(customise_filtered$overall_avg_inflate_after_help)

non_customise <- subset(overall_avg_data, study_cond == 0)
mean_non_custum <- mean(non_customise$overall_avg_inflate_after_help)
sd_non_custum <- sd(non_customise$overall_avg_inflate_after_help)
non_customise_filtered <- subset(non_customise, overall_avg_inflate_after_help >= (mean_non_custum - 2 * sd_non_custum) & overall_avg_inflate_after_help <= (mean_non_custum + 2 * sd_non_custum))
perform_shapiro_test(
  non_customise_filtered$overall_avg_inflate_after_help,
  "overall_avg_inflate_after_help non-customise condition"
)

qqPlot(non_customise_filtered$overall_avg_inflate_after_help)

# Find excluded participants for each study conditions
excluded_cust_ppt <- setdiff(customise$player_id, customise_filtered$player_id)
excluded_non_cust_ppt <- setdiff(non_customise$player_id, non_customise_filtered$player_id)

# Combine excluded participant lists for each set of conditions
all_excluded_ppt <- unique(c(excluded_cust_ppt, excluded_non_cust_ppt))

# Exclude these participants from respective datasets
customise_filtered <- customise_filtered[!customise_filtered$player_id %in% all_excluded_ppt, ]
non_customise_filtered <- non_customise_filtered[!non_customise_filtered$player_id %in% all_excluded_ppt, ]

combined_overall_sutdy_cond_data <- rbind(customise_filtered, non_customise_filtered)

# Levene's Test for Homogeneity of Variances
perform_levene_test(
  data = combined_overall_sutdy_cond_data,
  formula = overall_avg_inflate_after_help ~ study_cond
)

perform_wilcox_rank_sum_test(
  data = combined_overall_sutdy_cond_data,
  formula = overall_avg_inflate_after_help ~ study_cond,
  "Overall Inflate between study condition"
)

ggplot(combined_overall_sutdy_cond_data, aes(x = overall_avg_inflate_after_help, y = factor(study_cond), fill = factor(study_cond))) +
  geom_density_ridges(alpha = 0.4, scale = 0.6) +
  geom_boxplot(aes(y = factor(study_cond), x = overall_avg_inflate_after_help), width = 0.2, alpha = 0.3) +
  geom_jitter(height = 0.1, size = 1, alpha = 0.5, color = "black") +
  labs(
    title = "Avg inflates after requesting help from robot per study condition",
    x = "Avg inflates after help requested",
    y = "Study Condition"
  ) +
  theme_minimal() +
  theme(legend.position = "none", plot.margin = margin(t = 0.5, r = 0.1, b = 0.1, l = 0.5, unit = "cm")) +
  coord_flip() +
  scale_y_discrete(expand = c(0, 0), labels = c("non-custom", "custom"))

# Close the database connection
dbDisconnect(conn)

# Set the working directory and include required packages
setwd(getSrcDirectory(function() {})[1])

if (!require(DBI))
  install.packages("DBI")
if (!require(RSQLite))
  install.packages("RSQLite")
if (!require(car))
  install.packages("car")
if (!require(ggplot2))
  install.packages("ggplot2")
if (!require(ggridges))
  install.packages("ggridges")
library(DBI)
library(RSQLite)
library(car)
library(ggridges)
library(ggplot2)

# File to save output
output_file <- "objective_data_analysis_results.txt"

source("./analysis_functions.R")

# Set up connection to the SQLite database
db_path <- "./participant.db"
conn <- dbConnect(RSQLite::SQLite(), dbname = db_path)

################################################################################
print_and_save(
  "##### Is there a difference in inflate or collect between study conditions? #####"
)
################################################################################

print_and_save(
  "For each participant calculate the average number of inflates after the robot requested \na inflate and when it requested a collect"
)

#! Ensure that burst balloons are excluded from the analysis - this is done by checking if help_requested == 1, since no help will be requested if balloon burst
average_inflates_query <- "
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

avg_inflates_after_help_requested_data <- dbGetQuery(conn, average_inflates_query)

# Subset for inflate requests (robot_response = 1)
inflate_request_data <- subset(avg_inflates_after_help_requested_data, robot_response == 1)

# Shapiro-Wilk test for normality (inflate requests)
perform_shapiro_test(
  inflate_request_data$avg_inflate_after_help,
  "Normality of Inflates for Inflate Request"
)

# Wilcoxon Test for difference in inflates between study conditions (inflate requests)
perform_wilcox_rank_sum_test(
  data = inflate_request_data,
  formula = avg_inflate_after_help ~ study_cond,
  "Difference in Inflates for Inflate Request between Study Conditions"
)

# Subset for collect requests (robot_response = 0)
collect_request_data <- subset(avg_inflates_after_help_requested_data, robot_response == 0)

# Shapiro-Wilk test for normality (collect requests)
perform_shapiro_test(
  collect_request_data$avg_inflate_after_help,
  "Normality of Inflates for Collect Request"
)

# Wilcoxon Test for difference in inflates between study conditions (collect requests)
perform_wilcox_rank_sum_test(
  data = collect_request_data,
  formula = avg_inflate_after_help ~ study_cond,
  "Difference in Inflates for Collect Request between Study Conditions"
)

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

# Shapiro-Wilk Test for Normality
perform_shapiro_test(
  overall_avg_data$overall_avg_inflate_after_help,
  "Overall Average Inflation After Help"
)

# Levene's Test for Homogeneity of Variances
perform_levene_test(
  data = overall_avg_data,
  formula = overall_avg_inflate_after_help ~ study_cond
  )

perform_wilcox_rank_sum_test(
  data = overall_avg_data,
  formula = overall_avg_inflate_after_help ~ study_cond,
  "Overall Inflate between study condition"
  )

################################################################################
print_and_save(
  "\n\n######### Is there a difference in compliance between study conditions? #########"
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
                              FUN = sum)
# The resulting column with summed values is automatically named 'compliance', but it represents total compliance, re-name it to "total_compliance"
names(total_compliance)[4] <- "total_compliance"

# Add a new column 'count_response' with a default value of 0
compliance_data$count_response <- 0
# Count the number of rows for each unique combination of player_id, study_cond and robot_response
count_responses <- aggregate(count_response ~ player_id + study_cond + robot_response, 
                             data = compliance_data, 
                             FUN = length)

# Merge total_compliance and count_responses tables
normalised_compliance_data <- merge(total_compliance, count_responses, 
                                    by = c("player_id", "study_cond", "robot_response"))

# Calculate normalised compliance and store it in the "normalised_compliance" column
normalised_compliance_data$normalised_compliance <- normalised_compliance_data$total_compliance / normalised_compliance_data$count_response

# Subset the data for when robot_response is 1 (inflate request) i.e. don't include data for robot_response == 0
inflate_request_data <- subset(normalised_compliance_data, robot_response == 1)

perform_shapiro_test(
  inflate_request_data$normalised_compliance,
  "normalised_compliance_data (Robot Response 1)"
)

# The data set violates the assumption of normality, use Wilcox Rank Sum test as it is equivalent independent samples t-test
# Perform the Wilcoxon rank sum test for inflate requests
perform_wilcox_rank_sum_test(
  data = inflate_request_data,
  formula = normalised_compliance ~ study_cond,
  "Normalised Compliance for Inflate Request"
)

# Subset the data for when robot_response is 0 (collect request)
collect_request_data <- subset(normalised_compliance_data, robot_response == 0)

perform_shapiro_test(
  collect_request_data$normalised_compliance,
  "normalised_compliance_data (Robot Response 0)"
)

# Perform the Wilcoxon rank sum test for collect requests
perform_wilcox_rank_sum_test(
  data = collect_request_data,
  formula = normalised_compliance ~ study_cond,
  "Normalised Compliance for Collect Request"
)

################################################################################
print_and_save(
  "\n\n####### Is there a difference in reaction time between study conditions? #######"
)
################################################################################

overall_reaction_time_query <- "
  SELECT
    balloon_inflate.player_id,
    players.customise_first AS study_cond,
    robot_response,
    help_requested,
    balloon_id,
    balloon_limit,
    total_inflates,
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

# Fetch reaction_time data
overall_reaction_time_query <- dbGetQuery(conn, overall_reaction_time_query)

# Sum reaction_time for each robot response for each player
total_time <- aggregate(reaction_time ~ player_id + study_cond + robot_response, 
                        data = overall_reaction_time_query, 
                        FUN = sum)
names(total_time)[4] <- "total_reaction_time"

# Add a new column 'count_response' with a default value of 0
overall_reaction_time_query$count_response <- 0
# Count the number of decisions (occurrences) for each robot response for each player
count_decisions <- aggregate(count_response ~ player_id + study_cond + robot_response, 
                             data = overall_reaction_time_query, 
                             FUN = length)

# Merge total_time and count_decisions, these will be used to calculate the avg. reaction time
average_time_data <- merge(total_time, count_decisions, 
                           by = c("player_id", "study_cond", "robot_response"))

# Calculate average reaction time
average_time_data$average_reaction_time <- average_time_data$total_reaction_time / average_time_data$count_response

# Subset for robot_response = 1
overall_reaction_time_cust <- subset(average_time_data, study_cond == 0)
# Subset for robot_response = 2
overall_reaction_time_non_cust <- subset(average_time_data, study_cond == 1)

perform_shapiro_test(
  overall_reaction_time_cust$average_reaction_time,
  "Average Reaction Time for customise group"
)

perform_shapiro_test(
  overall_reaction_time_non_cust$average_reaction_time,
  "Average Reaction Time for non-customise group"
)

perform_wilcox_rank_sum_test(
  data = average_time_data,
  formula = average_reaction_time ~ study_cond,
  "Difference in Reaction Time between study conditions"
)

################################################################################
print_and_save(
  "\n\n###### Is there a difference in avg reaction time between robot responses? ######"
)
################################################################################

reaction_time_query <- "
  SELECT
    balloon_inflate.player_id,
    players.customise_first AS study_cond,
    robot_response,
    AVG(time_to_decide) AS average_reaction_time
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
  GROUP BY
    balloon_inflate.player_id,
    players.customise_first,
    robot_response
"

# Fetch reaction time data
reaction_time_data <- dbGetQuery(conn, reaction_time_query)

# Subset for robot_response = 1
reaction_time_for_inflate_request <- subset(reaction_time_data, robot_response == 1)

# For Inflate Request - Study Condition 0
inflate_cond_0 <- subset(reaction_time_for_inflate_request, study_cond == 0)
mean_inflate_0 <- mean(inflate_cond_0$average_reaction_time)
sd_inflate_0 <- sd(inflate_cond_0$average_reaction_time)
inflate_cond_0_filtered <- subset(inflate_cond_0, average_reaction_time >= (mean_inflate_0 - 2 * sd_inflate_0) & average_reaction_time <= (mean_inflate_0 + 2 * sd_inflate_0))
perform_shapiro_test(
  inflate_cond_0_filtered$average_reaction_time,
  "Shapiro-Wilk for Inflate Request, Study Condition 0"
)

# For inflate Request - Study Condition 1
inflate_cond_1 <- subset(reaction_time_for_inflate_request, study_cond == 1)
mean_inflate_1 <- mean(inflate_cond_1$average_reaction_time)
sd_inflate_1 <- sd(inflate_cond_1$average_reaction_time)
inflate_cond_1_filtered <- subset(inflate_cond_1, average_reaction_time >= (mean_inflate_1 - 2 * sd_inflate_1) & average_reaction_time <= (mean_inflate_1 + 2 * sd_inflate_1))
perform_shapiro_test(
  inflate_cond_1_filtered$average_reaction_time,
  "Shapiro-Wilk for Inflate Request, Study Condition 1"
)

inflate_request_combined <- rbind(inflate_cond_0_filtered, inflate_cond_1_filtered)

inflate_request_combined$study_cond <- as.factor(inflate_request_combined$study_cond)

# Levene's Test for Inflate Request
perform_levene_test(
  data = inflate_request_combined,
  formula = average_reaction_time ~ study_cond
)

perform_t_test(
  data = inflate_request_combined,
  formula = average_reaction_time ~ study_cond,
  "Average Reaction Time for inflate request (Robot Response 1)"
)

# Subset for robot_response = 0
reaction_time_for_collect_request <- subset(reaction_time_data, robot_response == 0)

# For Collect Request - Study Condition 0
collect_cond_0 <- subset(reaction_time_for_collect_request, study_cond == 0)
mean_collect_0 <- mean(collect_cond_0$average_reaction_time)
sd_collect_0 <- sd(collect_cond_0$average_reaction_time)
collect_cond_0_filtered <- subset(collect_cond_0, average_reaction_time >= (mean_collect_0 - 2 * sd_collect_0) & average_reaction_time <= (mean_collect_0 + 2 * sd_collect_0))
perform_shapiro_test(
  collect_cond_0_filtered$average_reaction_time,
  "Shapiro-Wilk for Collect Request, Study Condition 0"
)

qqPlot(collect_cond_0_filtered$average_reaction_time)

# For Collect Request - Study Condition 1
collect_cond_1 <- subset(reaction_time_for_collect_request, study_cond == 1)
mean_collect_1 <- mean(collect_cond_1$average_reaction_time)
sd_collect_1 <- sd(collect_cond_1$average_reaction_time)
collect_cond_1_filtered <- subset(collect_cond_1, average_reaction_time >= (mean_collect_1 - 2 * sd_collect_1) & average_reaction_time <= (mean_collect_1 + 2 * sd_collect_1))
perform_shapiro_test(
  collect_cond_1_filtered$average_reaction_time,
  "Shapiro-Wilk for Collect Request, Study Condition 1"
)

collect_request_combined <- rbind(collect_cond_0_filtered, collect_cond_1_filtered)

collect_request_combined$study_cond <- as.factor(collect_request_combined$study_cond)

# Levene's Test for Collect Request
perform_levene_test(
  data = collect_request_combined,
  formula = average_reaction_time ~ study_cond
)

perform_welch_t_test(
  data = collect_request_combined,
  formula = average_reaction_time ~ study_cond,
  "Average Reaction Time for collect request (Robot Response 0)"
)

hedges_g(
  collect_cond_1_filtered$average_reaction_time,
  collect_cond_0_filtered$average_reaction_time,
  "Reaction time"
)

# Calculate and save statistics for each condition
calculate_stats(collect_cond_1_filtered$average_reaction_time, "Collect Request", "Customisation Condition")
calculate_stats(collect_cond_0_filtered$average_reaction_time, "Collect Request", "Non-customisation Condition")

# Count the number of participants after filtering for inflate request
num_participants_inflate = nrow(inflate_request_combined)
print_and_save(paste("Number of participants remaining for inflate request (Robot Response 1):", num_participants_inflate))

# Count the number of participants after filtering for collect request
num_participants_collect = nrow(collect_request_combined)
print_and_save(paste("Number of participants remaining for collect request (Robot Response 0):", num_participants_collect))

# Visualisation
ggplot(collect_request_combined, aes(x = average_reaction_time, y = factor(study_cond), fill = factor(study_cond))) +
  geom_density_ridges(alpha = 0.4, scale = 0.6) +
  geom_boxplot(aes(y = factor(study_cond), x = average_reaction_time), width = 0.2, alpha = 0.3) +
  labs(
    title = "Reaction time for Collect request by robot",
    x = "Average Reaction Time (seconds)",
    y = "Study Condition"
  ) +
  theme_minimal() +
  theme(legend.position = "none", plot.margin = margin(t = 0.5, r = 0.1, b = 0.1, l = 0.5, unit = "cm")) +
  coord_flip() +
  scale_y_discrete(expand = c(0, 0), labels = c("non-custom", "custom"))

################################################################################
print_and_save(
  "\n\n#### Is there a difference in median reaction time between robot responses? ####"
)
################################################################################

reaction_time_query <- "
  SELECT
    balloon_inflate.player_id,
    players.customise_first AS study_cond,
    robot_response,
    time_to_decide
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
median_time_data <- aggregate(time_to_decide ~ player_id + study_cond + robot_response, 
                              data = reaction_time_data, 
                              FUN = median)
# Rename the aggregated column from time_to_decide to "median_reaction_time"
names(median_time_data)[4] <- "median_reaction_time"

# Create a table where robot_response = 1 i.e. exclude all robot_response = 0
reaction_time_for_inflate_request <- subset(median_time_data, robot_response == 1)

# Perform statistical analysis for inflate request
perform_shapiro_test(
  reaction_time_for_inflate_request$median_reaction_time,
  "Median Reaction Time for inflate request (Robot Response 1)"
)

perform_wilcox_rank_sum_test(
  data = reaction_time_for_inflate_request,
  formula = median_reaction_time ~ study_cond,
  "Median Reaction Time for collect request (Robot Response 1)"
)

# Create a table where robot_response = 0 i.e. exclude all robot_response = 1
reaction_time_for_collect_request <- subset(median_time_data, robot_response == 0)

# Perform statistical analysis for collect request
perform_shapiro_test(
  reaction_time_for_collect_request$median_reaction_time,
  "Median Reaction Time for collect request (Robot Response 0)"
)

perform_wilcox_rank_sum_test(
  data = reaction_time_for_collect_request,
  formula = median_reaction_time ~ study_cond,
  "Median Reaction Time for collect request (Robot Response 0)"
)

calculate_vd_a(
  reaction_time_for_collect_request,
  "study_cond",
  "median_reaction_time"
)

# Split the data by study condition to pass to calculate_stats()
customise_cond_data <- subset(reaction_time_for_collect_request, study_cond == 1)
non_customise_cond_data <- subset(reaction_time_for_collect_request, study_cond == 0)

# Calculate and save statistics for each condition
calculate_stats(customise_cond_data$median_reaction_time, "Collect Request", "Customisation Condition")
calculate_stats(non_customise_cond_data$median_reaction_time, "Collect Request", "Non-customisation Condition")

# generate a raincloud plot without displaing the data points
ggplot(reaction_time_for_collect_request, aes(x=median_reaction_time, y=factor(study_cond), fill=factor(study_cond))) +
  geom_density_ridges(alpha=0.4, scale=0.6) +
  geom_boxplot(aes(y=factor(study_cond), x=median_reaction_time), width=0.2, alpha=0.3) +
  # geom_jitter(height=0.1, size=1, alpha=0.5, color="black") +
  labs(title="Median Reaction time for Inflate request by robot",
       x="Median Reaction Time (seconds)",
       y="Study Condition") +
  theme_minimal() +
  theme(legend.position = "none", plot.margin=margin(t=0.5, r=0.1, b=0.1, l=0.5, unit="cm")) +
  coord_flip() +
  scale_y_discrete(expand=c(0, 0), labels=c("non-custom", "custom"))

################################################################################
print_and_save(
  "\n\n## Is there a overall difference in inflate vs collect when playing with robot? ##"
)
################################################################################

print_and_save(
  "For the game played with the robot, is there a difference in how much the participant inflated the
  \nballoon after they received a inflates or collects request from the robot, regardless of the study condition\n"
)
# i.e. did the player inflate the balloon more when the robot requested a inflate and less when the robot requested a collect?

overall_avg_query <- "
  SELECT
    balloon_inflate.player_id,
    robot_response,
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
    robot_response
"

overall_avg_data <- dbGetQuery(conn, overall_avg_query)

# Subset for inflate requests (robot_response = 1)
inflate_request_data <- subset(overall_avg_data, robot_response == 1)
mean_inflate_data <- mean(inflate_request_data$overall_avg_inflate_after_help)
sd_inflate_data <- sd(inflate_request_data$overall_avg_inflate_after_help)
inflate_data_filtered <- subset(inflate_request_data, overall_avg_inflate_after_help >= (mean_inflate_data - 2 * sd_inflate_data) & overall_avg_inflate_after_help <= (mean_inflate_data + 2 * sd_inflate_data))
perform_shapiro_test(
  inflate_data_filtered$overall_avg_inflate_after_help,
  "Shapiro-Wilk for Inflate Request data, robot request 1"
)

qqPlot(inflate_data_filtered$overall_avg_inflate_after_help)

# Subset for collect requests (robot_response = 0)
collect_request_data <- subset(overall_avg_data, robot_response == 0)
mean_collect_data <- mean(collect_request_data$overall_avg_inflate_after_help)
sd_collect_data <- sd(collect_request_data$overall_avg_inflate_after_help)
collect_data_filtered <- subset(collect_request_data, overall_avg_inflate_after_help >= (mean_collect_data - 2 * sd_collect_data) & overall_avg_inflate_after_help <= (mean_collect_data + 2 * sd_collect_data))
perform_shapiro_test(
  collect_data_filtered$overall_avg_inflate_after_help,
  "Shapiro-Wilk for Collect Request data, robot request 0"
)

qqPlot(collect_data_filtered$overall_avg_inflate_after_help)

combined_overall_avg_data <- rbind(inflate_data_filtered, collect_data_filtered)

# Wilcoxon Signed-Rank Test
perform_mann_whitney(
  inflate_data_filtered$overall_avg_inflate_after_help,
  collect_data_filtered$overall_avg_inflate_after_help,
  "Difference in Inflate vs. Collect responses"
)

# Find the effect size
perform_cliffs_delta(
  inflate_data_filtered$overall_avg_inflate_after_help,
  collect_data_filtered$overall_avg_inflate_after_help,
  "Effect size for the difference in the response for inflate vs. collect, when playing with robot"
)

calculate_stats(inflate_data_filtered$overall_avg_inflate_after_help, "Inflate Request", "Robot requested inflate")
calculate_stats(collect_data_filtered$overall_avg_inflate_after_help, "Collect Request", "Robot requested collect")

ggplot(combined_overall_avg_data, aes(x=overall_avg_inflate_after_help, y=factor(robot_response), fill=factor(robot_response))) +
  geom_density_ridges(alpha=0.4, scale=0.5) +
  geom_boxplot(aes(y=factor(robot_response), x=overall_avg_inflate_after_help), width=0.2, alpha=0.3) +
  labs(x="Average Inflates After Requesting Help",
       y="Robot Response") +
  theme_minimal() +
  theme(legend.position = "none", plot.margin=margin(t=1, r=1, b=1, l=1, unit="cm")) +
  coord_flip() +
  scale_y_discrete(expand=c(0, 0.1), labels=c("Collect Request", "Inflate Request"))

################################################################################
print_and_save(
  "\n\n######### Is there a difference in risk taking behaviour from baseline? ########"
)
################################################################################

average_inflates_query <- "
  SELECT
    balloon_inflate.player_id,
    game_round,
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

# Fetch reaction_time data
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

combined_overall_game_data <- rbind(game_one_filtered, game_two_filtered)

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

calculate_stats(game_one_filtered$average_balloon_inflate_per_game, "total inflates for game 1", "Game round 1")
calculate_stats(game_two_filtered$average_balloon_inflate_per_game, "total inflates for game 2", "Game round 2")

ggplot(combined_overall_game_data, aes(x=average_balloon_inflate_per_game, y=factor(game_round), fill=factor(game_round))) +
  geom_density_ridges(alpha=0.4, scale=0.5) +
  geom_boxplot(aes(y=factor(game_round), x=average_balloon_inflate_per_game), width=0.2, alpha=0.3) +
  labs(x="Average Inflates per game",
       y="Game round") +
  theme_minimal() +
  theme(legend.position = "none", plot.margin=margin(t=1, r=1, b=1, l=1, unit="cm")) +
  coord_flip() +
  scale_y_discrete(expand=c(0, 0.1), labels=c("Baseline", "w/ Robot"))

################################################################################
print_and_save(
  "\n\n########## Is there a difference in gender and risk taking behaviour? ##########"
)
################################################################################

gender_inflates_query <- "
  SELECT
    balloon_inflate.player_id,
    game_round,
    balloon_limit,
    total_inflates,
    gender
  FROM
    balloon_inflate
  JOIN
    players ON balloon_inflate.player_id = players.player_id
  WHERE
    players.testing = 0
    AND players.game_completed = 1
    AND game_round = 2
    AND balloon_inflate.balloon_id > 5
    AND total_inflates <= balloon_limit
"

# Fetch reaction_time data
gender_inflates_data <- dbGetQuery(conn, gender_inflates_query)

# Preprocess gender column
gender_inflates_data$gender_processed <- ifelse(tolower(gender_inflates_data$gender) == "female", 
                                         "f", 
                                         "m")

# Sum total_inflates for each robot response for each player
total_inflates_per_gender <- aggregate(total_inflates ~ player_id + gender_processed, 
                            data = gender_inflates_data, 
                            FUN = sum)

# Add a new column 'count_num_of_balloons' with a default value of 0
gender_inflates_data$count_num_of_balloons <- 0
# Count the number of un-poped balloons for each game round for each player
count_balloons_per_gender <- aggregate(count_num_of_balloons ~ player_id + gender_processed, 
                            data = gender_inflates_data, 
                            FUN = length)

# Merge total_inflates_per_gender and count_balloons tables
average_inflates_data_per_gender <- merge(total_inflates_per_gender, count_balloons_per_gender, 
                               by = c("player_id", "gender_processed"))

# Calculate average balloon inflate per gender
average_inflates_data_per_gender$average_balloon_inflate_per_gender <- average_inflates_data_per_gender$total_inflates / average_inflates_data_per_gender$count_num_of_balloons

perform_shapiro_test(
  average_inflates_data_per_gender$average_balloon_inflate_per_gender[average_inflates_data_per_gender$gender_processed == "f"],
  "average_balloon_inflate_per_gender for females"
)

perform_shapiro_test(
  average_inflates_data_per_gender$average_balloon_inflate_per_gender[average_inflates_data_per_gender$gender_processed == "m"],
  "average_balloon_inflate_per_gender for males"
)

print_and_save("The data set for males violates the assumption of normality")

# Wilcoxon Test for difference in inflates between study conditions
perform_wilcox_rank_sum_test(
  data = average_inflates_data_per_gender,
  formula = average_balloon_inflate_per_gender ~ gender_processed,
  "Difference in average inflates between genders"
)

################################################################################
print_and_save(
  "\n\n######## Is there a difference in compliance between genders conditions? ########"
)
################################################################################

gender_compliance_query <- "
  SELECT
  balloon_inflate.player_id, gender, robot_response,
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
gender_compliance_data <- dbGetQuery(conn, gender_compliance_query)

# Preprocess gender column
gender_compliance_data$gender_processed <- ifelse(tolower(gender_compliance_data$gender) == "female", 
                                                "f", 
                                                "m")

# Sum the values in the compliance column for each unique combination of player_id, study_cond and robot_response
total_compliance_per_gender <- aggregate(compliance ~ player_id + gender_processed + robot_response, 
                              data = gender_compliance_data, 
                              FUN = sum)
# The resulting column with summed values is automatically named 'compliance', but it represents total compliance, re-name it to "total_compliance"
names(total_compliance_per_gender)[4] <- "total_compliance_per_gender"

# Add a new column 'count_response' with a default value of 0
gender_compliance_data$count_response <- 0
# Count the number of rows for each unique combination of player_id, study_cond and robot_response
count_responses <- aggregate(count_response ~ player_id + gender_processed + robot_response, 
                             data = gender_compliance_data, 
                             FUN = length)

# Merge total_compliance_per_gender and count_responses tables
normalised_compliance_data_per_gender <- merge(total_compliance_per_gender, count_responses, 
                                    by = c("player_id", "gender_processed", "robot_response"))

# Calculate normalised compliance and store it in the "normalised_compliance" column
normalised_compliance_data_per_gender$normalised_compliance <- normalised_compliance_data_per_gender$total_compliance_per_gender / normalised_compliance_data_per_gender$count_response

perform_shapiro_test(
  normalised_compliance_data_per_gender$normalised_compliance[normalised_compliance_data_per_gender$gender_processed == "f"],
  "normalised_compliance_data_per_gender females"
)
perform_shapiro_test(
  normalised_compliance_data_per_gender$normalised_compliance[normalised_compliance_data_per_gender$gender_processed == "m"],
  "normalised_compliance_data_per_gender males"
)

# The data set violates the assumption of normality, use mann whitney u test as it is equivalent independent samples t-test
perform_wilcox_rank_sum_test(
  data = normalised_compliance_data_per_gender,
  formula = normalised_compliance ~ gender_processed,
  "Normalised Compliance accross genders"
)

################################################################################
print_and_save(
  "\n\n######### Is there a difference in inflate or collect between genders? ##########"
)
################################################################################

#! Ensure that burst balloons are excluded from the analysis - this is done by checking if help_requested == 1, since no help will be requested if balloon burst
average_inflates_query <- "
  SELECT
    balloon_inflate.player_id,
    gender,
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

avg_inflates_data <- dbGetQuery(conn, average_inflates_query)

# Preprocess gender column
avg_inflates_data$gender_processed <- ifelse(tolower(avg_inflates_data$gender) == "female", 
                                                  "f", 
                                                  "m")

# Preprocess gender column
avg_inflates_data$gender_processed <- ifelse(tolower(avg_inflates_data$gender) == "female", "f", "m")

# Subset for inflate requests (robot_response = 1)
inflate_request_data <- subset(avg_inflates_data, robot_response == 1)

# Subset for collect requests (robot_response = 0)
collect_request_data <- subset(avg_inflates_data, robot_response == 0)

# Perform statistical tests for inflate requests
perform_shapiro_test(
  inflate_request_data$avg_inflate_after_help,
  "Normality of Inflates for Inflate Request"
)
perform_wilcox_rank_sum_test(
  data = inflate_request_data,
  formula = avg_inflate_after_help ~ gender_processed,
  "Difference in Inflates for Inflate Request by Gender"
)

# Perform statistical tests for collect requests
perform_shapiro_test(
  collect_request_data$avg_inflate_after_help,
  "Normality of Inflates for Collect Request"
)
perform_wilcox_rank_sum_test(
  data = collect_request_data,
  formula = avg_inflate_after_help ~ gender_processed,
  "Difference in Inflates for Collect Request by Gender"
)

# Close the database connection
dbDisconnect(conn)

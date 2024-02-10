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

ggplot(collect_request_data, aes(x = normalised_compliance, y = factor(study_cond), fill = factor(study_cond))) +
  geom_density_ridges(alpha = 0.4, scale = 0.6) +
  geom_boxplot(aes(y = factor(study_cond), x = normalised_compliance), width = 0.2, alpha = 0.3) +
  geom_jitter(height=0.1, size=1, alpha=0.5, color="black") +
  labs(
    title = "Compliance",
    x = "avg inflates after help requested",
    y = "Study Condition"
  ) +
  theme_minimal() +
  theme(legend.position = "none", plot.margin = margin(t = 0.5, r = 0.1, b = 0.1, l = 0.5, unit = "cm")) +
  coord_flip() +
  # scale_y_discrete(expand = c(0, 0), labels = c("non-custom", "custom"))

# Close the database connection
dbDisconnect(conn)

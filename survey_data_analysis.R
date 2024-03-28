# Set the working directory and include the required packages

setwd(getSrcDirectory(function() {})[1])

if (!require(DBI)) install.packages("DBI")
if (!require(RSQLite)) install.packages("RSQLite")
if (!require(car)) install.packages("car")
library(DBI)
library(RSQLite)
library(car)

# File to save output
output_file <- "output_files/survey_data_analysis_results.txt"

source("./analysis_functions.R")

# Set up connection to the SQLite database
db_path <- "./participant.db"
conn <- dbConnect(RSQLite::SQLite(), dbname = db_path)

# Extract and analyse survey data

final_survey_query <- "
  SELECT final_survey.*, players.customise_first  AS study_cond
  FROM final_survey
  JOIN players ON final_survey.player_id = players.player_id
  WHERE players.testing = 0 AND players.game_completed = 1
  "

final_survey_data <- dbGetQuery(conn, final_survey_query)

# Subt data where study_cond is 1 (True)
final_survey_customise_cond <-
  final_survey_data[final_survey_data$study_cond == 1,]

# Subset data where study_cond is 0 (False)
final_survey_non_customise_cond <-
  final_survey_data[final_survey_data$study_cond == 0,]

################################################################################
print_and_save(
  "############################# Godspeed Likability ##############################"
)
################################################################################

# Select the columns for godspeed likability for each study condition
likability_customise <-
  final_survey_customise_cond[, c(
    "dislike_like",
    "unfriendly_friendly",
    "unkind_kind",
    "unpleasant_pleasant",
    "awful_nice"
  )]

likability_non_customise <-
  final_survey_non_customise_cond[, c(
    "dislike_like",
    "unfriendly_friendly",
    "unkind_kind",
    "unpleasant_pleasant",
    "awful_nice"
  )]

# Calculate the average likability score and add it as a column in the database for each condition
final_survey_customise_cond$avg_likability <-
  rowMeans(likability_customise, na.rm = TRUE)
final_survey_non_customise_cond$avg_likability <-
  rowMeans(likability_non_customise, na.rm = TRUE)

## Test for Normality in likability scale #####
perform_shapiro_test(
  final_survey_customise_cond$avg_likability,
  "avg_likability (Customise Condition)"
)
perform_shapiro_test(
  final_survey_non_customise_cond$avg_likability,
  "avg_likability (Non-Customise Condition)"
)

## Test for homogeneity of variance in likability scale ####
combined_data <-
  rbind(final_survey_customise_cond,
        final_survey_non_customise_cond)
combined_data$study_cond <-
  as.factor(combined_data$study_cond)
perform_levene_test(combined_data, avg_likability ~ study_cond)

# Perform T-test
perform_t_test(combined_data, avg_likability ~ study_cond, "Likability")

################################################################################
print_and_save(
  "############################ Godspeed Intelligence #############################"
)
################################################################################

# Select the columns for godspeed intelligence for each study condition
intelligence_customise <-
  final_survey_customise_cond[, c(
    "incompetent_competent",
    "ignorant_knowledgeable",
    "iriresponsible_responsible",
    "unitelligent_intelligent",
    "foolish_sensible"
  )]
intelligence_non_customise <-
  final_survey_non_customise_cond[, c(
    "incompetent_competent",
    "ignorant_knowledgeable",
    "iriresponsible_responsible",
    "unitelligent_intelligent",
    "foolish_sensible"
  )]

# Calculate the average intelligence score and add it as a column in the database for each condition
final_survey_customise_cond$avg_intelligence <-
  rowMeans(intelligence_customise, na.rm = TRUE)
final_survey_non_customise_cond$avg_intelligence <-
  rowMeans(intelligence_non_customise, na.rm = TRUE)

## Test for Normality in intelligence scale #####
perform_shapiro_test(
  final_survey_customise_cond$avg_intelligence,
  "avg_intelligence (Customise Condition)"
)
perform_shapiro_test(
  final_survey_non_customise_cond$avg_intelligence,
  "avg_intelligence (Non-Customise Condition)"
  )

## Test for homogeneity of variance in intelligence scale ####
combined_data <-
  rbind(final_survey_customise_cond,
        final_survey_non_customise_cond)
combined_data$study_cond <-
  as.factor(combined_data$study_cond)

# Compare variance for the avg_intelligence scale between the two groups (specified by study_cond)
perform_levene_test(combined_data, avg_intelligence ~ study_cond)

# Perform T-test for Godspeed intelligence
perform_t_test(combined_data, avg_intelligence ~ study_cond, "Intelligence")

################################################################################
print_and_save(
  "################################## Ownership ###################################"
)
################################################################################

# Select the columns for ownership for each study condition
ownership_customise <-
  final_survey_customise_cond[, c(
    "this_is_my_robot",
    "i_sense_that_i_own_this_robot",
    "this_robot_incorporates_a_part_of_myself"
  )]
ownership_non_customise <-
  final_survey_non_customise_cond[, c(
    "this_is_my_robot",
    "i_sense_that_i_own_this_robot",
    "this_robot_incorporates_a_part_of_myself"
  )]

# Calculate the average ownership score and add it as a column in the database for each condition
final_survey_customise_cond$avg_ownership <-
  rowMeans(ownership_customise, na.rm = TRUE)
final_survey_non_customise_cond$avg_ownership <-
  rowMeans(ownership_non_customise, na.rm = TRUE)

## Test for Normality in ownership scale #####
perform_shapiro_test(final_survey_customise_cond$avg_ownership,
                     "avg_ownership (Customise Condition)")
perform_shapiro_test(
  final_survey_non_customise_cond$avg_ownership,
  "avg_ownership (Non-Customise Condition)"
)

## Test for homogeneity of variance in Ownership scale ####
combined_data <-
  rbind(final_survey_customise_cond,
        final_survey_non_customise_cond)
combined_data$study_cond <-
  as.factor(combined_data$study_cond)

# Compare variance for the avg_ownership scale between the two groups (specified by study_cond)
perform_levene_test(combined_data, avg_ownership ~ study_cond)

# Perform T-test for Godspeed Ownership
perform_t_test(combined_data, avg_ownership ~ study_cond, "Ownership")

cohens_d(
  final_survey_customise_cond$avg_ownership,
  final_survey_non_customise_cond$avg_ownership,
  "Ownership"
)

# Calculate median for customise condition
median_ownership_customise <- median(final_survey_customise_cond$avg_ownership, na.rm = TRUE)

# Calculate IQR for customise condition
iqr_ownership_customise <- IQR(final_survey_customise_cond$avg_ownership, na.rm = TRUE)

# Calculate the median for non-customise condition
median_ownership_non_customise <- median(final_survey_non_customise_cond$avg_ownership, na.rm = TRUE)

# Calculate IQR for non-customise condition
iqr_ownership_non_customise <- IQR(final_survey_non_customise_cond$avg_ownership, na.rm = TRUE)

# Calculate the standard deviation for customise condition
std_dev_customise <- sd(final_survey_customise_cond$avg_ownership, na.rm = TRUE)

# Calculate the standard deviation for non-customise condition
std_dev_non_customise <- sd(final_survey_non_customise_cond$avg_ownership, na.rm = TRUE)

# Print the results
print(paste("Standard Deviation for Customise Condition:", std_dev_customise))
print(paste("Standard Deviation for Non-Customise Condition:", std_dev_non_customise))
print(paste("Median Ownership (Customise):", median_ownership_customise))
print(paste("IQR Ownership (Customise):", iqr_ownership_customise))
print(paste("Median Ownership (Non-Customise):", median_ownership_non_customise))
print(paste("IQR Ownership (Non-Customise):", iqr_ownership_non_customise))

################################################################################
print_and_save(
  "#################################### Trust #####################################"
)
################################################################################

# Select the columns for trustworthiness for each study condition
trust_customise <-
  final_survey_customise_cond[, c(
    "i_am_confident_in_the_robot",
    "the_robot_has_integrity",
    "the_robot_gave_good_advice",
    "the_robot_is_reliable",
    "i_can_trust_the_robot"
  )]
trust_non_customise <-
  final_survey_non_customise_cond[, c(
    "i_am_confident_in_the_robot",
    "the_robot_has_integrity",
    "the_robot_gave_good_advice",
    "the_robot_is_reliable",
    "i_can_trust_the_robot"
  )]

# Calculate the average trustworthiness score and add it as a column in the database for each condition
final_survey_customise_cond$avg_trust <-
  rowMeans(trust_customise, na.rm = TRUE)
final_survey_non_customise_cond$avg_trust <-
  rowMeans(trust_non_customise, na.rm = TRUE)

## Test for Normality in trust scale #####
perform_shapiro_test(final_survey_customise_cond$avg_trust,
                     "avg_trustworthiness (Customise Condition)")
perform_shapiro_test(
  final_survey_non_customise_cond$avg_trust,
  "avg_trustworthiness (Non-Customise Condition)"
)

## Test for homogeneity of variance in trust scale ####
combined_data <-
  rbind(final_survey_customise_cond,
        final_survey_non_customise_cond)
combined_data$study_cond <-
  as.factor(combined_data$study_cond)

# Compare variance for the avg_trust scale between the two groups (specified by study_cond)
perform_levene_test(combined_data, avg_trust ~ study_cond)

# Perform T-test for Godspeed trust
perform_t_test(combined_data, avg_trust ~ study_cond, "Trustworthiness")

################################################################################
print_and_save(
  "#################################### MANOVA ####################################"
)
################################################################################

# Combine the customise and non-customise condition data
final_survey_combined <- rbind(final_survey_customise_cond, final_survey_non_customise_cond)

# Ensure that study_cond is a factor
final_survey_combined$study_cond <- factor(final_survey_combined$study_cond)

perform_MANOVA_test(
  data = final_survey_combined,
  formula = cbind(avg_likability, avg_intelligence, avg_ownership, avg_trust) ~ study_cond,
  variable_name = "combined customise and non-customise condition survey data"
)

dbDisconnect(conn)

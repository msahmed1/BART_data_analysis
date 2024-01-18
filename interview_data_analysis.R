library(dplyr)
interview_data <- fread(
    "interview_data.csv",
    select = c("ID", "StrategyV2", "RobotLearningV2", "ComplianceV2", "HelpfulnessV2", "Ownership")
)

table(interview_data$StrategyV2)
table(interview_data$RobotLearningV2)
table(interview_data$ComplianceV2)
table(interview_data$HelpfulnessV2)
table(interview_data$Ownership)

# Set up connection to the SQLite database
db_path <- "./participant.db"
conn <- dbConnect(RSQLite::SQLite(), dbname = db_path)

participant_data_query <- "
  SELECT
    player_id,
    players.customise_first AS study_cond
  FROM
    players
  WHERE
    players.testing = 0
    AND players.game_completed = 1
"

# Fetch reaction time data
participant_data <- dbGetQuery(conn, participant_data_query)

merged_data <- merge(
    interview_data,
    participant_data,
    by.x = "ID",
    by.y = "player_id"
)

custom_cond <- subset(merged_data, study_cond == 1)
# table(custom_cond$StrategyV2)
# table(custom_cond$RobotLearningV2)
# table(custom_cond$ComplianceV2)
# table(custom_cond$HelpfulnessV2)
# table(custom_cond$Ownership)

non_custom_cond <- subset(merged_data, study_cond == 0)
# table(non_custom_cond$StrategyV2)
# table(non_custom_cond$RobotLearningV2)
# table(non_custom_cond$ComplianceV2)
# table(non_custom_cond$HelpfulnessV2)
# table(non_custom_cond$Ownership)

dbDisconnect(conn)

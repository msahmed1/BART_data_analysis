# Set the working directory and include required packages

setwd(getSrcDirectory(function() {
})[1])

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
output_file <- "output_files/stats_data_results.txt"

source("./analysis_functions.R")

library(DBI)
library(RSQLite)

db_path <- "./participant.db"

# Connect to the database
conn <- dbConnect(RSQLite::SQLite(), db_path)

stats_query <- "
  SELECT
    balloon_inflate.player_id,
    game_round,
    COUNT(CASE WHEN total_inflates >= balloon_limit THEN 1 END) AS burst_count,
    AVG(
      CASE 
        WHEN game_round = 1 AND total_inflates < balloon_limit THEN total_inflates
        WHEN game_round = 2 AND total_inflates_before_help_request < balloon_limit THEN total_inflates_before_help_request
      END
    ) AS avg_inflates_before_help
  FROM
    balloon_inflate
  JOIN
    players ON balloon_inflate.player_id = players.player_id
  WHERE
    players.testing = 0
    AND players.game_completed = 1
    AND balloon_inflate.balloon_id > 5
  GROUP BY
    balloon_inflate.player_id,
    game_round
"

stats_data <- dbGetQuery(conn, stats_query)

bust_count_game_1 <- subset(stats_data, game_round == 1)
calculate_stats(bust_count_game_1$burst_count, "game round 1", "burst count")

bust_count_game_2 <- subset(stats_data, game_round == 2)
calculate_stats(bust_count_game_2$burst_count, "game round 2", "burst count")

inflates_game_1 <- subset(stats_data, game_round == 1)
calculate_stats(inflates_game_1$avg_inflates_before_help, "game round 1", "inflates")

inflates_game_2 <- subset(stats_data, game_round == 2)
calculate_stats(inflates_game_2$avg_inflates_before_help, "game round 2", "inflates before help")

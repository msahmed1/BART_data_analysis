# Set the working directory and include required packages

setwd(getSrcDirectory(function() {
})[1])

if (!require(DBI))
  install.packages("DBI")
if (!require(RSQLite))
  install.packages("RSQLite")
if (!require(car))
  install.packages("car")
library(DBI)
library(RSQLite)
library(car)

# File to save output
output_file <- "demographic_analysis_results.txt"

source("./analysis_functions.R")

# Set up connection to the SQLite database
db_path <- "./participant.db"
conn <- dbConnect(RSQLite::SQLite(), dbname = db_path)

# Extract demographic data
query_participant_count <-
  "SELECT COUNT(*) FROM players WHERE testing = 0 and game_completed = 1"
total_participants <- dbGetQuery(conn, query_participant_count)
message <- paste(
  "Number of participants: ",
  total_participants
)
print_and_save(message)

query_age_count <- "SELECT age, COUNT(*) FROM players GROUP BY age"
participant_age_brackets <- dbGetQuery(conn, query_age_count)

print_and_save(paste(capture.output(print(participant_age_brackets)), collapse = "\n"))

# Identify gender split

# SQL query to select the gender column
gender_query <-
  "SELECT gender FROM players  WHERE testing = 0 and game_completed = 1"

# Run the query and store the result
gender_data <- dbGetQuery(conn, gender_query)

# convert the gender data to lower case
gender_data$gender <- tolower(gender_data$gender)

# Count the number of male and female entries (with frequency table)
gender_count <- table(gender_data$gender)

# Print the gender count
message <- paste(
  names(gender_count),
  ": ",
  gender_count,
  ","
)
print_and_save(message)

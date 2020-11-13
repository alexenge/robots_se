###################################################################################################
# PREPROCESSING
###################################################################################################

###############################################################################
## Read data from JATOS
###############################################################################

# Load the relevant packages
library(tidyverse)

# Here we define a cusstom function for reading the raw data from the files that are created
# by JATOS (the online platform where we have run our experiment). These are "JSON" files, which
# need some wrangling so R can understand them properly. You don't need to understand what is
# happening here.
read_jatos <- function(file) {
  require(jsonlite)
  read_file(file) %>%
    str_split("\n") %>%
    first() %>%
    discard(function(x) x == "") %>%
    map_dfr(fromJSON, flatten = TRUE) %>%
    as_tibble()
}

# List the file names of our two results files from JATOS...
list.files("raw", pattern = "jatos_results", full.names = TRUE) %>%
  # ... feed into our custom function...
  map(read_jatos) %>%
  # ... and combine them into a single data set
  bind_rows() -> dat_raw

# Create a new column with participant IDs
dat_raw <- dat_raw %>% fill(url.srid, .direction = "down") %>% rename(id = url.srid)

###############################################################################
## Preprocess some miscellaneous data (for timings + quality check)
###############################################################################

# Start with the raw data...
dat_raw %>%
  # ... rename some columns...
  rename(event = sender, text = German, ) %>%
  # ... select relevant columns...
  select(id, event, time_end, text) %>%
  # ... print the result...
  print() %>%
  # ... and save it as a text file
  write_csv(file = "data/dat_misc.csv")

###############################################################################
## Preprocess data from the main experiment (for RT and accuracy analysis)
###############################################################################

# Start with the raw data...
dat_raw %>%
  # ... filter only experimental trials...
  filter(correctResponse %in% c("positive", "negative")) %>%
  # ... filter out practice trials...
  filter(is.na(practice)) %>%
  # ... extract information about the condition of the trial...
  mutate(
    # ... namely which type of agent was presented...
    agent = case_when(
      str_detect(image, "human_") ~ "human",
      str_detect(image, "robot_") ~ "robot"
    ),
    # ... and if their eyes were open or closed...
    gaze = case_when(
      str_detect(image, "open") ~ "open",
      str_detect(image, "closed") ~ "closed"
    ),
    # ... transform the accuracy of the response to numeric...
    correct = case_when(
      correct == "TRUE" ~ 1,
      correct == "FALSE" ~ 0
    )
  ) %>%
  # ... rename some columns...
  rename(valence = correctResponse, rt = duration) %>%
  # ... select only the relevant columns...
  select(id, word, agent, gaze, valence, rt, correct) %>%
  # ... print the result...
  print() %>%
  # ... and save it as a text file
  write_csv(file = "data/dat_exp.csv")

###############################################################################
## Preprocess the rating data (for intentionality and agency ratings)
###############################################################################

# Start with the raw data...
dat_raw %>%
  # ... rename some columns and convert to numeric if necessary...
  mutate(
    stimulus = sender,
    intentionality = as.numeric(intentional),
    agency = as.numeric(selb),
    rationality = as.numeric(rational),
    intelligence = as.numeric(intelligent)
  ) %>%
  # ... filter only the rating trials...
  filter(!is.na(intentionality)) %>%
  # ... create new columns for our independent variables...
  mutate(
    agent = ifelse(str_detect(sender, "h"), "human", "robot"),
    gaze = ifelse(str_detect(sender, "o"), "open", "closed")
  ) %>%
  # ... select only the relevant columns...
  select(id, stimulus, agent, gaze, intentionality, agency, rationality, intelligence) %>%
  # ... print the result...
  print() %>%
  # ... and save it as a text file
  write_csv(file = "data/dat_ratings.csv")

###############################################################################
## Preprocess the ATAI data (for attitudes towards AI analysis)
###############################################################################

# Start with the raw data...
dat_raw %>%
  # ... filter the relevant ATAI trials...
  filter(sender == "AttitudesAI") %>%
  # ... rename some columns and convert to numeric if necessary...
  mutate(
    fear_ai = as.numeric(angstKI),
    trust_ai = as.numeric(vetrauenKI),
    destroy_ai = as.numeric(ZerstoerungKI),
    enrich_ai = as.numeric(BereicherungKI),
    unemploy_ai = as.numeric(ArbeitslosigkeitKI)
  ) %>%
  # ... select only the relevant columns...
  select(id, fear_ai, trust_ai, destroy_ai, enrich_ai, unemploy_ai) %>%
  # ... print the result...
  print() %>%
  # ... and save it as a text file
  write_csv(file = "data/dat_atai.csv")

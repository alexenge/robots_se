###################################################################################################
# SCRIPT 1: DATA QUALITY CHECK
###################################################################################################

###################################################################################################
# Exercise 0: Read data into R
###################################################################################################

# Install the relevant packages (you only need to do this once)
install.packages(c("jsonlite", "tidyverse", "magrittr"))

# Load the relevant packages
library(jsonlite)
library(tidyverse)
library(magrittr)

# Setting some options (nevermind)
options(tibble.print_max = 50)

# Define a custom function to read JATOS files
read_jatos <- function(file) {
  suppressMessages(
    # Read the JATOS results file...
    read_file(file) %>%
      # ... split it into rows ...
      str_split("\n") %>% first() %>%
      # ... filter empty rows ...
      discard(function(x) x == "") %>%
      # ... parse JSON into a data.frame ...
      map_dfr(fromJSON, flatten = TRUE) %>%
      # ... and convert to tibble (just a kind of pretty dataframe)
      as_tibble()
  )
}

# Apply this function to our own results files...
list.files(pattern = "jatos_results") %>%
  map(read_jatos) %>%
  # ... and bind the into a single tibble
  bind_rows() -> dat_all

###################################################################################################
# EXERCISE 1: Data Quality Check Group
###################################################################################################

###############################################################################
## -  How long did it take participants to complete the task?
###############################################################################

# Retrieve the end time of the "End of task" event (i.e. the end of the actual experiment)
time_end <- dat %>% filter(sender == "End of Task") %>% pull(time_end)

# Retrieve the end time of the "Demographic questions" (directly before the actual experiment)
time_start <- dat %>% filter(sender == "Demographic_questions") %>% pull(time_end)

# Compute the difference between the two and convert from ms to s to min
(time_task <- (time_end - time_start) / 1000 / 60)

# Plot a histogram
ggplot() +
  geom_histogram(aes(x = time_task), binwidth = 5, boundary = 10, color = "black", fill = "gray80") +
  theme_bw()

###############################################################################
## -  How many participants responded correctly on the Quality Control Question?
###############################################################################

# Retrieve participants' answer to the question "QC" (quality check)
(qual_check <- dat %>% filter(sender == "QC") %>% pull(German))

# Check how many of them responded (correctly) with "Chicago".
(qual_check == "Chicago") %>% sum()

###############################################################################
## -  How many errors did participants make?
###############################################################################

# Create a new column with participant IDs
dat %<>% fill(url.srid, .direction = "down") %>% mutate(id = url.srid)

# How many participants do we have?
(n <- n_distinct(dat$url.srid))

# Do the job
(dat %>%
    # Filter out practise trials
    filter(is.na(practice)) %>% 
    # Filter only the acutal reactions
    filter(correctResponse %in% c("positive", "negative")) %>% 
    # Count cases of correct, depending on ID
    count(id, correct) -> num_errors)

###############################################################################
## -  How many outliers were there?
###############################################################################

# Create new columns for our idenpendent variables...
dat %<>% mutate(
  # ... namely which type of agent was presented ...
  agent = case_when(
    str_detect(image, "human_") ~ "human",
    str_detect(image, "robot_") ~ "robot"
  ) %>% factor(levels = c("human", "robot")),
  # ... and if their eyes were open or closed.
  gaze = case_when(
    str_detect(image, "open") ~ "open",
    str_detect(image, "closed") ~ "closed"
  ) %>% factor(levels = c("open", "closed"))
)

# Do the job
(dat %>%
    # Filter out practise trials
    filter(is.na(practice)) %>% 
    # Filter only target trials
    filter(correctResponse %in% c("positive", "negative")) %>% 
    # Filter only correct responses
    filter(correct == "TRUE") %>% 
    # Group by participants and independent variables
    group_by(id, agent, gaze, correctResponse) %>% 
    # Identify outliers (Tukey fences) in each condition
    summarize(boxplot.stats(duration)$out) %>% 
    # Ungroup
    ungroup() %>% 
    # Now we count rows per participant, i.e. how many ouliers per participant across conditions 
    count(id, name = "outliers") -> num_outliers)
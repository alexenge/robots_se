###################################################################################################
# GROUP 1: DATA QUALITY CHECK
###################################################################################################

###############################################################################
## Exercise 0: Read data into R
###############################################################################

# Install the relevant packages (you only need to do this once)
install.packages(c("jsonlite", "tidyverse", "magrittr"))

# Load the relevant packages
library(tidyverse)
library(magrittr)

# Tell R to always print up to 50 lines of results
options(tibble.print_max = 50)

# Here we define a cusstom function for reading the raw data from the files that are created
# by JATOS (the online platform where we have run our experiment). These are "JSON" files, which
# need some wrangling so R can understand them properly. You don't need to understand what is
# happening here.
read_jatos <- function(file) {
  suppressMessages(require(jsonlite))
  suppressMessages(
    read_file(file) %>%
      str_split("\n") %>% first() %>%
      discard(function(x) x == "") %>%
      map_dfr(fromJSON, flatten = TRUE) %>%
      as_tibble()
  )
}

# These are the URLs of the two raw data files on GitHub...
c(
  "https://raw.githubusercontent.com/alexenge/robots_se/master/data/jatos_results_20201108083642",
  "https://raw.githubusercontent.com/alexenge/robots_se/master/data/jatos_results_20201108083705"
) %>%
  # ... which we feed into our custom function...
  map(read_jatos) %>%
  # ... and combine them into a single data set
  bind_rows() -> dat_all

# Create a new column with participant IDs
dat_all %<>% fill(url.srid, .direction = "down") %>% rename(id = url.srid)

# How many participants do we have?
(n <- n_distinct(dat_all$id))

###############################################################################
## Exercise 1.1: How long did it take participants to complete the task?
###############################################################################

# In our data set ("dat_all"), there is a column called "sender" which, for each row, tells us
# which kind of event has taken place. There is also a column called "time_end", which tells us
# when this event was happening. We can use these two columns to query when the actual experiment
# was starting (sender == "Demographic_questions") and when it was over (sender == "End of Task").
# By subtracting these two time points, we can calculate how long each participant took for the
# actual experiment.

# Retrieve the end time of the "End of task" event (i.e. the end of the actual experiment)
time_end <- dat_all %>% filter(sender == "End of Task") %>% pull(time_end)

# Retrieve the end time of the "Demographic questions" (directly before the actual experiment)
time_start <- dat_all %>% filter(sender == "Demographic_questions") %>% pull(time_end)

# Compute the difference between the two and convert from ms to s to min
(time_task <- (time_end - time_start) / 1000 / 60)

# Create a plot...
ggplot() +
  # ... showing a histogram of the task times in steps of 5 min...
  geom_histogram(aes(x = time_task), binwidth = 5, boundary = 10, color = "white") +
  # ... and use an APA-conform theme
  theme_classic()

###############################################################################
## Exercise 1.2: Did participants respond correctly to the quality check?
###############################################################################

# In the "sender" column, there is one event called "QC" (quality check). This was the question
# where participants were required to respond with "Chicago" despite the fact that the question
# asked for a city that was not in the US. You need to retrieve participants' responses to this
# question, which is stored in a column called "German". You can then calculate the percentage
# of participants that responded (correctly) with "Chicago".

# Retrieve participants' answer to the question "QC" (= quality check)
(qual_check <- dat_all %>% filter(sender == "QC") %>% pull(German))

# Check how many of them responded (correctly) with "Chicago"
(qual_check == "Chicago") %>% sum() / n

###############################################################################
## Exercise 1.3: How many errors did participants make in the main task?
###############################################################################

# You first need to extract from the dataset those rows that code the actual trials of the
# main experiment. This can be done by filtering only those rows where there as a code for
# the valence of the target word, which was either "positive" or "negative". These codes are
# stored in a column called "correctResponse". You also need to filter out the practice trials,
# which are all rows where the column "practice" doesn't have the value NA. Once you have this
# subset of the data, you can count the number of rows where participants made an error, which
# is coded in the column "correct" ("FALSE" -> error, "TRUE" -> correct).

# Extract the data from the main experimental task
dat_exp <- dat_all %>%
  # Filter only experimental trials
  filter(correctResponse %in% c("positive", "negative")) %>%
  # Filter out practice trials
  filter(is.na(practice))

# Start with the raw data...
(dat_exp %>%
    # ... filter trials where the response was incorrect...
    filter(correct == "FALSE") %>%
    # ... count the number of such trials per participant...
    count(id, name = "errors") %>%
    # ... and store these numbers in a vector called "errors"
    pull(errors) -> errors)

###############################################################################
## Exercise 1.4: How many outlier reaction times (RTs) did participants make?
###############################################################################

# This is tricky. We want the outliers separately for each experimental condition (that is for each
# combination of agent, eye gaze, and word valence). We therefore first need to create new columns
# that code for these conditions. We've done this for you.

# Create new columns for our idenpendent variables...
dat_exp %<>% mutate(
  # ... namely which type of agent was presented...
  agent = case_when(
    str_detect(image, "human_") ~ "human",
    str_detect(image, "robot_") ~ "robot"
  ) %>% factor(levels = c("human", "robot")),
  # ... if their eyes were open or closed...
  gaze = case_when(
    str_detect(image, "open") ~ "open",
    str_detect(image, "closed") ~ "closed"
  ) %>% factor(levels = c("open", "closed")),
  # ... and if the word valence was positive or negative
  valence = correctResponse %>%
    factor(levels = c("positive", "negative"))
)

# Now you can use the data from the main task (see exercise 1.3) to compute the number of outliers
# per participant and condition. A useful function to determine outliers from a set of data points
# is the function "boxplot.stats()". This function provides, among other things, a vector of
# outliers (called "out") which we can count per participant and condition.

# Start with the experimental data...
(dat_exp %>%
    # ... filter out incorrect responses
    filter(correct == "TRUE") %>%
    # ... group by participants and conditions...
    group_by(id, agent, gaze, valence) %>%
    # ... identify the number of outlier RTs per participant and condition...
    summarize(boxplot.stats(duration)$out) %>%
    # ... remove the grouping we created above...
    ungroup() %>%
    # ... and count the number of rows (outliers) per participant
    count(id, name = "outliers") -> outliers)


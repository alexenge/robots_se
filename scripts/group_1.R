###################################################################################################
# GROUP 1: DATA QUALITY CHECK
###################################################################################################

# Load the relevant packages
library(tidyverse)

###############################################################################
## Exercise 1.1: Read the data into R
###############################################################################

# Start by familiarizing yourself with information stored in the file "dat_misc.csv". You can open
# this file using a text editor (Editor/Notepad on Windows, TextEdit on Mac) or in a spreadsheet
# software (Microsoft Excel or Apple Numbers). Then, try to load the data into R, e.g. using the
# read_csv() function from the tidyverse. Do the same for the file "dat_exp.csv" which contains the
# trials from the main experiment.

# Read the misc data set
read_csv("data/dat_misc.csv") -> dat_misc

# Read the main experimental data set
read_csv("data/dat_exp.csv") -> dat_exp

# How many participants do we have?
(dat_exp %>% pull(id) %>% n_distinct() -> n)

###############################################################################
## Exercise 1.2: How long did it take participants to complete the main task?
###############################################################################

# The data set "dat_misc" contains information about every event that happened on participants'
# screens (column "event") together with the time that this event ended ("time_end"). You can use
# this information to calculate how long each participant took on the main task. The main task
# began with the end of the event "Demographic questions" and ended with the of the event "End of
# task". The difference between these two time points is the duration of the main task in milli-
# seconds. Try to compute this for all participants. If you want to and find the time, you can also
# try to plot these durations as a histogram.

# Retrieve the end time of the "End of task" event (i.e. the end of the actual experiment)
dat_misc %>% filter(event == "End of Task") %>% pull(time_end) -> time_end

# Retrieve the end time of the "Demographic questions" (directly before the actual experiment)
dat_misc %>% filter(event == "Demographic_questions") %>% pull(time_end) -> time_start

# Compute the difference between the two and convert from ms to s to min
((time_end - time_start) / 1000 / 60 -> time_task)

# Create a plot...
ggplot() +
   # ... showing a histogram of the task times in steps of 5 min...
   geom_histogram(aes(x = time_task), binwidth = 5, boundary = 10, color = "white") +
   # ... and use an APA-conform theme
   theme_classic()

# In base R, this is a bit easier
hist(time_task)

###############################################################################
## Exercise 1.3: Did participants respond correctly to the quality check?
###############################################################################

# In the "event" column, of "dat_misc" there is one event called "QC" (quality check). This was
# the question where participants were required to respond with "Chicago" despite the fact that
# the question asked for a city that was not in the US. You can retrieve participants' responses
# to this question, which is stored in a column called "text". You can then calculate the
# percentage of participants that responded (correctly) with "Chicago".

# Retrieve participants' answer to the question "QC" (= quality check)
(dat_misc %>% filter(event == "QC") %>% pull(text) -> qual_check)

# Check how many % of them responded (correctly) with "Chicago"
(qual_check == "Chicago") %>% sum() / n

###############################################################################
## Exercise 1.4: How many errors did participants make in the main task?
###############################################################################

# The data set "data_exp" contains all the trials from th e main experimental task. In there,
# you'll find one column which indicates if the response of the participant (positive or negative
# word) was correct (1) or not (0). Use this information to count how many errors each participant
# made, e.g. by using the "filter()" and "count()" functions from the tidyverse. Again, if you
# want to, try to plot the errors per participant as a histogram.

# Filter trials with errors and count the number of these trials per participant id
(dat_exp %>% filter(correct == 0) %>% count(id, name = "errors") -> errors)

# Plot this information as a histogram
ggplot(data = errors, aes(x = errors)) +
   geom_histogram(binwidth = 5, boundary = 0, color = "white") +
   theme_classic()

###############################################################################
## Exercise 1.5: How many outlier reaction times (RTs) did participants make?
###############################################################################

# This is quite advanced! Start by noticing "dat_exp" contains participants' reaction times (RTs)
# in the main experiment. We can use these to check if / how many outlier RTs participants made.
# To do so, we first need to compute each participants mean RT. This is done using the "group_by"
# function to group trials from the same participant and then adding a new column using the "mutate"
# function to compute the mean RT of this participant and another new column for the SD of this
# participants' RTs. Then, you can check for every trial if the actual RT in this trial was above
# or below a certain outlier threshold. A common choice is the mean +/- 2.5 standard devations.
# Try to count for how many trials per participant this was the case.

# Start with the experimental data...
(dat_exp %>%
    # ... group by participant IDs...
    group_by(id) %>%
    # ... add new columns for the mean RT per participant and their SD...
    mutate(rt_mean = mean(rt), rt_sd = sd(rt)) %>%
    # ... check if the actual RT in the trial was above/below a certain threshold (mean +/- 3 SD)...
    mutate(outlier = rt > mean(rt) + 2.5 * sd(rt) | rt < mean(rt) - 2.5 * sd(rt)) %>%
    # ... count the number of outliers per participant...
    filter(outlier) %>%
    count(id, name = "outliers") -> outliers)

# Add the total number of trials per participant and check how many percent were outliers
(outliers %>%
      ungroup() %>%
      mutate(
         total = dat_exp %>% count(id) %>% pull(n),
         percent = outliers / total * 100
      ) -> outliers_pct)

# Percantage of outliers in the whole data set (mean across participants)
outliers_pct %>% pull(percent) %>% mean()

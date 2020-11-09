###################################################################################################
# GROUP 4: INTENTIONALITY & AGENCY
###################################################################################################

###############################################################################
## Exercise 0: Read data into R
###############################################################################

# Install the relevant packages (you only need to do this once)
install.packages(c("jsonlite", "tidyverse", "magrittr", "knitr"))

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
dat_all %<>% fill(url.srid, .direction = "down") %>% mutate(id = factor(url.srid))

# How many participants do we have?
(n <- n_distinct(dat_all$id))

###############################################################################
## Exercise 4.1: How did you evaluate the intentionality of humans and robots?
###############################################################################

# You first need to extract from the raw data (which contain one row for everything that happend
# during the experiment) those trials which belong to the ratings at the ened of the experiment.
# These are those rows which have a numeric value between 0 and 6 in the column called
# intentionality (as well as agency, intelligence, ...). You also need to extract the information
# which kind of agent was shown (human or robot) and if their eyes were open or closed. This
# information is stored in an abbreviated format (h, r, o, c) in the column called "sender"

# Start with the raw data...
(dat_all %>%
  # ... filter only the relevant rating trials...
  filter(!is.na(intentional)) %>%
  # ... create new columns for our independent variables...
  mutate(
    agent = ifelse(str_detect(sender, "h"), "human", "robot"),
    gaze = ifelse(str_detect(sender, "o"), "open", "closed")
  ) %>%
  # ... convert ratings from characters to numeric...
  mutate(across(c(intentional, selb, rational, intelligent), .fns = as.numeric)) %>%
  # ... and select only the relevant variables and save as new data frame
  select(id, agent, gaze, intentional, selb, rational, intelligent) -> data_ratings)

# In this and all the other exercises up to 4.4, you then need to compute the average ratings
# across multiple trials (with different stimuli) from the same condition. We start with the
# intentionality ratings (column "intentional") and average them by participants and by agents
# (human or robot). In the tidyverse, we first need to create the grouping via "group_by()" and
# then average within these groups using "summarise()". You can then also do one more step of
# averaging where you compute the by-condition averages across (rather than within) participants.
# This is called "grand averaging" and is done by grouping only by agent (not by participant id).

# Start with the rating data...
(data_ratings %>%
  # ... and compute mean intentionality ratings by agent
  group_by(id, agent) %>%
  summarise(avg = mean(intentional)) -> ratings_int)

# Grand-average across participants
ratings_int %>%
  group_by(agent) %>%
  summarise(gavg = mean(avg), sd = sd(avg), se = sd(avg) / sqrt(n))

###############################################################################
## Exercise 4.2: How did you evaluate the agency of humans and robots?
###############################################################################

# Here you can do exactly the same as above, but using the column for the agency (rather then
# intentionality) ratings. These are stored in a column called "selb".

# Start with the rating data...
(data_ratings %>%
  # ... and compute mean agency ratings by agent
  group_by(id, agent) %>%
  summarise(avg = mean(selb)) -> ratings_selb)

# Grand-average across participants
ratings_selb %>%
  group_by(agent) %>%
  summarise(gavg = mean(avg), sd = sd(avg), se = sd(avg) / sqrt(n))

###############################################################################
## Exercise 4.3: Did eye gaze influence the intentionality ratings?
###############################################################################

# To do this you can perform the same steps as in 4.1, but add the column "gaze" (open or closed)
# as an additional grouping variable.

# We do the same as above, but add gaze as an additional factor for gaze
(data_ratings %>%
  group_by(id, agent, gaze) %>%
  summarise(avg = mean(intentional)) -> ratings_int_gaze)

# Grand-average across participants
ratings_int_gaze %>%
  group_by(agent, gaze) %>%
  summarise(gavg = mean(avg), sd = sd(avg), se = sd(avg) / sqrt(n))

###############################################################################
## Exercise 4.4: Did eye gaze influence the agency ratings?
###############################################################################

# Here you can do exactly the same as above, but using the column for the agency (rather then
# intentionality) ratings. These are stored in a column called "selb".

# And we do the same for agency
(data_ratings %>%
  group_by(id, agent, gaze) %>%
  summarise(avg = mean(selb)) -> ratings_selb_gaze)

# Grand-average across participants
ratings_selb_gaze %>%
  group_by(agent, gaze) %>%
  summarise(gavg = mean(avg), sd = sd(avg), se = sd(avg) / sqrt(n))

###############################################################################
## Exercise 4.5: Create a table and plot as an overview of the rating data
###############################################################################

# Finally, let's create a table and a plot to visualize all our rating results at once. One way
# to do this is to combine the rating tibbles we've created in steps 4.3 and 4.4 (which include
# the rating data by participants, agents, and gaze) and display them together in one table and
# in one plot. You can again use "group_by" and "summarise" to compute the grand averages across
# participants and then show these values in any way you find intuitive.

# Bind intentionality and agency ratings together (row-wise)
ratings <- bind_rows(
  "intentionality" = ratings_int_gaze,
  "agency" = ratings_selb_gaze,
  .id = "rating"
)

# Average across participants
ratings %>%
  group_by("Rating" = rating, "Agent" = agent, "Eyegaze" = gaze) %>%
  summarise(
    "Average rating" = mean(avg),
    "SD" = sd(avg),
    "SE" = sd(avg) / sqrt(n)
  ) %>%
  # Prettier table output is provided by the "kable" function from the knitr package
  knitr::kable(digits = 2)

# And we create a plot...
ratings %>%
  # ... with the ratings on the y-axis, gaze on the x-axis, and different colors for agents...
  ggplot(aes(x = gaze, y = avg, color = agent, group = agent)) +
  # ... and with two separate subplots for agency and intentionality ratings...
  facet_wrap(~rating) +
  # ... and we add individual data points (one point is one participant)...
  geom_point(position = position_jitterdodge(jitter.height = 0.1), alpha = 0.3) +
  # ... and summary statistics (mean and standard errors across participants)...
  stat_summary(fun.data = "mean_se", position = position_dodge(width = 0.8)) +
  stat_summary(fun = mean, geom = "line", position = position_dodge(width = 0.8)) +
  # ... and finally, we want to use an APA-conform theme
  theme_classic()

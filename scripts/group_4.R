###################################################################################################
# GROUP 4: INTENTIONALITY & AGENCY
###################################################################################################

# Load the relevant packages
library(tidyverse)

###############################################################################
## Exercise 4.1: Read the data into R
###############################################################################

# Start by familiarizing yourself with information stored in the file "dat_ratings.csv". You can
# open this file using a text editor (Editor/Notepad on Windows, TextEdit on Mac) or in a
# spreadsheet software (Microsoft Excel or Apple Numbers). Then, try to load the data into R, e.g.
# using the read_csv() function from the tidyverse.

# Read the rating data set
read_csv("data/dat_ratings.csv") -> dat_ratings

# How many participants do we have?
(dat_ratings %>% pull(id) %>% n_distinct() -> n)

###############################################################################
## Exercise 4.2: How did you evaluate the intentionality of humans and robots?
###############################################################################

# In this and all the other exercises up to 4.4, you then need to compute the average ratings
# across multiple trials (with different stimuli) from the same condition. We start with the
# intentionality ratings (column "intentionality") and average them by participants and by agents
# (human or robot). In the tidyverse, we first need to create the grouping via "group_by()" and
# then average within these groups using "summarise()". You can then also do one more step of
# averaging, where you compute the by-condition averages across (rather than within) participants.
# This is called "grand averaging" and is done by grouping only by agent (not by participant id).

# Start with the rating data...
(dat_ratings %>%
   # ... group by participant IDs and agents...
   group_by(id, agent) %>%
   # ... and compute the mean across trials within these groups
   summarise(avg = mean(intentionality)) -> ratings_int)

# Grand-average across participants
ratings_int %>%
  group_by(agent) %>%
  summarise(
    gavg = mean(avg),
    sd = sd(avg),
    se = sd(avg) / sqrt(n)
  )

###############################################################################
## Exercise 4.3: How did you evaluate the agency of humans and robots?
###############################################################################

# Here you can do exactly the same as in Exercise 4.2, but using the column for the agency ratings
# (rather then the intentionality ratings).

# Start with the rating data...
(dat_ratings %>%
   # ... group by participant IDs and agents...
   group_by(id, agent) %>%
   # ... and compute the mean across trials within these groups
   summarise(avg = mean(agency)) -> ratings_ag)

# Grand-average across participants
ratings_ag %>%
  group_by(agent) %>%
  summarise(
    gavg = mean(avg),
    sd = sd(avg),
    sd = sd(avg) / sqrt(n)
  )

###############################################################################
## Exercise 4.4: Did eye gaze influence the intentionality and agency ratings?
###############################################################################

# To do this you can perform the same steps as in Exercises 4.2 and 4.3, but add the factor "gaze"
# (open or closed) as an additional grouping variable.

# Intentionality: Start with the rating data...
(dat_ratings %>%
   # ... group by participant IDs, agents, and gaze...
   group_by(id, agent, gaze) %>%
   # ... and compute the mean across trials within these groups
   summarise(avg = mean(intentionality)) -> ratings_int_gaze)

# Intentionality: Grand-average across participants
ratings_int_gaze %>%
  group_by(agent, gaze) %>%
  summarise(
    gavg = mean(avg),
    sd = sd(avg),
    sd = sd(avg) / sqrt(n)
  )

# Agency: Start with the rating data...
(dat_ratings %>%
    # ... group by participant IDs, agents, and gaze...
    group_by(id, agent, gaze) %>%
    # ... and compute the mean across trials within these groups
    summarise(avg = mean(agency)) -> ratings_ag_gaze)

# Agency: Grand-average across participants
ratings_ag_gaze %>%
  group_by(agent, gaze) %>%
  summarise(
    gavg = mean(avg),
    sd = sd(avg),
    se = sd(avg) / sqrt(n)
  )

###############################################################################
## Exercise 4.5: Create a table and plot as an overview of the rating data
###############################################################################

# Finally, let's create a table and a plot to visualize all our rating results at once. One way
# to do this is to combine the rating tibbles we've created in Exercise 4.4 (which include the
# rating data by participants, agents, and gaze) and display them together in one table and in one
# plot. You can again use "group_by()" and "summarise()" to compute the grand averages across
# participants and then show these values in any way you find intuitive.

# Bind intentionality and agency ratings together (row-wise)
ratings <- bind_rows(
  "intentionality" = ratings_int_gaze,
  "agency" = ratings_ag_gaze,
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

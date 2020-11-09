###################################################################################################
# GROUP 3: ANALYSIS OF ACCURACY
###################################################################################################

###############################################################################
## Exercise 0: Read data into R
###############################################################################

# Install the relevant packages (you only need to do this once)
install.packages(c("jsonlite", "tidyverse", "magrittr", "ez", "emmeans"))

# Load the relevant packages
library(tidyverse)
library(magrittr)
library(ez)
library(emmeans)

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

# List the file names of our two results files from JATOS...
list.files("data", pattern = "jatos_results", full.names = TRUE) %>%
  # ... feed into our custom function...
  map(read_jatos) %>%
  # ... and combine them into a single data set
  bind_rows() -> dat_all

# Create a new column with participant IDs
dat_all %<>% fill(url.srid, .direction = "down") %>% mutate(id = factor(url.srid))

# How many participants do we have?
(n <- n_distinct(dat_all$id))

###############################################################################
## Exercise 3.1: Extract the experimental trials and code their condition
###############################################################################

# Thus far, our data set ("dat_all") contains a row for every event that has happened on the
# screen, not just the main experiment. Therefore, we first need to extract the relevant trials,
# which are the only rows whose which have the values "positive" or "negative" in the column
# "correctResponse". We also need to exlucde the practice trials, which we can do by selecting
# only trials where the column "practice" is NA (not available). Finally, we need to convert
# the information of the response of the participant was correct or not (stored in the column
# "correct") to a numeric variable (e.g. 1 and 0).

# Start with the raw data...
dat_all %>%
  # ... filter only experimental trials...
  filter(correctResponse %in% c("positive", "negative")) %>%
  # ... filter out practise trials...
  filter(is.na(practice)) %>%
  # ... and convert accuracy (correct or not) to a numeric variable
  mutate(acc = recode(correct, "TRUE" = 1, "FALSE" = 0)) -> dat_exp

# Next, we need to code the experimental conditions (independent variables) for our trials.
# We do this by extracting the type of agent (human or robot) and eye gaze (open or closed) from
# The filename of the image which was shown during the trial (column "image"). The information
# for our third independent variable (word valence, positive or negative) is already stored in
# the column "correctResponse".

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

# Let's get an overview over the number of trials per participants in each condition
dat_exp %>% count(id, agent, gaze, valence)

###############################################################################
## Exercise 3.2: Compute by-participant condition averages of accuracy
###############################################################################

# To do this, you need to group the data by the relevant grouping variables (participant id and
# our three independet variables) and compute the mean accuracy for each group. of trials. In the
# tidyverse, this averaging is done via the "group_by" and "summarize" commands.

# Start with the experimental data...
(dat_exp %>%
  # ... and compute by-participant averages for each condition
  group_by(id, agent, gaze, valence) %>%
  summarize(acc = mean(acc)) -> dat_avg)

###############################################################################
## Exercise 3.3: Run a repeat-measures ANOVA on these averaged accuracies
###############################################################################

# There's multiple ways to do this in R, but the most intuitive one is probably the "ezANOVA"
# function from the ez package. We just need to specify the names of our data set, our
# dependent variables, and our within-participant indepedent variables. Here we also use the
# base R function "aov", were we need to be a bit more explicit about the error term for our
# within design. Note that both lead to the same results.

# Run repeated-measures ANOVA, ez style
ezANOVA(data = dat_avg, dv = acc, wid = id, within = c("agent", "gaze", "valence"))

# Run repeated-measures ANOVA, base R style
mod <- aov(acc ~ agent * gaze * valence + Error(id / (agent * gaze * valence)), data = dat_avg)
summary(mod)

###############################################################################
## Exercise 3.4: Perform post-hoc t-tests for the significant factors
###############################################################################

# Spoiler alert! In the ANOVA, we find a three-way interaction between agent, gaze, and valence.
# To understand what this effecte means, we can use the emmeans package to compute follow-up
# contrasts. For example, we may be interested in the difference between positive and negative
# words seperately for humans with open eyes, robots with open eyes, humans with closed eyes, and
# robots with closed eyes. This can be specified in emmeans using a pair-wise contrast of valence
# within ("|") the different combinations of agent and gaze.

emmeans::emmeans(mod, specs = pairwise ~ valence | (agent * gaze))

###############################################################################
## Exerciese 3.5: Visualize the results
###############################################################################

# We can use tidyverse's ggplot package to create publication-ready plots - but understanding
# the syntax is rather complicated. You first create an empty ggplot which has some "aesthetics",
# that specfications for which variables to display on the x-axis (usually one of the IVs), on the
# y-axis (usually the DV), and in different colors and/or shapes (usually other IVs). Via plus
# symbols ("+"), we then add layers to the plots which show the actual data (e.g. as dots or
# or lines (so-called "geoms"). We can also different themes and other aspects of styling and
# text (e.g. axis labels) to the plot.

# Start with the averaged data...
dat_avg %>%
  # ... create a plot and specify the variables for axes and colors...
  ggplot(aes(x = valence, y = acc, color = agent, group = agent)) +
  # ... create two seperate subplots for open and closed eyes...
  facet_grid(~gaze) +
  # ... plot grand means (with standard errors) across participants...
  stat_summary(fun.data = mean_se, position = position_dodge(width = 0.2)) +
  # ... also plot some lines connecting these means...
  stat_summary(fun = mean, geom = "line", position = position_dodge(width = 0.2)) +
  # ... select which range of accuracies we want to show (here: 85-100%)
  coord_cartesian(ylim = c(0.85, 1.0)) +
  # ... and apply an APA-conform theme
  theme_classic()

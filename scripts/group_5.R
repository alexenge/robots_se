###################################################################################################
# GROUP 5: ATTITUDES TOWARDS & EXPERIENCE WITH ROBOTS
###################################################################################################

###############################################################################
## Exercise 0: Read data into R
###############################################################################

# Install the relevant packages (you only need to do this once)
install.packages(c("jsonlite", "tidyverse", "magrittr", "knitr", "psych"))

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
## Exercise 5.1: What are the dimensions to evaluate ATAI?
###############################################################################

# Take a look at the following paper:
#
# Sindermann, C., Sha, P., Zhou, M., Wernicke, J., Schmitt, H. S., Li, M., ... & Montag, C. (2020).
# Assessing the Attitude Towards Artificial Intelligence: Introduction of a Short Measure in
# German, Chinese, and English Language. KI-Künstliche Intelligenz, 1-10.
# https://doi.org/10.1007/s13218-020-00689-0.
#
# What are the dimensions they are using to operationalize attitudes towards AI? Which questions
# are used to measure these dimensions?

###############################################################################
## Exercise 5.2: How did you score on fear of and acceptance towards AI?
###############################################################################

# You first need to extract from the raw data (which contain one row for everything that happend
# during the experiment) those trials which belong to the ATAI questions. These rows can be found
# by looking for the label "AttitudesAI" in the column called "sender". The actual rating data for
# these questions is stored in columns called "angstKI", "vertrauenKI", etc. Try to extract these
# rows and columns as a new tibble.

# Start with the raw data
(dat_all %>%
  # Filter the relevant questionnaire trials
  filter(sender == "AttitudesAI") %>%
  # Select only the relevant columns
  select(contains("KI")) %>%
  # Convert the relevant variables to numeric
  mutate(across(.fns = as.numeric)) -> atai_raw)

###############################################################################
## Exercise 5.3: Summarise and plot the mean ATAI scores
###############################################################################

# Using the extracted raw ATAI data, you now need to compute each participant's score on the two
# dimensions (fear and acceptance) by computing the mean of the respective questions. For fear of
# AI, you need to compute the mean across the three columns "angstKI", "ZerstoerungKI", and
# "ArbeitslosikgkeitKI". For acceptance of AI, you need to compute the mean across the columns
# "vetrauenKI" and "BereicherungKI". Store these scores as a new tibble. You can then also compute
# the average scores on both subscales across all participants. Please also try to plot the
# individual scores in at least two different ways.

# Compute mean scores for each participant...
(atai_raw %>% transmute(
  # ... on the "fear of AI" subscale...
  fear = (angstKI + ZerstoerungKI + ArbeitslosigkeitKI) / 3,
  # ... and on the "acceptance of AI" subscale
  acceptance = (vetrauenKI + BereicherungKI) / 2
) -> atai)

# Mean scores accross participants
atai %>% summarise(fear = mean(fear), acceptance = mean(acceptance))

# A scatterplot of the individual scores helps to see if fear and acceptance are correlated:
atai %>% ggplot(aes(x = fear, y = acceptance)) +
  # Add data points...
  geom_point() +
  # ... show the entire range of the rating scale (0-10)...
  coord_cartesian(xlim = c(0, 10), ylim = c(0, 10)) +
  # ... and choose an APA-conform theme
  theme_classic()

# Of course, we can also compute this correlation numerically
cor.test(atai$fear, atai$acceptance)

# A "density plot" helps to see how fear and acceptance are distributed:
atai %>% ggplot() +
  # Add the distribution of the fear scores...
  geom_density(aes(x = fear, fill = "fear"), alpha = 0.5) +
  # ... and the distribution of the acceptance scores...
  geom_density(aes(x = acceptance, fill = "acceptance"), alpha = 0.5) +
  # ... pick different colors for fear and acceptance...
  scale_fill_manual(values = c(fear = "red", acceptance = "green")) +
  # ... show the entire range of the rating scale (0-10)...
  xlim(c(0, 10)) +
  # ... add a label for the x-axis...
  labs(x = "rating", fill = "distributions") +
  # ... and choose an APA-conform theme
  theme_classic()

###############################################################################
## Exercise 5.4: What is your average experience with robots?
###############################################################################

# Take a look at the question with the "Experience with Robots" (column "sender"). There,
# participants gave a categorical response in the column called "German". Try to extract the
# frequencies of the difference options and/or try to plot them.

# Start with the raw data...
(dat_all %>%
  # ... filter the questionnaire trial...
  filter(sender == "Experience with Robots") %>%
  # ... and count the frequencies of the difference responses
  count("experience" = German, name = "frequency") -> experience)

# For plotting...
experience %>%
  # ... we first factorize the categories so they are displayed in the right order...
  mutate(experience = factor(experience, levels = c("keine", "wenige", "mehrere", "erheblich"))) %>%
  # ... and create a frequency plot (basically a histogram)...
  ggplot(aes(x = experience, y = frequency)) +
  geom_bar(stat = "identity") +
  # ... we want to show all possible categories (even if they are empty)...
  scale_x_discrete(drop = FALSE) +
  # ... and we want to use an APA-conform theme
  theme_classic()

###############################################################################
## Exercise 5.5: Assess reliability of the ATAI subscales
###############################################################################

# Cronbach's alpha is a (suboptimal but) common measure for the internal consistency of a
# psychometric scale. Using the function "alpha()" from the psych package, you can easily
# compute Cronbach's alpha for the fear and acceptance subscales of the ATAI questionnaire
# (you should have already created the relevant tibble in exercise 5.3). In the output, focus on
# the standardized alpha ("std.alpha") and interpret it keeping in mind the length of the scale
# and the reliability estimates reported in the Sindermann et al. (2020) paper.

# Internal consistency of the fear subscale of the ATAI
atai_raw %>% select(angstKI, ZerstoerungKI, ArbeitslosigkeitKI) %>% psych::alpha()

# Internal consistency for the acceptance subscale of the ATAI
atai_raw %>% select(vetrauenKI, BereicherungKI) %>% psych::alpha()

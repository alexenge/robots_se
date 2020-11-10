###################################################################################################
# GROUP 5: ATTITUDES TOWARDS AI (ATAI)
###################################################################################################

# Load the relevant packages
library(tidyverse)

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
## Exercise 5.2: Read the data in R
###############################################################################

# Start by familiarizing yourself with information stored in the file "dat_atai.csv". You can open
# this file using a text editor (Editor/Notepad on Windows, TextEdit on Mac) or in a spreadsheet
# software (Microsoft Excel or Apple Numbers). Then, try to load the data into R, e.g. using the
# read_csv() function from the tidyverse.

# Read the rating data set
read_csv("data/dat_atai.csv") -> dat_atai

# How many participants do we have?
(dat_atai %>% pull(id) %>% n_distinct() -> n)

###############################################################################
## Exercise 5.3: Calculate the ATAI scores
###############################################################################

# Using the from exercise 5.2 ATAI data, you now need to compute each participant's score on the
# two dimensions (fear and acceptance) by computing the mean of the respective questions. For fear
# of AI, you need to compute the mean across the three columns "fair_ai", "destroy_ai", and
# "unemploy_ai". For acceptance of AI, you need to compute the mean across the columns "trust_ai"
# and "enricht_ai". Store these scores as a new object. You can then also compute the average
# scores on the two subscales across all participants.

# Start with the ATAI data...
dat_atai %>%
  # ... create new columns...
  transmute(
    # ... for participants' fear of AI score...
    fear = (fear_ai + destroy_ai + unemploy_ai) / 3,
    # ... and for participants' acceptance of AI score...
    acceptance = (trust_ai + enrich_ai) / 2
    # And save these scores as a new object
  ) -> scores_atai

# We can also compute the averaged scores across participants ("grand averages")
scores_atai %>% summarise(fear = mean(fear), acceptance = mean(acceptance))

###############################################################################
## Exercise 5.4: Plot the ATAI scores
###############################################################################

# Please plot the ATAI scores you've computed in Exericse 5.3 in at least two different ways. For
# instances, a scatterplot (with one subscale on the x-axis and the other on the y-axis) will help
# you to see if their is a systematic relationship between the two subscales. You can also plot the
# frequencies of different scores as histogram or denisty plot.

# Let's create an empty canvas for a scatterplot...
scores_atai %>% ggplot(aes(x = fear, y = acceptance)) +
  # ... add data points...
  geom_point() +
  # ... show the entire range of the rating scale (0-10)...
  coord_cartesian(xlim = c(0, 10), ylim = c(0, 10)) +
  # ... fit a regression line...
  geom_smooth(method = "lm") +
  # ... and choose an APA-conform theme
  theme_classic()

# Of course, we can also compute (and test) this correlation numerically
cor.test(scores_atai$fear, scores_atai$acceptance)

# Let's create an empty canvas for a density plot...
scores_atai %>% ggplot() +
  # ... add the distribution of the fear scores...
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
## Exercise 5.5: Assess reliability of the ATAI subscales
###############################################################################

# Cronbach's alpha is a common (though suboptimal) measure for the internal consistency of a
# psychometric scale. Using the function "alpha()" from the psych package, you can easily compute
# Cronbach's alpha for the fear and acceptance subscales of the ATAI questionnaire (use the raw
# ATAI data you've created in exercise 5.1). In the output, focus on the standardized alpha
# ("std.alpha") and interpret it keeping in mind the length of the scale and the reliability
# estimates reported in the Sindermann et al. (2020) paper.

# Internal consistency of the fear subscale of the ATAI
dat_atai %>% select(fear_ai, destroy_ai, unemploy_ai) %>% psych::alpha()

# Internal consistency for the acceptance subscale of the ATAI
dat_atai %>% select(trust_ai, enrich_ai) %>% psych::alpha()

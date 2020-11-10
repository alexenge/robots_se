###################################################################################################
# GROUP 3: ANALYSIS OF ACCURACY DATA
###################################################################################################

# Load the relevant packages
library(tidyverse)
library(ez)
library(emmeans)

###############################################################################
## Exercise 3.1: Read the data into R
###############################################################################

# Start by familiarizing yourself with information stored in the file "dat_exp.csv". You can open
# this file using a text editor (Editor/Notepad on Windows, TextEdit on Mac) or in a spreadsheet
# software (Microsoft Excel or Apple Numbers). Then, try to load the data into R, e.g. using the
# read_csv() function from the tidyverse.

# Read the main experimental data set
read_csv("data/dat_exp.csv") -> dat_exp

# How many participants do we have?
(dat_exp %>% pull(id) %>% n_distinct() -> n)

###############################################################################
## Exercise 3.2: Compute the average accuracies for each participant in each condition
###############################################################################

# Try to make out which column codes the participant IDs and which columns code our experimental
# factors (remember: There are 3 of them!). Use the "group_by()" function from the tidyverse to
# group your data by all of these columns. Then, you can compute the average accuracy (column
# "correct") for each of these groups of trials, using the "summarize()" function.

# Start with the correct trials...
(dat_exp %>%
   # ... and group by participant IDs and conditions...
   group_by(id, agent, gaze, valence) %>%
   # ... and compute the mean accuracy for each group of trials
   summarize(avg_correct = mean(correct)) -> dat_avg)

###############################################################################
## Exercise 3.3: Run a repeat-measures ANOVA on these averaged accuracies
###############################################################################

# There's multiple ways to do ANOVAS in R, but the most intuitive one is the "ezANOVA" function
# from the ez package. We just need to specify the name of our data set, and the column  our names
# of our participant IDs (argument "wid"), our dependent variable (argument "dv"), and our within-
# participant indepedent variables (argument "within").

# Run repeated-measures ANOVA, ez style
ezANOVA(data = dat_avg, dv = avg_correct, wid = id, within = c("agent", "gaze", "valence"))

# You can also use the base R function "aov", were you need to be a bit more explicit about the
# error term for our within-participants design. Note that both ways lead to the same result.

# Run repeated-measures ANOVA, base R style
mod <- aov(
  avg_correct ~ agent * gaze * valence + Error(factor(id) / (agent * gaze * valence)),
  data = dat_avg
)
summary(mod)

###############################################################################
## Exercise 3.4: Perform post-hoc t-tests for the significant interactions
###############################################################################

# Spoiler alert! In the ANOVA, we find a three-way interaction between agent, gaze, and valence.
# To understand what this effecte means, we can use the emmeans package to compute follow-up
# contrasts. For example, we may be interested in the difference between positive and negative
# words seperately for humans with open eyes, robots with open eyes, humans with closed eyes, and
# robots with closed eyes. This can be specified in emmeans using a pair-wise contrast of valence
# within the different combinations of agent and gaze.

# Compute pairwise follow-up contrasts of word valence within different levels of agents and gaze
emmeans::emmeans(mod, specs = pairwise ~ valence | (agent * gaze))

# Which contrast is responsible for observing the three-way interaction? Do you have an idea why
# this effect may occur?

###############################################################################
## Exercise 3.5: Visualize the results
###############################################################################

# We can use tidyverse's ggplot package to create publication-ready plots - but understanding
# the syntax is rather complicated. You first create an empty ggplot which has some "aesthetics",
# that is specfications for which variables to display on the x-axis (usually one of the IVs), on
# the y-axis (usually the DV), and in different colors and/or shapes (usually other IVs). With plus
# symbols (+), we then add layers to the plots which show the actual data (e.g. dots or or lines
# (so-called "geoms"). We can also add different themes and other aspects of styling and text (e.g.
# axis labels) to the plot.

# Start with the averaged data...
dat_avg %>%
  # ... create a plot and specify the variables for axes and colors...
  ggplot(aes(x = valence, y = avg_correct, color = agent, group = agent)) +
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

# How does this plot fit with your findings from the ANOVA and follow-up contrasts?

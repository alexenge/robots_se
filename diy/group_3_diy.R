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

...

###############################################################################
## Exercise 3.2: Compute the average accuracies for each participant in each condition
###############################################################################

# Try to make out which column codes the participant IDs and which columns code our experimental
# factors (remember: There are 3 of them!). Use the "group_by()" function from the tidyverse to
# group your data by all of these columns. Then, you can compute the average accuracy (column
# "correct") for each of these groups of trials, using the "summarize()" function.

...

###############################################################################
## Exercise 3.3: Run a repeat-measures ANOVA on these averaged accuracies
###############################################################################

# There's multiple ways to do ANOVAS in R, but the most intuitive one is the "ezANOVA" function
# from the ez package. We just need to specify the name of our data set, and the column  our names
# of our participant IDs (argument "wid"), our dependent variable (argument "dv"), and our within-
# participant indepedent variables (argument "within").

...

###############################################################################
## Exercise 3.4: Perform post-hoc t-tests for the significant interactions
###############################################################################

# Spoiler alert! In the ANOVA, we find a three-way interaction between agent, gaze, and valence.
# To understand what this effecte means, we can use the emmeans package to compute follow-up
# contrasts. For example, we may be interested in the difference between positive and negative
# words seperately for humans with open eyes, robots with open eyes, humans with closed eyes, and
# robots with closed eyes. This can be specified in emmeans using a pair-wise contrast of valence
# within the different combinations of agent and gaze.

...

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

...

# How does this plot fit with your findings from the ANOVA and follow-up contrasts?

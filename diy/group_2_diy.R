###################################################################################################
# GROUP 2: ANALYSIS OF REACTION TIMES
###################################################################################################

# Load the relevant packages
library(tidyverse)
library(ez)

###############################################################################
## Exercise 2.1: Read the data into R
###############################################################################

# Start by familiarizing yourself with information stored in the file "dat_exp.csv". You can open
# this file using a text editor (Editor/Notepad on Windows, TextEdit on Mac) or in a spreadsheet
# software (Microsoft Excel or Apple Numbers). Then, try to load the data into R, e.g. using the
# read_csv() function from the tidyverse.

...

###############################################################################
## Exercise 2.2: Compute the average RTs for each participant in each condition
###############################################################################

# First, in reaction time analysis we usually want to exclude trials were participant made an
# incorrect response (at least when the task is easy). You can do this by filtering your data
# on the information stored in the column called "correct".

...

# Try to make out which column codes the participant IDs and which columns code our experimental
# factors (remember: There are 3 of them!). Use the "group_by()" function from the tidyverse to
# group your data by all of these columns. Then, you can compute the average RT (column "rt")
# for each of these groups of trials, using the "summarize()" function.

...

# Why does the new, averaged data set have exactly 184 rows?

###############################################################################
## Exercise 2.3: Run a repeated-measures ANOVA on these averaged RTs
###############################################################################

# There's multiple ways to do ANOVAS in R, but the most intuitive one is the "ezANOVA" function
# from the ez package. We just need to specify the name of our data set, and the column  our names
# of our participant IDs (argument "wid"), our dependent variable (argument "dv"), and our within-
# participant indepedent variables (argument "within").

...

###############################################################################
## Exercise 2.4: Perform post-hoc t-tests for the significant interactions
###############################################################################

# Spoiler alert! In the ANOVA, we find a significant effect of valence and an interaction effect
# of agent and valence. To understand what these effects mean, we can decompose them into a set
# of t-tests, testing the difference between RTs for positive and negative words separately for
# human and robot primes. To do this, we first need to average across the factor of eye gaze
# (which doesn't seem to matter according to the ANOVA). You can do so by using the same procedure
# of "group_by" and "summarize" as in 2.2, but this time we don't use "gaze" as a grouping
# variable. You can then perform t-tests on subsets of these data (one for human and one for
# robobt agents). Each t-test should test for differences in RTs between positive and negative
# words (column "valence").

...

###############################################################################
## Exercise 2.5: Visualize the results
###############################################################################

# We can use tidyverse's ggplot package to create publication-ready plots - but understanding
# the syntax is rather complicated. You first create an empty ggplot which has some "aesthetics",
# that is specfications for which variables to display on the x-axis (usually one of the IVs), on
# the y-axis (usually the DV), and in different colors and/or shapes (usually other IVs). With plus
# symbols (+), we then add layers to the plots which show the actual data (e.g. dots or or lines
# (so-called "geoms"). We can also add different themes and other aspects of styling and text (e.g.
# axis labels) to the plot.

...

# How does this plot fit with your findings from the ANOVA and t-tests?

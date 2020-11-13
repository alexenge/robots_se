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

# Read the main experimental data set
read_csv("data/dat_exp.csv") -> dat_exp

# How many participants do we have?
(dat_exp %>% pull(id) %>% n_distinct() -> n)

###############################################################################
## Exercise 2.2: Compute the average RTs for each participant in each condition
###############################################################################

# First, in reaction time analysis we usually want to exclude trials were participant made an
# incorrect response (at least when the task is easy). You can do this by filtering your data
# on the information stored in the column called "correct".

# Filter only correct trials
dat_exp %>% filter(correct == 1) -> dat_correct

# Try to make out which column codes the participant IDs and which columns code our experimental
# factors (remember: There are 3 of them!). Use the "group_by()" function from the tidyverse to
# group your data by all of these columns. Then, you can compute the average RT (column "rt")
# for each of these groups of trials, using the "summarize()" function.

# Start with the correct trials...
(dat_correct %>%
    # ... and group by participant IDs and conditions...
    group_by(id, agent, gaze, valence) %>%
    # ... and compute the mean RT for each group of trials
    summarize(avg_rt = mean(rt)) -> dat_avg)

# Why does the new, averaged data set have exactly 184 rows?

###############################################################################
## Exercise 2.3: Run a repeated-measures ANOVA on these averaged RTs
###############################################################################

# There's multiple ways to do ANOVAS in R, but the most intuitive one is the "ezANOVA" function
# from the ez package. We just need to specify the name of our data set, and the column  our names
# of our participant IDs (argument "wid"), our dependent variable (argument "dv"), and our within-
# participant indepedent variables (argument "within").

# Run repeated-measures ANOVA
ezANOVA(data = dat_avg, dv = avg_rt, wid = id, within = c("agent", "gaze", "valence"))

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

# Start with the correct trials...
(dat_correct %>%
   # ... and group by participant IDs and conditions (excluding gaze)...
   group_by(id, agent, valence) %>%
   # ... and compute the mean RT for each group of trials
   summarize(avg_rt = mean(rt)) -> dat_posthoc)

# Paired-sample t-test for the effect of word valence with human primes
t.test(avg_rt ~ valence, data = dat_posthoc %>% filter(agent == "human"), paired = TRUE)

# Paired-sample t-test for the effect of word valence with robot primes
t.test(avg_rt ~ valence, data = dat_posthoc %>% filter(agent == "robot"), paired = TRUE)

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

# Start with the averaged data...
dat_avg %>%
  # ... create a plot and specify the variables for axes and colors...
  ggplot(aes(x = valence, y = avg_rt, color = agent, group = agent)) +
  # ... create two seperate subplots for open and closed eyes...
  facet_grid(~gaze) +
  # ... plot grand means (with standard errors) across participants...
  stat_summary(fun.data = mean_se, position = position_dodge(width = 0.2)) +
  # ... also plot some lines connecting these means...
  stat_summary(fun = mean, geom = "line", position = position_dodge(width = 0.2)) +
  # ... select which range of RTs we want to show (here: 600-800 ms)
  coord_cartesian(ylim = c(600, 800)) +
  # ... and apply an APA-conform theme
  theme_classic()

# How does this plot fit with your findings from the ANOVA and t-tests?

###############################################################################
## Add-on: Linear mixed effects-model instead of ANOVA
###############################################################################

# Linear mixed-effects models are an extension of multiple regression and offer an alternative
# approach to analysing experimental data with some advantages compared to ANOVAs. First, they
# can analyse data on the level of single trials (instead of by-participant averages), which
# leads to better statistical power. Second, they take into account the non-indepedence of data
# points coming from the same items (whereas ANOVA only takes into account the non-independence
# of data points coming from the same participant). This leads to better control of the Type I 
# (alpha) error rate.

# Load some new packages
library(afex)
library(emmeans)

# Run linear mixed-effects model
lmm <- lmer_alt(
  rt ~ agent * gaze * valence + (agent * gaze * valence||id) + (agent * gaze * valence||word),
  data = dat_exp,
  check_contrasts = TRUE,
  control = lmerControl(calc.derivs = FALSE, optimizer = "bobyqa")
)

# Show ANOVA-style output
anova(lmm)

# Re-compute the follow-up contrast for valence by agent
emm_options(lmer.df = "Satterthwaite", lmerTest.limit = Inf)
emmeans(lmm, specs = pairwise ~ valence|agent)

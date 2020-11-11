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

...

###############################################################################
## Exercise 5.3: Calculate the ATAI scores
###############################################################################

# Using the from exercise 5.2 ATAI data, you now need to compute each participant's score on the
# two dimensions (fear and acceptance) by computing the mean of the respective questions. For fear
# of AI, you need to compute the mean across the three columns "fair_ai", "destroy_ai", and
# "unemploy_ai". For acceptance of AI, you need to compute the mean across the columns "trust_ai"
# and "enricht_ai". Store these scores as a new object. You can then also compute the average
# scores on the two subscales across all participants.

...

###############################################################################
## Exercise 5.4: Plot the ATAI scores
###############################################################################

# Please plot the ATAI scores you've computed in Exericse 5.3 in at least two different ways. For
# instances, a scatterplot (with one subscale on the x-axis and the other on the y-axis) will help
# you to see if their is a systematic relationship between the two subscales. You can also plot the
# frequencies of different scores as histogram or denisty plot.

...

###############################################################################
## Exercise 5.5: Assess reliability of the ATAI subscales
###############################################################################

# Cronbach's alpha is a common (though suboptimal) measure for the internal consistency of a
# psychometric scale. Using the function "alpha()" from the psych package, you can easily compute
# Cronbach's alpha for the fear and acceptance subscales of the ATAI questionnaire (use the raw
# ATAI data you've created in exercise 5.1). In the output, focus on the standardized alpha
# ("std.alpha") and interpret it keeping in mind the length of the scale and the reliability
# estimates reported in the Sindermann et al. (2020) paper.

...

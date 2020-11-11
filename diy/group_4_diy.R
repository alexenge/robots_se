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

...

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

...

###############################################################################
## Exercise 4.3: How did you evaluate the agency of humans and robots?
###############################################################################

# Here you can do exactly the same as in Exercise 4.2, but using the column for the agency ratings
# (rather then the intentionality ratings).

...

###############################################################################
## Exercise 4.4: Did eye gaze influence the intentionality and agency ratings?
###############################################################################

# To do this you can perform the same steps as in Exercises 4.2 and 4.3, but add the factor "gaze"
# (open or closed) as an additional grouping variable.

...

###############################################################################
## Exercise 4.5: Create a table and plot as an overview of the rating data
###############################################################################

# Finally, let's create a table and a plot to visualize all our rating results at once. One way
# to do this is to combine the rating tibbles we've created in Exercise 4.4 (which include the
# rating data by participants, agents, and gaze) and display them together in one table and in one
# plot. You can again use "group_by()" and "summarise()" to compute the grand averages across
# participants and then show these values in any way you find intuitive.

...

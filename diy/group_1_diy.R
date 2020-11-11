###################################################################################################
# GROUP 1: DATA QUALITY CHECK
###################################################################################################

# Load the relevant packages
library(tidyverse)

###############################################################################
## Exercise 1.1: Read the data into R
###############################################################################

# Start by familiarizing yourself with information stored in the file "dat_misc.csv". You can open
# this file using a text editor (Editor/Notepad on Windows, TextEdit on Mac) or in a spreadsheet
# software (Microsoft Excel or Apple Numbers). Then, try to load the data into R, e.g. using the
# read_csv() function from the tidyverse. Do the same for the file "dat_exp.csv" which contains the
# trials from the main experiment.

...

###############################################################################
## Exercise 1.2: How long did it take participants to complete the main task?
###############################################################################

# The data set "dat_misc" contains information about every event that happened on participants'
# screens (column "event") together with the time that this event ended ("time_end"). You can use
# this information to calculate how long each participant took on the main task. The main task
# began with the end of the event "Demographic questions" and ended with the of the event "End of
# task". The difference between these two time points is the duration of the main task in milli-
# seconds. Try to compute this for all participants. If you want to and find the time, you can also
# try to plot these durations as a histogram.

...

###############################################################################
## Exercise 1.3: Did participants respond correctly to the quality check?
###############################################################################

# In the "event" column, of "dat_misc" there is one event called "QC" (quality check). This was
# the question where participants were required to respond with "Chicago" despite the fact that
# the question asked for a city that was not in the US. You can retrieve participants' responses
# to this question, which is stored in a column called "text". You can then calculate the
# percentage of participants that responded (correctly) with "Chicago".

...

###############################################################################
## Exercise 1.4: How many errors did participants make in the main task?
###############################################################################

# The data set "data_exp" contains all the trials from th e main experimental task. In there,
# you'll find one column which indicates if the response of the participant (positive or negative
# word) was correct (1) or not (0). Use this information to count how many errors each participant
# made, e.g. by using the "filter()" and "count()" functions from the tidyverse. Again, if you
# want to, try to plot the errors per participant as a histogram.

...

###############################################################################
## Exercise 1.5: How many outlier reaction times (RTs) did participants make?
###############################################################################

# This is quite advanced! Start by noticing "dat_exp" contains participants' reaction times (RTs)
# in the main experiment. We can use these to check if / how many outlier RTs participants made.
# To do so, we first need to compute each participants mean RT. This is done using the "group_by"
# function to group trials from the same participant and then adding a new column using the "mutate"
# function to compute the mean RT of this participant and another new column for the SD of this
# participants' RTs. Then, you can check for every trial if the actual RT in this trial was above
# or below a certain outlier threshold. A common choice is the mean +/- 2.5 standard devations.
# Try to count for how many trials per participant this was the case.

...

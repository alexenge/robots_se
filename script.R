#############################################################
## -	load data into r (jatos | lab.js output)
#############################################################

# # Install the pacman package (this only needs to be done once!)
# install.packages("pacman")
# 
# # Install/load all other necessary packages
# pacman::p_load("tidyverse", "jsonlite")

# Install missing packages
renv::restore(prompt = FALSE)

# Load packages
library(tidyverse)
library(jsonlite)

# Read the text file from JATOS ...
read_file("jatos_results_20201023124324") %>%
  # ... split it into rows ...
  str_split("\n") %>% first() %>%
  # ... filter empty rows ...
  discard(function(x) x == "") %>%
  # ... parse JSON into a data.frame
  map_dfr(fromJSON, flatten = TRUE) -> data_raw

##############################################################
# Excercises
#############################################################

##############################################################
# Excercise 1: Data Quality Check Group [done!]
#############################################################

#############################################################
## -	How long did it take participants to complete the task?
#############################################################

# Retrieve the end time of the "End of task" event (i.e. the end of the actual experiment)
time_end <- data_raw %>% filter(sender == "End of Task") %>% pull(time_end)

# Retrieve the end time of the "Demographic questions" event (i.e. directly before the actual experiment)
time_start <- data_raw %>% filter(sender == "Demographic_questions") %>% pull(time_end)

# Compute the difference between the two and convert from ms to s to min
(time_task <- (time_end - time_start) / 1000 / 60)

#############################################################
## - How many participants responded correctly on the Quality Control Question?
#############################################################

# Retrieve participants' answer to the question "QC" (quality check)
(qual_check <- data_raw %>% filter(sender == "QC") %>% pull(German))

# Check how many of them respneded (correctly) with "Chicago".
(qual_check == "Chicago") %>% sum()

#############################################################
## - How many errors did participants make?
#############################################################



# create participant IDs (probably not the most elegant code. But it works..)
nSubs<-3 # how many participants? | Important: Specify number of participants, change if needed!!
# no user input required from here to end of excercise
dim(data_raw)->dd #how many rows in total?
matrix(which(data_raw$sender %in% "Welcome"))->w #get row indices of "Welcome"s
dd[1]->w[nSubs+1] #add number of rows to w

data_raw$id<-0 #create new column ID
for (i in c(1:nSubs-1)){
  if (i==0){data_raw$id[w[i+1]:w[i+2]]<-(i+1)}
  else {data_raw$id[(w[i+1]+1):w[i+2]]<-(i+1)}
}

# do the job
data_raw %>%
  #filter practise trials
  filter(is.na(practice)) %>% 
  #filter only reactions
  filter(correctResponse=="positive"|response=="negative") %>% 
  # count cases of correct, depending on ID
  count(correct,id) -> errors

#############################################################
## -	How many outliers were there?
#############################################################

# recode primes into factors using recode_factos(), first argument: variable to recode, followed by list of recodings, explicit recoding of all robots (or open eyes), humans (or eyes closed) are recoded by default
data_raw$agent <- recode_factor(data_raw$image,robot_open_1.jpg = "robot",robot_open_2.jpg = "robot",robot_open_3.jpg = "robot",robot_closed_1.jpg = "robot",robot_closed_2.jpg = "robot",robot_closed_3.jpg = "robot",.default = "human")
data_raw$gaze <- recode_factor(data_raw$image,robot_open_1.jpg = "open",robot_open_2.jpg = "open",robot_open_3.jpg = "open",human_open_1.jpg = "open",human_open_2.jpg = "open",human_open_3.jpg = "open",.default = "closed")

data_raw %>% 
  #filter out practise trials
  filter(is.na(practice)) %>% 
  #filter only targets
  filter(correctResponse=="positive"|response=="negative") %>% 
  # filter only correct answers
  filter(correct=="TRUE") %>% 
  # group by IVs
  group_by(id,agent,gaze,correctResponse) %>% 
  # identify outliers (Tukey fences) in each condition
  summarize(boxplot.stats(duration)$out)  %>% 
  # ungroup
  ungroup() %>% 
  # now we count rows per participant, ie how many ouliers per participant across conditions 
   count(id) -> numOutlier

##############################################################
# Excercise 2: Prepare ANOVA RT overview [done!]
#############################################################

#############################################################
## -	Could you walk us through the ANOVA results?
#############################################################  

#SET FACTORS FOR ANOVA
data_raw$agent<- factor(data_raw$agent, levels = c("human", "robot")) # we already created agent above
data_raw$gaze <- factor(data_raw$gaze, levels = c("open", "closed")) # we already created gaze above
data_raw$Word_Valence <- recode_factor(data_raw$response,negative = "negative",positive = "positive")
data_raw$Word_Valence <- factor(data_raw$Word_Valence, levels = c("positive", "negative"))

data_raw %>% 
  #filter out practise trials
  filter(is.na(practice)) %>% 
  #filter only targets
  filter(correctResponse=="positive"|response=="negative") %>% 
  # filter only correct answers
  filter(correct=="TRUE") -> data1

#RUN ANALYSIS OF VARIANCE 
# Compute the analysis of variance Agent*Gaze*Word_Valence
res.aov <- aov(duration ~ agent*gaze*Word_Valence, 
               data = data1)
# Summary of the analysis
summary(res.aov)

#############################################################
## -	-	Could you walk us through the post-hoc tests?
#############################################################  

# we ran ANOVA in the last step, just do tukey tests:
TukeyHSD(res.aov)

#############################################################
## - Could you generate a table with means?
#############################################################    

group_by(data1, agent, gaze, Word_Valence) %>%
  summarise(
    count = n(),
    RT = mean(duration, na.rm = TRUE),
    sd = sd(duration, na.rm = TRUE),
    sem = sd(duration)/sqrt(length(duration))
  )->rob_means


#############################################################
## -	Could you plot a graph depicting the results and walk us through the plot?
#############################################################   
# PLOT MEANS

# specifiy what to plot, aes defines aesthetics: "group" connects the lines, "color" prints different colors and creates a legend
ggplot(rob_means, aes(x=gaze, y=RT,color=agent,group=agent)) + 
  # do the line plot
  geom_line()+ 
  # put dots at the end of the lines - looks better
  geom_point() +  
  # and add the error bars: specify a range (mean +-sem)
  geom_errorbar(aes(ymin=RT-sem, ymax=RT+sem), width=.2,position=position_dodge(0.05))  + 
  # and make sure that positive an negative words get their panel each
  facet_grid( ~ Word_Valence)
# flow: tell r to make a plot, specify geometry, the + binds different parts together that are actually superimposed one after the other, the last line is something like "all of the above, but in two panels"

##############################################################
# Excercise 3: Prepare ANOVA accuracy overview [not done - adapt from excercise 2]
#############################################################

##############################################################
# Excercise 4: Intentionality analysis + plot by Gaze & Agent [still missing]
#############################################################

#############################################################
#-	How did you evaluate intentionality of humans and robots?
#############################################################

#############################################################
#  -	How did you evaluate agency of humans and robots?
#############################################################

#############################################################
#  -	How did you evaluate intentionality eyes open eyes closed human robot?
#############################################################

#############################################################
#  -	How did you evaluate agency eyes open eyes closed human robot? 
#############################################################

#############################################################
#  -	Table + plot as an overview
#############################################################


##############################################################
# Excercise 5: Attitudes towards robots + Experience with Robots [partially done]
#############################################################

#############################################################
#-	What are the dimensions to evaluate ATAI?  (Acceptance, Fear)
#############################################################

#############################################################
#-	-	How did you score on Acceptance towards AI and Fear towards AI?
#############################################################
data_raw %>% 
  #filter questionnaire trials
  filter(sender=="AttitudesAI") %>% 
  # select only relevant columns
  select(contains("KI")) -> AttAi

AttAi$fear <- as.numeric(AttAi[,1])+as.numeric(AttAi[,3]) + +as.numeric(AttAi[,5])
AttAi$acceptance <- as.numeric(AttAi[,2])+as.numeric(AttAi[,4]) 

#############################################################
#-	Can you prepare a table with the scores & generate a plot to visually present the data? 
#############################################################

#############################################################
##  -	Can you present how the group rated experience with robots?
#############################################################   

data_raw %>% 
  #filter questionnaire trials
  filter(sender=="Experience with Robots") %>% 
  # select only relevant columns
  select(contains("German"))


#############################################################
## -	Assess reliability
#############################################################  
#require(ltm)
# reuse AttAi froom above
  AttAi[,c(2,4)] %>% 
    # Item 2 and 4 are acceptance scale
  cronbach.alpha(standardized = TRUE) -> acceptance # compute standardized alpha
   # Item 1 3 5 are fear scale
  AttAi[,c(0,2,4)] %>% 
    cronbach.alpha(standardized = TRUE) -> fear # compute standardized alpha
  
  

  

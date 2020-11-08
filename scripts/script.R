###################################################################################################
# FOR EVERYBODY: LOAD DATA FROM JATOS INTO R
###################################################################################################

# Install the pacman package (this only needs to be done once!)
install.packages("pacman")

# Install/load all other necessary packages (this may take a while when run for the first time)
pacman::p_load("jsonlite", "afex", "emmeans", "knitr", "tidyverse", "magrittr", "psych")

# Setting for printing stuff (nevermind)
options(tibble.print_max = 50)

# Define a custom function to read JATOS files
read_jatos <- function(file) {
  suppressMessages(
    # Read the JATOS results file...
    read_file(file) %>%
      # ... split it into rows ...
      str_split("\n") %>% first() %>%
      # ... filter empty rows ...
      discard(function(x) x == "") %>%
      # ... parse JSON into a data.frame ...
      map_dfr(fromJSON, flatten = TRUE) %>%
      # ... and convert to tibble (just a kind of pretty dataframe)
      as_tibble()
  )
}

# Apply this function to our own results files...
list.files(pattern = "jatos_results") %>%
  map(read_jatos) %>%
  # ... and bind the into a single tibble
  bind_rows() -> dat

###################################################################################################
# EXERCISE 1: Data Quality Check Group
###################################################################################################

###############################################################################
## -  How long did it take participants to complete the task?
###############################################################################

# Retrieve the end time of the "End of task" event (i.e. the end of the actual experiment)
time_end <- dat %>% filter(sender == "End of Task") %>% pull(time_end)

# Retrieve the end time of the "Demographic questions" (directly before the actual experiment)
time_start <- dat %>% filter(sender == "Demographic_questions") %>% pull(time_end)

# Compute the difference between the two and convert from ms to s to min
(time_task <- (time_end - time_start) / 1000 / 60)

# Plot a histogram
ggplot() +
  geom_histogram(aes(x = time_task), binwidth = 5, boundary = 10, color = "black", fill = "gray80") +
  theme_bw()

###############################################################################
## -  How many participants responded correctly on the Quality Control Question?
###############################################################################

# Retrieve participants' answer to the question "QC" (quality check)
(qual_check <- dat %>% filter(sender == "QC") %>% pull(German))

# Check how many of them responded (correctly) with "Chicago".
(qual_check == "Chicago") %>% sum()

###############################################################################
## -  How many errors did participants make?
###############################################################################

# Create a new column with participant IDs
dat %<>% fill(url.srid, .direction = "down") %>% mutate(id = url.srid)

# How many participants do we have?
(n <- n_distinct(dat$url.srid))

# Do the job
(dat %>%
    # Filter out practise trials
    filter(is.na(practice)) %>% 
    # Filter only the acutal reactions
    filter(correctResponse %in% c("positive", "negative")) %>% 
    # Count cases of correct, depending on ID
    count(id, correct) -> num_errors)

###############################################################################
## -  How many outliers were there?
###############################################################################

# Create new columns for our idenpendent variables...
dat %<>% mutate(
  # ... namely which type of agent was presented ...
  agent = case_when(
    str_detect(image, "human_") ~ "human",
    str_detect(image, "robot_") ~ "robot"
  ) %>% factor(levels = c("human", "robot")),
  # ... and if their eyes were open or closed.
  gaze = case_when(
    str_detect(image, "open") ~ "open",
    str_detect(image, "closed") ~ "closed"
  ) %>% factor(levels = c("open", "closed"))
)

# Do the job
(dat %>%
  # Filter out practise trials
  filter(is.na(practice)) %>% 
  # Filter only target trials
  filter(correctResponse %in% c("positive", "negative")) %>% 
  # Filter only correct responses
  filter(correct == "TRUE") %>% 
  # Group by participants and independent variables
  group_by(id, agent, gaze, correctResponse) %>% 
  # Identify outliers (Tukey fences) in each condition
  summarize(boxplot.stats(duration)$out) %>% 
  # Ungroup
  ungroup() %>% 
  # Now we count rows per participant, i.e. how many ouliers per participant across conditions 
  count(id, name = "outliers") -> num_outliers)

###################################################################################################
# EXERCISE 2: Prepare ANOVA RT overview
###################################################################################################

###############################################################################
## -  Could you walk us through the ANOVA results?
###############################################################################

# We already have factorized columns for agent and gaze.
# But we also need one for the valence of the target word.
dat %<>% mutate(word_valence = factor(correctResponse, levels = c("positive", "negative")))

# Create a new subset of the data
dat %>% 
  # Filter out practise trials
  filter(is.na(practice)) %>% 
  # Filter only target trials
  filter(correctResponse %in% c("positive", "negative")) %>% 
  # Filter only correct answers
  filter(correct == "TRUE") -> dat_rt

# Run analysis of variance (ANOVA) for agent x gaze x word_valence
mod_rt <- aov_ez(
  id = "id",
  dv = "duration",
  within = c("agent", "gaze", "word_valence"),
  data = dat_rt,
  fun_aggregate = mean
)

# Take a look at the results
summary(mod_rt)

###############################################################################
## -  Could you walk us through the post-hoc tests?
###############################################################################

# Compute follow-up pair-wise tests using the emmeans package
means_mod_rt <- emmeans(mod_rt, specs = pairwise ~ agent * gaze * word_valence)

# Retrieve means for all combinations of factor levels (useful e.g. for plotting)
means_mod_rt$emmeans

# Retrive contrasts between all combinations of factor levels
means_mod_rt$contrasts

###############################################################################
## -  Could you generate a table with means from the raw data?
###############################################################################

# This requires two steps of aggregation
(dat_rt %>%
   # Aggregate within participants across trials from the same condition
   group_by(id, agent, gaze, word_valence) %>%
   summarise(duration = mean(duration, na.rm = TRUE)) %>%
   # Aggregate across participants
   group_by(agent, gaze, word_valence) %>%
   summarise(
     RT = mean(duration, na.rm = TRUE),
     sd = sd(duration, na.rm = TRUE),
     SE = sd(duration) / sqrt(n)
   ) -> means_rt)

###############################################################################
## -  Could you plot a graph depicting the results and walk us through the plot?
###############################################################################

# Tell R what to plot and what are our variables
means_rt %>% ggplot(aes(x = word_valence, y = RT, color = agent, group = agent)) +
  # Create separate subplots for the two types of word valence
  facet_grid( ~ gaze) +
  # Add lines, dots, and errorbars (SEs)
  geom_line(position = position_dodge(width = 0.2)) +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(aes(ymin= RT - SE, ymax = RT + SE), width = 0.2, position = position_dodge(width = 0.2)) +
  # Plot a wider range on the y-axis (600-800 ms)
  coord_cartesian(ylim = c(600, 800)) +
  # Some styling
  theme_classic()

# Short explanation: The first two lines tell R to make a plot with certain "aesthetics", i.e. our dependent variable
# (reaction time) on the y-axis, our first independent variable (word_valence) on the x-axis, seperate colors for our
# second independent variable (agent), and two seperate subplots for our third independent variable (gaze). We then
# add some elements we want to show: Points (means), errorbars (SEs), and lines connecting dots belonging to the same
# condition. The final two lines are just for styling the plot, i.e. adjusting the range of the y-axis and setting a
# more APA style conform theme.

###################################################################################################
# EXERCISE 3: Prepare ANOVA accuracy overview
###################################################################################################

###############################################################################
## -  Could you walk us through the ANOVA results?
###############################################################################

# We already have factorized columns for agent and gaze.
# But we also need one for the valence of the target word.
dat %<>% mutate(word_valence = factor(response, levels = c("positive", "negative")))

# Create a new subset of the data
dat %>% 
  # Filter out practise trials
  filter(is.na(practice)) %>% 
  # Filter only target trials
  filter(response %in% c("positive", "negative")) %>%
  # Convert accuracy (correct or not) to a numeric variable
  mutate(correct = recode(correct, "TRUE" = 1, "FALSE" = 0)) -> data_2

# Run analysis of variance (ANOVA) for agent x gaze x word_valence
mod_2 <- aov_ez(
  id = "id",
  dv = "correct",
  within = c("agent", "gaze", "word_valence"),
  data = data_2,
  fun_aggregate = mean
)

# Take a look at the results
summary(mod_2)

###############################################################################
## -  Could you walk us through the post-hoc tests?
###############################################################################

# Compute follow-up pair-wise tests using the emmeans package
means_mod_2 <- emmeans(mod_2, specs = pairwise ~ agent * gaze * word_valence)

# Retrieve means for all combinations of factor levels (useful e.g. for plotting)
means_mod_2$emmeans

# Retrive contrasts between all combinations of factor levels
means_mod_2$contrasts

###############################################################################
## -  Could you generate a table with means from the raw data?
###############################################################################

# This requires two steps of aggregation
(data_2 %>%
   # Aggregate within participants across trials from the same condition
   group_by(id, agent, gaze, word_valence) %>%
   summarise(correct = mean(correct, na.rm = TRUE)) %>%
   # Aggregate across participants
   group_by(agent, gaze, word_valence) %>%
   summarise(
     accuracy = mean(correct, na.rm = TRUE),
     sd = sd(correct, na.rm = TRUE),
     SE = sd(correct) / sqrt(n)
   ) -> means_raw)

###############################################################################
## -  Could you plot a graph depicting the results and walk us through the plot?
###############################################################################

# Tell R what to plot and what are our variables
means_raw %>% ggplot(aes(x = gaze, y = accuracy, color = agent, group = agent)) +
  # Create separate subplots for the two types of word valence
  facet_grid( ~ word_valence) +
  # Add dots, errorbars, and lines
  geom_point(position = position_dodge(width = 0.2)) +  
  geom_errorbar(aes(ymin = accuracy - SE, ymax = accuracy + SE), width = 0.2, position = position_dodge(width = 0.2)) + 
  geom_line(position = position_dodge(width = 0.2)) +
  # Plot the entire range from 0 to 100 percent
  coord_cartesian(ylim = c(0, 1)) +
  # Some styling
  theme_classic()

# Short explanation: The first two lines tell R to make a plot with certain "aesthetics", i.e. our dependent variable
# (accuracy) on the y-axis, our first independent variable (gaze) on the x-axis, seperate colors for our second
# independent variable (agent), and two seperate subplots for our third independent variable (word valence). We then
# add some elements we want to show: Points (means), errorbars (SEs), and lines connecting dots belonging to the same
# condition. The final two lines are just for styling the plot, i.e. adjusting the range of the y-axis and setting a
# more APA style conform theme.

###################################################################################################
# EXERCISE 4: Intentionality analysis + plot by Gaze & Agent
###################################################################################################

###############################################################################
## -  How did you evaluate intentionality of humans and robots?
###############################################################################

# Start with the raw data
(dat %>%
   # Subset only the relevant rating trials
   filter(!is.na(intentional)) %>%
   # Create new columns for our independent variables
   mutate(
     ratings_agent = ifelse(str_detect(sender, "h"), "human", "robot"),
     ratings_gaze = ifelse(str_detect(sender, "o"), "open", "closed")
   ) %>%
   # Convert ratings to numeric
   mutate(across(c(intentional, selb, rational, intelligent), .fns = as.numeric)) %>%
   # Select only the relevant variables and save as new data frame
   select(id, ratings_agent, ratings_gaze, intentional, selb, rational, intelligent) -> data_ratings)

# Compute mean intentionality ratings by agent
(data_ratings %>%
    # Average within participants
    group_by(id, ratings_agent) %>%
    summarise(avg = mean(intentional)) -> ratings_int)

# Grand-average across participants
ratings_int %>%
  group_by(ratings_agent) %>%
  summarise(gavg = mean(avg), sd = sd(avg), se = sd(avg) / sqrt(n))

###############################################################################
## -  How did you evaluate agency of humans and robots?
###############################################################################

# Compute mean agency ratings by agent
(data_ratings %>%
   # Average within participants
   group_by(id, ratings_agent) %>%
   summarise(avg = mean(selb)) -> ratings_selb)

# Grand-average across participants
ratings_selb %>%
  group_by(ratings_agent) %>%
  summarise(gavg = mean(avg), sd = sd(avg), se = sd(avg) / sqrt(n))

###############################################################################
## -  How did you evaluate intentionality eyes open eyes closed human robot?
###############################################################################

# We do the same as above, but add gaze as an additional factor
(data_ratings %>%
   # Average within participants
   group_by(id, ratings_agent, ratings_gaze) %>%
   summarise(avg = mean(intentional)) -> ratings_int_gaze)

# Grand-average across participants
ratings_int_gaze %>%
  group_by(ratings_agent, ratings_gaze) %>%
  summarise(gavg = mean(avg), sd = sd(avg), se = sd(avg) / sqrt(n))

###############################################################################
## -  How did you evaluate agency eyes open eyes closed human robot? 
###############################################################################

# And we do the same for agency instead of intentionality
(data_ratings %>%
   # Average within participants
   group_by(id, ratings_agent, ratings_gaze) %>%
   summarise(avg = mean(selb)) -> ratings_selb_gaze)

# Grand-average across participants
ratings_selb_gaze %>%
  group_by(ratings_agent, ratings_gaze) %>%
  summarise(gavg = mean(avg), sd = sd(avg), se = sd(avg) / sqrt(n))

###############################################################################
## -  Table + plot as an overview
###############################################################################

# Bind intentionality and agency ratings together (column-wise)
ratings <- bind_rows("intentionality" = ratings_int_gaze, "agency" = ratings_selb_gaze, .id = "rating")

# Create a table
ratings %>%
  # Average across participants
  group_by("Rating" = rating, "Agent" = ratings_agent, "Eyegaze" = ratings_gaze) %>%
  summarise(
    "Average rating" = mean(avg),
    "SD" = sd(avg),
    "SE" = sd(avg) / sqrt(n)
  ) %>%
  # Prettier table output with knitr package
  kable(digits = 2)

# Create a plot
ratings %>%
  # Create a plot with the ratings on the y-axis, gaze on the x-axis, and different colors for different agents
  ggplot(aes(x = ratings_gaze, y = avg, color = ratings_agent, group = ratings_agent)) +
  # Create two separate subplots for agency and intentionality ratings
  facet_wrap(~ rating) +
  # Add individual data points (one point is one participant)
  geom_point(position = position_jitterdodge(dodge.width = 0.8)) +
  # Add summary statistics (mean and standard errors across participants)
  stat_summary(fun.data = "mean_se", position = position_dodge(width = 0.8)) +
  # Styling
  theme_bw()

###################################################################################################
# EXERCISE 5: Attitudes towards robots + experience with robots
###################################################################################################

###############################################################################
## -  What are the dimensions to evaluate ATAI?
###############################################################################

# Take a look at the following paper: Sindermann, C., Sha, P., Zhou, M., Wernicke, J., Schmitt, H. S., Li, M., ... &
# Montag, C. (2020). Assessing the Attitude Towards Artificial Intelligence: Introduction of a Short Measure in German,
# Chinese, and English Language. KI-Künstliche Intelligenz, 1-10. https://doi.org/10.1007/s13218-020-00689-0

###############################################################################
## -  How did participants score on Acceptance towards AI and Fear towards AI?
###############################################################################

# Start with the raw data
(dat %>%
   # Filter the relevant questionnaire trials
   filter(sender == "AttitudesAI") %>%
   # Select only the relevant columns
   select(contains("KI")) %>%
   # Convert the relevant variables to numeric
   mutate(across(.fns = as.numeric)) -> att_ai_raw)

###############################################################################
## -  Can you prepare a table with the scores & generate a plot to visually present the data? 
###############################################################################

# Compute mean scores for each participant
(att_ai_raw %>% transmute(
  fear = (angstKI + ZerstoerungKI + ArbeitslosigkeitKI) / 3,
  acceptance = (vetrauenKI + BereicherungKI) / 2
) -> att_ai)

# Means accross participants
mean(att_ai$fear)
mean(att_ai$acceptance)

# Scatterplot (helps to see the relationship between fear and acceptance)
att_ai %>% ggplot(aes(x = fear, y = acceptance)) +
  geom_point() +
  coord_cartesian(xlim = c(0, 10), ylim = c(0, 10)) +
  theme_bw()

# Density plot (helps to see how the scores are distributed)
att_ai %>% ggplot() +
  geom_density(aes(x = fear, fill = "fear"), alpha = 0.5) +
  geom_density(aes(x = acceptance, fill = "acceptance"), alpha = 0.5) +
  scale_fill_manual(values = c(fear = "red", acceptance = "green")) +
  coord_cartesian(xlim = c(0, 10)) +
  xlab("rating") +
  theme_bw()

###############################################################################
## -  Can you present how the group rated experience with robots?
###############################################################################

# Start with the raw data
dat %>%
  # Filter questionnaire trials
  filter(sender == "Experience with Robots") %>% 
  # Select only relevant column
  mutate(experience = factor(German, levels = c("keine", "wenige", "mehrere", "erheblich"))) %>%
  # Create bar chart
  ggplot(aes(x = experience)) +
  stat_count() +
  scale_x_discrete(drop = FALSE) +
  theme_bw()

###############################################################################
## -	Assess reliability
###############################################################################

# Cronbach's alpha for the "fear" subscale of the ATAI
att_ai_raw %>% select(angstKI, ZerstoerungKI, ArbeitslosigkeitKI) %>% alpha()

# Cronbach's alpha for the "acceptance" subscale of the ATAI
att_ai_raw %>% select(vetrauenKI, BereicherungKI) %>% alpha()


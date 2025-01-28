# STORM DATA SCRIPT
# 20/12/2020, PS (PRE-PROCESSING BY MT) and edited Oct 2022 KGL

# ANONYMISED and edited BY MT 25/07/2024

# LOAD LIBRARIES AND DATA ----

# removes everything from Environment
rm(list=ls())

library(dplyr)
library(stringr)
library(tidyr)
library(car)
library(ggplot2)
library(ltm)

#Read in the file
df_all_data <- read.csv('STORM_Project_2021_anonymised.csv')


# DATA CLEANING ----

#Remove columns that are not needed (start date, end date, status, IP address, recorded date, response id, recipient first name, recipient last name, recipient email, external reference, latitude, longitude distribution channel, language, email, other uni)
#The column with answers for the 'other' box for "attended uni" had 3 universities mentioned, these were marked with the right uni code and the column was assigned NAs and was also removed
#Q14 and Q16 are asking the same thing - answers from Q16 were moved to Q14 and the column is deleted

for (i in 1:length(df_all_data$X)){
  if (df_all_data$Q14[i] == ''){
    df_all_data$Q14[i] <- df_all_data$Q16[i] 
  }
}
# A similar situation with questions Q13 and Q15
for (i in 1:length(df_all_data$X)){
  if (df_all_data$Q13[i] == ''){
    df_all_data$Q13[i] <- df_all_data$Q15[i] 
  }
}

#Remove the columns
df_all_data <- df_all_data[,-c(1:5, 9:18, 20, 23, 29:30)]

#Remove rows that are not needed (first two rows that are not participants)
df_all_data <- df_all_data[-c(1:2),]

#Rename all the columns to make it easier to reference them
colnames(df_all_data) <- c('Progress', 'Duration_Seconds', 'Finished', 'Participant_ID', 'Age', 'Uni',	'Full_Part',	'PTY',	
                           'PTY_Other',	'Course_Duration',	'Current_Year',		
                           'Course_Name',	'Gender',	
                           'Perception_1',	'Perception_2',	'Perception_3',	'Perception_4',	'Perception_5',	
                           'Perception_6',	'Perception_7',	'Perception_8',	'Perception_9',	'Perception_10',	
                           'Perception_11',	'Perception_12',	'Perception_13',	'Perception_14',	
                           'Perception_15',	'Perception_16',	'Awareness_1',	'Awareness_2',	
                           'Awareness_3',	'Awareness_4',	'Awareness_5',	'Awareness_6',	'Awareness_7',	
                           'Awareness_8',	'Experience_1',	'Experience_2',	'Experience_3',	'Experience_4',	
                           'Experience_5',	'Experience_6',	'Experience_7',	'Experience_8',	'Knowledge_1',	
                           'Knowledge_2',	'Knowledge_3',	'Knowledge_4',	'Knowledge_5',	'Knowledge_6',	
                           'Knowledge_7',	'Knowledge_8',	'crisis_aware',	'crisis_learnt',	
                           'understanding_crisis',	'os_explain',	'os_learnt',	'applicability',	
                           'comments')


# EXCLUSIONS ----

#------------1. select individuals who were too quick 
#place in a new data frame
#first make sure that this variable is numeric
df_all_data$Duration_Seconds <- as.numeric(df_all_data$Duration_Seconds)

# average RT in minutes before exclusions
mean(df_all_data$Duration_Seconds)/60
# 77.10 minutes

#then filter those who spent under 4 minutes
df_incl_cri_met <- filter(df_all_data, Duration_Seconds > 240)
#108 cases were removed, 810 total

#------------2. Select individuals from Unis with more than 10 entries
# First, check the frequency for each university
responses_per_uni <- as.data.frame(table(df_incl_cri_met$Uni))

#based on the table, make a selection of universities with enough responses (> 9)
for (i in 1:length(df_incl_cri_met$Uni)){
  uni <- df_incl_cri_met$Uni[i]
  uni_row <- as.numeric(which(responses_per_uni == uni, arr.ind = TRUE)[1,1])
  uni_freq <-responses_per_uni[uni_row,2]
  if (uni_freq > 9){
    df_incl_cri_met$include[i] <- 1
  } else {
    df_incl_cri_met$include[i] <- 0
  }

}

#filter based on the criterion
df_incl_cri_met <- df_incl_cri_met %>%
  filter(include == 1)

#Also remove those that did not state their University or where university  - in the "Uni" column, this is marked with an empty cell
df_incl_cri_met <- df_incl_cri_met %>%
  filter(Uni != "")
#remaining 760


#-----------3. Missing Data

#--A-- Frequency of progress stages can be checked in %, however, the pre-registration suggests
# that we will be keeping participants who complete at least enough for 
# one of the scales.
table(df_incl_cri_met$Progress)

#--B-- The pre-registration stated that participants who enter random values into free text
# boxes will be removed, this can be controlled well with the gender question, as there 
# is a relatively small number of answers that can be given so it will be easy to filter the odd ones out.
# the frequency of different types of answers can be checked with a table
# and inappropriate answers can be filtered out.

#Frequencies of the answers to the gender question
table(df_incl_cri_met$Gender)

#Filter out the odd answers
df_incl_cri_met <- df_incl_cri_met %>% 
  filter(!str_detect(Gender, "18|AFAB|x"))
#3 cases were removed, total 757


#I also checked the last comment question for duplicates - there were none, and screened
# for any odd answers and there was none
duplicates <- table(df_incl_cri_met$comments) > 2
which(duplicates == TRUE)

#There seem to be some duplicates in the ID so these will also be removed
which(table(df_incl_cri_met$Participant_ID) > 1)
length(which(table(df_incl_cri_met$Participant_ID) > 1))

df_incl_cri_met <- df_incl_cri_met %>% 
  distinct(Participant_ID, .keep_all = TRUE)

#15 removed 742 remain

#-----------4. Keep only undergraduate students
# I displayed the df_incl_cri_met window and used the filter option at the top 
# to look for characters msc and ma in the Course_Name column to find the strings
# that I put into the code to filter these out

#Filter out individuals who are doing an MSc student
df_incl_cri_met <- df_incl_cri_met %>% 
  filter(!str_detect(Course_Name, "MSc|MSci|Doctorate of Clinical Psychology"))
#6 cases removed, 736 total

#There are also students who are not from psychology so they should be removed
table(df_incl_cri_met$Course_Name)

exclude_course <- c("BA (HONS) SOCIOLOGY (WITH FOUNDATION YEAR) SW", "BA HONS CRIMINOLOGY", "criminology", "Criminology", "criminology with foundation year ", "LLB Law and Criminology ", "Natural Sciences", "TEST")

df_incl_cri_met$exclude <- NA
for (i in 1: length(df_incl_cri_met$Progress)){
  check <- df_incl_cri_met$Course_Name[i] == exclude_course
  df_incl_cri_met$exclude[i] <- sum(check == TRUE) 
}

df_incl_cri_met <- df_incl_cri_met %>% 
  filter(exclude == 0)
#10 cases removed, 726 total


#Now let's check if there are still 10 cases per each included uni

table(df_incl_cri_met$Uni)
#Yes, all good, 19 Unis left


# Check response time once more
# median RT in minutes
median(df_incl_cri_met$Duration_Seconds)/60
# 16.27 minutes



#-----------DATA CLEANING - VARIABLES PREP ----------------

# ---------- CODING UKRN ----

#Create a separate UKRN variable, this can be based on Uni code 
df_incl_cri_met$UKRN <- df_incl_cri_met$Uni
df_incl_cri_met$UKRN[grep("UKRN", df_incl_cri_met$UKRN)] <- "UKRN"
df_incl_cri_met$UKRN[grep("UNI", df_incl_cri_met$UKRN)] <- "Non-UKRN"
df_incl_cri_met$UKRN[df_incl_cri_met$UKRN == "Other"] <- NA

#summarise this in a table
table(df_incl_cri_met$UKRN)
# Non-UKRN = 502; UKRN = 224

#------------Timepoint variable ----- 
df_incl_cri_met$timepoint=2


#----------Coded variable for crisis explanation ------------------
# Add in the answers for the coded variable crisis_explain
#Add ID column to original data 
df_incl_cri_met$ID <- 1:nrow(df_incl_cri_met)

# i.e., Merge two datasets 
#Read in crisis_explain data file 
crisis_explain_wave2 <- read.csv('crisis_explain_wave2.csv')
crisis_explain_wave2 <- crisis_explain_wave2[,c(1,7)]

colnames(crisis_explain_wave2) <- c('Participant_ID',	'crisis_explain')

#also remove duplicates
crisis_explain_wave2 <- crisis_explain_wave2 %>% 
  distinct(Participant_ID, .keep_all = TRUE)

#in order to not introduce empty rows for previosuly removed participants, we will only import data only for those participants who are currently in the data frame
keep_ptts <- intersect(df_incl_cri_met$Participant_ID, crisis_explain_wave2$Participant_ID)

crisis_explain_wave2 <- crisis_explain_wave2 %>% 
  filter(Participant_ID %in% c(keep_ptts))

#Merge
clean_data <- merge(df_incl_cri_met, crisis_explain_wave2, by = "Participant_ID", all = TRUE)


# -------EXCLUSION - YEAR GROUP ----

# Make a file with only full time students for now. This file should be used for any academic year group analysis 
Wave1_FT=subset(clean_data, Full_Part %in% c("Full-time"))
# removed 29, total = 697

# New coding for the current year of study
Wave1_FT$Current_Year[Wave1_FT$Current_Year == "1st Year"] <- "Year 1"
Wave1_FT$Current_Year[Wave1_FT$Current_Year == "2nd Year"] <- "Year 2"
Wave1_FT$Current_Year[Wave1_FT$Current_Year == "3rd Year" & Wave1_FT$PTY == "No"] <- "Final Year"
Wave1_FT$Current_Year[Wave1_FT$Current_Year == "3rd Year" & Wave1_FT$PTY == "Yes"] <- "Placement Year"
Wave1_FT$Current_Year[Wave1_FT$Current_Year == "3rd Year" & Wave1_FT$PTY == "Other (Please specify)"] <- "Placement Year"
Wave1_FT$Current_Year[Wave1_FT$Current_Year == "4th Year"] <- "Final Year"


# see how many in each year
table(Wave1_FT$Current_Year)


# 1 hasn't reported year - so we have removed them
final_df <- Wave1_FT %>%
  filter(Current_Year != "")


# Check how many per University
table(final_df$Uni)


#Final sample 696

table(final_df$UKRN)

#474 non-UKRN and 222 in UKRN

# ------- VARIABLE PREP ----

#---------- Create all scale scores
#Create a function to re-code the answers into scores - courtesy of a nice 
# person who shared it on stack exchange!

recode2 <- function ( data, fields, recodes) {
  for ( i in which(names(data) %in% fields) ) { # iterate over column indexes that are present in the passed dataframe that are also included in the fields list
    data[,i] <- car::recode( data[,i], recodes)
  }
  data
}

#Run this function on all variables that need to be re-coded to numbers

#Perception of Open Research - standard scoring
final_df <- recode2(final_df, fields = c('Perception_1', 'Perception_3', 'Perception_5', 
                                                       'Perception_7', 'Perception_9', 'Perception_11', 
                                                       'Perception_13', 'Perception_15'), 
                           recodes = "'Strongly disagree' = -2; 'Disagree' = -1; 'Have no opinion' = 0; 
                           'Agree' = 1; 'Strongly agree' = 2")
#Perception of Open Research - reverse coded items
final_df <- recode2(final_df, fields = c('Perception_2', 'Perception_4', 'Perception_6', 
                                                       'Perception_8', 'Perception_10', 'Perception_12', 
                                                       'Perception_14', 'Perception_16'), 
                           recodes = "'Strongly disagree' = 2; 'Disagree' = 1; 'Have no opinion' = 0; 
                           'Agree' = -1; 'Strongly agree' = -2")

# Awareness of Open Research
final_df <- recode2(final_df, fields = c('Awareness_1', 'Awareness_2', 'Awareness_3', 
                                                       'Awareness_4', 'Awareness_5', 'Awareness_6', 
                                                       'Awareness_7', 'Awareness_8'), 
                           recodes = "'I have heard of this' = 1; '' = 0")

#Experience of Open Research
final_df <- recode2(final_df, fields = c('Experience_1', 'Experience_2', 'Experience_3', 
                                                       'Experience_4', 'Experience_5', 'Experience_6', 
                                                       'Experience_7', 'Experience_8'), 
                           recodes = "'I have done/used this' = 1; '' = 0")

#Knowledge of Open Research - standard scoring
final_df <- recode2(final_df, fields = c('Knowledge_1', 'Knowledge_3', 'Knowledge_6', 
                                                       'Knowledge_8'), 
                           recodes = "'Strongly disagree' = -2; 'Strongly Disagree' = -2; 'Disagree' = -1; 
                           'Have no opinion' = 0; 'Agree' = 1; 'Strongly Agree' = 2; 'Strongly agree' = 2")
#Knowledge of Open Research - reverse coded items
final_df <- recode2(final_df, fields = c('Knowledge_2', 'Knowledge_4', 'Knowledge_5', 
                                                       'Knowledge_7'), 
                           recodes = "'Strongly disagree' = 2; 'Strongly Disagree' = 2; 'Disagree' = 1; 
                           'Have no opinion' = 0; 'Agree' = -1; 'Strongly Agree' = -2; 'Strongly agree' = -2")

#Secondary DVs with yes/no/notsure answers
final_df <- recode2(final_df, fields = c('crisis_aware', 'crisis_learnt', 'os_explain', 
                                                       'os_learnt', 'applicability'), 
                           recodes = "'Yes' = 1; 'No' = 0; 'Not sure' = 0")


#Lastly, make sure that all of the new number variables are numerical 
# for future calculations.
final_df[,14:55] <- sapply(final_df[,14:55],as.numeric)
#There is a categorical variable in the middle so the code had to be split
final_df[,57:59] <- sapply(final_df[,57:59],as.numeric)
#Check if it worked
str(final_df)


#----------------------Create new variables

#----- Perception of open research Q28_1-Q28_16 - Total and Mean Scores
#This is contextual perception
#Here this shows precisely how many elements the students have judged positively
final_df <- final_df %>% 
  rowwise() %>%
  mutate(Perception_Score_Total = sum(Perception_1, Perception_2, Perception_3, Perception_4, Perception_5, 
           Perception_6, Perception_7, Perception_8, Perception_9, Perception_10, Perception_11,  
           Perception_12, Perception_13, Perception_14, Perception_15, Perception_16)) 

# -- Create a new variable - Perception_Score_Mean - as a mean of all answers to the perception questions
# Skip blank cells
# Mean contectual perception of open science practices
final_df <- final_df %>% 
  rowwise() %>% 
  mutate(Perception_Score_Mean=mean(c(Perception_1, Perception_2, Perception_3, Perception_4, 
                                        Perception_5, Perception_6, Perception_7, Perception_8, 
                                        Perception_9, Perception_10, Perception_11, Perception_12, 
                                        Perception_13, Perception_14, Perception_15, Perception_16))) 

#---- Knowledge of open research Q22-Q37 - Total and Mean Scores
# Also called situational perceptions
#--Create a new variable - Knowledge_Score_Total
#Here this shows precisely how many elements the students have judged positively
final_df <- final_df %>% 
  rowwise() %>%
  mutate(Knowledge_Score_Total = sum(Knowledge_1, Knowledge_2, Knowledge_3, Knowledge_4, Knowledge_5, 
           Knowledge_6,Knowledge_7,Knowledge_8)) 

# -- Knowledge_Score_Mean - as a mean of all answers to the perception questions
# Mean situational perception of open science practices
final_df <- final_df %>% 
  rowwise() %>% 
  mutate(Knowledge_Score_Mean=mean(c(Knowledge_1, Knowledge_2, Knowledge_3, Knowledge_4, Knowledge_5, 
                                       Knowledge_6, Knowledge_7, Knowledge_8))) 


#----- Awareness of open research Q181_1-Q181_8 - Total Scores
# -- Create a new variable - Awareness_Score_Total
#Here this shows precisely how many elements the students are aware of
final_df <- final_df %>% 
  mutate(Awareness_Score_Total = sum(Awareness_1, Awareness_2, Awareness_3, Awareness_4, Awareness_5, 
           Awareness_6, Awareness_7, Awareness_8))  

#----- Experience of open research Q182_1-Q182_8 - Total Scores
# -- Create a new variable - Experience_Score_Total
#Here this shows precisely how many elements the students have experience of
final_df <- final_df %>% 
  mutate(Experience_Score_Total = sum(Experience_1, Experience_2, Experience_3, Experience_4, Experience_5, 
           Experience_6, Experience_7, Experience_8))  


#---------FINAL EXCLUSIONS--------------
#Check how many people have actually completed enough of the questionnaire to use for the analyses
#Then dIvide the dataset into different outcome variables of interest


# make new columns which will mark inclusion in new datasets
final_df$Perception_Completed=1
final_df$Awareness_Completed=1
final_df$Experience_Completed=1
final_df$Knowledge_Completed=1

final_df$crisis_aware_Completed=1
final_df$crisis_learnt_Completed=1
final_df$os_explain_Completed=1
final_df$os_learnt_Completed=1
final_df$applicability_Completed=1
final_df$crisis_explain_Completed=1

#Mark for exclusion those that did not complete the specific questions

final_df$Perception_Completed[is.na(final_df$Perception_Score_Total) == TRUE] <- 0
final_df$Awareness_Completed[is.na(final_df$Awareness_Score_Total) == TRUE] <- 0
final_df$Experience_Completed[is.na(final_df$Experience_Score_Total) == TRUE] <- 0
final_df$Knowledge_Completed[is.na(final_df$Knowledge_Score_Total) == TRUE] <- 0

final_df$crisis_aware_Completed[is.na(final_df$crisis_aware) == TRUE] <- 0
final_df$crisis_learnt_Completed[is.na(final_df$crisis_learnt) == TRUE] <- 0
final_df$crisis_learnt_Completed[final_df$crisis_learnt == ""] <- 0
final_df$os_explain_Completed[is.na(final_df$os_explain) == TRUE] <- 0
final_df$os_learnt_Completed[is.na(final_df$os_learnt) == TRUE] <- 0
final_df$applicability_Completed[is.na(final_df$applicability) == TRUE] <- 0
final_df$applicability_Completed[final_df$applicability == ""] <- 0
final_df$crisis_explain_Completed[final_df$crisis_explain == 9999] <- 0


#Restructure the columns so that they make more sense
#Removed columns that are redundant - Duration_Seconds, PTY_Other, Finished, Course_Duration_Other, Current_Year_Other, all individual scale scores, all columns marking exclusion
final_df_all <- final_df #save all of the columns just in case needed later

final_df <- final_df %>% 
  dplyr::select("ID", "Progress", "Uni", "PTY", "Current_Year", "Course_Duration", "Course_Name", "Gender", "Age", "UKRN", "timepoint", "Perception_1", "Perception_2", "Perception_3", "Perception_4",  "Perception_5", 
                "Perception_6", "Perception_7", "Perception_8", "Perception_9", "Perception_10", "Perception_11", "Perception_12", "Perception_13", "Perception_14",  "Perception_15", "Perception_16", "Knowledge_1", "Knowledge_2", "Knowledge_3", 
                "Knowledge_4", "Knowledge_5", "Knowledge_6", "Knowledge_7", "Knowledge_8", "Awareness_1", "Awareness_2", "Awareness_3", "Awareness_4", "Awareness_5", "Awareness_6", "Awareness_7", "Awareness_8", "Experience_1", "Experience_2", 
                "Experience_3", "Experience_4", "Experience_5", "Experience_6", "Experience_7", "Experience_8", "Perception_Score_Total", "Perception_Score_Mean", "Knowledge_Score_Total", "Knowledge_Score_Mean",  "Awareness_Score_Total", "Experience_Score_Total", 
                "crisis_aware", "crisis_learnt", "crisis_explain", "os_explain", "os_learnt", "applicability", "comments")

colnames(final_df) <- c("ID", "Progress", "Uni", "PTY", "Current_Year", "Course_Duration", "Course_Name", "Gender", "Age", "UKRN", "timepoint", "Perception_Replication1", "Perception_Replication2", "Perception_Materials1", "Perception_Materials2",  "Perception_Education1", 
                        "Perception_Education2", "Perception_Access1", "Perception_Access2", "Perception_Data1", "Perception_Data2", "Perception_Power1", "Perception_Power2", "Perception_Preregistration1", "Perception_Preregistration2",  "Perception_Preprint1", "Perception_Preprint2", "Knowledge_Replication", 
                        "Knowledge_Materials", "Knowledge_Education", "Knowledge_Access", "Knowledge_Data", "Knowledge_Power", "Knowledge_Preregistration", "Knowledge_Preprint", "Awareness_Replication", "Awareness_Materials", "Awareness_Education", "Awareness_Access", "Awareness_Data", "Awareness_Power", "Awareness_Preregistration", 
                        "Awareness_Preprint", "Experience_Replication", "Experience_Materials", "Experience_Education", "Experience_Access", "Experience_Data", "Experience_Power", "Experience_Preregistration", "Experience_Preprint", "Perception_Score_Total", "Perception_Score_Mean", "Knowledge_Score_Total", "Knowledge_Score_Mean",  "Awareness_Score_Total", "Experience_Score_Total", 
                        "crisis_aware", "crisis_learnt", "crisis_explain", "os_explain", "os_learnt", "applicability", "comments")


#--------------MAKE SEPARATE DFs FOR OUTCOMES --------------------

# Set 1 of Analyses

# The outcomes are compared by UKRN status 
# The outcomes are compared by the academic year group
# The results are taking into consideration the clustering of university
# Here we want to know the differences on binary outcome variables: crisis aware, crisis explain, crisis learnt, os explain, os learnt and applicability

#This one has a lot of missing data but I did not remove any because these are single questions not forming any sub-scales
Set1_AllData <- final_df_all %>%
  dplyr::select("ID", "Uni", "Full_Part", "PTY", "Current_Year", "Course_Duration", "Course_Name", "Gender", "Age", "UKRN", "timepoint", "crisis_aware", "crisis_learnt", "crisis_explain", "os_explain", "os_learnt", "applicability", "crisis_aware_Completed", "crisis_learnt_Completed", "crisis_explain_Completed", "os_explain_Completed", "os_learnt_Completed", "applicability_Completed")

# Set 2 of Analyses

# The outcomes are compared by UKRN status 
# The outcomes are compared by the academic year group
# The results are taking into consideration the clustering of university

#QUESTION
# Perhaps with these we could also add the variables on crisis aware, crisis learn, crisis explain, open science explain, open science learn as explainer variables
# But there is quite a lot of missing data here for crisis explain and crisis learnt

#WITHIN-SUBJECT ANALYSES 
# Comparing responses to Replication, Materials, Education, Access, Data, Power, Preregistration, Preprint

Set2_ConceptualPerception <- final_df_all %>%
  dplyr::select("ID", "Uni", "Full_Part", "PTY", "Current_Year", "Course_Duration", "Course_Name", "Gender", "Age", "UKRN", "timepoint", "Perception_1", "Perception_2", "Perception_3", "Perception_4",  "Perception_5", 
           "Perception_6", "Perception_7", "Perception_8", "Perception_9", "Perception_10", "Perception_11", "Perception_12", "Perception_13", "Perception_14",  "Perception_15", "Perception_16", "Perception_Score_Total", "Perception_Score_Mean", "crisis_aware", "crisis_learnt", "crisis_explain", "os_explain", "os_learnt", "Perception_Completed")
Set2_ConceptualPerception <- Set2_ConceptualPerception[Set2_ConceptualPerception$Perception_Completed == 1, ]
colnames(Set2_ConceptualPerception) <- c("ID", "Uni", "Full_Part", "PTY", "Current_Year", "Course_Duration", "Course_Name", "Gender", "Age", "UKRN", "timepoint", "Perception_Replication1", "Perception_Replication2", "Perception_Materials1", "Perception_Materials2",  "Perception_Education1", 
"Perception_Education2", "Perception_Access1", "Perception_Access2", "Perception_Data1", "Perception_Data2", "Perception_Power1", "Perception_Power2", "Perception_Preregistration1", "Perception_Preregistration2",  "Perception_Preprint1", "Perception_Preprint2", "Perception_Score_Total", "Perception_Score_Mean", "crisis_aware", "crisis_learnt", "crisis_explain", "os_explain", "os_learnt", "Perception_Completed")

Set2_SituationalPerception <- final_df_all %>% 
  dplyr::select("ID", "Uni", "Full_Part", "PTY", "Current_Year", "Course_Duration", "Course_Name", "Gender", "Age", "UKRN", "timepoint", "Knowledge_1", "Knowledge_2", "Knowledge_3", "Knowledge_4", "Knowledge_5", "Knowledge_6", "Knowledge_7", "Knowledge_8", "Knowledge_Score_Total", "Knowledge_Score_Mean", "crisis_aware", "crisis_learnt", "crisis_explain", "os_explain", "os_learnt", "Knowledge_Completed")
Set2_SituationalPerception <- Set2_SituationalPerception[Set2_SituationalPerception$Knowledge_Completed == 1,]
colnames(Set2_SituationalPerception) <- c("ID", "Uni", "Full_Part", "PTY", "Current_Year", "Course_Duration", "Course_Name", "Gender", "Age", "UKRN", "timepoint", "Knowledge_Replication", "Knowledge_Materials", "Knowledge_Education", "Knowledge_Access", "Knowledge_Data", "Knowledge_Power", "Knowledge_Preregistration", "Knowledge_Preprint", "Knowledge_Score_Total", "Knowledge_Score_Mean", "crisis_aware", "crisis_learnt", "crisis_explain", "os_explain", "os_learnt", "Knowledge_Completed") 

Set2_Awareness <- final_df_all %>% 
  dplyr::select("ID", "Uni", "Full_Part", "PTY", "Current_Year", "Course_Duration", "Course_Name", "Gender", "Age", "UKRN", "timepoint", "Awareness_1", "Awareness_2", "Awareness_3", "Awareness_4", "Awareness_5", "Awareness_6", "Awareness_7", "Awareness_8", "Awareness_Score_Total", "crisis_aware", "crisis_learnt", "crisis_explain", "os_explain", "os_learnt", "Awareness_Completed")
Set2_Awareness <- Set2_Awareness[Set2_Awareness$Awareness_Completed == 1,]
colnames(Set2_Awareness) <- c("ID", "Uni", "Full_Part", "PTY", "Current_Year", "Course_Duration", "Course_Name", "Gender", "Age", "UKRN", "timepoint", "Awareness_Replication", "Awareness_Materials", "Awareness_Education", "Awareness_Access", "Awareness_Data", "Awareness_Power", "Awareness_Preregistration", "Awareness_Preprint", "Awareness_Score_Total", "crisis_aware", "crisis_learnt", "crisis_explain", "os_explain", "os_learnt", "Awareness_Completed")

Set2_Experience <- final_df_all %>% 
  dplyr::select("ID", "Uni", "Full_Part", "PTY", "Current_Year", "Course_Duration", "Course_Name", "Gender", "Age", "UKRN", "timepoint", "Experience_1", "Experience_2", "Experience_3", "Experience_4", "Experience_5", "Experience_6", "Experience_7", "Experience_8", "Experience_Score_Total", "crisis_aware", "crisis_learnt", "crisis_explain", "os_explain", "os_learnt", "Experience_Completed") 
Set2_Experience <- Set2_Experience[Set2_Experience$Experience_Completed == 1,]
colnames(Set2_Experience) <- c("ID", "Uni", "Full_Part", "PTY", "Current_Year", "Course_Duration", "Course_Name", "Gender", "Age", "UKRN", "timepoint", "Experience_Replication", "Experience_Materials", "Experience_Education", "Experience_Access", "Experience_Data", "Experience_Power", "Experience_Preregistration", "Experience_Preprint", "Experience_Score_Total", "crisis_aware", "crisis_learnt", "crisis_explain", "os_explain", "os_learnt", "Experience_Completed") 



setwd("C:/Users/marto05/OneDrive - Linköpings universitet/10. Side projects/2. STORM/STORM/Analysis Wave 2 2021/cleaned_data")
write.csv(Set1_AllData, "W2_Set1_AllData.csv", row.names = FALSE)
write.csv(Set2_ConceptualPerception, "W2_Set2_ConceptualPerception.csv", row.names = FALSE)
write.csv(Set2_SituationalPerception, "W2_Set2_SituationalPerception.csv", row.names = FALSE)
write.csv(Set2_Awareness, "W2_Set2_Awareness.csv", row.names = FALSE)
write.csv(Set2_Experience, "W2_Set2_Experience.csv", row.names = FALSE)

write.csv(final_df, "W2_final_wave2.csv", row.names = FALSE)
write.csv(final_df_all, "W2_final_wave2_extended.csv", row.names = FALSE)

setwd("C:/Users/marto05/OneDrive - Linköpings universitet/10. Side projects/2. STORM/STORM/Analysis Wave 2 2021")


#------------------------Cronbach's Alpha------------------------
#NOTE DONE
#Set1_ConceptualPerception
#cronbach.alpha(Set2_ConceptualPerception[,12:28], na.rm=TRUE)
#Set1_SituationalPerception 
#cronbach.alpha(Set2_SituationalPerception[,12:19], na.rm=TRUE)
#Set1_Awareness
#cronbach.alpha(Set2_Awareness[,12:19], na.rm=TRUE)
#Set1_Experience
#cronbach.alpha(Set2_Experience[,12:19], na.rm=TRUE)




# STORM DATA SCRIPT
# 22/08/2024, Marta Topor

library(tidyverse)

#----PULL DATA FROM ALL THREE WAVES FOR CROSS-WAVE ANALYSIS

datapath1 <- gsub("/Analysis Cross-waves","", getwd())
dataset1 <- read.csv(paste(datapath1, "/Analysis Wave 1 2020/cleaned_data/W1_final_wave1_extended.csv", sep = "")) 

datapath2 <- gsub("/Analysis Cross-waves","", getwd())
dataset2 <- read.csv(paste(datapath2, "/Analysis Wave 2 2021/cleaned_data/W2_final_wave2_extended.csv", sep = "")) 

datapath3 <- gsub("/Analysis Cross-waves","", getwd())
dataset3 <- read.csv(paste(datapath2, "/Analysis Wave 3 2022/cleaned_data/W3_final_wave3_extended.csv", sep = "")) 


#Find all columns that are different between the datasets & remove
#Set1
setdiff(colnames(dataset1), colnames(dataset2))
setdiff(colnames(dataset1), colnames(dataset3))
#Set2
setdiff(colnames(dataset2), colnames(dataset1))
setdiff(colnames(dataset2), colnames(dataset3))
#Set3
setdiff(colnames(dataset3), colnames(dataset1))
setdiff(colnames(dataset3), colnames(dataset2))

#Remove
dataset1 <- dataset1 %>% dplyr::select(-c(Course_Duration_Other, Current_Year_Other, Previous_Training))
dataset2 <- dataset2 %>% dplyr::select(-c(Participant_ID, Age, exclude))
dataset3 <- dataset3 %>% dplyr::select(-c(Course_Duration_Other, Current_Year_Other, Previous_Training, long_desc))


#Order the columns alphabetically so that the datasets have identical structure
ordered_dataset1 <- dataset1[,order(colnames(dataset1))]
ordered_dataset2 <- dataset2[,order(colnames(dataset2))]
ordered_dataset3 <- dataset3[,order(colnames(dataset3))]

#Join the dataframes

alldata <- rbind(ordered_dataset1, ordered_dataset2, ordered_dataset3)


#Unique universities
table(alldata$Uni, alldata$timepoint)

#Only select data from UKRN in years 1 and 2

cut_dataset <- alldata %>% filter(Current_Year == "Year 1" | Current_Year == "Year 2", 
                                  UKRN == "UKRN")

table(cut_dataset$timepoint)

#Compare for categorical variables - crisis aware, os_learnt, os_explain and applicability
#compare awareness and experience scores out of 8
#compare conceptual and situational perceptions



#--------------MAKE SEPARATE DFs FOR OUTCOMES --------------------

# Set 1 of Analyses

# The outcomes are compared by STORM wave - 2020, 2021 and 2022
# The results are taking into consideration the clustering of university
# Here we want to know the differences on binary outcome variables: crisis aware, crisis explain, crisis learnt, os explain, os learnt and applicability

#This one has a lot of missing data but I did not remove any because these are single questions not forming any sub-scales
Set1_AllData <- cut_dataset %>%
  dplyr::select("ID", "Uni", "Full_Part", "PTY", "Current_Year", "Course_Duration", "Course_Name", "Gender", "UKRN", "timepoint", "crisis_aware", "crisis_learnt", "crisis_explain", "os_explain", "os_learnt", "applicability", "crisis_aware_Completed", "crisis_learnt_Completed", "crisis_explain_Completed", "os_explain_Completed", "os_learnt_Completed", "applicability_Completed")

Set1_AllData$crisis_explain[Set1_AllData$crisis_explain == 9999] <- NA

# Set 2 of Analyses

# The outcomes are compared by UKRN status 
# The outcomes are compared by the academic year group
# The results are taking into consideration the clustering of university

#QUESTION
# Perhaps with these we could also add the variables on crisis aware, crisis learn, crisis explain, open science explain, open science learn as explainer variables
# But there is quite a lot of missing data here for crisis explain and crisis learnt

#WITHIN-SUBJECT ANALYSES 
# Comparing responses to Replication, Materials, Education, Access, Data, Power, Preregistration, Preprint

Set2_ConceptualPerception <- cut_dataset %>%
  dplyr::select("ID", "Uni", "Full_Part", "PTY", "Current_Year", "Course_Duration", "Course_Name", "Gender", "UKRN", "timepoint", "Perception_1", "Perception_2", "Perception_3", "Perception_4",  "Perception_5", 
                "Perception_6", "Perception_7", "Perception_8", "Perception_9", "Perception_10", "Perception_11", "Perception_12", "Perception_13", "Perception_14",  "Perception_15", "Perception_16", "Perception_Score_Total", "Perception_Score_Mean", "crisis_aware", "crisis_learnt", "crisis_explain", "os_explain", "os_learnt", "Perception_Completed")
Set2_ConceptualPerception <- Set2_ConceptualPerception[Set2_ConceptualPerception$Perception_Completed == 1, ]
colnames(Set2_ConceptualPerception) <- c("ID", "Uni", "Full_Part", "PTY", "Current_Year", "Course_Duration", "Course_Name", "Gender", "UKRN", "timepoint", "Perception_Replication1", "Perception_Replication2", "Perception_Materials1", "Perception_Materials2",  "Perception_Education1", 
                                         "Perception_Education2", "Perception_Access1", "Perception_Access2", "Perception_Data1", "Perception_Data2", "Perception_Power1", "Perception_Power2", "Perception_Preregistration1", "Perception_Preregistration2",  "Perception_Preprint1", "Perception_Preprint2", "Perception_Score_Total", "Perception_Score_Mean", "crisis_aware", "crisis_learnt", "crisis_explain", "os_explain", "os_learnt", "Perception_Completed")


Set2_SituationalPerception <- cut_dataset %>% 
  dplyr::select("ID", "Uni", "Full_Part", "PTY", "Current_Year", "Course_Duration", "Course_Name", "Gender", "UKRN", "timepoint", "Knowledge_1", "Knowledge_2", "Knowledge_3", "Knowledge_4", "Knowledge_5", "Knowledge_6", "Knowledge_7", "Knowledge_8", "Knowledge_Score_Total", "Knowledge_Score_Mean", "crisis_aware", "crisis_learnt", "crisis_explain", "os_explain", "os_learnt", "Knowledge_Completed")
Set2_SituationalPerception <- Set2_SituationalPerception[Set2_SituationalPerception$Knowledge_Completed == 1,]
colnames(Set2_SituationalPerception) <- c("ID", "Uni", "Full_Part", "PTY", "Current_Year", "Course_Duration", "Course_Name", "Gender", "UKRN", "timepoint", "Knowledge_Replication", "Knowledge_Materials", "Knowledge_Education", "Knowledge_Access", "Knowledge_Data", "Knowledge_Power", "Knowledge_Preregistration", "Knowledge_Preprint", "Knowledge_Score_Total", "Knowledge_Score_Mean", "crisis_aware", "crisis_learnt", "crisis_explain", "os_explain", "os_learnt", "Knowledge_Completed") 


Set2_Awareness <- cut_dataset %>% 
  dplyr::select("ID", "Uni", "Full_Part", "PTY", "Current_Year", "Course_Duration", "Course_Name", "Gender", "UKRN", "timepoint", "Awareness_1", "Awareness_2", "Awareness_3", "Awareness_4", "Awareness_5", "Awareness_6", "Awareness_7", "Awareness_8", "Awareness_Score_Total", "crisis_aware", "crisis_learnt", "crisis_explain", "os_explain", "os_learnt", "Awareness_Completed")
Set2_Awareness <- Set2_Awareness[Set2_Awareness$Awareness_Completed == 1,]
colnames(Set2_Awareness) <- c("ID", "Uni", "Full_Part", "PTY", "Current_Year", "Course_Duration", "Course_Name", "Gender", "UKRN", "timepoint", "Awareness_Replication", "Awareness_Materials", "Awareness_Education", "Awareness_Access", "Awareness_Data", "Awareness_Power", "Awareness_Preregistration", "Awareness_Preprint", "Awareness_Score_Total", "crisis_aware", "crisis_learnt", "crisis_explain", "os_explain", "os_learnt", "Awareness_Completed")


Set2_Experience <- cut_dataset %>% 
  dplyr::select("ID", "Uni", "Full_Part", "PTY", "Current_Year", "Course_Duration", "Course_Name", "Gender", "UKRN", "timepoint", "Experience_1", "Experience_2", "Experience_3", "Experience_4", "Experience_5", "Experience_6", "Experience_7", "Experience_8", "Experience_Score_Total", "crisis_aware", "crisis_learnt", "crisis_explain", "os_explain", "os_learnt", "Experience_Completed") 
Set2_Experience <- Set2_Experience[Set2_Experience$Experience_Completed == 1,]
colnames(Set2_Experience) <- c("ID", "Uni", "Full_Part", "PTY", "Current_Year", "Course_Duration", "Course_Name", "Gender", "UKRN", "timepoint", "Experience_Replication", "Experience_Materials", "Experience_Education", "Experience_Access", "Experience_Data", "Experience_Power", "Experience_Preregistration", "Experience_Preprint", "Experience_Score_Total", "crisis_aware", "crisis_learnt", "crisis_explain", "os_explain", "os_learnt", "Experience_Completed") 





setwd("C:/Users/marto05/OneDrive - Linköpings universitet/10. Side projects/2. STORM/STORM/Analysis Cross-waves/cleaned_data")
write.csv(Set1_AllData, "all_Set1_AllData.csv", row.names = FALSE)
write.csv(Set2_ConceptualPerception, "all_Set2_ConceptualPerception.csv", row.names = FALSE)
write.csv(Set2_SituationalPerception, "all_Set2_SituationalPerception.csv", row.names = FALSE)
write.csv(Set2_Awareness, "all_Set2_Awareness.csv", row.names = FALSE)
write.csv(Set2_Experience, "all_Set2_Experience.csv", row.names = FALSE)

write.csv(cut_dataset, "all_waves_extended.csv", row.names = FALSE)



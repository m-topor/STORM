# STORM DATA SCRIPT
# 20/12/2020, PS (PRE-PROCESSING BY MT) and edited Oct 2022 by KGL

# NOTE - NOT CURRENTLY ANONYMOUS. WHEN WE'RE HAPPY WITH EVERYTHING ELSE, PS WILL CREATE A CODE FOR UNIS
# (WITH A SHEET WHERE WE CAN LOOK IT UP BUT NOT SHARE) AND CHANGE THE CODE HERE ACCORDINGLY

#Note KGL that two different files are needed for the analysis. One with part time students and one without 

# LOAD LIBRARIES AND DATA ----

# removes everything from Environment
rm(list=ls())

install.packages("effsize")
install.packages("ltm")

library(dplyr)
library(stringr)
library(tidyr)
library(car)
library(ggplot2)
library(DescTools)
library(rstatix)
library (effsize)
library(ltm)

#Read in the file
STORM_dataset <- read.csv('STORM_dataset_Wave1.csv')

# SET UP RAINCLOUD PLOT FUNCTION ----

# geom_flat_violin            
"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

geom_flat_violin <- function(mapping = NULL, data = NULL, stat = "ydensity",
                             position = "dodge", trim = TRUE, scale = "area",
                             show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomFlatViolin,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      trim = trim,
      scale = scale,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomFlatViolin <-
  ggproto("GeomFlatViolin", Geom,
          setup_data = function(data, params) {
            data$width <- data$width %||%
              params$width %||% (resolution(data$x, FALSE) * 0.9)
            
            # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
            data %>%
              group_by(group) %>%
              mutate(ymin = min(y),
                     ymax = max(y),
                     xmin = x,
                     xmax = x + width / 2)
            
          },
          
          draw_group = function(data, panel_scales, coord) {
            # Find the points for the line to go all the way around
            data <- transform(data, xminv = x,
                              xmaxv = x + violinwidth * (xmax - x))
            
            # Make sure it's sorted properly to draw the outline
            newdata <- rbind(plyr::arrange(transform(data, x = xminv), y),
                             plyr::arrange(transform(data, x = xmaxv), -y))
            
            # Close the polygon: set first and last point the same
            # Needed for coord_polar and such
            newdata <- rbind(newdata, newdata[1,])
            
            ggplot2:::ggname("geom_flat_violin", GeomPolygon$draw_panel(newdata, panel_scales, coord))
          },
          
          draw_key = draw_key_polygon,
          
          default_aes = aes(weight = 1, colour = "grey20", fill = "white", size = 0.5,
                            alpha = NA, linetype = "solid"),
          
          required_aes = c("x", "y")
  )
# raincloud theme
raincloud_theme <- theme(
  text = element_text(size = 10),
  axis.title.x = element_text(size = 16),
  axis.title.y = element_text(size = 16),
  axis.text = element_text(size = 14),
  axis.text.x = element_text(angle = 45, vjust = 0.5),
  legend.title = element_text(size = 16),
  legend.text = element_text(size = 16),
  legend.position = "right",
  plot.title = element_text(lineheight = .8, face = "bold", size = 16),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  axis.line.x = element_line(colour = "black", size = 0.5, linetype = "solid"),
  axis.line.y = element_line(colour = "black", size = 0.5, linetype = "solid"))
# function for upper and lower bound
lb <- function(x) mean(x) - sd(x)
ub <- function(x) mean(x) + sd(x)




# DESCRIPTIVES & DATA VIZ ----

# make columns numeric
STORM_dataset$Perception_Score_Total <- as.numeric(STORM_dataset$Perception_Score_Total)
STORM_dataset$Awareness_Score_Total <- as.numeric(STORM_dataset$Awareness_Score_Total)
STORM_dataset$Experience_Score_Total <- as.numeric(STORM_dataset$Experience_Score_Total)
STORM_dataset$Knowledge_Score_Total <- as.numeric(STORM_dataset$Knowledge_Score_Total)
STORM_dataset$crisis_aware <- as.numeric(STORM_dataset$crisis_aware)
STORM_dataset$crisis_learnt <- as.numeric(STORM_dataset$crisis_learnt)
STORM_dataset$os_explain <- as.numeric(STORM_dataset$os_explain)
STORM_dataset$os_learnt <- as.numeric(STORM_dataset$os_learnt)
STORM_dataset$applicability <- as.numeric(STORM_dataset$applicability)
STORM_dataset$crisis_explain <- as.numeric(STORM_dataset$crisis_explain)


# plot histograms
ggplot(STORM_dataset, aes(x=Perception_Score_Total)) + geom_histogram()
ggplot(STORM_dataset, aes(x=Awareness_Score_Total)) + geom_histogram()
ggplot(STORM_dataset, aes(x=Experience_Score_Total)) + geom_histogram()
ggplot(STORM_dataset, aes(x=Knowledge_Score_Total)) + geom_histogram()

# descriptives
Perception_descriptives<-STORM_dataset %>%
  drop_na(Perception_Score_Total) %>%
  summarise(mean=mean(Perception_Score_Total),
            sd=sd(Perception_Score_Total))
Perception_descriptives

Awareness_descriptives<-STORM_dataset %>%
  drop_na(Awareness_Score_Total) %>%
  summarise(mean=mean(Awareness_Score_Total),
            sd=sd(Awareness_Score_Total))
Awareness_descriptives

Experience_descriptives<-STORM_dataset %>%
  drop_na(Experience_Score_Total) %>%
  summarise(mean=mean(Experience_Score_Total),
            sd=sd(Experience_Score_Total))
Experience_descriptives

Knowledge_descriptives<-STORM_dataset %>%
  drop_na(Knowledge_Score_Total) %>%
  summarise(mean=mean(Knowledge_Score_Total),
            sd=sd(Knowledge_Score_Total))
Knowledge_descriptives

# DO THE DVS DIFFER BY UKRN status?----

# make UKRN a factor
STORM_dataset$UKRN=factor(STORM_dataset$UKRN)


# t-tests for ordinal DVs
#t-test perception and descriptives by group 
t.test(STORM_dataset$Perception_Score_Total~STORM_dataset$UKRN)
cohen.d (STORM_dataset$Perception_Score_Total~STORM_dataset$UKRN,pooled=TRUE,paired=FALSE,
         na.rm=FALSE, mu=0, hedges.correction=FALSE)

# PERCEPTION - NS

#T-test by group
t.test(STORM_dataset$Awareness_Score_Total~STORM_dataset$UKRN)
cohen.d (STORM_dataset$Awareness_Score_Total~STORM_dataset$UKRN,pooled=TRUE,paired=FALSE,
         na.rm=FALSE, mu=0, hedges.correction=FALSE)

# AWARENESS - Significant

t.test(STORM_dataset$Experience_Score_Total~STORM_dataset$UKRN)
cohen.d (STORM_dataset$Experience_Score_Total~STORM_dataset$UKRN,pooled=TRUE,paired=FALSE,
         na.rm=FALSE, mu=0, hedges.correction=FALSE)

# EXPERIENCE - NS

#T-tests knowledge and descriptives
t.test(STORM_dataset$Knowledge_Score_Total~STORM_dataset$UKRN)
cohen.d (STORM_dataset$Knowledge_Score_Total~STORM_dataset$UKRN,pooled=TRUE,paired=FALSE,
         na.rm=FALSE, mu=0, hedges.correction=FALSE)
# KNOWLEDGE - NS

#Descriptives (split into two groups first)
just_UKRN<- filter(STORM_dataset, UKRN == "UKRN")
mean(just_UKRN$Perception_Score_Total, na.rm = TRUE)
sd (just_UKRN$Perception_Score_Total, na.rm = TRUE)
mean(just_UKRN$Awareness_Score_Total, na.rm = TRUE)
sd (just_UKRN$Awareness_Score_Total, na.rm = TRUE)
mean(just_UKRN$Knowledge_Score_Total, na.rm = TRUE)
sd (just_UKRN$Knowledge_Score_Total, na.rm = TRUE)
mean(just_UKRN$Experience_Score_Total, na.rm = TRUE)
sd (just_UKRN$Experience_Score_Total, na.rm = TRUE)

#then the other group
just_noUKRN<- filter(STORM_dataset, UKRN == "Non-UKRN")
mean(just_noUKRN$Perception_Score_Total, na.rm = TRUE)
sd (just_noUKRN$Perception_Score_Total, na.rm = TRUE)
mean(just_noUKRN$Awareness_Score_Total, na.rm = TRUE)
sd (just_noUKRN$Awareness_Score_Total, na.rm = TRUE)
mean(just_noUKRN$Knowledge_Score_Total, na.rm = TRUE)
sd (just_noUKRN$Knowledge_Score_Total, na.rm = TRUE)
mean(just_noUKRN$Experience_Score_Total, na.rm = TRUE)
sd (just_noUKRN$Experience_Score_Total, na.rm = TRUE)

#Make plots Perception
ggplot(data = STORM_dataset, 
       aes(x = UKRN, y = Perception_Score_Total, fill = UKRN)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = Perception_Score_Total, color = UKRN), 
             position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha = 0.5) +
  coord_flip()+ # flip or not
  theme_classic()+
  theme(legend.position = "none")+
  labs(x="UKRN Status", y = "Contextual Perception Score (Total)")

#Make plots Perception _no jitter
ggplot(data = STORM_dataset, 
       aes(x = UKRN, y = Perception_Score_Total, fill = UKRN)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = Perception_Score_Total, color = UKRN), size = .5, alpha = 0.8) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha = 0.5) +
  coord_flip()+ # flip or not
  theme_classic()+
  theme(legend.position = "none")+
  labs(x="UKRN Status", y = "Contextual Perception Score (Total)")

#Make plots Knowledge
ggplot(data = STORM_dataset, 
       aes(x = UKRN, y = Knowledge_Score_Total, fill = UKRN)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = Knowledge_Score_Total, color = UKRN), 
             position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha = 0.5) +
  coord_flip()+ # flip or not
  theme_classic()+
  theme(legend.position = "none")+
  labs(x="UKRN Status", y = "Knowledge Score (Total)")

#Make plots Knowledge_no jitter 
ggplot(data = STORM_dataset, 
       aes(x = UKRN, y = Knowledge_Score_Total, fill = UKRN)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = Knowledge_Score_Total, color = UKRN), size = .5, alpha = 0.8) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha = 0.5) +
  coord_flip()+ # flip or not
  theme_classic()+
  theme(legend.position = "none")+
  labs(x="UKRN Status", y = "Situational Perception Score (Total)")

#Make plots Awareness
ggplot(data = STORM_dataset, 
       aes(x = UKRN, y = Awareness_Score_Total, fill = UKRN)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = Awareness_Score_Total, color = UKRN), 
             position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha = 0.5) +
  coord_flip()+ # flip or not
  theme_classic()+
  theme(legend.position = "none")+
  labs(x="UKRN Status", y = "Awareness Score (Total)")

#Make plots Awareness_no jitter
ggplot(data = STORM_dataset, 
       aes(x = UKRN, y = Awareness_Score_Total, fill = UKRN)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = Awareness_Score_Total, color = UKRN), size = .5, alpha = 0.8) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha = 0.5) +
  coord_flip()+ # flip or not
  theme_classic()+
  theme(legend.position = "none")+
  labs(x="UKRN Status", y = "Awareness Score (Total)")

#Make plots Experience
ggplot(data = STORM_dataset, 
       aes(x = UKRN, y = Experience_Score_Total, fill = UKRN)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = Experience_Score_Total, color = UKRN), 
             position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha = 0.5) +
  coord_flip()+ # flip or not
  theme_classic()+
  theme(legend.position = "none")+
  labs(x="UKRN Status", y = "Experience Score (Total)")

#Make plots Experience_no jitter
ggplot(data = STORM_dataset, 
       aes(x = UKRN, y = Experience_Score_Total, fill = UKRN)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = Experience_Score_Total, color = UKRN), size = .5, alpha = 0.8) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha = 0.5) +
  coord_flip()+ # flip or not
  theme_classic()+
  theme(legend.position = "none")+
  labs(x="UKRN Status", y = "Experience Score (Total)")

# CHI squared for all categorical

table(STORM_dataset$UKRN, STORM_dataset$crisis_aware)
chisq.test(STORM_dataset$UKRN, STORM_dataset$crisis_aware)
# CRISIS AWARE - Sig

table(STORM_dataset$UKRN, STORM_dataset$crisis_learnt)
chisq.test(STORM_dataset$UKRN, STORM_dataset$crisis_learnt)
# CRISIS LEARNT - NS

table(STORM_dataset$UKRN, STORM_dataset$os_explain)
chisq.test(STORM_dataset$UKRN, STORM_dataset$os_explain)
# OS EXPLAIN - NS

table(STORM_dataset$UKRN, STORM_dataset$os_learnt)
chisq.test(STORM_dataset$UKRN, STORM_dataset$os_learnt)
# OS LEARNT - Sig

table(STORM_dataset$UKRN, STORM_dataset$applicability)
chisq.test(STORM_dataset$UKRN, STORM_dataset$applicability)
# OS LEARNT - NS

table(STORM_dataset$UKRN, STORM_dataset$crisis_explain)
chisq.test(STORM_dataset$UKRN, STORM_dataset$crisis_explain)
# crisis explain- Sig



# DOES PERFORMANCE ON THE PRIMARY DVS DIFFER FOR DIFFERENT PRINCIPLES?----

# 1 way ANOVAS with score as DV and OR principle as IV (continuous)
# for categorical, chi squared (8 OR principles x 2 response categories)

# PERCEPTION
# select only relevant columns to make things clearer
STORM_perception<-STORM_dataset %>%
  dplyr::select(X,Perception_1:Perception_16)

# make new columns for principle scores
STORM_perception$Replication=rowMeans(STORM_perception[,c('Perception_1', 'Perception_2')])
STORM_perception$Materials=rowMeans(STORM_perception[,c('Perception_3', 'Perception_4')])
STORM_perception$Education=rowMeans(STORM_perception[,c('Perception_5', 'Perception_6')])
STORM_perception$Access=rowMeans(STORM_perception[,c('Perception_7', 'Perception_8')])
STORM_perception$Data=rowMeans(STORM_perception[,c('Perception_9', 'Perception_10')])
STORM_perception$Power=rowMeans(STORM_perception[,c('Perception_11', 'Perception_12')])
STORM_perception$Preregistration=rowMeans(STORM_perception[,c('Perception_13', 'Perception_14')])
STORM_perception$Preprint=rowMeans(STORM_perception[,c('Perception_15', 'Perception_16')])

# KNOWLEDGE
# select only relevant columns to make things clearer
STORM_knowledge<-STORM_dataset %>%
  dplyr::select(X,Knowledge_1:Knowledge_8)

# only one Q per principle, so just rename
colnames(STORM_knowledge) <- c('x','Replication', 'Materials', 'Preregistration', 'Access', 'Preprint', 'Data', 
                               'Power', 'Education')

# AWARENESS
# select only relevant columns to make things clearer
STORM_awareness<-STORM_dataset %>%
  dplyr::select(X, Awareness_1:Awareness_8)

# only one Q per principle, so just rename
colnames(STORM_awareness) <- c('x', 'Data','Materials', 'Access', 'Preregistration', 'Preprint', 'Education', 
                               'Power', 'Replication')

# EXPERIENCE
# select only relevant columns to make things clearer
STORM_experience<-STORM_dataset %>%
  dplyr::select(X, Experience_1:Experience_8)

# only one Q per principle, so just rename
colnames(STORM_experience) <- c('x','Data', 'Materials', 'Access', 'Preregistration', 'Preprint', 'Education', 
                                'Power', 'Replication')

#Reliability tests 
data_perception_forreliability <- STORM_perception[,-c(1)]
cronbach.alpha(data_perception_forreliability, na.rm=TRUE)
data_knowledge_forreliability <- STORM_knowledge[,-c(1)]
cronbach.alpha(data_knowledge_forreliability, na.rm=TRUE)
data_awareness_forreliability <- STORM_awareness[,-c(1)]
cronbach.alpha(data_awareness_forreliability, na.rm=TRUE)
data_experience_forreliability <- STORM_experience[,-c(1)]
cronbach.alpha(data_experience_forreliability, na.rm=TRUE)


# make long data in order to do repeated measures ANOVAs
Perception_Long <- gather(STORM_perception, principle, score, Replication:Preprint, factor_key=TRUE)
Knowledge_Long <- gather(STORM_knowledge, principle, score, Replication:Education, factor_key=TRUE)
Awareness_Long <- gather(STORM_awareness, principle, score, Data:Replication, factor_key=TRUE)
Experience_Long <- gather(STORM_experience, principle, score, Data:Replication, factor_key=TRUE)

# 1 way ANOVAS with score as DV and OR principle as IV 
anova_test(data=Perception_Long, dv=score, wid=X, within=principle, effect.size = "pes")
# p=0
# follow up t-tests
pairwise.t.test(Perception_Long$score, Perception_Long$principle, p.adj = "bonf")
# sig: materials-replication, education-replication, access-replication, data-replication, prereg-replication,
# preprint-replication, data-materials, power-materials, prereg-materials, preprint-materials, data-education,
# power-education, prereg-education, preprint-education, data-access, power-access, prereg-access, preprint-access,
# power-data, preprint-data, prereg-power, preprint-power, preprint-prereg
# not sig: power-replication, education-materials, access-materials, access-education, prereg-data
# make principles in alphabetical order
Perception_Long$principle <- factor(Perception_Long$principle,levels 
                                    = c("Replication","Power","Materials","Education","Access","Preregistration",
                                        "Data","Preprint"))
# plot Perception
ggplot(data = Perception_Long, 
       aes(x = principle, y = score, fill = principle)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = score, color = principle), 
             position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha = 0.5) +
  coord_flip() + # flip or not
  theme_classic()+
  theme(legend.position = "none")+
  labs(x="Principle", y = "Contextual Perception Score (Total)")


# plot perception_no jitter
perception_plot <- ggplot(data = Perception_Long, 
                          aes(x = principle, y = score, fill = principle)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = score, color = principle), size = .5, alpha = 0.8) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha = 0.5) +
  coord_flip() + # flip or not
  theme_classic()+
  theme(legend.position = "none")+
  labs(x="Principle", y = "Contextual Perception Score (Total)")

perception_plot

perception_plot2 <-perception_plot + scale_x_discrete(labels =c("Replication studies","Open Materials","Open Educational Materials","Open Access","Open Data", "Statistical Power", "Preregistration",
                                                                "Preprints"))
perception_plot2


anova_test(data=Knowledge_Long, dv=score, wid=x, within=principle,effect.size = "pes")

# p=0
# follow up t-tests
pairwise.t.test(Knowledge_Long$score, Knowledge_Long$principle, p.adj = "bonf")

# sig: materials-replication, prereg-replication, access-replication, preprint-replication, data-replication,
# power-replication, education-replication, access-materials, preprint-materials, data-materials, access-prereg,
# preprint-prereg, data-prereg, preprint-access, data-access, power-access, education-access, data-preprint,
# power-preprint, education-preprint, power-data, education-data
# nonsig: prereg-materials, power-materials, education-materials, power-prereg, education-prereg, education-power
# make principles in alphabetical order
Knowledge_Long$principle <- factor(Knowledge_Long$principle,levels 
                                   = c("Replication","Power","Materials","Education","Access","Preregistration",
                                       "Data","Preprint"))
# plot knowledge
ggplot(data = Knowledge_Long, 
       aes(x = principle, y = score, fill = principle)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = score, color = principle), 
             position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha = 0.5) +
  coord_flip() + # flip or not
  theme_classic()+
  theme(legend.position = "none")+
  labs(x="Principle", y = "Situational Perception Score (Total)")

# plot knowledge_no jitter
knowledge_plot <- ggplot(data = Knowledge_Long, 
       aes(x = principle, y = score, fill = principle)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = score, color = principle), size = .5, alpha = 0.8) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha = 0.5) +
  coord_flip() + # flip or not
  theme_classic()+
  theme(legend.position = "none")+
  labs(x="Principle", y = "Situational Perception (Total)")
knowledge_plot


knowledge_plot2 <-knowledge_plot + scale_x_discrete(labels =c("Replication studies","Open Materials","Open Educational Materials","Open Access","Open Data", "Statistical Power", "Preregistration",
                                                                "Preprints"))
knowledge_plot2

# CHI SQ FOR AWARENESS & EXPERIENCE

# chi sq

# awareness
table(Awareness_Long$principle, Awareness_Long$score)
principles_awareness<-chisq.test(Awareness_Long$principle, Awareness_Long$score)
principles_awareness
# p < .001
# standardised residuals
principles_awareness$stdres
# More likely to be aware of data, open access, & replication than not
# More likely to be not aware of open materials, prereg, preprints, & power than aware 
# no diff for open educational materials

# experience
table(Experience_Long$principle, Experience_Long$score)
principles_experience<-chisq.test(Experience_Long$principle, Experience_Long$score)
principles_experience
# p < .001
# standardised residuals
principles_experience$stdres
# More likely to have experience with open access, open educational materials, and replication than not
# More likely to not have experience with open data, open materials, prereg, preprints, & power than have


# DOES PERFORMANCE ON THE DIFFERENT ORDINAL DVS DIFFER? ----

# primary DVs only

# only include those that completed all scales
STORM_allscales<-subset(STORM_dataset, Perception_Completed==1 & Awareness_Completed==1 & 
                          Experience_Completed==1 & Knowledge_Completed==1)

# create z scores
STORM_allscales<-STORM_allscales %>% 
  drop_na(Perception_Score_Total, Awareness_Score_Total, Experience_Score_Total, Knowledge_Score_Total) %>%
  mutate(PerceptionZ = (Perception_Score_Total - mean(Perception_Score_Total))/sd(Perception_Score_Total)) %>%
  mutate(AwarenessZ = (Awareness_Score_Total - mean(Awareness_Score_Total))/sd(Awareness_Score_Total)) %>%
  mutate(ExperienceZ = (Experience_Score_Total - mean(Experience_Score_Total))/sd(Experience_Score_Total)) %>%
  mutate(KnowledgeZ = (Knowledge_Score_Total - mean(Knowledge_Score_Total))/sd(Knowledge_Score_Total))

# make long data in order to do repeated measures ANOVA
STORM_long <- gather(STORM_allscales, Scale, zScore, PerceptionZ:KnowledgeZ, factor_key=TRUE)

# Repeated measures ANOVAs to compare performance across scales 
# DV will be z score, IV will be scale (knowledge, perception, awareness, experience)
anova_test(data=STORM_long, dv=zScore, wid=X, within=Scale, effect.size = "pes")

# NS

# DO THE DVS DIFFER BY ACADEMIC YEAR GROUP? ----

#Read in the correct file_only full time students
STORM_datasetFT <- read.csv('Pre-processed_Data_wave1_FTonly.csv')

#Make 9999 say missing
STORM_datasetFT["crisis_explain"][STORM_datasetFT["crisis_explain"]==9999] <-NA


#Descriptives for each year group 

#then the other group
#Just Year 1
just_Year_1<- filter(STORM_datasetFT, Current_Year == "Year 1")
mean(just_Year_1$Perception_Score_Total, na.rm = TRUE)
sd (just_Year_1$Perception_Score_Total, na.rm = TRUE)
mean(just_Year_1$Awareness_Score_Total, na.rm = TRUE)
sd (just_Year_1$Awareness_Score_Total, na.rm = TRUE)
mean(just_Year_1$Knowledge_Score_Total, na.rm = TRUE)
sd (just_Year_1$Knowledge_Score_Total, na.rm = TRUE)
mean(just_Year_1$Experience_Score_Total, na.rm = TRUE)
sd (just_Year_1$Experience_Score_Total, na.rm = TRUE)

#Just Year 2
just_Year_2<- filter(STORM_datasetFT, Current_Year == "Year 2")
mean(just_Year_2$Perception_Score_Total, na.rm = TRUE)
sd (just_Year_2$Perception_Score_Total, na.rm = TRUE)
mean(just_Year_2$Awareness_Score_Total, na.rm = TRUE)
sd (just_Year_2$Awareness_Score_Total, na.rm = TRUE)
mean(just_Year_2$Knowledge_Score_Total, na.rm = TRUE)
sd (just_Year_2$Knowledge_Score_Total, na.rm = TRUE)
mean(just_Year_2$Experience_Score_Total, na.rm = TRUE)
sd (just_Year_2$Experience_Score_Total, na.rm = TRUE)

#Just placement 
just_Placement<- filter(STORM_datasetFT, Current_Year == "Placement Year")
mean(just_Placement$Perception_Score_Total, na.rm = TRUE)
sd (just_Placement$Perception_Score_Total, na.rm = TRUE)
mean(just_Placement$Awareness_Score_Total, na.rm = TRUE)
sd (just_Placement$Awareness_Score_Total, na.rm = TRUE)
mean(just_Placement$Knowledge_Score_Total, na.rm = TRUE)
sd (just_Placement$Knowledge_Score_Total, na.rm = TRUE)
mean(just_Placement$Experience_Score_Total, na.rm = TRUE)
sd (just_Placement$Experience_Score_Total, na.rm = TRUE)

#Just final year
just_Final_Year<- filter(STORM_datasetFT, Current_Year == "Final Year")
mean(just_Final_Year$Perception_Score_Total, na.rm = TRUE)
sd (just_Final_Year$Perception_Score_Total, na.rm = TRUE)
mean(just_Final_Year$Awareness_Score_Total, na.rm = TRUE)
sd (just_Final_Year$Awareness_Score_Total, na.rm = TRUE)
mean(just_Final_Year$Knowledge_Score_Total, na.rm = TRUE)
sd (just_Final_Year$Knowledge_Score_Total, na.rm = TRUE)
mean(just_Final_Year$Experience_Score_Total, na.rm = TRUE)
sd (just_Final_Year$Experience_Score_Total, na.rm = TRUE)

# run ANOVA test (you need to run the anova twice the first bit tells you the main ANOVA outuput. The second bit is needed to generate the eta squared values)

# ANOVA with year group (Y1, Y2, placement, final) as DV and all continuous outcome measures as IVs
summary(aov(STORM_datasetFT$Perception_Score_Total~STORM_datasetFT$Current_Year))
anova_perception<-(aov(STORM_datasetFT$Perception_Score_Total~STORM_datasetFT$Current_Year))
EtaSq(anova_perception,type=1,anova=TRUE)
# PERCEPTION - NS

# plot_perception
STORM_datasetFT$Current_Year <- factor(STORM_datasetFT$Current_Year, levels = c("Year 1", "Year 2", "Placement Year", "Final Year"))
ggplot(data = STORM_datasetFT, 
       aes(x = Current_Year, y = Perception_Score_Total, fill = Current_Year)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = Perception_Score_Total, color = Current_Year), 
             position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha = 0.5) +
  scale_y_continuous(limits = c(-10, 30), breaks = seq(-10, 30, by = 10))+
  coord_flip()+ # flip or not
  theme_classic()+
  theme(legend.position = "none")+
  labs(x="Current Year", y = "Contextual Perceptions Score (Total)")

# plot perception_no jitter
STORM_datasetFT$Current_Year <- factor(STORM_datasetFT$Current_Year, levels = c("Year 1", "Year 2", "Placement Year", "Final Year"))
ggplot(data = STORM_datasetFT, 
       aes(x = Current_Year, y = Perception_Score_Total, fill = Current_Year)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = Perception_Score_Total, color = Current_Year), size = .5, alpha = 0.8) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha = 0.5) +
  scale_y_continuous(limits = c(-10, 30), breaks = seq(-10, 30, by = 10))+
  coord_flip()+ # flip or not
  theme_classic()+
  theme(legend.position = "none")+
  labs(x="Current Year", y = "Contextual Perceptions Score (Total)")

summary(aov(STORM_datasetFT$Awareness_Score_Total~STORM_datasetFT$Current_Year))
anova_awareness<-(aov(STORM_datasetFT$Awareness_Score_Total~STORM_datasetFT$Current_Year))
EtaSq(anova_awareness,type=1,anova=TRUE)
# AWARENESS - NS

#Plot Awareness
STORM_datasetFT$Current_Year <- factor(STORM_datasetFT$Current_Year, levels = c("Year 1", "Year 2", "Placement Year", "Final Year"))
ggplot(data = STORM_datasetFT, 
       aes(x = Current_Year, y = Awareness_Score_Total, fill = Current_Year)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = Awareness_Score_Total, color = Current_Year), 
             position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha = 0.5) +
  coord_flip() + # flip or not
  theme_classic()+
  theme(legend.position = "none")+
  labs(x="Current Year", y = "Awareness Score (Total)")


#Plot Awareness_no jitter
STORM_datasetFT$Current_Year <- factor(STORM_datasetFT$Current_Year, levels = c("Year 1", "Year 2", "Placement Year", "Final Year"))
ggplot(data = STORM_datasetFT, 
       aes(x = Current_Year, y = Awareness_Score_Total, fill = Current_Year)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = Awareness_Score_Total, color = Current_Year), size = .5, alpha = 0.8) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha = 0.5) +
  coord_flip() + # flip or not
  theme_classic()+
  theme(legend.position = "none")+
  labs(x="Current Year", y = "Awareness Score (Total)")

summary(aov(STORM_datasetFT$Experience_Score_Total~STORM_datasetFT$Current_Year))
anova_experience<-(aov(STORM_datasetFT$Experience_Score_Total~STORM_datasetFT$Current_Year))
EtaSq(anova_experience,type=1,anova=TRUE)
# EXPERIENCE - SIG p=.015
#Follow up t-tests 
pairwise.t.test(STORM_datasetFT$Experience_Score_Total, STORM_datasetFT$Current_Year, p.adj = "bonf")

# plot Experience
STORM_datasetFT$Current_Year <- factor(STORM_datasetFT$Current_Year, levels = c("Year 1", "Year 2", "Placement Year", "Final Year"))
ggplot(data = STORM_datasetFT, 
       aes(x = Current_Year, y = Experience_Score_Total, fill = Current_Year)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = Experience_Score_Total, color = Current_Year), 
             position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha = 0.5) +
  coord_flip() + # flip or not
  theme_classic()+
  theme(legend.position = "none")+
  labs(x="Current Year", y = "Experience Score (Total)")

# plot Experience_no jitter
STORM_datasetFT$Current_Year <- factor(STORM_datasetFT$Current_Year, levels = c("Year 1", "Year 2", "Placement Year", "Final Year"))
ggplot(data = STORM_datasetFT, 
       aes(x = Current_Year, y = Experience_Score_Total, fill = Current_Year)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = Experience_Score_Total, color = Current_Year), size = .5, alpha = 0.8) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha = 0.5) +
  coord_flip() + # flip or not
  theme_classic()+
  theme(legend.position = "none")+
  labs(x="Current Year", y = "Experience Score (Total)")

summary(aov(STORM_datasetFT$Knowledge_Score_Total~STORM_datasetFT$Current_Year))
anova_knowledge<-(aov(STORM_datasetFT$Knowledge_Score_Total~STORM_datasetFT$Current_Year))
EtaSq(anova_knowledge,type=1,anova=TRUE)
# KNOWLEDGE - NS

#Plot Knowledge
STORM_datasetFT$Current_Year <- factor(STORM_datasetFT$Current_Year, levels = c("Year 1", "Year 2", "Placement Year", "Final Year"))
ggplot(data = STORM_datasetFT, 
       aes(x = Current_Year, y = Knowledge_Score_Total, fill = Current_Year)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = Knowledge_Score_Total, color = Current_Year), 
             position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha = 0.5) +
  scale_y_continuous(limits = c(0, 35), breaks = seq(0,35, by = 10))+
  coord_flip() + # flip or not
  theme_classic()+
  theme(legend.position = "none")+
  labs(x="Current Year", y = "Sitautional Perceptions Score (Total)")


#Plot Knowledge_no jitter
STORM_datasetFT$Current_Year <- factor(STORM_datasetFT$Current_Year, levels = c("Year 1", "Year 2", "Placement Year", "Final Year"))
ggplot(data = STORM_datasetFT, 
       aes(x = Current_Year, y = Knowledge_Score_Total, fill = Current_Year)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = Knowledge_Score_Total, color = Current_Year), size = .5, alpha = 0.8) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha = 0.5) +
  scale_y_continuous(limits = c(0, 35), breaks = seq(0,35, by = 10))+
  coord_flip() + # flip or not
  theme_classic()+
  theme(legend.position = "none")+
  labs(x="Current Year", y = "Situational Perceptions Score (Total)")

# CHI squared for all categorical

table(STORM_datasetFT$Current_Year, STORM_datasetFT$crisis_aware)
chisq.test(STORM_datasetFT$Current_Year, STORM_datasetFT$crisis_aware)
# CRISIS AWARE - p<.001

table(STORM_datasetFT$Current_Year, STORM_datasetFT$crisis_learnt)
chisq.test(STORM_datasetFT$Current_Year, STORM_datasetFT$crisis_learnt)
# CRISIS LEARNT - p=0.001

table(STORM_datasetFT$Current_Year, STORM_datasetFT$os_explain)
chisq.test(STORM_datasetFT$Current_Year, STORM_datasetFT$os_explain)
# OS EXPLAIN - NS

table(STORM_datasetFT$Current_Year, STORM_datasetFT$os_learnt)
chisq.test(STORM_datasetFT$Current_Year, STORM_datasetFT$os_learnt)
# OS LEARNT - p=0.002

table(STORM_datasetFT$Current_Year, STORM_datasetFT$applicability)
chisq.test(STORM_datasetFT$Current_Year, STORM_datasetFT$applicability)
# OS LEARNT - NS

table(STORM_datasetFT$Current_Year, STORM_datasetFT$crisis_explain)
chisq.test(STORM_datasetFT$Current_Year, STORM_datasetFT$crisis_explain)
# RC Explain - NS

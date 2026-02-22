# STORM Project Data and Script Repository  
  
## Project summary  
The project was conducted at British universities and aimed to investigate student knowldge and perceptions regarding open and reproducible research practices.  
  
Only psychology students were invited. The study involved an online survey.  
  
Data were collected in three waves - 2020, 2021 and 2022.  
  
Data were analysed with the consideration of whether the students' university was part of the UKRN and which year of study the students were in.  
  
Project's OSF page:  
Project pre-print:    
Published article:    
  
## Details about data and analyses  

### General  Structure
The repository is divided into folders with data and scripts for each of the data collection waves.  
  
In each folder, there are two raw-anonymised data files: STORM_Project_year_anonymised.csv and crisis_explain_waveX.csv.   
These files have to be pre-processed before any data analysis can take place. The file STORM_Project_year_anonymised.csv is the raw dataset downloaded from Qualtrics. The other file, crisis_explain_waveX.csv only contains information about one outcome variable - crisis_explain - which is where student explained what is mean by the "reproducibility crisis". This was a qualitative variable which was later coded as either correct or incorrect. The coding is saved in the  crisis_explain_waveX.csv file. The two files are merged during preprocessing.
  
There are two scripts: s1_Preprocessing and s2_Plotting.  
These scripts prepare the data for analyses and visualise the achieved data patterns.  
  
There are two folders: cleaned_data and stats.   
Preprocessed data resulting from script s1_Preprocessing are located in the cleaned_data folder. A Meta-Data text file explaining the variables in the data files is also placed there.  
Statistical models are placed in Stan and R scripts in the folder stats. There is also a text file explaining the data analysis plan for that specific wave.   

### Main differences between the waves  
Below is a list of differences between waves 1, 2 and 3  

1. During Wave 2, participants also answered some additional questionnaires that were part of separate analyses - link to the paper:  
As a result, the structure of the file is slightly different. Age was collected from participants at Wave 2 but not at Wave 1.   
  
2. The raw data file downloaded for Wave 3 has some empty columns and some columns are already transformed into numbers. The Gender column is categorised as 1 = male, 2 = female, 3 = non-binary and 4 = other. All variables of interest already have numerical values and they are recoded in the preprocessing script to ensure that they are consistent with the other two waves. The columns full time/part time study, PTY for professional training year, Course_Duration and Previous_Training are epmty as the data were not collected. In waves 1 and 2 we filtered out participants who were not full time students. For this wave, this is not possible.  
  
### To reproduce the results.  
The data were analysed on a Windows computer and the analyses have not been tested on a Mac to date.  
Preparations:  
* RStudio   
* The scripts are written in RMarkdown, if you have not used markdown in R before, you will need to install packages: rmarkdown and knitr. Separately to RStudio, you should also install MikTex. MikTex is needed to save the output as a PDF. An alterntive solution is to change the output format to html (the first few lines of the code). Then MikTex will not be needed.    
* Install the rstan package  
* Install the ggplot2 package  

Clone the repository to your computer and open the R project.   
  
The folders are divided by Waves. Within each folder, you will find a preprocessing script and raw data. The data are already preprocessed and the clean data can be found in the cleaned_data folder. You don't need to do anything with those. For the main results, go into the stats folder and run the .Rmd file.  
  
Note that each script takes a long time to run and the time might differ for different machines. For a not very fancy Windows laptop, it's ~2h per script (~4h for Wave 3).  
  

  

#' Title: A2 Hospital Readmission
#' Author: Priscilla Chacur
#' Date: April 2023

# libraries
library(stringr)
library(DataExplorer)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(powerjoin)
library(scales)
library(skimr)
library(tm)
library(vtreat)
library(pROC)
library(qdapRegex)
library(data.table)
library(ModelMetrics)
library(MLmetrics)
library(caret)
library(rpart.plot)
library(gridExtra)
library(cowplot)


# Set working directory
setwd("~/Desktop/Data Visualization with R/hult_R_class/personalFiles")

#Getting rid of scientific notation
# Options
options(scipen = 999)

#Getting each data file individually
#Test files
diabetesHospitalInfoTest <- read.csv('~/Desktop/Data Visualization with R/hult_R_class/personalFiles/A2/caseData/diabetesHospitalInfoTest.csv')
diabetesMedsTest <- read.csv('~/Desktop/Data Visualization with R/hult_R_class/personalFiles/A2/caseData/diabetesMedsTest.csv')
diabetesPatientTest <- read.csv('~/Desktop/Data Visualization with R/hult_R_class/personalFiles/A2/caseData/diabetesPatientTest.csv')

#Train files  
diabetesHospitalInfoTrain <- read.csv('~/Desktop/Data Visualization with R/hult_R_class/personalFiles/A2/caseData/diabetesHospitalInfoTrain.csv')
diabetesMedsTrain <- read.csv('~/Desktop/Data Visualization with R/hult_R_class/personalFiles/A2/caseData/diabetesMedsTrain.csv')
diabetesPatientTrain <- read.csv('~/Desktop/Data Visualization with R/hult_R_class/personalFiles/A2/caseData/diabetesPatientTrain.csv')

#################################################################################
#################### UNDERSTANDING OF INDIVIDUAL DATASET ########################
#################################################################################


summary(diabetesHospitalInfoTest)
summary(diabetesMedsTest)
summary(diabetesPatientTest)
summary(diabetesHospitalInfoTrain)
summary(diabetesMedsTrain)
summary(diabetesPatientTrain)

skim(diabetesHospitalInfoTest)

#################################################################################
############################# MERGING DATASETS #################################
#################################################################################


#Getting the data
folderPath <- '~/Desktop/Data Visualization with R/hult_R_class/personalFiles/A2/caseData'

#Getting all csv files in a folder (reg expression with .csv)

#Testing files
# Multiple files as a list
test <- list.files(path       = folderPath, 
                  pattern    = '*Test.csv',
                  full.names = T)

#List apply function --> reading all csv files
testingdata <- lapply(test, read.csv)

# Using merge to put all the data together
testDF <- power_left_join(testingdata, by = "tmpID")


#Training files
# Multiple files as a list
train <- list.files(path       = folderPath, 
                   pattern    = '*Train.csv',
                   full.names = T)

#List apply function --> reading all csv files
trainingdata <- lapply(train, read.csv)

# Using merge to put all the data together
trainDF <- power_left_join(trainingdata, by = "tmpID")


#Add column to both df to identify test or train
testDF$set_identifyer <- "test"
trainDF$set_identifyer <- "train"

#Merging both datasets
alldata <- rbind(testDF, trainDF)

summary(alldata$set_identifyer)

summary(alldata)
str(alldata)
dim(alldata)
sapply(alldata,class)

#################################################################################
############################# CLEANING DATASET #################################
#################################################################################

skim(alldata)

#Checking for each column 
unique(alldata$acetohexamide)
 
#Substituting "?" for null - race
alldata$race <- gsub("\\?", NA, alldata$race)
unique(alldata$race)

#Substituting "?" for null- payer_code
alldata$payer_code <- gsub("\\?", NA, alldata$payer_code)
unique(alldata$payer_code)

#Substituting "None" for 0- max_glu_serum
alldata$max_glu_serum <- gsub("\\None", 0, alldata$max_glu_serum)

#Substituting "?" for null - medical_specialty
alldata$medical_specialty <- gsub("\\?", NA, alldata$medical_specialty)
unique(alldata$medical_specialty)


##Organizing Variables

unique(alldata$max_glu_serum)

skim(alldata)

#Organizing medicine amount variables

  #Variables with values as ("No","Down", "Steady, "UP")
    #variables -> metformin, repaglinide, nateglinide, chlorpropamide, glimepiride, glipizide, glyburide
                #tolbutamide, pioglitazone,rosiglitazone, acarbose, miglitol, tolazamide, insulin
    #Substitute ("No" --> 0 ,"Down"--> 1 , "Steady--> 2, "UP"--> 3)
    medicine_amount <- c(
         "metformin", "repaglinide", "nateglinide", "chlorpropamide", "glimepiride", "glipizide", "glyburide",
          "tolbutamide", "pioglitazone","rosiglitazone", "acarbose", "miglitol", "tolazamide", "insulin")
  
    # modify only the columns selected in medicine amount
    alldata <- alldata %>% 
      mutate(across(medicine_amount, ~recode(., "No"=0, "Down"=1, "Steady"=2, "Up"=3)))
    
    # show the frequency of each value in each selected column
    alldata %>% 
      select(all_of(medicine_amount)) %>% 
      lapply(table)

#Substituting "None" for 0- max_glu_serum
    alldata$max_glu_serum <- gsub("\\None", 0, alldata$max_glu_serum)

#Variables with just one value --> able to identify based on n_unique from skim()
    
  #Variables with 10,000 values of "No" --> acetohexamide, troglitazone, examide, citoglipton
  #Action: Drop the columns 
  # select columns with just one unique value
    cols_to_drop <- alldata %>% 
      select("acetohexamide", "troglitazone", "examide", "citoglipton") %>% 
      select_if(~n_distinct(.) == 1) %>% 
      colnames()
    
    # drop columns with just one unique value
    alldata <- alldata %>% 
      select(-all_of(cols_to_drop))

#Variable with 2 values
  #change
      #Ch   No 
      #4276 5724 
  #diabetesMed
      #No  Yes 
      #2522 7478 
  #gender
  table(alldata$gender)
  #Use ifelse() to change values to 1 and 0s
  alldata$change <- ifelse(alldata$change == "Ch", 1, 0)
  alldata$diabetesMed <- ifelse(alldata$diabetesMed == "Yes", 1, 0)
  alldata$gender <- ifelse(alldata$gender == "Female", 1, 0)
  
#Grouping "Not Available", "", "Not Mapped" together
  #admission_type_id
    #use ifelse() function to replace "", "Not Available" or "Not Mapped" with Not Specified
    alldata$admission_type_id <- ifelse(alldata$admission_type_id %in% c("", "Not Available", "Not Mapped"), "Not specified", 
                                        alldata$admission_type_id)
 
#admission_source_id
    #use ifelse() function to replace "", "Not Available" or "Not Mapped" with Not Specified
    alldata$admission_source_id <- ifelse(alldata$admission_source_id %in% c("", "Not Available", "Not Mapped"), "Not specified", 
                                        alldata$admission_source_id)

#A1Cresult
    #A1C Test measures the hemoglobin protein in the blood that is coated with sugar (%)
    #The greater A1C, the greater the risk is for developing type 2 diabetes.
    alldata <- alldata %>% 
      mutate(across(A1Cresult, ~recode(., "None"=0, "Norm"=1, ">7"=2, ">8"=3)))

#max_glu_serum
    #The glucose serum test is most often used to screen for Types 1 and 2 diabetes
    #The greater A1C, the greater the risk is for developing diabetes
    alldata <- alldata %>% 
      mutate(across(max_glu_serum, ~recode(., "0"=0, "Norm"=1, ">200"=2, ">300"=3)))

str(alldata)

#Checking the data again
str(alldata)
skim(alldata)

sapply(alldata,class)

#Getting the % of null for each column
  pMiss <- function(x){sum(is.na(x))/length(x)*100}
    missing <- apply(alldata,2,pMiss)
  # combine column names and missing percentages into a character vector
  missing_info <- paste(names(missing), round(missing, 2))
  # display the missing information
  cat(paste(missing_info, collapse = "\n"))
  
  
#Understanding the % of data missing in each column (plotting)
  #Creating temp list to get missing values
  tmp <- list()
  for(i in 1:ncol(alldata)){
    print(i)
    x <- alldata[,i]
    x <- as.character(x)  # convert to character vector
    x <- ifelse(nchar(x)==0,NA,x)
    tmp[[i]] <- x
  }
  tmp <- do.call(cbind, tmp)
  colnames(tmp) <- names(alldata)
  tmp <- as.data.frame(tmp)
  plot_missing(tmp, title = 'Percentage of Missing Values in Each Column')
  ggsave("missing_values.png")

  
#discharge_disposition_id
  table(alldata$discharge_disposition_id)
  #195 Expired (This means the patient has passed and for that reason, it, obviously will not be returning to the hospital)
  #dropping expired value
  # Drop rows with "Expired" as discharge_disposition_id value
  alldata <- alldata[alldata$discharge_disposition_id != "Expired", ]
  alldata$discharge_disposition_id <- ifelse(alldata$discharge_disposition_id == "", "Not Mapped", alldata$discharge_disposition_id)
  unique(alldata$discharge_disposition_id)
  
  
#################################################################################
##################### SEPARATING TRAIN AND TEST #################################
#################################################################################

# Splitting the data into train and test sets based on set_identifyer so that
#we can use 
trainDF <- alldata[alldata$set_identifyer == "train", ]
testDF <- alldata[alldata$set_identifyer == "test", ]

################################# EDA ON TRAIN ################################# 

# Age distribution
ggplot(data = trainDF, aes(x = age, fill = readmitted_y)) +
  geom_histogram(binwidth = 5, color = "black", position = "dodge") +
  labs(title = "Distribution of Age by Readmission", x = "Age", y = "Count", fill = "Readmission")
  ggsave("Distribution of Age by Readmission.png")



# Weight distribution
  ggplot(data = trainDF, aes(x = wgt, fill = readmitted_y)) +
    geom_histogram(binwidth = 5, color = "black", position = "dodge") +
    labs(title = "Distribution of Weight by Readmission", x = "Weight", y = "Count", fill = "Readmission")
  #Hard to get conclusions from it because there is no info on height

# Number diagnoses distribution
  ggplot(data = trainDF, aes(x = number_diagnoses, fill = readmitted_y)) +
    geom_histogram(binwidth = 1, color = "black", position = "dodge") +
    labs(title = "Distribution of Number of Diagnoses by Readmission", x = "Number of Diagnoses", y = "Count", fill = "Readmission")
    ggsave("Distribution of Number of Diagnoses by Readmission.png")
    
# time_in_hospital distribution
    ggplot(data = trainDF, aes(x = time_in_hospital, fill = readmitted_y)) +
      geom_histogram(binwidth = 1, color = "black", position = "dodge") +
      labs(title = "Distribution of Time in Hospital (days) by Readmission", x = "Time in Hospital in Days", y = "Count", fill = "Readmission")
    ggsave("Distribution of Time in Hospital (days) by Readmission.png")
  
######################## FEATURE ENGINEERING ####################################

    #renaming to overwrite the 10% of train data previously ran
    idx_train_fe <- trainDF
    #This was trained only on 10% of the dataset to ensure that it was working and now its being added to all of the train data
    # Partitioning 10% train set --> for feature engineering
         #train_split <- round(nrow(trainDF) %*% .9) 
          #set.seed(123)
           #idx      <- sample(1:nrow(trainDF), train_split)
          #idx_train <- trainDF[idx, ]
        #idx_train_fe  <- trainDF[-idx, ]
    
skim(idx_train_fe)
    
table(idx_train_fe$age)   

## Group diagnoses based on different categories

#Creating bins for diagnoses 1
unique(idx_train_fe$diag_1_desc)

  #Diagnosis 1
  #Grouping based on key words on the original column to a new column to be able to categorize
idx_train_fe$diag1_cat <- idx_train_fe %>%
  mutate(diag1_cat = case_when(
    grepl("cerebral|intracerebral|dyspareunia|hypertrophic|anaphylactic|reticulosarcoma", tolower(diag_1_desc)) ~ "Cerebral", 
    grepl("pneumonia|bronchitis|respiratory|pulmonale|pulmonary|asthma|laryngopharyngitis|pneumonitis|nasal|nausea|pneumothorax", 
          tolower(diag_1_desc)) ~ "Respiratory",
    grepl("neoplasms|neoplasm|cancer|congenital|radiotherapy", tolower(diag_1_desc)) ~ "Neoplasm",
    grepl("circulatory|hypotension|hyperosmolality|hematemesis|blood|embolism|septicemia|anemia|pancreatitis|hemorrhoids|achlorhydria|hemopericardium|extradural|hemorrhage", 
          tolower(diag_1_desc)) ~ "Circulatory/Blood",
    grepl("arthropathy|osteoporosis|rheumatism|osteoarthrosis|fracture|femur|orthopedic|osteomyelitis|spinal|cervical|spinal|myositis|joint|muscle|neck|shoulder|mitral valve|hernia", 
          tolower(diag_1_desc)) ~ "Bones and Musculoskeletal Disorders",
    grepl("kidney|liver|budd-chiari", tolower(diag_1_desc)) ~ "Kidney/ Liver",
    grepl("pressure ulcer|skin|cellulitis|adenovirus", tolower(diag_1_desc)) ~ "Skin",
    grepl("urinary|bladder|gallbladder|cystitis|renal|cholecystitis|glomerulonephritis|anal|uninodular|glomerulonephritis|polyp|diverticulum|extracorporeal", 
          tolower(diag_1_desc)) ~ "Genitourinary",
    grepl("gastritis|pericarditis|abdominal|intestine|gastric|appendicitis|pericarditis|digestive|intestines|peptic|gastroenteritis|duodenal ulcer|ulcerative|intussusception|cardiospasm", 
          tolower(diag_1_desc)) ~ "Esophagous and Gastrointestinal",
    grepl("consciousness|anxiety|amnestic|breathing exercises|schizophrenia|bipolar", tolower(diag_1_desc)) ~ "Mental Conditions",
    grepl("heart|atrioventricular|infarction|coronary|aorta|tachycardia|cardiac|cerebrovascular|artery|eosinophilic|angina|artery|hypertension", 
          tolower(diag_1_desc)) ~ "Heart",
    grepl("diabetes|thyrotoxic", tolower(diag_1_desc)) ~ "Diabetes Specified and Unspecified",
    grepl("alzheimer's|peripheral|nervous|arthritis|epilepsy|immunodeficiency|neutropenia|esotropia|involuntary|ménière's", 
          tolower(diag_1_desc)) ~ "Neurological",
    grepl("poisoning|drug|alcohol", tolower(diag_1_desc)) ~ "Substances and Poisoning",
    grepl("postoperative", tolower(diag_1_desc)) ~ "Postoperative",
    grepl("obesity|anaphylactic", tolower(diag_1_desc)) ~ "Obesity",
    grepl("uterus|vaginal", tolower(diag_1_desc)) ~ "Reproductive",
    TRUE ~ "Other"
  )) %>%
  select(diag1_cat, diag_1_desc)


#Changing the vectors to variable
idx_train_fe$diag1_cat <- (idx_train_fe$diag1_cat$diag1_cat)

skim(idx_train_fe)
summary(idx_train_fe)

#Understanding how many diagnosis there are in each category
table(idx_train_fe$diag1_cat)
  
idx_train_fe

#####Diagnosis 2
  #Grouping based on key words on the original column to a new column to be able to categorize
  idx_train_fe$diag2_cat <- idx_train_fe %>%
    mutate(diag2_cat = case_when(
      grepl("cerebral|intracerebral|dyspareunia|hypertrophic|anaphylactic|reticulosarcoma", tolower(diag_2_desc)) ~ "Cerebral", 
      grepl("pneumonia|bronchitis|respiratory|pulmonale|pulmonary|asthma|laryngopharyngitis|pneumonitis|nasal|nausea|pneumothorax", 
            tolower(diag_2_desc)) ~ "Respiratory",
      grepl("neoplasms|neoplasm|cancer|congenital|radiotherapy", tolower(diag_2_desc)) ~ "Neoplasm",
      grepl("circulatory|hypotension|hyperosmolality|hematemesis|blood|embolism|septicemia|anemia|pancreatitis|hemorrhoids|achlorhydria|hemopericardium|extradural|hemorrhage", 
            tolower(diag_2_desc)) ~ "Circulatory/Blood",
      grepl("arthropathy|osteoporosis|rheumatism|osteoarthrosis|fracture|femur|orthopedic|osteomyelitis|spinal|cervical|spinal|myositis|joint|muscle|neck|shoulder|mitral valve|hernia", 
            tolower(diag_2_desc)) ~ "Bones and Musculoskeletal Disorders",
      grepl("kidney|liver|budd-chiari", tolower(diag_2_desc)) ~ "Kidney/ Liver",
      grepl("pressure ulcer|skin|cellulitis|adenovirus", tolower(diag_2_desc)) ~ "Skin",
      grepl("urinary|bladder|gallbladder|cystitis|renal|cholecystitis|glomerulonephritis|anal|uninodular|glomerulonephritis|polyp|diverticulum|extracorporeal", 
            tolower(diag_2_desc)) ~ "Genitourinary",
      grepl("gastritis|pericarditis|abdominal|intestine|gastric|appendicitis|pericarditis|digestive|intestines|peptic|gastroenteritis|duodenal ulcer|ulcerative|intussusception|cardiospasm", 
            tolower(diag_2_desc)) ~ "Esophagous and Gastrointestinal",
      grepl("consciousness|anxiety|amnestic|breathing exercises|schizophrenia|bipolar", tolower(diag_2_desc)) ~ "Mental Conditions",
      grepl("heart|atrioventricular|infarction|coronary|aorta|tachycardia|cardiac|cerebrovascular|artery|eosinophilic|angina|artery|hypertension", 
            tolower(diag_2_desc)) ~ "Heart",
      grepl("diabetes|thyrotoxic", tolower(diag_2_desc)) ~ "Diabetes Specified and Unspecified",
      grepl("alzheimer's|peripheral|nervous|arthritis|epilepsy|immunodeficiency|neutropenia|esotropia|involuntary|ménière's", 
            tolower(diag_2_desc)) ~ "Neurological",
      grepl("poisoning|drug|alcohol", tolower(diag_2_desc)) ~ "Substances and Poisoning",
      grepl("postoperative", tolower(diag_2_desc)) ~ "Postoperative",
      grepl("obesity|anaphylactic", tolower(diag_2_desc)) ~ "Obesity",
      grepl("uterus|vaginal", tolower(diag_2_desc)) ~ "Reproductive",
      TRUE ~ "Other"
    )) %>%
    select(diag2_cat, diag_2_desc)
  
  #Changing the vectors to variable
  idx_train_fe$diag2_cat <- (idx_train_fe$diag2_cat$diag2_cat)
  
  skim(idx_train_fe)
  summary(idx_train_fe)
  
  #Understanding how many diagnosis there are in each category
  table(idx_train_fe$diag2_cat)
  
  idx_train_fe
  

#####Diagnosis 3
  #Grouping based on key words on the original column to a new column to be able to categorize
  idx_train_fe$diag3_cat <- idx_train_fe %>%
    mutate(diag3_cat = case_when(
      grepl("cerebral|intracerebral|dyspareunia|hypertrophic|anaphylactic|reticulosarcoma", tolower(diag_3_desc)) ~ "Cerebral", 
      grepl("pneumonia|bronchitis|respiratory|pulmonale|pulmonary|asthma|laryngopharyngitis|pneumonitis|nasal|nausea|pneumothorax", 
            tolower(diag_3_desc)) ~ "Respiratory",
      grepl("neoplasms|neoplasm|cancer|congenital|radiotherapy", tolower(diag_3_desc)) ~ "Neoplasm",
      grepl("circulatory|hypotension|hyperosmolality|hematemesis|blood|embolism|septicemia|anemia|pancreatitis|hemorrhoids|achlorhydria|hemopericardium|extradural|hemorrhage", 
            tolower(diag_3_desc)) ~ "Circulatory/Blood",
      grepl("arthropathy|osteoporosis|rheumatism|osteoarthrosis|fracture|femur|orthopedic|osteomyelitis|spinal|cervical|spinal|myositis|joint|muscle|neck|shoulder|mitral valve|hernia", 
            tolower(diag_3_desc)) ~ "Bones and Musculoskeletal Disorders",
      grepl("kidney|liver|budd-chiari", tolower(diag_3_desc)) ~ "Kidney/ Liver",
      grepl("pressure ulcer|skin|cellulitis|adenovirus", tolower(diag_3_desc)) ~ "Skin",
      grepl("urinary|bladder|gallbladder|cystitis|renal|cholecystitis|glomerulonephritis|anal|uninodular|glomerulonephritis|polyp|diverticulum|extracorporeal", 
            tolower(diag_3_desc)) ~ "Genitourinary",
      grepl("gastritis|pericarditis|abdominal|intestine|gastric|appendicitis|pericarditis|digestive|intestines|peptic|gastroenteritis|duodenal ulcer|ulcerative|intussusception|cardiospasm", 
            tolower(diag_3_desc)) ~ "Esophagous and Gastrointestinal",
      grepl("consciousness|anxiety|amnestic|breathing exercises|schizophrenia|bipolar", tolower(diag_3_desc)) ~ "Mental Conditions",
      grepl("heart|atrioventricular|infarction|coronary|aorta|tachycardia|cardiac|cerebrovascular|artery|eosinophilic|angina|artery|hypertension", 
            tolower(diag_3_desc)) ~ "Heart",
      grepl("diabetes|thyrotoxic", tolower(diag_3_desc)) ~ "Diabetes Specified and Unspecified",
      grepl("alzheimer's|peripheral|nervous|arthritis|epilepsy|immunodeficiency|neutropenia|esotropia|involuntary|ménière's", 
            tolower(diag_3_desc)) ~ "Neurological",
      grepl("poisoning|drug|alcohol", tolower(diag_3_desc)) ~ "Substances and Poisoning",
      grepl("postoperative", tolower(diag_3_desc)) ~ "Postoperative",
      grepl("obesity|anaphylactic", tolower(diag_3_desc)) ~ "Obesity",
      grepl("uterus|vaginal", tolower(diag_3_desc)) ~ "Reproductive",
      TRUE ~ "Other"
    )) %>%
    select(diag3_cat, diag_3_desc)
  
  #Changing the vectors to variable
  idx_train_fe$diag3_cat <- (idx_train_fe$diag3_cat$diag3_cat)
  
  skim(idx_train_fe)
  summary(idx_train_fe)
  
  #Understanding how many diagnosis there are in each category
  table(idx_train_fe$diag3_cat)
  
  idx_train_fe
  
## Group discharge_disposition_id based on different categories
  
  unique(idx_train_fe$discharge_disposition_id)
  
  # Options & Functions
  Sys.setlocale('LC_ALL','C')
  
  #Return to lower text and if it cant (emojis for instances) substitute for NA
  tryTolower <- function(x){
    # return NA when there is an error
    y = NA
    # tryCatch error
    try_error = tryCatch(tolower(x), error = function(e) e)
    # if not an error
    if (!inherits(try_error, 'error'))
      y = tolower(x)
    return(y)}
  
  #Corpus - collections of documents
  cleanCorpus<-function(corpus, customStopwords){
    #add content_transformer(qdapRegex::rm_url for the different packages
    #when you start working with vectors, you need to use content_transformer if its not 
    #part of the text mining package. Because tm_map only works with vectors
    corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url)) 
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, stripWhitespace)
    corpus <- tm_map(corpus, removeNumbers)
    corpus <- tm_map(corpus, content_transformer(tryTolower))
    corpus <- tm_map(corpus, removeWords, customStopwords)
    return(corpus)
  }
  
  # Create custom stop words
  #(stopwords('english') this is already pre created
  customStopwords <- c(stopwords('english'), 'to', 'with','another','units')
  
  # Make a volatile corpus (its volatile because if you are offline, you lose access)
  txtCorpus <- VCorpus(VectorSource(idx_train_fe$discharge_disposition_id))
  
  # Preprocess the corpus
  txtCorpus <- cleanCorpus(txtCorpus, customStopwords)
  
  # Make a Document Term Matrix or Term Document Matrix depending on analysis
  txtDtm  <- DocumentTermMatrix(txtCorpus)
  txtDtmM <- as.matrix(txtDtm)
  
  dim(txtDtmM)
  
  #Creating the plot
  diagFreq <- colSums(txtDtmM)
  diagFreq <- data.frame(word=names(diagFreq),
                         frequency=diagFreq, 
                         row.names = NULL)
  
  # Simple barplot; values greater than 50 
  topWords      <- subset(diagFreq, diagFreq$frequency >= 200) 
  topWords      <- topWords[order(topWords$frequency, decreasing=F),]
  
  #Getting frequency for
  diagFreq
  topWords

  # Chg to factor for ggplot
  topWords$word <- factor(topWords$word, 
                          levels=unique(as.character(topWords$word))) 
  
  ggplot(topWords, aes(x=word, y=frequency)) + 
    geom_bar(stat="identity", fill='darkred') + 
    coord_flip()+ theme_gdocs() +
    geom_text(aes(label=frequency), colour="white",hjust=1.25, size=5.0)
  #result does not bring useful results
  
  unique(idx_train_fe$discharge_disposition_id)
  
  #Creating variables for grouped categories for discharge_disposition_id
  
  discharged_to_home <- c("Discharged to home"
                      )
  hospice <- c("Hospice / home",
               "Hospice / medical facility",
               "Discharged/transferred/referred to a psychiatric hospital of a psychiatric distinct part unit of a hospital",
               "Discharged/transferred to another rehab fac including rehab units of a hospital.")
  discharged_with_assistance <- c("Discharged/transferred to another short term hospital",
                                "Discharged/transferred to home with home health service")
  transferred_to_care_facility <- c("Discharged/transferred to ICF",
                                  "Discharged/transferred to SNF",
                                  "Discharged/transferred to another  type of inpatient care institution",
                                  "Discharged/transferred to a long term care hospital.")
  
  
  # Subset the data based on the categories
  idx_train_fe$discharge_disposition_id <- ifelse(idx_train_fe$discharge_disposition_id %in% discharged_to_home, "Discharged to Home", idx_train_fe$discharge_disposition_id)
  idx_train_fe$discharge_disposition_id <- ifelse(idx_train_fe$discharge_disposition_id %in% hospice, "Hospice", idx_train_fe$discharge_disposition_id)
  idx_train_fe$discharge_disposition_id <- ifelse(idx_train_fe$discharge_disposition_id %in% discharged_with_assistance, "Discharged with Assistance", idx_train_fe$discharge_disposition_id)
  idx_train_fe$discharge_disposition_id <- ifelse(idx_train_fe$discharge_disposition_id %in% transferred_to_care_facility, "Transferered to Care Facility", idx_train_fe$discharge_disposition_id)
  
    
  unique(idx_train_fe$discharge_disposition_id)
  
  idx_train_fe
## Feature Engineering for further ratios
  
  #Number of lab procedures by the time in the hospital
  idx_train_fe$lab_procedures_by_time <- round(idx_train_fe$num_lab_procedures/idx_train_fe$time_in_hospital)
  
  #Number of lab procedures by the time in the hospital
  idx_train_fe$medications_by_lab_procedures <- round(idx_train_fe$num_medications/idx_train_fe$num_lab_procedures)
  
  str(idx_train_fe)

##Aggregate function
  
  # Convert some columns to factors
  idx_train_fe$race <- as.factor(idx_train_fe$race)
  idx_train_fe$admission_source_id <- as.factor(idx_train_fe$admission_source_id)
  idx_train_fe$admission_type_id <- as.factor(idx_train_fe$admission_type_id)
  idx_train_fe$discharge_disposition_id <- as.factor(idx_train_fe$discharge_disposition_id)
  idx_train_fe$diag1_cat <- as.factor(idx_train_fe$diag1_cat)
  idx_train_fe$diag2_cat <- as.factor(idx_train_fe$diag2_cat)
  idx_train_fe$diag3_cat <- as.factor(idx_train_fe$diag3_cat)
  

  # Aggregate data by discharge disposition
  selected_discharge <- c("Discharged to Home", "Hospice", "Discharged with Assistance", "Transferered to Care Facility")
  
  mean_labprocedures_discharge <- aggregate(num_lab_procedures ~ discharge_disposition_id, 
                        data = subset(idx_train_fe, discharge_disposition_id %in% selected_discharge), 
                        FUN = mean)
  
  # Merge the aggregated data with the original data set
  idx_train_fe <- merge(idx_train_fe, mean_labprocedures_discharge, by = "discharge_disposition_id")
  # Rename the new column
  names(idx_train_fe)[names(idx_train_fe) == "num_lab_procedures"] <- "mean_lab_procedures_discharge"
  #Finding the values on the dataset
  unique(idx_train_fe$num_lab_procedures.y)
  
##Group By Function
  
  #Mean of number of lab procedures and admissions for each race group
  means_by_race <- idx_train_fe %>% 
    group_by(race) %>% 
    summarize(mean_number_diagnoses_race = mean(number_diagnoses), 
              mean_medications_race = mean(num_medications))

  # Join the means to the main data set
  idx_train_fe <- left_join(idx_train_fe, means_by_race, by = "race")
  
##Subset Function 
  
  #Subseting based on admission_type_id and number_diagnoses
  subset_idx_train_fe <- subset(idx_train_fe, select = c(admission_type_id, number_diagnoses))
  #Getting mean number_diagnoses by admission_type_id
  number_diagnoses_mean_by_admission_type <- 
    with(subset_idx_train_fe, tapply(number_diagnoses, admission_type_id, mean, na.rm = TRUE)
         [as.character(admission_type_id)])
  #Adding new column to idx_train_fe
  idx_train_fe$number_diagnoses_mean_by_admission_type <- 
    number_diagnoses_mean_by_admission_type[as.character(idx_train_fe$admission_type_id)]
  
  
#Checking the data
  skim(idx_train_fe)
  
###########################################################################################
########################### END OF FEATURE ENGINEERING ####################################
###########################################################################################
########################### APPLY FEATURES TO TEST DATA ####################################
###########################################################################################
  
  
#Apply idx_train_fe features from train dataset to test dataset
  
  table(testDF$age)   
  
  ## Group diagnoses based on different categories
  
  #Creating bins for diagnoses 1
  unique(testDF$diag_1_desc)
  
  #Diagnosis 1
  #Grouping based on key words on the original column to a new column to be able to categorize
  testDF$diag1_cat <- testDF %>%
    mutate(diag1_cat = case_when(
      grepl("cerebral|intracerebral|dyspareunia|hypertrophic|anaphylactic|reticulosarcoma", tolower(diag_1_desc)) ~ "Cerebral", 
      grepl("pneumonia|bronchitis|respiratory|pulmonale|pulmonary|asthma|laryngopharyngitis|pneumonitis|nasal|nausea|pneumothorax", 
            tolower(diag_1_desc)) ~ "Respiratory",
      grepl("neoplasms|neoplasm|cancer|congenital|radiotherapy", tolower(diag_1_desc)) ~ "Neoplasm",
      grepl("circulatory|hypotension|hyperosmolality|hematemesis|blood|embolism|septicemia|anemia|pancreatitis|hemorrhoids|achlorhydria|hemopericardium|extradural|hemorrhage", 
            tolower(diag_1_desc)) ~ "Circulatory/Blood",
      grepl("arthropathy|osteoporosis|rheumatism|osteoarthrosis|fracture|femur|orthopedic|osteomyelitis|spinal|cervical|spinal|myositis|joint|muscle|neck|shoulder|mitral valve|hernia", 
            tolower(diag_1_desc)) ~ "Bones and Musculoskeletal Disorders",
      grepl("kidney|liver|budd-chiari", tolower(diag_1_desc)) ~ "Kidney/ Liver",
      grepl("pressure ulcer|skin|cellulitis|adenovirus", tolower(diag_1_desc)) ~ "Skin",
      grepl("urinary|bladder|gallbladder|cystitis|renal|cholecystitis|glomerulonephritis|anal|uninodular|glomerulonephritis|polyp|diverticulum|extracorporeal", 
            tolower(diag_1_desc)) ~ "Genitourinary",
      grepl("gastritis|pericarditis|abdominal|intestine|gastric|appendicitis|pericarditis|digestive|intestines|peptic|gastroenteritis|duodenal ulcer|ulcerative|intussusception|cardiospasm", 
            tolower(diag_1_desc)) ~ "Esophagous and Gastrointestinal",
      grepl("consciousness|anxiety|amnestic|breathing exercises|schizophrenia|bipolar", tolower(diag_1_desc)) ~ "Mental Conditions",
      grepl("heart|atrioventricular|infarction|coronary|aorta|tachycardia|cardiac|cerebrovascular|artery|eosinophilic|angina|artery|hypertension", 
            tolower(diag_1_desc)) ~ "Heart",
      grepl("diabetes|thyrotoxic", tolower(diag_1_desc)) ~ "Diabetes Specified and Unspecified",
      grepl("alzheimer's|peripheral|nervous|arthritis|epilepsy|immunodeficiency|neutropenia|esotropia|involuntary|ménière's", 
            tolower(diag_1_desc)) ~ "Neurological",
      grepl("poisoning|drug|alcohol", tolower(diag_1_desc)) ~ "Substances and Poisoning",
      grepl("postoperative", tolower(diag_1_desc)) ~ "Postoperative",
      grepl("obesity|anaphylactic", tolower(diag_1_desc)) ~ "Obesity",
      grepl("uterus|vaginal", tolower(diag_1_desc)) ~ "Reproductive",
      TRUE ~ "Other"
    )) %>%
    select(diag1_cat, diag_1_desc)
  
  
  #Changing the vectors to variable
  testDF$diag1_cat <- (testDF$diag1_cat$diag1_cat)
  
  skim(testDF)
  summary(testDF)
  
  #Understanding how many diagnosis there are in each category
  table(testDF$diag1_cat)
  
  testDF
  
  #####Diagnosis 2
  #Grouping based on key words on the original column to a new column to be able to categorize
  testDF$diag2_cat <- testDF %>%
    mutate(diag2_cat = case_when(
      grepl("cerebral|intracerebral|dyspareunia|hypertrophic|anaphylactic|reticulosarcoma", tolower(diag_2_desc)) ~ "Cerebral", 
      grepl("pneumonia|bronchitis|respiratory|pulmonale|pulmonary|asthma|laryngopharyngitis|pneumonitis|nasal|nausea|pneumothorax", 
            tolower(diag_2_desc)) ~ "Respiratory",
      grepl("neoplasms|neoplasm|cancer|congenital|radiotherapy", tolower(diag_2_desc)) ~ "Neoplasm",
      grepl("circulatory|hypotension|hyperosmolality|hematemesis|blood|embolism|septicemia|anemia|pancreatitis|hemorrhoids|achlorhydria|hemopericardium|extradural|hemorrhage", 
            tolower(diag_2_desc)) ~ "Circulatory/Blood",
      grepl("arthropathy|osteoporosis|rheumatism|osteoarthrosis|fracture|femur|orthopedic|osteomyelitis|spinal|cervical|spinal|myositis|joint|muscle|neck|shoulder|mitral valve|hernia", 
            tolower(diag_2_desc)) ~ "Bones and Musculoskeletal Disorders",
      grepl("kidney|liver|budd-chiari", tolower(diag_2_desc)) ~ "Kidney/ Liver",
      grepl("pressure ulcer|skin|cellulitis|adenovirus", tolower(diag_2_desc)) ~ "Skin",
      grepl("urinary|bladder|gallbladder|cystitis|renal|cholecystitis|glomerulonephritis|anal|uninodular|glomerulonephritis|polyp|diverticulum|extracorporeal", 
            tolower(diag_2_desc)) ~ "Genitourinary",
      grepl("gastritis|pericarditis|abdominal|intestine|gastric|appendicitis|pericarditis|digestive|intestines|peptic|gastroenteritis|duodenal ulcer|ulcerative|intussusception|cardiospasm", 
            tolower(diag_2_desc)) ~ "Esophagous and Gastrointestinal",
      grepl("consciousness|anxiety|amnestic|breathing exercises|schizophrenia|bipolar", tolower(diag_2_desc)) ~ "Mental Conditions",
      grepl("heart|atrioventricular|infarction|coronary|aorta|tachycardia|cardiac|cerebrovascular|artery|eosinophilic|angina|artery|hypertension", 
            tolower(diag_2_desc)) ~ "Heart",
      grepl("diabetes|thyrotoxic", tolower(diag_2_desc)) ~ "Diabetes Specified and Unspecified",
      grepl("alzheimer's|peripheral|nervous|arthritis|epilepsy|immunodeficiency|neutropenia|esotropia|involuntary|ménière's", 
            tolower(diag_2_desc)) ~ "Neurological",
      grepl("poisoning|drug|alcohol", tolower(diag_2_desc)) ~ "Substances and Poisoning",
      grepl("postoperative", tolower(diag_2_desc)) ~ "Postoperative",
      grepl("obesity|anaphylactic", tolower(diag_2_desc)) ~ "Obesity",
      grepl("uterus|vaginal", tolower(diag_2_desc)) ~ "Reproductive",
      TRUE ~ "Other"
    )) %>%
    select(diag2_cat, diag_2_desc)
  
  #Changing the vectors to variable
  testDF$diag2_cat <- (testDF$diag2_cat$diag2_cat)
  
  skim(testDF)
  summary(testDF)
  
  #Understanding how many diagnosis there are in each category
  table(testDF$diag2_cat)
  
  testDF
  
  
  #####Diagnosis 3
  #Grouping based on key words on the original column to a new column to be able to categorize
  testDF$diag3_cat <- testDF %>%
    mutate(diag3_cat = case_when(
      grepl("cerebral|intracerebral|dyspareunia|hypertrophic|anaphylactic|reticulosarcoma", tolower(diag_3_desc)) ~ "Cerebral", 
      grepl("pneumonia|bronchitis|respiratory|pulmonale|pulmonary|asthma|laryngopharyngitis|pneumonitis|nasal|nausea|pneumothorax", 
            tolower(diag_3_desc)) ~ "Respiratory",
      grepl("neoplasms|neoplasm|cancer|congenital|radiotherapy", tolower(diag_3_desc)) ~ "Neoplasm",
      grepl("circulatory|hypotension|hyperosmolality|hematemesis|blood|embolism|septicemia|anemia|pancreatitis|hemorrhoids|achlorhydria|hemopericardium|extradural|hemorrhage", 
            tolower(diag_3_desc)) ~ "Circulatory/Blood",
      grepl("arthropathy|osteoporosis|rheumatism|osteoarthrosis|fracture|femur|orthopedic|osteomyelitis|spinal|cervical|spinal|myositis|joint|muscle|neck|shoulder|mitral valve|hernia", 
            tolower(diag_3_desc)) ~ "Bones and Musculoskeletal Disorders",
      grepl("kidney|liver|budd-chiari", tolower(diag_3_desc)) ~ "Kidney/ Liver",
      grepl("pressure ulcer|skin|cellulitis|adenovirus", tolower(diag_3_desc)) ~ "Skin",
      grepl("urinary|bladder|gallbladder|cystitis|renal|cholecystitis|glomerulonephritis|anal|uninodular|glomerulonephritis|polyp|diverticulum|extracorporeal", 
            tolower(diag_3_desc)) ~ "Genitourinary",
      grepl("gastritis|pericarditis|abdominal|intestine|gastric|appendicitis|pericarditis|digestive|intestines|peptic|gastroenteritis|duodenal ulcer|ulcerative|intussusception|cardiospasm", 
            tolower(diag_3_desc)) ~ "Esophagous and Gastrointestinal",
      grepl("consciousness|anxiety|amnestic|breathing exercises|schizophrenia|bipolar", tolower(diag_3_desc)) ~ "Mental Conditions",
      grepl("heart|atrioventricular|infarction|coronary|aorta|tachycardia|cardiac|cerebrovascular|artery|eosinophilic|angina|artery|hypertension", 
            tolower(diag_3_desc)) ~ "Heart",
      grepl("diabetes|thyrotoxic", tolower(diag_3_desc)) ~ "Diabetes Specified and Unspecified",
      grepl("alzheimer's|peripheral|nervous|arthritis|epilepsy|immunodeficiency|neutropenia|esotropia|involuntary|ménière's", 
            tolower(diag_3_desc)) ~ "Neurological",
      grepl("poisoning|drug|alcohol", tolower(diag_3_desc)) ~ "Substances and Poisoning",
      grepl("postoperative", tolower(diag_3_desc)) ~ "Postoperative",
      grepl("obesity|anaphylactic", tolower(diag_3_desc)) ~ "Obesity",
      grepl("uterus|vaginal", tolower(diag_3_desc)) ~ "Reproductive",
      TRUE ~ "Other"
    )) %>%
    select(diag3_cat, diag_3_desc)
  
  #Changing the vectors to variable
  testDF$diag3_cat <- (testDF$diag3_cat$diag3_cat)
  
  skim(testDF)
  summary(testDF)
  
  #Understanding how many diagnosis there are in each category
  table(testDF$diag3_cat)
  
  testDF
  
  ## Group discharge_disposition_id based on different categories
  
  unique(testDF$discharge_disposition_id)
  
  # Options & Functions
  Sys.setlocale('LC_ALL','C')
  
  #Return to lower text and if it cant (emojis for instances) substitute for NA
  tryTolower <- function(x){
    # return NA when there is an error
    y = NA
    # tryCatch error
    try_error = tryCatch(tolower(x), error = function(e) e)
    # if not an error
    if (!inherits(try_error, 'error'))
      y = tolower(x)
    return(y)}
  
  #Corpus - collections of documents
  cleanCorpus<-function(corpus, customStopwords){
    #add content_transformer(qdapRegex::rm_url for the different packages
    #when you start working with vectors, you need to use content_transformer if its not 
    #part of the text mining package. Because tm_map only works with vectors
    corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url)) 
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, stripWhitespace)
    corpus <- tm_map(corpus, removeNumbers)
    corpus <- tm_map(corpus, content_transformer(tryTolower))
    corpus <- tm_map(corpus, removeWords, customStopwords)
    return(corpus)
  }
  
  # Create custom stop words
  #(stopwords('english') this is already pre created
  customStopwords <- c(stopwords('english'), 'to', 'with','another','units')
  
  # Make a volatile corpus (its volatile because if you are offline, you lose access)
  txtCorpus <- VCorpus(VectorSource(testDF$discharge_disposition_id))
  
  # Preprocess the corpus
  txtCorpus <- cleanCorpus(txtCorpus, customStopwords)
  
  # Make a Document Term Matrix or Term Document Matrix depending on analysis
  txtDtm  <- DocumentTermMatrix(txtCorpus)
  txtDtmM <- as.matrix(txtDtm)
  
  dim(txtDtmM)
  
  #Creating the plot
  diagFreq <- colSums(txtDtmM)
  diagFreq <- data.frame(word=names(diagFreq),
                         frequency=diagFreq, 
                         row.names = NULL)
  
  # Simple barplot; values greater than 50 
  topWords      <- subset(diagFreq, diagFreq$frequency >= 200) 
  topWords      <- topWords[order(topWords$frequency, decreasing=F),]
  
  #Getting frequency for
  diagFreq
  topWords
  
  # Chg to factor for ggplot
  topWords$word <- factor(topWords$word, 
                          levels=unique(as.character(topWords$word))) 
  
  ggplot(topWords, aes(x=word, y=frequency)) + 
    geom_bar(stat="identity", fill='darkred') + 
    coord_flip()+ theme_gdocs() +
    geom_text(aes(label=frequency), colour="white",hjust=1.25, size=5.0)
  #result does not bring useful results
  
  unique(testDF$discharge_disposition_id)
  
  #Creating variables for grouped categories for discharge_disposition_id
  
  discharged_to_home <- c("Discharged to home"
  )
  hospice <- c("Hospice / home",
               "Hospice / medical facility",
               "Discharged/transferred/referred to a psychiatric hospital of a psychiatric distinct part unit of a hospital",
               "Discharged/transferred to another rehab fac including rehab units of a hospital.")
  discharged_with_assistance <- c("Discharged/transferred to another short term hospital",
                                  "Discharged/transferred to home with home health service")
  transferred_to_care_facility <- c("Discharged/transferred to ICF",
                                    "Discharged/transferred to SNF",
                                    "Discharged/transferred to another  type of inpatient care institution",
                                    "Discharged/transferred to a long term care hospital.")
  
  
  # Subset the data based on the categories
  testDF$discharge_disposition_id <- ifelse(testDF$discharge_disposition_id %in% discharged_to_home, "Discharged to Home", testDF$discharge_disposition_id)
  testDF$discharge_disposition_id <- ifelse(testDF$discharge_disposition_id %in% hospice, "Hospice", testDF$discharge_disposition_id)
  testDF$discharge_disposition_id <- ifelse(testDF$discharge_disposition_id %in% discharged_with_assistance, "Discharged with Assistance", testDF$discharge_disposition_id)
  testDF$discharge_disposition_id <- ifelse(testDF$discharge_disposition_id %in% transferred_to_care_facility, "Transferered to Care Facility", testDF$discharge_disposition_id)
  
  
  unique(testDF$discharge_disposition_id)
  
  testDF
  ## Feature Engineering for further ratios
  
  #Number of lab procedures by the time in the hospital
  testDF$lab_procedures_by_time <- round(testDF$num_lab_procedures/testDF$time_in_hospital)
  
  #Number of lab procedures by the time in the hospital
  testDF$medications_by_lab_procedures <- round(testDF$num_medications/testDF$num_lab_procedures)
  
  str(testDF)
  
  ##Aggregate function
  
  # Convert some columns to factors
  testDF$race <- as.factor(testDF$race)
  testDF$admission_source_id <- as.factor(testDF$admission_source_id)
  testDF$admission_type_id <- as.factor(testDF$admission_type_id)
  testDF$discharge_disposition_id <- as.factor(testDF$discharge_disposition_id)
  testDF$diag1_cat <- as.factor(testDF$diag1_cat)
  testDF$diag2_cat <- as.factor(testDF$diag2_cat)
  testDF$diag3_cat <- as.factor(testDF$diag3_cat)
  
  
  # Aggregate data by discharge disposition
  selected_discharge <- c("Discharged to Home", "Hospice", "Discharged with Assistance", "Transferered to Care Facility")
  
  mean_labprocedures_discharge <- aggregate(num_lab_procedures ~ discharge_disposition_id, 
                                            data = subset(testDF, discharge_disposition_id %in% selected_discharge), 
                                            FUN = mean)
  
  # Merge the aggregated data with the original data set
  testDF <- merge(testDF, mean_labprocedures_discharge, by = "discharge_disposition_id")
  # Rename the new column
  names(testDF)[names(testDF) == "num_lab_procedures"] <- "mean_lab_procedures_discharge"
  #Finding the values on the dataset
  unique(testDF$num_lab_procedures.y)
  
  ##Group By Function
  
  #Mean of number of lab procedures and admissions for each race group
  means_by_race <- testDF %>% 
    group_by(race) %>% 
    summarize(mean_number_diagnoses_race = mean(number_diagnoses), 
              mean_medications_race = mean(num_medications))
  
  # Join the means to the main data set
  testDF <- left_join(testDF, means_by_race, by = "race")
  
  ##Subset Function 
  
  #Subseting based on admission_type_id and number_diagnoses
  subset_testDF <- subset(testDF, select = c(admission_type_id, number_diagnoses))
  #Getting mean number_diagnoses by admission_type_id
  number_diagnoses_mean_by_admission_type <- 
    with(subset_testDF, tapply(number_diagnoses, admission_type_id, mean, na.rm = TRUE)
         [as.character(admission_type_id)])
  #Adding new column to testDF
  testDF$number_diagnoses_mean_by_admission_type <- 
    number_diagnoses_mean_by_admission_type[as.character(testDF$admission_type_id)]
  
  
  #Checking the data
  skim(testDF)
  
########################### MODELLING AND PREPARING ####################################

  # See other script Z_FULL_withDataPrep...
  
  # MODIFY
  # Identify the informative and target
  names(idx_train_fe)
  targetVar       <- names(idx_train_fe)[40]
  targetVar
  
  #selecting the names of the columns that we decide on
  exclude_cols <- c("readmitted_y", "tmpID")
  all_cols <- names(idx_train_fe)
  informativeVars <- all_cols[setdiff(seq_along(all_cols), match(exclude_cols, all_cols))]
  
  informativeVars
  
  str(idx_train_fe)
  
  #### SAMPLE
  # Segment the prep data
  set.seed(1234)
  idx         <- sample(1:nrow(idx_train_fe),.1*nrow(idx_train_fe))
  prepData    <- idx_train_fe[idx,]
  nonPrepData <- idx_train_fe[-idx,]

  # Design a "C"ategorical variable plan 
  #it accept the rows
  plan <- designTreatmentsC(prepData, 
                            #what column will it be
                            informativeVars,
                            #what is success? targetVar, 1)
                            targetVar, 1)
  
  # Apply to xVars
  #prepare using the "plan" on everything thats left "nonPrepData"
  treatedX <- prepare(plan, nonPrepData)
  
########################### Logistic Regression ####################################
  
  #Logistic regression 
  
  # Fit a logistic regression mode
  #glm General linear model
  fit <- glm(readmitted_y ~., data = treatedX, family ='binomial')
  summary(fit)
  
  # Backward Variable selection to reduce chances of multi-colinearity (Model took 1 hr and 45 min to run)
       #this is building many models
        bestFit <- step(fit, direction='backward')
        ## now you have a parsimonial model
        summary(bestFit)
  
  # Compare model size
  length(coefficients(fit))
    #150 variables
  length(coefficients(bestFit))
    #61 variables
  
  #getting predictions ---
    ##testTreated created to be used for other models
  testTreated <- prepare(plan, testDF)
  patientPreds <- predict(bestFit,  testTreated, type='response')
  tail(patientPreds)
  
  #classifying
  cutoff      <- 0.5
  patientClasses <- ifelse(patientPreds>=cutoff,1,0)
  
  
  #storing the organize w/actual in result
  results <- data.frame(actual  = testDF$readmitted_y,
                        ID    = testDF$tmpID,
                        classes = patientClasses,
                        probs   = patientPreds)
  head(results)
  
  
  #computing a confusion matrix
  (confMat <- ConfusionMatrix(results$classes, results$actual))
  #Result
    #       y_pred
    #y_true     0    1
    #FALSE    1085  280
    #TRUE     565   361
  
  #computing the model accuracy
  sum(diag(confMat)) / sum(confMat)
  Accuracy(results$classes, results$actual)
    #Result: 0.6311654
  
########################### Decision Tree ####################################
  
  #Decision Tree
  
  decision_tree_fit <- train(as.factor(readmitted_y) ~., #formula based
                    data = treatedX, #data in
                     #"recursive partitioning (trees)
                     method = "rpart", 
                     #Define a range for the CP to test
                     tuneGrid = data.frame(cp = c(0.0001, 0.001,0.005, 0.01, 0.05, 0.07, 0.1, .25)), 
                     #ie don't split if there are less than 1 record left and only do a split if there are at least 2+ records
                     control = rpart.control(minsplit = 1, minbucket = 2)) 
  
  # Examine
  decision_tree_fit
  
  # Plot the CP Accuracy Relationship to adjust the tuneGrid inputs
  plot(decision_tree_fit)
  ggsave("Complexity vs Accuracy- Decision Tree.png")
  

    # Plot a pruned tree
  pdf('decision_tree_fit.pdf')
  prp(decision_tree_fit$finalModel, extra = 1)
  dev.off()

  
  # Make some predictions on the training set
  trainCaret <- predict(decision_tree_fit, treatedX)
  head(trainCaret)
  
  # Get the conf Matrix
  confusionMatrix(trainCaret, as.factor(treatedX$readmitted_y))
    #Confusion Matrix and Statistics
      #            Reference
      #Prediction  FALSE TRUE
      #FALSE       3233 1850
      #TRUE        380  701
   #Result
        #Sensitivity : 0.8948               
        #Specificity : 0.2748               
        #Pos Pred Value : 0.6360               
        #Neg Pred Value : 0.6485               
        #Prevalence : 0.5861               
        #Detection Rate : 0.5245               
        #Detection Prevalence : 0.8246               
        #Balanced Accuracy : 0.5848               
        #'Positive' Class : FALSE 
  
  # Now more consistent accuracy & fewer rules!
  testCaret <- predict(decision_tree_fit,testTreated)
  confusionMatrix(testCaret,as.factor(testTreated$readmitted_y))
    #Result for test data
      #Accuracy : 0.6434
      #Sensitivity : 0.9026               
      #Specificity : 0.2613               
      #Pos Pred Value : 0.6430               
      #Neg Pred Value : 0.6453               
      #Prevalence : 0.5958               
      #Detection Rate : 0.5378               
      #Detection Prevalence : 0.8363               
      #Balanced Accuracy : 0.5820               
      #'Positive' Class : FALSE 
  
  # As an example here is how you get probabilities from predict()
  testCaretProbs <- predict(decision_tree_fit,testTreated,type = 'prob')
  head(testCaretProbs)
      #Result
      # FALSE      TRUE
      #1 0.2998102 0.7001898
      #2 0.6410590 0.3589410
      #3 0.3734940 0.6265060
      #4 0.6410590 0.3589410
      #5 0.6410590 0.3589410
      #6 0.6410590 0.3589410
  
  #storing the organize w/actual in result
  results_decision_tree <- data.frame(actual  = testDF$readmitted_y,
                        ID    = testDF$tmpID,
                        probs   = testCaretProbs)
  head(results_decision_tree)
  
########################### Random Forest ######################################
  
  # Fit a random forest model with Ranger
  
  # Ranger is a fast implementation of random forests (Breiman 2001) or recursive partitioning, 
    #particularly suited for high dimensional data.
  random_forest <-  ranger(as.factor(readmitted_y) ~ .,
                    data  = treatedX, 
                    num.trees = 120,
                    importance = 'permutation',
                    mtry  = 1, 
                    probability = T)
  
  # Look at improved var importance
  varImpDF <- data.frame(variables = names(importance(random_forest)),
                         importance = importance(random_forest),
                         row.names = NULL)
  varImpDF <- varImpDF[order(varImpDF$importance, decreasing = T),]
  
  ggplot(varImpDF, aes(x=importance, y = reorder(variables, importance))) + 
    geom_bar(stat='identity', position = 'dodge') + 
    ggtitle('Variable Importance') + 
    theme_gdocs()
  #ggsave("Random Forest 1.png")
  
  
  # Confusion Matrix
  trainClass <- predict(random_forest, treatedX)
  # In ranger objects, the predictions are within a list and need to be declared
  head(trainClass$predictions)
  
  # Using the prediction probability list element, classify with 0.50 cutoff 
  classOutcome <- ifelse(trainClass$predictions[,2]>=0.5,TRUE,FALSE)
  confusionMatrix(as.factor(classOutcome), 
                  as.factor(treatedX$readmitted_y))
      #First try with test: Accuracy 0.5979921
  
  
  #Random Forest subseting
       varImpDF_selected <- subset(varImpDF, importance > 0.00015)
      # Fit a random forest model with Ranger
      
      # Ranger is a fast implementation of random forests (Breiman 2001) or recursive partitioning, 
      #particularly suited for high dimensional data.
      random_forest <-  ranger(as.factor(readmitted_y) ~ .,
                               data  = treatedX[, c(varImpDF_selected$variables, 'readmitted_y')], 
                               num.trees = 120,
                               importance = 'permutation',
                               mtry  = 1, 
                               probability = T)
      
      # Look at improved var importance
      varImpDF_selected <- data.frame(variables = names(importance(random_forest)),
                             importance = importance(random_forest),
                             row.names = NULL)
      varImpDF_selected <- varImpDF_selected[order(varImpDF_selected$importance, decreasing = T),]
      
      ggplot(varImpDF_selected, aes(x=importance, y = reorder(variables, importance))) + 
        geom_bar(stat='identity', position = 'dodge') + 
        ggtitle('Variable Importance') + 
        theme_gdocs()
      ggsave("Variable Importance.png", width=15, height = 15)
      
      
      # Confusion Matrix
      trainClass <- predict(random_forest, treatedX)
      # In ranger objects, the predictions are within a list and need to be declared
      head(trainClass$predictions)
      # Using the prediction probability list element, classify with 0.50 cutoff 
      classOutcome_2 <- ifelse(trainClass$predictions[,2]>=0.5,TRUE,FALSE)
      confusionMatrix(as.factor(classOutcome_2), 
                      as.factor(treatedX$readmitted_y))
      #Second try with train: Accuracy 0.7004
          #95% CI : (0.6887, 0.7118) 
      
  ### Applying to the validation test set
    #Validation for test
  readmitted_patients <- predict(random_forest, testTreated)
  
  # In ranger objects, the predictions are within a list and need to be declared
  head(readmitted_patients$predictions)
  
  #Getting the probabilities
  testcaretproRF <-readmitted_patients$predictions
  head(testcaretproRF)
  
  # Accuracy Comparison from MLmetrics
  classOutcomeTest <- ifelse(readmitted_patients$predictions[,2]>=0.5,
                             TRUE,FALSE)
  
  #Confusion Matrix
  confusionMatrix(as.factor(classOutcomeTest), 
                  as.factor(testTreated$readmitted_y))
              #Result: Accuracy : 0.6294               
      #Another way of Measuring accuracy
            Accuracy(as.factor(classOutcomeTest), 
                     as.factor(testTreated$readmitted_y))
              #First try: Accuracy 0.5979921 (all variables)
              #Second try test: Accuracy 0.6364 (feature importance >0.00015)

            
  #storing the organize w/actual in result
  results_random_forest <- data.frame(ID = testDF$tmpID,
                                      actual= testDF$readmitted_y,
                                      probs = readmitted_patients)
  head(results_random_forest)
  
  
  # Ordering the rows by the probs column
  results_random_forest <- results_random_forest[order(-results_random_forest$probs.TRUE.),]
  
  top_100_patients <- head(results_random_forest, n = 100)
  head(top_100_patients)
    #Top 100 patients only with Random Forest
  
  
  
#### Tuning with Random Forest
  #### FINAL MODEL ####
  
  ## Another method that is fast but can help you identify the optimal number of trees is the ranger method using caret.  
  #The only "gotcha" is starting each parameter with a .
  grid <- expand.grid(.mtry = c(1,2),
                      .splitrule = 'extratrees',
                      .min.node.size = c(1,2))
  fitControl <- trainControl(method = "CV",
                             number = 2,
                             verboseIter = TRUE,
                             classProbs = TRUE)
  
  # Create a factor variable
  treatedX$readmitted_y <- factor(treatedX$readmitted_y)
  
  # Get the levels of the factor variable
  levels(treatedX$readmitted_y) <- make.names(levels(treatedX$readmitted_y))
  
  fit_rf <- train(as.factor(readmitted_y) ~ ., 
              data = treatedX[,c(varImpDF_selected$variables,'readmitted_y')],
               method = 'ranger',
               tuneGrid = grid,
               trControl = fitControl,
               )
  
  fit_rf$finalModel
    #Result
      #Type:                             Classification 
      #Number of trees:                  500 
      #Sample size:                      6164 
      #Number of independent variables:  34 
      #Mtry:                             2 
      #Target node size:                 1 
      #Variable importance mode:         none 
      #Splitrule:                        extratrees 
      #Number of random splits:          1 
      #OOB prediction error:             0.2240804 % 
  
  # Make some predictions on the training set
  traincaret_rf <- predict(fit_rf, treatedX)
  head(traincaret_rf)
  
  # Get the conf Matrix
  confusionMatrix(traincaret_rf, as.factor(treatedX$readmitted_y))
    #Accuracy : 0.8957               
    #95% CI : (0.8878, 0.9032) 
  
  
  # Predict on test data
  testcaretRF_test <- predict(fit_rf,testTreated)
  head(testcaretRF_test)
  
  #Generating prob
  testcaretRF_probability <- predict(fit_rf,testTreated, type = 'prob')
  
  # Get the conf Matrix
  class_patients <- ifelse(testcaretRF_probability$`TRUE`>=0.5,TRUE,FALSE)
  
  #storing the organize w/actual in result
  results_rf_tuning <- data.frame(ID = testDF$tmpID,
                                      actual= testDF$readmitted_y,
                                      probs = testcaretRF_probability,
                                      classes = class_patients)
  head(results_rf_tuning)
  
  #Get conf matrix with probability
    (confMat_tuning_rf <- ConfusionMatrix(results_rf_tuning$classes, results_rf_tuning$actual))
  
  #computing the model accuracy
    sum(diag(confMat_tuning_rf)) / sum(confMat_tuning_rf)
    sum(diag(confMat_tuning_rf)) / sum(confMat_tuning_rf)
      #Accuracy for test: 0.6350938

    
## Creating dataframe with top 100 patients
    #Getting the list of IDs of the top 100 patients
    top100_tuning_rf <- head(results_rf_tuning, n = 100)

    # Saving a copy of the data on local working directory of probability
    write.csv(top100_tuning_rf, 'final_top100_patients_probability.csv', row.names = F)
    
    # Subset testTreated based on tmpIDs in top100_tuning_rf
    test_subset_100_patients <- testDF[testDF$tmpID %in% top100_tuning_rf$ID, ]
    names(testDF)
    test_subset_100_patients
    
    # Saving a copy of the data on local working directory for the EDA
    write.csv(test_subset_100_patients, 'final_top100_patients_tuningRF.csv', row.names = F)

  
    
################################################# EDA ###########################################################
    
####################### EDA for the idx_train_fe ######################
  
    summary(idx_train_fe)
    str(idx_train_fe)
    dim(idx_train_fe)
    sapply(idx_train_fe,class)
    
    
    #Relationship between Number Inpatient (overnight) & Readmitted
    ggplot(idx_train_fe) +
      geom_bar(aes(x = number_inpatient, fill = admission_type_id))
    ggplot(test_subset_100_patients) +
      geom_bar(aes(x = number_inpatient, fill = admission_type_id))
    
    
    #Changing the data class to be able to plot
    idx_train_fe$age <- as.numeric(idx_train_fe$age)
    # Create new age factor variable with 5-year bins
    idx_train_fe$age_group <- cut(idx_train_fe$age, breaks = seq(0, max(idx_train_fe$age), by = 5))
    ggplot(idx_train_fe, aes(x = age, fill = diag1_cat)) +
      geom_bar() +
      labs(title = "Relationship between Age and Diagnosis 1 Category", 
           x = "Age", y = "Count")
    
    
    #Gender and First Diagnosis
    ggplot(idx_train_fe, aes(x = gender, fill = diag3_cat)) +
      geom_bar() +
      labs(title = "Relationship between Age and Readmitted", 
           x = "Gender", y = "Count")
    
    
    #Getting y axis as Category for the Diagnosis 1 and property type as stacked variables
    ggplot(idx_train_fe, aes(y = diag1_cat, fill = admission_type_id)) +
      geom_bar() +
      labs(title = "Relationship between Diagnosis 1 and Admission Type", 
           x = "Count", y = "Diagnosis 1")
    
    #Getting y axis as Category for the Diagnosis 2 and property type as stacked variables
    ggplot(idx_train_fe, aes(y = diag2_cat, fill = admission_type_id)) +
      geom_bar() +
      labs(title = "Relationship between Diagnosis 2 and Admission Type", 
           x = "Count", y = "Diagnosis 2")
    
    #Getting y axis as Category for the Diagnosis 3 and property type as stacked variables
    ggplot(idx_train_fe, aes(y = diag3_cat, fill = admission_type_id)) +
      geom_bar() +
      labs(title = "Relationship between Diagnosis 3 and Admission Type", 
           x = "Count", y = "Diagnosis 3")
    
    
    #Subseting diag 1 for specific categories + most important to define
    select_diag1 <- c('Heart','Diabetes Specific and Unspecified',
                      'Respiratory','Circulatory/Blood', 'Kidney/ Liver',
                      'Respiratory','Genitourinary')
    idx_train_fe_filtered <- idx_train_fe %>%
      filter(diag1_cat %in% select_diag1)
      #With Admissions type
    ggplot(idx_train_fe_filtered, aes(y = diag1_cat, fill = admission_type_id)) +
      geom_bar() +
      labs(title = "Relationship between Diagnosis 1 and Admission Type",
           y = "Diagnosis 1 Category", x = "Count")
    
      #Number of medications and main diag1_cat
    ggplot(idx_train_fe_filtered, aes(x = num_medications, fill = diag1_cat)) +
      geom_bar() +
      labs(title = "Relationship between Diagnosis 1 Category and Number of Medication",
           y = "Count", x = "Number of Medication")
  
    #Number of dignosis and main diag1_cat
    ggplot(idx_train_fe_filtered, aes(x = number_diagnoses, fill = diag1_cat)) +
      geom_bar() +
      labs(title = "Relationship between Diagnosis 1 Category and Number of Diagnosis",
           y = "Count", x = "Number of Diagnosis")
    
    #Lab Procedures by Time in Hospital and main diag1_cat
    ggplot(idx_train_fe_filtered, aes(x = lab_procedures_by_time, fill = diag1_cat)) +
      geom_bar() +
      labs(title = "Lab Procedures by Time in Hospital by main Diagnosis 1 Category",
           y = "Count", x = "Lab Procedures by Time in Hospital")
    
    #Age and main diag1_cat
    idx_train_fe$age_group <- cut(idx_train_fe$age, breaks = seq(0, max(idx_train_fe$age), by = 5))
    ggplot(idx_train_fe_filtered, aes(x = age, fill = diag1_cat)) +
      geom_bar() +
      labs(title = "Lab Procedures by Time in Hospital by main Diagnosis 1 Category",
           y = "Count", x = "Age")
    
    #Number of dignosis and main diag1_cat
    idx_train_fe_filtered$number_diagnoses <- factor(idx_train_fe_filtered$number_diagnoses)
    ggplot(idx_train_fe_filtered, aes(x = age, fill = number_diagnoses)) +
      geom_bar() +
      labs(title = "Lab Procedures by Time in Hospital by main Diagnosis 1 Category",
           y = "Count", x = "Lab Procedures by Time in Hospital")
    
    
    #Subsetting the graph to take a better look at age and number of diagnosis
    idx_train_fe_filtered %>%
      mutate(number_diagnoses_numeric = as.numeric(as.character(number_diagnoses))) %>%
      filter(number_diagnoses_numeric > 6, age > 40, age < 80) %>%
      ggplot(aes(x = age, y = ..count.., fill = factor(number_diagnoses))) +
      geom_bar(position = "dodge") +
      labs(title = "Lab Procedures by Time in Hospital by main Diagnosis 1 Category",
           y = "Count", x = "Lab Procedures by Time in Hospital") +
      scale_fill_discrete(name = "Number of Diagnoses")
    
    # Compute the quartiles of num_medications
    quartiles <- quantile(idx_train_fe_filtered$num_medications, probs = c(0.25, 0.5, 0.75))
          # Print out the quartiles
          cat("First quartile (Q1):", round(quartiles[1], 2), "\n",
                #10
              "Median (Q2):", round(quartiles[2], 2), "\n",
                #14
              "Third quartile (Q3):", round(quartiles[3], 2), "\n")
                #19.5
          
          
   # Compute the quartiles of age
          quartiles <- quantile(idx_train_fe_filtered$age, probs = c(0.25, 0.5, 0.75))
          # Print out the quartiles
          cat("First quartile (Q1):", round(quartiles[1], 2), "\n",
              #58
              "Median (Q2):", round(quartiles[2], 2), "\n",
              #70
              "Third quartile (Q3):", round(quartiles[3], 2), "\n")
               #79
    
    #Subsetting the graph to take a better look at age and number of medications using the quantiles
    idx_train_fe_filtered %>%
      mutate(num_medications_numeric = as.numeric(as.character(num_medications))) %>%
      filter(num_medications_numeric > 10, num_medications_numeric <20, age > 58, age < 80) %>%
      ggplot(aes(x = age, y = ..count.., fill = factor(num_medications))) +
      geom_bar(position = "dodge") +
      labs(title = "Number of Medications by Age",
           y = "Count", x = "Age") +
      scale_fill_discrete(name = "Number of Medications")
    
    ### RACE
    #Subseting diag 1 for specific categories + most important to define
    select_diag1 <- c('Heart','Diabetes Specific and Unspecified',
                      'Respiratory','Circulatory/Blood', 'Kidney/ Liver',
                      'Respiratory','Genitourinary')
    idx_train_fe_filtered <- idx_train_fe %>%
      filter(diag1_cat %in% select_diag1)
    #With Race
    ggplot(idx_train_fe_filtered, aes(y = diag1_cat, fill = race)) +
      geom_bar() +
      labs(title = "Relationship between Diagnosis 1 and Race",
           y = "Diagnosis 1 Category", x = "Count")
    
    
############## EDA Combining General Data with top 100 patients ########################
    
    cols_only_in_idx_train_fe <- setdiff(names(idx_train_fe), names(test_subset_100_patients))
    cols_only_in_idx_train_fe
    
    # Combine the two data frames for graphs
    df_combined <- rbind(transform(idx_train_fe, group = "Training Set"), 
                         transform(test_subset_100_patients, group = "Top 100 Patients"))
    
    #Number of medications
    ggplot(df_combined, aes(y = group, x = num_medications, fill = group)) +
      geom_boxplot(color = 'black',fill = "lightblue", outlier.shape = 1) +
      stat_summary(fun = mean, geom = "point", shape = 21, size = 3, fill = "white", color = "blue") +
      geom_text(data = df_combined %>% group_by(group) %>% summarize(mean_num_medications = mean(num_medications)),
                aes(label = paste0("Mean: ", round(mean_num_medications, 1)), y = group, x = mean_num_medications+1),
                hjust = .015, size = 3.5, color = "blue") +
      labs(title = "Medications Distribution by Group",
           x = "Number of Medications",
           y = "Group") +
      theme_linedraw()
    
    #Number of diagnosis
    ggplot(df_combined, aes(y = group, x = number_diagnoses, fill = group)) +
      geom_boxplot(color = 'black',fill = "lightblue", outlier.shape = 1) +
      stat_summary(fun = mean, geom = "point", shape = 21, size = 3, fill = "white", color = "blue") +
      geom_text(data = df_combined %>% group_by(group) %>% summarize(mean_num_diag = mean(number_diagnoses)),
                aes(label = paste0("Mean: ", round(mean_num_diag, 1)), y = group, x = mean_num_diag+1),
                hjust =.5, size = 3.5, color = "blue") +
      labs(title = "Number of Diagnosis Distribution by Group",
           x = "Number of Diagnosis",
           y = "Group") +
      theme_linedraw()
    
  #Diagnosis 1 category
  ggplot(df_combined, aes(x = group, fill = diag1_cat)) +
      geom_bar(position = "fill") +
      labs(title = "Diagnosis Category 1 Distribution",
           y = "Proportion", x = "Group")
  
  #Diagnosis 2 category
  ggplot(df_combined, aes(x = group, fill = diag2_cat)) +
    geom_bar(position = "fill") +
    labs(title = "Diagnosis Category 2 Distribution",
         y = "Proportion", x = "Group")
  
  #Diagnosis 3 category
  ggplot(df_combined, aes(x = group, fill = diag3_cat)) +
    geom_bar(position = "fill") +
    labs(title = "Diagnosis Category 3 Distribution",
         y = "Proportion", x = "Group")
  
  #Race Distribution
  ggplot(df_combined, aes(x = group, fill = race)) +
    geom_bar(position = "fill") +
    labs(title = "Race Distribution",
         y = "Proportion", x = "Group")
  
  # Gender Distribution by Group
  df_combined$gender <- as.factor(df_combined$gender)
  ggplot(df_combined, aes(x = group, fill = gender)) +
    geom_bar(position = "fill") +
    labs(title = "Gender Distribution by Group",
         y = "Proportion", x = "Gender")

  #Age
    # Group the data by age and group, and calculate the number of people in each group
    df_combined_counts <- df_combined %>%
      group_by(age, group) %>%
      summarize(count = n())
    # Calculate the total number of people in each group
    df_combined_totals <- df_combined_counts %>%
      group_by(group) %>%
      summarize(total = sum(count))
    # Calculate the percentage of people in each group corresponding to each age
    df_combined_prop <- df_combined_counts %>%
      left_join(df_combined_totals, by = "group") %>%
      mutate(prop = count / total)
    # Create the line graph
    ggplot(df_combined_prop, aes(x = age, y = prop, color = group)) +
      geom_line() +
      scale_y_continuous(labels = scales::percent) +
      labs(title = "Age Distribution by Group", y = "Percentage of Group", x = "Age")
  
#Diagnosis 1
    top5_diag1 <- df_combined %>%
      count(group, diag1_cat) %>%
      arrange(group, desc(n)) %>%
      group_by(group) %>%
      top_n(4, n) %>%
      mutate(prop = n / sum(n)) %>%
      arrange(group, desc(n))
    # Create the bar graph
    ggplot(top5_diag1, aes(x = diag1_cat, y = prop, fill = group)) +
      geom_col(position = 'dodge') +
      labs (y = "Proportion", fill = "") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
      ggtitle("Top 6 Diagnosis Categories (Diagnosis 1 Category )") +
      theme(plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))  
    
    #Diagnosis 2
    top5_diag2 <- df_combined %>%
      count(group, diag2_cat) %>%
      arrange(group, desc(n)) %>%
      group_by(group) %>%
      top_n(4, n) %>%
      mutate(prop = n / sum(n)) %>%
      arrange(group, desc(n))
    # Create the bar graph
    ggplot(top5_diag2, aes(x = diag2_cat, y = prop, fill = group)) +
      geom_col(position = 'dodge') +
      labs (y = "Proportion", fill = "") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
      ggtitle("Top 5 Diagnosis Categories (Diagnosis 2 Category )") +
      theme(plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))  
    
    #Diagnosis 3
    top5_diag3 <- df_combined %>%
      count(group, diag3_cat) %>%
      arrange(group, desc(n)) %>%
      group_by(group) %>%
      top_n(4, n) %>%
      mutate(prop = n / sum(n)) %>%
      arrange(group, desc(n))
    # Create the bar graph
    ggplot(top5_diag3, aes(x = diag3_cat, y = prop, fill = group)) +
      geom_col(position = 'dodge') +
      labs (y = "Proportion", fill = "") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
      ggtitle("Top Diagnosis Categories (Diagnosis 3 Category )") +
      theme(plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
  
    
    #Admission source
    admission_source <- df_combined %>%
      count(group, admission_source_id) %>%
      arrange(group, desc(n)) %>%
      group_by(group) %>%
      top_n(4, n) %>%
      mutate(prop = n / sum(n)) %>%
      arrange(group, desc(n))
    # Create the bar graph
    ggplot(admission_source, aes(x = admission_source_id, y = prop, fill = group)) +
      geom_col(position = 'dodge') +
      labs (y = "Proportion", fill = "") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
      ggtitle("Admission Source") +
      theme(plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
    
    #Admission Type
    admission_type <- df_combined %>%
      count(group, admission_type_id) %>%
      arrange(group, desc(n)) %>%
      group_by(group) %>%
      top_n(4, n) %>%
      mutate(prop = n / sum(n)) %>%
      arrange(group, desc(n))
    # Create the bar graph
    ggplot(admission_type, aes(x = admission_type_id, y = prop, fill = group)) +
      geom_col(position = 'dodge') +
      labs (y = "Proportion", fill = "") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
      ggtitle("Admission Type") +
      theme(plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
    
    #Race
    race_test <- df_combined %>%
      count(group, race) %>%
      arrange(group, desc(n)) %>%
      group_by(group) %>%
      mutate(prop = n / sum(n)) %>%
      arrange(group, desc(n))
    # Create the bar graph
    ggplot(race_test, aes(x = race, y = prop, fill = group)) +
      geom_col(position = 'dodge') +
      labs (y = "Proportion", fill = "") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
      ggtitle("Race") +
      theme(plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

  
    
#End

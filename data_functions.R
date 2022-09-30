library(readxl)
library(dplyr)

allPatients <- read_excel("data.xlsx", sheet = "All")


# Functions ---------------------------------------------------------------


dataFunction <- function(data){

countData <- data %>% 
  mutate(A = smi_count - (smi_cpa_combined + mental_health_patients_smi),
         B = cpa_count - smi_cpa_combined,
         C = mental_health_patients - mental_health_patients_smi,
         `A&B` = smi_cpa_combined,
         `A&C` = mental_health_patients_smi,
         AllSMI = A+`A&B`+`A&C`
  ) %>% 
  arrange(PCN) %>% 
  mutate(Group = PCN)

return(countData)

}


totalDataFunction <- function(data){
  
  countData <- data %>% 
    summarise_if(is.numeric, sum, na.rm = TRUE) %>% 
    mutate(CCG = "Total",
           PCN = "Total") %>% 
    arrange(PCN) %>% 
    mutate(Group = PCN)
    
  return(countData)
  
}


# All Patients (regardless of activity) -------------------------------------------

allPatientsData <- dataFunction(allPatients)

allPatientsTotal <- totalDataFunction(allPatientsData)
  


# Rename and group CCG data -----------------------------------------------


# Rename CCG, PCN and Total to Group to make it generic for a later function to read

CCG_Function <- function(data){
  
  CCGData <- data %>% 
    select(-PCN, -Group) %>% 
    group_by(CCG) %>% 
    arrange(CCG) %>% 
    summarise_all(list(sum)) %>% 
    select(Group = CCG,
           everything())
  
  return(CCGData)

  }

allPatientsDataCCG <- CCG_Function(allPatientsData)

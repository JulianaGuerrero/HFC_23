########################################################################################################
#                                                                                                      #
#             HIGH-FREQUENCY CHECKS AND PHONE TRACKING CODE -- LOGIC HIGH-FREQUENCY CHECKS             #
#                                                                                                      #
########################################################################################################

  ## PURPOSE      Create a logic high-frequency check sheet in the HFC dashboard.
  ## AUTHOR      Juliana Guerrero (adapted from Adrien Ciret & Marc-Andrea Fiorina)

## LAST UPDATE  October 5, 2023

########################################################################################################

# 1. Import Data ----


########################################################################################################

# 2. Create Logic Datasets ----

logic_variables <- hfc_data %>% select(
    
    ID_05,ID_05_enter, SubmissionDate, starttime, endtime ,
    startdate, enddate, enumerator_name,
    
   ends_with("_dur")
   
  ) %>% names()
  
  

### Check for Duplicate Surveys

duplicate_check <- hfc_data %>%
  
  group_by(ID_05) %>%
  
  mutate(
    
    counter = n(),
    
    dup_issue = case_when(
      
      counter > 1 ~ 1,
      
      TRUE        ~ 0
      
    )
    
  ) %>%
  
  ungroup() %>%
  
  filter(dup_issue == 1) %>%
  
  mutate(issue = "Multiple surveys with same household id") %>%
  
  select(logic_variables,issue)

### Check Start/End/Submission Date Discrepancies

start_end_check <- hfc_data %>%
  
  filter(startdate != enddate) %>%
  
  mutate(issue = "End of survey date is different from start of survey date") %>%
  
  select(logic_variables,issue,negative_mods) # calculate this


end_submission_check <- hfc_data %>%
  
  filter(enddate != submissiondate) %>%
  
  mutate(issue = "Submission date is different from end of survey date") %>%
  
  select(logic_variables,issue,negative_mods) # 




### Check for Modules with Negative Durations (i.e. end_mod was recorded before start_mod)

negative_length_check <- hfc_data %>%
  
  filter(negative_durs > 0) %>%
  
  mutate(issue = "This survey has one or more negative/no durations") %>%
  
  select(logic_variables,issue,negative_mods) #,negative_mods

### Check for High Survey Length

  high_length_check <- hfc_data %>%
    
    group_by(ID_05) %>%
    
    mutate(
      
      duration = sum(survey_dur, na.rm = TRUE),
      
      n_subs   = row_number()
      
    ) %>%
    
    filter(n_subs == max(n_subs, na.rm = TRUE)) %>%
    
    ungroup() %>%
    
    select(-n_subs) %>%
    
    filter(survey_dur > 180) %>%
    
    mutate(issue = "Survey total duration is greater than 3 hours") %>%
    
    select(logic_variables,issue,negative_mods) #,negative_mods
  
  ### Check for low Survey Length
  
  low_length_check <- hfc_data %>%
    
    group_by(ID_05) %>%
    
    mutate(
      
      duration = sum(survey_dur, na.rm = TRUE),
      
      n_subs   = row_number()
      
    ) %>%
    
    filter(n_subs == max(n_subs, na.rm = TRUE)) %>%
    
    ungroup() %>%
    
    select(-n_subs) %>%
    
    filter(survey_dur < 60) %>%
    
    mutate(issue = "Survey total duration is less than 1 hour") %>%
    
    select(logic_variables,issue,negative_mods)
  
  

########################################################################################################

# 3. Create Final Logic Dataset ----

logic <-  duplicate_check %>%
  
  full_join(start_end_check) %>%
  
  full_join(end_submission_check) %>%
  
  full_join(negative_length_check) %>%
    
  full_join(high_length_check) %>%  
    
  full_join(low_length_check) %>%
    
  arrange(SubmissionDate, ID_05) %>% filter(!is.na(issue))

########################################################################################################

# 4. Export Data ----

hfc_sheet %>%
  
  sheet_write(data = logic, sheet = "logic_data")

  1
  
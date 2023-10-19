########################################################################################################
#                                                                                                      #
#                     HIGH-FREQUENCY CHECKS  -- ENUMERATOR HIGH-FREQUENCY CHECKS                       #
#                                                                                                      #
########################################################################################################

  ## PURPOSE      Create an enumerator high-frequency check sheet in the HFC dashboard.



## AUTHOR      Juliana Guerrero (adapted from Adrien Ciret & Marc-Andrea Fiorina)

## LAST UPDATE  October 5, 2023



########################################################################################################

  # 1. Import Data ----
#}
  ########################################################################################################

  # 2. Create Enumerator Datasets ----
  
    
    sample_enum <- hfc_constr %>%
      
      filter(!is.na(ID_03) ) %>%
      
      group_by(ID_03) %>%
      
      summarize(total = n()) %>%
      
      ungroup() %>%
      
      select(ID_03, total) %>%
      
      distinct() %>%
      
      right_join(hfc_constr, by = c("ID_03")) 
    
 
  
  sample_enum <- sample_enum %>%
    
    filter(!is.na(ID_03) ) %>%
    
    group_by(ID_05) %>%
    
    mutate(
      
      n_subs = row_number()
      
    )
  
  
  ### Enumerator Progress and Duration Dataset
  
  enum <- sample_enum %>%
    
    filter(!is.na(ID_03) ) %>%
    
    group_by(ID_05) %>%
    
    mutate(
      
      duration = sum(survey_dur, na.rm = TRUE)
      
    ) %>%
    
    filter(n_subs == max(n_subs, na.rm = TRUE)) %>%
    
    ungroup() %>%
    
    select(-n_subs) %>%
    
    group_by(ID_03) %>%
    
    mutate(n = n())


    
    enum <- enum %>%
      
      summarise(
        
        survey_num     = first(n),#, na.rm = TRUE
        
        total_num      = first(total), #, na.rm = TRUE
        
        complete_num   = sum(survey_complete, na.rm = TRUE),
        
        ave_duration   = round((mean(duration, na.rm = TRUE)), 2),
        
        across(matches("_dur$"), ~round(mean(as.numeric(.x), na.rm = TRUE), 2))
        
      ) 
  
  ### Daily Enumerator Dataset

  enum_daily <- sample_enum %>%
    
    filter(!is.na(ID_03) ) %>%
    
    group_by(ID_05) %>%
    
    mutate(
      
      duration  = sum(survey_dur, na.rm = TRUE),
      
      n_subs = row_number()
      
    ) %>%
    
    filter(n_subs == max(n_subs, na.rm = TRUE)) %>%
    
    ungroup() %>%
    
    select(-n_subs) %>%
    
    select(ID_03, enddate) %>%
    
    group_by(ID_03, enddate) %>%
    
    count() %>%
    
    ungroup() %>%
    
    arrange(enddate) %>%
    
    pivot_wider(names_from = enddate, values_from = n)
  
  # NOTE -- The below collects information on what happened during the latest day of data collection,
  
  ### Enumerator Last Day Dataset

  last_day <- sample_enum %>%
    
    filter(!is.na(ID_03) ) %>%
    
    group_by(ID_05) %>%
    
    mutate(
      
      duration = sum(survey_dur, na.rm = TRUE),
      
      n_subs = row_number()
      
    ) %>%
    
    filter(n_subs == max(n_subs, na.rm = TRUE)) %>%
    
    ungroup()   %>%
    
    select(-n_subs) %>%
    
    filter(enddate == max((enddate), na.rm = TRUE)) %>%
    
    group_by(ID_03) %>%
    
    mutate(n = n()) %>%
    
    summarize(
      
      last_date          = unique(enddate),
      
      ld_survey_num      = first(n),#, na.rm = TRUE
      
      ld_ave_duration    = round((mean(duration, na.rm = TRUE)) / 60, 2),
      
      ld_complete        = sum(survey_complete, na.rm = TRUE)
      
    )
  
 
  ### Enumerator Consent Dataset
  

  consent <- sample_enum %>%
    
    filter(!is.na(ID_03) ) %>%
    
    group_by(ID_03) %>%
    
    summarize(
      
    consent_secured   = sum(as.numeric(consent), na.rm = TRUE))
      
  
  
  ### Enumerator Modules  Dataset
  
  (duration_to_check <- sample_enum %>%
    
      ungroup() %>%
      
      select(ends_with("_dur")) %>%
    
    names())
  
  enum_duration <- sample_enum %>%
    
    filter(!is.na(ID_03) ) %>%
    
    group_by(ID_03) %>%
    
    summarize(
      
          across(
            
            c(duration_to_check),
            
            ~ round(mean(.x, na.rm = TRUE), 0)
          )
        
          )
  
  ### Enumerator Don't Know / Refused to Respond Dataset
  
  # NOTE -- The below calculates the percentage of 'Don't know' (-99) and 'refused to respond' (-66) answers
  # for each enumerator.
  
  dkrtr_rates <- sample_enum %>%
    
    select(ID_03, where(is.numeric)) %>%
    
    mutate(
      
      ncols = rowSums(across(everything(), ~ !is.na(.)), na.rm = TRUE),
      
      dk    = rowSums(across(where(is.numeric), ~ . == '-99'), na.rm = TRUE),
      
      rtr   = rowSums(across(where(is.numeric), ~ . == '-66'), na.rm = TRUE),
      
      dk_rate  = round(dk  / ncols, 4),
      
      rtr_rate = round(rtr / ncols, 4)
      
    ) %>%
    
    group_by(ID_03) %>%
    
    summarize(
      
      dk_rate  = round(mean(dk_rate,  na.rm = TRUE), 4),
      
      rtr_rate = round(mean(rtr_rate, na.rm = TRUE), 4)
      
    )
  
  ### Enumerator Variable Values Dataset
  
  hfc_constr$c_41<-recode(hfc_constr$c_41,`No`="0",`Yes`="1")
  hfc_constr$d_01<-recode(hfc_constr$d_01,`No`="0",`Yes`="1")
  hfc_constr$d_61_a<-recode(hfc_constr$d_61_a,`No`="0",`Yes`="1")
  
    vars_to_check <- hfc_constr %>%
      
      select(
        ## Land module
        c_00_1old,  c_14_00,  c_23_1, c_23_2, c_23_3, 
        c_00_2, c_00 , c_00_1b,  c_00_1b1,  c_41, c_41_num, c_41_5_1,   c_41_7_1,  c_00_1a, c_00_1a1,
        
        ## Permanent crops
        d_01,  d_02_10_1,  d_02_10_2, d_02_10_3, d_02_10_4, d_02_10_5, d_02_10_6, #
        d_02_10_7, d_02_10_8, d_02_10_9, #d_02_10_10, d_02_10_11,
        d_02_10_a_1,  d_02_10_a_2, d_02_10_a_3, d_02_10_a_4, d_02_10_a_5, d_02_10_a_6, 
        d_02_10_a_7, d_02_10_a_8, d_02_10_a_9, #d_02_10_a_10, d_02_10_a_11,
        ## Seasonal crops
        #perm_hq_1, perm_hq_10, perm_hq_11, perm_hq_2, perm_hq_3, perm_hq_4, perm_hq_5, perm_hq_6, perm_hq_7, perm_hq_8, perm_hq_9,
        perm_HQ_1, perm_HQ_2, perm_HQ_3, perm_HQ_4, perm_HQ_5, perm_HQ_6, perm_HQ_7,
        perm_HQ_8,perm_HQ_9,#perm_HQ_10,
        ## Livestock
        d_61_1, d_61_2, d_61_3, d_61_4, d_61_5, d_61_6, d_61_7, d_61_8, d_61_a, d_62_1, d_62_2 , d_62_3, d_62_4, d_62_5, d_62_6, d_62_7, d_62_8 , d_66_2_1_1, d_66_2_2_1, d_66_2_3_1, d_66_2_4_1, d_66_2_5_1, d_66_2_6_1, d_66_2_7_1, d_66_2_8_1, d_66_4_1_1, d_66_4_2_1, d_66_4_3_1, d_66_4_4_1, d_66_4_5_1, d_66_4_6_1, d_66_4_7_1, d_66_4_8_1, d_66_a_1, d_66_a_2, d_66_a_3, d_66_a_4, d_66_a_5, d_66_a_6, d_66_a_7, d_66_a_8,
        
        ## Module E - services
        e_05_1, e_05_2, e_05_3, e_05_4, e_05_5, e_05_6, e_05_7, e_05_8, e_05_9, e_05_10, e_05_11
        
        
      ) %>%
      
      names()
    

  enum_vars <- hfc_constr %>% 
    
    filter(!is.na(ID_03) ) %>% 
    
    mutate(
      
      across(
        
        c(vars_to_check),
        
        ~ case_when(
          
          .x == -77 | .x == -99 | .x == -66 | .x == -88 ~ NA_real_,
          
          TRUE                              ~ as.numeric(.x)
          
        )
        
      )
      
    ) %>%
    
   #rename(enumerator_name=id_03) %>% 
    
    group_by(ID_03) %>%
    
    summarize(
      
      across(
        
        c(vars_to_check),
        
        ~ round(mean(.x, na.rm = TRUE), 0)
        
      )
      
    ) #%>% mutate(#d_02_p =as.character(d_02_p),
                 
  #d_05 =as.character(d_05))
  
  ########################################################################################################
  
  # 3. Create Final Enumerator Dataset ----
  
  # NOTE -- The below brings everything together and creates proportions for the 'outcome' and consent
  # variables.
  
  enum <- enum %>%
    
    left_join(enumerators,by=c('ID_03'='id')) %>% 
    
    left_join(last_day) %>%
    
    left_join(consent) %>%
    
    left_join(enum_duration) %>%

    left_join(dkrtr_rates) %>%
    
    left_join(enum_vars) %>%
    
    left_join(enum_daily) 
  
  enum <- enum %>% relocate(enumerator_name, .after=ID_03)

  ########################################################################################################
  
  # 4. Export Data ----
  
  # NOTE -- The below is what exports the data to Google Sheets - for IE team and Survey Firm
  
  hfc_sheet %>%
    
    sheet_write(data = enum, sheet = "enum_data")
  
  # In case googlesheets4 asks me to enter '1' to continue
  
  1
  
 
  
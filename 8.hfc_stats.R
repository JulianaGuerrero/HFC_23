########################################################################################################
#                                                                                                      #
#                HIGH-FREQUENCY CHECKS AND PHONE TRACKING CODE -- DESCRIPTIVE STATISTICS               #
#                                                                                                      #
########################################################################################################

  ## PURPOSE      Create a descriptive statistics sheet in the HFC dashboard.

## AUTHOR      Juliana Guerrero (adapted from Adrien Ciret & Marc-Andrea Fiorina)

## LAST UPDATE  October 5, 2023


########################################################################################################


## 1. Import Data ----
  


########################################################################################################

  # 2. Create Descriptive Stats Dataset ----

  # Descriptive variables may differ if needed across countries
     
      desc_stat_vars <- hfc_data %>% select(
        
        # Module C - Land Market
        c_25_old_1, c_25_old_2, c_25_old_3, c_25_old_4, c_25_old_5,
        c_25_1, c_25_2, c_25_3, c_25_4, c_25_5,
        c_29_old_1, c_29_old_2, c_29_old_3, c_29_old_4, c_29_old_5,
        c_31_old_1, c_31_old_2, c_31_old_3, c_31_old_4, c_31_old_5,
        c_33_1, c_33_2, c_33_3, c_33_4, c_33_5,
        c_23_old_1, c_23_old_2, c_23_old_3, c_23_old_4, c_23_old_5,
        c_39_1, c_39_2, c_39_3, c_39_4, c_39_5,
        c_00_1b, c_00_1b1, 
        c_00_1b2_1, c_00_1b2_2, #c_00_1b2_3, 
        c_00_1b3_1, c_00_1b3_2, #c_00_1b3_3,
        c_41, c_41_num,
        c_42, c_42_num,
        c_43, c_43_num,
        c_44, c_44_num, 
        c_45, c_45_num,
        c_00_1a, c_00_1a1, c_00_1a3_1, c_00_1a5_q_1,
        #c_00_1a3_3,
        #c_00_1a3_2, 
        
        # Module D1 - Permanent crops
        d_01, 
        d_02_p_1, d_02_p_2, d_02_p_3, d_02_p_4, d_02_p_5, d_02_p_6, d_02_p_7, d_02_p_8, d_02_p_9, 
        d_02_p_10, d_02_p_11, d_02_p_12, d_02_p_13, d_02_p_14, d_02_p_15, d_02_p_16, d_02_p_17,
        d_04_06_1, d_04_06_2, d_04_06_3,
        perm_HQ_1, perm_HQ_2, perm_HQ_3,
        perm_CQ_new_1, perm_CQ_new_2, perm_CQ_new_3,
        #perm_gq_new_1, perm_gq_new_2, perm_gq_new_3,
        d_04_11_a3_1, d_04_11_a4_1, d_04_11_a5_1, d_04_11_a6_1, d_04_11_b1_1, d_04_11_b3_1, d_04_11_b4_1, 
        d_04_11_b5_1, d_04_11_b6_1,
        d_04_14_15a_1, d_04_15_1, d_04_27_1, d_04_16_1, d_04_04_01_1,#,
        perm_LQ_new_1, perm_LQ_new_2, perm_LQ_new_3,
        
        # Module D1 - Seasonal crops
        
        
        
        # Module D2 - Livestock
        
        
        
        # Module E - Products & Services
        
        
      ) %>%
        
                                            names()
      
      
      #desc_stat_vars = hfc_data %>% select(c_00_1b) %>% names()  
  
  for(i in 1:length(desc_stat_vars)) {
    
      desc_stat_var <- desc_stat_vars[i]
      
      temp_desc <- hfc_data %>%
        
      select(!!desc_stat_var)

  
      temp_desc <- temp_desc %>%
      
          mutate(
        
              across(
          
                  desc_stat_var,
          
                  ~ case_when(
            
                      (.x == -66 | .x == -99) ~ NA_real_,
            
                      TRUE                    ~ as.numeric(.x)
            
                  )
          
              )
        
          ) %>%
      
          summarize(
        
              variable = desc_stat_var,
        
              mean     = round(mean(!!sym(desc_stat_var), na.rm = TRUE), 2),
        
              sd       = round(sd(!!sym(desc_stat_var), na.rm = TRUE), 2),
        
              min      = round(min(!!sym(desc_stat_var), na.rm = TRUE), 2),
        
              max      = round(max(!!sym(desc_stat_var), na.rm = TRUE), 2),
        
              quant    = round(quantile(!!sym(desc_stat_var), c(0.05, 0.25, 0.5, 0.75, 0.95), na.rm = TRUE), 2),
        
              q        = c(5, 25, 50, 75, 95)
        
          ) %>%
      
          mutate(
        
              across(
          
                  c(min, max),
          
                  ~ case_when(
            
                      is.infinite(.x) ~ NA_real_,
            
                      TRUE ~ as.numeric(.x)
            
                  )
          
              )
        
          ) %>%
      
          pivot_wider(
        
              names_from   = q,
        
              names_prefix = "quant_",
        
              values_from  = quant
        
          ) %>%
      
          select(variable:sd, min, quant_5:quant_95, max)
    
      if(i == 1) {
        
        desc_stats <- temp_desc
        
      } else {
        
        desc_stats <- desc_stats %>%
          
          bind_rows(temp_desc)
        
      }
      
  }
  
########################################################################################################
  
  # 3. Export Data ----
  
  
  hfc_sheet %>%
    
      sheet_write(data = desc_stats, sheet = "desc_stats_data")
  
1
########################################################################################################


  
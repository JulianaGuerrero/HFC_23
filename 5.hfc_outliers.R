########################################################################################################
#                                                                                                      #
#            HIGH-FREQUENCY CHECKS AND PHONE TRACKING CODE -- OUTLIER HIGH-FREQUENCY CHECKS            #
#                                                                                                      #
########################################################################################################

  ## PURPOSE      Create an outlier high-frequency check sheet in the HFC dashboard.



## AUTHOR      Juliana Guerrero (adapted from Adrien Ciret & Marc-Andrea Fiorina)

## LAST UPDATE  October 5, 2023

########################################################################################################

## 1. Import Data ----
   

########################################################################################################

  # 2. Create Variable-Specific Outlier Dataset Function ----

outlier_function <- function(outlier_var) {
  
  temp_summary <- hfc_data %>%
    
    select(!!outlier_var) %>%
    
    mutate(
      
      across(
        
        outlier_var,
        
        ~ case_when(
          
          (.x == -66 | .x == -99 | .x ==-77 | .x ==-88) ~ NA_real_,
          
          TRUE                    ~ as.numeric(.x)
          
        )
        
      )
      
    ) %>%
    
    summarize(
      
      across(
        
        outlier_var,
        
        ~ list(mean = mean(.x, na.rm = TRUE), sd = sd(.x, na.rm = TRUE), q1 = unname(quantile(.x, 0.25, na.rm = TRUE)),
               
               q3 = unname(quantile(.x, 0.75, na.rm = TRUE)))
        
      )
      
    ) %>%
    
    pluck(1)
  
  
  temp_twenty_five        <- temp_summary$q1
  
  temp_seventy_five       <- temp_summary$q3
  
  temp_inter_quartile     <- temp_seventy_five - temp_twenty_five
  
  temp_lower_quartile     <- temp_twenty_five - 1.5 * temp_inter_quartile
  
  temp_upper_quartile     <- temp_seventy_five + 1.5 * temp_inter_quartile
  
  temp_check <- hfc_data %>%
    
    mutate(
      
      across(
        
        outlier_var,
        
        ~ case_when(
          
          (.x == -66 | .x == -99 | .x == -77 | .x == -88) ~ NA_real_,
          
          TRUE                    ~ as.numeric(.x)
          
        )
        
      )
      
    )
  
  
  
  temp_check <- temp_check %>%
    
    filter(
      
      across(
        
        outlier_var,
        
        ~ .x < temp_lower_quartile | .x > temp_upper_quartile
        
      )
      
    ) %>%
    
    mutate(
      
      issue_var              = outlier_var,
      
      mean                   = round(temp_summary$mean, 2),
      
      sd                     = round(temp_summary$sd, 2),
      
      lower_quartile  = round(temp_lower_quartile , 2),
      
      upper_quartile = round(temp_upper_quartile , 2)
      
    ) %>%
    
    rename(issue_value = !!outlier_var) %>%
    
    select(
      
      ID_05, ID_03, enumerator_name,SubmissionDate, issue_var, issue_value,
      
      mean, sd, lower_quartile, upper_quartile
      
    )
  
  return(temp_check)
  
}


########################################################################################################

# 3. Create Final Outlier Dataset ----


## Keeping relevant variables
#hfc_data = fup3
outlier_vars <- hfc_data %>%
  
  select( c_14_00, c_40, c_00, c_14_00, c_00_1b1, c_41_num, c_42_num, c_43_num, c_44_num, c_45_num, c_00_1a1, d_largest_sale_value, d_largest_sale_seas_value, d2_13_3, d2_13_2, d2_13_1 , d2_11_1, d2_11_2, d2_11_3, e_09_03, e_09_06, e_09_08, e_09_11, e_60, e_29, e_29_01, e_36
   )     %>% 
  names()
      
    
    ## Looping over each selected variable to check for outliers
    
    for(i in 1:length(outlier_vars)) {
      
      temp_outlier_check <- outlier_function(outlier_vars[i])
      
      if(i == 1) {
        
        outlier_check <- temp_outlier_check
        
      } else {
        
        outlier_check <- outlier_check %>%
          
          bind_rows(temp_outlier_check)
        
      }
      
      print(paste0("Variable ", outlier_vars[i], " done."))
      
    }
 
# Keep last week of data collection only
#outlier_check <- outlier_check %>% filter(submissiondate >=as.Date("2021-08-17"))

# add location vars
outlier_check = outlier_check %>% 
  left_join(hfc_constr %>% select(ID_05,pl_id_06,pl_id_07,
                                  pl_id_08,pl_id_09),
            by='ID_05')

    ########################################################################################################
    
    # 4. Export Data ----
    
    hfc_sheet %>%
      
      sheet_write(data = outlier_check, sheet = "outlier_data")

    # In case googlesheets4 asks me to enter '1' to continue
    
    1
    
  
    # 
    # export outlier list
 write.csv(outlier_check,
           file.path(dropbox,
          'Rwanda Roads Data/Primary data/HH survey/endline/data/raw/outputs/outliers_0925.csv'),
          row.names=F)   
    
    ########################################################################################################
    
    ## 5. Export Data for household without permanent crops or seasonal crops----
    
    # 
    crops <-  hfc_data %>%
      
        select(ID_05, ID_03,enumerator_name, ID_04, SubmissionDate, id_24,
               pl_id_06,pl_id_07,pl_id_08,pl_id_09,
               #village, cell, sector, district,
               d_n_1, d_01,d_61_a,b_01_1_1,b_01_1_2,b_01_1_3,
               b_01_oth_1_1,b_01_oth_1_2,b_01_oth_1_3) %>% 
      
        filter(d_n_1 == 0 | d_01 == 0) %>%
      
        rename(permanent_crops = d_n_1,seasonal_crops = d_01,phone_number =id_24,
               village = pl_id_06,
               cell = pl_id_07,
               sector = pl_id_08,
               district = pl_id_09) %>% 
   mutate(permanent_crops=ifelse(permanent_crops==0,'No','Yes'),
          seasonal_crops=ifelse(seasonal_crops==0,'No','Yes'),
          livestock = ifelse(d_61_a==0,'No','Yes')) %>% 
   mutate(occupation_s1= case_when(b_01_1_1==1~'Agriculture on own farm',
                                b_01_1_1==2~'Wage farm/livestock',
                                b_01_1_1==3~'Petty/retail trade',
                                b_01_1_1==4~'Other private sector',
                                b_01_1_1==5~'Public works',
                                b_01_1_1==6~'Government/public sector',
                                b_01_1_1==7~'Services (incl. tourism)',
                                b_01_1_1==8~'Household chores/ taking care of dependants',
                                b_01_1_1==9~'School',
                                b_01_1_1==10~'Livestock keeping',
                                b_01_1_1==11~'casual work (non-agricultural)',
                                b_01_1_1==-77~b_01_oth_1_1,
                                TRUE~'None/Dont know'),
          occupation_s2= case_when(b_01_1_2==1~'Agriculture on own farm',
                                   b_01_1_2==2~'Wage farm/livestock',
                                   b_01_1_2==3~'Petty/retail trade',
                                   b_01_1_2==4~'Other private sector',
                                   b_01_1_2==5~'Public works',
                                   b_01_1_2==6~'Government/public sector',
                                   b_01_1_2==7~'Services (incl. tourism)',
                                   b_01_1_2==8~'Household chores/ taking care of dependants',
                                   b_01_1_2==9~'School',
                                   b_01_1_2==10~'Livestock keeping',
                                   b_01_1_2==11~'casual work (non-agricultural)',
                                   b_01_1_2==-77~b_01_oth_1_2,
                                   TRUE~'None/Dont know'),
          occupation_s3= case_when(b_01_1_3==1~'Agriculture on own farm',
                                   b_01_1_3==2~'Wage farm/livestock',
                                   b_01_1_3==3~'Petty/retail trade',
                                   b_01_1_3==4~'Other private sector',
                                   b_01_1_3==5~'Public works',
                                   b_01_1_3==6~'Government/public sector',
                                   b_01_1_3==7~'Services (incl. tourism)',
                                   b_01_1_3==8~'Household chores/ taking care of dependants',
                                   b_01_1_3==9~'School',
                                   b_01_1_3==10~'Livestock keeping',
                                   b_01_1_3==11~'casual work (non-agricultural)',
                                   b_01_1_3==-77~b_01_oth_1_3,
                                   TRUE~'None/Dont know')) %>% 
   select(-c(d_61_a,b_01_1_1,b_01_1_2,b_01_1_3,b_01_oth_1_3,b_01_oth_1_2,b_01_oth_1_1))
 

        

    hfc_sheet %>%
      
      sheet_write(data = crops, sheet = "nocrops_data")
    
    1
    
    livestock = hfc_data %>% 
      select(ID_05, ID_03,enumerator_name, ID_04, SubmissionDate, id_24,
             pl_id_06,pl_id_07,pl_id_08,pl_id_09,
             #village, cell, sector, district,
             d_61_a) %>% 
      
      filter(d_61_a == 0) %>%
      
      rename(livestock = d_61_a,phone_number =id_24,
             village = pl_id_06,
             cell = pl_id_07,
             sector = pl_id_08,
             district = pl_id_09) %>% 
      mutate(livestock = ifelse(livestock==0,'No','Yes'))
    
    
    hfc_sheet %>%
      
      sheet_write(data = livestock, sheet = "nolivestock_data")
    
    1
    
    
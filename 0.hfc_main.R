########################################################################################################
#                                                                                                      #
#                                  HIGH-FREQUENCY CHECKS  -- MAIN                                      #
#                                                                                                      #
########################################################################################################

  ## PURPOSE      High-frequency checks

  ## AUTHOR      Juliana Guerrero (adapted from Adrien Ciret & Marc-Andrea Fiorina)

  ## LAST UPDATE  October 5, 2023


####################################################################################################### 
# remove objects
rm(list=ls())

hfc_code_filepath <- 'C:/Users/wb605157/DIME/GitHub/Rwanda-Roads/Data-processing/HH Survey/Survey_2023/HFC/'
  ## 4. Run R Scripts ----

   
  source(paste0(hfc_code_filepath,   "2.hfc_construct.R"))
      
  1
  
  1
  
  print("Script 2 Done.")
  
  source(paste0(hfc_code_filepath,   "3.hfc_admin.R"))
  
  
# In case googledrive and googlesheets4 packages ask for '1' to confirm account re-authentication
  
  1
  
  1
      
  print("Script 3 Done.")
    
  source(paste0(hfc_code_filepath,   "4.hfc_enum.R"))
      
  1
  
  print("Script 4 Done.")
      
  source(paste0(hfc_code_filepath,   "5.hfc_outliers.R")) 
      
  1
  
  print("Script 5 Done.")
      
  source(paste0(hfc_code_filepath,   "6.hfc_logic.R"))
      
  1
  
  print("Script 6 Done.")

  
  
  source(paste0(hfc_code_filepath,   "8.hfc_stats.R"))
  
  1
  
  print("Script 8 Done.")
  
  
  source(paste0(hfc_code_filepath,   "9.hfc_bc.R"))
  
  1
  
  print("Backcheck export Done.")
  
 
  
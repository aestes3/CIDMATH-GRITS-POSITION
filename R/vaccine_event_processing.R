library(here)
library(tidyverse)
library(readxl)
library(DT)
library(httr)

###############################################################################################
# STEP 3: Merge driver table on vaccine event data                                            #                                           
###############################################################################################  
#Outer join on patient ID and CVX code, and vax admin date is between driver table vaccine
#receipt start + end dates

vaccine_event_processing <- function(vax_df, patient_id_column, bday_column, cvx_column, between_vax_bday_column)
    {

    #browser()
    #Load ETL Data Function to get CVX codes from CDC URL
    vax_group_cvx_codes_url <- 'https://www2a.cdc.gov/vaccines/iis/iisstandards/downloads/web_vax2vg.xlsx'
    raw_xlsx <- httr::GET(vax_group_cvx_codes_url)$content
    tmp <- tempfile(fileext = '.xlsx')
    writeBin(raw_xlsx, tmp)
    cdc_vax_cvx <- readxl::read_excel(tmp)
    
    # Subset CVX codes 
    vax_cvx_short <- subset(cdc_vax_cvx, select = c("CVX Code", "VG_Name", "vaccine status"))
    #Rename Columns
    names(vax_cvx_short)[1] <- "cvx_code"
    names(vax_cvx_short)[2] <- "vaccine_group"
    names(vax_cvx_short)[3] <- "vaccine_status"
    
    #load vaccine schedule
    load(here::here("data/vax_schedule.Rdata"))
    # already renamed here
    #names(vax_schedule)[1] <-"vaccine_group"
    
    #Create patient id column    
    vax_df_patient_id_data <- vax_df[[patient_id_column]]
    
    # ### Create bday column 2 ###
    vax_df_bday_data <- vax_df[[bday_column]]
    
    # Birthday is last of every month
    vax_df_bday_end_data <- rollforward(vax_df_bday_data)
    
    #Create cvx
    cvx_code <- vax_df[[cvx_column]]
    
    
    #Patient DOB and difference in days of vaccine event
    diff_vax_dob_data <- vax_df[[between_vax_bday_column]]
    
    diff_vax_dob_data <- as.numeric(diff_vax_dob_data)
    
    #merge data to have ID, End of Month DOB, Diff in DOB and Vax Event and CVX Code
    sorted_vax_data <- data.frame(vax_df_patient_id_data, vax_df_bday_end_data, 
                                  diff_vax_dob_data, cvx_code)
    
    # Combine vax data and sorted data to add vaccine group and active/inactive status
    combined_vax_data <- merge(sorted_vax_data, vax_cvx_short) |>  
        arrange(vax_df_patient_id_data)

    
    ###########################################################################
    #                     START MERGE                                         #
    ###########################################################################
    
    #Load driver table
    load(here("data/driver_table.Rdata"))
    
    #Sort driver table data to include only ID, vax group, utd range start and end
    sorted_driver_table <- subset(vaccine_schedule_data, select = c("vax_df_patient_id_data",
                                                          "vaccine_group",
                                                          "vaccine_dose",
                                                          "utd_range_start_days",
                                                          "utd_range_end_days"))
    #Rename vaccine_dose to expected_vaccine_dose
    names(sorted_driver_table)[3] <- "expected_vaccine_dose"
    
    #Standardize vaccine_group names with CDC names
    load(here("data/vax_group_relationship.Rdata"))
    
    # Join combined_vax_data onto vax_group_relationships by CVX code
    test_combined_vax_data <- merge(combined_vax_data, vax_group_relationship,
                                    by.x = "cvx_code", by.y= "cvx_code", all.x=TRUE)
      
    #test_combined_vax_data$vaccine_dose_by_group <-  |> group_by(vax_df_patient_id_data)
    add_vax_count_col <- test_combined_vax_data |>
      group_by(vax_df_patient_id_data, vaccine_group.x, vaccine_group_schedule) |> 
            arrange(vax_df_patient_id_data, vaccine_group.x,vaccine_group_schedule, diff_vax_dob_data) |> mutate(vax_count_col = row_number())
      #summarise(vaccine_dose_by_group=n(),.groups = 'drop') |>  
      #as.data.frame()
      
    
    #Join by parameters
    by <- join_by(vax_df_patient_id_data, vaccine_group_schedule==vaccine_group,
                  between(diff_vax_dob_data, utd_range_start_days, utd_range_end_days))
    
    actual_vaccines_received <- full_join(add_vax_count_col, sorted_driver_table,
                                          by=by) |> subset(select = -vaccine_status.y) |> 
                                                                               subset(select = -vaccine_group.y) |>  
                                                                                                          #filter(!is.na(utd_range_start_days)) |> 
                                                                                                                                        rename(vaccine_group = vaccine_group.x) |> 
                                                                                                                                                rename(vaccine_status = vaccine_status.x)
   
    
    ###########################################################################
    #                       Finish Merge                                      #
    ###########################################################################
    
    
    #view(actual_vaccines_received)
    save(actual_vaccines_received, file = here("data/vaccine_event_processed.Rdata"))
    return(actual_vaccines_received)
}

#################
# Test Function #
#################

#check <- vaccine_event_processing(data,"id", "bday", "cvx", "vax_date")

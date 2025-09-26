library(here)
library(tidyverse)
library(readxl)
library(DT)
library(httr)
#Dummy Data
#load(here("data/DummyData.Rdata"))


driver_table <- function(vax_df, 
                         patient_id_column, 
                         bday_column,
                         recent_vax_date
                         )
    { 

        ### Create patient id column 1 ###  
        vax_df_patient_id_data <- vax_df[[patient_id_column]]
        
        # ### Create bday column 2 ###
        vax_df_bday_data <- vax_df[[bday_column]]
        
        # Birthday is last of every month
        vax_df_bday_end_data <- rollforward(vax_df_bday_data)
        
        ## Create recent vax date column 3 ##
        vax_df_recent_vax_date <- vax_df[[recent_vax_date]]
        
        ####### USE LUBERDATE PACKAGE TO CONVERT TO END OF MONTH WITH MM/YY FORMAT
        
        
        ######## Create latest valid vaccine event column 3 ########
        
        # Find most recent vax date
        recent_vax <- max(vax_df_recent_vax_date, na.rm = TRUE)
        
       
        
        #Create interval for bday - latest vax event
        ### Change to make sure that it uses the Month/yr instead of bday date
        
        time_diff_bday_recent_vax_int <- interval(start = vax_df_bday_end_data, end = recent_vax) 
        time_diff_bday_recent_vax_dur <- as.duration(time_diff_bday_recent_vax_int)
        
        #Convert to unit days
        time_diff_bday_recent_vax_days <- make_difftime(time_diff_bday_recent_vax_dur, units ="days")
        
        #merge data to match formats and combine all steps 
        #Creates dataset of all user inputs and column of time diff between latest vax event and bday_end_data
        vax_patient_level_data <- unique(data.frame(vax_df_patient_id_data, vax_df_bday_end_data,
                                             recent_vax,time_diff_bday_recent_vax_days, row.names = NULL)) #Remove row names warning solved
        

       
###############################################################################################
# STEP 2: Need some way to merge the STEP 1 table onto the vaccine schedule table.            #
# Do a merge on any on-time timeliness rows of the vaccine schedule table where the STEP 1,   #
# Column 4 diff in days is > dose_admin_end in days.                                          #
############################################################################################### 
        
        #Import vax schedule
        load(here("data/vax_schedule.Rdata"))
        #Include only on time vaccines
        on_time_vax <- vax_schedule |> filter(timeliness == "On-Time")
        
       #Create duration of end time periods of vaccinations for each vaccine based on number and specific unit type (Weeks, months)
        dose_admin_end_dur <- duration(num= on_time_vax$dose_admin_end, 
                                       units = on_time_vax$dose_admin_end_measure_unit)
        
        #Convert duration into days from time period previously used
        on_time_vax$dose_admin_end_days <- make_difftime(dose_admin_end_dur, unit= "days")
        
        # Perform the cross join and filter where time_diff_bday_recent_vax_days > dose_admin_end_days
        # To see what vaccines should have been received per ID by the latest vaccination date
        merged_data <- vax_patient_level_data |> 
                    crossing(on_time_vax) |>  # Create all combinations of vax_patient_level_data and vax_schedule
                    filter(time_diff_bday_recent_vax_days > dose_admin_end_days) # Filter for date_1 > date_2 (converted admin_end to days) 
        
        #Create columns for up to date status (utd) range start to end
        #Must fall within this time frame to be considered an on-time vaccine
        merged_data$utd_range_start <- as.Date(merged_data$vax_df_bday_end_data) + duration(num = merged_data$dose_admin_start,
                                                      units = merged_data$dose_admin_start_measure_unit)
        
        merged_data$utd_range_end <- as.Date(merged_data$vax_df_bday_end_data) + duration(num = merged_data$dose_admin_end,
                                                     units = merged_data$dose_admin_end_measure_unit)
        
        #span <- interval(as.Date(merged_data$utd_range_start), as.Date(merged_data$utd_range_end)) 
        #check <- as.duration(span)
        #merged_data$days_between_utd <- as.numeric(check, "days")
        
        ## Create 2 columns from utd_range_start and end to start at 0 days 
        #Subtract from vax_df_bday_end_data
        merged_data$utd_range_start_days <- as.duration(merged_data$utd_range_start - merged_data$vax_df_bday_end_data)
        merged_data$utd_range_end_days <- as.duration(merged_data$utd_range_end - merged_data$vax_df_bday_end_data)
        
        #Round and convert to days
        merged_data$utd_range_start_days <- floor(as.numeric(merged_data$utd_range_start_days, unit= "days"))
        merged_data$utd_range_end_days <- ceiling(as.numeric(merged_data$utd_range_end_days, unit = "days"))
        
        
      
        # View the result
        vaccine_schedule_data <- merged_data
        #view(vaccine_schedule_data)
        save(vaccine_schedule_data, file = here("data/driver_table.Rdata"))
        
    return(vaccine_schedule_data)
}


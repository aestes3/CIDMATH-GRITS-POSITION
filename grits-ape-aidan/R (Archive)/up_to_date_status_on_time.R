#Load packages
library(here)
library(tidyverse)
library(readxl)
library(DT)

up_to_date_status_on_time <- function(vaccine_df, dose_num, vax_group){
    
    #Load vaccine schedule
    load(here("data/vax_schedule.Rdata"))
    
    #Filter vaccine schedule data for vax group and dose number
    vax_schedule <- vax_schedule |>
        filter(vaccine_group == vax_group) |>
        filter(vaccine_dose == dose_num) |> filter(timeliness == "On-Time")
    
    #Filter vaccine_df data for vax group and dose number
    vaccine_df <- vaccine_df |>
        filter(vaccine_group == vax_group) |>
        filter(dose_number == dose_num)
                                                                       
########################### Cannot load combined_vax_data                                                              
    
    #Turn Patient bday to variable
    bdate <- vaccine_df$vax_df_bday_data
    
    
    #Get dose vax dates 
    dose_date <- vaccine_df$vax_date_data
    
    #create interval of dose  (On-time)
    
    #######################################################################
    # Had to modify original vaccine schedule xlsx to remove NAs in units #
    #######################################################################
    
    #Convert admin start measure unit to string
    start_measure_unit <- as.character(vax_schedule$dose_admin_start_measure_unit)
    end_measure_unit <- as.character(vax_schedule$dose_admin_end_measure_unit)
    
    #Get vaccine time unit to create interval
    period_start <- period(vax_schedule$dose_admin_start, units = start_measure_unit)
    period_end <- period(vax_schedule$dose_admin_end, units = end_measure_unit)
    
    interval <- interval(start = bdate + period_start , end = bdate + period_end)
    #Check dose on time
    vax_event_on_time <- dose_date %within% interval
    
   
    return(vax_event_on_time)
}
############
# TEST #####
############
# check <- up_to_date_status_on_time(load_1_polio, 1, "Polio (IPV)")

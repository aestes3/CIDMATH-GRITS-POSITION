#Load packages
library(here)
library(tidyverse)
library(readxl)
library(DT)

#########Check if On-time #########

#Import all data columns
load_up_to_date_status_data <- function(vax_df, patient_id_column, bday_column, cvx_column, 
                              vax_date_column,patient_id_eval,vaccine_group_eval){
    
    #Import short_combine cvx codes, vaccine group, and target pathogen
    load(here::here("data/short_combine.Rdata"))
    names(short_combine)[2] <- "cvx_code_data"
    
    #load vaccine schedule
    load(here::here("data/vax_schedule.Rdata"))
    #names(vax_schedule)[1] <-"vaccine_group"
    
    
    #Create patient id column    
    vax_df_patient_id_data <- vax_df[[patient_id_column]]
    
    #Create bday column
    vax_df_bday_data <- vax_df[[bday_column]]
    
    #Create cvx
    cvx_code_data <- vax_df[[cvx_column]]
    
    #Create vax date column
    vax_date_data <- vax_df[[vax_date_column]]
    
    #merge data to match short_combine
    sorted_vax_data <- data.frame(vax_df_patient_id_data, vax_df_bday_data, cvx_code_data, vax_date_data)
    
    #Combine all and filter to evaluate 1 patient and one vaccine group and sort by vax date
    combined_vax_data <- merge(sorted_vax_data, short_combine) |>  
        arrange(vax_df_patient_id_data) |> 
        filter(vax_df_patient_id_data == patient_id_eval) |>
        filter(vaccine_group == vaccine_group_eval) |> 
        arrange(vax_date_data)
    
    #Rank vax dates to create dose number
    combined_vax_data$dose_number <- rank(combined_vax_data$vax_date_data)

    view(combined_vax_data)
    
   
    return(combined_vax_data)
}


##########################################################
# Example request for patient ID 1's Polio vaccine data  #
##########################################################

#load(here("data/DummyData.Rdata"))
#load_1_polio <- load_up_to_date_status_data(data, "id", "bday", "cvx", "vax_date", 1, "Polio (IPV)")


#Aidan Estes
#GRITS CIDMATH Functions
#File Start Date: 11/01/24

#Load packages
library(here)
library(tidyverse)
library(readxl)
library(DT)

#########Check if On-time #########

#Import all data columns
up_to_date_status <- function(vax_df, patient_id_column, bday_column, cvx_column, 
                              vax_date_column,patient_id_eval,vaccine_group_eval){
    
    #Load up to date status function
    source(here("R/load_up_to_date_status_data.R"))
        
    
    hepB_up_to_date <- function(){
        
        #Turn Patient 1 bday to variable
        bdate <- vax_df_bday_data[1]
        
        #Get vax dates 
        hepB1_dose_date <- combined_vax_data$vax_date_data[combined_vax_data$dose_number == 1]
        hepB2_dose_date <- combined_vax_data$vax_date_data[combined_vax_data$dose_number == 2]
        hepB3_dose_date <- combined_vax_data$vax_date_data[combined_vax_data$dose_number == 3]
        
        
        #create interval of HepB dose 1 (On-time)
        interval_hepB_1 <- interval(start = bdate, end = bdate+ months(1))
        #Check dose on time
        hepB1on_time <- hepB1_dose_date %within% interval_hepB_1
        
        #Create interval of HepB Dose 2 (On-time)
        interval_hepB_2 <- interval(start = bdate + months(1), end=bdate + months(3))
        #Check dose 2 on time
        hepB2_on_time <- hepB2_dose_date %within% interval_hepB_2
        
        #create interval of HepB dose 3 (On-time)
        interval_hepB_3 <- interval(start = bdate + months(6), end=bdate + months(15))
        #Check dose 3 on time
        hepB3_on_time <- hepB3_dose_date %within% interval_hepB_3
        
        
    }
    
    #Polio Function
        polio_up_to_date <- function()
            #combined_vax_data, bdate, vax_date, polio1_on_time, polio2_on_time,
            #polio3_on_time, polio4_on_time, interval_polio_1,
           # interval_polio_2, interval_polio_3, interval_polio_4, polio1_dose_date,
           # polio2_dose_date, polio3_dose_date, polio4_dose_date) 
            {
            
            #create patient bdate variable 
            bdate <- vax_df_bday_data[1]
            #Get vax dates 
            polio1_dose_date <- combined_vax_data$vax_date_data[combined_vax_data$dose_number == 1]
            polio2_dose_date <- combined_vax_data$vax_date_data[combined_vax_data$dose_number == 2]
            polio3_dose_date <- combined_vax_data$vax_date_data[combined_vax_data$dose_number == 3]
            polio4_dose_date <- combined_vax_data$vax_date_data[combined_vax_data$dose_number == 4]
            
            #####Create time intervals for polio###
            interval_polio_1 <- interval(start = bdate + months(2), end = bdate+ months(3))
            #Check dose on time
            polio1_on_time   <-  polio1_dose_date %within% interval_polio_1
            
            interval_polio_2 <- interval(start = bdate + months(4), end = bdate+ months(5))
            polio2_on_time   <- polio2_dose_date %within% interval_polio_2
            
            interval_polio_3 <- interval(start= bdate + months(6), end = bdate + months(18))
            polio3_on_time   <- polio3_dose_date %within% interval_polio_3
            
            interval_polio_4 <- interval(start = bdate + months(48), end = bdate + months(72))
            polio4_on_time   <- polio4_dose_date %within% interval_polio_4
            #####
            #Combine results into column
            on_time_results <- c(polio1_on_time, polio2_on_time, polio3_on_time, polio4_on_time)
            combined_vax_data$results <- on_time_results
            return(combined_vax_data)
        } 
        #Run polio function if eval otherwise skip
                if (vaccine_group_eval == "Polio (IPV)") {
                    result <- polio_up_to_date()
                 }else { 
                    result <- "Skipping Polio (IPV) processing"
                 return(result)
                } 
            
            
    return(combined_vax_data)
    }

up_to_date_status(data, "id", "bday", "cvx", "vax_date", 1, "Polio (IPV)")


#########Check if correctly spaced #########

#Dose 1 to 2 correctly spaced (TRUE/FALSE)
spacing_hepB_1_2 <- interval(start = patient_1_vax1 + weeks(4), end= patient_1_vax2)
spacing_hepB_1_2_correct <- patient_1_vax2 %within% spacing_hepB_1_2

#Dose 2 to 3 correctly spaced (TRUE/FALSE)
spacing_hepB_2_3 <- interval(start = patient_1_vax2 + weeks(8), end = patient_1_vax3)
spacing_hepB_2_3_correct <- patient_1_vax3 %within% spacing_hepB_2_3

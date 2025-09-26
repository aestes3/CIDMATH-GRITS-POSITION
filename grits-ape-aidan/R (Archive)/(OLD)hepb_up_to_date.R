#Load packages
library(here)
library(tidyverse)
library(readxl)
library(DT)

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
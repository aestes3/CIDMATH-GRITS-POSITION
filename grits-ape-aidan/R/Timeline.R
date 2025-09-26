# Load packages
library(tidyverse)
library(readxl)
library(DT)
library(knitr)
library(kableExtra)
library(httr)
library(here)


plot_vaccine_timeline <- function(patient_id) {
    
    vaccination_data <- actual_vaccines_received |> subset(
        select = c("vax_df_patient_id_data","diff_vax_dob_data" ,"vaccine_group_schedule", "vax_count_col",
                   "expected_vaccine_dose", "utd_range_start_days", "utd_range_end_days")) |>  
        filter(vax_df_patient_id_data == patient_id & vaccine_group_schedule != "COVID-19")  |> #Hard code for now to ID one only
        filter(vaccine_group_schedule != "RSV") |> #Drop unnecessary vaccines 
        filter(vaccine_group_schedule != "Influenza") |> 
        filter(vaccine_group_schedule != "Hepatitis A")
    #Create actual vaccine date in days var
    
    vaccination_data$actual_vaccination_days <- 0 + vaccination_data$diff_vax_dob_data
    
    # Create the timeline plot
    ggplot(vaccination_data, aes(y = vaccine_group_schedule, x = utd_range_start_days,
                                 xend = utd_range_end_days, color = "On-time Range")) +
        # Recommended range
        geom_segment(aes(xend = utd_range_end_days, yend = vaccine_group_schedule),
                     na.rm= TRUE, linewidth = 4, show.legend = TRUE) + 
        
        # Actual event
        geom_point(aes(x = actual_vaccination_days), size = 3, color = "gold", show.legend = FALSE) + 
        
        #Add Line Divders for between each up to date start / end days
        geom_segment(aes(x = utd_range_start_days, xend = utd_range_start_days,
                         y = as.numeric(factor(vaccine_group_schedule)) - 0.2,
                         yend = as.numeric(factor(vaccine_group_schedule)) + 0.2,
                         color = "Dosage Start"),
                     linetype = "solid", size = 1, show.legend = TRUE) +
        
        geom_segment(aes(x = utd_range_end_days, xend = utd_range_end_days,
                         y = as.numeric(factor(vaccine_group_schedule)) - 0.2,
                         yend = as.numeric(factor(vaccine_group_schedule)) + 0.2,
                         color = "Dosage End"),
                     linetype = "solid", size = 1, show.legend = TRUE) +
        
        #Annotate Dividers
        scale_color_manual(name = "Dividers",
                           values = c("On-time Range" = "black",
                                      "Dosage Start" = "red",
                                      "Dosage End" = "blue"),
                           breaks = c("Dosage Start", "Dosage End", "On-time Range")) +
        
        #Add graph labels
        labs(title = "Vaccination Timeline",
             x = "Patient Birth to Vax (Days)",
             y = "Vaccine", 
             caption = "Lines show the recommended window, dots show actual vaccination dates") +
        theme_classic() +
        theme(legend.position = "top")
    
}

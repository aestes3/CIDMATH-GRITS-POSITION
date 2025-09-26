#Aidan Estes
#GRITS CIDMATH
#File Start Date: 10/25/24

#Load packages
library(here)
library(tidyverse)
library(readxl)
library(DT)

#########Import and label dataframes#############

#Read CVX code excel documents
cvx_clean <- read_xlsx(here("data-raw/cvx_clean.xlsx"))

vax_to_target <- read_xlsx(here("data-raw/vax-to-target.xlsx"))

#Merge CVX Code files
combined <- merge(cvx_clean,vax_to_target, by = "Vaccine Group")
#Fix Combined column names
colnames(combined) <- c("vaccine_group", "cvx", "cvx_desc", "vax_full_name", "note", 
                        "active","internalID", "isVax", "vax_updated_date",
                        "target_pathogen")
                                                     

#Import Dummy Data, set date format and column names
data <- read_xlsx(here("data-raw/DummyDataForTesting.xlsx"), 
                  col_names = c("scenario_number", "id", "bday", "cvx", "vax_date", "vax_name", "dose_number", "t_type", "between_vax_bday"),
                  col_types = c("numeric", "numeric", "date", "text", "date", "text", "numeric", "text", "numeric"),
                  skip = 2) 

save(data, file = here("data/DummyData.Rdata"))

#Import vaccine scheduling, set format and column names
vax_schedule <- read_xlsx(here("data-raw/Vaccine Schedule.xlsx"),
                          col_names = c("vaccine_group", "vaccine_dose", "indication", "recurring", 
                                        "recurrence_pattern_value", "recurrence_pattern_unit", "season_vax_indic_start_month", 
                                        "season_vax_indic_end_month", "timeliness", "dose_admin_start", "dose_admin_start_measure_unit",
                                        "dose_admin_end","dose_admin_end_measure_unit", "caveat_additional_info"),
                         col_types = c("text", "numeric", "text", "text",
                                        "numeric", "text", "text",
                                        "text", "text", "numeric", "text",
                                        "numeric", "text","text"),
                          skip = 2)
#Save as Rdata file
save(vax_schedule, file = here("data/vax_schedule.Rdata"))

#Import Time between doses
time_between_doses <- read_xlsx(here("data-raw/Vaccine Schedule.xlsx"), sheet ="Time Between Doses")
                                #col_types = "text", "numeric", "numeric", "numeric",
                                #"numeric", "text", "text")
#Save as Rdata file
save(time_between_doses, file = here("data/time_between_doses.Rdata"))

#Import Vax Standard Names
vax_group_relationship <- read_xlsx(here("data-raw/Vax Group Relationship Table.xlsx"),
                                    col_names = c("cvx_code", "vaccine_status", "vaccine_group", "vaccine_group_schedule"),
                                    col_types = c("text","text","text","text"),
                                    skip=1)
save(vax_group_relationship, file = here("data/vax_group_relationship.Rdata"))

#########Organize Data frames ############

#Shorten combined dataset to include only relevant data
short_combine <- subset(combined, select = c("vaccine_group", "cvx", "target_pathogen"))
                        
#Save as Rdata file                        
save(short_combine,file = here("data/short_combine.Rdata"))


#get all patient 1 data
patient_1 <-  filter(data, id == "1") |>  
                                        subset(select = -c(t_type, dose_number, scenario_number))

#Get all data on patient 1 for HepB and arrange it by date ascending
patient_1_hepB <- merge(patient_1, short_combine) |>  
                                                     filter(target_pathogen == "Hepatitis B") |> 
                                                                                                 arrange(vax_date)    
                                                                                                                                                                                            

#Filter Vax schedule for only Hep B
vax_schedule_hepB <- filter(vax_schedule, vaccine_name == "Hepatitis B") 

#Filter time between for Hep B
time_between_hepB <- filter(time_between_doses, vaccine_name == "Hepatitis B")
                                                                            
                                 
#########Check if On-time #########

#Turn Patient 1 bday to variable
patient_1_bday <- patient_1_hepB$bday[1]
#Create var for HepB dose 1 vax date
patient_1_vax1 <- patient_1_hepB$vax_date[1]

#create dose number
patient_1_hepB$dose_number <- rank(patient_1_hepB$vax_date)

#create interval of HepB dose 1 (On-time)
dose_hepB_1 <- interval(start = patient_1_bday, end = patient_1_bday+ months(1))
#Check dose on time
hepB_dose1_on_time <- patient_1_vax1 %within% dose_hepB_1

#Create var for HepB dose 2 vax date
patient_1_vax2 <- patient_1_hepB$vax_date[2]
#Create interval of HepB Dose 2 (On-time)
dose_hepB_2 <- interval(start = patient_1_bday + months(1), end=patient_1_bday + months(3))
#Check dose 2 on time
hepB_dose2_on_time <- patient_1_vax2 %within% dose_hepB_2

#Create var for HepB Dose 3 vax date
patient_1_vax3 <- patient_1_hepB$vax_date[3]
#create interval of HepB dose 3 (On-time)
dose_hepB_3 <- interval(start = patient_1_bday + months(6), end=patient_1_bday + months(15))
#Check dose 3 on time
hepB_dose3_on_time <- patient_1_vax3 %within% dose_hepB_3

#########Check if correctly spaced #########

#Dose 1 to 2 correctly spaced (TRUE/FALSE)
spacing_hepB_1_2 <- interval(start = patient_1_vax1 + weeks(4), end= patient_1_vax2)
spacing_hepB_1_2_correct <- patient_1_vax2 %within% spacing_hepB_1_2

#Dose 2 to 3 correctly spaced (TRUE/FALSE)
spacing_hepB_2_3 <- interval(start = patient_1_vax2 + weeks(8), end = patient_1_vax3)
spacing_hepB_2_3_correct <- patient_1_vax3 %within% spacing_hepB_2_3


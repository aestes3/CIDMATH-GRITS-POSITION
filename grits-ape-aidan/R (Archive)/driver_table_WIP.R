library(here)
library(tidyverse)
library(readxl)
library(DT)
#Dummy Data
#load(here("data/DummyData.Rdata"))


### Create patient id column 1 ###  
vax_df_patient_id_data <- data$id

### Create bday column 2 ###
vax_df_bday_data <- data$bday

######## Create latest valid vaccine event column 3 ########


#Create latest vax date column 
sum_test <- summary(data$vax_date)
recent_vax <- as.POSIXct.numeric(sum_test["Max."], tz="UTC")

#Create interval for bday - latest vax event
time_diff <- interval(start = vax_df_bday_data, end = recent_vax)
bday_diff <- as.duration(time_diff)

#Convert to unit days
latest_diff_bday_days <- make_difftime(time_diff, units ="days")

#merge data to match short_combine
sorted_vax_data <- unique(data.frame(vax_df_patient_id_data, vax_df_bday_data,
                                     recent_vax,latest_diff_bday_days))



# Perform the cross join and filter where date_1 > date_2
merged_data <- test %>%
    crossing(vax_schedule) %>%  # Create all combinations of A and B
    filter(latest_diff_bday_days > dose_admin_end*30.4167) # Filter for date_1 > date_2 (converted admin_end to days)


# View the result
print(merged_data)

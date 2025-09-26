## Putting it all together
library(here)
library(tidyverse)
library(readxl)
library(DT)


# 1. Load your vaccine event data (Dummy Data I created)

load(here("data/DummyData.Rdata"))


# 2. Run driver_table.R
source(here("R/driver_table.R"))
driver_table(data, "id", "bday")


# 3. Join vax event data to driver table
source(here("R/vaccine_event_processing.R"))
vaccine_event_processing(data,"id", "bday", "cvx", "between_vax_bday")

# 4. Plot timeline example

# Sample Data
vaccination_data <- data.frame(
    dose = c(1, 1, 2, 2, 3),
    Vaccine = c("MMR", "HepB", "MMR", "HepB", "MMR"),
    Recommended_Start = as.Date(c("2024-01-01", "2024-02-01", "2024-01-05", "2024-02-10", "")),
    Recommended_End = as.Date(c("2024-02-15", "2024-03-01", "2024-02-20", "2024-03-10", "")),
    Actual_Vaccination_Date = as.Date(c("2024-02-10", "2024-02-25", "2024-02-15", "2024-03-05", "2024-03-05"))
)

# Create the timeline plot
ggplot(vaccination_data, aes(y = Vaccine, x = Recommended_Start, xend = Recommended_End, color = Vaccine)) +
    geom_segment(aes(xend = Recommended_End, yend = Vaccine), linewidth = 2) + # Recommended range
    geom_point(aes(x = Actual_Vaccination_Date), size = 4, color = "black") + # Actual event
    labs(title = "Vaccination Timeline",
         x = "Date",
         y = "Vaccine",
         caption = "Lines show the recommended window, dots show actual vaccination dates") +
    theme_minimal() +
    theme(legend.position = "none")


##########



# 3. Specify vaccine group and patient_id
vax_group <- "MMR"
patient_id = 1
dose_number = 1

# 4. Run load_up_to_date_status_data.R
source(here("R/load_up_to_date_status_data.R"))
patient_df <- load_up_to_date_status_data(data, "id", "bday", "cvx", "vax_date", patient_id, vax_group)

# 5. Run up_to_date_status_on_time.R
source(here("R/up_to_date_status_on_time.R"))
up_to_date_status_on_time(patient_df, dose_number , vax_group)


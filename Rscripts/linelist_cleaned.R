# Install and load required packages
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}
pacman::p_load(
  rio,        # Importing data  
  here,       # Relative file pathways  
  janitor,    # Data cleaning and tables
  matchmaker, # Dictionary-based cleaning
  epikit,     # age_categories() function
  tidyverse,  # Data management and visualization
  skimr       # Summary statistics and visualization
)

# Import CSV file from desktop
linelist_raw <- import("C:/Users/user/OneDrive - Technische Hochschule Deggendorf/Desktop/Dataset.csv")

# Summarize the dataset
summary(linelist_raw)

# Clean column names
linelist <- janitor::clean_names(linelist_raw)

# Rename columns with spaces
linelist <- linelist %>%
  dplyr::rename(
    "Gender" = "gender",
    "Height" = "height",
    "Age" = "age",
    "Weight" = "weight",
    "Family_History_With_Overweight" = "family_history_with_overweight",
    "Frequent_High_Caloric_Food_Consumption" = "favc",
    "Frequency_Vegetables_Consumption" = "fcvc",
    "Number_Of_Main_Meals" = "ncp",
    "Consumption_Of_Food_Between_Meals" = "caec",
    "Smoke" = "smoke",
    "Consumption_Of_Water_Daily" = "ch2o",
    "Calories_Consumption_Monitoring" = "scc",
    "Frequency_Physical_Activity" = "faf",
    "Time_Using_Technology_Devices" = "tue",
    "Consumption_Alcohol" = "calc",
    "Transportation_Mode" = "mtrans",
    "Obesity_Level" = "n_obeyesdad"
  )

# Check for missing values
print(paste0("Total missing values in the dataset: ", sum(is.na(linelist))))

# Check for duplicates
duplicates <- duplicated(linelist)
num_duplicates <- sum(duplicates)
print(paste0("Number of duplicates: ", num_duplicates))

# Remove duplicates
linelist <- linelist[!duplicated(linelist), ]

# Summarize the dataset
skim(linelist)
names(linelist)
glimpse(linelist)
# Install and load the DT package
if (!requireNamespace("DT", quietly = TRUE)) {
  install.packages("DT")
}
library(DT)

linelist %>% 
  datatable(class="cell-border" ,
            caption='Interactive table view of the dataset',
            rowname = F,
            filter = 'top',
            options = list(pageLength = 20, autoWidth = TRUE))
# Check the structure of variables in the linelist data frame
str(linelist)
# Check the class of a specific variable
class(linelist$Gender)
# Convert character columns to logical
linelist$Frequent_High_Caloric_Food_Consumption <- linelist$Frequent_High_Caloric_Food_Consumption == "yes"
linelist$Smoke <- linelist$Smoke == "yes"
linelist$Family_History_With_Overweight <- linelist$Family_History_With_Overweight == "yes"
linelist$Calories_Consumption_Monitoring <- linelist$Calories_Consumption_Monitoring == "yes"
# Save cleaned data to CSV
write.csv(linelist, file = "C:/Users/user/OneDrive - Technische Hochschule Deggendorf/Desktop/linelist.csv", row.names = FALSE)
class(linelist$Smoke)
class(linelist$Transportation_Mode)


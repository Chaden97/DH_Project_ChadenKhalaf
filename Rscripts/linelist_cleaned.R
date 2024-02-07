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
linelist_raw <- import("Dataset.csv")

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
# Convert columns to numeric if they are not already
linelist$Time_Using_Technology_Devices <- as.numeric(as.character(linelist$Time_Using_Technology_Devices))
linelist$Frequency_Physical_Activity <- as.numeric(as.character(linelist$Frequency_Physical_Activity))
linelist$Consumption_Of_Water_Daily <- as.numeric(as.character(linelist$Consumption_Of_Water_Daily))
linelist$Frequency_Vegetables_Consumption <- as.numeric(as.character(linelist$Frequency_Vegetables_Consumption))
linelist$Number_Of_Main_Meals <- as.numeric(as.character(linelist$Number_Of_Main_Meals))

# Round numeric columns
linelist$Time_Using_Technology_Devices <- round(linelist$Time_Using_Technology_Devices, digits = 0)
linelist$Frequency_Physical_Activity <- round(linelist$Frequency_Physical_Activity, digits = 0)
linelist$Consumption_Of_Water_Daily <- round(linelist$Consumption_Of_Water_Daily, digits = 0)
linelist$Frequency_Vegetables_Consumption <- round(linelist$Frequency_Vegetables_Consumption, digits = 0)
linelist$Number_Of_Main_Meals <- round(linelist$Number_Of_Main_Meals, digits = 0)

# Recode categorical variables
linelist$Time_Using_Technology_Devices <- ifelse(linelist$Time_Using_Technology_Devices == 0, "0-2 hours",
                                                 ifelse(linelist$Time_Using_Technology_Devices == 1, "3-5 hours",
                                                        "More than 5 hours"))
linelist$Consumption_Of_Water_Daily <- ifelse(linelist$Consumption_Of_Water_Daily == 1, "Less than 1 litre",
                                              ifelse(linelist$Consumption_Of_Water_Daily == 2, "Between 1 and 2 litres",
                                                     "More than 2 litres"))
linelist$Frequency_Physical_Activity <- ifelse(linelist$Frequency_Physical_Activity == 0, 'No',
                                               ifelse(linelist$Frequency_Physical_Activity == 1, '1 or 2 days',
                                                      ifelse(linelist$Frequency_Physical_Activity == 2, '2 to 4 days',
                                                             '4 or 5 days')))
linelist$Frequency_Vegetables_Consumption <- ifelse(linelist$Frequency_Vegetables_Consumption == 1, 'Never',
                                                    ifelse(linelist$Frequency_Vegetables_Consumption == 2, 'Sometimes',
                                                           'Always'))
linelist$Number_Of_Main_Meals <- ifelse(linelist$Number_Of_Main_Meals == 1, 'One or two',
                                        ifelse(linelist$Number_Of_Main_Meals == 2, 'Three or four',
                                               ifelse(linelist$Number_Of_Main_Meals == 3, 'Four or five', 'More than five')))

# Save cleaned data to CSV
write.csv(linelist, file = "linelist.csv", row.names = FALSE)
class(linelist$Smoke)
class(linelist$Transportation_Mode)


#############################################+
## Data cleaning and prep
## Valentina LM for the CostAction Team
## First draft: 2022-04-25
## Revised: 2024-08-12
#############################################+

## ---- packages ----
# Load necessary libraries
library(readr)      # For reading delimited files
library(tidyverse)      # For data manipulation functions like rename(), mutate(), select(), filter()
library(lubridate)  # For date-time manipulation functions like ymd()
library(jtools)     # For %nin% function

## ---- functions ----
not_all_na <- function(x) any(!is.na(x))
not_any_na <- function(x) all(!is.na(x))
recode_plyr <- function(x) {
  as.numeric(plyr::mapvalues(x, from = c("Strongly disagree", "Disagree",
                                         "Neither agree nor disagree",
                                         "Agree", "Strongly agree"),
                             to = c(1,2,3,4,5)))
  # please note: 'I don't know' and 'N/A' -> NA
}

## ---- data and data tidying ----

# Read the delimited file containing the data
scores_load <- read_delim("data/scores_anonymised.csv")

# Rename columns for easier access and understanding
scores_load <- scores_load %>%
  rename(
    date_time = 'Tijdstempel',  # Rename 'Tijdstempel' to 'date_time'
    technology = 'Select the technology you need to assess'  # Rename technology column
  )

# Create an ID column based on row numbers and add an assessment round column based on date_time
scores_load <- scores_load %>%
  mutate(ID = rownames(.),  # Create an ID column using row numbers
         round = 
           case_when(#between(date_time, ymd('2022-03-28'), ymd('2022-04-27'))
                     # ~ 'Round 1',
                     # between(date_time, ymd('2022-04-28'), ymd('2022-05-05'))
                     # ~ 'Round 2', 
             
                     # between(date_time, ymd('2022-03-28'), ymd('2022-04-27'))
                     # ~ 'Round 1',
                     # between(date_time, ymd('2022-05-06'), ymd('2022-05-10'))
                     # ~ 'Round 3',
                     # TRUE ~ 'Round 2'
                     
                     between(date_time, ymd('2022-03-28'), ymd('2022-04-28'))
                     ~ 'Before', # before the workshop
                     TRUE ~ 'After'))  # Assign round based on date_time ranges
# Relocate the ID and round columns to the beginning
scores_load <- scores_load %>%
  relocate(ID, .before = coder) %>%
  relocate(round, .before = coder)

table(scores_load$round)

# Remove comments column and date_time column
scores_load <- scores_load %>%
  select(-starts_with("Comments"), -date_time) -> scores

# Check the list of unique technologies
distinct(scores["technology"]) -> techs  # Store unique technologies in techs

# Replace empty strings with NA values for better data handling
scores[scores == ""] <- NA  # Convert empty strings to NA

# Check for duplicates based on round, coder, and technology
duplicate_rows <- scores %>%
  group_by(round, coder, technology) %>%
  filter(n() > 1)  # Keep rows where the count of each group is greater than 1
# Extract the ID values of duplicate rows
duplicate_IDs <- duplicate_rows$ID
# Create a new data frame with unique rows based on ID
df_unique <- scores %>% 
  filter(ID %nin% duplicate_IDs)  # Keep rows where ID is not in duplicate_IDs

# Remove specific technologies
drop <- c("Acoustic hardware",
          "3D technology to improve experience")
df_unique <- df_unique %>%
  filter(!technology %in% drop)  # Keep rows where technology is not in the drop list

# Create an empty data frame to store the final result
dat <- data.frame()

# Process each unique technology
for(t in techs$technology){ 
  if(is.na(t)){  # Skip if technology is NA
    next
  }
  
  # Filter df_unique based on current technology
  sub <- df_unique %>% 
    filter(technology == t) %>%
    select(ID,
           round,
           coder,
           technology,
           matches(t))  # Select relevant columns
  
  # Remove columns that are all NA
  sub <- sub %>%
    select(where(~ !all(is.na(.))))
  
  # Rename columns for consistency
  colnames(sub) <- c("ID",
                     "round",
                     "coder",
                     "technology",
                     "audience",
                     "engagement_others",
                     "engagement_feedback",
                     "application",
                     "new_data",
                     "extend_data",
                     "improve_quality",
                     "improve_flow",
                     "improve_curation")
  
  # Append the current technology's data to the final data frame
  if(nrow(dat) == 0){
    dat <- sub
  }else{
    dat <- rbind(dat, sub)
  }
}


## ---- data: recode to numeric ----
# Apply the custom recode_plyr function to the selected columns (audience to improve_curation)
# This function is expected to convert categorical responses to numeric values
sapply(select(dat, audience:improve_curation), recode_plyr) -> scores_rec

# Note: The warning is likely due to the presence of 'N/A' and 'I don't know' responses,
# which are not coded in the recode_plyr function and are thus set to NA.

# Combine the ID, round, coder, and technology columns from the original data (dat)
# with the recoded numeric scores (scores_rec) into a new data frame called scores_num
bind_cols(select(dat, ID:technology), as.data.frame(scores_rec)) -> scores_num

## ---- data: long formats ----
# Convert the wide format data frame (dat) into a long format
# This transformation will create two new columns: 'criterion' for the original column names
# and 'rank' for the corresponding values
dat %>% 
  pivot_longer(
    cols = audience:improve_curation,  # Specify the range of columns to pivot
    names_to = "criterion",              # New column to hold the names of the original columns
    values_to = "rank"                   # New column to hold the values from the original columns
  ) -> scores_tidy_long  # Store the long format data in scores_tidy_long

# Convert the wide format data frame (scores_num) into a long format
# This transformation will create two new columns: 'criterion' for the original column names
# and 'rank' for the corresponding values
scores_num %>% 
  pivot_longer(
    cols = audience:improve_curation,  # Specify the range of columns to pivot
    names_to = "criterion",              # New column to hold the names of the original columns
    values_to = "rank"                   # New column to hold the values from the original columns
  ) -> scores_num_long  # Store the long format data in scores_num_long



## ---- Clean working environment ----
# Specify the objects to keep
objects_to_keep <- c("dat", "scores_num", 
                     "scores_tidy_long", "scores_num_long")  # Replace with your actual object names

# Remove all objects except those specified
rm(list = setdiff(ls(), objects_to_keep))

# Optionally, confirm which objects remain in the workspace
remaining_objects <- ls()
cat("Remaining objects in the workspace:", paste(remaining_objects, collapse = ", "), "\n")

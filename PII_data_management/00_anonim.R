#############################################+
## Anonymization of data
## Valentina LM for the CostAction Team
## First draft: 2022-05-08
#############################################+

## ---- packages ----
library(tidyverse)
library(anonymizer) # installed from github
library(writexl)

## ---- data anonymization ----

# check names
scores <- readxl::read_xlsx("Data/2022-05-08-OriginalData.xlsx") # all data
sort(scores$`E-mailadres`) # a few names are not in the right order
scores$`E-mailadres`[scores$`E-mailadres` == "Elena.tricarico@unifi.it"] <- "elena.tricarico@unifi.it"
scores$`E-mailadres`[scores$`E-mailadres` == "Maarten.degroot@gozdis.si"] <- "maarten.degroot@gozdis.si" 
scores$`E-mailadres`[scores$`E-mailadres` == "Hele@ceh.ac.uk"] <- "hele@ceh.ac.uk"
sort(scores$`E-mailadres`) # check, ok, order is correct now

# remove first experimental attempts
scores_selected <- scores_complete[8:dim(scores_complete)[1],] # exclude first 7 obs, attempts made by the team to test the survey

# anonymization
scores_selected %>% 
  mutate(coder = anonymize(.x = `E-mailadres`, .algo = "crc32", .seed = 1977)) %>% 
  relocate(coder, .after = `E-mailadres`) -> coders_translation_table
coders_translation_table %>% 
  select(-`E-mailadres`) -> scores_anonymsed

## ---- output ----
## individuals are no longer identifiable by their true names
write.csv(coders_translation_table, "coders_translation_table.csv")
write_xlsx(coders_translation_table, "coders_translation_table.xlsx")
write.csv(scores_anonymsed, "scores_anonymised.csv")
write_xlsx(scores_anonymsed, "scores_anonymised.xlsx")





#############################################+
## Anonymization of data
## Valentina LM for the COST Action Alien CSI
## First draft: 2022-05-08
## Revised: 2024-05-15
#############################################+

## ---- packages ----
library(tidyverse)
# devtools::install_github("paulhendricks/anonymizer")
library(anonymizer) # installed from github
library(writexl)

# ## ---- data info ----
# scores_round1 <- read_delim("round1/output/assessments_1stRound.csv")[,-1] # before the workshop
# scores_round2 <- read_delim("round2/output/workshop_individual_ass.csv")[,-1] # all individual assessments during the workshop
# # fino a 5/5/2022 15:24:06 sono individual assessments
# scores_round3 <- read_delim("data/workshop_3round_data.csv")[,-1]
# scores_round3 <- read_delim("data/2022-05-06-subset.csv")[,-1]
# scores_round2$date_time %in% scores_round1$date_time # 300 uguali, 1st round + 2nd round

## ---- data anonymization ----

# check names
scores <- readxl::read_xlsx("00_preliminary_steps/PII_data_management/2022-05-08-OriginalData.xlsx") # all data
sort(scores$`E-mailadres`) # a few names are not in the right order
scores$`E-mailadres`[scores$`E-mailadres` == "Elena.tricarico@unifi.it"] <- "elena.tricarico@unifi.it"
scores$`E-mailadres`[scores$`E-mailadres` == "Maarten.degroot@gozdis.si"] <- "maarten.degroot@gozdis.si" 
scores$`E-mailadres`[scores$`E-mailadres` == "Hele@ceh.ac.uk"] <- "hele@ceh.ac.uk"
sort(scores$`E-mailadres`) # check, ok, order is correct now

# remove first experimental attempts
scores_selected <- scores[8:dim(scores)[1],] # exclude first 7 obs, attempts made by the team to test the survey

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
write.csv(scores_anonymsed, "data/scores_anonymised.csv")
write_xlsx(scores_anonymsed, "data/scores_anonymised.xlsx")





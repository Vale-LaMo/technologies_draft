# technologies
Analysis for horizon scan of novel technologies for IAS citizen science

## survey.R
This code performs a random assignment of 10 technologies per assessor and
taking into account that each technology is evaluated by exactly 10
assessors
Results are saved into 'assignments.csv'
Please note this data is not anonymized

## PII_data_management
This folder contains scripts and data used for the anonymization process.
Please note this data is not anonymized, only the anonymized output "scores_anonymised.csv" and "scores_anonymised.xlsx" will be shared in the data folder as a starting point for the subsequent analyses.
The coders translation table provides the link between the ID of the assessors (coder field) and the names (E-mailadres field).

## Scripts
This folder contains the main scripts for the analyses.
00_datacleaning.R : reads the anonymised data and performs some data cleaning and tidying - the results are stored in R objects used by subsequent scripts (the datacleaning must re-run again to produce those objects) - CHANGE THIS TO SAVE THE OUTPUT AS A BACKUP
01_dataexploration.R : WORK IN PROGRESS

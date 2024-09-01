# technologies
Analysis for horizon scan of novel technologies for IAS citizen science

## Scripts
This folder contains the main scripts for the analyses (note that the preliminary steps to anonymise data are .gitignored!)

### Data cleaning
00_datacleaning.R : reads the anonymised data and performs some data cleaning and tidying - the results are stored in R objects used by subsequent scripts, and saved in the output folder:

- dat.RData : original assessments, anonimysed, in wide format
- scores_num.RData : original assessments, anonimysed, in wide format but converted to numeric
- scores_num_long.RData : original assessments, anonimysed, converted to numeric and in long format
- scores_tidy_long.RData : original assessments, anonimysed, in long format 

Assessments were collected before the workshop discussion (individual assessments - each coder rated a certain number of technologies without consultation with others), and after a panel discussion about each technology (consensus building phase). The data are thus divided into 2 rounds (before and after the workshop).

### Data exploration
01_dataexploration.R : performs exploratory data analyses on the technology assessments dataset, focusing on the number of coders and responses indicating uncertainty (e.g., "I don't know" or "N/A"). 

1. **Assessors per Technology**: 
   - The code groups the dataset by technology and counts the number of coders for each technology.
   - It generates a bar plot to visualize the number of coders, using a color gradient to represent the count.

2. **Counting Uncertainty Responses**: 
   - It counts occurrences of "I don't know" and "N/A" responses for each technology and criterion.
   - The total counts are summarized and visualized using bar plots, including a stacked bar plot to show detailed counts by rank.

3. **Basic Statistics Calculation**: 
   - The code calculates basic statistics (minimum, maximum, mean, median, quartiles, and mode) for each criterion and by technology.
   - It nests the data by round to perform similar analyses for each assessment round.

4. **Visualizations**: 
   - The code generates various plots, including violin plots to show the distribution of ranks by criterion and balloon plots for frequency of ranks.
   - It allows for saving the plots to files for further use.
   - The last plots provide a comparison between rounds for each technology


### Agreement metrics

02_agreement_metrics: calculates various metrics of agreement between experts for different technologies. Here's a breakdown of what the code does:

1. **Load required packages**: The code loads the irr package for inter-rater reliability calculations and the lme4 package for linear mixed-effects models.

2. **Set the Round variable**: The code allows you to calculate metrics for different rounds (Before, After) or for all assessments (All).

3. **Calculate agreement metrics per technology**: The code iterates through each unique technology in the scores_num data frame. For each technology, it creates a scores matrix by selecting relevant columns from the scores_num data frame based on the Round variable. 
It calculates the following metrics:
  - Fleiss' kappa: A measure of inter-rater agreement for multiple raters. The code checks for NAs and calculates the kappa value, z-score, and p-value.
  - Intraclass correlation coefficient (ICC): A measure of reliability. The code checks for NAs and calculates the ICC value, lower bound, and upper bound.
  - Krippendorff's alpha: A measure of inter-rater reliability. The code checks for NAs and calculates the alpha value and the number of raters.
  - The code also performs an ANOVA test to check for significant differences between coders for each criterion. It fits a linear mixed-effects model and performs the ANOVA test. If the F-value is significant (p < 0.05), it stores "< 0.05" in the p_anova vector.

4. **Create the irr_table**: The code creates a data frame irr_table_temp with the calculated metrics for each technology. It then joins this table with the summary_coders_tech data frame to add the number of coders per technology.

5. **Save the irr_table**: Finally, the code saves the irr_table as a CSV file in the output directory. The file name depends on the value of the Round variable.

The code provides a comprehensive analysis of agreement between experts for different technologies, allowing to assess the reliability of the assessments and identify any significant differences between coders.


#### NMDS
**not revised yet*

#############################################+
## Agreement metrics - comparison between rounds
## Valentina LM for the CostAction Team
## First draft: 2022-04-25
## First revision: 2024-08-26
## Revised: 2024-10-28
#############################################+

##---- Setup ----
# Load necessary packages
library(MASS)         # For Box-Cox transformation
library(dplyr)        # For data manipulation
library(tidyr)        # For pivoting data
library(emmeans)      # For estimated marginal means and contrasts
library(lme4)         # For mixed-effects models
library(ggplot2)      # For data visualization
library(robustlmm)    # For robust regression

### ---- Combine the data frames ---
# irr_table_before <- read.csv("output/irr_table_Before_20241025.csv")
# irr_table_after <- read.csv("output/irr_table_After_20241025.csv")
# irr_table_all <- read.csv("output/irr_table_pooled_20241025.csv")
irr_table_before <- read.csv("output/irr_table_Before_20250217.csv")
irr_table_after <- read.csv("output/irr_table_After_20250217.csv")

irr_table_combined <- bind_rows(
  mutate(irr_table_before, Round = "Before"),
  mutate(irr_table_after, Round = "After")#,
  # mutate(irr_table_all, Round = "All")
)
irr_table_combined$Round <- factor(irr_table_combined$Round, levels = c("Before", "After"))

##---- Data Exploration ----
# Plot histograms to check distribution of the metrics
hist(irr_table_combined$kripp, main = "Krippendorff's Alpha")  # Distribution looks normal
hist(irr_table_combined$fleiss, main = "Fleiss' Kappa")        # Distribution looks normal
hist(irr_table_combined$icc, main = "ICC")                     # Distribution is skewed

# Check correlations between metrics
cor(irr_table_combined$icc, irr_table_combined$kripp, use = "complete.obs")
cor(irr_table_combined$icc, irr_table_combined$fleiss, use = "complete.obs")
cor(irr_table_combined$fleiss, irr_table_combined$kripp, use = "complete.obs")
# Result: relatively high correlation between metrics

##---- Reshape Data for Mixed-Effects Modeling ----
# Convert to long format for easier modeling with ICC transformed variable
irr_table_long <- irr_table_combined %>%
  dplyr::select(technology, icc, kripp, fleiss, Round) %>%
  pivot_longer(cols = c("icc", "kripp", "fleiss"))

##---- Mixed-Effects Modeling for Round Effect on Each Metric ----
# Loop through each metric to fit separate models
metrics <- c("icc", "kripp", "fleiss")
pairwise_comparisons <- list()
emm_df <- list()
model <- list()

for (i in seq_along(metrics)) {
  # Fit the mixed model for each metric
  model[[i]] <- rlmer(value ~ Round + (1 | technology),
                      data = filter(irr_table_long, name == metrics[i]))
  # Calculate estimated marginal means (EMM) for each Round
  emm <- emmeans(model[[i]], ~ Round)
  # Perform pairwise comparisons between Rounds
  pairwise_comparisons[[i]] <- contrast(emm, method = "pairwise")
  # Convert EMM results to data frame for plotting
  emm_df[[i]] <- as.data.frame(emm)
}
pairwise_comparisons  # View results of pairwise comparisons
# Summary of models
summary(model[[1]])
summary(model[[2]])
summary(model[[3]])

# Plot for ICC (metric = 1)
i <- 1
ggplot(emm_df[[i]], aes(x = Round, y = emmean, color = Round)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) +
  labs(title = paste("Estimated Marginal Means of ", metrics[i]," by Round",sep=""),
       x = "Round",
       y = "Estimated Value") +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_text(data = as.data.frame(pairwise_comparisons[[i]]),
            aes(x = 1.5, y = max(emm_df[[i]]$emmean) + 0.2,
                label = ifelse(p.value < 0.05, "*", "")),
            color = "red", size = 5)

# Plot for Krippendorf (metric = 2)
i <- 2
ggplot(emm_df[[i]], aes(x = Round, y = emmean, color = Round)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) +
  labs(title = paste("Estimated Marginal Means of ", metrics[i]," by Round",sep=""),
       x = "Round",
       y = "Estimated Value") +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_text(data = as.data.frame(pairwise_comparisons[[i]]),
            aes(x = 1.5, y = max(emm_df[[i]]$emmean) + 0.2,
                label = ifelse(p.value < 0.05, "*", "")),
            color = "red", size = 5)

# Plot for ICC (metric = 3)
i <- 3
ggplot(emm_df[[i]], aes(x = Round, y = emmean, color = Round)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) +
  labs(title = paste("Estimated Marginal Means of ", metrics[i]," by Round",sep=""),
       x = "Round",
       y = "Estimated Value") +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_text(data = as.data.frame(pairwise_comparisons[[i]]),
            aes(x = 1.5, y = max(emm_df[[i]]$emmean) + 0.2,
                label = ifelse(p.value < 0.05, "*", "")),
            color = "red", size = 5)


##---- 'Experimentwise' Model ----
# results showed in the paper

robust_model <- rlmer(value ~ Round*name + (1 | technology), data = irr_table_long) # + (1 | name)
# the interaction Round*name allows the effect of Round to differ across the three metrics

# Since the primary focus of this analysis is to assess the consistency of ratings within each round (Before vs. After),
# individual assessor variability was not explicitly modeled.
# Instead, the fixed effect of Round was included to compare the ICC values between the two rounds,
# while variability between technologies was accounted for by including technology as a random effect.
# This approach simplifies the model by avoiding the inclusion of individual assessors,
# which is justified by the absence of specific rater-related hypotheses in the current analysis.
# Also, recall that we had to stick to ICC1 (oneway) rather than also testing assessors effect, because we do not have
# a fully crossed design
# See also end of 02_agreement_metrics.R for a check

# Summarize the model results
summary(robust_model)

# Calculate estimated marginal means for each 'Round' within each 'name'
emm_results <- emmeans(robust_model, ~ Round | name)

# Convert the results to a data frame
emm_df <- as.data.frame(emm_results)
pairwise_comparisons <- contrast(emm_results, method = "pairwise")
pairwise_comparisons

# Plot the estimated marginal means with ggplot2
ggplot(emm_df, aes(x = Round, y = emmean, color = Round)) +
  geom_point(size = 3) +                           # Plot estimated means as points
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) +  # Error bars for confidence intervals
  facet_wrap(~ name) +                             # Separate plots by each name
  labs(title = "Estimated Marginal Means of Value by Round and Name",
       x = "Round",
       y = "Estimated Value") +
  theme_minimal() +                                # Use a minimal theme for a cleaner look
  theme(legend.position = "none")                  # Remove legend if not needed


#############################################+
## Agreement metrics - comparison
## Valentina LM for the CostAction Team
## First draft: 2022-04-25
## Revised: 2024-08-26
#############################################+

### ---- Load Required Packages ----
library(dplyr)
library(ggplot2)
library(hrbrthemes)
library(viridis)

### ---- Combine the data frames ---
irr_table_before <- read.csv("output/irr_table_Before_20240826.csv")
irr_table_after <- read.csv("output/irr_table_After_20240826.csv")
# irr_table_all <- read.csv("output/irr_table_pooled_20240826.csv")

irr_table_combined <- bind_rows(
  mutate(irr_table_before, Round = "Before"),
  mutate(irr_table_after, Round = "After")#,
  # mutate(irr_table_all, Round = "All")
)
irr_table_combined$Round <- factor(irr_table_combined$Round, levels = c("Before", "After"))


### ---- ICC Plots ----

# Create a boxplot for ICC values
ggplot(irr_table_combined, aes(x = Round, y = icc, col = Round)) +
  geom_boxplot() +
  labs(title = "Comparison of ICC Values Across Assessment Rounds",
       x = "Assessment Round",
       y = "Intraclass Correlation Coefficient (ICC)") +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(irr_table_combined, aes(x = Round, y = icc, col = Round)) +
  geom_boxplot(outlier.shape = NA) +  # Hide outliers in boxplot
  geom_jitter(width = 0.2, alpha = 0.5) +  # Add individual points
  labs(title = "Agreement Across Assessment Rounds",
       x = "Assessment Round",
       y = "Intraclass Correlation Coefficient (ICC)") +
  theme_ipsum_ps(axis_title_size = 10, axis = TRUE) +
  theme(legend.position = "none") +
  ylim(c(0,1)) +
  scale_color_viridis(discrete = TRUE, option = "H", begin = 0, end = 0.8) -> icc_plot
icc_plot


# # Calculate mean and standard deviation for each round
# summary_stats <- irr_table_combined %>%
#   group_by(Round) %>%
#   summarise(mean_icc = mean(icc, na.rm = TRUE),
#             sd_icc = sd(icc, na.rm = TRUE))

# # Create a bar plot with error bars
# ggplot(summary_stats, aes(x = Round, y = mean_icc, fill = Round)) +
#   geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
#   geom_errorbar(aes(ymin = mean_icc - sd_icc, ymax = mean_icc + sd_icc), width = 0.2) +
#   labs(title = "Mean ICC Values with Standard Deviation",
#        x = "Assessment Round",
#        y = "Mean Intraclass Correlation Coefficient (ICC)") +
#   theme_minimal() +
#   theme(legend.position = "none")


### ---- Krippendorff Plots ----

# Create a boxplot for Krippendorff's alpha values
ggplot(irr_table_combined, aes(x = Round, y = kripp, col = Round)) +
  geom_boxplot(outlier.shape = NA) +  # Hide outliers in boxplot
  geom_jitter(width = 0.2, alpha = 0.5) +  # Add individual points
  labs(title = "Agreement Across Assessment Rounds",
       x = "Assessment Round",
       y = "Krippendorff's Alpha") +
  theme_ipsum_ps(axis_title_size = 10, axis = TRUE) +
  theme(legend.position = "none") +
  # ylim(c(-0.1,0.2)) +
  scale_color_viridis(discrete = TRUE, option = "H", begin = 0, end = 0.8) -> kripp_plot
kripp_plot

# # Calculate mean and standard deviation for each round
# summary_stats <- irr_table_combined %>%
#   group_by(Round) %>%
#   summarise(mean_kripp = mean(kripp, na.rm = TRUE),
#             sd_kripp = sd(kripp, na.rm = TRUE))
# 
# # Create a bar plot with error bars
# ggplot(summary_stats, aes(x = Round, y = mean_kripp, fill = Round)) +
#   geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
#   geom_errorbar(aes(ymin = mean_kripp - sd_kripp, ymax = mean_kripp + sd_kripp), width = 0.2) +
#   labs(title = "Mean Krippendorff's Alpha with Standard Deviation",
#        x = "Assessment Round",
#        y = "Mean Krippendorff's Alpha") +
#   theme_minimal() +
#   theme(legend.position = "none")


### ---- Fleiss Plots ----

# Create a boxplot for Fleiss' kappa
ggplot(irr_table_combined, aes(x = Round, y = fleiss, colour = Round)) +
  geom_boxplot(outlier.shape = NA) +  # Hide outliers in boxplot
  geom_jitter(width = 0.2, alpha = 0.5) +  # Add individual points
  labs(title = "Agreement Across Assessment Rounds",
       x = "Assessment Round",
       y = "Fleiss' Kappa") +
  # theme_minimal() +
  theme_ipsum_ps(axis_title_size = 10, axis = TRUE) +
  theme(legend.position = "none") +
  ylim(c(-0.1,0.2)) +
  scale_color_viridis(discrete = TRUE, option = "H", begin = 0, end = 0.8) -> Fleiss_plot
Fleiss_plot

# tiff("figs/icc_plot.tiff",
#      height = 14, width = 20, units = "cm", res = 300, compression = "lzw",
#      pointsize = 8)
# icc_plot
# dev.off()
# tiff("figs/Kripp_plot.tiff",
#      height = 14, width = 20, units = "cm", res = 300, compression = "lzw",
#      pointsize = 8)
# kripp_plot
# dev.off()
# tiff("figs/Fleiss_plot.tiff",
#      height = 14, width = 20, units = "cm", res = 300, compression = "lzw",
#      pointsize = 8)
# Fleiss_plot
# dev.off()



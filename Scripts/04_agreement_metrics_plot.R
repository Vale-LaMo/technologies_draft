#############################################+
## Agreement metrics - comparison
## Valentina LM for the CostAction Team
## First draft: 2022-04-25
## First revision: 2024-08-26
## Revised: 2024-10-28
#############################################+

### ---- Load Required Packages ----
library(dplyr)
library(ggplot2)
library(hrbrthemes)
library(viridis)

### ---- Depends on previous script ---
source("Scripts/03_agreement_metrics_regressionmodels.R")

##---- Set limits for the y-axis ----
ymin = -0.5
# ymax = 1

### ---- ICC Plots ----

# Create a boxplot for ICC values - it's the baseline plot, showing also the outliers
ggplot(irr_table_combined, aes(x = Round, y = icc, col = Round)) +
  geom_boxplot() +
  labs(#title = "Comparison of ICC Values Across Assessment Rounds",
       x = "Assessment Round",
       y = "Intraclass Correlation Coefficient (ICC)") +
  theme_minimal() +
  theme(legend.position = "none")

# Improved plot (removed outliers, used viridis for colors)
ggplot(irr_table_combined, aes(x = Round, y = icc, col = Round)) +
  geom_boxplot(outlier.shape = NA) +  # Hide outliers in boxplot
  geom_jitter(width = 0.2, alpha = 0.5) +  # Add individual points
  labs(#title = Agreement Across Assessment Rounds",
       x = "Assessment Round",
       y = "Intraclass Correlation Coefficient (ICC)") +
  theme_ipsum_ps(axis_title_size = 10, axis = TRUE) +
  theme(legend.position = "none") +
  ylim(c(ymin,1)) +
  scale_color_viridis(discrete = TRUE, option = "H", begin = 0, end = 0.8) -> icc_plot
icc_plot


## Add thresholds
# Choose colors from the "H" viridis palette for threshold lines
threshold_colors <- viridis(5, option = "H", begin = 0, end = 0.8)

ggplot(irr_table_combined, aes(x = Round, y = icc, col = Round)) +
  geom_boxplot(outlier.shape = NA) +  # Hide outliers in boxplot
  geom_jitter(width = 0.2, alpha = 0.5) +  # Add individual points
  labs(x = "Assessment Round",
       y = "Intraclass Correlation Coefficient (ICC)") +
  # theme_ipsum_ps(axis_title_size = 10, axis = TRUE) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "none") +
  ylim(c(ymin, 1)) +
  scale_color_viridis(discrete = TRUE, option = "H", begin = 0, end = 0.8) +
  
  # Add reference lines for ICC thresholds with colors from viridis palette
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey") +
  geom_hline(yintercept = 0.4, linetype = "dashed", color = "darkgrey") +  # Poor-Fair
  geom_hline(yintercept = 0.6, linetype = "dashed", color = "darkgrey") +  # Fair-Good
  geom_hline(yintercept = 0.75, linetype = "dashed", color = "darkgrey") +  # Good-Excellent
  # geom_hline(yintercept = 1, linetype = "dashed", color = threshold_colors[5]) +  
  
  # Add annotations on the right side of the plot
  annotate("text", x = max(as.numeric(irr_table_combined$Round)) + 0.6, y = -0.25, label = "No consistency", color = threshold_colors[1], hjust = 1, cex = 3) +
  annotate("text", x = max(as.numeric(irr_table_combined$Round)) + 0.6, y = 0.2, label = "Poor", color = threshold_colors[1], hjust = 1, cex = 3) +
  annotate("text", x = max(as.numeric(irr_table_combined$Round)) + 0.6, y = 0.50, label = "Fair", color = threshold_colors[1], hjust = 1, cex = 3) +
  annotate("text", x = max(as.numeric(irr_table_combined$Round)) + 0.6, y = 0.68, label = "Good", color = threshold_colors[1], hjust = 1, cex = 3) +
  annotate("text", x = max(as.numeric(irr_table_combined$Round)) + 0.6, y = 0.92, label = "Excellent", color = threshold_colors[1], hjust = 1, cex = 3) -> icc_plot

icc_plot

# # Calculate mean and standard deviation for each round
(summary_stats <- irr_table_combined %>%
  group_by(Round) %>%
  summarise(mean_icc = mean(icc, na.rm = TRUE),
            sd_icc = sd(icc, na.rm = TRUE),
            median_icc = median(icc, na.rm = TRUE),
            iqr_icc = IQR(icc, na.rm = T)))

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
  labs(#title = Agreement Across Assessment Rounds",
       x = "Assessment Round",
       y = "Krippendorff's Alpha") +
  theme_ipsum_ps(axis_title_size = 10, axis = TRUE) +
  theme(legend.position = "none") +
  ylim(c(ymin,1)) +
  scale_color_viridis(discrete = TRUE, option = "H", begin = 0, end = 0.8) -> kripp_plot
kripp_plot

# paper plot
ggplot(irr_table_combined, aes(x = Round, y = kripp, col = Round)) + 
  geom_boxplot(outlier.shape = NA) +  # Hide outliers in boxplot
  geom_jitter(width = 0.2, alpha = 0.5) +  # Add individual points
  labs(x = "Assessment Round",
       y = "Krippendorff's Alpha") +
  # theme_ipsum_ps(axis_title_size = 10, axis = TRUE) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "none") +
  scale_y_continuous(limits = c(ymin, 1)) +  # Set y-axis limits explicitly
  
  # Use the Viridis color scale for points
  scale_color_viridis(discrete = TRUE, option = "H", begin = 0, end = 0.8) +
  
  # Add reference lines for thresholds with dark grey color
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey") + 
  geom_hline(yintercept = 0.67, linetype = "dashed", color = "darkgrey") +
  geom_hline(yintercept = 0.79, linetype = "dashed", color = "darkgrey") +
  
  # Add annotations for thresholds on the right side
  annotate("text", x = max(as.numeric(irr_table_combined$Round)) + 0.6, y = -0.25, label = "Disagreement", color = threshold_colors[1], hjust = 1, cex = 3) +
  annotate("text", x = max(as.numeric(irr_table_combined$Round)) + 0.6, y = 0.67/2, label = "Poor", color = threshold_colors[1], hjust = 1, cex = 3) +
  annotate("text", x = max(as.numeric(irr_table_combined$Round)) + 0.6, y = 0.73, label = "Moderate", color = threshold_colors[1], hjust = 1, cex = 3) +
  # annotate("text", x = max(as.numeric(irr_table_combined$Round)) + 0.6, y = 0.68, label = "Reliable", color = "darkgrey", hjust = 1, cex = 3) +
  annotate("text", x = max(as.numeric(irr_table_combined$Round)) + 0.6, y = 0.92, label = "Reliable", color = threshold_colors[1], hjust = 1, cex = 3) -> kripp_plot

kripp_plot


# # Calculate mean and standard deviation for each round
(summary_stats <- irr_table_combined %>%
    group_by(Round) %>%
    summarise(mean_kripp = mean(kripp, na.rm = TRUE),
              sd_kripp = sd(kripp, na.rm = TRUE),
              median_kripp = median(kripp, na.rm = TRUE),
              iqr_kripp = IQR(kripp, na.rm = T)))
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
  labs(#title = "Agreement Across Assessment Rounds",
       x = "Assessment Round",
       y = "Fleiss' Kappa") +
  # theme_minimal() +
  theme_ipsum_ps(axis_title_size = 10, axis = TRUE) +
  theme(legend.position = "none") +
  ylim(c(ymin,1)) +
  scale_color_viridis(discrete = TRUE, option = "H", begin = 0, end = 0.8) -> fleiss_plot
fleiss_plot

# paper plot
ggplot(irr_table_combined, aes(x = Round, y = fleiss, col = Round)) + 
  geom_boxplot(outlier.shape = NA) +  # Hide outliers in boxplot
  geom_jitter(width = 0.2, alpha = 0.5) +  # Add individual points
  labs(x = "Assessment Round",
       y = "Fleiss' Kappa") +
  # theme_ipsum_ps(axis_title_size = 10, axis = TRUE) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "none") +
  # ylim(c(0,1)) +
  scale_y_continuous(limits = c(ymin, 1)) +  # Set y-axis limits explicitly
  
  # Use the Viridis color scale for points
  scale_color_viridis(discrete = TRUE, option = "H", begin = 0, end = 0.8) +
  
  # Add reference lines for thresholds with dark grey color
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey") + 
  geom_hline(yintercept = 0.2, linetype = "dashed", color = "darkgrey") +  
  geom_hline(yintercept = 0.4, linetype = "dashed", color = "darkgrey") + 
  geom_hline(yintercept = 0.6, linetype = "dashed", color = "darkgrey") + 
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "darkgrey") + 
  
  # Add annotations for thresholds on the right side
  annotate("text", x = max(as.numeric(irr_table_combined$Round)) + 0.6, y = -0.25, label = "Poor", color = threshold_colors[1], hjust = 1, cex = 3) +
  annotate("text", x = max(as.numeric(irr_table_combined$Round)) + 0.6, y = 0.1, label = "Slight", color = threshold_colors[1], hjust = 1, cex = 3) +
  annotate("text", x = max(as.numeric(irr_table_combined$Round)) + 0.6, y = 0.3, label = "Fair", color = threshold_colors[1], hjust = 1, cex = 3) +
  annotate("text", x = max(as.numeric(irr_table_combined$Round)) + 0.6, y = 0.5, label = "Moderate", color = threshold_colors[1], hjust = 1, cex = 3) +
  annotate("text", x = max(as.numeric(irr_table_combined$Round)) + 0.6, y = 0.7, label = "Substantial", color = threshold_colors[1], hjust = 1, cex = 3) +
  annotate("text", x = max(as.numeric(irr_table_combined$Round)) + 0.6, y = 0.9, label = "Almost perfect", color = threshold_colors[1], hjust = 1, cex = 3) -> fleiss_plot

fleiss_plot

# # Calculate mean and standard deviation for each round
(summary_stats <- irr_table_combined %>%
    group_by(Round) %>%
    summarise(mean_fleiss = mean(fleiss, na.rm = TRUE),
              sd_fleiss = sd(fleiss, na.rm = TRUE),
              median_fleiss = median(fleiss, na.rm = TRUE),
              iqr_fleiss = IQR(fleiss, na.rm = T)))

# # Uncomment to save the plots
# tiff("figs/icc_plot.tiff",
#      height = 14, width = 20, units = "cm", res = 300, compression = "lzw",
#      pointsize = 8)
# icc_plot
# dev.off()
# tiff("figs/kripp_plot.tiff",
#      height = 14, width = 20, units = "cm", res = 300, compression = "lzw",
#      pointsize = 8)
# kripp_plot
# dev.off()
# tiff("figs/fleiss_plot.tiff",
#      height = 14, width = 20, units = "cm", res = 300, compression = "lzw",
#      pointsize = 8)
# fleiss_plot
# dev.off()


# # Uncomment to save the plots. NEW PLOTS 20250217!
# tiff("figs/icc_plot_20250217.tiff",
#      height = 14, width = 20, units = "cm", res = 300, compression = "lzw",
#      pointsize = 8)
# icc_plot
# dev.off()
# tiff("figs/kripp_plot_20250217.tiff",
#      height = 14, width = 20, units = "cm", res = 300, compression = "lzw",
#      pointsize = 8)
# kripp_plot
# dev.off()
# tiff("figs/fleiss_plot_20250217.tiff",
#      height = 14, width = 20, units = "cm", res = 300, compression = "lzw",
#      pointsize = 8)
# fleiss_plot
# dev.off()


##---- References -----
# Cicchetti, D.V. (1994). Guidelines, criteria, and rules of thumb for evaluating normed and standardized
# assessment instruments in psychology. Psychological Assessment, 6(4):284–290.
# 
# Marzi, G., Balzano, M., Marchiori, D. (2024) K-Alpha Calculator–Krippendorff’s Alpha Calculator: 
# A user-friendly tool for computing Krippendorff’s Alpha inter-rater reliability coefficient. MethodsX, 12: 102545
#
# Landis, J.R., Koch, G.G. (1977). The measurement of observer agreement for categorical data.
# Biometrics, 33 (1): 159–174, doi:10.2307/2529310, JSTOR 2529310, PMID 843571.


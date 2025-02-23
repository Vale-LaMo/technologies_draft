#############################################+
## Boxplots per criterion
## Valentina LM for the CostAction Team
## First draft: 2022-05-04
## Revised: 2024-10-28
#############################################+

## ---- packages ----
library(tidyverse)
library(ggpubr)
library(hrbrthemes)
library(viridis)
library(writexl)
library(extrafont)
library(ggbeeswarm)
library(ggrepel)
library(vegan)
library(factoextra)

load("output/scores_num_long.RData")
load("output/scores_tidy_long.RData")
source("Scripts/01_dataexploration.R")
source("Scripts/05_ranking.R")
source("Scripts/06_nmds_clustering.R")

##---- Criteria contribution ----
criteria_names <- c("Application", "Audience", "Engagement via feedback",
                    "Engagement with others", "Extend data",
                    "Improve data curation", "Improve data flow",
                    "Improve data quality", "New data")
# custom colors from the clustering (same as in 06_nmds_clustering,R but different order)
custom_colors_boxplot <- c("#7A0403FF", "#F8BD39FF", "#F76E1AFF", "#CC2B04FF")
wscr_clusters <- left_join(weighted_scores_criteria_long, site.scrs[,c("Technology","Cluster")], by = c("technology" = "Technology"))


# Preprocess the data to identify the top 3 and bottom 3 for each criteria
label_data <- wscr_clusters %>%
  group_by(criteria) %>%
  arrange(desc(w.scores)) %>%
  slice(c(1:3)) %>%  # Select top 3 
  # slice(c(1:3, (n() - 2):n())) %>%  # Select top 3 and bottom 3
  # slice(c(1:1, (n()):n())) %>%
  ungroup()

# baseline (simple) boxplot on criteria contribution
ggplot(wscr_clusters, aes(y = criteria, x = w.scores)) +
  # geom_boxplot() +
  geom_boxplot(outlier.shape = NA) +  # Boxplots without outliers
  geom_beeswarm(aes(color = technology), dodge.width = 0.75) +
  # geom_beeswarm(size = 0.8, alpha = 0.5, col = criteria_col[2], dodge.width = 0.75) +
  theme_minimal(base_size = 10) +
  scale_fill_viridis() +
  # theme(legend.position = "bottom") +
  # scale_y_discrete(labels = criteria) +
  labs(x = "Weighted score", y = "Criteria")
# geom_text_repel(aes(label = technology), size = 3, max.overlaps = 25) +
# geom_text_repel(data = label_data, aes(label = technology), size = 2.5)
# # Uncomment to save the plot
# ggsave("figs/ranking_additional_plots/criteria_contribution.jpg")

# detailed boxplot on criteria contribution
ggplot(wscr_clusters, aes(y = criteria, x = w.scores)) +
  # geom_boxplot() +
  geom_boxplot(outlier.shape = NA) +  # Boxplots without outliers
  geom_beeswarm(aes(color = technology), dodge.width = 0.75) +
  # geom_beeswarm(size = 0.8, alpha = 0.5, col = criteria_col[2], dodge.width = 0.75) +
  theme_minimal(base_size = 10) +
  scale_fill_viridis() +
  theme(legend.position = "bottom") +
  # scale_y_discrete(labels = criteria) +
  labs(x = "Weighted score", y = "Criteria") +
  # geom_text_repel(aes(label = technology), size = 3, max.overlaps = 25) +
  geom_text_repel(data = label_data, aes(label = technology), size = 2.5)
# # Uncomment to save the plot
# ggsave("figs/ranking_additional_plots/criteria_contribution_detailed.jpg")

# detailed boxplot on criteria contribution with colours from clustering
ggplot(wscr_clusters, aes(y = criteria, x = w.scores)) +
  # geom_boxplot() +
  geom_boxplot(outlier.shape = NA) +  # Boxplots without outliers
  geom_beeswarm(aes(color = Cluster), dodge.width = 0.75) +
  # geom_beeswarm(size = 0.8, alpha = 0.5, col = criteria_col[2], dodge.width = 0.75) +
  theme_minimal(base_size = 10) +
  scale_color_manual(values = custom_colors_boxplot) +  # Apply custom colors to clusters
  theme(legend.position = "none") +
  # scale_y_discrete(labels = criteria) +
  labs(x = "Weighted score", y = "Criteria") +
  # geom_text_repel(aes(label = technology), size = 3, max.overlaps = 25) +
  geom_text_repel(data = label_data, aes(label = technology, color = Cluster), size = 2.5)
# # Uncomment to save the plot
# ggsave("figs/ranking_additional_plots/criteria_contribution_clustercolours.jpg")

# def for paper
# detailed boxplot on criteria contribution with colours from clustering
ggplot(wscr_clusters, aes(y = reorder(criteria, w.scores, FUN = median), x = w.scores)) +
  geom_boxplot() +
  # geom_boxplot(outlier.shape = NA) +  # Boxplots without outliers
  geom_beeswarm(aes(color = Cluster), dodge.width = 0.75) +
  # geom_beeswarm(size = 0.8, alpha = 0.5, col = criteria_col[2], dodge.width = 0.75) +
  theme_minimal(base_size = 10) +
  # scale_color_manual(values = custom_colors_boxplot) +  # Apply custom colors to clusters
  scale_fill_viridis(discrete = TRUE, option = "H", begin = 1, end = 0) +
  theme(legend.position = "none") +
  # scale_y_discrete(labels = criteria) +
  labs(x = "Weighted score", y = "Criteria") -> contribution_criteria
  # geom_text_repel(aes(label = technology), size = 3, max.overlaps = 25) +
  # geom_text_repel(data = label_data, aes(label = technology, color = Cluster), size = 2.5)
contribution_criteria
criteria_names <- c("Extend data", "New data", "Improve data quality",
                    "Engagement via feedback", "Audience", "Improve data flow",
                    "Application", "Improve data curation", "Engagement with others")
ggplot(wscr_clusters, aes(y = reorder(criteria, w.scores, FUN = median), x = w.scores)) +
  # geom_boxplot() +
  geom_boxplot(outlier.shape = NA) +  # Boxplots without outliers
  geom_beeswarm(aes(color = Cluster), dodge.width = 0.75) +
  # geom_beeswarm(size = 0.8, alpha = 0.5, col = criteria_col[2], dodge.width = 0.75) +
  theme_minimal(base_size = 10) +
  # scale_color_manual(values = custom_colors_boxplot) +  # Apply custom colors to clusters
  scale_fill_viridis(discrete = TRUE, option = "H", begin = 1, end = 0) +
  theme(legend.position = "none") +
  scale_y_discrete(labels = rev(criteria_names)) +
  labs(x = "Weighted score", y = "Criteria") -> contribution_criteria
# geom_text_repel(aes(label = technology), size = 3, max.overlaps = 25) +
# geom_text_repel(data = label_data, aes(label = technology, color = Cluster), size = 2.5)
contribution_criteria
# # Uncomment to save the plot
# ggsave("figs/ranking_additional_plots/criteria_contribution_clustercolours.jpg")
# 1.3 ratio
tiff("figs/ranking_additional_plots/criteria_contribution_clustercolours.tiff", res=1000, width = 18, height = 14, units = "cm")
print(contribution_criteria)
dev.off()
# ggsave("figs/ranking_additional_plots/criteria_contribution_clustercolours.tiff", plot = final_plot, dpi = 300, width = 22, height = 27, units = "cm")


##---- check if medians (or means) are statistically different
kruskal.test(w.scores ~ criteria, data = wscr_clusters)  # Non-parametric test for medians
library(FSA)  # Load the package
dunn_result <- dunnTest(w.scores ~ criteria, data = wscr_clusters, method = "bonferroni")  # Adjust for multiple comparisons
print(dunn_result)
filter(dunn_result$res, P.adj < 0.05)

# Extract and split significant comparisons
significant_comparisons <- dunn_result$res %>%
  filter(P.adj < 0.05) %>%
  separate(Comparison, into = c("group1", "group2"), sep = " - ") %>%
  select(group1, group2, P.adj)

library(ggsignif)
# Create a list of comparison pairs
comparison_list <- significant_comparisons %>%
  mutate(pair = pmap(list(group1, group2), c)) %>%
  pull(pair)
# Ensure the number of p-values matches the number of comparisons
annotations <- significant_comparisons$P.adj
# Add the comparisons and p-values as annotations, with connecting lines
contribution_criteria_comp <- contribution_criteria +
  geom_signif(
    comparisons = comparison_list,  # List of comparisons
    annotations = round(annotations, 3),      # Adjusted p-values as annotations
    y_position = max(wscr_clusters$w.scores) + seq(0.5, 2, length.out = length(comparison_list)),  
    tip_length = 0.01,
    textsize = 3,
    color = "black",  # Color of the lines and text
    vjust = 0.5      # Adjust vertical positioning of the text
  )
# Plot the result
print(contribution_criteria_comp)

# # anova
# hist(wscr_clusters$w.scores)
# shapiro.test(wscr_clusters$w.scores) # not normal, following stats sensitive to outliers, I prefere KW with Dunn test
# aov_result <- aov(w.scores ~ criteria, data = wscr_clusters)  
# summary(aov_result)  # Parametric test for means
# TukeyHSD(aov_result) -> tukey_result
# significant_pairs <- as.data.frame(tukey_result$criteria) %>%
#   rownames_to_column("Comparison") %>%
#   filter(`p adj` < 0.05)  # Keep only significant results
# print(significant_pairs)


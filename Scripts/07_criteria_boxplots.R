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
  scale_y_discrete(labels = criteria_names) +
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
  scale_y_discrete(labels = criteria_names) +
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
  scale_y_discrete(labels = criteria_names) +
  labs(x = "Weighted score", y = "Criteria") +
  # geom_text_repel(aes(label = technology), size = 3, max.overlaps = 25) +
  geom_text_repel(data = label_data, aes(label = technology, color = Cluster), size = 2.5)
# # Uncomment to save the plot
# ggsave("figs/ranking_additional_plots/criteria_contribution_clustercolours.jpg")

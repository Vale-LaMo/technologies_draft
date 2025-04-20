## ---- packages ----
library(tidyverse)
library(ggpubr)
library(vegan)
library(factoextra)

## ---- data ----
weighted_scores_criteria <- read.csv("output/weighted_scores_criteria.csv")
weighted_scores_criteria %>%
  dplyr::select(-no.coders) -> data_clustering

library(ggfortify)

# Run PCA on your criteria scores
pca_res <- prcomp(data_clustering[, 3:11], scale. = TRUE)

# Visualize the PCA to check groupings
autoplot(pca_res, loadings = TRUE, loadings.label = TRUE)
# Orthogonality in PCA means that the variables (criteria) are uncorrelated in
# the reduced space. If the first two PCs already separate the criteria
# into two distinct groups, it suggests that those two PCs already explain
# enough of the key structure in the data.

# # Uncomment to save the plot
tiff("figs/PCA.tiff",
     height = 20, width = 20*1.365411, units = "cm", res = 300, compression = "lzw",
     pointsize = 6)
autoplot(pca_res, loadings = TRUE, loadings.label = TRUE)
dev.off()

# even if
summary(pca_res) # look like I should use 3/4 PCs
# Compute proportion of variance
var_explained <- summary(pca_res)$importance[2,] 
# Create a scree plot
qplot(y = var_explained, x = seq_along(var_explained)) +
  geom_point(size = 3) +
  geom_line() +
  labs(x = "Principal Component", y = "Variance Explained") +
  theme_minimal()
# elbow at 4


loadings <- as.data.frame(pca_res$rotation)
# first axes positively related to application, audience, engagement (both) and negatively related with all the others
# second axes negatively related with everything, but particularly improve flow, application, improve quality and curation

# Faceted plot for the paper
weighted_scores_criteria_long_facets <- weighted_scores_criteria_long %>%
  mutate(criteria_group = case_when(
    criteria %in% c("application", "audience", "engagement_feedback", "engagement_others") ~ "Engagement and application",
    TRUE ~ "Data improvement"
  ))
weighted_scores_criteria_long_facets$criteria_group = factor(weighted_scores_criteria_long_facets$criteria_group, levels = c("Data improvement", "Engagement and application"))
weighted_scores_criteria_long_facets %>% 
  group_by(criteria_group, technology) %>% 
  mutate(criteria_group_sum = sum(w.scores, na.rm = TRUE)) %>% 
  ungroup() -> weighted_scores_criteria_long_facets


# Plot all Viridis palette colors
# scales::show_col(viridis(13, option = "H"),ncol=9, cex_label = .5)
# viridis(13, option = "H")
# (viridis(30, option = "D"))
# Define colors manually from Viridis shades
custom_colors <- c(
  "new_data" = "#EDD03AFF",
  "improve_quality" = "#FB8022FF",
  "improve_flow" = "#E4460AFF",
  "extend_data" = "#BA1E02FF",
  "improve_curation" = "#7A0403FF",
  "engagement_others"  = "#30123BFF",
  "engagement_feedback" = "#4454C4FF",
  "audience" =  "#4490FEFF",
  "application" = "#1FC8DEFF"
)

# Ensure the dataset uses the correct colors
weighted_scores_criteria_long_facets <- weighted_scores_criteria_long_facets %>%
  mutate(criteria = factor(criteria, levels = names(custom_colors)))

reference_group <- "Data improvement"
reference_group <- "Engagement and application"
ordered_techs <- weighted_scores_criteria_long_facets %>%
  filter(criteria_group == reference_group) %>%
  arrange(criteria_group_sum) %>%
  pull(technology) %>%
  unique()
weighted_scores_criteria_long_facets$technology <- factor(
  weighted_scores_criteria_long_facets$technology,
  levels = ordered_techs
)

ggplot(weighted_scores_criteria_long_facets, aes(x = w.scores, y = technology, fill = criteria)) +
  geom_bar(stat = "identity", width = 0.8) +
# ggplot(weighted_scores_criteria_long_facets, aes(x = w.scores, y = reorder(technology, criteria_group_sum), fill = criteria)) +
#   geom_bar(stat = "identity", width = 0.8) +  # Stacked bar
  scale_fill_manual(values = custom_colors, # Apply custom colors
  # scale_fill_viridis_d(option = "H", begin = 1, end = 0,
                       labels = c("New data", "Improve data quality", "Improve data flow", "Extend data", "Improve data curation",
                       "Engagement with others", "Engagement via feedback", "Audience", "Application")) +
  facet_grid(. ~ criteria_group, scales = "free_x", space = "free_x") +  # Facet by new groups
  theme_minimal(base_size = 10) +
  labs(x = "Score", y = "Technology", fill = "Criteria") +
  theme(
    axis.text.y = element_text(size = 8),
    axis.title.x = element_text(vjust = 1, hjust = 0.5, size = 10),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.text = element_text(size = 8),  # Facet labels
    legend.position = c(0.3, -0.15),
    plot.margin = margin(5.5, 40, 70, 5.5),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  ) +
  guides(fill = guide_legend(nrow = 3)) -> faceted_bar_plot_PCA
faceted_bar_plot_PCA
# ggsave("figs/ranking_additional_plots/faceted_bar_plot_gr.jpg")

cor_table <- weighted_scores_criteria_long_facets %>% 
  dplyr::select(technology, criteria_group, criteria_group_sum) %>% 
  pivot_wider(names_from = criteria_group, values_from = criteria_group_sum, values_fn = mean)
cor(cor_table$`Engagement and application`, cor_table$`Data improvement`, method = c("spearman"))
cor.test(cor_table$`Engagement and application`, cor_table$`Data improvement`, method = c("spearman"))


ggplot(cor_table, aes(x = `Data improvement`, y = `Engagement and application`, label = technology)) +
  geom_point() +
  geom_text(hjust = 1, vjust = 1, size = 2.5) +
  theme_minimal()

model <- lm(`Engagement and application` ~ `Data improvement`, data = cor_table)
summary(model)


# for the tiff image
ggplot(weighted_scores_criteria_long_facets, aes(x = w.scores, y = reorder(technology, sum.scores), fill = criteria)) +
  geom_bar(stat = "identity", width = 0.8) +  # Stacked bar
  scale_fill_manual(values = custom_colors, # Apply custom colors
                    # scale_fill_viridis_d(option = "H", begin = 1, end = 0,
                    labels = c("New data", "Improve data quality", "Improve data flow", "Extend data", "Improve data curation",
                               "Engagement with others", "Engagement via feedback", "Audience", "Application")) +
  facet_grid(. ~ criteria_group, scales = "free_x", space = "free_x") +  # Facet by new groups
  theme_minimal(base_size = 10) +
  labs(x = "Score", y = "Technology", fill = "Criteria") +
  theme(
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(vjust = 1, hjust = 0.5, size = 10),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.text = element_text(size = 10),  # Facet labels
    legend.position = c(0.3, -0.2),
    plot.margin = margin(5.5, 40, 70, 5.5),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  ) +
  guides(fill = guide_legend(nrow = 2)) -> faceted_bar_plot_PCA_tiff



# Faceted plot - NMDS with three groups
weighted_scores_criteria_long_facets <- weighted_scores_criteria_long %>%
  mutate(criteria_group = case_when(
    criteria %in% c("improve_curation") ~ "Curation",
    criteria %in% c("application", "audience", "engagement_feedback", "engagement_others") ~ "Engagement and application",
    TRUE ~ "Data improvement"
  ))
weighted_scores_criteria_long_facets$criteria_group = factor(weighted_scores_criteria_long_facets$criteria_group, levels = c("Data improvement", "Curation", "Engagement and application"))
custom_colors <- c(
  "new_data" = "#EDD03AFF",
  "improve_quality" = "#FB8022FF",
  "improve_flow" = "#D23105FF",
  "extend_data" = "#7A0403FF",
  "improve_curation" = "#404688FF",
  "engagement_others"  = "#30123BFF",
  "engagement_feedback" = "#4454C4FF",
  "audience" =  "#4490FEFF",
  "application" = "#1FC8DEFF"
)

# Ensure the dataset uses the correct colors
weighted_scores_criteria_long_facets <- weighted_scores_criteria_long_facets %>%
  mutate(criteria = factor(criteria, levels = names(custom_colors)))

ggplot(weighted_scores_criteria_long_facets, aes(x = w.scores, y = reorder(technology, sum.scores), fill = criteria)) +
  geom_bar(stat = "identity", width = 0.8) +  # Stacked bar
  scale_fill_manual(values = custom_colors)+ # Apply custom colors
  # scale_fill_viridis_d(option = "H", begin = 1, end = 0,
  # labels = c("Engagement with others"
  # "Application", "Audience", "Engagement via feedback", "Engagement with others", "Improve data curation", "Extend data", "Improve data flow", "Improve data quality", "New data")) +
  facet_grid(. ~ criteria_group, scales = "free_x", space = "free_x") +  # Facet by new groups
  theme_minimal(base_size = 10) +
  labs(x = "Score", y = "Technology", fill = "Criteria") +
  theme(
    axis.text.y = element_text(size = 8),
    axis.title.x = element_text(vjust = 1, hjust = 0.5, size = 10),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.text = element_text(size = 8),  # Facet labels
    legend.position = c(0.4, -0.15),
    plot.margin = margin(5.5, 40, 60, 5.5),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  ) +
  guides(fill = guide_legend(nrow = 3)) -> faceted_bar_plot_gr
faceted_bar_plot_gr

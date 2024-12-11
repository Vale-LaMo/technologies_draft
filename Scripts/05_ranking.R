#############################################+
## Ranking
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

load("output/scores_num_long.RData")
load("output/scores_tidy_long.RData")
source("Scripts/01_dataexploration.R")

## ---- scores tot and per criteria ----
scores_num_long %>%
  group_by(technology) %>%
  summarise(sum.scores = sum(rank, na.rm = TRUE),
            no.coders = length(unique(coder))) -> scores_tech
scores_num_long %>% 
  group_by(technology, criterion) %>% 
  summarise(sum.scores = sum(rank, na.rm = TRUE),
            no.coders = length(unique(coder))) -> scores_tech_criteria
pivot_wider(scores_tech_criteria, names_from = criterion,
            values_from = sum.scores) -> scores_tech_criteria_wide
# Merge on "technology"
dati_wide <- left_join(scores_tech_criteria_wide, scores_tech, by = c("technology","no.coders"))


as.matrix(dati_wide[,3:12]) -> mat
as.numeric(dati_wide$no.coders) -> dev
plyr::aaply(mat, 2, "/", dev) -> weighted.scores # weight the scores based on the number of coders
bind_cols(dati_wide[,1:2],
          as.data.frame(t(weighted.scores))) -> weighted_scores_criteria
# write.csv(weighted_scores_criteria, "output/weighted_scores_criteria.csv")
# write_xlsx(weighted_scores_criteria, "output/weighted_scores_criteria.xlsx")

weighted_scores_criteria %>% 
  pivot_longer(cols = application:new_data,
               names_to = "criteria",
               values_to = "w.scores") -> weighted_scores_criteria_long

##---- ranking plot ----

viridis(1, option = "H", begin = 0.8, end = 0) -> bar_color
weighted_scores_criteria_long %>% 
  ggplot(aes(reorder(technology, sum.scores), sum.scores)) + 
  geom_bar(stat="identity", fill = bar_color) + 
  theme_ipsum_ps(axis_title_size = 10, axis = FALSE, base_family = "IBM Plex Sans SC") +
  coord_flip() + 
  labs(x = "Technology", y = "Sum of scores") +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        #axis.title.x = element_blank(),
        axis.title.x = element_text(vjust = 1, hjust = 0.5),
        axis.text.x = element_blank(), # Simplify x-axis
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "none") -> plot_ranking_base
plot_ranking_base

weighted_scores_criteria_long %>% 
  ggplot(aes(reorder(technology, sum.scores), w.scores, fill=criteria)) + 
  geom_bar(position="stack", stat="identity") + 
  theme_ipsum_ps(axis_title_size = 10, axis = FALSE, base_family = "IBM Plex Sans SC") +
  scale_fill_viridis(discrete = TRUE, option = "H", begin = 1, end = 0,
                     labels = c("Application", "Audience", "Engagement via feedback", "Engagement with others", "Extend data", "Improve data curation", "Improve data flow", "Improve data quality", "New data")) +
  coord_flip() + 
  labs(x = "Technology", y = "Score") +
  theme(legend.position = c(0.4, -0.15),
        plot.margin = margin(5.5, 40, 60, 5.5),
        axis.title.x = element_text(vjust = 1, hjust = 0.5)) +
  guides(fill = guide_legend(nrow = 3)) -> plot_ranking
plot_ranking

# not very good
# weighted_scores_criteria_long %>%
#   ggplot(aes(reorder(technology, sum.scores), w.scores, fill = criteria)) + 
#   geom_bar(position = "dodge", stat = "identity") + # Change position to dodge for grouping
#   theme_ipsum_ps(axis_title_size = 10, axis = FALSE, base_family = "IBM Plex Sans SC") +
#   scale_fill_viridis(discrete = TRUE, option = "H", begin = 1, end = 0,
#                      labels = c("Application", "Audience", "Engagement via feedback", "Engagement with others", 
#                                 "Extend data", "Improve data curation", "Improve data flow", "Improve data quality", "New data")) +
#   coord_flip() + 
#   labs(x = "Technology", y = "Score") +
#   theme(legend.position = "bottom", # Position legend for grouped bars
#         plot.margin = margin(5.5, 40, 60, 5.5),
#         axis.title.x = element_text(vjust = 1, hjust = 0.5)) +
#   guides(fill = guide_legend(nrow = 3)) -> plot_grouped
# plot_grouped

weighted_scores_criteria_long %>%
  ggplot(aes(x = w.scores, y = reorder(technology, sum.scores), color = criteria)) + 
  geom_point(size = 3, alpha = 0.8) + # Dot size and transparency
  scale_color_viridis_d(option = "H", begin = 1, end = 0, 
                        labels = c("Application", "Audience", "Engagement via feedback", 
                                   "Engagement with others", "Extend data", 
                                   "Improve data curation", "Improve data flow", 
                                   "Improve data quality", "New data")) +
  theme_minimal(base_size = 10) +
  labs(x = "Score", y = "Technology", color = "Criteria") +
  theme(axis.text.y = element_text(size = 8),
        axis.title.x = element_text(size = 10),
        legend.position = "bottom",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8)) +
  guides(color = guide_legend(nrow = 3)) -> dot_chart
dot_chart # so cool :) but not so easy to read


weighted_scores_criteria_long %>%
  ggplot(aes(x = criteria, y = reorder(technology, sum.scores), fill = w.scores)) + 
  geom_tile(color = "white") + # Heatmap with tile borders
  scale_fill_viridis(option = "H", begin = 0, end = 1, 
                     name = "Score", na.value = "grey90") + # Viridis color scale for scores
  theme_minimal(base_size = 10) +
  labs(x = "Criteria", y = "Technology") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels for readability
        axis.text.y = element_text(size = 8),
        legend.position = "bottom",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8)) +
  guides(fill = guide_colorbar(barwidth = 10, barheight = 0.5)) -> heatmap_plot
heatmap_plot
# ggsave("figs/ranking_additional_plots/heatmap_plot.jpg")

weighted_scores_criteria_long %>%
  ggplot(aes(x = w.scores, y = reorder(technology, sum.scores), fill = criteria)) + 
  geom_bar(stat = "identity", width = 0.8) + # Bars instead of dots
  scale_fill_viridis_d(option = "H", begin = 1, end = 0, 
                       labels = c("Application", "Audience", "Engagement via feedback", 
                                  "Engagement with others", "Extend data", 
                                  "Improve data curation", "Improve data flow", 
                                  "Improve data quality", "New data")) +
  facet_grid(. ~ criteria, scales = "free_x", space = "free_x") + # Side-by-side facets
  theme_minimal(base_size = 10) +
  labs(x = "Score", y = "Technology", fill = "Criteria") +
  theme(axis.text.y = element_text(size = 8), # Show technology names only once
        axis.title.x = element_text(size = 10),
        axis.text.x = element_blank(), # Remove x-axis text for a cleaner look
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.text = element_text(size = 8), # Smaller facet labels
        legend.position = "bottom", # Move legend below the plot
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8)) +
  guides(fill = guide_legend(nrow = 3)) -> faceted_bar_plot
faceted_bar_plot
# ggsave("figs/ranking_additional_plots/faceted_bar_plot.jpg")

criteria <- sort(unique(weighted_scores_criteria_long$criteria))
unique(bind_rows(ggplot_build(faceted_bar_plot)$data)$fill) -> criteria_col
# viridis_pal(option = "H", begin = 1, end = 0)(length(criteria)) -> criteria_col
for(i in 1:length(criteria)) {
  weighted_scores_criteria_long %>%
    filter(criteria == criteria[i]) %>% 
    ggplot(aes(x = w.scores, y = reorder(technology, w.scores), fill = criteria)) + 
    geom_bar(stat = "identity", width = 0.8) + # Bars instead of dots
    scale_fill_manual(values = criteria_col[i]) +
    # facet_grid(. ~ criteria, scales = "free_x", space = "free_x") + # Side-by-side facets
    theme_minimal(base_size = 10) +
    labs(x = "Weighted score", y = "Technology", fill = "Criteria", title = criteria[i]) +
    xlim(0,6) +
    theme(#axis.text.y = element_text(size = 8), # Show technology names only once
      axis.title.x = element_text(size = 10),
      #axis.text.x = element_blank(), # Remove x-axis text for a cleaner look
      #axis.ticks.x = element_blank(),
      #panel.grid.major.x = element_blank(),
      strip.text = element_text(size = 8), # Smaller facet labels
      legend.position = "none", # Move legend below the plot
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 8)) +
    guides(fill = guide_legend(nrow = 3))
  # ggsave(paste("figs/ranking_additional_plots/ranking_",criteria[i],".jpg",sep = ""))
}


# weighted_scores_criteria_long_gr <- weighted_scores_criteria_long %>%
#   mutate(group = case_when(
#     criteria == "application" ~ "Application",
#     criteria %in% c("audience", "engagement_feedback", "engagement_others") ~ "Engagement",
#     criteria %in% c("extend_data", "new_data") ~ "New info",
#     TRUE ~ "Improve data"
#   ))
# weighted_scores_criteria_long_gr$group <- factor(weighted_scores_criteria_long_gr$group,
#   levels = c("New info", "Improve data", "Engagement", "Application"))
# weighted_scores_criteria_long_gr %>%
#   ggplot(aes(x = w.scores, y = reorder(technology, sum.scores), fill = criteria)) + 
#   geom_bar(position="stack", stat = "identity", width = 0.8) + # Bars
#   scale_fill_viridis_d(option = "H", begin = 1, end = 0, 
#                        labels = c("Application", "Audience", "Engagement via feedback", 
#                                   "Engagement with others", "Extend data", 
#                                   "Improve data curation", "Improve data flow", 
#                                   "Improve data quality", "New data")) +
#   facet_grid(. ~ factor(group)) + # Facets for groups
#   theme_minimal(base_size = 10) +
#   labs(x = "Score", y = "Technology", fill = "Criteria") +
#   theme(axis.text.y = element_text(size = 8), # Technology names only once
#         axis.title.x = element_text(size = 10),
#         axis.text.x = element_blank(), # Simplify x-axis
#         axis.ticks.x = element_blank(),
#         panel.grid.major.x = element_blank(),
#         strip.text = element_text(size = 10), # Larger facet labels
#         legend.position = "bottom", # Legend below plot
#         legend.title = element_text(size = 10),
#         legend.text = element_text(size = 8)) +
#   guides(fill = guide_legend(nrow = 3)) -> grouped_facet_bar_plot
# grouped_facet_bar_plot

##---- analyse confidence in the assessments for the final ranking ----

# Read and analyse data on the confidence level
confidence <- read.csv("Data/coders_confidence_level.csv")
colnames(confidence) <-  c("X","ResearchStage","job","age","TaxonomicExpertise",
                           "3D technology to improve experience",
                           "Acoustic analysis", "Acoustic hardware", "Adaptive sampling",
                           "Advanced natural language generation",
                           "AI analysis on sensors", "Artificial intelligence",
                           "Augmented reality", "Chatbot", "Collective intelligence", 
                           "Computational infrastructure to handle big volumes of data",
                           "Data aggregation software and visualisation and analysis tools",
                           "Data analysis to reveal early signals of invasion",
                           "Data exchange tools", "Digital twins" , "DNA-based technologies",
                           "Drones", "Ecological interaction approaches", "eNose technology",
                           "Gamification", "Google Street view", "GPS tracking devices",
                           "Image analysis satellites", "Internet of things",
                           "Low earth orbit ultrafast broadband", "Machine learning",
                           "Mobile based data collection platforms", "Neural marketing",
                           "Open data and open source", "Open source hardware",
                           "Push Notifications", "Robots", "Social media", "Social media mining",
                           "Technologies to increase data quality and reduce uncertainty",
                           "Tools for sensory impaired citizen scientists",
                           "Virtual reality", "Visual storytelling", "Wildlife cameras",
                           "coder")
confidence %>% 
  pivot_longer(cols = '3D technology to improve experience':'Wildlife cameras',
               names_to = "technology",
               values_to = "class") -> confidence_long
na.omit(confidence_long) -> confidence_long
confidence_long$class <- factor(confidence_long$class,
                                levels = c("not confident at all", "slightly confident",
                                           "somewhat confident", "fairly confident",
                                           "completely confident"))
recode_plyr_conf <- function(x) {
  as.numeric(plyr::mapvalues(x, from = c("not confident at all", "slightly confident",
                                         "somewhat confident", "fairly confident",
                                         "completely confident"),
                             to = c(1,2,3,4,5)))
}
confidence_long$class_num <- recode_plyr_conf(confidence_long$class)

# Ensure both datasets can be ordered in the same way
left_join(confidence_long, weighted_scores_criteria[,c("technology","sum.scores")], by = "technology") -> confidence_long
# left_join(confidence_long, unknown, by = "technology") -> confidence_long
# confidence_long$tot[is.na(confidence_long$tot)] <- 0
confidence_long %>% 
  filter(technology != "Acoustic hardware") %>% 
  filter(technology != "3D technology to improve experience") -> confidence_long

##---- confidence violin plot -----
# Create the violin plot with modified labels and top x-axis
plot_violin <- ggplot(confidence_long, aes(x = reorder(technology, sum.scores), y = class_num, fill = "black")) +
  
  # Add reference lines for confidence levels
  geom_hline(yintercept = 1, lwd = 0.2, color = "grey", ) +
  geom_hline(yintercept = 2, lwd = 0.2, color = "grey") +
  geom_hline(yintercept = 3, lwd = 0.2, color = "grey") +
  geom_hline(yintercept = 4, lwd = 0.2, color = "grey") +
  geom_hline(yintercept = 5, lwd = 0.2, color = "grey") +
  
  geom_violin(trim = FALSE, color = NA) +  # Remove borders with color = NA
  
  labs(y = "Confidence level", x="") +  # Update axis labels
  
  theme_ipsum_ps(axis_title_size = 10, axis = FALSE) +
  scale_fill_viridis(discrete = TRUE, option = "H", begin = 0.8, end = 0) +  # Apply fill colors
  
  # ylim(c(1, 5)) +
  scale_y_continuous( 
    #position = "left", # Corrected to scale_y_continuous to work with the y-axis confidence levels
    breaks = c(1, 2, 3, 4, 5),
    labels = c("not at all", "slightly",
               "somewhat", "fairly",
               "completely")
  ) +
  
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 8),  # Technology labels
    axis.text.y = element_blank(),  # Remove x-axis labels
    axis.ticks.x = element_blank(),  # Remove x-axis ticks
    panel.grid.major.x = element_blank(),  # Remove major vertical grid lines
    panel.grid.minor.x = element_blank(),  # Remove minor vertical grid lines
    axis.title.x = element_text(vjust = 1, hjust = 0.5),  # Adjust title for top x-axis
    axis.title.y = element_blank()
  ) +
  
  coord_flip() +  # Flip coordinates for vertical technology labels
  scale_x_discrete(position = "top")  # Place x-axis on top
plot_violin



##---- combine the plots side by side with patchwork ----
library(patchwork)
combined_plot <- plot_ranking + plot_spacer() +
  plot_violin + #plot_spacer() +
  #plot_balloon +
  plot_layout(ncol = 3, widths = c(3, -0.85, 1))
# Display combined plot
combined_plot

combined_plot_heatmap <- heatmap_plot + plot_spacer() +
  plot_ranking_base + plot_spacer() +
  plot_violin +
  plot_layout(ncol = 5, widths = c(3, -0.9, 1.5, -1.3, 1))
# Display combined plot
combined_plot_heatmap

# # Uncomment to save the plot
# tiff("figs/ranking_confidence_plot.tiff",
#      height = 20, width = 20*1.365411, units = "cm", res = 300, compression = "lzw",
#      pointsize = 6)
# combined_plot
# dev.off()

##---- Test the relationship between confidence and score ----
confidence_long %>% 
  group_by(technology) %>% 
  summarise(median = median(class_num, na.rm = T),
            mode = Mode(class_num),
            score = mean(sum.scores)) -> dati_corr
cor(dati_corr$median, dati_corr$score, use = "complete.obs", method = "spearman")
cor.test(dati_corr$median, dati_corr$score, use = "complete.obs", method = "spearman")
(lm(score ~ median, data = dati_corr)) -> corr_model
library(sjPlot)
plot_model(corr_model, type = "pred", terms = "median")

# Create the plot with predictions and customizations
p <- plot_model(corr_model, type = "pred", terms = "median") +
  geom_jitter(data = dati_corr, aes(x = median, y = score), 
              alpha = 0.5, color = "darkblue", size = 1.2) +  # Adds data points
  labs(x = "Median confidence", y = "Score", title = "") + 
  theme_ipsum_ps(axis_title_size = 10, axis = TRUE) +
  scale_fill_viridis(discrete = FALSE, option = "H", begin = 0.8, end = 0) +
  theme(legend.position = "bottom")
# Display the plot
print(p)



##---- Heatmaps: I don't know ----

# Ensure both datasets can be ordered in the same way
left_join(unknown_counts_complete, weighted_scores_criteria[,c("technology","sum.scores")], by = "technology") -> IDK_long

IDK_long %>%
  ggplot(aes(x = criterion, y = reorder(technology, sum.scores), fill = total_idk)) +
  geom_tile(color = "white") +  # White borders for tiles
  scale_color_viridis(discrete = TRUE, option = "H", begin = 0, end = 0.8) +
  # scale_fill_gradient(low = "#E0F7FA", high = "#0073C2FF") +  # Gradient from light to dark
  theme_ipsum_ps(axis_title_size = 10, axis = FALSE) +
  scale_x_discrete(labels = c("Application", "Audience", "Engagement via feedback",
                              "Engagement with others", "Extend data",
                              "Improve data curation", "Improve data flow",
                              "Improve data quality", "New data")) +
  scale_fill_viridis(discrete = FALSE, option = "mako", begin = 1, end = 0) +
  labs(x = "Criterion", y = "Technology", fill = "No. of 'I Don't Know'") +  # Single legend for fill
  theme(
    legend.position = c(0.1,-0.35), legend.direction = "horizontal",
    plot.margin = margin(5.5, 40, 60, 5.5),
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis text for readability
  ) +
  coord_fixed(ratio = 0.8) -> heatmap_plot
heatmap_plot

fill_colors <- viridis(1, option = "H", begin = 0, end = 0.8)
# Marginal histogram plot (right-side bar plot)
histogram_plot <- IDK_long %>%
  ggplot(aes(y = reorder(technology, sum.scores), x = total_idk)) +
  geom_bar(stat = "identity", fill = fill_colors[1]) +
  # scale_fill_viridis(option = "mako", begin = 1, end = 0) +
  theme_void() +  # Removes axis and grid
  theme(legend.position = "none")
histogram_plot

# Combine both plots with patchwork
heathist_plot <- heatmap_plot + plot_spacer() +
  histogram_plot +
  plot_layout(widths = c(4, -1.9, 0.5))
# Display combined plot
heathist_plot
# # Uncomment to save the plot
# tiff("figs/heatmap_IDK_plot.tiff",
#      height = 20, width = 20, units = "cm", res = 300, compression = "lzw",
#      pointsize = 6)
# heatmap_plot + plot_spacer() +
#   histogram_plot +
#   plot_layout(widths = c(4, -1.8, 0.7))
# dev.off()


##---- Heatmaps: N/A ----

# Ensure both datasets can be ordered in the same way
left_join(NA_counts_complete, weighted_scores_criteria[,c("technology","sum.scores")], by = "technology") -> NAS_long

NAS_long %>%
  ggplot(aes(x = criterion, y = reorder(technology, sum.scores), fill = total_idk)) +
  geom_tile(color = "white") +  # White borders for tiles
  scale_color_viridis(discrete = TRUE, option = "H", begin = 0, end = 0.8) +
  # scale_fill_gradient(low = "#E0F7FA", high = "#0073C2FF") +  # Gradient from light to dark
  theme_ipsum_ps(axis_title_size = 10, axis = FALSE) +
  scale_x_discrete(labels = c("Application", "Audience", "Engagement via feedback",
                              "Engagement with others", "Extend data",
                              "Improve data curation", "Improve data flow",
                              "Improve data quality", "New data")) +
  scale_fill_viridis(discrete = FALSE, option = "mako", begin = 1, end = 0) +
  labs(x = "Criterion", y = "Technology", fill = "No. of 'N/A'") +  # Single legend for fill
  theme(
    legend.position = c(0.1,-0.35), legend.direction = "horizontal",
    plot.margin = margin(5.5, 40, 60, 5.5),
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis text for readability
  ) +
  coord_fixed(ratio = 0.8) -> heatmap_plot
heatmap_plot

# fill_colors <- viridis(1, option = "H", begin = 0, end = 0.8)
# Marginal histogram plot (right-side bar plot)
histogram_plot <- IDK_long %>%
  ggplot(aes(y = reorder(technology, sum.scores), x = total_idk)) +
  geom_bar(stat = "identity", fill = fill_colors[1]) +
  # scale_fill_viridis(option = "mako", begin = 1, end = 0) +
  theme_void() +  # Removes axis and grid
  theme(legend.position = "none")
histogram_plot

# Combine both plots with patchwork
heathist_plot <- heatmap_plot + plot_spacer() +
  histogram_plot +
  plot_layout(widths = c(4, -1.9, 0.5))
# Display combined plot
heathist_plot
# # Uncomment to save the plot
# tiff("figs/heatmap_NA_plot.tiff",
#      height = 20, width = 20, units = "cm", res = 300, compression = "lzw",
#      pointsize = 6)
# heatmap_plot + plot_spacer() +
#   histogram_plot +
#   plot_layout(widths = c(4, -1.8, 0.7))
# dev.off()



##---- Balloon plot ----
# # Create the ggballoonplot with correct reordering and complete technology list
# scores_tidy_long %>% 
#   group_by(technology, rank) %>% 
#   summarise(count = n(), .groups = 'drop') %>% # Aggregate counts
#   complete(technology, rank = unique(rank), fill = list(count = 0)) %>% # Complete for all ranks
#   filter(rank == "I don't know") %>%           # Filter for "I don't know"
#   left_join(weighted_scores_criteria[, c("technology", "sum.scores")], by = "technology") %>%
#   mutate(technology = factor(technology, levels = unique(technology[order(sum.scores)]))) %>% # Reorder based on scores
#   ggballoonplot(aes(x = technology, y = rank, size = count)) +
#   labs(title = "",
#        x = "Technology",
#        y = "Rank") +
#   theme_ipsum_ps(axis_title_size = 10, axis = TRUE) +  
#   scale_size(range = c(1, 5)) +  # Adjust size range to make balloons smaller
#   scale_color_viridis(discrete = TRUE, option = "H", begin = 0.8, end = 0) +
#   # scale_fill_manual(values = "blue") +
#   theme(
#     legend.position = "none",
#     axis.text.y = element_blank(), 
#     axis.text.x = element_blank(),  
#     axis.ticks.x = element_blank(),  
#     panel.grid.major.x = element_blank(),  
#     panel.grid.minor.x = element_blank(),   
#     axis.title.y = element_blank()
#   ) +
#   coord_flip() -> plot_balloon
# plot_balloon

# ## ---- scores per aggregated criteria ----
# dati_wide %>% 
#   mutate(engagement = audience + engagement_feedback + engagement_others,
#          new_information = new_data + extend_data,
#          improve_data = improve_curation + improve_flow + improve_quality) %>% 
#   dplyr::select(no.coders, application, engagement, new_information, improve_data) -> dati_wide_aggr
# 
# as.matrix(dati_wide_aggr[,3:6]) -> mat
# as.numeric(dati_wide_aggr$no.coders) -> dev
# plyr::aaply(mat, 2, "/", dev) -> weighted.scores
# bind_cols(dati_wide[,1:2],
#           as.data.frame(t(weighted.scores))) -> average_scores_criteria
# left_join(average_scores_criteria, rank_sum_tech) -> average_scores_aggregated_criteria
# # write.csv(average_scores_criteria, "round2/output/average_scores_aggregated_criteria.csv")
# # write_xlsx(average_scores_criteria, "round2/output/average_scores_aggregated_criteria.xlsx")

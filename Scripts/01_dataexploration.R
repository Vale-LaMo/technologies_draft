#############################################+
## Exploratory analyses
## Valentina LM for the CostAction Team
## First draft: 2022-04-25
## Revised: 2024-08-12
#############################################+

### ---- Load Required Packages ----
library(tidyverse)  # For data manipulation and visualization
library(ggpubr)     # For ggplot2 extensions and easy publication-ready plots
library(hrbrthemes)
library(viridis)

### ---- Function for the statistical mode ----
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

## ---- Load data ----
load("output/dat.RData")
load("output/scores_num.RData")
load("output/scores_num_long.RData")
load("output/scores_tidy_long.RData")


## ---- Assessors (Coders) per Technology ----
# Group data by technology and count the number of coders for each technology
summary_coders_tech <- dat %>% 
  group_by(technology) %>% 
  summarise(n = n())  # Count the number of entries (coders) per technology

# Create a bar plot to visualize the number of coders per technology
summary_coders_tech %>% 
  ggplot(aes(reorder(technology, n), n)) + 
  geom_col(aes(fill = n)) +  # Use a bar chart with fill based on count
  scale_fill_gradient2(low = "blue", high = "red") +  # Color gradient from blue to red
  coord_flip() +  # Flip coordinates for better readability
  theme_minimal() +  # Use a minimal theme for the plot
  scale_y_continuous(breaks = c(2, 4, 6, 8, 10, 12)) +  # Set y-axis breaks
  labs(x = "Technology", y = "No. of Coders")  # Label axes
# Uncomment to save the plot
# ggsave("figs/summary_coders_per_technology.tiff", dpi = 300, compression = 'lzw')

## Analyses repeated for each round
# Group and nest the data by the 'round' column
results <- dat %>%
  group_by(round) %>%
  nest() %>%  # Nest the data for each round
  mutate(
    # Assessors (Coders) per Technology
    summary_coders_tech = map(data, ~ {
      .x %>%
        group_by(technology) %>%
        summarise(n = n())  # Count the number of coders per technology
    }),
    
    # Create a bar plot for coders per technology
    plot_coders = map(summary_coders_tech, ~ {
      ggplot(.x, aes(reorder(technology, n), n)) + 
        geom_col(aes(fill = n)) + 
        scale_fill_gradient2(low = "blue", high = "red") + 
        coord_flip() + 
        theme_minimal() + 
        scale_y_continuous(breaks = c(2, 4, 6, 8, 10, 12)) + 
        labs(x = "Technology", y = "No. of Coders")
    })
  )

# Access the results for each round
# for the first round:
results$summary_coders_tech[[1]]
results$summary_coders_tech[[2]]
# plot for the first round:
print(results$plot_coders[[1]])
print(results$plot_coders[[2]])


## ---- Count 'I Don't Know' / 'N/A' Responses per Technology ----
# Count occurrences of "I don't know" and "N/A" for each technology and criterion
unknown_criteria <- scores_tidy_long %>% 
  group_by(technology, criterion) %>% 
  count(rank) %>% 
  filter(rank == "I don't know" | rank == "N/A")  # Filter for specific ranks

# Summarize total counts of "I don't know" and "N/A" per technology
unknown <- scores_tidy_long %>% 
  group_by(technology) %>% 
  count(rank) %>% 
  filter(rank == "I don't know" | rank == "N/A") %>%
  summarise(tot = sum(n))  # Total counts

# Create a bar plot to visualize the total counts of "I don't know" / "N/A" per technology
unknown %>% 
  ggplot(aes(reorder(technology, tot), tot)) + 
  geom_col(aes(fill = tot)) +  # Bar chart with fill based on total
  scale_fill_gradient2(low = "blue", high = "red") +  # Color gradient
  coord_flip() +  # Flip coordinates for readability
  theme_minimal() +  # Minimal theme
  scale_y_continuous(breaks = c(2, 4, 6, 8, 10, 12)) +  # Set y-axis breaks
  labs(x = "Technology", y = "No. of 'I Don't Know' / 'N/A'")  # Label axes

# Count detailed occurrences of "I don't know" and "N/A" by technology and rank
unknown_ranks <- scores_tidy_long %>% 
  filter(rank == "I don't know" | rank == "N/A") %>%
  group_by(technology, rank) %>% 
  count(rank) %>% 
  summarise(tot = sum(n))  # Total counts by rank

# Create a stacked bar plot for detailed counts of "I don't know" / "N/A"
unknown_ranks %>% 
  ggplot(aes(reorder(technology, tot), tot, fill = rank)) + 
  geom_bar(position = "stack", stat = "identity") +  # Stacked bar chart
  scale_fill_manual(values = c("#0073C2FF", "#EFC000FF")) +  # Custom colors
  coord_flip() +  # Flip coordinates for readability
  theme_minimal() +  # Minimal theme
  scale_y_continuous(breaks = c(2, 6, 10, 14, 18, 22)) +  # Set y-axis breaks
  labs(x = "Technology", y = "No. of 'I Don't Know' / 'N/A'") +  # Label axes
  theme(legend.title = element_blank())  # Remove legend title
# Uncomment to save the plot
# ggsave("figs/summary_unknowns_detailed.tiff", dpi = 300, compression = 'lzw')


## Analyses repeated for each round
# Group and nest the data by the 'round' column
results <- scores_tidy_long %>%
  group_by(round) %>%
  nest() %>%  # Nest the data for each round
  mutate(
    
    # Count occurrences of "I don't know" and "N/A" for each technology and criterion
    unknown_ranks = map(data, ~ {
      .x %>%
        filter(rank == "I don't know" | rank == "N/A") %>%
        group_by(technology, rank) %>% 
        count(rank) %>% 
        summarise(tot = sum(n))
    }),
    
    # # Create a bar plot for coders per technology
    plot_unknown = map(unknown_ranks, ~ {
      ggplot(.x, aes(reorder(technology, tot), tot, fill = rank)) + 
        geom_bar(position = "stack", stat = "identity") +  # Stacked bar chart
        scale_fill_manual(values = c("#0073C2FF", "#EFC000FF")) +  # Custom colors
        coord_flip() +  # Flip coordinates for readability
        theme_minimal() +  # Minimal theme
        scale_y_continuous(breaks = c(2, 6, 10, 14, 18, 22)) +  # Set y-axis breaks
        labs(x = "Technology", y = "No. of 'I Don't Know' / 'N/A'") +  # Label axes
        theme(legend.title = element_blank())  # Remove legend title
      
    })

  )

# Access the results for each round
# for the first round:
results$unknown_ranks[[1]]
results$unknown_ranks[[2]]
# plot for the first round:
print(results$plot_unknown[[1]])
print(results$plot_unknown[[2]])



## ---- Basic Statistics ----
# Calculate basic statistics for each criterion, regardless of technology
basic_stats_criteria <- scores_num_long %>% 
  group_by(criterion) %>% 
  summarise(
    min = min(rank, na.rm = TRUE),
    q1 = quantile(rank, 0.25, na.rm = TRUE),
    median = median(rank, na.rm = TRUE),
    mean = mean(rank, na.rm = TRUE),
    mode = Mode(rank),
    q3 = quantile(rank, 0.75, na.rm = TRUE),
    max = max(rank, na.rm = TRUE)
  )
basic_stats_criteria

# Calculate basic statistics for each criterion by technology
basic_stats_tech_criteria <- scores_num_long %>% 
  group_by(technology, criterion) %>% 
  summarise(
    min = min(rank, na.rm = TRUE),
    q1 = quantile(rank, 0.25, na.rm = TRUE),
    median = median(rank, na.rm = TRUE),
    mean = mean(rank, na.rm = TRUE),
    mode = Mode(rank),
    q3 = quantile(rank, 0.75, na.rm = TRUE),
    max = max(rank, na.rm = TRUE)
  )
basic_stats_tech_criteria

## Analyses repeated for each round
# Group and nest the data by the 'round' column
results <- scores_num_long %>%
  group_by(round) %>%
  nest() %>%  # Nest the data for each round
  mutate(
    
    # calculate basic stats per round
    basic_stats_tech_criteria = map(data, ~ {
      .x %>%
        group_by(technology, criterion) %>% 
        summarise(
          # min = min(rank, na.rm = TRUE),
          q1 = quantile(rank, 0.25, na.rm = TRUE),
          median = median(rank, na.rm = TRUE),
          mean = mean(rank, na.rm = TRUE),
          mode = Mode(rank),
          q3 = quantile(rank, 0.75, na.rm = TRUE),
          # max = max(rank, na.rm = TRUE)
        )
    })
  )

# Access the results for each round
# for the first round:
results$basic_stats_tech_criteria[[1]]
results$basic_stats_tech_criteria[[2]]

## ---- Statistics and Plots per Criterion ----
# Display the dimensions of the scores_num_long data frame
dim(scores_num_long)

# Create a violin plot for rank distribution by criterion, regardless of technology
scores_num_long %>% 
  ggplot(aes(x = criterion, y = rank)) + 
  scale_x_discrete(guide = guide_axis(angle = 45)) +  # Rotate x-axis labels for readability
  geom_violin()  # Violin plot to show distribution
# Uncomment to save the plot
# ggsave("round1/criteria_violin.tiff", dpi = 300, compression = 'lzw')

# # Create balloon plots for frequency of ranks by criterion
# pp <- scores_num_long %>% 
#   group_by(criterion, rank) %>% 
#   summarise(Freq = n())  # Count frequency of each rank per criterion
# 
# # Generate a balloon plot using ggballoonplot
# ggballoonplot(pp, x = "criterion", y = "rank",
#               fill = "Freq", size = "Freq",
#               ggtheme = theme_gray()) +
#   scale_y_continuous(limits = c(1, 5))  # Set y-axis limits
# # Uncomment to save the plot
# # ggsave("round1/criteria_balloon.tiff", dpi = 300, compression = 'lzw')


## Analyses repeated for each technology
# Group and nest the data by the 'technology' column
results <- scores_num_long %>%
  group_by(technology) %>%
  nest() %>%  # Nest the data for each round
  mutate(
    
    # Count occurrences of "I don't know" and "N/A" for each technology and criterion
    violin_by_tech = map(data, ~ {
      .x %>% 
        mutate(technology = technology) %>%
        mutate(Round = factor(round, levels = c("Before", "After")))
    }),
    
    violin_plot = map(violin_by_tech, ~ {
      # scores_num_long %>%
      # mutate(criterion_label = factor(criterion, 
      #                                 levels=c("Application", "Audience",
      #                                          "Engagement via feedback", "Engagement with other",
      #                                          "Extend data", "Improve data curation",
      #                                          "Improve data flow", "Improve data quality",
      #                                          "Provide new data"))) %>%
      ggplot(.x, aes(fill=Round, y=rank, x=criterion)) + 
        geom_violin(position="dodge", alpha=1, scale = "width", width = 0.5, linewidth = 0) +
        scale_fill_viridis(discrete = TRUE, option = "H", begin = 0, end = 0.8) +
        scale_x_discrete(guide = guide_axis(angle = 45),
                         labels=c("Application", "Audience",
                                 "Engagement via feedback", "Engagement with others",
                                 "Extend data", "Improve data curation",
                                 "Improve data flow", "Improve data quality",
                                 "Provide new data")) + # Rotate x-axis labels for readability
        # theme_minimal() +  # Minimal theme
        # scale_y_continuous(breaks = c(2, 6, 10, 14, 18, 22)) +  # Set y-axis breaks
        ggtitle(paste(.x$technology[1])) +
        labs(x = "Criterion", y = "Score") +  # Label axes
        theme_ipsum_ps(axis_title_size = 10, axis = TRUE) +
        theme(legend.title = element_blank(), legend.position = "bottom") # Remove legend title
    })
    
  )
      
# Access the results for each technology
# e.g., for the first two technologies
results$violin_by_tech[[1]]
results$violin_by_tech[[2]]
# plot for the technologies:
print(results$violin_plot[[1]])
i=1
while(i <= length(unique(scores_num_long$technology))) {
  print(results$violin_plot[[i]])
  i = i+1
}

# tiff("figs/violin_plot.tiff",
#      height = 14, width = 20, units = "cm", res = 300, compression = "lzw",
#      pointsize = 8)
# print(results$violin_plot[[1]])
# dev.off()


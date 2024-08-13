#############################################+
## Exploratory analyses
## Valentina LM for the CostAction Team
## First draft: 2022-04-25
## Revised: 2024-08-12
#############################################+

### ---- Load Required Packages ----
library(tidyverse)  # For data manipulation and visualization
library(ggpubr)     # For ggplot2 extensions and easy publication-ready plots

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

## ---- Basic Statistics ----
# Calculate basic statistics for each criterion, regardless of technology
basic_stats_criteria <- scores_num_long %>% 
  group_by(criterion) %>% 
  summarise(
    min = min(rank, na.rm = TRUE),
    q1 = quantile(rank, 0.25, na.rm = TRUE),
    median = median(rank, na.rm = TRUE),
    mean = mean(rank, na.rm = TRUE),
    q3 = quantile(rank, 0.75, na.rm = TRUE),
    max = max(rank, na.rm = TRUE)
  )

# Calculate basic statistics for each criterion by technology
basic_stats_tech_criteria <- scores_num_long %>% 
  group_by(technology, criterion) %>% 
  summarise(
    min = min(rank, na.rm = TRUE),
    q1 = quantile(rank, 0.25, na.rm = TRUE),
    median = median(rank, na.rm = TRUE),
    mean = mean(rank, na.rm = TRUE),
    q3 = quantile(rank, 0.75, na.rm = TRUE),
    max = max(rank, na.rm = TRUE)
  )

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

# Create balloon plots for frequency of ranks by criterion
pp <- scores_num_long %>% 
  group_by(criterion, rank) %>% 
  summarise(Freq = n())  # Count frequency of each rank per criterion

# Generate a balloon plot using ggballoonplot
ggballoonplot(pp, x = "criterion", y = "rank",
              fill = "Freq", size = "Freq",
              ggtheme = theme_gray()) +
  scale_y_continuous(limits = c(1, 5))  # Set y-axis limits
# Uncomment to save the plot
# ggsave("round1/criteria_balloon.tiff", dpi = 300, compression = 'lzw')


##---- Analyses repeated for each round ----

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
# plot for the first round:
print(results$plot_coders[[1]])
print(results$plot_coders[[2]])


# Group and nest the data by the 'round' column
results <- scores_tidy_long %>%
  group_by(round) %>%
  nest() %>%  # Nest the data for each round
  mutate(
    
    # Count occurrences of "I don't know" and "N/A" for each technology and criterion
    unknown_criteria <- map(data, ~ {
      .x %>%
      group_by(technology, criterion) %>% 
      count(rank) %>% 
      filter(rank == "I don't know" | rank == "N/A")  # Filter for specific ranks
    })#,
    
    # # Summarize total counts of "I don't know" and "N/A" per technology
    # unknown <- map(data, ~ {
    #   .x %>%
    #   group_by(technology) %>% 
    #   count(rank) %>% 
    #   filter(rank == "I don't know" | rank == "N/A") %>%
    #   summarise(tot = sum(n))  # Total counts
    # })
    
    # # Create a bar plot for coders per technology
    # plot_unknown = map(unknown, ~ { 
    #   ggplot(aes(reorder(technology, tot), tot)) + 
    #   geom_col(aes(fill = tot)) + 
    #   scale_fill_gradient2(low = "blue", high = "red") + 
    #   coord_flip() + 
    #   theme_minimal() + 
    #   scale_y_continuous(breaks = c(2, 4, 6, 8, 10, 12)) + 
    #   labs(x = "Technology", y = "No. of 'I Don't Know' / 'N/A'")
    #   })
  )

  # Access the results for each round
  # for the first round:
  results$unknown_criteria[[1]]
  # plot for the first round:
  print(results$plot_unknown[[1]])
  print(results$plot_unknown[[2]])
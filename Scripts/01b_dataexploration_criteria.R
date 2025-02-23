#############################################+
## Exploratory analyses - 'I Don't Know' / 'N/A' per criterion
## Valentina LM for the CostAction Team
## First draft: 2022-04-25
## Revised: 2024-08-12
#############################################+


## ---- Count 'I Don't Know' / 'N/A' Responses per Technology ----
# Count occurrences of "I don't know" and "N/A" for each technology and criterion
unknown_criteria <- scores_tidy_long %>% 
  group_by(technology, criterion) %>% 
  count(rank) %>% 
  filter(rank == "I don't know" | rank == "N/A")  # Filter for specific ranks

# Summarize total counts of "I don't know" and "N/A" per criterion
unknown <- scores_tidy_long %>% 
  group_by(criterion) %>% 
  count(rank) %>% 
  filter(rank == "I don't know" | rank == "N/A") %>%
  summarise(tot = sum(n))  # Total counts

# Create a bar plot to visualize the total counts of "I don't know" / "N/A" per criterion
unknown %>% 
  ggplot(aes(reorder(criterion, tot), tot)) + 
  geom_col(aes(fill = tot)) +  # Bar chart with fill based on total
  scale_fill_gradient2(low = "blue", high = "red") +  # Color gradient
  coord_flip() +  # Flip coordinates for readability
  theme_minimal() +  # Minimal theme
  scale_y_continuous(breaks = c(2, 4, 6, 8, 10, 12)) +  # Set y-axis breaks
  labs(x = "Criterion", y = "No. of 'I Don't Know' / 'N/A'")  # Label axes

# # Count detailed occurrences of "I don't know" and "N/A" by criterion and rank
# unknown_ranks <- scores_tidy_long %>% 
#   filter(rank == "I don't know" | rank == "N/A") %>%
#   group_by(criterion, rank) %>% 
#   count(rank) %>% 
#   summarise(tot = sum(n))  # Total counts by rank
# 
# # Create a stacked bar plot for detailed counts of "I don't know" / "N/A"
# unknown_ranks %>% 
#   ggplot(aes(reorder(criterion, tot), tot, fill = rank)) + 
#   geom_bar(position = "stack", stat = "identity") +  # Stacked bar chart
#   scale_fill_manual(values = c("#0073C2FF", "#EFC000FF")) +  # Custom colors
#   coord_flip() +  # Flip coordinates for readability
#   theme_minimal() +  # Minimal theme
#   scale_y_continuous(breaks = c(2, 6, 10, 14, 18, 22)) +  # Set y-axis breaks
#   labs(x = "Criterion", y = "No. of 'I Don't Know' / 'N/A'") +  # Label axes
#   theme(legend.title = element_blank())  # Remove legend title
# # Uncomment to save the plot
# # ggsave("figs/summary_unknowns_detailed_criterion.tiff", dpi = 300, compression = 'lzw')

# Count occurrences of "I don't know" and "N/A" by criterion and rank, adding total count per criterion
unknown_ranks <- scores_tidy_long %>% 
  filter(rank %in% c("I don't know", "N/A")) %>%
  count(criterion, rank) %>%
  group_by(criterion) %>%
  mutate(total_count = sum(n)) %>%  # Calculate total for ordering
  ungroup()

# Create a stacked bar plot with technologies ordered by total count
unknown_ranks %>% 
  ggplot(aes(reorder(criterion, total_count), n, fill = rank)) + 
  geom_bar(position = "stack", stat = "identity") +  # Stacked bar chart
  scale_fill_manual(values = c("#0073C2FF", "#EFC000FF")) +  # Custom colors
  coord_flip() +  # Flip coordinates for readability
  theme_minimal() +  # Minimal theme
  scale_y_continuous(breaks = seq(0, max(unknown_ranks$total_count), by = 2)) +  # Dynamic y-axis breaks
  labs(x = "Criterion", y = "No. of 'I Don't Know' / 'N/A'") +  # Label axes
  theme(legend.title = element_blank(), legend.position = "bottom")  # Remove legend title
# Uncomment to save the plot
# ggsave("figs/summary_unknowns_detailed_criterion.tiff", dpi = 300, compression = 'lzw')

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
        group_by(criterion, rank) %>% 
        count(rank) %>% 
        summarise(tot = sum(n))
    }),
    
    # # Create a bar plot for coders per technology
    plot_unknown = map(unknown_ranks, ~ {
      ggplot(.x, aes(reorder(criterion, tot), tot, fill = rank)) + 
        geom_bar(position = "stack", stat = "identity") +  # Stacked bar chart
        scale_fill_manual(values = c("#0073C2FF", "#EFC000FF")) +  # Custom colors
        coord_flip() +  # Flip coordinates for readability
        theme_minimal() +  # Minimal theme
        scale_y_continuous(breaks = c(2, 6, 10, 14, 18, 22)) +  # Set y-axis breaks
        labs(x = "Criterion", y = "No. of 'I Don't Know' / 'N/A'") +  # Label axes
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

## ---- Count only 'I Don't Know' Responses per Technology ----
# Count occurrences of "I don't know" for each technology and criterion
unknown_criteria <- scores_tidy_long %>% 
  group_by(technology, criterion) %>% 
  count(rank) %>% 
  filter(rank == "I don't know")  # Filter for specific ranks

# Summarize total counts of "I don't know" per criterion
unknown <- scores_tidy_long %>% 
  group_by(criterion) %>% 
  count(rank) %>% 
  filter(rank == "I don't know") %>%
  summarise(tot = sum(n))  # Total counts

# Create a bar plot to visualize the total counts of "I don't know" per technology
unknown %>% 
  ggplot(aes(reorder(criterion, tot), tot)) + 
  geom_col(fill = "#0073C2FF") +
  # geom_col(aes(fill = tot)) +  # Bar chart with fill based on total
  # scale_fill_manual(values = c("#0073C2FF"))
  # scale_fill_gradient2(low = "#0073C2FF", high = "blue") +  # Color gradient
  coord_flip() +  # Flip coordinates for readability
  theme_minimal() +  # Minimal theme
  scale_y_continuous(breaks = c(2, 4, 6, 8, 10, 12)) +  # Set y-axis breaks
  labs(x = "Criterion", y = "No. of 'I Don't Know'")  # Label axes
# Uncomment to save the plot
# ggsave("figs/summary_IDontKnow_criterion.tiff", dpi = 300, compression = 'lzw')

# Count occurrences of "I don't know" responses and ensure all technology-criterion combinations are present
unknown_counts_complete <- scores_tidy_long %>%
  mutate(n = if_else(rank == "I don't know", 1, 0)) %>%  # Create a count of 1 for "I don't know", 0 otherwise
  group_by(technology, criterion) %>%
  summarise(total_idk = sum(n), .groups = "drop") %>%  # Sum counts of "I don't know" per tech-criterion
  complete(technology, criterion, fill = list(total_idk = 0))  # Fill missing pairs with 0

# # Create a heatmap with circles to represent counts of "I don't know"
# unknown_counts_complete %>%
#   ggplot(aes(x = criterion, y = reorder(technology, -total_idk), fill = total_idk, size = total_idk)) +
#   geom_point(shape = 21, color = "white") +  # Circles with white borders
#   scale_fill_gradient(low = "#E0F7FA", high = "#0073C2FF") +  # Remove fill legend
#   scale_size(range = c(2, 10), guide = "none") +  # Adjust size range for better visibility
#   theme_minimal() +
#   labs(x = "Criterion", y = "Technology", size = "No. of 'I Don't Know'") +  # Label only size legend
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis text for readability
#   )

unknown_counts_complete %>%
  ggplot(aes(x = criterion, y = reorder(technology, -total_idk), fill = total_idk)) +
  geom_tile(color = "white") +  # White borders for tiles
  scale_fill_gradient(low = "#E0F7FA", high = "#0073C2FF") +  # Gradient from light to dark
  theme_minimal() +
  labs(x = "Criterion", y = "Technology", fill = "No. of 'I Don't Know'") +  # Single legend for fill
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis text for readability
  )

## ---- Count only 'N/A' Responses per Criterion ----
# Count occurrences of "N/A" for each technology and criterion
NA_criteria <- scores_tidy_long %>% 
  group_by(technology, criterion) %>% 
  count(rank) %>% 
  filter(rank == "N/A")  # Filter for specific ranks

# Summarize total counts of "N/A" per criterion
NAS <- scores_tidy_long %>% 
  group_by(criterion) %>% 
  count(rank) %>% 
  filter(rank == "N/A") %>%
  summarise(tot = sum(n))  # Total counts

# Create a bar plot to visualize the total counts of "N/A" per criterion
NAS %>% 
  ggplot(aes(reorder(criterion, tot), tot)) + 
  geom_col(fill = "#EFC000FF") +
  # geom_col(aes(fill = tot)) +  # Bar chart with fill based on total
  # scale_fill_manual(values = c("#0073C2FF"))
  # scale_fill_gradient2(low = "#0073C2FF", high = "blue") +  # Color gradient
  coord_flip() +  # Flip coordinates for readability
  theme_minimal() +  # Minimal theme
  scale_y_continuous(breaks = c(2, 4, 6, 8, 10, 12)) +  # Set y-axis breaks
  labs(x = "Criterion", y = "No. of 'N/A'")  # Label axes
# Uncomment to save the plot
# ggsave("figs/summary_NA_criterion.tiff", dpi = 300, compression = 'lzw')

# Count occurrences of "N/A" responses and ensure all technology-criterion combinations are present
NA_counts_complete <- scores_tidy_long %>%
  mutate(n = if_else(rank == "N/A", 1, 0)) %>%  # Create a count of 1 for "I don't know", 0 otherwise
  group_by(technology, criterion) %>%
  summarise(total_idk = sum(n), .groups = "drop") %>%  # Sum counts of "I don't know" per tech-criterion
  complete(technology, criterion, fill = list(total_idk = 0))  # Fill missing pairs with 0

NA_counts_complete %>%
  ggplot(aes(x = criterion, y = technology, fill = total_idk)) +
  # ggplot(aes(x = criterion, y = reorder(technology, -total_idk), fill = total_idk)) +
  geom_tile(color = "white") +  # White borders for tiles
  scale_fill_gradient(low = "white", high = "#0073C2FF") +  # Gradient from light to dark
  theme_minimal() +
  labs(x = "Criterion", y = "Technology", fill = "No. of 'N/A'") +  # Single legend for fill
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis text for readability
  )

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

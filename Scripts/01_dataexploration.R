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

## ---- Assessors (Coders) per Round (Before/After) ----
length(unique(dat$coder)) # total no. of coders
coders_round <- dat %>%
  group_by(round,coder) %>%
  summarise(n_assessments = n())
table(coders_round$coder, coders_round$round) # coders per round
colSums(table(coders_round$coder, coders_round$round)) # no. of coders in the different rounds
sum(coders_round$n_assessments) # tot. no. of assessments

# Identify coders who participated in both rounds
coders_in_both <- scores_tidy_long %>%
  group_by(coder) %>%
  filter(n_distinct(round) == 2) %>%
  ungroup()
dim(coders_in_both)
(num_coders_in_both_rounds <- n_distinct(coders_in_both$coder))

# Check if they rated different technologies in each round
coder_technology_diff <- coders_in_both %>%
  group_by(coder) %>%
  summarize(different_technologies = n_distinct(technology[round == 1] != technology[round == 2])) %>%
  filter(different_technologies > 0)
(num_coders_with_diff_tech <- nrow(coder_technology_diff))

# For each coder, find overlapping technologies between the two rounds
coder_technology_overlap <- coders_in_both %>%
  group_by(coder) %>%
  summarize(
    overlap_technologies = list(intersect(
      technology[round == 1], 
      technology[round == 2]
    )),
    .groups = "drop"
  )
coder_technology_overlap %>%
  mutate(
    has_overlap = lengths(overlap_technologies) > 0  # TRUE if there is at least one overlapping technology
  )

## ---- Assessors (Coders) per Technology ----
# Group data by technology and count the number of coders for each technology
summary_coders_tech <- dat %>% 
  group_by(technology) %>% 
  summarise(n = n())  # Count the number of entries (coders) per technology
sum(summary_coders_tech$n) # ok, it gives again the no. of assessments
summary(summary_coders_tech$n)
sd(summary_coders_tech$n)

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

length(unique(dat$technology)) # total no. of technologies
# Access the results for each round
# for the first round:
results$summary_coders_tech[[1]]
dim(results$summary_coders_tech[[1]]) # no. of technologies assessed during the first round
results$summary_coders_tech[[2]]
dim(results$summary_coders_tech[[2]]) # no. of technologies assessed during the second round
# plot for the first round:
print(results$plot_coders[[1]])
print(results$plot_coders[[2]])

sum(results$summary_coders_tech[[1]]$n) # ok, it's the number of assessments during the first round
summary(results$summary_coders_tech[[1]]$n)
sd(results$summary_coders_tech[[1]]$n)

sum(results$summary_coders_tech[[2]]$n) # ok, it's the number of assessments during the second round
summary(results$summary_coders_tech[[2]]$n)
sd(results$summary_coders_tech[[2]]$n)

## ---- Assessments per Round (Before/After) ----
dim(dat)[1] # total no. of assessments
assessments_round <- dat %>%
  group_by(round) %>%
  summarise(n_assessments = n())
assessments_round # no. of assessments per round

coders_round %>%
  group_by(round) %>% 
  summarise(mean = mean(n_assessments), sd = sd(n_assessments))

assessments_coders <- dat %>%
  group_by(coder) %>%
  summarise(n_assessments = n())
summary(assessments_coders$n_assessments)
sd(assessments_coders$n_assessments) 


## ---- Technologies per Round (Before/After) ----
techs_round <- dat %>%
  group_by(round,technology) %>%
  summarise(n_assessments = n())
table(techs_round$technology, techs_round$round) # techs per round
length(unique(dat$technology))

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

# # Count detailed occurrences of "I don't know" and "N/A" by technology and rank
# unknown_ranks <- scores_tidy_long %>% 
#   filter(rank == "I don't know" | rank == "N/A") %>%
#   group_by(technology, rank) %>% 
#   count(rank) %>% 
#   summarise(tot = sum(n))  # Total counts by rank
# 
# # Create a stacked bar plot for detailed counts of "I don't know" / "N/A"
# unknown_ranks %>% 
#   ggplot(aes(reorder(technology, tot), tot, fill = rank)) + 
#   geom_bar(position = "stack", stat = "identity") +  # Stacked bar chart
#   scale_fill_manual(values = c("#0073C2FF", "#EFC000FF")) +  # Custom colors
#   coord_flip() +  # Flip coordinates for readability
#   theme_minimal() +  # Minimal theme
#   scale_y_continuous(breaks = c(2, 6, 10, 14, 18, 22)) +  # Set y-axis breaks
#   labs(x = "Technology", y = "No. of 'I Don't Know' / 'N/A'") +  # Label axes
#   theme(legend.title = element_blank())  # Remove legend title
# # Uncomment to save the plot
# # ggsave("figs/summary_unknowns_detailed.tiff", dpi = 300, compression = 'lzw')

# Count occurrences of "I don't know" and "N/A" by technology and rank, adding total count per technology
unknown_ranks <- scores_tidy_long %>% 
  filter(rank %in% c("I don't know", "N/A")) %>%
  count(technology, rank) %>%
  group_by(technology) %>%
  mutate(total_count = sum(n)) %>%  # Calculate total for ordering
  ungroup()

# Create a stacked bar plot with technologies ordered by total count
unknown_ranks %>% 
  ggplot(aes(reorder(technology, total_count), n, fill = rank)) + 
  geom_bar(position = "stack", stat = "identity") +  # Stacked bar chart
  scale_fill_manual(values = c("#0073C2FF", "#EFC000FF")) +  # Custom colors
  coord_flip() +  # Flip coordinates for readability
  theme_minimal() +  # Minimal theme
  scale_y_continuous(breaks = seq(0, max(unknown_ranks$total_count), by = 2)) +  # Dynamic y-axis breaks
  labs(x = "Technology", y = "No. of 'I Don't Know' / 'N/A'") +  # Label axes
  theme(legend.title = element_blank(), legend.position = "bottom")  # Remove legend title
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

## ---- Count only 'I Don't Know' Responses per Technology ----
# Count occurrences of "I don't know" for each technology and criterion
unknown_criteria <- scores_tidy_long %>% 
  group_by(technology, criterion) %>% 
  count(rank) %>% 
  filter(rank == "I don't know")  # Filter for specific ranks

# Summarize total counts of "I don't know" per technology
unknown <- scores_tidy_long %>% 
  group_by(technology) %>% 
  count(rank) %>% 
  filter(rank == "I don't know") %>%
  summarise(tot = sum(n))  # Total counts

# Create a bar plot to visualize the total counts of "I don't know" per technology
unknown %>% 
  ggplot(aes(reorder(technology, tot), tot)) + 
  geom_col(fill = "#0073C2FF") +
  # geom_col(aes(fill = tot)) +  # Bar chart with fill based on total
  # scale_fill_manual(values = c("#0073C2FF"))
  # scale_fill_gradient2(low = "#0073C2FF", high = "blue") +  # Color gradient
  coord_flip() +  # Flip coordinates for readability
  theme_minimal() +  # Minimal theme
  scale_y_continuous(breaks = c(2, 4, 6, 8, 10, 12)) +  # Set y-axis breaks
  labs(x = "Technology", y = "No. of 'I Don't Know'")  # Label axes
# Uncomment to save the plot
# ggsave("figs/summary_IDontKnow.tiff", dpi = 300, compression = 'lzw')

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

## ---- Count only 'N/A' Responses per Technology ----
# Count occurrences of "N/A" for each technology and criterion
NA_criteria <- scores_tidy_long %>% 
  group_by(technology, criterion) %>% 
  count(rank) %>% 
  filter(rank == "N/A")  # Filter for specific ranks

# Summarize total counts of "N/A" per technology
NAS <- scores_tidy_long %>% 
  group_by(technology) %>% 
  count(rank) %>% 
  filter(rank == "N/A") %>%
  summarise(tot = sum(n))  # Total counts

# Create a bar plot to visualize the total counts of "N/A" per technology
NAS %>% 
  ggplot(aes(reorder(technology, tot), tot)) + 
  geom_col(fill = "#EFC000FF") +
  # geom_col(aes(fill = tot)) +  # Bar chart with fill based on total
  # scale_fill_manual(values = c("#0073C2FF"))
  # scale_fill_gradient2(low = "#0073C2FF", high = "blue") +  # Color gradient
  coord_flip() +  # Flip coordinates for readability
  theme_minimal() +  # Minimal theme
  scale_y_continuous(breaks = c(2, 4, 6, 8, 10, 12)) +  # Set y-axis breaks
  labs(x = "Technology", y = "No. of 'N/A'")  # Label axes
# Uncomment to save the plot
# ggsave("figs/summary_NA.tiff", dpi = 300, compression = 'lzw')

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
  # tiff(paste("figs/violin_plots/violin_plot_",results$violin_plot[[i]]$labels$title,".tiff",sep=""),
  #      height = 14, width = 20, units = "cm", res = 300, compression = "lzw",
  #      pointsize = 8)
  # print(results$violin_plot[[i]])
  # dev.off()
  i = i+1
}

# tiff("figs/violin_plot.tiff",
#      height = 14, width = 20, units = "cm", res = 300, compression = "lzw",
#      pointsize = 8)
# print(results$violin_plot[[1]])
# dev.off()


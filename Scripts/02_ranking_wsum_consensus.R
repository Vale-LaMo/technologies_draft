#############################################+
## Ranking
## Valentina LM for the CostAction Team
## First draft: 2022-05-04
#############################################+

## ---- packages ----
library(tidyverse)
library(ggpubr)
library(writexl)

## ---- functions ----
recode_plyr <- function(x) {
  as.numeric(plyr::mapvalues(x, from = c("Strongly disagree", "Disagree",
                                         "Neither agree nor disagree",
                                         "Agree", "Strongly agree"),
                             to = c(1,2,3,4,5)))
  # please note: 'I don't know' and 'N/A' -> NA
}

## ---- data and data tidying ----
temp <- read_delim("Data/workshop_3round_data.csv")[,-1]

## ---- data: recode to numeric ----
sapply(temp[,4:12], recode_plyr) -> scores_rec
bind_cols(temp[,1:3],as.data.frame(scores_rec)) -> scores_num

## ---- data:long formats ----
temp %>% 
  pivot_longer(
    cols = audience:improve_curation,
    names_to = "criterion",
    values_to = "rank"
  ) -> scores_tidy_long
scores_num %>% 
  pivot_longer(
    cols = audience:improve_curation,
    names_to = "criterion",
    values_to = "rank"
  ) -> scores_num_long

## ---- sum of scores ----
scores_num_long %>% 
  group_by(technology) %>% 
  summarise(sum.scores = sum(rank, na.rm = TRUE),
            no.coders = length(unique(coder))) %>% 
  mutate(w.rank = sum.scores / no.coders) %>% 
  arrange(desc(w.rank)) -> rank_sum_tech

## ---- scores per criteria ----
scores_num_long %>% 
  group_by(technology, criterion) %>% 
  summarise(sum.scores = sum(rank, na.rm = TRUE),
            no.coders = length(unique(coder))) -> dati
pivot_wider(dati, names_from = criterion,
              values_from = sum.scores) -> dati_wide

as.matrix(dati_wide[,3:11]) -> mat
as.numeric(dati_wide$no.coders) -> dev
plyr::aaply(mat, 2, "/", dev) -> weighted.scores
bind_cols(dati_wide[,1:2],
          as.data.frame(t(weighted.scores))) -> average_scores_criteria
write.csv(average_scores_criteria, "round3_consensus_building/output/average_scores_criteria.csv")
write_xlsx(average_scores_criteria, "round3_consensus_building/output/average_scores_criteria.xlsx")

## ---- scores per aggregated criteria ----
dati_wide %>% 
  mutate(engagement = audience + engagement_feedback + engagement_others,
         new_information = new_data + extend_data,
         improve_data = improve_curation + improve_flow + improve_quality) %>% 
  dplyr::select(no.coders, application, engagement, new_information, improve_data) -> dati_wide_aggr

as.matrix(dati_wide_aggr[,3:6]) -> mat
as.numeric(dati_wide_aggr$no.coders) -> dev
plyr::aaply(mat, 2, "/", dev) -> weighted.scores
bind_cols(dati_wide[,1:2],
          as.data.frame(t(weighted.scores))) -> average_scores_criteria
left_join(average_scores_criteria, rank_sum_tech) -> average_scores_aggregated_criteria
write.csv(average_scores_criteria, "round3_consensus_building/output/average_scores_aggregated_criteria.csv")
write_xlsx(average_scores_criteria, "round3_consensus_building/output/average_scores_aggregated_criteria.xlsx")


## ---- plots ----
average_scores_aggregated_criteria %>% 
  pivot_longer(cols = application:improve_data,
               names_to = "aggregated_criteria",
               values_to = "scores") -> average_scores_tot

average_scores_tot %>% 
  ggplot(aes(reorder(technology, w.rank), scores, fill=aggregated_criteria)) + 
  geom_bar(position="stack", stat="identity") + 
  scale_fill_manual(values = c("purple", "darkgreen", "red3", "#EFC000FF")) +
  coord_flip() + 
  theme_minimal() + 
  labs(x = "Technology", y = "score") +
  theme(legend.title = element_blank())
ggsave("round3_consensus_building/figs_ranking/summary_ranking.tiff",
       dpi=300, compression = 'lzw')

average_scores_tot %>% 
  filter(aggregated_criteria == "application") %>% 
  ggplot(aes(reorder(technology, scores), scores)) + 
  geom_bar(position="stack", stat="identity", fill = "purple") + 
  coord_flip() + 
  theme_minimal() + 
  labs(x = "Technology", y = "Application") +
  theme(legend.title = element_blank())
ggsave("round3_consensus_building/figs_ranking/summary_ranking_application.tiff",
       dpi=300, compression = 'lzw')

average_scores_tot %>% 
  filter(aggregated_criteria == "engagement") %>% 
  ggplot(aes(reorder(technology, scores), scores)) + 
  geom_bar(position="stack", stat="identity", fill = "darkgreen") + 
  coord_flip() + 
  theme_minimal() + 
  labs(x = "Technology", y = "Engagement") +
  theme(legend.title = element_blank())
ggsave("round3_consensus_building/figs_ranking/summary_ranking_engagement.tiff",
       dpi=300, compression = 'lzw')

average_scores_tot %>% 
  filter(aggregated_criteria == "new_information") %>% 
  ggplot(aes(reorder(technology, scores), scores)) + 
  geom_bar(position="stack", stat="identity", fill = "#EFC000FF") + 
  coord_flip() + 
  theme_minimal() + 
  labs(x = "Technology", y = "New information") +
  theme(legend.title = element_blank())
ggsave("round3_consensus_building/figs_ranking/summary_ranking_newinfo.tiff",
       dpi=300, compression = 'lzw')


average_scores_tot %>% 
  filter(aggregated_criteria == "improve_data") %>% 
  ggplot(aes(reorder(technology, scores), scores)) + 
  geom_bar(position="stack", stat="identity", fill = "red3") + 
  coord_flip() + 
  theme_minimal() + 
  labs(x = "Technology", y = "Data improvement") +
  theme(legend.title = element_blank())
ggsave("round3_consensus_building/figs_ranking/summary_ranking_improvement.tiff",
       dpi=300, compression = 'lzw')

## ---- to dos ----
# dividere le barre in stack in base alle parti che compongono le barre di engagement, information, ecc..

  
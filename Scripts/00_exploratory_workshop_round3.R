#############################################+
## Exploratory analyses - IRR metrics
## Valentina LM for the CostAction Team
## First draft: 2022-04-25
#############################################+

## ---- packages ----
library(tidyverse)
library(ggpubr)
library(irr)
library(lme4)

## ---- functions ----
recode_plyr <- function(x) {
  as.numeric(plyr::mapvalues(x, from = c("Strongly disagree", "Disagree",
                                         "Neither agree nor disagree",
                                         "Agree", "Strongly agree"),
                             to = c(1,2,3,4,5)))
  # please note: 'I don't know' and 'N/A' -> NA
}

## ---- data and data tidying ----
scores <- read_delim("output/workshop_individual_ass.csv")[,-1]
scores_new <- read_delim("Data/2022-05-06-subset.csv", delim = ";")

scores_new %>% 
  rename(date_time = 'Tijdstempel',
         coder = 'E-mailadres',
         technology = 'Select the technology you need to assess') %>% 
  select(-starts_with("Comments")) -> scores_new
scores_new[ scores_new == "" ] <- NA

distinct(scores_new["technology"]) -> techs_new

scores_new <- scores_new %>% 
  mutate(technology = case_when(technology == "Social media" ~ 
                                  "Social media have",
                                technology == "Social media mining" ~ 
                                  "Social media mining has",
                                technology == "3D technology to improve experience" ~ 
                                  "3D technology to improve CS experience",
                                TRUE ~ technology))
techs_new$technology
# techs_new$technology[17] <- "Social media have"
techs_new$technology[10] <- "3D technology to improve CS experience"
techs_new$technology[6] <- "Social media mining has"

temp <- data.frame()

for(t in techs_new$technology){
# for(i in 1:length(techs_new$technology)){
  # t <- techs_new$technology[i]
  if(is.na(t)){
    next
  }
  sub <- scores_new %>% 
    filter(technology == t) %>% 
    select(date_time, 
           coder,
           technology,
           matches(t))
  
  colnames(sub) <- c("date_time",
                      "coder",
                      "technology",
                      "audience",
                      "engagement_others",
                      "engagement_feedback",
                      "application",
                      "new_data",
                      "extend_data",
                      "improve_quality",
                      "improve_flow",
                      "improve_curation")
  
  if(nrow(temp) == 0){
    temp <- sub
  }else{
    temp <- rbind(temp, sub)
  }
}


# drop <- (na.omit(techs_new))[[1]]
drop <- c("Open source hardware",
          "Collective intelligence",
          "Social media mining has",
          "Data aggregation software and visualisation and analysis tools",
          "eNose technology",
          "Acoustic analysis",
          # "Augmented reality",
          "Push notifications")
          # "Robots"
          # "Digital twins"
          # "Advanced natural language generation",
          # "Computational infrastructure to handle big volumes of data",
          # "Ecological interaction approaches")
scores %>%
  filter(!technology %in% drop) -> scores_temp

bind_rows(scores_temp, temp) -> temp2
write.csv(temp2, "Data/workshop_3round_data.csv")
write_xlsx(temp2, "Data/workshop_3round_data.xlsx")
# dim(temp2)
unique(scores$technology) %in% unique(temp2$technology) # ok

temp <- temp2

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


## ---- assessors (coders) per technology ----
temp %>% 
  group_by(technology) %>% 
  summarise(n = n()) -> summary_coders_tech

summary_coders_tech %>% 
  ggplot(aes(reorder(technology, n), n)) + 
  geom_col(aes(fill = n)) +
  scale_fill_gradient2(low = "blue",
                       high = "red") +
  coord_flip() + 
  theme_minimal() +
  scale_y_continuous(breaks = c(2,4,6,8,10,12)) +
  labs(x = "Technology", y = "No. of coders")
ggsave("round3_consensus_building/summary_coders_tech.tiff",
       dpi=300, compression = 'lzw')

## ---- no. 'I don't know' per technology ----
# scores_tidy[scores_tidy == "I don't know"]
# scores_tidy[scores_tidy == "N/A"] 

scores_tidy_long %>% 
  group_by(technology, criterion) %>% 
  count(rank) %>% 
  filter(rank == "I don't know" | rank == "N/A") -> unknown_criteria

scores_tidy_long %>% 
  group_by(technology) %>% 
  count(rank) %>% 
  filter(rank == "I don't know" | rank == "N/A") %>%
  summarise(tot = sum(n)) -> unknown

unknown %>% 
  ggplot(aes(reorder(technology, tot), tot)) + 
  geom_col(aes(fill = tot)) +
  scale_fill_gradient2(low = "blue",
                       high = "red") +
  coord_flip() + 
  theme_minimal() + 
  scale_y_continuous(breaks = c(2,6,10,14,18)) +
  labs(x = "Technology", y = "No. of I don't know / N/A")

scores_tidy_long %>% 
  filter(rank == "I don't know" | rank == "N/A") %>%
  group_by(technology, rank) %>% 
  count(rank) %>% 
  summarise(tot = sum(n)) -> unknown_ranks
unknown_ranks %>% 
  ggplot(aes(reorder(technology, tot), tot, fill=rank)) + 
  geom_bar(position="stack", stat="identity") + 
  scale_fill_manual(values = c("#0073C2FF", "#EFC000FF")) +
  # geom_col(aes(fill = tot)) +
  # scale_fill_gradient2(low = "blue",
  # high = "red") +
  coord_flip() + 
  theme_minimal() + 
  scale_y_continuous(breaks = c(2,6,10,14,18,22)) +
  labs(x = "Technology", y = "No. of I don't know / N/A") +
  theme(legend.title = element_blank())
ggsave("round3_consensus_building/summary_unknowns_detailed.tiff",
       dpi=300, compression = 'lzw')


## ---- basic stats ----
# per criteria, regardless of technology
scores_num_long %>% 
  group_by(criterion) %>% 
  summarise(min = min(rank, na.rm = TRUE),
            q1 = quantile(rank, 0.25, na.rm = TRUE),
            median = median(rank, na.rm = TRUE),
            mean = mean(rank, na.rm=TRUE),
            q3 = quantile(rank, 0.75, na.rm = TRUE),
            max = max(rank, na.rm = TRUE))

scores_num_long %>% 
  group_by(technology, criterion) %>% 
  summarise(min = min(rank, na.rm = TRUE),
            q1 = quantile(rank, 0.25, na.rm = TRUE),
            median = median(rank, na.rm = TRUE),
            mean = mean(rank, na.rm=TRUE),
            q3 = quantile(rank, 0.75, na.rm = TRUE),
            max = max(rank, na.rm = TRUE))


## ---- stats and plots per criteria ----
dim(scores_num_long)
# regardless of technology
scores_num_long %>% 
  ggplot(aes(x=criterion, y=rank)) + 
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  geom_violin() 
ggsave("round3_consensus_building/criteria_violin.tiff",
       dpi=300, compression = 'lzw')
# balloon plots
scores_num_long %>% 
  group_by(criterion, rank) %>% 
  summarise(Freq=n()) -> pp
# dim(pp)
ggballoonplot(pp, x = "criterion", y = "rank",
              fill = "Freq", size = "Freq",
              ggtheme = theme_gray()) +
  scale_y_continuous(limits = c(1,5))
ggsave("round3_consensus_building/criteria_balloon.tiff",
       dpi=300, compression = 'lzw')

scores_num %>%
  select(audience:improve_curation) %>%
  t() -> scores_matrix
# icc(as.data.frame(scores_matrix), type="consistency",
#     model="twoway", unit = "average") -> icc_crit
kripp.alpha(t(scores_matrix), method = "ordinal") -> kripp_alpha_crit
kripp_alpha_crit # doesn't make sense to measure agreement across technologies
# kappam.fleiss(scores_matrix)

## ---- stats and plots per tech ----

i = 1
distinct(scores_num["technology"]) -> techs

icc_value <- vector("numeric",dim(techs)[1])
icc_lbound <- vector("numeric",dim(techs)[1])
icc_ubound <- vector("numeric",dim(techs)[1])
kripp_value <- vector("numeric",dim(techs)[1])
fleiss_value <- vector("numeric",dim(techs)[1])
fleiss_z <- vector("numeric",dim(techs)[1])
fleiss_p <- vector("numeric",dim(techs)[1])
n <- vector("numeric",dim(techs)[1])
anova.s <- vector("list",dim(techs)[1])
p_anova <- vector("numeric",dim(techs)[1])

while (i <= dim(techs)[1]) {
  tech_plot <- techs[[i,1]]
  
  # grouped violinplots
  scores_num_long %>%
    filter(technology == tech_plot) %>%
    ggplot(aes(x=criterion, y=rank)) +
    scale_x_discrete(guide = guide_axis(angle = 45)) +
    labs(title = tech_plot) +
    geom_violin()
  ggsave(paste("round3_consensus_building/figs_techs/",tech_plot,"_violin.tiff",sep=""),
         dpi=300, compression = 'lzw')

  # balloon plots
  scores_num_long %>%
    group_by(technology, criterion, rank) %>%
    summarise(Freq=n()) %>%
    filter(technology == tech_plot) -> pp
  # dim(pp)
  ggballoonplot(pp, x = "criterion", y = "rank",
                fill = "Freq", size = "Freq",
                ggtheme = theme_gray()) +
    scale_y_continuous(limits = c(1,5)) +
    labs(title = tech_plot)
  ggsave(paste("round3_consensus_building/figs_techs/",tech_plot,"_balloon.tiff",sep=""),
         dpi=300, compression = 'lzw')
  
  ## ---- icc, Krippendorf's , Fleiss ----
  scores_num %>% 
    filter(technology == tech_plot) %>% 
    select(audience:improve_curation) %>% 
    t() -> scores_matrix
  
  # Fleiss doesn't cope well with NAs
  matrix <- na.omit(scores_matrix)
  if (dim(matrix)[1] != 0) {
    kappam.fleiss(matrix) -> fleiss_tech
    fleiss_value[i] <- fleiss_tech$value
    fleiss_z[i] <- fleiss_tech$statistic
    fleiss_p[i] <- fleiss_tech$p.value
  } else {
    fleiss_value[i] <- NA
    fleiss_z[i] <- NA
    fleiss_p[i] <- NA
  }
  
    icc(as.data.frame(scores_matrix), type="consistency",
        model="twoway", unit = "average") -> icc_tech
    kripp.alpha(t(scores_matrix), method = "ordinal") -> kripp_alpha_tech
    
    ## ---- anova ----
    na.omit(scores_num_long) %>% 
      filter(technology == tech_plot) -> dataset_long
    lmer(rank ~ coder + (1 | criterion), data = dataset_long, REML=TRUE) -> mod
    # summary(lmer(rank ~ coder + (1 | criterion), data = dataset_long, REML=TRUE)) -> mod.summary
    anova(lmer(rank ~ coder + (1 | criterion), data = dataset_long, REML=TRUE)) -> anova.s[[i]]
    qf(p=.05, df1=8, df2=dim(mod@frame)[1]-9, lower.tail=FALSE) -> threshold
    # The degrees of freedom for the numerator are the degrees of freedom for the between group (k-1)
    # and the degrees of freedom for the denominator are the degrees of freedom 
    # for the within group (N-k)
    if (anova.s[[i]]$`F value` > threshold) p_anova[i] <- "< 0.05" else p_anova[i] <- NA

    icc_value[i] <- icc_tech$value
    icc_lbound[i] <- icc_tech$lbound
    icc_ubound[i] <- icc_tech$ubound
    kripp_value[i] <- kripp_alpha_tech$value
    n[i] <- kripp_alpha_tech$raters
  
  i = i + 1
} 

data.frame(
  technology = techs[[1]],
  icc = icc_value,
  icc.lbound = icc_lbound,
  icc.ubound = icc_ubound,
  kripp = kripp_value,
  fleiss = fleiss_value,
  fleiss.z = fleiss_z,
  fleiss.p = fleiss_p,
  p.anova = p_anova,
  num = n
) -> irr_table_temp

left_join(irr_table_temp, summary_coders_tech) %>% 
  rename(no.coders = n) -> irr_table
write.csv(irr_table, "round3_consensus_building/output/irr_table_consensus.csv")
write_xlsx(irr_table, "round3_consensus_building/output/irr_table_consensus.xlsx")

## Fleiss: a significant p-value means the stat is significantly different from 0 (agreement)
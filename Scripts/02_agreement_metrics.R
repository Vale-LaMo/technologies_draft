#############################################+
## Agreement metrics
## Valentina LM for the CostAction Team
## First draft: 2022-04-25
## Revised: 2024-08-26
#############################################+

### ---- Load Required Packages ----
library(irr)        # For inter-rater reliability calculations
library(lme4)       # For linear mixed-effects models


Round = "Before" # change this to calculate metrics for the different rounds or for the pooled assessments
# possible values: Before, After, All


## ---- consistency/agreement metrics per tech ----

# Please note:
# We are assessing agreement/consistency for All Criteria Together:
# ICC is calculated across all criteria as a single outcome.
# This is useful because we're interested in the general agreement or consistency between raters
# across the full set of criteria.
# For Each Criterion Separately: It may be more insightful to calculate ICC for each individual criterion
# if we'd be interested in how raters' consistency varies across different aspects of the technologies.

##---- Check on techs with no assessment or only one assessemnt in one round ----
# Rational: If a technology was only scored in the first round, there will be no variability
# in ratings for that technology in the second round. ICC relies on variability in scores across
# raters and rounds. If a technology has no scores in the second round, ICC cannot be computed
# for that round, or it may be misleading because there's no comparison between rounds.
# For ICC calculations, a technology with only one round of ratings won't allow for proper
# measurement of consistency or agreement across rounds.
# As a result, we exclude those technologies from our ICC analysis.

# Subset data for techs rated in both rounds
shared_techs_data <- scores_num_long %>% 
  filter(technology %in% intersect(unique(scores_num_long$technology[scores_num_long$round == "Before"]),
                                   unique(scores_num_long$technology[scores_num_long$round == "After"])))
unique(shared_techs_data$technology) # 34 technology, we lose 3
# Further filter for more than two coders
excluded_techs <- as.data.frame(table(shared_techs_data$technology, shared_techs_data$round)) %>%
    filter(Freq <= 18) # we lose other 12, beca
names(excluded_techs) <- c("technology", "round", "Freq")

# Filter using semi_join
filtered_techs <- shared_techs_data %>%
  anti_join(excluded_techs, by = "technology")

i = 1

# techs <- data.frame(unique(scores_num$technology))
techs <- data.frame(unique(filtered_techs$technology))

icc_value <- vector("numeric",dim(techs)[1])
icc_lbound <- vector("numeric",dim(techs)[1])
icc_ubound <- vector("numeric",dim(techs)[1])
icc_value_3 <- vector("numeric",dim(techs)[1])
icc_lbound_3 <- vector("numeric",dim(techs)[1])
icc_ubound_3 <- vector("numeric",dim(techs)[1])
kripp_value <- vector("numeric",dim(techs)[1])
fleiss_value <- vector("numeric",dim(techs)[1])
fleiss_z <- vector("numeric",dim(techs)[1])
fleiss_p <- vector("numeric",dim(techs)[1])
n <- vector("numeric",dim(techs)[1])
mod <- vector("list",dim(techs)[1])
anova.s <- vector("list",dim(techs)[1])
threshold <- vector("numeric",dim(techs)[1])
p_anova <- vector("character",dim(techs)[1])

scores_num_clean <- scores_num %>% 
  # Remove rows where all values in selected columns are NA
  filter(!if_all(audience:improve_flow, is.na))

while (i <= dim(techs)[1]) {
  tech_plot <- techs[[i,1]]
  
  ## ---- icc, Krippendorf's , Fleiss ----
  # scores_num %>% 
  #   filter(technology == tech_plot) %>% 
  #   select(audience:improve_curation) %>% 
  #   t() -> scores_matrix
  
  if(Round == "Before") {
    scores_num_clean %>% 
      filter(technology == tech_plot, round == "Before") %>% 
      # dplyr::select(audience:improve_curation) %>% 
      dplyr::select(audience:improve_flow) %>% # improve_curation has been removed because of too many NAs
      t() -> scores_matrix
  } else {
    if(Round == "After") {
      scores_num_clean %>% 
        filter(technology == tech_plot, round == "After") %>% 
        # dplyr::select(audience:improve_curation) %>% 
        dplyr::select(audience:improve_flow) %>% # improve_curation has been removed because of too many NAs
        t() -> scores_matrix
    } else {
      scores_num_clean %>% 
        filter(technology == tech_plot) %>% 
        # dplyr::select(audience:improve_curation) %>% 
        dplyr::select(audience:improve_flow) %>% # improve_curation has been removed because of too many NAs
        t() -> scores_matrix
    }
    
  }
  
  # Fleiss doesn't cope well with NAs
  matrix <- na.omit(scores_matrix)
  if (dim(matrix)[1] != 0 & dim(scores_matrix)[2] > 2) {
    kappam.fleiss(matrix) -> fleiss_tech
    fleiss_value[i] <- fleiss_tech$value
    fleiss_z[i] <- fleiss_tech$statistic
    fleiss_p[i] <- fleiss_tech$p.value
  } else {
    fleiss_value[i] <- NA
    fleiss_z[i] <- NA
    fleiss_p[i] <- NA
  }
  
  # one-way ICC
  if (dim(matrix)[1] != 0 & dim(scores_matrix)[2] > 2) {
    icc(as.data.frame(scores_matrix), type="consistency", model="oneway", unit = "average") -> icc_tech
    icc_value[i] <- icc_tech$value
    icc_lbound[i] <- icc_tech$lbound
    icc_ubound[i] <- icc_tech$ubound
  } else {
    icc_value[i] <- NA
    icc_lbound[i] <- NA
    icc_ubound[i] <- NA
  }
  
  # The presence of assessors in both rounds does not directly affect the ICC calculation for each round
  # because ICC is computed within each dataset independently
  
  # # icc_tech_2 Two-Way Random Effects Model is not implemented because it requires a fully crossed design
  # # Two-Way Mixed Effects Model (icc3)
  # if (dim(matrix)[1] != 0 & dim(scores_matrix)[2] > 2) {
  #   icc(as.data.frame(scores_matrix), type="agreement", model="oneway", unit = "average") -> icc_tech_3
  #   icc_value_3[i] <- icc_tech_3$value
  #   icc_lbound_3[i] <- icc_tech_3$lbound
  #   icc_ubound_3[i] <- icc_tech_3$ubound
  # } else {
  #   icc_value_3[i] <- NA
  #   icc_lbound_3[i] <- NA
  #   icc_ubound_3[i] <- NA
  # }
  
  if (dim(matrix)[1] != 0 & dim(scores_matrix)[2] > 2) {
    kripp.alpha(t(scores_matrix), method = "ordinal") -> kripp_alpha_tech
    kripp_value[i] <- kripp_alpha_tech$value
    n[i] <- kripp_alpha_tech$raters
  } else {
    kripp_value[i] <- NA
    n[i] <- NA
  }
  
  ## ---- anova ----
  na.omit(scores_num_long) %>%
    filter(technology == tech_plot) -> dataset_long
  if(Round == "Before") dataset_long %>% filter(round == "Before") -> dataset_long
  if(Round == "After") dataset_long %>% filter(round == "After") -> dataset_long
  # if(dim(dataset_long)[2]>7) {
    lmer(rank ~ coder + (1 | criterion), data = dataset_long, REML=TRUE) -> mod[[i]]
    # summary(lmer(rank ~ coder + (1 | criterion), data = dataset_long, REML=TRUE)) -> mod.summary
    # anova(lmer(rank ~ coder + (1 | criterion), data = dataset_long, REML=TRUE)) -> anova.s[[i]]
    anova(mod[[i]]) -> anova.s[[i]]
    qf(p=.05, df1=8, df2=dim(mod[[i]]@frame)[1]-9, lower.tail=FALSE) -> threshold[i]
    # The degrees of freedom for the numerator are the degrees of freedom for the between group (k-1)
    # and the degrees of freedom for the denominator are the degrees of freedom
    # for the within group (N-k)
    if (anova.s[[i]]$`F value` > threshold[i]) p_anova[i] <- "< 0.05" else p_anova[i] <- 'not sign.'
  # } else {
  #   p_anova[i] <- NA
  # }
  
  i = i + 1
} 

data.frame(
  technology = techs[[1]],
  icc = icc_value,
  icc.lbound = icc_lbound,
  icc.ubound = icc_ubound,
  # icc_3 = icc_value_3,
  # icc.lbound_3 = icc_lbound_3,
  # icc.ubound_3 = icc_ubound_3,
  kripp = kripp_value,
  fleiss = fleiss_value,
  fleiss.z = fleiss_z,
  fleiss.p = fleiss_p,
  p.anova = p_anova,
  num = n
) -> irr_table_temp

left_join(irr_table_temp, summary_coders_tech) %>% 
  rename(no.coders = n) -> irr_table
# write.csv(irr_table, "round1/output/irr_table_new.csv")
# if(Round == "All") write.csv(irr_table, "output/irr_table_pooled_20240826.csv")
# if(Round == "Before") write.csv(irr_table, "output/irr_table_Before_20240826.csv")
# if(Round == "After") write.csv(irr_table, "output/irr_table_After_20240826.csv")

# if(Round == "All") write.csv(irr_table, "output/irr_table_pooled_20241025.csv")
# if(Round == "Before") write.csv(irr_table, "output/irr_table_Before_20241025.csv")
# if(Round == "After") write.csv(irr_table, "output/irr_table_After_20241025.csv")

if(Round == "Before") write.csv(irr_table, "output/irr_table_Before_20250217.csv")
if(Round == "After") write.csv(irr_table, "output/irr_table_After_20250217.csv")

read.csv("output/irr_table_After_20250217.csv") -> prova


## Fleiss: a significant p-value means the stat is significantly different from 0 (agreement)

##---- Check on participation to both rounds ----
# Subset data for assessors who rated in both rounds
shared_assessors_data <- scores_num_long %>% 
  filter(coder %in% intersect(unique(scores_num_long$coder[scores_num_long$round == "Before"]),
                              unique(scores_num_long$coder[scores_num_long$round == "After"])))
unique(shared_assessors_data$coder) # length should be 16

# Fit a mixed model with round as a fixed effect and assessor as a random effect
library(lmerTest)
model_shared_assessors <- lmer(rank ~ round*coder + (1 | criterion), data = shared_assessors_data)

# Check if there is an interaction between round and coder
summary(model_shared_assessors)
library(broom.mixed)
print(tidy(model_shared_assessors), n = 34)

# Interpretation
# No significant interaction: If the p-values for most or all coders are not significant,
# this suggests that there are no strong individual differences in the ratings between rounds.

# This result suggests that the effect of the assessors is consistent across the rounds,
# meaning that assessors' behavior (or ratings) doesn't vary in a way that depends on whether
# they are rating in the "Before" or "After" round. This would imply that the assessors' ratings
# do not introduce a bias when comparing the ICC before and after,
# as there is no differential effect by round.
# In other words, Since we did not find significant interactions, 
# the presence of some assessors in both rounds does not seem to distort
# the comparison of the ICCs between the rounds. 
# We can then safely interpret the ICCs for each round without worrying about assessors' influence
# on the comparison itself.
# -> the assessors' repeated participation is unlikely to affect our analysis of the changes
# in ICC across the rounds.

# the differences between raters' behaviors are captured in the ICC calculation itself






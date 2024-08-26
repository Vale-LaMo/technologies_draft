#############################################+
## Agreement metrics
## Valentina LM for the CostAction Team
## First draft: 2022-04-25
## Revised: 2024-08-26
#############################################+

### ---- Load Required Packages ----
library(irr)        # For inter-rater reliability calculations
library(lme4)       # For linear mixed-effects models


Round = "All" # change this to calculate metrics for the different rounds or for the pooled assessments
# possible values: Before, After, All


## ---- agreement metrics per tech ----

i = 1

techs <- data.frame(unique(scores_num$technology))

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
  
  ## ---- icc, Krippendorf's , Fleiss ----
  # scores_num %>% 
  #   filter(technology == tech_plot) %>% 
  #   select(audience:improve_curation) %>% 
  #   t() -> scores_matrix
  
  if(Round == "Before") {
    scores_num %>% 
      filter(technology == tech_plot, round == "Before") %>% 
      select(audience:improve_curation) %>% 
      t() -> scores_matrix
  } else {
    if(Round == "After") {
      scores_num %>% 
        filter(technology == tech_plot, round == "After") %>% 
        select(audience:improve_curation) %>% 
        t() -> scores_matrix
    } else {
      scores_num %>% 
        filter(technology == tech_plot) %>% 
        select(audience:improve_curation) %>% 
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
  
  if (dim(matrix)[1] != 0 & dim(scores_matrix)[2] > 2) {
    icc(as.data.frame(scores_matrix), type="consistency", model="twoway", unit = "average") -> icc_tech
    icc_value[i] <- icc_tech$value
    icc_lbound[i] <- icc_tech$lbound
    icc_ubound[i] <- icc_tech$ubound
  } else {
    icc_value[i] <- NA
    icc_lbound[i] <- NA
    icc_ubound[i] <- NA
  }
  
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
  if(dim(dataset_long)[2]>7) {
    lmer(rank ~ coder + (1 | criterion), data = dataset_long, REML=TRUE) -> mod
    # summary(lmer(rank ~ coder + (1 | criterion), data = dataset_long, REML=TRUE)) -> mod.summary
    anova(lmer(rank ~ coder + (1 | criterion), data = dataset_long, REML=TRUE)) -> anova.s[[i]]
    qf(p=.05, df1=8, df2=dim(mod@frame)[1]-9, lower.tail=FALSE) -> threshold
    # The degrees of freedom for the numerator are the degrees of freedom for the between group (k-1)
    # and the degrees of freedom for the denominator are the degrees of freedom
    # for the within group (N-k)
    if (anova.s[[i]]$`F value` > threshold) p_anova[i] <- "< 0.05" else p_anova[i] <- NA
  } else {
    p_anova[i] <- NA
  }
  
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
# write.csv(irr_table, "round1/output/irr_table_new.csv")
if(Round == "All") write.csv(irr_table, "output/irr_table_pooled_20240826.csv")
if(Round == "Before") write.csv(irr_table, "output/irr_table_Before_20240826.csv")
if(Round == "After") write.csv(irr_table, "output/irr_table_After_20240826.csv")

## Fleiss: a significant p-value means the stat is significantly different from 0 (agreement)

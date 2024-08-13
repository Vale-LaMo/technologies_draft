
library(irr)        # For inter-rater reliability calculations
library(lme4)       # For linear mixed-effects models


scores_num %>%
  select(audience:improve_curation) %>%
  t() -> scores_matrix

## ---- stats and plots per tech ----

i = 1

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
  # ggsave(paste("round1/figs_techs/",tech_plot,"_violin.tiff",sep=""),
  #        dpi=300, compression = 'lzw')
  
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
  # ggsave(paste("round1/figs_techs/",tech_plot,"_balloon.tiff",sep=""),
  #        dpi=300, compression = 'lzw')
  
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
write.csv(irr_table, "round1/output/irr_table_new.csv")

## Fleiss: a significant p-value means the stat is significantly different from 0 (agreement)

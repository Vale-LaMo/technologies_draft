#############################################+
## Ordination
## Valentina LM for the CostAction Team
## First draft: 2022-05-03
#############################################+

# NMDS is a very flexible distance-based ordination technique.
# Advantage over other methods: objects that are similar in real life are closer in the NMDS space
# Warning: axes have no meaning


## ---- packages ----
library(tidyverse)
library(ggpubr)
library(vegan)

## ---- data ----
load("output/scores_num.RData")

## ---- NMDS overall ----
dataset_tot <- na.omit(scores_num)
dataset_tot_NMDS <-  metaMDS(dataset_tot[,5:13],
                             k=2,
                             trymax=100)
ordiplot(dataset_tot_NMDS,type="n")
ordihull(dataset_tot_NMDS,
         groups = dataset_tot$technology,
         draw = "polygon", col = "grey90", label = T)
## add points for criteria and assessments
# orditorp(dataset_tot_NMDS, display="species") # criteria
# orditorp(dataset_tot_NMDS, display="sites",col="red",air=0.01) # assessments

## investigate the criteria that drive the technologies distribution pattern (intrinsic criteria)
dataset.spp.fit <- envfit(dataset_tot_NMDS, 
                          dataset_tot, 
                          permutations = 999)
names(dataset.spp.fit) 
dataset.spp.fit$vectors # they are all signif., not very informative



## ---- NMDS overall ggplots ----
site.scrs <- as.data.frame(scores(dataset_tot_NMDS, 
                                  display = "sites")) #save NMDS results (coords) into dataframe
site.scrs <- cbind(site.scrs, 
                   Technology = dataset_tot$technology, 
                   coder = dataset_tot$coder) #add grouping variables to dataframe
head(site.scrs) # x,y points for each site

spp.scrs <- as.data.frame(scores(dataset.spp.fit, 
                                 display = "vectors")) #save criteria intrinsic values into dataframe
spp.scrs <- cbind(spp.scrs, 
                  Criteria = rownames(spp.scrs)) # add criteria names to dataframe
spp.scrs <- cbind(spp.scrs, 
                  pval = dataset.spp.fit$vectors$pvals) # add pvalues to dataframe so you can select species(criteria) which are significant
#spp.scrs<- cbind(spp.scrs, abrev = abbreviate(spp.scrs$Species, minlength = 6)) #abbreviate species names
sig.spp.scrs.overall <- subset(spp.scrs, pval<=0.05) #subset data to show species significant at 0.05
head(spp.scrs)

nmds.plot.overall <- ggplot(site.scrs, aes(x=NMDS1, y=NMDS2))+ #sets up the plot
  geom_point(aes(NMDS1, NMDS2, colour = factor(site.scrs$Technology), size = 2),
             show.legend = FALSE)+ # adds sites (assessment) points to plot, colour determined by technology
  coord_fixed() +
  theme_classic() + 
  theme(panel.background = element_rect(fill = NA, 
                                        colour = "black", 
                                        size = 1, 
                                        linetype = "solid"))+
  labs(colour = "Technology") # add legend labels
  # theme(legend.position = "right", legend.text = element_text(size = 12), legend.title = element_text(size = 12), axis.text = element_text(size = 10)) # add legend at right of plot

nmds.plot.overall # + labs(title = "Basic ordination plot")
#Significant Species (Criteria)
nmds.plot.overall +
  geom_segment(data = sig.spp.scrs.overall, aes(x = 0, xend=NMDS1, y=0, yend=NMDS2), arrow = arrow(length = unit(0.25, "cm")), colour = "grey10", lwd=0.3) + #add vector arrows of significant species
  ggrepel::geom_text_repel(data = sig.spp.scrs.overall, aes(x=NMDS1, y=NMDS2, label = Criteria), cex = 3, direction = "both", segment.size = 0.25)+ #add labels for species, use ggrepel::geom_text_repel so that labels do not overlap
  labs(title = "Ordination with criteria vectors")
# ggsave("round1/ordination.tiff",
#        dpi=300, compression = 'lzw')


## ---- NMDS single techs ggplots ----
# based on overall NMDS

techs <- unique(scores_num$technology)
i = 1
while (i <= length(techs)[1]) {
  tech_plot <- techs[i]
  site.scrs.s <- filter(site.scrs, Technology == tech_plot)
  
  nmds.plot <- ggplot(site.scrs.s, aes(x=NMDS1, y=NMDS2))+ #sets up the plot
    geom_point(aes(NMDS1, NMDS2, colour = factor(site.scrs.s$Technology), size = 2),
               show.legend = FALSE)+ #adds site points to plot, shape determined by Landuse, colour determined by Management
    coord_fixed()+
    theme_classic()+ 
    theme(panel.background = element_rect(fill = NA, colour = "black", size = 1, linetype = "solid"))+
    labs(colour = "Technology") +
    scale_x_continuous(limits = c(-0.5,0.9)) +
    scale_y_continuous(limits = c(-0.5,0.5)) +# add legend labels
    geom_text(data = NMDS.mean.tech, aes(label = group),
              position = position_dodge(width=0.9),  size=2)
  
  nmds.plot +
    # geom_text(label=site.scrs.s$coder, hjust=0, vjust=0, size=2) +
    labs(title = paste("Ordination with criteria vectors: ",tech_plot,sep=""))
  ggsave(paste("round1/figs_techs/ordination_",tech_plot,".tiff",sep=""), 
                dpi=300, compression = 'lzw')
  
  i = i + 1
}
  
# ## ---- NMDS single techs ----
# # not used
# dataset_NMDS <- vector("list",dim(techs)[1])
# dataset.spp.fit <- vector("list",dim(techs)[1])
# dataset <- vector("list",dim(techs)[1])
# 
# i = 1
# while (i <= dim(techs)[1]) {
#   tech_plot <- techs[[i,1]]
#   dataset_tot %>% 
#     filter(technology == tech_plot) -> dataset[[i]]
#   dataset_NMDS[[i]] = metaMDS(dataset[[i]][,4:12],k=2,trymax=100)
#   # plot(example_NMDS)
#   # orditorp(example_NMDS,display="sites",col="red",air=0.01)
#   ordiplot(dataset_NMDS[[i]],type="n")
#   ordihull(dataset_NMDS[[i]],groups=dataset[[i]]$technology,draw="polygon",col="grey90",label=T)
#   # ordiellipse(dataset_NMDS,groups=dataset$technology,draw="polygon",col="grey90",label=T)
#   # orditorp(example_NMDS,display="species")
#   # orditorp(example_NMDS,display="sites")
#   
#   dataset.spp.fit[[i]] <- envfit(dataset_NMDS[[i]], 
#                                  dataset[[i]], permutations = 999)
#   # head(dataset.spp.fit)
#   
#   i=i+1
# }


## ---- NMDS overall ggplots ellipses ----
#data for ellipse, in this case using the management factor
df_ell <- data.frame() #sets up a data frame before running the function.
for(g in unique(site.scrs$Technology)){
  df_ell <- rbind(df_ell, 
                  cbind(as.data.frame(with(site.scrs[site.scrs$Technology==g,],
                                           veganCovEllipse(cov.wt(cbind(NMDS1,NMDS2),
                                                                  wt=rep(1/length(NMDS1),
                                                                         length(NMDS1)))$cov,
                                                           center=c(mean(NMDS1),mean(NMDS2))))) ,Technology=g))
}

# data for labelling the ellipse
NMDS.mean.tech = aggregate(site.scrs[ ,c("NMDS1", "NMDS2")], 
                           list(group = site.scrs$Technology), mean)

# nmds.plot.overall + 
#   geom_path(data = NMDS.mean.tech, 
#             aes(x = NMDS1, y = NMDS2, group = site.scrs$Technology))
# #this is the ellipse, seperate ones by Site. 

# plotting the centers of the ellipses
nmds.plot.overall1 <- ggplot(NMDS.mean.tech, aes(x=NMDS1, y=NMDS2, label=NMDS.mean.tech$group)) + #sets up the plot
  geom_point(aes(NMDS1, NMDS2, colour = factor(group), size = 2),
             show.legend = FALSE)+ # adds sites (assessment) points to plot, colour determined by technology
  coord_fixed() +
  theme_classic() + 
  theme(panel.background = element_rect(fill = NA, 
                                        colour = "black", 
                                        size = 1, 
                                        linetype = "solid")) +
  geom_text(hjust=0, vjust=0, size=2) +
  # geom_text(data = a, aes(label = mpg), 
  #           position = position_dodge(width=0.9),  size=geom.text.size) + 
  labs(colour = "group") # add legend labels
# theme(legend.position = "right", legend.text = element_text(size = 12), legend.title = element_text(size = 12), axis.text = element_text(size = 10)) # add legend at right of plot
nmds.plot.overall1 # + labs(title = "Basic ordination plot")
# ggsave("round1/ordination_centers.tiff",
#        dpi=300, compression = 'lzw')

nmds.plot.overall2 <- ggplot(NMDS.mean.tech, aes(x=NMDS1, y=NMDS2)) + #sets up the plot
  geom_point(aes(NMDS1, NMDS2, colour = factor(group), size = 2),
             show.legend = FALSE)+ # adds sites (assessment) points to plot, colour determined by technology
  coord_fixed() +
  theme_classic() + 
  theme(panel.background = element_rect(fill = NA, 
                                        colour = "black", 
                                        size = 1, 
                                        linetype = "solid")) +
  # geom_text(hjust=0, vjust=0, size=2) +
  # geom_text(data = a, aes(label = mpg), 
  #           position = position_dodge(width=0.9),  size=geom.text.size) + 
  labs(colour = "group") # add legend labels

nmds.plot.overall2 +
  geom_segment(data = sig.spp.scrs.overall, aes(x = 0, xend=NMDS1, y=0, yend=NMDS2), arrow = arrow(length = unit(0.25, "cm")), colour = "grey10", lwd=0.3) + #add vector arrows of significant species
  ggrepel::geom_text_repel(data = sig.spp.scrs.overall, aes(x=NMDS1, y=NMDS2, label = Criteria), cex = 3, direction = "both", segment.size = 0.25)+ #add labels for species, use ggrepel::geom_text_repel so that labels do not overlap
  labs(title = "Ordination with criteria vectors") -> plot2
plot2 + 
  geom_text(data = NMDS.mean.tech, aes(label = group),
            position = position_dodge(width=0.9),  size=2)
# ggsave("round1/ordination_centers_criteria.tiff",
#        dpi=300, compression = 'lzw')



## ---- function for ellipsess ----
veganCovEllipse <- function (cov, center = c(0, 0), scale = 1, npoints = 100) 
{
  theta <- (0:npoints) * 2 * pi/npoints
  Circle <- cbind(cos(theta), sin(theta))
  t(center + scale * t(Circle %*% chol(cov)))
}




## ---- new -----
weighted_scores_criteria <- read.csv("output/weighted_scores_criteria.csv")[,-13]
dataset_tot_NMDS <-  metaMDS(weighted_scores_criteria[,4:12],
                             k=2,
                             trymax=100)
ordiplot(dataset_tot_NMDS,type="n")
ordihull(dataset_tot_NMDS,
         groups = weighted_scores_criteria$technology,
         draw = "polygon", col = "grey90", label = T)
## add points for criteria and assessments
# orditorp(dataset_tot_NMDS, display="species") # criteria
# orditorp(dataset_tot_NMDS, display="sites",col="red",air=0.01) # assessments

## investigate the criteria that drive the technologies distribution pattern (intrinsic criteria)
dataset.spp.fit <- envfit(dataset_tot_NMDS, 
                          weighted_scores_criteria[,4:12], 
                          permutations = 999)
names(dataset.spp.fit) 
dataset.spp.fit$vectors # they are all signif., not very informative



## ---- NMDS overall ggplots ----
site.scrs <- as.data.frame(scores(dataset_tot_NMDS, 
                                  display = "sites")) #save NMDS results (coords) into dataframe
site.scrs <- cbind(site.scrs, 
                   Technology = weighted_scores_criteria$technology, 
                   coder = dataset_tot$coder) #add grouping variables to dataframe
head(site.scrs) # x,y points for each site

## ---- NMDS overall ggplots ellipses ----
#data for ellipse, in this case using the management factor
df_ell <- data.frame() #sets up a data frame before running the function.
for(g in unique(site.scrs$Technology)){
  df_ell <- rbind(df_ell, 
                  cbind(as.data.frame(with(site.scrs[site.scrs$Technology==g,],
                                           veganCovEllipse(cov.wt(cbind(NMDS1,NMDS2),
                                                                  wt=rep(1/length(NMDS1),
                                                                         length(NMDS1)))$cov,
                                                           center=c(mean(NMDS1),mean(NMDS2))))) ,Technology=g))
}

# data for labelling the ellipse
NMDS.mean.tech = aggregate(site.scrs[ ,c("NMDS1", "NMDS2")], 
                           list(group = site.scrs$Technology), mean)



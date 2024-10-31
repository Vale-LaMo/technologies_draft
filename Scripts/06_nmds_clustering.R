#############################################+
## Ordination
## Valentina LM for the CostAction Team
## First draft: 2022-05-03
#############################################+

# NMDS is a very flexible distance-based ordination technique.
# Advantage over other methods: objects that are similar in real life are closer in the NMDS space
# Warning: axes have no meaning


## ---- SEE README.md for guidance on interpretation ----


## ---- packages ----
library(tidyverse)
library(ggpubr)
library(vegan)

## ---- data ----
weighted_scores_criteria <- read.csv("output/weighted_scores_criteria.csv")
weighted_scores_criteria %>%
  dplyr::select(-no.coders) -> data_clustering


## ---- NMDS ----
dataset_tot_NMDS <-  metaMDS(data_clustering[ ,3:11],
                             k=2,
                             trymax=100)

## investigate the criteria that drive the technologies distribution pattern (intrinsic criteria)
dataset.spp.fit <- envfit(dataset_tot_NMDS, 
                          dplyr::select(data_clustering, application:new_data), 
                          permutations = 999)
names(dataset.spp.fit) 
dataset.spp.fit$vectors # they are all significant



## ---- NMDS ggplots ----
site.scrs <- as.data.frame(scores(dataset_tot_NMDS, 
                                  display = "sites")) #save NMDS results (coords) into dataframe
site.scrs <- cbind(site.scrs, 
                   Technology = data_clustering$technology) #add grouping variables to dataframe
head(site.scrs) # x,y points for each site

spp.scrs <- as.data.frame(scores(dataset.spp.fit, 
                                 display = "vectors")) #save criteria intrinsic values into dataframe
spp.scrs <- cbind(spp.scrs, 
                  Criteria = rownames(spp.scrs)) # add criteria names to dataframe
spp.scrs <- cbind(spp.scrs, 
                  pval = dataset.spp.fit$vectors$pvals) # add pvalues to dataframe so you can select species(criteria) which are significant
#spp.scrs<- cbind(spp.scrs, abrev = abbreviate(spp.scrs$Species, minlength = 6)) #abbreviate species names
sig.spp.scrs.overall <- subset(spp.scrs, pval<=0.05) #subset data to show species significant at 0.05
sig.spp.scrs.overall
# changed to include improve curation, which is almost significant
sig.spp.scrs.overall <- subset(spp.scrs, pval<=0.1) #subset data to show species significant at 0.05
sig.spp.scrs.overall

# Baseline plot
# nmds.plot.overall <- ggplot(site.scrs, aes(x=NMDS1, y=NMDS2))+ #sets up the plot
#   geom_point(aes(NMDS1, NMDS2, colour = factor(site.scrs$Technology), size = 2),
#              show.legend = FALSE)+ # adds sites (assessment) points to plot, colour determined by technology
#   coord_fixed() +
#   theme_classic() + 
#   theme(panel.background = element_rect(fill = NA, 
#                                         colour = "black", 
#                                         size = 1, 
#                                         linetype = "solid"))+
#   labs(colour = "Technology") # add legend labels
#   # theme(legend.position = "right", legend.text = element_text(size = 12), legend.title = element_text(size = 12), axis.text = element_text(size = 10)) # add legend at right of plot
# 
# nmds.plot.overall # + labs(title = "Basic ordination plot")
# #Significant Species (Criteria)
# nmds.plot.overall +
#   geom_segment(data = sig.spp.scrs.overall, aes(x = 0, xend=NMDS1, y=0, yend=NMDS2), arrow = arrow(length = unit(0.25, "cm")), colour = "grey10", lwd=0.3) + #add vector arrows of significant species
#   ggrepel::geom_text_repel(data = sig.spp.scrs.overall, aes(x=NMDS1, y=NMDS2, label = Criteria), cex = 3, direction = "both", segment.size = 0.25)+ #add labels for species, use ggrepel::geom_text_repel so that labels do not overlap
#   labs(title = "Ordination with criteria vectors")


# Ensure ggplot2 and ggrepel packages are loaded
library(ggplot2)
library(ggrepel)


## ---- Perform hierarchical clustering and add it to NMDS ----

# Step 1: Perform clustering on NMDS coordinates to create groups
set.seed(123) # For reproducibility
n_clusters <- 4 # Set desired number of clusters
aggl.clust.c <- hcut(site.scrs[, c("NMDS1", "NMDS2")], k=n_clusters,
                     hc_func = "hclust",
                     hc_method = "ward.D2",
                     hc_metric = "euclidean") # eventually change to Euclidean, but then check all file names!!
# cut the tree and join cluster info with data
site.scrs$Cluster <- as.character(cutree(aggl.clust.c, k = n_clusters))

# Step 2: Fit criteria onto the NMDS solution
criteria_fit <- envfit(dataset_tot_NMDS, data_clustering[, 3:11], perm = 999)
# Extract coordinates of arrows
criteria_scores <- as.data.frame(scores(criteria_fit, display = "vectors"))
criteria_scores$Criterion <- rownames(criteria_scores)

# Step 3: Plot the NMDS with ellipses around each cluster
# viridis(n_clusters, option = "H", begin = 1, end = 0.66)
custom_colors <- c("1" = "#7A0403FF",   
                   "2" = "#F8BD39FF",   
                   "3" = "#CC2B04FF",
                   "4" = "#F76E1AFF")  
nmds.plot.overall <- ggplot(site.scrs, aes(x = NMDS1, y = NMDS2)) +
  geom_point(aes(colour = Cluster, size = 2), show.legend = FALSE) +
  geom_text_repel(aes(label = Technology), size = 3, box.padding = 0.5, max.overlaps = Inf) +
  stat_ellipse(aes(group = Cluster, colour = Cluster), type = "t", level = 0.95, linetype = "dashed") +
  coord_fixed() +
  theme_ipsum_ps() +
  theme(legend.position = "none") +
  labs(colour = "Cluster") +
  scale_colour_manual(values = custom_colors)
# Plot the NMDS
# check file name!
# tiff("figs/clustering_nmds/nmds.tiff", res=1000, width = 22, height = 27, units = "cm")
print(nmds.plot.overall)
# dev.off()

# Step 4: Plot NMDS with ellipses and arrows
scaling_factor <- 0.5
criteria_scores$Criterion <- c("Application", "Audience", "Engagement via feedback", "Engagement with others",
                               "Extend data", "Improve curation", "Improve flow", "Improve quality", "New data")
nmds.plot.overall <- ggplot(site.scrs, aes(x = NMDS1, y = NMDS2)) +
  geom_point(aes(colour = Cluster, size = 2), show.legend = FALSE) +
  # geom_text_repel(aes(label = Technology), size = 3, box.padding = 0.5, max.overlaps = Inf) +
  stat_ellipse(aes(group = Cluster, colour = Cluster), type = "t", level = 0.95, linetype = "dashed") +
  geom_segment(data = criteria_scores, aes(x = 0, y = 0, xend = NMDS1*scaling_factor, yend = NMDS2*scaling_factor), 
               arrow = arrow(length = unit(0.3, "cm")), colour = "#3E378FFF") +
  geom_text(data = criteria_scores, aes(x = NMDS1*scaling_factor, y = NMDS2*scaling_factor, label = Criterion), 
            colour = "#3E378FFF", size = 3, hjust = 0.4, vjust = 2) +
  coord_fixed() +
  theme_ipsum_ps() +
  theme(legend.position = "none") +
  labs(colour = "Cluster") +
  scale_color_manual(values = custom_colors) +
  xlim(c(-0.4,0.5))
# Plot the NMDS
# tiff("figs/clustering_nmds/nmds_vectors.tiff", res=1000, width = 22, height = 27, units = "cm")
print(nmds.plot.overall)
# dev.off()

# Step 5: Plot the corresponding tree
aggl.clust.c$labels <- site.scrs$Technology
# tiff("figs/clustering_nmds/nmds_hclust.tiff", res=1000, width = 22, height = 27, units = "cm")
fviz_dend(aggl.clust.c,
          rect = TRUE, rect_fill = TRUE,
          lower_rect = 0,
          # palette = "uchicago",
          palette = viridis(n_clusters, option = "H", begin = 1, end = 0.66),
          labels_track_height = 0.8,
          horiz = TRUE,
          cex = 0.8,
          # dLeaf = 15,
          main="Hierarchical clustering on NMDS",
          axes = F)
# dev.off()



## ---- Perform kmeans clustering and add it to NMDS ----

# Step 1: Perform clustering on NMDS coordinates to create groups
set.seed(123) # For reproducibility
n_clusters <- 4 # Set desired number of clusters
site.scrs$Cluster <- as.factor(kmeans(site.scrs[, c("NMDS1", "NMDS2")], centers = n_clusters)$cluster)

# Step 2: Fit criteria onto the NMDS solution
criteria_fit <- envfit(dataset_tot_NMDS, data_clustering[, 3:11], perm = 999)
# Extract coordinates of arrows
criteria_scores <- as.data.frame(scores(criteria_fit, display = "vectors"))
criteria_scores$Criterion <- rownames(criteria_scores)

# Step 3: Plot the NMDS with ellipses around each cluster
# viridis(n_clusters, option = "H", begin = 1, end = 0.66)
custom_colors <- c("1" = "#7A0403FF",   
                   "2" = "#F8BD39FF",   
                   "3" = "#CC2B04FF",
                   "4" = "#F76E1AFF")  
nmds.plot.overall <- ggplot(site.scrs, aes(x = NMDS1, y = NMDS2)) +
  geom_point(aes(colour = Cluster, size = 2), show.legend = FALSE) +
  geom_text_repel(aes(label = Technology), size = 3, box.padding = 0.5, max.overlaps = Inf) +
  stat_ellipse(aes(group = Cluster, colour = Cluster), type = "t", level = 0.95, linetype = "dashed") +
  coord_fixed() +
  theme_ipsum_ps() +
  theme(legend.position = "none") +
  labs(colour = "Cluster") +
  scale_colour_manual(values = custom_colors)
# Plot the NMDS
# check file name!
# tiff("figs/clustering_nmds/nmds_kmeans.tiff", res=1000, width = 22, height = 27, units = "cm")
print(nmds.plot.overall)
# dev.off()

# Step 4: Plot NMDS with ellipses and arrows
scaling_factor <- 0.5
criteria_scores$Criterion <- c("Application", "Audience", "Engagement via feedback", "Engagement with others",
                               "Extend data", "Improve curation", "Improve flow", "Improve quality", "New data")
nmds.plot.overall <- ggplot(site.scrs, aes(x = NMDS1, y = NMDS2)) +
  geom_point(aes(colour = Cluster, size = 2), show.legend = FALSE) +
  # geom_text_repel(aes(label = Technology), size = 3, box.padding = 0.5, max.overlaps = Inf) +
  stat_ellipse(aes(group = Cluster, colour = Cluster), type = "t", level = 0.95, linetype = "dashed") +
  geom_segment(data = criteria_scores, aes(x = 0, y = 0, xend = NMDS1*scaling_factor, yend = NMDS2*scaling_factor), 
               arrow = arrow(length = unit(0.3, "cm")), colour = "#3E378FFF") +
  geom_text(data = criteria_scores, aes(x = NMDS1*scaling_factor, y = NMDS2*scaling_factor, label = Criterion), 
            colour = "#3E378FFF", size = 3, hjust = 0.4, vjust = 2) +
  coord_fixed() +
  theme_ipsum_ps() +
  theme(legend.position = "none") +
  labs(colour = "Cluster") +
  scale_color_manual(values = custom_colors) +
  xlim(c(-0.4,0.5))

# Plot the NMDS
# tiff("figs/clustering_nmds/nmds_kmeans_vectors.tiff", res=1000, width = 22, height = 27, units = "cm")
print(nmds.plot.overall)
# dev.off()




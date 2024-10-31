#############################################+
## Ordination
## Valentina LM for the CostAction Team
## First draft: 2024-10-30
#############################################+

##---- load packages ----
library(tidyverse)
library(viridis)
library(hrbrthemes)
library(cluster) 
library(factoextra)
library(NbClust)
library(ggpubr)
library("reshape2")
library("purrr")
library("dendextend")
library(ggsci)
library(scales)

##---- load data ----
weighted_scores_criteria <- read.csv("output/weighted_scores_criteria.csv")
weighted_scores_criteria %>%
  dplyr::select(-no.coders) -> data_clustering


#----- Dissimilarity Matrix: Euclidean vs. Manhattan -----#
# to perform different types of hierarchical clustering
# package functions used: daisy(), diana(), clusplot()
eucl.dist <- daisy(data_clustering[ ,3:11], metric = c("euclidean"))
manh.dist <- daisy(data_clustering[ ,3:11], metric = c("manhattan"))

# identify patterns like clusters that are more rectangular or elongated,
# which might suggest using Manhattan distance
pairs(data_clustering[ ,3:11])

# Spherical clusters in PCA-transformed space may indicate that Euclidean distance
# is appropriate. Elongated or rectangular clusters can be better suited for
# Manhattan distance.
library(ggfortify)
pca_res <- prcomp(data_clustering[ ,3:11], scale. = TRUE)
autoplot(pca_res)

# Large outliers or widely varying feature ranges indicate that Manhattan distance
# might be more appropriate, as it is less sensitive to outliers and scale differences.
boxplot(data_clustering[ ,3:11])

# Strongly correlated features mean that distances will be affected by both magnitude
# and direction (fitting Euclidean), while weakly correlated features suggest
# Manhattan distance, as it’s less influenced by directionality.
mean(cor(data_clustering[ ,3:11]))

# overall, I'd chose the Manhattan distance (but I am going to do clustering with
# both and then test their performance to see if Manhattan is confirmed)

# class(gower.dist) 
## dissimilarity , dist
summary(eucl.dist)
summary(manh.dist)

##---- Manhattan clustering ----

dist_matrix <- manh.dist

#------------ DIVISIVE CLUSTERING ------------#
divisive.clust <- diana(as.matrix(dist_matrix), 
                        diss = TRUE, keep.diss = TRUE)
plot(divisive.clust, main = "Divisive")

#------------ AGGLOMERATIVE CLUSTERING ------------#
# I am looking for the most balanced approach
# Complete linkages is the approach that best fits this demand - I will leave only this one here, don't want to get it cluttered
# complete
met = "ward.D2"
aggl.clust.c <- hclust(dist_matrix, method = met)
plot(aggl.clust.c)
     # main = "Agglomerative, complete linkages")
# aggl.clust.c$order
# cutree(aggl.clust.c, k = 5) -> clust.num

#------------ DEFINITION OF THE NUMBER OF CLUSTERS ------------#

dm <- as.matrix(dist_matrix) # dissimilarity matrix to be used in the following steps
fviz_nbclust(dm, hcut, method = "wss") -> fcl_elb
fviz_nbclust(dm, hcut, method = "silhouette")-> fcl_sil
# Gap statistic
# nboot = 50 to keep the function speedy. 
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
set.seed(123)
fviz_nbclust(as.matrix(dist_matrix), hcut, nstart = 10,  method = "gap_stat", nboot = 1000) -> fcl_gap

ggplot(fcl_elb$data, aes(x=clusters, y=y, group=1)) +
  geom_line(lwd = 0.75, col="#E7B800") +
  geom_point(shape = 21, colour = "#E7B800", fill = "#E7B800", size = 3, stroke = 1) +
  theme_ipsum() +
  ylab("Total Within Sum of Squares") +
  xlab("Number of clusters k") -> plot_elb
ggplot(fcl_sil$data, aes(x=clusters, y=y, group=1)) +
  geom_line(lwd = 0.75, col="#E7B800") +
  geom_point(shape = 21, colour = "#E7B800", fill = "#E7B800", size = 3, stroke = 1) +
  theme_ipsum() +
  ylab("Average silhouette width") +
  xlab("Number of clusters k") -> plot_sil
ggplot(fcl_gap$data, aes(x=clusters, y=gap, group=1)) +
  geom_line(lwd = 0.75, col="#E7B800") +
  geom_point(shape = 21, colour = "#E7B800", fill = "#E7B800", size = 3, stroke = 1) +
  theme_ipsum() +
  ylab("Gap statistic (k)") +
  xlab("Number of clusters k") -> plot_gap
library(patchwork)
plot_elb / plot_sil / plot_gap + plot_annotation(tag_levels = c('a'), tag_prefix = '(', tag_suffix = ')')
# ggsave("figures/FigS2.tiff", width = 14, units="cm")
# look for a elbow in the first plot, and for a peak in the other two
# The silhouette score indicates how well-separated the clusters are.

# Data are not very structured (no peak in the gap statistic and no clear elbow)
# Nevertheless, based on the plots we can hypothesize the presence of at least 4-5 clusters
# The silhouette method suggests only 2 but that is not very informative


#------------ final AGGLOMERATIVE CLUSTERING ------------#

no_clusters <- 4
# threshold_colors <- viridis(5, option = "H", begin = 0, end = 0.8)
aggl.clust.c <- hcut(dist_matrix, k=no_clusters,
                     hc_func = "hclust",
                     hc_method = met)
factoextra::fviz_silhouette(aggl.clust.c)

# cut the tree and join cluster info with data
clust.num <- cutree(aggl.clust.c, k = no_clusters)
data_clustering.cl_all <- cbind(data_clustering, clust.num)

# Visualize the dendrogram
aggl.clust.c$labels <- data_clustering.cl_all$technology
# tiff("figs/clustering_nmds/hclust_manhattan.tiff", res=1000, width = 22, height = 27, units = "cm")
fviz_dend(aggl.clust.c,
          rect = TRUE, rect_fill = TRUE,
          lower_rect = 0,
          # palette = "uchicago",
          palette = viridis(no_clusters, option = "H", begin = 1, end = 0.66),
          labels_track_height = 10,
          horiz = TRUE,
          cex = 0.8,
          # dLeaf = 15,
          main="Hierarchical clustering - Manhattan",
          axes = F)
# dev.off()
# Visualize clusters as scatter plots
fviz_cluster(aggl.clust.c, data=dist_matrix, geom = c("point","text"))


##---- Euclidean clustering ----

dist_matrix <- eucl.dist

#------------ DIVISIVE CLUSTERING ------------#
divisive.clust <- diana(as.matrix(dist_matrix), 
                        diss = TRUE, keep.diss = TRUE)
plot(divisive.clust, main = "Divisive")

#------------ AGGLOMERATIVE CLUSTERING ------------#
# I am looking for the most balanced approach
# Complete linkages is the approach that best fits this demand - I will leave only this one here, don't want to get it cluttered
# complete
aggl.clust.c <- hclust(dist_matrix, method = met)
plot(aggl.clust.c)#,
     # main = "Agglomerative, complete linkages")
# aggl.clust.c$order
# cutree(aggl.clust.c, k = 5) -> clust.num

#------------ DEFINITION OF THE NUMBER OF CLUSTERS ------------#

dm <- as.matrix(dist_matrix) # dissimilarity matrix to be used in the following steps
fviz_nbclust(dm, hcut, method = "wss") -> fcl_elb
fviz_nbclust(dm, hcut, method = "silhouette")-> fcl_sil
# Gap statistic
# nboot = 50 to keep the function speedy. 
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
set.seed(123)
fviz_nbclust(as.matrix(dist_matrix), hcut, nstart = 25,  method = "gap_stat", nboot = 1000) -> fcl_gap

ggplot(fcl_elb$data, aes(x=clusters, y=y, group=1)) +
  geom_line(lwd = 0.75, col="#E7B800") +
  geom_point(shape = 21, colour = "#E7B800", fill = "#E7B800", size = 3, stroke = 1) +
  theme_ipsum() +
  ylab("Total Within Sum of Squares") +
  xlab("Number of clusters k") -> plot_elb
ggplot(fcl_sil$data, aes(x=clusters, y=y, group=1)) +
  geom_line(lwd = 0.75, col="#E7B800") +
  geom_point(shape = 21, colour = "#E7B800", fill = "#E7B800", size = 3, stroke = 1) +
  theme_ipsum() +
  ylab("Average silhouette width") +
  xlab("Number of clusters k") -> plot_sil
ggplot(fcl_gap$data, aes(x=clusters, y=gap, group=1)) +
  geom_line(lwd = 0.75, col="#E7B800") +
  geom_point(shape = 21, colour = "#E7B800", fill = "#E7B800", size = 3, stroke = 1) +
  theme_ipsum() +
  ylab("Gap statistic (k)") +
  xlab("Number of clusters k") -> plot_gap
library(patchwork)
plot_elb / plot_sil / plot_gap + plot_annotation(tag_levels = c('a'), tag_prefix = '(', tag_suffix = ')')
# ggsave("figures/FigS2.tiff", width = 14, units="cm")
# look for a elbow in the first plot, and for a peak in the other two
# The silhouette score indicates how well-separated the clusters are.


#------------ final AGGLOMERATIVE CLUSTERING ------------#

no_clusters <- 4
aggl.clust.c <- hcut(dist_matrix, k=no_clusters,
                     hc_func = "hclust",
                     hc_method = met)
factoextra::fviz_silhouette(aggl.clust.c)

# cut the tree and join cluster info with data
clust.num <- cutree(aggl.clust.c, k = no_clusters)
data_clustering.cl_all <- cbind(data_clustering, clust.num)

# Visualize the dendrogram
aggl.clust.c$labels <- data_clustering.cl_all$technology
# tiff("figs/clustering_nmds/hclust_euclidean.tiff", res=1000, width = 22, height = 27, units = "cm")
fviz_dend(aggl.clust.c,
          rect = TRUE, rect_fill = TRUE,
          lower_rect = 0,
          # palette = "uchicago",
          palette = viridis(no_clusters, option = "H", begin = 1, end = 0.66),
          labels_track_height = 10,
          horiz = TRUE,
          cex = 0.8,
          # dLeaf = 15,
          main="Hierarchical clustering - Euclidean",
          axes = F)
# dev.off()
# Visualize clusters as scatter plots
fviz_cluster(aggl.clust.c, data=dist_matrix)


## ---- kmeans clustering (optional) ----

# Step 1: Perform k-means clustering on the selected columns
set.seed(123)  # For reproducibility
kmeans_result <- kmeans(data_clustering[, 3:11], centers = no_clusters)

# Step 2: Calculate principal components (PCA) for visualization
pca_result <- prcomp(data_clustering[, 3:11], scale. = TRUE)  # Standardize data for PCA

# Step 3: Prepare data for plotting
plot_data <- as.data.frame(pca_result$x[, 1:2])  # Extract first two principal components
plot_data$Cluster <- as.factor(kmeans_result$cluster)  # Add cluster assignments
plot_data$Technology <- data_clustering$technology  # Add technology names

# Step 4: Plot the k-means clusters using the first two principal components
ggplot(plot_data, aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_text_repel(aes(label = Technology), size = 3, max.overlaps = 10) +
  theme_minimal() +
  labs(title = "K-means Clustering of Technologies", x = "PC1", y = "PC2", color = "Cluster") +
  theme(legend.position = "right")

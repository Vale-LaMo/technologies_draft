# technologies
Analysis for horizon scan of novel technologies for IAS citizen science

## Scripts
This folder contains the main scripts for the analyses (note that the preliminary steps to anonymise data are .gitignored)

### Data cleaning

00_datacleaning.R : reads the anonymised data and performs some data cleaning and tidying - the results are stored in R objects used by subsequent scripts, and saved in the output folder:

- dat.RData : original assessments, anonimysed, in wide format
- scores_num.RData : original assessments, anonimysed, in wide format but converted to numeric
- scores_num_long.RData : original assessments, anonimysed, converted to numeric and in long format
- scores_tidy_long.RData : original assessments, anonimysed, in long format 

Assessments were collected before the workshop discussion (individual assessments - each coder rated a certain number of technologies without consultation with others), and after a panel discussion about each technology (consensus building phase). The data are thus divided into 2 rounds (before and after the workshop).

### Data exploration
01_dataexploration.R : performs exploratory data analyses on the technology assessments dataset, focusing on the number of coders and responses indicating uncertainty (e.g., "I don't know" or "N/A"). 

1. **Assessors per Technology**: 
   - The code groups the dataset by technology and counts the number of coders for each technology.
   - It generates a bar plot to visualize the number of coders, using a color gradient to represent the count.

2. **Counting Uncertainty Responses**: 
   - It counts occurrences of "I don't know" and "N/A" responses for each technology and criterion.
   - The total counts are summarized and visualized using bar plots, including a stacked bar plot to show detailed counts by rank.

3. **Basic Statistics Calculation**: 
   - The code calculates basic statistics (minimum, maximum, mean, median, quartiles, and mode) for each criterion and by technology.
   - It nests the data by round to perform similar analyses for each assessment round.

4. **Visualizations**: 
   - The code generates various plots, including violin plots to show the distribution of ranks by criterion and balloon plots for frequency of ranks.
   - It allows for saving the plots to files for further use.
   - The last plots provide a comparison between rounds for each technology


### Agreement metrics

02_agreement_metrics.R: calculates various metrics of agreement between experts for different technologies. Here's a breakdown of what the code does:

1. **Load required packages**: The code loads the irr package for inter-rater reliability calculations and the lme4 package for linear mixed-effects models.

2. **Set the Round variable**: The code allows you to calculate metrics for different rounds (Before, After) or for all assessments (All).

3. **Calculate agreement metrics per technology**: The code iterates through each unique technology in the scores_num data frame. For each technology, it creates a scores matrix by selecting relevant columns from the scores_num data frame based on the Round variable. 
It calculates the following metrics:
  - Fleiss' kappa: A measure of inter-rater agreement for multiple raters. The code checks for NAs and calculates the kappa value, z-score, and p-value.
  - Intraclass correlation coefficient (ICC): A measure of reliability. The code checks for NAs and calculates the ICC value, lower bound, and upper bound.
  - Krippendorff's alpha: A measure of inter-rater reliability. The code checks for NAs and calculates the alpha value and the number of raters.
  - The code also performs an ANOVA test to check for significant differences between coders for each criterion. It fits a linear mixed-effects model and performs the ANOVA test. If the F-value is significant (p < 0.05), it stores "< 0.05" in the p_anova vector.

4. **Create the irr_table**: The code creates a data frame irr_table_temp with the calculated metrics for each technology. It then joins this table with the summary_coders_tech data frame to add the number of coders per technology.

5. **Save the irr_table**: Finally, the code saves the irr_table as a CSV file in the output directory. The file name depends on the value of the Round variable.

The code provides a comprehensive analysis of agreement between experts for different technologies, allowing to assess the reliability of the assessments and identify any significant differences between coders.

### Regression analyses on agreement metrics

03_agreement_metrics_regressionmodels.R: The script primarily focuses on analyzing and comparing agreement metrics across the two assessment rounds. It is divided into main sections:

1. **Setup and data preparation:** essential packages are loaded, and the script reads in two CSV files (irr_table_Before_20241025.csv and irr_table_After_20241025.csv), which represent agreement metrics for "Before" and "After" rounds.
The tables are combined into a single data frame, irr_table_combined, with an added factor Round to distinguish between the two rounds.

2. **Data Exploration:** Histograms are generated for key agreement metrics (e.g., Krippendorff’s Alpha, Fleiss’ Kappa, ICC) to inspect their distributions.

3. **Model Fitting and Transformation:** This section fits robust linear mixed models, comparing IRR values across rounds, and visualizing these comparisons. Models are fitted both for each metric separately, and also with a single mixed model.

### Graphical representation of the agreement metrics results

04_agreement_metrics_plot.R: This script focuses on visualizing agreement metrics across different rounds of assessment.

1. **Setup and Library Loading**: The script begins by loading essential packages for plotting and color schemes, such as `ggplot2` for data visualization and `viridis` for color scaling, along with `hrbrthemes` for theme customization.

2. **Data Preparation**: It reads in a data file with combined agreement metrics, allowing for the separation of values by rounds of assessment.
   
3. **Plot Creation for Agreement Metrics**: Boxplots and scatter (jitter) plots are created for each metric (ICC, Krippendorff’s Alpha, and Fleiss’ Kappa), showing the distribution across rounds.
   - Each plot includes:
      - **Boxplots without outliers** for visual clarity.
      - **Jittered points** to reveal individual data values within each round.
      - **Threshold Reference Lines** indicating standard cutoffs for interpretation (e.g., "Poor," "Fair," "Good," "Excellent" for ICC) to contextualize metric reliability.
      - **Annotations** on the plot's right side label thresholds for easy interpretation.

4. **Color and Style Customization**: The script uses the `viridis` color palette, which is colorblind-friendly, to color the plots by assessment rounds. The plot titles and labels are likely customized using themes from `hrbrthemes` to ensure consistency and readability.


### Ranking of technologies and confidence in the assessments

05_ranking.R: This R script performs data processing, visualization, and analysis for ranking technologies based on specific criteria. 

1. **Loading Data and Libraries**:
   - Essential libraries (`tidyverse`, `ggpubr`, `hrbrthemes`, `viridis`, and `writexl`) are loaded to manage data and create plots.
   - Data files (`scores_num_long` and `scores_tidy_long`) are loaded to access rankings and criteria scores.

2. **Calculating Scores**:
   - Scores are aggregated for each technology and criterion, with results stored in `scores_tech` and `scores_tech_criteria`.
   - A weighted score per criterion is computed by dividing scores by the number of coders, resulting in a normalized dataset (`weighted_scores_criteria`).

3. **Plotting Rankings**:
   - A stacked bar chart visualizes each technology’s score across criteria, colored using the `viridis` palette for clear differentiation.

4. **Confidence Analysis**:
   - Confidence levels of coders are analyzed using responses from `coders_confidence_level.csv`.
   - A violin plot shows the distribution of coder confidence levels by technology.

5. **Combining Plots**:
   - Using the `patchwork` library, the ranking and confidence plots are combined side by side for comparison.

6. **Correlation Analysis**:
   - A Spearman correlation is calculated between the median confidence level and technology score, with a regression model (linear model) fitted to illustrate the relationship.

7. **Heatmap for 'I Don't Know'/NA Responses**:
   - Technologies with "I Don't Know"/NA responses are analyzed, generating heatmaps to show these responses across criteria. This helps in identifying areas where coders were uncertain.

8. **Saving and Output**:
   - Each plot can be saved as `.tiff` files if uncommented. The script produces an insightful visualization suite for understanding technology rankings, coder confidence, and areas of uncertainty. 


#### Clustering on weighted scores

06_clustering.R: This script provides a comprehensive approach to clustering based on technology ratings, using both hierarchical and k-means clustering methods, and testing Euclidean and Manhattan distance metrics. Here’s an overview:

### 1. **Load Packages and Data**
   - A series of libraries is loaded to support clustering (`cluster`, `factoextra`, `NbClust`), visualization (`ggplot2`, `viridis`, `patchwork`), and data manipulation (`tidyverse`, `reshape2`).
   - The data file, `weighted_scores_criteria.csv`, is loaded, and columns excluding `no.coders` are selected for clustering (`data_clustering`).

### 2. **Distance Matrix Calculation**
   - **Euclidean and Manhattan Distances**: Two distance matrices are created, one for Euclidean and another for Manhattan, allowing comparisons to determine which metric best fits the data structure.
   - **PCA & Boxplots**: A PCA plot and boxplot are generated to evaluate cluster structure and feature variability, which can guide the choice between distance metrics based on cluster shapes and feature correlations.

### 3. **Clustering Approaches**
   - **Divisive Clustering**: Using the `diana()` function, a divisive hierarchical clustering approach is implemented and visualized.
   - **Agglomerative Clustering**: Agglomerative clustering with complete linkage (`ward.D2` method) is applied to the distance matrix, with a plot for visual inspection.

### 4. **Optimal Cluster Number Determination**
   - **Cluster Validity Indices**: Three indices (elbow, silhouette, and gap statistic) are calculated using the `NbClust` and `fviz_nbclust` functions to identify the optimal cluster number (`no_clusters`), with plots generated for each index.
   - **Elbow Plot, Silhouette Plot, Gap Statistic Plot**: These help visualize clustering quality and support the hypothesis of 4-5 clusters, despite data structure challenges.

### 5. **Final Clustering and Visualization**
   - The final hierarchical clustering (`aggl.clust.c`) is performed with `hcut`, and clusters are visualized via dendrograms and scatter plots.
   - **k-means Clustering** (optional): A k-means clustering algorithm is applied as an alternative, followed by PCA to visualize clusters along the two principal components with labeled data points.

This code provides a flexible, comparative framework for clustering the technology data. By evaluating both Manhattan and Euclidean distances and various cluster validity measures, it helps identify an optimal approach to cluster structure that can be explored visually and quantitatively.


#### Clustering and NMDS combined

06_nmds_clustering.R: This code performs a Non-Metric Multidimensional Scaling (NMDS) analysis combined with clustering to explore relationships between various technologies and criteria, aiming to identify clusters and visualize relationships with specific criteria as vectors.

1. **Data Setup and NMDS**:
   - **Packages Loaded**: `tidyverse` (for data manipulation), `ggpubr` (for enhanced ggplot functions), and `vegan` (for NMDS and ordination tools).
   - **Data**: The dataset, `weighted_scores_criteria`, is read from a CSV file, and one column (`no.coders`) is excluded to create `data_clustering`.
   - **NMDS Calculation**: NMDS is run on the `data_clustering` data, with 2 dimensions (`k=2`) and a maximum of 100 tries to optimize the solution (`trymax=100`).

2. **Environmental Fitting**:
   - **envfit Function**: Environmental variables (criteria) are fitted to the NMDS solution, examining which criteria (columns in `data_clustering`) drive the distribution of the technologies.
   - **Subset for Significant Criteria**: The function identifies criteria significant at a 0.1 level, and this subset is saved for use in plotting arrows (or vectors).

3. **Initial Plotting of NMDS**:
   - **Base Plot**: Creates an NMDS scatterplot of technology points using `ggplot2`.
   - **Vector Overlay**: Adds significant criteria as arrows, with `ggrepel` used for labeling to avoid overlapping labels.

4. **Hierarchical Clustering**:
   - **Agglomerative Clustering**: Performs clustering on NMDS coordinates using Ward’s method and Manhattan/Euclidean distance. The `cutree` function divides the data into `n_clusters` clusters, with each technology assigned to a cluster.
   - **Ellipse Plot**: Uses ellipses to encircle clusters on the NMDS plot and custom colors for cluster representation.

5. **Tree Plot (Dendrogram)**:
   - A dendrogram is generated using `fviz_dend` from the `factoextra` package, visualizing the hierarchical clustering with colored rectangles around clusters.

6. **K-means Clustering**:
   - **K-means Analysis**: Another clustering approach on the NMDS coordinates, using K-means clustering for the same `n_clusters` value.
   - **Ellipse Plot with K-means**: Similar to hierarchical clustering, but using K-means-derived clusters, it plots ellipses around each K-means cluster and overlays criterion arrows.

This analysis identifies how technologies group based on NMDS space and visually represents relationships between criteria and technology clusters, providing insights into patterns driven by the criteria.

#### Guidance to interpret the results of NMDS and clustering

In the NMDS plot, the goal is to visualize how the technologies cluster in terms of the scores they received across different assessment criteria. Here’s a breakdown of the interpretation:

**Positioning in NMDS Space:** Each point represents a technology, positioned in a way that best reflects the distances between it and other technologies based on their scores. Technologies closer together are more similar in their ratings across criteria, while those further apart are less similar.

**Clusters and Ellipses:** The ellipses group technologies based on similarity, indicating clusters where the technologies are more closely related in terms of scoring patterns. This clustering reveals general patterns or subgroups of technologies based on shared attributes in the ratings.

**Arrows (Assessment Criteria):** Each arrow represents a criterion or assessment aspect in the NMDS space. The direction of an arrow shows which direction higher scores for that criterion pull the technologies in this reduced space. The arrow length suggests the relative importance or impact of the criterion in differentiating technologies—the longer the arrow, the more it contributes to the observed separation between technologies. For instance, if a group of technologies clusters along the axis of an arrow, they likely scored similarly for that criterion.

In general, the arrows help clarify which criteria are most influential in shaping the arrangement of the technologies in this NMDS space, guiding you toward understanding why particular clusters form as they do.

Indeed, points that are closer to an arrow (and, more specifically, in the direction the arrow points) tend to be more strongly associated with that criterion. This indicates that these technologies have higher values for that particular criterion compared to others. The closer a point is to an arrow, the more it aligns with the characteristic the arrow represents.
Here’s a summary of how to interpret this spatial relationship:

- Points in the Direction of the Arrow: Technologies that fall along the arrow's path are more influenced by or have higher values for that criterion.
- Distance to the Arrow: The closer a point is to the arrow, the stronger its association with that criterion.
- Opposite Direction: If a technology is on the opposite side or far from an arrow, it may score lower on that criterion, relative to others.

This makes it easier to spot which criteria are driving the formation of clusters and how individual technologies relate to each criterion.

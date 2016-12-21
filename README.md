# Master-thesis: DATA ANALYSIS: CLUSTERING OF ELECTRICITY CONSUMPTION PROFILES
The development of the project in R, consists of a data analysis on the hourly electricity consumption curves of residential consumers, with the aim to segment the consumers based on clustering techniques of unsupervised learning.

Chapter 2 of this work has segmented the electricity consumers of the project of “Rubí Brilla” according to the similarities of their electrical load profiles, using the proportion of energy usage per hour (%) as a common framework. The objective behind this segmentation is to be able to provide personalised recommendations to each group in order to reduce their energy consumption and the associated costs, fostering energy efficiency measures and improving the consumer engagement.
The desired segmentation is obtained by an iterative process, based on computational clusters calculation (using “R” software) and finalized by a post-clustering analysis applying visualization and statistical techniques to detect the outliers and reallocate them to a more appropriate group. Three different clustering techniques (Hierarchical clustering, K-means clustering and Self-Organizing Maps), were tested and compared, giving similar outputs. The solution from the Hierarchical clustering is the one that better adapts to the segmentation sought, which is used as the base of the post-clustering stage to obtain the final segmentation.
The final result grouped the 121 users in 7 clusters, each group representing a distinct load profile. The first six clusters are devoted to residential consumers and the cluster 7 is devoted to business. Described and distributed as follows:
Cluster 1 Morning Peaks (7 consumers)
Cluster 2 Late night Peaks (17 consumers)
Cluster 3 Flat Consumers (28 consumers)
Cluster 4 Evening Peaks (7 consumers)
Cluster 5 Daytime Consumers (16 consumers)
Cluster 6 Double Peaks (18 consumers)
Cluster 7 Business (7 consumers)
The segmentation of the electricity consumers provides knowledge and a better understanding of the consumer. In this particular case, it allows to personalise energy savings recommendations according to the consumers’ specific characteristics; improving the consumer experience by being able to provide the adequate advices at the appropriate time, facts that increase the effectiveness of the energy efficiency advices’ service.

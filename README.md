# Clustering Analysis of the Social Influence on Businesses through Social Media

## Datasets
Training Dataset : (https://data.world/ahalps/social-influence-on-shopping) <br />
Testing Dataset : (fyp-questionnaire.csv) 

## Problem Statement
Companies who are looking to adapt and capitalize on these great opportunities by investing into establishing their social presence must first understand their business objectives and utilize the proper machine learning techniques to grasp the most effective results and improve their organization performance (Tajudeen, 2014). Reasonably, a company cannot simply choose to begin building their social media presence on a platform merely for its popularity but instead, the choice of social media platform is then reliant on the business objective and their target customers. 

As a result, this report investigates the social influence of major social media platforms (Facebook, Instagram, Snapchat and Twitter) on businesses in Malaysia, where three clustering machine learning techniques are conducted to discern the correlation between demographics and the popularity of a social media platform through interesting groups and patterns discovered. 

## Research Methodology
The study was conducted on a great foundation of research and literature review, where each selected machine learning technique and model were selected with understanding of how it seemed fitting to work with the datasets. During the training stage, a dataset retrieved from a survey conducted in the US was used, where it was used to train the clustering  models as well as for trial and error with other algorithms (for example, algorithms used during the data pre-processing stage, EDA etc). 

As part of creating realistic and more applicable insights, the study  conducted a data gathering phase (within Malaysia) where responses were exported and used as the test dataset. Hence, key insights found are applicable to local culture and environment. Once exported, dataset is categorized to four main demographics according to the responses; Gender, Age, Location and Employment Status.   

## Machine Learning Algorithms Applied
## Exploratory Data Analysis
-------------------------
![hampel-filter](/results/hampel-filter.png)
1. **Hampel Filter** - Detect possible outliers and deals with extreme outliers. 
<br />
<br />
![ros-test](/results/ros-test.png)
![ros-test-2](/results/ros-test-2.png)
2. **Rosners' Test** - Confirmation of outliers. 
<br />
<br />
![winsor-imp](/results/winsor-imp.png)
![winsor-imp-2](/results/winsor-imp-2.png)
3. **Winsorization** - Alternative to eliminating outliers, that is to impute  mean values to replace them. 
<br />
<br />
![log-trans](/results/log-trans.png)
4. **Log Transformation** - Selected option as it was more fitting for dataset as the dataset is left-skewed.
<br />
Findings:
The residual standard error improved to from 0.1569 to 0.1259 which is an improvement to the  previous attempt (NOT SHOWN). That being noted, the residual stand error decreased which means that the regression model fits better hence selected.
<br />
<br />
5. **Min-Max Scaling** - Scales values 
<br />
Findings:
Did not affect skewness hence not fitting and not utilized.
<br />
<br />
![chi-square](/results/chi-square.png)
6. **Pearson's Chi-Squared Test** - Association measurement between categorical variables. 
<br />
Findings:
Amongst the five categories, ‘University’ and ‘Web’ have p-values that are less than 0.05, indicating a rejected null hypothesis whereas the other 3 variables have a p-value greater than 0.05, hence an accepted null hypothesis.

###### Data Modelling
-------------------------
Identify Optimal Clusters:
![sil-model-opt-clusters](/results/sil-model-opt-clusters.png)
7. **Silhouette Method**
<br />
The optimal number of clusters are determined with the silhouette method algorithm onto the test dataset. Here, it determines the optimal number of clusters to be 4 cluster points. 
<br />
<br />
Distance Measures: 
8. **Gower Distance** - Used for PAM Clustering and K-Prototypes
9. **Levenshtein Distance** - Used for Levenshtein Model.

Clustering Models:
10. **PAM Clustering** - Works well with Gower distance. 
11. **K-Prototype** - Improved adaptation of K-Means as it is more fitting for categorical/mixed datasets.
12. **Levenshtein Model** - Clusters based on string distance matrix. 

Evaluation: 
13. **Silhouette Width** (Refer to below)


## Evaluation
To validate the accuracy of the clusters, the Silhouette Width is utilized. The algorithm operates by measuring the similarity of objects within each cluster between the range of -1 and 1 where the value closer to 1 resembles a well-matched object within the cluster. With that, the following depicts the Silhouette Width being enacted onto the three clustering model results prior to comparison.

![sil-wid-pam](/results/sil-wid-pam.png)<br />
Silhouette Width of PAM Clustering

![sil-wid-pam](/results/sil-wid-kp.png)<br />
Silhouette Width of K-Prototype

![sil-wid-pam](/results/sil-wid-lv.png)<br />
Silhouette Width of Levenshtein Model

![final-evaluation-sil-wid](/results/final-evaluation-sil-wid.png)

The model with the best silhouette width average is the Partition Around Medoids (PAM) with the average of 0.44. With that, the evaluation is completed where the PAM model will be deployed onto the test dataset.

## Results & Findings
![res-1](/results/test-dataset-res.png)

Cluster 3 is identified to be the most dominant cluster amongst three demographics, whilst cluster 1 and 2 is seen to be the co-leading clusters for the ‘Working Status’ data frame. For the ‘Gender’, ‘Location’ and ‘Age’ sets, cluster 3 primarily consists of one primary social media platform, that is ‘Twitter’, followed by ‘Snapchat’. This contrasts from the ‘Working Status’ demographic where the most influential social media platform is ‘Instagram’ followed by ‘Facebook’. Furthermore, within the ‘Working Status’ demographic, an interesting underlying pattern discovered is how there is an entire cluster comprising only of respondents  who answered with ‘Twitter’. Aside from that, more hidden patterns are noticed within all the sets. Firstly, ‘Instagram’ is seen to have a solid proportion within each cluster which resembles its strength of influence regardless of demographic difference. Next, an underlying trend is identified, where respondents who answered ‘Twitter’ are often grouped together in a bunch in contrast to the other options. This could resemble the distinction of preference from users who are influenced by ‘Twitter’.

![res-2](/results/test-dataset-res-2.png)

The stacked barplot above visualizes cluster 3 of each set. With that, it is observed how through the first three sets, there is a clear preference of ‘Twitter’ and ‘Snapchat’. Firstly, within the ‘Gender’ category, it is noted how the clustering presents ‘Snapchat’ only selected by females rather than males. The same pattern can be seen in the following two clusters, where there are sole categories with only ‘Twitter’ as their most influential social media platform, such as respondents from ‘Johor Bahru’ and ‘Negeri Sembilan’ as well as respondents aging from ’25-39’. In terms of individual cluster analysis, it is interesting to note that there are two categories that have ‘Snapchat’ equal or greater than the latter social media platform. This is evident in respondents from ‘Penang’ (from the ‘Location’ set) and ’15-19’ (from the ‘Age’ set). Particularly for the age demographic, it is significant to highlight how the preference of ‘Snapchat’ for the age ’15-19’ is stronger than for ‘Instagram’. Lastly, for the cluster 3 of the ‘Working Status’. There is no distinct pattern aside from how ‘Facebook’ is seen to be slightly favorited. More deep diving analysis for the ‘Working Status’ set will be conducted. 

![res-3](/results/test-dataset-res-3.png)

Like the other three dominant clusters in other sets, ‘Instagram’ is seen to have a solid ratio amongst the clusters. Additionally, there is also a lack of influence from ‘Facebook’ as there are no strong significance proving so. As for ‘Snapchat’, it is again noted of how it is popular amongst the younger age group (as previously justified in the ‘Age’ set), that is in the ‘High School Graduate’ category in cluster 2. 



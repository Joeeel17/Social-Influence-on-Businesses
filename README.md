# Clustering Analysis of the Social Influence on Businesses through Social Media
This study focuses on understanding the social influence on businesses through social media, where it aims to discover the most apt social media platform based on the organization’s business objective through clear-cut analysis. Moreover, the study’s secondary objective is to discern the correlation between demographics and the popularity of a social media platform through interesting groups and patterns discovered. These will be done by applying congruous methodologies when dealing with the mixed-data dataset whilst deploying clustering analysis to identify hidden patterns and clusters within the dataset.

The study was conducted on a great foundation of research and literature review, where each selected ML technique and model were selected with understanding of how it seemed fitting to work with the datasets. Moreover, as part of creating realistic and more applicable insights, the study also conducted a data gathering phase (within local grounds) where responses were exported and used as the test dataset. Hence, key insights found are applicable to local culture and environment.    

# Datasets
Training Dataset : (https://data.world/ahalps/social-influence-on-shopping) <br />
Testing Dataset : (fyp-questionnaire.csv) 

# Machine Learning Algorithms Applied
Data Preparation
-------------------------
1. Hampel Filter - Detect possible outliers.
2. Rosners' Test - Confirmation of outliers. 
3. Winsorization - Alternative to eliminating outliers, that is to replace with mean values.
4. Log Transformation - Selected option as it was more fitting for dataset as the dataset is left-skewed.
5. Min-Max Scaling - Scaled values but did not affect skewness hence not fitting.
6. Pearson's Chi-Squared Test - Association measurement between categorical variables. 

Data Modelling
-------------------------
Identify Optimal Clusters:
1. Silhouette Method

Distance Measures: 
1. Gower Distance - Used for PAM Clustering and K-Prototypes
2. Levenshtein Distance - Used for Levenshtein Model.

Clustering Models:
1. PAM Clustering - Works well with Gower distance. 
2. K-Prototype - K-Prototype is improved adaptation of K-Means as it is more fitting for categorical/mixed datasets.
3. Levenshtein Model - Clusters based on string distance matrix. 

Evaluation: 
1. Silhouette Width 

Results
-------------------------
Silhouette Width identified PAM Clustering to have the best clustering performance in contrast to other techniques. 
![sil-wid-pam](/results/sil-wid-pam.png)
![sil-wid-pam](/results/sil-wid-kp.png)
![sil-wid-pam](/results/sil-wid-lv.png)

Test dataset is divided to four main demographics: Gender, Age, Location and Employment Status. Clustering analysis is as follows: 
![res-1](/results/test-dataset-res.png)

The following depicts the structure of each of the dominant clusters per demographic.  
![res-2](/results/test-dataset-res-2.png)

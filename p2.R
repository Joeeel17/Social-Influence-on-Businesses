df <- read.csv("D:/Portfolio/FYP/social-media-influence.csv")
fyp.df <- read.csv("D:/Portfolio/FYP/fyp-questionnaire.csv")
head(df)
tail(df)

install.packages("Hmisc")
install.packages("dplyr")
install.packages("ggraph")
install.packages("tidygraph")
install.packages("cluster")
install.packages("magrittr")
install.packages("mice")
install.packages("DataExplorer")
install.packages("EnvStats")      # Rosners' Test
install.packages("VIM")           # V for Missing Data
install.packages("caret")         # Scaling
install.packages("e1071")         # Skewness
install.packages("daisy")         # Gower Distance
install.packages("ggstatsplot")   # Mosaic Plot
install.packages("Rtsne")         # PAM Clustering Vis
install.packages("stringdist")
install.packages("poLCA")         # LCA


library(hrbrthemes)
library(cluster)
library(dplyr)
library(plyr)
library(ggplot2)
library(gridExtra)
library(Hmisc)
library(tidygraph)
library(mice)
library(DataExplorer)
library(EnvStats)
library(VIM)
library(tidyverse)
library(caret)
library(e1071)
library(daisy)
library(ggplot2)
library(corrplot)                 # Corr Plot EDA
library(Rtsne)
library(stringdist)
library(factoextra)
library(RColorBrewer)
library(poLCA) 
library(ggstatsplot)
library(depmixS4)                 # LCA

head(df)
unique(df[c("Segment.Description")])       
sum(is.na(df))
summary(is.na(df))


# No missing values
marginplot(df[c(5,6)])

# Outliers
summary(df$Count)
summary(df$Percentage)

bx_count_df <- ggplot(df) +
  aes(x = "", y = Count) +
  geom_boxplot(fill = "#89CFF0") +
  theme_minimal()
bx_perc_df <- ggplot(df) +
  aes(x = "", y = Percentage) +
  geom_boxplot(fill = "#89CFF0") +
  theme_minimal()

grid.arrange(bx_count_df,bx_perc_df, nrow = 1)

# Remove Question column
glimpse(df)
df2 = subset(df, select= -c(Question))
glimpse(df2)

# removing irrelevant columns
df2 <- subset(df2, Count!=0)

str(df2)
summary(df2$Count)
summary(df2$Percentage)

bx_count <- ggplot(df2) +
  aes(x = "", y = Count) +
  geom_boxplot(fill = "#89CFF0") +
  theme_minimal()

bx_perc <- ggplot(df2) +
  aes(x = "", y = Percentage) +
  geom_boxplot(fill = "#89CFF0") +
  theme_minimal()
grid.arrange(bx_count,bx_perc, nrow = 1)

bx_count_2 <- ggplot(fyp.df3) +
  aes(x = "", y = count) +
  geom_boxplot(fill = "#89CFF0") +
  theme_minimal()
bx_count_2

# Hampel Filter 
lower_bound <- median(df2$Count) - 3 * mad(df2$Count, constant = 1)
lower_bound
upper_bound <- median(df2$Count) + 3 * mad(df2$Count, constant = 1)
upper_bound
outlier_ind <- which(df2$Count < lower_bound | df2$Count > upper_bound)
str(outlier_ind)                # 233 observed outliers

# Rosner's Test
# Tested first with 233 but only 113 were found TRUE
ros <- rosnerTest(df2$Count, 
                  k=233
)
ros$all.stats

ros <- rosnerTest(df2$Count, 
                  k=113
                  )
ros$all.stats
length(unique(df4[["segment.description"]]))

qplot(data = df2, x = Count) + ylab("Answer") + 
  xlim(c(450, 1000))      
plot_histogram(df2)       # Percentage=1 can be outlier

# Values > 128
sum(df2$Count >=128)
sum(df2$Percentage ==1)

outliers <- df2

# Changing values where Count >= 128 to NA
# Changing values where Perc >= 1 to NA
outliers$Count[outliers$Count >= 128] <- NA
outliers$Percentage[outliers$Percentage == 1] <- NA
outliers

# 113 outliers in Count
# 103 outliers in Percentage
md.pattern(outliers)

# Imputation 
tempData <- mice(outliers, m=5, maxit=50, meth="pmm", seed=500)
tempData$imp$Count
tempData$imp$Percentage
df3 <- complete(tempData, )
summary(df3)
xyplot(tempData, Count ~ Percentage, pch=18, cex=1)

# Values Imputed      - did not use
md.pattern(df3)

ggplot(df3, aes(x=Percentage, y=Count, color=Answer)) + 
  geom_point(size=2) +
  theme_ipsum()

plot_histogram(df3)
ggplot(fyp.df3, aes(x=count, fill=gender, color=gender)) +
  geom_histogram(position="identity")

# Data Integration
# CAREFUL TO DO
write.csv(df3,"D:/Portfolio/FYP/completed-data.csv", row.names = FALSE)
df4 <- read.csv("D:/Portfolio/FYP/completed-data.csv")
glimpse(df4)
is.null(df4)
summary(df4)

# Data Transformation
df4$Segment.Type <- as.factor(df4$Segment.Type)
df4$Segment.Description <- as.factor(df4$Segment.Description)
df4$Answer <- as.factor(df4$Answer)
df4$Count <- as.numeric(df4$Count)
df4$Percentage <- as.numeric(df4$Percentage)
str(df4)

# Identifying unique values 
unique(df4[c("Answer")])                      # 5
unique(df4[c("Segment.Type")])                # 5
length(unique(df4[["segment.description"]]))  # 288

# Encode categorical data
df4$Answer = factor(df4$Answer,
                               levels = c('None', 'Facebook', 'Instagram', 'Snapchat', 'Twitter'),
                               labels = c(0,1,2,3,4))
df4$Segment.Type = factor(df4$Segment.Type,
                                     levels = c('Mobile', 'Web', 'Gender', 'University', 'Custom'),
                                     labels = c(1,2,3,4,5))


# Renaming for easy access
df4 = df4 %>% 
  rename(
    segment.type = Segment.Type,
    segment.description = Segment.Description,
    answer = Answer,
    count = Count,
    percentage = Percentage
  )
str(df4)
unique(df4[c("segment.description")]) 

test<-df4

str(test)
View(test)

# Changing Row Names

test$segment.description <- sub(".*University.*", "University", test$segment.description)
test$segment.description <- sub(".*Illinois State.*", "University", test$segment.description)
test$segment.description <- sub(".*Tech.*", "University", test$segment.description)
test$segment.description <- sub(".*Westport, CT.*", "University", test$segment.description)
test$segment.description <- sub(".*Cal Poly.*", "University", test$segment.description)
test$segment.description <- sub(".*Institute.*", "University", test$segment.description)
test$segment.description <- sub(".*UMass.*", "University", test$segment.description)
test$segment.description <- sub(".*Westport, CT.*", "University", test$segment.description)

test$segment.description <- sub(".*College.*", "College", test$segment.description)


test$segment.description <- sub(".*High School.*", "High School", test$segment.description)
test$segment.description <- sub(".*HS.*", "High School", test$segment.description)
test$segment.description <- sub(".*Student.*", "High School", test$segment.description)
test$segment.description <- sub(".*Academy.*", "High School", test$segment.description)


test$segment.description <- sub(".*Asian.*", "Asian", test$segment.description)
test$segment.description <- sub(".*Jewish.*", "Jewish", test$segment.description)
test$segment.description <- sub(".*Black*", "Black", test$segment.description)
test$segment.description <- sub(".*Hispanic*", "Hispanic", test$segment.description)
test$segment.description <- sub(".*White*", "White", test$segment.description)


unique(test[c("segment.description")]) 

# Scaling
# Requires all variables to be numerical
# df4 = original data
df4$segment.type <- as.numeric(df4$segment.type)
df4$segment.description <- as.numeric(df4$segment.description)
df4$answer <- as.numeric(df4$answer)
str(df4)

# Skewness
df4.skew.count <- ggplot(data = df4, aes(count)) + geom_histogram()
df4.skew.perc <- ggplot(data = df4, aes(percentage)) + geom_histogram() 
grid.arrange(df4.skew.count,df4.skew.perc, nrow=1)

# Log Transformation
ggplot(data = df4, aes(x = percentage, y = count)) +
  geom_point() +
  scale_x_log10() + scale_y_log10()
df5.lm.model = lm(percentage ~ count, data = df4)
summary(df5.lm.model)

df5.lm.model2 = lm(log1p(percentage) ~ log1p(count), data = df4)
summary(df5.lm.model2)

# df5.lm.model2 more accurate
# Smaller Residual standard error (0.1569 > 0.1259) 
# R2 greater (0.0317 < 0.03787)

summary(df4)
df5 = log1p(df4)
summary(df5)
plot_histogram(df4)
plot_histogram(df5)

df5.skew.count <- ggplot(data = df5, aes(count)) + geom_histogram()
df5.skew.perc <- ggplot(data = df5, aes(percentage)) + geom_histogram()
grid.arrange(df4.skew.count,df4.skew.perc, df5.skew.count,df5.skew.perc, nrow=2)

# Skewness looks better;
  # 0 = perfectly symmetrical
  # > 1 && < -1 = highly skewed
  # between -1 and -0.5 or 1 and 0.5 = moderately skewed
  # between -0.5 and 0.5 = approximately symmetric
# Acceptable skewness between 2 and --2 

skewness(df4$count) # highlight skewed
skewness(df5$count) # highly skewed, close to 0

skewness(df4$percentage)  # moderately skewed
skewness(df5$percentage)  # highly skewed, close to 0

df4.skew <- ggplot(df4, aes(percentage, count)) + geom_point()
df5.skew <- ggplot(df5, aes(percentage, count)) + geom_point()
grid.arrange(df4.skew, df5.skew, nrow=1)

# EDA
eda.df <- df4[1:636,]

df4$segment.type <- as.factor(df4$segment.type)
df4$segment.description <- as.factor(df4$segment.description)
df4$answer <- as.factor(df4$answer)

chi.ans.st <- chisq.test(table(eda.df$answer, eda.df$segment.type))
chi.ans.des <- chisq.test(table(eda.df$answer, eda.df$segment.description))
chi.st.des <- chisq.test(table(eda.df$segment.type, eda.df$segment.description))
chi.ans.st                # p-value < 0.05
chi.ans.des               # p-value < NA


corrplot(chi.ans.st$residuals, is.cor=FALSE)

# METHOD 2
# RESULTS
  # Chisq=3632, p < 0.05 = reject hypo -> relationship
  # Chisq=41.65, p < 0.05 = reject hypo -> relationship
  # Chisq=486.9, p > 0.05 = accept hypo -> no sig elationship
summary(table(df4$segment.type, df4$segment.description))
summary(table(df4$segment.type, df4$answer))
summary(table(df4$answer, df4$segment.description))


ggbarstats(
  data=df4,
  x=answer,
  y=segment.type
) + labs(caption=NULL)


# Distance Matrix Calculation
  # First check if any of the continuous variables still need to be altered
  # Note LV model will be using different distance metric
hist.count <- ggplot(df7, aes(x=count))+
  geom_histogram(color="darkblue", fill="lightblue")
hist.perc <- ggplot(df7, aes(x=percentage))+
  geom_histogram(color="darkblue", fill="lightblue")
grid.arrange(hist.count, hist.perc, nrow=1)

# Numeral values df7
gower_df <- daisy(df7,
                  metric = "gower",
                  type=list(logratio=4))
summary(gower_df)

silhouette <- c()
silhouette = c(silhouette, NA)
for(i in 2:10){
  pam_clusters = pam(as.matrix(gower_df),
                     diss = TRUE,
                     k = i)
  silhouette = c(silhouette ,pam_clusters$silinfo$avg.width)
}
plot(1:10, silhouette,
     xlab = "Clusters",
     ylab = "Silhouette Width")
lines(1:10, silhouette)

# String values df3.2
gower_df_string <- daisy(train.df2,
                         metric = "gower",
                         type=list(logratio=4))
summary(gower_df_string)
fviz_dist(gower_df_string, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

silhouette <- c()
silhouette = c(silhouette, NA)
for(i in 2:10){
  pam_clusters = pam(as.matrix(gower_df_string),
                     diss = TRUE,
                     k = i)
  silhouette = c(silhouette ,pam_clusters$silinfo$avg.width)
}
plot(1:10, silhouette,
     xlab = "Clusters",
     ylab = "Silhouette Width")
lines(1:10, silhouette)

# Results: 10 clusters 
# Reminder: Results show 10 clusters are best regardless if training or entire set

# PAM
# Can use the string values 
pam_model = pam(gower_df_string, diss = TRUE, k = 10)
train.df2[pam_model$medoids, ]

# Each row depicts a cluster

tsne_object <- Rtsne(gower_df_string, is_distance = TRUE)
tsne_df <- tsne_object$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_model$clustering))
ggplot(aes(x = X, y = Y), data = tsne_df) +
  geom_point(aes(color = cluster))

pam.res <- pam(train.df2, 10)
pam.res
pam.res$medoids
head(pam.res$clustering)

# Bind cluster to training dataset
pam.train <- train.df2
pam.train$cluster = pam.res$cluster
pam.train$cluster <- as.factor(pam.train$cluster)
head(pam.train)

fviz_cluster(pam.res, data = pam.train, 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)
ggplot(pam.train, aes(x=Percentage, y=Count, shape=Answer, color=cluster)) +
  geom_point() +
  geom_smooth(method=lm, aes(fill=cluster))
pam.res$clusinfo

ggplot(pam.train, aes(x=Percentage, y=Count, color=cluster)) + 
  geom_point(size=2) +
  theme_ipsum()

coul <- brewer.pal(5, "Set2") 
counts <- table(pam.train$Answer, pam.train$cluster)
barplot(counts, main="Popularity of SocMed Per Cluster",
        xlab="Clusters", col=coul,
        legend = rownames(counts))

# Per Cluster
cluster8 <- dplyr::filter(pam.train, cluster %in% c("8"))

# Not Significant since Ratio is not good
counts2 <- table(cluster8$Answer, cluster8$Segment.Type)
barplot(counts2, main="Popularity of SocMed Per Segment Type in Cluster 8",xlab="Clusters", col=coul, legend = rownames(counts))

# Finding top groups in Cluster 8 
cluster8.2 <- dplyr::filter(cluster8, Segment.Type %in%
                              c("Custom"))
cluster8.2.top <- cluster8.2 %>% 
  arrange(Count) %>% 
  group_by(Answer) %>%
  slice(1:2)

cluster8.2.bp <- ggplot(cluster8.2.top, aes(x=reorder(Segment.Description, -Count), y=Count, fill=Answer)) +
  geom_bar(stat="identity")+theme_minimal()
cluster8.2.bp

# Pie chart
cluster8.bp <- ggplot(cluster8, aes(x="", y=Count, fill=Answer))+
  geom_bar(width = 1, stat = "identity")
cluster8.bp
cluster8.pc <- cluster8.bp + coord_polar("y", start=0)
cluster8.pc


# LV
# String Distance
# Smaller set to test
length(unique(train.df2$Segment.Description))       # 234
str(train.df2)

# First and last third of df3
train.df3 <- df3.2[1:212,]
train.df4 <- df3.2[424:636,]
uniqueseg <- unique(as.character(train.df3$Segment.Description))
uniqueseg.df4 <- unique(as.character(train.df4$Segment.Description))

# First third 
lv.model <- stringdistmatrix(uniqueseg.df4, uniqueseg.df4, method = "lv")
rownames(lv.model) <- uniqueseg.df4
lv.hc <- hclust(as.dist(lv.model))

plot(lv.hc, hang=-1, cex=0.7)
rect.hclust(lv.hc, k=10, border=2:5)

lv.res <- data.frame(uniqueseg, cutree(lv.hc, k=5))
names(lv.res) <- c('modelname', 'cluster')

plot(table(lv.res$cluster))
print(paste('Average number of models per cluster : ', mean(table(lv.res$cluster))))

# Last third
lv2.model <- stringdistmatrix(uniqueseg.df4, uniqueseg, method = "lv")
rownames(lv2.model) <- uniqueseg.df4
lv2.hc <- hclust(as.dist(lv2.model))

plot(lv2.hc, hang=-1, cex=0.7)
rect.hclust(lv2.hc, k=10, border=2:5)

lv2.res <- data.frame(uniqueseg.df4, cutree(lv2.hc, k=5))
names(lv2.res) <- c('modelname', 'cluster')

plot(table(lv2.res$cluster))
print(paste('Average number of models per cluster : ', mean(table(lv2.res$cluster))))

# info of top groups
t <- table(lv2.res$cluster)
t <- cbind(t, t/length(lv2.res$cluster))
t <- t[order(t[,2], decreasing=TRUE),]
p <- data.frame(factorName=rownames(t), binCount=t[,1], percentFound=t[,2])
lv2.res <- merge(x=lv2.res, y=p, by.x='cluster', by.y='factorName', all.x=T)
lv2.res <- lv.res[rev(order(lv2.res$binCount)),]
names(lv2.res) <- c('cluster', 'modelname')
lv2.res[c('cluster', 'modelname')]

# LCA
str(train.df3)
lca.df = poLCA(cbind(Segment.Type, 
                   Segment.Description,
                   Answer) ~ 1, 
             maxiter=50000, nclass=10, 
             nrep=10, data=train.df3)

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
library(ggstatsplot)
library(fpc)                      # Cluster Validation


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

sum(df$Count==0)
sum(df$Percentage==1)

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
bx_count

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
write.csv(df3,"D:/Portfolio/FYP/completed-data.csv", row.names = FALSE)
df4 <- read.csv("D:/Portfolio/FYP/completed-data.csv")
glimpse(df4)
is.null(df4)
summary(df4)
View(df4)

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
View(df4)

df4$segment.description <- sub(".*Asian.*", "Asian", df4$segment.description)
df4$segment.description <- sub(".*Jewish.*", "Jewish", df4$segment.description)
df4$segment.description <- sub(".*Black*", "Black", df4$segment.description)
df4$segment.description <- sub(".*Hispanic*", "Hispanic", df4$segment.description)
df4$segment.description <- sub(".*White*", "White", df4$segment.description)


# Scaling
# Requires all variables to be numerical
# df4 = original data
test <- df4
df4$segment.type <- as.numeric(df4$segment.type)
df4$segment.description <- as.numeric(df4$segment.description)
df4$answer <- as.numeric(df4$answer)
str(df4)

# Skewness
df4.skew.count <- ggplot(data = df4, aes(count)) + geom_histogram()
df4.skew.perc <- ggplot(data = df4, aes(percentage)) + geom_histogram() 
grid.arrange(df4.skew.count,df4.skew.perc, nrow=1)

summary(df4$count)
summary(df4$percentage)
skewness(df4$count) 
skewness(df4$percentage) 


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
grid.arrange(df5.skew.count,df5.skew.perc, nrow=2)

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

# Conclusion: 
  # MMS scales the values but skewness doesn't change
  # Log Trans shows better results
    # Residual standard error (0.1569)
    # R2 is close to 0 (0.0317)

# EDA
eda.df <- df4[1:636,]
eda.df$segment.type <- as.factor(eda.df$segment.type)
eda.df$segment.description <- as.factor(eda.df$segment.description)
eda.df$answer <- as.factor(eda.df$answer)
glimpse(eda.df)

chi.ans.st <- chisq.test(table(df4$answer, df4$segment.type), simulate.p.value=TRUE)
chi.ans.des <- chisq.test(table(df4$answer, df4$segment.description), simulate.p.value=TRUE)
chi.st.des <- chisq.test(table(df4$segment.type, df4$segment.description),simulate.p.value=TRUE)
chi.ans.st
chi.ans.des
chi.st.des

# RESULTS
# Chisq=3632, p < 0.05 = reject hypo -> relationship
# Chisq=486.9, p > 0.05 = accept hypo -> no sig relationship
# Chisq=41.65, p < 0.05 = reject hypo -> relationship

corrplot(chi.ans.st$residuals, is.cor=FALSE)

# METHOD 2
df4$segment.type <- as.factor(df4$segment.type)
df4$segment.description <- as.factor(df4$segment.description)
df4$answer <- as.factor(df4$answer)
str(df4)

summary(table(df4$segment.type, df4$segment.description))
summary(table(df4$segment.type, df4$answer))
summary(table(df4$answer, df4$segment.description))
summary(table(df4$answer, df4$segment.type, df4$segment.description))

ggbarstats(
  data=df3.2,
  x=Answer,
  y=Segment.Type
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
df7 <- df5
str(df7)
glimpse(df7)
gower_df <- daisy(df7,
                  metric = "gower",
                  type=list(logratio=4))
summary(gower_df)

# Gower on strings
df3.2 <- read.csv("D:/Portfolio/FYP/completed-data.csv")
str(df3.2)
df3.2 = df3.2 %>% 
  rename(
    segment.type = Segment.Type,
    segment.description = Segment.Description,
    answer = Answer,
    count = Count,
    percentage = Percentage
  )
df3.2$segment.type <- as.factor(df3.2$segment.type)
df3.2$segment.description <- as.factor(df3.2$segment.description)
df3.2$answer <- as.factor(df3.2$answer)
train.df2 <- df3.2[1:636,]
train.df2.2 <- df3.2[1:230,]
train.df3 <- df3.2[1:212,]
train.df4 <- df3.2[424:636,]

gower_df_string <- daisy(train.df2,
                         metric = "gower",
                         type=list(logratio=4))
summary(gower_df_string)
fviz_dist(gower_df_string, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))


# LV String Distance
# String values df3.2
# First and last third of df3
str(df3.2)
View(train.df2)
uniqueseg.df4 <- unique(as.character(train.df4$segment.description))
uniqueans.df4 <- unique(as.character(train.df4$answer))
uniqueans.df4
lv.model <- stringdistmatrix(uniqueseg.df4, uniqueseg.df4, method = "lv")
lv.model

# Silhouette with Strings
silhouette <- c()
silhouette = c(silhouette, NA)
for(i in 2:10){
  pam_clusters = pam(as.matrix(gower_df_string),
                     diss = TRUE,
                     k = i)
  silhouette = c(silhouette ,pam_clusters$silinfo$avg.width)
}
silhouette
plot(1:10, silhouette,
     xlab = "Clusters",
     ylab = "Silhouette Width")
lines(1:10, silhouette)

# Results: 10 clusters 
# Reminder: Results show 10 clusters are best regardless if training or entire set

# Silhouette with Num
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

ggplot(pam.train, aes(x=percentage, y=count, color=cluster)) + 
  geom_point(size=2) +
  theme_ipsum()

coul <- brewer.pal(5, "Set2") 
counts <- table(pam.train$answer, pam.train$cluster)
barplot(counts, main="Popularity of SocMed Per Cluster",
        xlab="Clusters", col=coul,
        legend = rownames(counts),
        args.legend = list(x = "topleft",
         bty = "n", inset=c(0, 0)))

# Per Cluster
pam.cluster4 <- dplyr::filter(pam.train, cluster %in% c("4"))
pam.cluster9 <- dplyr::filter(pam.train, cluster %in% c("9"))

# Not Significant since Ratio is not good
counts2 <- table(pam.cluster4$answer, pam.cluster4$segment.type)
counts3 <- table(pam.cluster9$answer, pam.cluster9$segment.type)

barplot(counts2, 
        main="Popularity of SocMed Per Segment Type in Cluster 4",
        xlab="Clusters", 
        col=coul, 
        legend = rownames(counts),
        args.legend = list(x = "topright",
                           bty = "n", inset=c(0, 0)))
barplot(counts3, 
         main="Popularity of SocMed Per Segment Type in Cluster 9",
         xlab="Clusters", 
         col=coul, 
         legend = rownames(counts),
         args.legend = list(x = "topright",
                            bty = "n", inset=c(0, 0)))
grid.arrange(c4.bp, c9.bp, nrow=2)
summary(pam.cluster4$answer)
summary(pam.cluster9$answer)

# Finding top groups in Cluster 8 
pam.cluster4.2 <- dplyr::filter(pam.cluster4, Segment.Type %in%
                              c("Custom"))
pam.cluster4.2.top <- pam.cluster4.2 %>% 
  arrange(Count) %>% 
  group_by(Answer) %>%
  slice(1:2)
pam.cluster4.2.bp <- ggplot(pam.cluster4.2.top, aes(x=reorder(Segment.Description, -Count), y=Count, fill=Answer)) +
  geom_bar(stat="identity")+theme_minimal()
pam.cluster4.2.bp

pam.cluster9.2 <- dplyr::filter(pam.cluster9, segment.type %in%
                                  c("Custom"))
pam.cluster9.2.top <- pam.cluster9.2 %>% 
  arrange(count) %>% 
  group_by(answer) %>%
  slice(1:2)
pam.cluster9.2.bp <- ggplot(pam.cluster9.2.top, aes(x=reorder(segment.description, -count), y=count, fill=answer)) +
  geom_bar(stat="identity")+theme_minimal()
grid.arrange(pam.cluster4.2.bp, pam.cluster9.2.bp, nrow=2)

# Pie chart
pam.cluster4.bp <- ggplot(pam.cluster4, aes(x="", y=count, fill=answer))+
  geom_bar(width = 1, stat = "identity")
pam.cluster4.bp
pam.cluster4.pc <- pam.cluster4.bp + coord_polar("y", start=0)
pam.cluster4.pc

pam.cluster9.bp <- ggplot(pam.cluster9, aes(x="", y=count, fill=answer))+
  geom_bar(width = 1, stat = "identity")
pam.cluster9.bp
pam.cluster9.pc <- pam.cluster9.bp + coord_polar("y", start=0)
grid.arrange(pam.cluster9.pc, pam.cluster4.pc, nrow=1)
summary(pam.cluster4.pc$data)

# K-Prototype
library(clustMixType)
set.seed(7)
str(train.df2)
str(train.df4)
train.df.kproto <- train.df2 %>%
  clustMixType::kproto(k=10,
                       nstart=25)

train.df.kproto <- train.df4 %>%
clustMixType::kproto(k=10,
                       nstart=25) # no. of initializations
train.df.kproto
train.df.kproto$cluster
train.df.kproto$size

clustMixType::clprofiles(object=train.df.kproto,
                           x=train.df2)
train.df.kproto %>%
  summary()
kproto.res <- train.df2 %>%
  mutate(kproto_clusters = train.df.kproto$cluster)
glimpse(kproto.res)
kproto.res$kproto_clusters <- as.factor(kproto.res$kproto_clusters)

kproto.res$answer
kproto.res$kproto_clusters
counts2 <- table(kproto.res$answer, kproto.res$kproto_clusters)

barplot(counts, main="Popularity of SocMed Per Cluster",
        xlab="Clusters", col=coul,
        legend = rownames(counts),
        args.legend = list(x = "topright",
        bty = "n", inset=c(-0.3, 0)))

# Distribution of Cluster
ggplot(kproto.res, aes(x=percentage, y=count, shape=answer, color=kproto_clusters)) +
  geom_point() +
  geom_smooth(method=lm, aes(fill=kproto_clusters))



# Per Cluster
kp.cluster8 <- dplyr::filter(kproto.res, kproto_clusters %in% c("8"))
kp.cluster8.2 <- dplyr::filter(kproto.res, segment.type %in%
                                  c("Custom"))

# Barplot
kp.cluster8.top <- kp.cluster8 %>% 
  arrange(count) %>% 
  group_by(answer) %>%
  slice(1:2)
kp.cluster8.bp <- ggplot(kp.cluster8.top, aes(x=reorder(segment.description, -count), y=count, fill=answer)) +
  geom_bar(stat="identity")+theme_minimal()
kp.cluster8.bp


# Better
# Groups in Cluster 8
# Shows what makes cluster 8
kp.cluster8.2.top <- kp.cluster8.2 %>% 
  arrange(Count) %>% 
  group_by(Answer) %>%
  slice(1:2)
unique(kp.cluster8.2[c("Segment.Description")])       


kp.cluster8.2.bp <- ggplot(kp.cluster8.2.top, aes(x=reorder(Segment.Description, -Count), y=Count, fill=Answer)) +
  geom_bar(stat="identity")+theme_minimal()
kp.cluster8.2.bp

# Pie chart
kp.cluster8.2.bp <- ggplot(kp.cluster8.2, aes(x="", y=Count, fill=Answer))+
  geom_bar(width = 1, stat = "identity")
kp.cluster8.2.pc <- kp.cluster8.2.bp + coord_polar("y", start=0)
kp.cluster8.2.pc

# LV
uniqueseg.df4 <- unique(as.character(train.df4$segment.description))
lv.model <- stringdistmatrix(uniqueseg.df4, uniqueseg.df4, method = "lv")
head(lv.model)

set.seed(7)
rownames(lv.model) <- uniqueseg.df4
lv.hc <- hclust(as.dist(lv.model))
plot(lv.hc, hang=-1, cex=0.7)
rect.hclust(lv.hc, k=5, border=2:5)
str(uniqueseg)
lv.res <- data.frame(uniqueseg, cutree(lv.hc, k=5))
str(lv.res)
plot(table(lv.res$cluster))
print(paste('Average number of models per cluster : ', mean(table(lv.res$cluster))))
names(lv.res) <- c('cluster', 'modelname')

# Integrate into data
lv.cluster.res <- lv.res[c('cluster', 'modelname')]
lv.cluster.res$cluster <- as.factor(lv.cluster.res$cluster)
lv.cluster.res = lv.cluster.res %>% 
  rename(
    segment.description = modelname
  )
train.df4.lv <- train.df4
train.df4.lv = subset(train.df4.lv, select= -c(segment.type, percentage))
lv.cluster.res.2 <- inner_join(train.df4.lv, lv.cluster.res)
str(lv.cluster.res.2)

# Overall
counts.lv <- table(lv.cluster.res.2$answer, lv.cluster.res.2$cluster)
barplot(counts.lv, 
        main="Popularity of SocMed Per Cluster",
        xlab="Clusters", 
        col=coul, 
        legend = rownames(counts.lv),
        args.legend = list(x = "topright",
                           bty = "n", inset=c(0, 0)))

# Per Cluster
# Barplot
# Cluster 3
lv.cluster3 <- dplyr::filter(lv.cluster.res.2, cluster %in% c("3"))
lv.cluster3.top <- lv.cluster3 %>% 
  arrange(count) %>% 
  group_by(answer) %>%
  slice(1:2)
lv.cluster3.bp <- ggplot(lv.cluster3.top, aes(x=reorder(segment.description, -count), y=count, fill=answer)) +
  geom_bar(stat="identity")+theme_minimal()
lv.cluster3.bp

# Cluster 4
lv.cluster4 <- dplyr::filter(lv.cluster.res.2, cluster %in% c("4"))
lv.cluster4.top <- lv.cluster4 %>% 
  arrange(count) %>% 
  group_by(answer) %>%
  slice(1:2)
lv.cluster4.bp <- ggplot(lv.cluster4.top, aes(x=reorder(segment.description, -count), y=count, fill=answer)) +
  geom_bar(stat="identity")+theme_minimal()
lv.cluster4.bp



# Silhouette Coefficient - Cluster Validation
# PAM
fviz_silhouette(pam.res,
                ggtheme = theme_classic())
pam.silinfo <- pam.res$silinfo
pam.silinfo$clus.avg.widths
pam.silinfo$avg.width

# K-P
kp.silinfo <- silhouette(train.df.kproto$cluster, dist(train.df.kproto$dists))
head(kp.silinfo[, 1:3], 10)
fviz_silhouette(kp.silinfo)
kp2.silinfo <- summary(kp.silinfo)
kp2.silinfo$avg.width

# LV
lv.silinfo <- silhouette(lv.res$cluster, dist(lv.model))
fviz_silhouette(lv.silinfo)
lv.silinfo <- summary(lv.silinfo)
lv.silinfo$avg.width

# PAM BEST


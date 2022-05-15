fyp.df <- read.csv("D:/Portfolio/FYP/fyp-questionnaire.csv")

install.packages("dplyr")
install.packages("tidyverse")
install.packages("mice")
install.packages("DataExplorer")
install.packages("EnvStats")      # Rosners' Test
install.packages("VIM")           # V for Missing Data
install.packages("caret")         # Scaling
install.packages("e1071")         # Skewness
install.packages("daisy")         # Gower Distance

library(dplyr)
library(tidyverse)
library(ggplot2)
library(gridExtra)

sum(is.na(fyp.df))
summary(is.na(fyp.df))
md.pattern(fyp.df)
View(fyp.df)
str(fyp.df)
fyp.df2 = subset(fyp.df, select= c(
  What.is.your.gender.,
  Where.are.you.from.,
  What.is.your.age.group.,
  What.is.your.employment.status...academic.level.,
  What.social.platform.has.influenced.your.online.shopping.most.
))

str(fyp.df2)
fyp.df2 = fyp.df2 %>%
  rename(
    gender = What.is.your.gender.,
    location = Where.are.you.from.,
    age = What.is.your.age.group.,
    employment.status = What.is.your.employment.status...academic.level.,
    answer = What.social.platform.has.influenced.your.online.shopping.most.
  )
table(fyp.df2[ , c("gender","location", "age", "employment.status", "answer")])
fyp.df3 = fyp.df2 %>%
  group_by(gender, location, age, employment.status, answer) %>%
  mutate(count = n())
summary(fyp.df3)

# Removing Duplicates
fyp.df3 <- fyp.df3[!duplicated(fyp.df3), ]

fyp.df3 <- subset(fyp.df3, count!=0)
md.pattern(fyp.df3)
str(fyp.df3)
View(ws.df)

write.csv(fyp.df3,"D:/Portfolio/FYP/fyp-completed-data.csv", row.names = FALSE)
fyp.df4 <- read.csv("D:/Portfolio/FYP/fyp-completed-data.csv")

fyp.df4$gender <- as.factor(fyp.df4$gender)
fyp.df4$location <- as.factor(fyp.df4$location)
fyp.df4$age <- as.factor(fyp.df4$age)
fyp.df4$employment.status <- as.factor(fyp.df4$employment.status)
fyp.df4$answer <- as.factor(fyp.df4$answer)
fyp.df4$count <- as.numeric(fyp.df4$count)
str(fyp.df4)

fyp.df4$gender = factor(fyp.df4$gender,
                        levels = c('Male', 'Female'),
                        labels = c(0,1))
fyp.df4$employment.status = factor(fyp.df4$employment.status,
                                   levels = c("Bachelors' Graduate", 'Working', 'High School Graduate', 'Diploma Graduate'),
                                   labels = c(0,1,2,3))
fyp.df4$answer = factor(fyp.df4$answer,
                        levels = c('None', 'Facebook', 'Instagram', 'Snapchat', 'Twitter'),
                        labels = c(0,1,2,3,4))

str(fyp.df4)
fyp.df4.dup <- fyp.df4
fyp.df4 <- fyp.df4.dup
str(fyp.df4)

# Hampel Filter 
lower_bound <- median(fyp.df4$count) - 3 * mad(fyp.df4$count, constant = 1)
lower_bound
upper_bound <- median(fyp.df4$count) + 3 * mad(fyp.df4$count, constant = 1)
upper_bound
outlier_ind <- which(fyp.df4$count < lower_bound | fyp.df4$count > upper_bound)

# 7 observed outliers
str(outlier_ind)                

# Rosner's Test
# Considers 7 values as outliers
ros <- rosnerTest(fyp.df4$count, 
                  k=7
)
ros$all.stats

# Confirms 9 outliers
plot_histogram(fyp.df4$count)

sum(fyp.df4$count > 5)
fyp.outliers <- fyp.df4
str(fyp.df4)

# Changing values where Count >= 10 to NA
fyp.outliers$count[fyp.outliers$count > 5] <- NA
fyp.outliers
md.pattern(fyp.outliers)

tempData <- mice(fyp.outliers, m=5, maxit=50, meth="pmm", seed=500)
class(tempData)
summary(tempData)
tempData$imp$count
fyp.df4 <- complete(tempData, )
str(fyp.df4)
md.pattern(fyp.df4)
plot_histogram(fyp.df4$count)

summary(fyp.df4)
skewness(fyp.df4$count)

# Mean & Skewness is positive so no editing needed

fyp.df4.dup2 <- fyp.df4
fyp.df4 <- fyp.df4.dup2

max_col_num <- 4 
max_col_size <- 65 
str(fyp.df4)

# Fix to dataset format
final.fyp.df <- as.data.frame(matrix(ncol = 4, nrow = 65))
final.fyp.df
final.fyp.df = final.fyp.df %>%
  rename(
    segment.type = V1,
    segment.description = V2,
    count = V3,
    answer = V4
  )
str(final.fyp.df)

# Gower Distance
set.seed(123)
gower_fyp_string <- daisy(fyp.df4,
                         metric = "gower",
                         type=list(logratio=4))
summary(gower_fyp_string)
fviz_dist(gower_fyp_string, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

# Gower Distance
silhouette <- c()
silhouette = c(silhouette, NA)
for(i in 2:10){
  pam_clusters = pam(as.matrix(gower_fyp_string),
                     diss = TRUE,
                     k = i)
  silhouette = c(silhouette ,pam_clusters$silinfo$avg.width)
}
silhouette
plot(1:10, silhouette,
     xlab = "Clusters",
     ylab = "Silhouette Width")
lines(1:10, silhouette)

# Gender
gender.df <- final.fyp.df
gender.df$segment.type <- "Gender"
gender.df$segment.type <- as.factor(gender.df$segment.type)
gender.df$segment.description <- as.factor(fyp.df4$gender)
gender.df$segment.description = factor(gender.df$segment.description,
                          levels = c(0,1),
                          labels = c('Male', 'Female'))
gender.df$answer <- as.factor(fyp.df4$answer)
gender.df$count <- fyp.df4$count
gender.df$answer = factor(gender.df$answer,
                          levels = c(0,1,2,3,4),
                        labels = c('None', 'Facebook', 'Instagram', 'Snapchat', 'Twitter'))
str(gender.df)
                        
# Location
loc.df <- final.fyp.df
loc.df$segment.type <- "Location"
loc.df$segment.description <- as.factor(fyp.df4$location)
loc.df$segment.type <- as.factor(loc.df$segment.type)
loc.df$count <- paste(fyp.df4$count)
loc.df$count <- as.numeric(loc.df$count)
loc.df$answer <- as.factor(fyp.df4$answer)
loc.df$answer = factor(loc.df$answer,
                          levels = c(0,1,2,3,4),
                          labels = c('None', 'Facebook', 'Instagram', 'Snapchat', 'Twitter'))
str(loc.df)

# Age
age.df <- final.fyp.df
age.df$segment.description <- as.factor(fyp.df4$age)
age.df$segment.type <- "Age"
age.df$segment.type <- as.factor(age.df$segment.type)
age.df$count <- paste(fyp.df4$count)
age.df$count <- as.numeric(age.df$count)
age.df$answer <- as.factor(fyp.df4$answer)
age.df$answer = factor(age.df$answer,
                       levels = c(0,1,2,3,4),
                       labels = c('None', 'Facebook', 'Instagram', 'Snapchat', 'Twitter'))
str(age.df)

# Working Status
ws.df <- final.fyp.df
ws.df$segment.description <- as.factor(fyp.df4$employment.status)
ws.df$segment.type <- "Working Status"
ws.df$segment.type <- as.factor(ws.df$segment.type)
ws.df$segment.description = factor(ws.df$segment.description,
                                   levels = c(0,1,2,3),
                                   labels = c("Bachelors' Graduate", 'Working', 'High School Graduate', 'Diploma Graduate'))
ws.df$count <- paste(fyp.df4$count)
ws.df$count <- as.numeric(ws.df$count)
ws.df$answer <- as.factor(fyp.df4$answer)
ws.df$answer = factor(ws.df$answer,
                       levels = c(0,1,2,3,4),
                       labels = c('None', 'Facebook', 'Instagram', 'Snapchat', 'Twitter'))


# PAM
set.seed(123)
pam_fyp_model = pam(gower_fyp_string, diss = TRUE, k = 4)
fyp.df4[pam_fyp_model$medoids, ]

# Gender
pam.fyp.gender <- pam(gender.df, 4)
pam.fyp.gender
pam.fyp.gender$medoids
head(pam.fyp.gender$clustering)

# Bind cluster to training dataset
pam.test.gender <- gender.df
pam.test.gender$cluster = pam.fyp.gender$cluster
pam.test.gender$cluster <- as.factor(pam.test.gender$cluster)
head(pam.test.gender)

coul <- brewer.pal(5, "Set2") 
counts <- table(pam.test.gender$answer, pam.test.gender$cluster)

pam.bp.gender <- barplot(counts, main="Gender",
        xlab="Clusters", col=coul,
        legend = rownames(counts),
        args.legend = list(x = "topleft",
                           bty = "n", inset=c(0, 0)))

pam.fyp.gender$clusinfo

# Cluster 3
# Per Cluster
fyp.cluster3 <- dplyr::filter(pam.test.gender, cluster %in% c("3"))

str(pam.test.gender)
counts.fyp.gender <- table(fyp.cluster3$answer, fyp.cluster3$segment.description)
barplot(counts.fyp.gender, 
        main="SocMed Per Gender in Cluster 3",
        xlab="Genders", 
        col=coul, 
        legend = rownames(counts.fyp.gender),
        args.legend = list(x = "topleft",
                           bty = "n", inset=c(0, -0.1)))

# Location
pam.fyp.loc <- pam(loc.df, 4)
pam.fyp.loc
pam.fyp.loc$medoids
head(pam.fyp.loc$clustering)

# Bind cluster to training dataset
pam.test.loc <- loc.df
pam.test.loc$cluster = pam.fyp.loc$cluster
pam.test.loc$cluster <- as.factor(pam.test.loc$cluster)
head(pam.test.loc)

coul <- brewer.pal(5, "Set2") 
counts <- table(pam.test.loc$answer, pam.test.loc$cluster)
pam.bp.loc <- barplot(counts, main="Location",
                      xlab="Clusters", col=coul,
                      legend = rownames(counts),
                      args.legend = list(x = "topleft",
                                         bty = "n", inset=c(0, 0)))
pam.fyp.loc$clusinfo

# Cluster 3
fyp.cluster3.loc <- dplyr::filter(pam.test.loc, cluster %in% c("3"))
str(pam.test.loc)

counts.fyp.loc <- table(fyp.cluster3.loc$answer, fyp.cluster3.loc$segment.description)
barplot(counts.fyp.loc, 
        main="SocMed Per Location in Cluster 3",
        xlab="Locations", 
        col=coul, 
        legend = rownames(counts.fyp.loc),
        args.legend = list(x = "topleft",
                           bty = "n", inset=c(0, -0.1)))
View(pam.test.loc)

# Age
pam.fyp.age <- pam(age.df, 4)
pam.fyp.age
pam.fyp.age$medoids
head(pam.fyp.age$clustering)

# Bind cluster to training dataset
pam.test.age <- age.df
pam.test.age$cluster = pam.fyp.age$cluster
pam.test.age$cluster <- as.factor(pam.test.age$cluster)
str(pam.test.age)

coul <- brewer.pal(5, "Set2") 
counts <- table(pam.test.age$answer, pam.test.age$cluster)
pam.bp.age <- barplot(counts, main="Age",
                      xlab="Clusters", col=coul,
                      legend = rownames(counts),
                      args.legend = list(x = "topleft",
                                         bty = "n", inset=c(0, 0)))
pam.fyp.age$clusinfo

# Cluster 3
fyp.cluster3.age <- dplyr::filter(pam.test.age, cluster %in% c("3"))
str(pam.test.age)

counts.fyp.age <- table(fyp.cluster3.age$answer, fyp.cluster3.age$segment.description)
barplot(counts.fyp.age, 
        main="SocMed Per Age in Cluster 3",
        xlab="Age Demographics", 
        col=coul, 
        legend = rownames(counts.fyp.age),
        args.legend = list(x = "topleft",
                           bty = "n", inset=c(0, -0.1)))
View(pam.test.age)

# Working Status
pam.fyp.ws <- pam(ws.df, 4)
pam.fyp.ws$medoids
head(pam.fyp.ws$clustering)

# Bind cluster to training dataset
pam.test.ws <- ws.df
pam.test.ws$cluster = pam.fyp.ws$cluster
pam.test.ws$cluster <- as.factor(pam.test.ws$cluster)
str(pam.test.ws)

coul <- brewer.pal(5, "Set2") 
counts <- table(pam.test.ws$answer, pam.test.ws$cluster)
pam.bp.ws <- barplot(counts, main="Working Status",
                      xlab="Clusters", col=coul,
                      legend = rownames(counts),
                      args.legend = list(x = "topright",
                                         bty = "n", inset=c(0, 0)))
pam.fyp.ws$clusinfo

# Cluster 3
fyp.cluster3.ws <- dplyr::filter(pam.test.ws, cluster %in% c("3"))
str(pam.test.ws)

counts.fyp.ws <- table(fyp.cluster3.ws$answer, fyp.cluster3.ws$segment.description)
barplot(counts.fyp.ws, 
        main="SocMed Per Working Status in Cluster 3",
        xlab="Working Status", 
        col=coul, 
        legend = rownames(counts.fyp.ws),
        args.legend = list(x = "topright",
                           bty = "n", inset=c(0, 0)))
View(pam.test.ws)

# Cluster 1
fyp.cluster1.ws <- dplyr::filter(pam.test.ws, cluster %in% c("1"))
str(pam.test.ws)

counts.fyp.ws <- table(fyp.cluster1.ws$answer, fyp.cluster1.ws$segment.description)
barplot(counts.fyp.ws, 
        main="SocMed Per Working Status in Cluster 1",
        xlab="Working Status", 
        col=coul, 
        legend = rownames(counts.fyp.ws),
        args.legend = list(x = "topright",
                           bty = "n", inset=c(0, 0)))

# Cluster 2
fyp.cluster2.ws <- dplyr::filter(pam.test.ws, cluster %in% c("2"))
str(pam.test.ws)

counts.fyp.ws <- table(fyp.cluster2.ws$answer, fyp.cluster2.ws$segment.description)
barplot(counts.fyp.ws, 
        main="SocMed Per Working Status in Cluster 2",
        xlab="Working Status", 
        col=coul, 
        legend = rownames(counts.fyp.ws),
        args.legend = list(x = "topleft",
                           bty = "n", inset=c(0, 0)))

# EVERYTHING
pam.bp.gender
pam.bp.loc
pam.bp.age
pam.bp.ws

# 1 = FB
# 2 = IG
# 3 = SC
# 4 = TW

# Gender
fyp.cluster1.gender <- dplyr::filter(pam.test.gender, cluster %in% c("1"))
fyp.cluster2.gender <- dplyr::filter(pam.test.gender, cluster %in% c("2"))
fyp.cluster3.gender <- dplyr::filter(pam.test.gender, cluster %in% c("3"))
fyp.cluster4.gender <- dplyr::filter(pam.test.gender, cluster %in% c("4"))
fyp.cluster1.gender$answer
fyp.cluster2.gender$answer
fyp.cluster3.gender$answer
fyp.cluster4.gender$answer
sum(fyp.cluster4.gender$answer=="Facebook")

# Location
fyp.cluster1.loc <- dplyr::filter(pam.test.loc, cluster %in% c("1"))
fyp.cluster2.loc <- dplyr::filter(pam.test.loc, cluster %in% c("2"))
fyp.cluster3.loc <- dplyr::filter(pam.test.loc, cluster %in% c("3"))
fyp.cluster4.loc <- dplyr::filter(pam.test.loc, cluster %in% c("4"))
fyp.cluster1.loc$answer
fyp.cluster2.loc$answer
fyp.cluster3.loc$answer
fyp.cluster4.loc$answer
sum(fyp.cluster4.loc$answer=="Instagram")

# Age
fyp.cluster1.age <- dplyr::filter(pam.test.age, cluster %in% c("1"))
fyp.cluster2.age <- dplyr::filter(pam.test.age, cluster %in% c("2"))
fyp.cluster3.age <- dplyr::filter(pam.test.age, cluster %in% c("3"))
fyp.cluster4.age <- dplyr::filter(pam.test.age, cluster %in% c("4"))
fyp.cluster1.age$answer
fyp.cluster2.age$answer
fyp.cluster3.age$answer
fyp.cluster4.age$answer
sum(fyp.cluster4.age$answer=="Snapchat")

# WS
fyp.cluster1.ws <- dplyr::filter(pam.test.ws, cluster %in% c("1"))
fyp.cluster2.ws <- dplyr::filter(pam.test.ws, cluster %in% c("2"))
fyp.cluster3.ws <- dplyr::filter(pam.test.ws, cluster %in% c("3"))
fyp.cluster4.ws <- dplyr::filter(pam.test.ws, cluster %in% c("4"))
fyp.cluster1.ws$answer
fyp.cluster2.ws$answer
fyp.cluster3.ws$answer
fyp.cluster4.ws$answer
sum(fyp.cluster3.ws$answer=="Instagram")

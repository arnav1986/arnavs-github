rm(list=ls())

#set working directory
setwd("C:/Users/Pc/Desktop/Edwisor Data scientist/Project - Santandar customer transaction prediction")
getwd()

#Load the librarires
libraries = c("plyr","dplyr", "ggplot2","rpart","dplyr","DMwR","randomForest","usdm","corrgram","DataCombine")
lapply(X = libraries,require, character.only = TRUE)
rm(libraries)
library(matrixStats)
library(fBasics)
library(psych)
library(tidyr)

#Read the csv file
train = read.csv(file = "train_santander.csv", header = T, sep = ",", na.strings = c(" ", "", "NA"))
test = read.csv(file = "test_santander.csv", header = T, sep = ",", na.strings = c(" ", "", "NA"))

head(train)
dim(train)
dim(test)
#************************************************************************************************
#Drop ID_Code column from train and test
train <- subset(train, select = -c(ID_code))
test <- subset(test, select = -c(ID_code))
#************************************************************************************************
#Check for missing values in both train and test
missing_values = sapply(train, function(x){sum(is.na(x))})
missing_values1 = sapply(test, function(x){sum(is.na(x))})
#No missing values found in the datasets
#************************************************************************************************
# Outlier analysis
#We saw in Python progrmming that we discard the outlier treatment as after removing outliers we reduce our dataset to 1/100th.
#***********************************************************************************************
#Feature Selection
#Correlation analysis
vifcor(train)
# As we can see that none of the 201 input variables has a collinearity problem.
#***********************************************************************************************
#Visualizations
bar1 = ggplot(data = train, aes(x = target)) + geom_bar()
gridExtra::grid.arrange(bar1)

feature_groups <- 1:25
col_names <- colnames(train)[c(1,feature_groups)]
temp <- gather(train[,col_names], key="features", value="value", -target)
temp$target <- factor(temp$target)
temp$features <- factor(temp$features, levels=col_names[-1], labels=col_names[-1])
ggplot(data=temp, aes(x=value)) + geom_density(aes(fill=target, color=target), alpha=0.3) + scale_color_manual(values = c("1" = "dodgerblue", "0"="firebrick1")) + theme_classic() + facet_wrap(~ features, ncol = 4, scales = "free")

col_names <- colnames(train)[c(1,feature_groups+25)]
temp <- gather(train[,col_names], key="features", value="value", -target)
temp$target <- factor(temp$target)
temp$features <- factor(temp$features, levels=col_names[-1], labels=col_names[-1])
ggplot(data=temp, aes(x=value)) +
  geom_density(aes(fill=target, color=target), alpha=0.3) +
  scale_color_manual(values = c("1" = "dodgerblue", "0"="firebrick1")) +
  theme_classic() +
  facet_wrap(~ features, ncol = 4, scales = "free")

col_names <- colnames(train)[c(1,feature_groups+50)]
temp <- gather(train[,col_names], key="features", value="value", -target)
temp$target <- factor(temp$target)
temp$features <- factor(temp$features, levels=col_names[-1], labels=col_names[-1])
ggplot(data=temp, aes(x=value)) +
  geom_density(aes(fill=target, color=target), alpha=0.3) +
  scale_color_manual(values = c("1" = "dodgerblue", "0"="firebrick1")) +
  theme_classic() +
  facet_wrap(~ features, ncol = 4, scales = "free")

col_names <- colnames(train)[c(1,feature_groups+75)]
temp <- gather(train[,col_names], key="features", value="value", -target)
temp$target <- factor(temp$target)
temp$features <- factor(temp$features, levels=col_names[-1], labels=col_names[-1])
ggplot(data=temp, aes(x=value)) +
  geom_density(aes(fill=target, color=target), alpha=0.3) +
  scale_color_manual(values = c("1" = "dodgerblue", "0"="firebrick1")) +
  theme_classic() +
  facet_wrap(~ features, ncol = 4, scales = "free")

#Compare density plots of Train and Test

train$key <- "train"
test$key <- "test"
full <- rbind(train[,-1], test)
full$key <- factor(full$key)

feature_groups <- 1:25
col_names <- colnames(full)[feature_groups]
temp <- gather(full[,c("key",col_names)], key="features", value="value", -key)
temp$features <- factor(temp$features, levels=col_names, labels=col_names)
ggplot(data=temp, aes(x=value)) +
  geom_density(aes(fill=key, color=key), alpha=0.3) +
  scale_color_manual(values = c("train" = "dodgerblue", "test"="firebrick1")) +
  theme_classic() +
  facet_wrap(~ features, ncol = 4, scales = "free")

feature_groups <- 1:20
col_names <- colnames(full)[feature_groups + 25]
temp <- gather(full[,c("key",col_names)], key="features", value="value", -key)
temp$features <- factor(temp$features, levels=col_names, labels=col_names)
ggplot(data=temp, aes(x=value)) +
  geom_density(aes(fill=key, color=key), alpha=0.3) +
  scale_color_manual(values = c("train" = "dodgerblue", "test"="firebrick1")) +
  theme_classic() +
  facet_wrap(~ features, ncol = 4, scales = "free")

col_names <- colnames(full)[feature_groups + 50]
temp <- gather(full[,c("key",col_names)], key="features", value="value", -key)
temp$features <- factor(temp$features, levels=col_names, labels=col_names)
ggplot(data=temp, aes(x=value)) +
  geom_density(aes(fill=key, color=key), alpha=0.3) +
  scale_color_manual(values = c("train" = "dodgerblue", "test"="firebrick1")) +
  theme_classic() +
  facet_wrap(~ features, ncol = 4, scales = "free")
#***********************************************************************************************
#Feature scaling
#Apply standardisation on both Train and test datset
features <- colnames(train[, c(2:201)])


#Standardization
for (i in features){
  print(i)
  train[,i] = (train[,i] - mean(train[,i]))/sd(train[,i])
}

for (i in features){
  print(i)
  test[,i] = (test[,i] - mean(test[,i]))/sd(test[,i])
}
#***********************************************************************************************
#Feature Engineering
train$sum <- rowSums(train[,2:201])
train$min <- do.call(pmin, train[,2:201])
train$max <- do.call(pmax, train[,2:201])
train$mean <- rowMeans(train[,2:201])
train$std <- rowSds(as.matrix(train[,2:201]), na.rm = TRUE)
train$skew <- rowSkewness(as.matrix(train[,2:201]), na.rm = TRUE)
train$kurt <- rowKurtosis(as.matrix(train[,2:201]), na.rm = TRUE)
train$med <- rowMedians(as.matrix(train[,2:201]), na.rm = TRUE)

test$sum <- rowSums(test[,1:200])
test$min <- do.call(pmin, test[,1:200])
test$max <- do.call(pmax, test[,1:200])
test$mean <- rowMeans(test[,1:200])
test$std <- rowSds(as.matrix(test[,1:200]), na.rm = TRUE)
test$skew <- rowSkewness(as.matrix(test[,1:200]), na.rm = TRUE)
test$kurt <- rowKurtosis(as.matrix(test[,1:200]), na.rm = TRUE)
test$med <- rowMedians(as.matrix(test[,1:200]), na.rm = TRUE)

#feature_groups <- 1:20
#col_names <- colnames(train)[c(1,feature_groups+190)]
#temp <- gather(train[,col_names], key="features", value="value", -target)
#temp$target <- factor(temp$target)
#temp$features <- factor(temp$features, levels=col_names[-1], labels=col_names[-1])
#ggplot(data=temp, aes(x=value)) + geom_density(aes(fill=target, color=target), alpha=0.3) + scale_color_manual(values = c("1" = "dodgerblue", "0"="firebrick1")) + theme_classic() + facet_wrap(~ features, ncol = 4, scales = "free")

#**********************************************************************************************

#Modelling
#1. Decision Tree

train$target = replace(train$target, train$target == 0, 'No')
train$target = replace(train$target, train$target == 1, 'Yes')

print('Number of transactions in train dataset before applying sampling methods')
print(table(train$target))

# Downsample for target class 
#install.packages('DMwR')
library(DMwR)
#install.packages('ROSE')
library(ROSE)
#install.packages('resample')
library(resample)
#install.packages('caret')
library(caret)
#install.packages('C50')
library(rpart)
library(e1071)

df_majority = subset(train, target == 'No')
df_minority = subset(train, target == 'Yes')
df_majority_downsampled = df_majority[sample(nrow(df_majority), 20098, replace = F),]
df_downsampled = rbind(df_minority,df_majority_downsampled)

print('Number of transactions in train dataset after applying Under sampling method')
print(table(df_downsampled$target))

set.seed(1234)
train.index = createDataPartition(df_downsampled$target, p = .80, list = FALSE)
train = df_downsampled[train.index,]
test = df_downsampled[-train.index,]

C50_model = rpart(target~., train, method = "class")
C50_predictions = predict(C50_model, test[,-1], type = "class")

confusion_C50 = table(test$target, C50_predictions)
confusionMatrix(confusion_C50)

#install.packages("pROC")
library(pROC)
roc.curve(test[,1], C50_predictions)
        #**********************************************************************************
#Random forest
library(randomForest)

#train$target = replace(train$target, train$target == 'No', 0)
#train$target = replace(train$target, train$target == 'Yes', 1)
#test$target = replace(test$target, test$target == 'No', 0)
#test$target = replace(test$target, test$target == 'Yes', 1)

train$target = as.factor(train$target)
train = subset(train, select = -c(key))

test$target = as.factor(test$target)
test = subset(test, select = -c(key))
sapply(train, class)

rf_model = randomForest(target~., train, importance = TRUE, ntree=100)
rf_predictions = predict(rf_model, test[,-1])

confusion_rf = table(test$target, rf_predictions)
confusionMatrix(confusion_rf)

roc.curve(test[,1], rf_predictions)
      #************************************************************************************
#Logistic regression

set.seed(1234)
train.index = createDataPartition(df_downsampled$target, p = .80, list = FALSE)
train = df_downsampled[train.index,]
test = df_downsampled[-train.index,]
train = subset(train, select = -c(key))
test = subset(test, select = -c(key))
train$target = as.factor(train$target)
test$target = as.factor(test$target)

lg_model = glm(target~., data = train, family = "binomial")
summary(lg_model)

lg_predictions = predict.lm(lg_model, newdata = test, type = "response")

confusion_lg = table(test$target, lg_predictions)
confusionMatrix(confusion_lg)

roc.curve(test[,1], lg_predictions)
      #***********************************************************************************

#KNN

library(class)

knn_predictions = knn(train[,2:209], test[,2:209], train$target, k=1)

confusion_knn = table(knn_predictions, test$target)
confusionMatrix(confusion_knn)

roc.curve(test[,1], knn_predictions)
      #***********************************************************************************

#Naive Bayes

library(e1071)

nb_model = naiveBayes(target~., data = train)
nb_predictions = predict(nb_model, test[,2:209], type = 'class')

confusion_nb = table(test$target, nb_predictions)
confusionMatrix(confusion_nb)

roc.curve(test[,1], nb_predictions)

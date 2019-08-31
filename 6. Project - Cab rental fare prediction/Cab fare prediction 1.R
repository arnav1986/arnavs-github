rm(list = ls())

library(tidyverse)
library(lubridate)
#install.packages("caret")
library(caret)
#install.packages("randomForest")
library(randomForest)
#install.packages("tictoc")
library(tictoc)
#install.packages("flexclust")
library(flexclust)
library(dplyr)
library(tidyr)

# grabbing  data
sampletrain = read.csv("train_cab.csv")

sampletest = read.csv("test.csv")


#convert fare_amount from factor to numeric
sampletrain$fare_amount = as.numeric(as.character(sampletrain$fare_amount))

# select fare_amount between 0 and 100
#sampletrain = subset(sampletrain, fare_amount >=0 & fare_amount <=100)

#convert pickup_datetime from factor to datetime numeric
sampletrain$pickup_datetime = strptime(x = as.character(sampletrain$pickup_datetime), "%Y-%m-%d %H:%M:%S UTC")

#Check for missing values
sum(is.na(sampletrain))
colSums(is.na(sampletrain))


#Fill fare amount with mean
sampletrain$fare_amount[is.na(sampletrain$fare_amount)] = mean(sampletrain$fare_amount, na.rm = T)

#Dropping 55 observations in passenger_count
sampletrain = na.omit(sampletrain)

#Create datetime features based on pickup_datetime
sampletrain$pickup_day_of_week = as.POSIXlt(sampletrain$pickup_date)$wday
#sampletrain$pickup_day_of_week = weekdays(as.POSIXlt(sampletrain$pickup_datetime))
sampletrain$pickup_year = as.numeric(format(sampletrain$pickup_datetime, "%Y"))
sampletrain$pickup_date = as.Date(format(sampletrain$pickup_datetime, "%Y-%m-%d"))
sampletrain$pickup_day = as.numeric(format(sampletrain$pickup_datetime, "%d"))
sampletrain$pickup_hour = as.numeric(format(sampletrain$pickup_datetime, "%H"))
sampletrain$pickup_month = as.numeric(format(sampletrain$pickup_datetime, "%m"))

sampletrain$pickup_day_of_week = as.numeric(sampletrain$pickup_day_of_week)
sapply(sampletrain, class)

sampletrain = dplyr::select(sampletrain, -c(pickup_datetime))
sampletrain = na.omit(sampletrain)

# filter out outliers
sampletrain = filter(sampletrain,
                      pickup_longitude < -72.986532 &
                        pickup_longitude > -74.263242 &
                        pickup_latitude > 40.573143 &
                        pickup_latitude < 41.709555 &
                        dropoff_longitude < -72 &
                        dropoff_longitude > -75 &
                        dropoff_latitude > 40 &
                        dropoff_latitude < 42 &
                        fare_amount >= 0 &
                        fare_amount <= 100 &
                        passenger_count < 10)


#convert pickup_datetime from factor to datetime numeric
sampletest$pickup_datetime = strptime(x = as.character(sampletest$pickup_datetime), "%Y-%m-%d %H:%M:%S UTC")


#Create datetime features based on pickup_datetime
sampletest$pickup_day_of_week = as.POSIXlt(sampletest$pickup_date)$wday
#sampletest$pickup_day_of_week = weekdays(as.numeric(sampletest$pickup_datetime))
sampletest$pickup_year = as.numeric(format(sampletest$pickup_datetime, "%Y"))
sampletest$pickup_date = as.Date(format(sampletest$pickup_datetime, "%Y-%m-%d"))
sampletest$pickup_day = as.numeric(format(sampletest$pickup_datetime, "%d"))
sampletest$pickup_hour = as.numeric(format(sampletest$pickup_datetime, "%H"))
sampletest$pickup_month = as.numeric(format(sampletest$pickup_datetime, "%m"))

sampletest$pickup_day_of_week = as.numeric(sampletest$pickup_day_of_week)
sapply(sampletest, class)

sampletest = dplyr::select(sampletest, -c(pickup_datetime))

# Creating clusters for pickup location
pickup_geoData <- dplyr::select(sampletrain,pickup_longitude, pickup_latitude)
pickup_clusters <- flexclust::kcca(pickup_geoData, k = 15, kccaFamily("kmeans"))
pickup_geoData$pickup_cluster <- as.factor(pickup_clusters@cluster)


pickup_geoDataPlot <- ggplot(pickup_geoData,aes(pickup_longitude, pickup_latitude, color = pickup_cluster))
pickup_geoDataPlot + geom_point(shape = 16, size = 0.2) + 
  scale_colour_hue() + 
  coord_fixed() + 
  theme(legend.position="none")

sampletrain$pickup_geoCluster <- pickup_geoData$pickup_cluster

pickup_geoData_test <- dplyr::select(sampletest, pickup_longitude, pickup_latitude)
sampletest$pickup_geoCluster <- as.factor(flexclust::predict(pickup_clusters, newdata = pickup_geoData_test))

#********************************************************************************************
# Creating clusters for dropoff location
dropoff_geoData <- dplyr::select(sampletrain, dropoff_longitude, dropoff_latitude)
dropoff_clusters <- flexclust::kcca(dropoff_geoData, k = 15, kccaFamily("kmeans"))
dropoff_geoData$dropoff_cluster <- as.factor(dropoff_clusters@cluster)

dropoff_geoDataPlot <- ggplot(dropoff_geoData,aes(dropoff_longitude, dropoff_latitude, color = dropoff_cluster))
dropoff_geoDataPlot + geom_point(shape = 16, size = 0.2) + 
  scale_colour_hue() + 
  coord_fixed() + 
  theme(legend.position="none")

sampletrain$dropoff_geoCluster <- dropoff_geoData$dropoff_cluster

dropoff_geoData_test <- dplyr::select(sampletest, dropoff_longitude, dropoff_latitude)
sampletest$dropoff_geoCluster <- as.factor(flexclust::predict(dropoff_clusters, newdata = dropoff_geoData_test))

#**********************************************************************************************
# Calculating manhattan distance
#sampletrain <- mutate(sampletrain, havershineDist = havershine((sampletrain$pickup_longitude, sampletrain$dropoff_longitude), (pickup_latitude, pickup_longitude), R = 6371.0)

sampletrain <- mutate(sampletrain, manhattanDist = abs(pickup_longitude - dropoff_longitude) + abs(pickup_latitude - dropoff_latitude))
sampletest <- mutate(sampletest, manhattanDist = abs(pickup_longitude - dropoff_longitude) + abs(pickup_latitude - dropoff_latitude))

head(sampletrain$pickup_date)
# drop unwanted columns
sampletrain <- dplyr::select(sampletrain, -pickup_date, -pickup_hour)

sampletest <- dplyr::select(sampletest, -pickup_date, -pickup_hour)

sampletrain$pickup_geoCluster = as.numeric(sampletrain$pickup_geoCluster)
sampletrain$dropoff_geoCluster = as.numeric(sampletrain$dropoff_geoCluster)
sampletest$pickup_geoCluster = as.numeric(sampletest$pickup_geoCluster)
sampletest$dropoff_geoCluster = as.numeric(sampletest$dropoff_geoCluster)


#**********************************************************************************************
# Dividing data in to test and train

train_index = sample(1:nrow(sampletrain), 0.8 * nrow(sampletrain))
train = sampletrain[train_index,]
test = sampletrain[-train_index,]

sapply(sampletrain, class)
sapply(sampletest,class)
#sapply(train, is.factor)
#sapply(train, is.character)

#Linear Regression
#Check Multicollinearity
#install.packages("usdm")
library(usdm)
summary(sampletrain)
cor(train)
vifcor(sampletrain[, 0:-1], th = 1)

#run regression model
lm_model = lm(fare_amount ~., data = train)

#Summary of the model
summary(lm_model)

#Predict
predictions_LR = predict(lm_model, sampletest)

output <- cbind(sampletest, predictions_LR)
predictions_LR

#Calculate RMSE
#install.packages("Metrics")
library(Metrics)
rmse(sampletrain$fare_amount, predictions_LR)

##.......................##

#Decision Tree

library(rpart)
library(MASS)
#run decision tree model
fit = rpart(fare_amount ~ ., data = train, method = "anova")

#summary of the model
summary(fit)

#Predict
predictions_DT = predict(fit, sampletest)

output1 <- cbind(sampletest, predictions_DT)
predictions_DT

#Calculate RMSE
rmse(sampletrain$fare_amount, predictions_DT)

#................................................#

# Random Forrest

# Defult ntree = 500.
# Finding a better mtry than standard p/3 takes a long time, will use defult.

RF_model = randomForest(fare_amount ~ ., train, importance = TRUE, ntree = 500)

summary(RF_model)

# model prediction
predictRF <- predict(RF_model, sampletest)

output2 = cbind(sampletest, predictRF)

# Using root mean squared as error function
rmse(sampletrain$fare_amount, predictRF)

#*********************************************************************************************#


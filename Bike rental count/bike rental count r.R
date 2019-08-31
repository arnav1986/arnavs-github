rm(list=ls())

#set working directory
setwd("C:/Users/Pc/Desktop/Edwisor Data scientist/6. Project - Bike rental count")
getwd()

#Load the librarires
libraries = c("plyr","dplyr", "ggplot2","rpart","dplyr","DMwR","randomForest","usdm","corrgram","DataCombine")
lapply(X = libraries,require, character.only = TRUE)
rm(libraries)

#Read the csv file
day = read.csv(file = "day.csv", header = T, sep = ",", na.strings = c(" ", "", "NA"))

########################################EXPLORE THE DATA########################################
#First few rows
head(day)

#Dimensions of data
dim(day)

#Column names
names(day)

#Structure of variables
str(day)

########################################FEATURE ENGINEERING########################################
#Create columns
day$actual_temp <- day$temp*39
day$actual_feel_temp <- day$atemp*50
day$actual_windspeed <- day$windspeed*67
day$actual_hum = day$hum * 100

day$actual_season = factor(x = day$season, levels = c(1,2,3,4), labels = c("Spring","Summer","Fall","Winter"))
day$actual_yr = factor(x = day$yr, levels = c(0,1), labels = c("2011","2012"))
day$actual_holiday = factor(x = day$holiday, levels = c(0,1), labels = c("Working day","Holiday"))
day$actual_weathersit = factor(x = day$weathersit, levels = c(1,2,3,4), 
                               labels = c("Clear","Cloudy/Mist","Rain/Snow/Fog","Heavy Rain/Snow/Fog"))

day$weathersit = as.factor(day$weathersit)
day$season = as.factor(day$season)
day$dteday = as.character(day$dteday)
day$mnth = as.factor(day$mnth)
day$weekday = as.factor(as.character(day$weekday))
day$workingday = as.factor(as.character(day$workingday))
day$yr = as.factor(day$yr)
day$holiday = as.factor(day$holiday)

########################################MISSING VALUES########################################
missing_values = sapply(day, function(x){sum(is.na(x))})


########################################EXPLORE USING GRAPHS########################################
#Check the distribution of categorical Data using bar graph
bar1 = ggplot(data = day, aes(x = actual_season)) + geom_bar() + ggtitle("Count of Season")
bar2 = ggplot(data = day, aes(x = actual_weathersit)) + geom_bar() + ggtitle("Count of Weather")
bar3 = ggplot(data = day, aes(x = actual_holiday)) + geom_bar() + ggtitle("Count of Holiday")
bar4 = ggplot(data = day, aes(x = workingday)) + geom_bar() + ggtitle("Count of Working day")
# ## Plotting plots together
gridExtra::grid.arrange(bar1,bar2,bar3,bar4,ncol=2)

#Check the distribution of numerical data using histogram
hist1 = ggplot(data = day, aes(x =actual_temp)) + ggtitle("Distribution of Temperature") + geom_histogram(bins = 25)
hist2 = ggplot(data = day, aes(x =actual_hum)) + ggtitle("Distribution of Humidity") + geom_histogram(bins = 25)
hist3 = ggplot(data = day, aes(x =actual_feel_temp)) + ggtitle("Distribution of Feel Temperature") + geom_histogram(bins = 25)
hist4 = ggplot(data = day, aes(x =actual_windspeed)) + ggtitle("Distribution of Windspeed") + geom_histogram(bins = 25)
gridExtra::grid.arrange(hist1,hist2,hist3,hist4,ncol=2)

#Check the distribution of numerical data using scatterplot
scat1 = ggplot(data = day, aes(x =actual_temp, y = cnt)) + ggtitle("Distribution of Temperature") + geom_point() + xlab("Temperature") + ylab("Bike COunt")
scat2 = ggplot(data = day, aes(x =actual_hum, y = cnt)) + ggtitle("Distribution of Humidity") + geom_point(color="red") + xlab("Humidity") + ylab("Bike COunt")
scat3 = ggplot(data = day, aes(x =actual_feel_temp, y = cnt)) + ggtitle("Distribution of Feel Temperature") + geom_point() + xlab("Feel Temperature") + ylab("Bike COunt")
scat4 = ggplot(data = day, aes(x =actual_windspeed, y = cnt)) + ggtitle("Distribution of Windspeed") + geom_point(color="red") + xlab("Windspeed") + ylab("Bike COunt")
gridExtra::grid.arrange(scat1,scat2,scat3,scat4,ncol=2)

#Check for outliers in data using boxplot
cnames = colnames(day[,c("actual_temp","actual_feel_temp","actual_windspeed","actual_hum")])
for (i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = cnames[i]), data = day)+
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i])+
           ggtitle(paste("Box plot for",cnames[i])))
}
gridExtra::grid.arrange(gn1,gn3,gn2,gn4,ncol=2)

#Remove outliers in Windspeed
val = day[,19][day[,19] %in% boxplot.stats(day[,19])$out]
day = day[which(!day[,19] %in% val),]

#Check for multicollinearity using VIF
df = day[,c("instant","temp","atemp","hum","windspeed")]
vifcor(df)

#Check for collinearity using corelation graph
corrgram(day, order = F, upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

#Remove the unwanted variables

day <- subset(day, select = -c(holiday,instant,dteday,atemp,casual,registered,actual_temp,actual_feel_temp,actual_windspeed,
                               actual_hum,actual_season,actual_yr,actual_holiday,actual_weathersit))

rmExcept(keepers = "day")
########################################DECISION TREE########################################
#MAPE: 17.47%
#MAE: 684
#RMSE: 864.8
#Accuracy: 82.53%

#Divide the data into train and test
set.seed(123)
train_index = sample(1:nrow(day), 0.8 * nrow(day))
train = day[train_index,]
test = day[-train_index,]

#rpart for regression
dt_model = rpart(cnt ~ ., data = train, method = "anova")

#Predict the test cases
dt_predictions = predict(dt_model, test[,-10])

#Create dataframe for actual and predicted values
df = data.frame("actual"=test[,10], "pred"=dt_predictions)
head(df)

#calculate MAPE
regr.eval(trues = test[,10], preds = dt_predictions, stats = c("mae","mse","rmse","mape"))

#calculate MAPE
MAPE = function(actual, pred){
  print(mean(abs((actual - pred)/actual)) * 100)
}
MAPE(test[,10], dt_predictions)

########################################RANDOM FOREST########################################
#MAPE: 10.68%
#MAE: 392
#RMSE: 535
#Accuracy: 89.32%

#Train the data using random forest
rf_model = randomForest(cnt~., data = train, ntree = 500)

#Predict the test cases
rf_predictions = predict(rf_model, test[,-10])

#Create dataframe for actual and predicted values
df = cbind(df,rf_predictions)
head(df)

#Calculate MAPE
regr.eval(trues = test[,10], preds = rf_predictions, stats = c("mae","mse","rmse","mape"))
MAPE(test[,10], rf_predictions)


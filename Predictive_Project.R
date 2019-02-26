#we are loading the packages dynamically only if the package is not present, if the package is not found then it will get loaded from the internet at runtime.
is_installed <- function(mypkg) is.element(mypkg, installed.packages()[,1])
load_or_install<-function(package_names)
{
  for(package_name in package_names)
  {
    if(!is_installed(package_name))
    {
      install.packages(package_name,repos="http://lib.stat.cmu.edu/R/CRAN")
    }
    library(package_name,character.only=TRUE,quietly=TRUE,verbose=FALSE)
  }
}
load_or_install(c("randomForest","dplyr","forecast","car","caret"))

# loading the sales dataset from cvs file
dataFrame1 <- read.csv("./Sales.csv",header = TRUE)
dataframets<- dataFrame1
summary(dataframets)

# Assigning 1 to Direct, 2 to Outlets, 3 to Retail 
dataframets$CHANNEL <- sub("Direct", "1", dataframets$CHANNEL)
dataframets$CHANNEL <- sub("Outlets", "2", dataframets$CHANNEL)
dataframets$CHANNEL <- sub("Retail", "3", dataframets$CHANNEL)
dataframets$CHANNEL <- as.numeric(as.character(dataframets$CHANNEL)) 

#Cleaning of data to ignore all the null values present in given dataset.
dataframets <- subset(dataFrame1,dataFrame1$CHANNEL != "" &  dataFrame1$FISC_YR != 2018)
groupDf <- dataframets %>% group_by(FISC_YR) %>% summarise(TOTALSALE =sum(DEMAND))
fit <- auto.arima(groupDf[2])
forcastedDf <- forecast(fit,h=2)
plot(forcastedDf, type="p", xlab = "Years", ylab = "Sales", lwd = 2, col = "Blue",main = "tsclean Predicted sales for 2019")

# Dividing the given dataset into training, validation, testing data sets. Dividing the records with not null values in in two datasets training and validation and all the records with null values in the testing dataset.

training <- subset(dataFrame1,dataFrame1$CHANNEL != "" )
testing <- subset(dataFrame1,dataFrame1$CHANNEL == "" )
training_size <- floor(0.50 * nrow(training))
train_ind <- sample(seq_len(nrow(training)), size = training_size)
training_SubSet <- training[train_ind,]
validationdata <- training[-train_ind,]
train <- sample(nrow(training_SubSet), 0.01*nrow(training_SubSet), replace = FALSE)
TrainSet <- training_SubSet[train,]
trainNull <- subset(TrainSet,TrainSet$CHANNEL == "")
TrainSet$CHANNEL <- factor(TrainSet$CHANNEL)

#Testing the model using the validationSet  
validationDataSize <- sample(nrow(training_SubSet), 0.01*nrow(training_SubSet), replace = FALSE)
ValidSet <- training_SubSet[validationDataSize,]
ValidSet$CHANNEL <- factor(ValidSet$CHANNEL)
validModel <- randomForest(ValidSet$CHANNEL ~ ., data = TrainSet, importance = TRUE)
validPredict <- predict(validModel,testing,type="response",norm.votes=TRUE, predict.all=FALSE, proximity=FALSE, nodes=FALSE)
plot(validPredict, lwd = 2, col = "Blue")
importance(validModel)

#Predicting the testingset values using the model
testModel <- randomForest(TrainSet$CHANNEL ~ ., data = TrainSet, importance = TRUE)
importance(testModel)
plot(testModel)
testPredict <- predict(testModel,testing,type="response",norm.votes=TRUE, predict.all=FALSE, proximity=FALSE, nodes=FALSE)
testing$CHANNEL <- testPredict
plot(testing$CHANNEL, lwd = 2, col = "Black")

#Appending the predictive values to testing dataset
merged_data_frame <- rbind(testing,training)
acf(merged_data_frame$DEMAND)
pacf(merged_data_frame$DEMAND)
merged_data_frame$CHANNEL <- factor(merged_data_frame$CHANNEL)

#Calculating the cumulative sales for each channel 
groupChannel <- merged_data_frame %>% group_by(CHANNEL) %>% summarise(TOTALSALE =sum(DEMAND))
plot(groupChannel$CHANNEL, groupChannel$TOTALSALE, xlab = "Channel", ylab = "Sales", lwd = 2, col = "royalblue", main = "Channel wise sales")

#Calculating the cumulative sales for each year
merged_data_frame <- subset(merged_data_frame,merged_data_frame$FISC_YR != 2018)
groupDf <- merged_data_frame %>% group_by(FISC_YR) %>% summarise(TOTALSALE =sum(DEMAND))
fit <- auto.arima(groupDf[2])
forcastedDf <- forecast(fit,h=2)
plot(forcastedDf, type = "p", xlab = "Years", ylab = "Sales", lwd = 2, col = "Blue", pch = 19, main = "Random Forest Predicted sales for 2019")
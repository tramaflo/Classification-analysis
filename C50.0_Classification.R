# Customer brand preferences ----------------------------------------------
# Floriana Trama ----------------------------------------------------------
# Data analysis department ------------------------------------------------
# Y = Brand ---------------------------------------------------------------
# C5.0 Model --------------------------------------------------------------


# Libraries ---------------------------------------------------------------

library(readr)

library(caret)


# Pre-processing data ----

CompleteDataset <- read.csv("C:/Users/T450S/Desktop/Floriana/Ubiqum/Data Analytics II/Task 2/Database/CompleteResponses.csv")
attributes(CompleteDataset)

summary(CompleteDataset)

str(CompleteDataset)

CompleteDataset$brand<-as.factor(CompleteDataset$brand)

plot(CompleteDataset$brand)

CompleteDataset$elevel<-as.factor(CompleteDataset$elevel)

CompleteDataset$car<-as.factor(CompleteDataset$car)

CompleteDataset$zipcode<-as.factor(CompleteDataset$zipcode)

CompleteDataSubset <- CompleteDataset[c(1,2,7)]



# Set seed ----------------------------------------------------------------

set.seed(123)


# Create 75%/25% training and test sets -----------------------------------

inTraining <- createDataPartition(CompleteDataSubset$brand, p = .75, list = FALSE)

training <- CompleteDataSubset[inTraining,]

testing <- CompleteDataSubset[-inTraining,]


# 10 fold cross validation ------------------------------------------------

fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)


C5model_Brand <- train(brand~.,
                       data = CompleteDataSubset,
                       method = "C5.0",
                       trainControl = fitControl,
                       metric = "Accuracy",
                       tuneLength = 2)


# Training results --------------------------------------------------------

C5model_Brand

varImp(C5model_Brand)

prediction <- predict(C5model_Brand, testing)

confusionMatrix(prediction, testing$brand)





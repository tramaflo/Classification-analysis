# Customer brand preferences ----------------------------------------------
# Floriana Trama ----------------------------------------------------------
# Data analysis department ------------------------------------------------
# Y = Brand ---------------------------------------------------------------
# Random forest - Manual grid ---------------------------------------------



# Libraries ---------------------------------------------------------------

library(readr)

library(caret)


# Data exploration --------------------------------------------------------

CompleteDataset <- read.csv("C:/Users/T450S/Desktop/Floriana/Ubiqum/Data Analytics II/Task 2/Database/CompleteResponses.csv")

summary(CompleteDataset)

str(CompleteDataset)

hist(CompleteDataset$salary)

hist(CompleteDataset$age)

hist(CompleteDataset$credit)


# Pre-processing data -----------------------------------------------------

CompleteDataset$brand<-as.factor(CompleteDataset$brand)

plot(CompleteDataset$brand)

CompleteDataset$elevel<-as.factor(CompleteDataset$elevel)

CompleteDataset$car<-as.factor(CompleteDataset$car)

CompleteDataset$zipcode<-as.factor(CompleteDataset$zipcode)

plot(CompleteDataset$brand,CompleteDataset$salary)

plot(CompleteDataset$brand,CompleteDataset$age)

ggplot(CompleteDataset, aes(age, salary, color = as.factor(brand)))+ geom_jitter(alpha = 0.5)


# Features selection ------------------------------------------------------

CompleteDataSubset <- CompleteDataset[c(1,2,7)]


# Set seed ----------------------------------------------------------------

set.seed(123)


# Create 75%/25% training and test sets -----------------------------------

inTraining <- createDataPartition(CompleteDataSubset$brand, p = .75, list = FALSE)

training <- CompleteDataSubset[inTraining,]

testing <- CompleteDataSubset[-inTraining,]


# 10 fold cross validation ------------------------------------------------

fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)


# Dataframe for manual tuning of mtry -------------------------------------

rfGrid <- expand.grid(mtry=c(1,2,3))


# Train Random Forest Regression model ------------------------------------

system.time(rfFitm1 <- train(brand~., data = training, method = "rf", 
                             trControl=fitControl, tuneGrid=rfGrid), preProcess= c("scale"))

# Ttraining results -------------------------------------------------------

rfFitm1

varImp(rfFitm1)


# Predictions -------------------------------------------------------------

prediction <- predict(rfFitm1, testing)

confusionMatrix(prediction, testing$brand)

postResample(prediction, testing$brand)

Specialtable <- cbind(testing, prediction )



# Calculate error ---------------------------------------------------------

Specialtable$brand2 <- as.numeric(Specialtable$brand)

Specialtable$prediction2 <- as.numeric(Specialtable$prediction)

error <- abs(Specialtable$brand2 - Specialtable$prediction2)

Errorplot <- cbind(Specialtable, error)


# GGplot for errors -------------------------------------------------------

ggplot(Errorplot, aes(age, salary, color = error))+
  
  geom_jitter(alpha = 0.5)+
  
  scale_color_gradient(low="white", high="red")


# Incomplete survey dataset -----------------------------------------------

IncompleteDataset <- read_csv("Floriana/Ubiqum/Data Analytics II/Task 2/Database/SurveyIncomplete.csv")

IncompleteDataSubset <- IncompleteDataset[c(1,2,7)]


# Prediction on Incomplete dataset ----------------------------------------

prediction <- predict(rfFitm1, IncompleteDataSubset)

prediction

Specialtable <- cbind(IncompleteDataSubset, prediction )

summary(prediction)


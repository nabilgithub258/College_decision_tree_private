library(corrgram)
library(corrplot)
library(caTools)
library(Amelia)
library(ggplot2)
library(dplyr)
library(rpart)
library(rpart.plot)
library(randomForest)

#### College data is inside ISLR

str(College)

View(College)

#### changing the name for the simplicity

df <- College

#### EDA time

ggplot(df,aes(Room.Board, Grad.Rate)) + geom_point(position=position_jitter(w=1, h=0),aes(color=Private),alpha=0.5,size=2)

ggplot(df,aes(Grad.Rate)) + geom_histogram(aes(fill=Private),color='black') + theme_bw()

ggplot(df,aes(F.Undergrad)) + geom_histogram((aes(fill=Private)),color='red',alpha=0.5)

#### in the ggplot we see there something off because the graduation rate is going above 100
#### lets find out which one is it

subset(df,Grad.Rate > 100)

#### Getting rid of 118 graduation rate and making it to 100

df['Cazenovia College','Grad.Rate'] <- 100

#### Run subset again just to make sure

subset(df,Grad.Rate > 100)

ggplot(df,aes(Grad.Rate)) + geom_histogram(aes(fill=Private),color='black',alpha=0.7) + theme_bw()

#### I think our data is all clean to undergo model transformation
#### train and test data

sample <- sample.split(df$Private,SplitRatio = 0.7)

train <- subset(df,sample == TRUE)

test <- subset(df,sample == FALSE)

#### Making the model now for decision tree

tree.model <- rpart(Private ~ .,method='class',train)

prp(tree.model)

#### Prediction
tree.predict <- predict(tree.model,test)

print(tree.predict)

#### Lets make a column next to Yes which will say YES if its above 0.5

tree.predict <- as.data.frame(tree.predict)

View(tree.predict)

predict.column <- function(x){
  if (x >= 0.5){
    return ('YES')
  }else {
    return('NO')
  }
}

tree.predict$Predict <- sapply(tree.predict$Yes,predict.column)

str(tree.predict)

print(head(tree.predict))

#### Now make the confusion matrix with test and model in mind

table(tree.predict$Predict,test$Private)

#### calculate the accuracy of our model
#### accuracy = true positives + true negatives / total predictions

accuracy <- (162+52) / (162+52+7+12)

print(accuracy)








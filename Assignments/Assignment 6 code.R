# Libraries
library(e1071)
library(stringr)
library(mlr)
library(caret)
library(mclust)
library(tm)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(Cairo)
library(philentropy)
library(forcats)
library(lsa)
library(igraph)
library(ggplot2)
library(corrplot)
library(pastecs)
library(dplyr)
library(ggpubr)
library(gridExtra)

# load data
digit_training <- read.csv("data/Kaggle-digit-train-sample-small-1400.csv")
digit_test  <- read.csv("data/Kaggle-digit-test-sample1000.csv")
training_df <- digit_training
test_df <- digit_test

#categorize label column
training_df$label <- case_when(training_df$label==0~"Zero", training_df$label==1~"One", training_df$label==2~"Two",
                               training_df$label==3~"Three", training_df$label==4~"Four", training_df$label==5~"Five", 
                               training_df$label==6~"Six", training_df$label==7~"Seven", training_df$label==8~"Eight", 
                               training_df$label==9~"Nine")
training_df$label <- as.factor(training_df$label)
plot(x= training_df$label)

#since we're going to do a bunch of sampling, establish sample size
sampleSize <- nrow(training_df)*0.75

# First NB attempt
samples1 <- sample(1:nrow(training_df),sampleSize, replace=FALSE)
NBtrain1 <- training_df[samples1,]
NBtest1 <- training_df[-samples1,]
NB1 <- naiveBayes(label ~ ., data=NBtrain1)
NB1_predict <- predict(NB1, NBtest1)
table(NB1_predict, NBtest1$label)

# Plot prediction and actual to see how close prediction came
plot(NB1_predict)
plot(x=NBtest1$label)

#More NB model attempts
samples2 <-sample(1:nrow(training_df),sampleSize, replace=FALSE)
NBtrain2 <- training_df[samples2,]
NBtest2 <- training_df[-samples2,]
NB2 <- naiveBayes(label ~ ., data=NBtrain2)
NB2_predict <- predict(NB2, NBtest2)
table(NB2_predict, NBtest2$label)
plot(NB2_predict)

samples3 <-sample(1:nrow(training_df),sampleSize, replace=FALSE)
NBtrain3 <- training_df[samples3,]
NBtest3 <- training_df[-samples3,]
NB3 <- naiveBayes(label ~ ., data=NBtrain3)
NB3_predict <- predict(NB3, NBtest3)
table(NB3_predict, NBtest3$label)
plot(NB3_predict)

##function to automate this - unsuccesful
#NBTry <- function(trainset, identifier, trainratio)
#{
  #samplesize <- nrow(trainset)*trainratio
  #samples <- sample(1:nrow(trainset), samplesize, replace=FALSE)
  #NBTrain <- trainset[samples,]
  #NBTest <- trainset[-samples,]
  #NB <- naiveBayes(trainset[,identifier] ~ ., data=NBTrain)
  #NB_predict <- predict(NB, NBTest)
 # table(NB_predict, NBTest[,identifier])
#}

samples4 <-sample(1:nrow(training_df),sampleSize, replace=FALSE)
NBtrain4 <- training_df[samples4,]
NBtest4 <- training_df[-samples4,]
NB4 <- naiveBayes(label ~ ., data=NBtrain4)
NB4_predict <- predict(NB4, NBtest4)
table(NB4_predict, NBtest4$label)
plot(NB4_predict)

samples5 <-sample(1:nrow(training_df),sampleSize, replace=FALSE)
NBtrain5 <- training_df[samples5,]
NBtest5 <- training_df[-samples5,]
NB5 <- naiveBayes(label ~ ., data=NBtrain5)
NB5_predict <- predict(NB5, NBtest5)
table(NB5_predict, NBtest5$label)
plot(NB5_predict)


#Prep training and test sets for decision trees
DTtraining_df <- training_df
DTtraining_df[,-1] <- DTtraining_df[,-1]/255
for (i in 2:ncol(DTtraining_df))
{
  breaks <- c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1)
  labels <- c("10%","20%", "30%", "40%", "50%", "60%","70%","80%","90%","100%")
  DTtraining_df[[i]] <-fct_explicit_na(cut(DTtraining_df[[i]], breaks= breaks, labels = labels),"Not present")
}

DTtest_df<- digit_test
DTtest_df[,-1] <- DTtest_df[,-1]/255
for (i in 2:ncol(DTtest_df))
{
  breaks <- c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1)
  labels <- c("10%","20%", "30%", "40%", "50%", "60%","70%","80%","90%","100%")
  DTtest_df[[i]] <-fct_explicit_na(cut(DTtest_df[[i]], breaks= breaks, labels = labels),"Not present")
}

#DT number 1
DT_train1 <- DTtraining_df[samples1,]
DT_test1 <- DTtraining_df[-samples1,-1]
test1_labels <- DTtraining_df[-samples1,1]
DT1 <- rpart(DT_train1$label ~ ., data=DT_train1, method="class")
fancyRpartPlot(DT1)
DT_predict1 <- predict(DT1, DT_test1, type="class")
DTresults1 <- data.frame(Predicted = DT_predict1, Actual = test1_labels)
table(DTresults1)
plot(DT_predict1)

# more decision trees
DT_train2 <- DTtraining_df[samples2,]
DT_test2 <- DTtraining_df[-samples2,-1]
test2_labels <- DTtraining_df[-samples2,1]
DT2 <- rpart(DT_train2$label ~ ., data=DT_train2, method="class")
fancyRpartPlot(DT2)
DT_predict2 <- predict(DT2, DT_test2, type="class")
DTresults2 <- data.frame(Predicted = DT_predict2, Actual = test2_labels)
table(DTresults2)

DT_train3 <- DTtraining_df[samples3,]
DT_test3 <- DTtraining_df[-samples3,-1]
test3_labels <- DTtraining_df[-samples3,1]
DT3 <- rpart(DT_train3$label ~ ., data=DT_train3, method="class")
fancyRpartPlot(DT3)
DT_predict3 <- predict(DT3, DT_test3, type="class")
DTresults3 <- data.frame(Predicted = DT_predict3, Actual = test3_labels)
table(DTresults3)

DT_train4 <- DTtraining_df[samples4,]
DT_test4 <- DTtraining_df[-samples4,-1]
test4_labels <- DTtraining_df[-samples4,1]
DT4 <- rpart(DT_train4$label ~ ., data=DT_train4, method="class")
fancyRpartPlot(DT4)
DT_predict4 <- predict(DT4, DT_test4, type="class")
DTresults4 <- data.frame(Predicted = DT_predict4, Actual = test4_labels)
table(DTresults4)

DT_train5 <- DTtraining_df[samples5,]
DT_test5 <- DTtraining_df[-samples5,-1]
test5_labels <- DTtraining_df[-samples5,1]
DT5 <- rpart(DT_train5$label ~ ., data=DT_train5, method="class")
fancyRpartPlot(DT5)
DT_predict5 <- predict(DT5, DT_test5, type="class")
DTresults5 <- data.frame(Predicted = DT_predict5, Actual = test5_labels)
table(DTresults5)

#Model 2 in both cases seemed to be the "best", so use those models on unlablled set
NBPredict <- predict(NB2, digit_test)
digit_test$NBPrediction <- NBPredict

DTPredict <- predict(DT2, DTtest_df, type="class")
digit_test$DTPrediction <- DTPredict

#see how much the two models matched
table(NBPredict, DTPredict)

#plot both models
plot(NBPredict)
plot(DTPredict)
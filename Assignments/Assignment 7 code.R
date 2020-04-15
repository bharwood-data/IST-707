library(bio3d)
library(class)
library(mlr)
library(ggplot2)
library(plyr)
library(dplyr)
library(lattice)
library(caret)
library(e1071)
#library(ElemStatLearn)
#library(gmodels)
library(GGally)
library(randomForest)
library(stringr)
library(naivebayes)
library(mclust)
library(cluster)
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
library(corrplot)
library(pastecs)
library(ggpubr)
library(psych)
library(tidyr)


digit_training <- read.csv("data/Kaggle-digit-train-sample-small-1400.csv")
digit_test  <- read.csv("data/Kaggle-digit-test-sample1000.csv")
training_df <- digit_training
test_df <- digit_test

training_df$label <- as.factor(training_df$label)

set.seed(121804)

digit_training_N <- as.data.frame(lapply(Newdigit_training[,c(2:785)], normalize.vector))
training_Labels <- training_df$label
digit_training_N[is.nan(as.matrix(digit_training_N))] <- 0


ReadyTrainingDF <- data.frame(Number = training_Labels, digit_training_N[,2:785])

sum(is.na(test_df))
test_df[is.na(test_df)] <-0
digit_test_N <- as.data.frame(lapply(test_df[,c(2:785)], normalize.vector))
digit_test_N[is.na(digit_test_N)] <- 0


ReadyTestingDF <- data.frame(Number = as.factor(test_df[,1]), digit_test_N[,2:785])

n <- round(nrow(ReadyTrainingDF)/5)
s <- sample(1:nrow(ReadyTrainingDF),n)

Train <- ReadyTrainingDF[-s,]
Test <- ReadyTrainingDF[s,]
Train[,1] <- as.factor(Train[,1])
Test[,1] <- as.factor(Test[,1])

Test_no_labels <- Test[,-1]
Test_labels <- Test[,1]
Train_no_labels <- Train[,-1]
Train_labels <- Train[,1]

knn_fit <- train(Number ~., data=Train, method="knn")
knn_test <- predict(knn_fit, Test_no_labels)
KTable <- table(knn_test, Test_labels)
(MR_K <- 1 - sum(diag(KTable))/sum(KTable))
KTable

knn <- predict(knn_fit, digit_test[,-1])
table(knn)

plot(knn_test)
plot(x=Test_labels)
plot(knn)

# Random Forest
digit_fit_RF <- randomForest(Number~., data=)
print(digit_fit_RF)
test_pred_RF <- predict(digit_fit_RF, Test_no_labels)
(RFTable <- table(test_pred_RF, Test_labels))
hist(treesize(digit_fit_RF))
varImpPlot(digit_fit_RF)
(MR_RF <- 1-sum(diag(RFTable))/sum(RFTable))

plot(test_pred_RF)
plot(x=Test_labels)

1-sum(diag(table(test_pred_RF, knn_test)))/sum(table(test_pred_RF, knn_test))

RF_digit_pred <- predict(digit_fit_RF, ReadyTestingDF[,-1])
table(RF_digit_pred)
plot(RF_digit_pred)

### SVM

SVM_fit_P <- svm(Number ~ ., data=Train, kernel="polynomial", cost=0.1, scale=FALSE)
SVM_test_P<- predict(SVM_fit_P, Test_no_labels, type="class")
(PTable <- table(SVM_test_P, Test_labels))
(MR_P<- 1-sum(diag(PTable))/sum(PTable))
plot(SVM_test_P)

SVM_P_Pred <- predict(SVM_fit_P, ReadyTestingDF[,-1])
table(SVM_P_Pred)
plot(SVM_P_Pred)

SVM_fit_L <- svm(Number ~ ., data=Train, kernel="linear",cost=0.1, scale=FALSE)
SVM_test_L <- predict(SVM_fit_L, Test_no_labels, type="class")
(LTable <- table(SVM_test_L, Test_labels))
(MR_L<- 1-sum(diag(LTable))/sum(LTable))
plot(SVM_test_L)

SVM_L_Pred <- predict(SVM_fit_L, ReadyTestingDF[,-1])
table(SVM_L_Pred)
plot(SVM_L_Pred)

SVM_fit_R <- svm(Number ~ ., data=Train, kernel="radial", cost=0.1, scale=FALSE)
SVM_test_R <- predict(SVM_fit_R, Test_no_labels, type="class")
(RTable <- table(SVM_test_R, Test_labels))

SVM_fit_S <- svm(Number ~. , data=Train, kernel="sigmoid", cost=0.1, scale=FALSE)
SVM_test_S <- predict(SVM_fit_S, Test_no_labels, type="class")
(STable <- table(SVM_test_S, Test_labels)) 

NB1 <- naiveBayes(Number ~ ., data=Train)
NB1_predict <- predict(NB1, Test)
NBTable <- table(NB1_predict, Test_labels)
(MR_NB <- 1-sum(diag(NBTable))/sum(NBTable))
plot(NB1_predict)
NB_predict <- predict(NB1, ReadyTestingDF)
table(NB_predict)
plot(NB_predict)

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
DT_train1 <- DTtraining_df[-s,]
DT_test1 <- DTtraining_df[s,-1]
test1_labels <- DTtraining_df[s,1]
DT1 <- rpart(DT_train1$label ~ ., data=DT_train1, method="class")
fancyRpartPlot(DT1)
DT_predict1 <- predict(DT1, DT_test1, type="class")
DTresults1 <- data.frame(Predicted = DT_predict1, Actual = test1_labels)
DTTable <- table(DTresults1)
(MR_DT <- 1-sum(diag(DTTable))/sum(DTTable))
plot(DT_predict1)

DT_Pred <- predict(DT1, DTtest_df, type="class")
table(DT_Pred)
plot(DT_Pred)

ids <- data.frame(knn, RF_digit_pred[-1000], SVM_P_Pred[-1000], NB_predict[-1000], DT_Pred[-1000])

nrow(ids[which(ids$knn==ids$RF_digit_pred..1000. & ids$knn==ids$SVM_P_Pred..1000. & ids$knn==ids$NB_predict..1000.& ids$knn==ids$DT_Pred..1000.),])
nrow(ids[which(ids$knn==ids$RF_digit_pred..1000. & ids$knn==ids$SVM_P_Pred..1000.),])

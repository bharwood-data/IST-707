# libraries
library(network)
library(wordcloud)
library(tm)
library(slam)
library(quanteda)
library(SnowballC)
library(arules)
library(proxy)
library(stringr)
library(textmineR)
library(igraph)
library(lsa)
library(tidyr)
library(gofastr)
library(mclust)
library(cluster)
library(stringi)
library(proxy)
library(Matrix)
library(tidytext)
library(plyr) 
library(ggplot2)
library(factoextra)
library(wordcloud)
library(e1071)
library(ROCR)

# prep
set.seed(121804)
setwd("E:/Documents/IST 707")

#load data
data <- read.csv("data/deception_data_converted_final1.csv")
data <- data[-(83:84),]

lie <- data$lie
sent <- data$sentiment

data <- data[,-(1:2)]

data <- unite(data, "review")

head(data, n=5)

lie_df <- data.frame(lie, data)
dfCorpus <- Corpus(VectorSource(t(data)))
tm::inspect(dfCorpus)

getTransformations()
nreviews <- length(t(data))

(minTermFreq <- nreviews * 0.0001)

(maxTermFreq <- nreviews /2)
(STOPS <-stopwords('english'))

reviews_dtm <- DocumentTermMatrix(dfCorpus,
                                 control = list(
                                   stopwords = TRUE, 
                                   #wordLengths=c(4, 10),
                                   removePunctuation = TRUE,
                                   removeNumbers = TRUE,
                                   tolower=TRUE,
                                   #stemming = F,
                                   #stemWords=TRUE,
                                   remove_separators = TRUE,
                                   #stem=TRUE,
                                   stopwords("english"),
                                   bounds = list(global = c(minTermFreq, maxTermFreq))
                                 ))
tm::inspect(reviews_dtm)
WordFreq <- colSums(as.matrix(reviews_dtm))
head(WordFreq)
length(WordFreq)
Row_Sum_Per_Review <- rowSums(as.matrix(reviews_dtm))

reviews_M <- as.matrix(reviews_dtm)
reviews_m1 <- apply(reviews_M, 1, function(i) i/sum(i))
reviews_norm <- t(reviews_m1)
reviews_df <- as.data.frame(reviews_M)

wordcloud(colnames(reviews_norm), reviews_norm, max.words = 1000)

m <- reviews_M
m1 <- reviews_norm

M <- data.frame(lie, m1)

#function to quickly produce precision, recall, and f1 for a prediction table
model_metrics<- function(pred_table, labels)
{
  precision <-c()
  recall <-c()
  f1 <- c()
  cats <- length(labels)
  for (i in 1:cats)
  {
    precision[[i]] <- pred_table[i,i]/sum(pred_table[,i])
    precision
    recall[[i]] <- pred_table[i,i]/sum(pred_table[i,])
    recall
    f1[[i]] <- 2*precision[[i]]*recall[[i]]/(precision[[i]]+recall[[i]])
    f1
  }
  df <- rbind(precision, recall, f1)
  df <- t(df)
  rownames(df) <- labels
  return(df)
}

sampleSize <- nrow(M)*0.8

#Naive Bayes

# MOdel 1
samples1 <- sample(1:nrow(M),sampleSize, replace=FALSE)
LNBtrain1 <- M[samples1,]
LNBtest1 <- M[-samples1,]
LNB1 <- naiveBayes(lie ~ ., data=LNBtrain1)
LNB1_predict <- predict(LNB1, LNBtest1)
table(LNB1_predict, LNBtest1$lie)
Lpred1 <- prediction(as.numeric(LNB1_predict), as.numeric(LNBtest1$lie))
Lperf1 <- performance(Lpred1, measure="tpr", x.measure="fpr")
LNBplot1 <- (plot(Lperf1, main= "ROC curve for Naive Bayes", col="blue", lwd=3)+ abline(coef=c(0,1)))
LNB1_pred_table <- table(LNB1_predict, LNBtest1$lie)
print(knitr::kable(LNB1_pred_table))
LNB1_acc <- (sum(diag(LNB1_pred_table)/sum(LNB1_pred_table)))
print(paste0("Naive Bayes Accuracy: ", LNB1_acc))
model_metrics(LNB1_pred_table,c("Lie","Truth"))

Ltune1 <- tune.svm(x=LNBtrain1[,-1], y=LNBtrain1[,1], type="C-classification", kernel = "polynomial", degree=2, cost=10^(1:3), gamma=c(0.1,1,10), coef0=c(0.1,1,10))
(Lgamma1 <- Ltune1$best.parameters$gamma)
(Lcoef01 <- Ltune1$best.parameters$coef0)
(Lcost1 <- Ltune1$best.parameters$cost)
LSVM1 <- svm(lie ~ ., data=LNBtrain1, kernel="polynomial", degree=2, gamma=Lgamma1, coef0=Lcoef01, cost=Lcost1, scale=FALSE)
LSVM_pred1<- predict(LSVM1, LNBtest1[,-1], type="class")
LSVMpred1 <- prediction(as.numeric(LSVM_pred1), as.numeric(LNBtest1$lie))
LSVMperf1 <- performance(LSVMpred1, measure="tpr", x.measure="fpr")
plot(LSVMperf1, main ="ROC curve for SVM", col="purple", lwd=3)+abline(coef=c(0,1))
(LSVM1_pred_table <- table(LSVM_pred1, LNBtest1$lie))
LSVM1_acc <- (sum(diag(LSVM1_pred_table)/sum(LSVM1_pred_table)))
print(paste0("SVM accuracy: ", LSVM1_acc))
model_metrics(LSVM1_pred_table, c("Lie","Truth"))

# Model 2
samples2 <- sample(1:nrow(M),sampleSize, replace=FALSE)
LNBtrain2 <- M[samples2,]
LNBtest2 <- M[-samples2,]
LNB2 <- naiveBayes(lie ~ ., data=LNBtrain2)
LNB2_predict <- predict(LNB2, LNBtest2)
Lpred2 <- prediction(as.numeric(LNB2_predict), as.numeric(LNBtest2$lie))
Lperf2 <- performance(Lpred2, measure="tpr", x.measure="fpr")
LNBplot2 <- (plot(Lperf2, main= "ROC curve for Naive Bayes", col="blue", lwd=3)+ abline(coef=c(0,1)))
LNB2_pred_table <- table(LNB2_predict, LNBtest2$lie)
print(knitr::kable(LNB2_pred_table))
LNB2_acc <- (sum(diag(LNB2_pred_table)/sum(LNB2_pred_table)))
print(paste0("Naive Bayes Accuracy: ", LNB2_acc))
model_metrics(LNB2_pred_table,c("Lie","Truth"))

Ltune2 <- tune.svm(x=LNBtrain2[,-1], y=LNBtrain2[,1], type="C-classification", kernel = "polynomial", degree=2, cost=10^(1:3), gamma=c(0.1,1,10), coef0=c(0.1,1,10))
Lgamma2 <- Ltune2$best.parameters$gamma
Lcoef02 <- Ltune2$best.parameters$coef0
Lcost2 <- Ltune2$best.parameters$cost
LSVM2 <- svm(lie ~ ., data=LNBtrain2, kernel="polynomial", degree=2, gamma=Lgamma2, coef0=Lcoef02, cost=Lcost2, scale=FALSE)
LSVM_pred2<- predict(LSVM2, LNBtest2[,-1], type="class")
LSVMpred2 <- prediction(as.numeric(LSVM_pred2), as.numeric(LNBtest2$lie))
LSVMperf2 <- performance(LSVMpred2, measure="tpr", x.measure="fpr")
plot(LSVMperf2, main ="ROC curve for SVM", col="purple", lwd=3)+abline(coef=c(0,1))
(LSVM2_pred_table <- table(LSVM_pred2, LNBtest2$lie))
LSVM2_acc <- (sum(diag(LSVM2_pred_table)/sum(LSVM2_pred_table)))
print(paste0("SVM accuracy: ", LSVM2_acc))
model_metrics(LSVM2_pred_table, c("Lie","Truth"))

# Model 3
samples3 <- sample(1:nrow(M),sampleSize, replace=FALSE)
LNBtrain3 <- M[samples3,]
LNBtest3 <- M[-samples3,]
LNB3 <- naiveBayes(lie ~ ., data=LNBtrain3)
LNB3_predict <- predict(LNB3, LNBtest3)
Lpred3 <- prediction(as.numeric(LNB3_predict), as.numeric(LNBtest3$lie))
Lperf3 <- performance(Lpred3, measure="tpr", x.measure="fpr")
plot(Lperf3, main= "ROC curve for Naive Bayes", col="blue", lwd=3)+ abline(coef=c(0,1))
LNB3_pred_table <- table(LNB3_predict, LNBtest3$lie)
print(knitr::kable(LNB3_pred_table))
LNB3_acc <- (sum(diag(LNB3_pred_table)/sum(LNB3_pred_table)))
print(paste0("Naive Bayes Accuracy: ", LNB3_acc))
model_metrics(LNB3_pred_table,c("Lie","Truth"))

Ltune3 <- tune.svm(x=LNBtrain3[,-1], y=LNBtrain3[,1], type="C-classification", kernel = "polynomial", degree=2, cost=10^(1:3), gamma=c(0.1,1,10), coef0=c(0.1,1,10))
Lgamma3 <- Ltune3$best.parameters$gamma
Lcoef03 <- Ltune3$best.parameters$coef0
Lcost3 <- Ltune3$best.parameters$cost
LSVM3 <- svm(lie ~ ., data=LNBtrain3, kernel="polynomial", degree=2, gamma=Lgamma3, coef0=Lcoef03, cost=Lcost3, scale=FALSE)
LSVM_pred3<- predict(LSVM3, LNBtest3[,-1], type="class")
LSVMpred3 <- prediction(as.numeric(LSVM_pred3), as.numeric(LNBtest3$lie))
LSVMperf3 <- performance(LSVMpred3, measure="tpr", x.measure="fpr")
plot(LSVMperf3, main ="ROC curve for SVM", col="purple", lwd=3)+abline(coef=c(0,1))
(LSVM3_pred_table <- table(LSVM_pred3, LNBtest3$lie))
LSVM3_acc <- (sum(diag(LSVM3_pred_table)/sum(LSVM3_pred_table)))
print(paste0("SVM accuracy: ", LSVM3_acc))
model_metrics(LSVM3_pred_table, c("Lie","Truth"))

## Sentiment, using same sample sets
M1 <- data.frame(sent, m1)

#model 1
SNBtrain1 <- M1[samples1,]
SNBtest1 <- M1[-samples1,]
SNB1 <- naiveBayes(sent ~ ., data=SNBtrain1)
SNB1_predict <- predict(SNB1, SNBtest1)
Spred1 <- prediction(as.numeric(SNB1_predict), as.numeric(SNBtest1$sent))
Sperf1 <- performance(Spred1, measure="tpr", x.measure="fpr")
plot(Sperf1, main= "ROC curve for Naive Bayes", col="blue", lwd=3)+ abline(coef=c(0,1))
SNB1_pred_table <- table(SNB1_predict, SNBtest1$sent)
print(knitr::kable(SNB1_pred_table))
SNB1_acc <- (sum(diag(SNB1_pred_table)/sum(SNB1_pred_table)))
print(paste0("Naive Bayes Accuracy: ", SNB1_acc))
model_metrics(SNB1_pred_table,c("Negative","Positive"))

Stune1 <- tune.svm(x=SNBtrain1[,-1], y=SNBtrain1[,1], type="C-classification", kernel = "polynomial", degree=2, cost=10^(1:3), gamma=c(0.1,1,10), coef0=c(0.1,1,10))
Sgamma1 <- Stune1$best.parameters$gamma
Scoef01 <- Stune1$best.parameters$coef0
Scost1 <- Stune1$best.parameters$cost
SSVM1 <- svm(sent ~ ., data=SNBtrain1, kernel="polynomial", degree=2, gamma=Sgamma1, coef0=Scoef01, cost=Scost1, scale=FALSE)
SSVM_pred1<- predict(SSVM1, SNBtest1[,-1], type="class")
SSVMpred1 <- prediction(as.numeric(SSVM_pred1), as.numeric(SNBtest1$sent))
SSVMperf1 <- performance(SSVMpred1, measure="tpr", x.measure="fpr")
plot(SSVMperf1, main ="ROC curve for SVM", col="purple", lwd=3)+abline(coef=c(0,1))
(SSVM1_pred_table <- table(SSVM_pred1, SNBtest1$sent))
SSVM1_acc <- (sum(diag(SSVM1_pred_table)/sum(SSVM1_pred_table)))
print(paste0("SVM accuracy: ", SSVM1_acc))
model_metrics(SSVM1_pred_table, c("Negative","Positive"))

#Model 2
SNBtrain2 <- M1[samples2,]
SNBtest2 <- M1[-samples2,]
SNB2 <- naiveBayes(sent ~ ., data=SNBtrain2)
SNB2_predict <- predict(SNB2, SNBtest2)
Spred2 <- prediction(as.numeric(SNB2_predict), as.numeric(SNBtest2$sent))
Sperf2 <- performance(Spred2, measure="tpr", x.measure="fpr")
plot(Sperf2, main= "ROC curve for Naive Bayes", col="blue", lwd=3)+ abline(coef=c(0,1))
SNB2_pred_table <- table(SNB2_predict, SNBtest2$sent)
print(knitr::kable(SNB2_pred_table))
SNB2_acc <- (sum(diag(SNB2_pred_table)/sum(SNB2_pred_table)))
print(paste0("Naive Bayes Accuracy: ", SNB2_acc))
model_metrics(SNB2_pred_table,c("Negative","Positive"))

Stune2 <- tune.svm(x=SNBtrain2[,-1], y=SNBtrain2[,1], type="C-classification", kernel = "polynomial", degree=2, cost=10^(1:3), gamma=c(0.1,1,10), coef0=c(0.1,1,10))
Sgamma2 <- Stune2$best.parameters$gamma
Scoef02 <- Stune2$best.parameters$coef0
Scost2 <- Stune2$best.parameters$cost
SSVM2 <- svm(sent ~ ., data=SNBtrain2, kernel="polynomial", degree=2, gamma=Sgamma2, coef0=Scoef02, cost=Scost2, scale=FALSE)
SSVM_pred2<- predict(SSVM2, SNBtest2[,-1], type="class")
SSVMpred2 <- prediction(as.numeric(SSVM_pred2), as.numeric(SNBtest2$sent))
SSVMperf2 <- performance(SSVMpred2, measure="tpr", x.measure="fpr")
plot(SSVMperf2, main ="ROC curve for SVM", col="purple", lwd=3)+abline(coef=c(0,1))
(SSVM2_pred_table <- table(SSVM_pred2, SNBtest2$sent))
SSVM2_acc <- (sum(diag(SSVM2_pred_table)/sum(SSVM2_pred_table)))
print(paste0("SVM accuracy: ", SSVM2_acc))
model_metrics(SSVM2_pred_table, c("Negative","Positive"))

#Model 3
SNBtrain3 <- M1[samples3,]
SNBtest3 <- M1[-samples3,]
SNB3 <- naiveBayes(sent ~ ., data=SNBtrain3)
SNB3_predict <- predict(SNB3, SNBtest3)
Spred3 <- prediction(as.numeric(SNB3_predict), as.numeric(SNBtest3$sent))
Sperf3 <- performance(Spred3, measure="tpr", x.measure="fpr")
plot(Sperf3, main= "ROC curve for Naive Bayes", col="blue", lwd=3) + abline(coef=c(0,1))
SNB3_pred_table <- table(SNB3_predict, SNBtest3$sent)
print(knitr::kable(SNB3_pred_table))
SNB3_acc <- (sum(diag(SNB3_pred_table)/sum(SNB3_pred_table)))
print(paste0("Naive Bayes Accuracy: ", SNB3_acc))
model_metrics(SNB3_pred_table,c("Negative","Positive"))

Stune3 <- tune.svm(x=SNBtrain3[,-1], y=SNBtrain3[,1], type="C-classification", kernel = "polynomial", degree=2, cost=10^(1:3), gamma=c(0.1,1,10), coef0=c(0.1,1,10))
Sgamma3 <- Stune3$best.parameters$gamma
Scoef03 <- Stune3$best.parameters$coef0
Scost3 <- Stune3$best.parameters$cost
SSVM3 <- svm(sent ~ ., data=SNBtrain3, kernel="polynomial", degree=2, gamma=Sgamma3, coef0=Scoef03, cost=Scost3, scale=FALSE)
SSVM_pred3<- predict(SSVM3, SNBtest3[,-1], type="class")
SSVMpred3 <- prediction(as.numeric(SSVM_pred3), as.numeric(SNBtest3$sent))
SSVMperf3 <- performance(SSVMpred3, measure="tpr", x.measure="fpr")
plot(SSVMperf3, main ="ROC curve for SVM", col="purple", lwd=3)+abline(coef=c(0,1))
(SSVM3_pred_table <- table(SSVM_pred3, SNBtest3$sent))
SSVM3_acc <- (sum(diag(SSVM3_pred_table)/sum(SSVM3_pred_table)))
print(paste0("SVM accuracy: ", SSVM3_acc))
model_metrics(SSVM3_pred_table, c("Negative","Positive"))



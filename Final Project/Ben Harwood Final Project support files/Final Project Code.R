library(bio3d)
library(ggplot2)
library(cluster)
library(arules)
library(arulesViz)
library(caret)
library(dplyr)
library(factoextra)
library(proxy)
library(tidyr)
library(reshape2)
library(NbClust)
library(FunCluster)
library(caret)
library(naivebayes)
library(e1071)
library(class)
library(randomForest)
library(rpart)
library(rattle)
library(ROCR)

setwd("E:/Documents/IST 707")
set.seed(121804)

#load data
census <- read.csv("data/acs2015_census_tract_data.csv")
UWay <- read.csv("data/United_Way_Data.csv")
Ohio_UW <- read.csv("data/ohio.csv")

#isolate complete case
census1 <- census[complete.cases(census),]

#Subset to only KY and TN and remove unnecessary columns
KY_TN <- subset(census1, census1$State == "Kentucky" | census1$State == "Tennessee")
KY_TN <- KY_TN[,-c(1,15,17)]
OH <- subset(census1, census1$State =="Ohio")
OH <- OH[,-c(1,2,15,17)]

#Convert percentage columns to the actually number of people represented
columns <- c(6:11,15:27,30:34) 
for(i in columns)
{
  KY_TN[[i]] <- round(KY_TN$TotalPop * KY_TN[[i]] / 100)
  OH[[i-1]] <- round(OH$TotalPop*OH[[i-1]]/100)
}
#Convert avg measures to population measure
columns_mean <- c(13,14,28)
for (i in columns_mean)
{
  KY_TN[[i]] <- KY_TN$TotalPop * KY_TN[[i]]
  OH[[i-1]] <- OH$TotalPop*OH[[i-1]]
}

#Condense by county
KY_TN <- KY_TN %>% group_by(State, County) %>% summarise_all(sum)
KY_TN$Income <- KY_TN$Income/KY_TN$TotalPop
KY_TN$IncomePerCap <- KY_TN$IncomePerCap/KY_TN$TotalPop
KY_TN$MeanCommute <- KY_TN$MeanCommute/KY_TN$TotalPop

OH <- OH %>% group_by(County) %>% summarise_all(sum)
OH$Income <- OH$Income/OH$TotalPop
OH$IncomePerCap <- OH$IncomePerCap/OH$TotalPop
OH$MeanCommute <- OH$MeanCommute/OH$TotalPop

#Prepare United Way data for integration
UWay <- UWay[,c(2,7,8)]
colnames(UWay) <- c("Gross","County", "State")
UWay$Gross <- as.numeric(gsub("[$,]","",UWay$Gross))
UWay$State <- ifelse(UWay$State == "KY", "Kentucky", "Tennessee")

Ohio_UW <- Ohio_UW[,4:5]
colnames(Ohio_UW) <- c("County", "Gross")
Ohio_UW$Gross <- as.numeric(gsub("[$,]","",Ohio_UW$Gross))

#Merge United Way data into Census data by county. Doing this for each individual state then combining because of duplicate County names
KY <- subset(KY_TN, KY_TN$State == "Kentucky")
UWay_KY <- subset(UWay, UWay$State == "Kentucky")
KY <- full_join(KY, UWay_KY, by ="County")
TN <- subset(KY_TN, KY_TN$State =="Tennessee")
UWay_TN <- subset(UWay, UWay$State == "Tennessee")
TN <- full_join(TN, UWay_TN, by ="County")
KY_TN_Uway <- rbind(KY, TN)
names(KY_TN_Uway)[names(KY_TN_Uway) == "State.x"] <- "State"
KY_TN_Uway <- KY_TN_Uway[,-36]
KY_TN_Uway$Gross <- replace_na(KY_TN_Uway$Gross,0)
KY_TN_Uway$UW <- ifelse(KY_TN_Uway$Gross == 0,"No","Yes")
KY_TN_Uway$Gross <- NULL

OH_Uway <- full_join(OH, Ohio_UW, by="County")
OH_Uway$Gross <- replace_na(OH_Uway$Gross,0)
OH_Uway$UW <- ifelse(OH_Uway$Gross==0,"No","Yes")
OH_Uway$Gross <- NULL


#Begin conversion to format for ARM
KY_TN_Uway_ARM <- KY_TN_Uway[,-c(1,2)]

#Function to quickly discretize with explanatory labels for specific columns and quantile levels
quantisize <- function(Data, columns, prob)
{
  df <- Data
  for (k in columns)
  {
    steps <- 1/prob
    newCol <- c()
    q <- quantile(Data[[k]], probs=c(seq(prob, prob*(steps-1), prob)))
    Labels <- c()
    for (i in 0:steps)
    {
      if (i== steps) {break}
      if (i==0) {j <- 0} else {j <- t(q)[[i]]}
      if (i==steps-1) {m <- max(Data[[k]])} else {m <- t(q)[[i+1]]}
      Labels[[i+1]] <- paste("Q",i+1,"(",k,") = (",j,",",m,"]", sep="")
    }
    Data[[k]] <- cut(Data[[k]], breaks=c(-1, t(q),max(Data[[k]])), labels= Labels)
    df <- Data
  }
  return(df)
}

# Convert values to quantile ranges
KY_TN_Uway_ARM <- quantisize(KY_TN_Uway_ARM, colnames(KY_TN_Uway_ARM[,-c(9,33)]), 0.25)
KY_TN_Uway_ARM$Pacific <- ifelse(KY_TN_Uway_ARM$Pacific == 0, "Pac = No", "Pac = Yes")

#Write data to csv then read in as transactions
write.csv(KY_TN_Uway_ARM, "E:/Documents/IST 707/Data/KY_TN_Uway_ARM.csv")
ARM <- read.transactions("E:/Documents/IST 707/Data/KY_TN_Uway_ARM.csv", format="basket", sep=",", header=TRUE, rm.duplicates=FALSE)

#Function to determine optimal support and confidence levels
ruleCounts <- function(data, target, min, max)
{
  Scounts <- c()
  for (i in 1:9)
  {
    for (j in 1:9)
    {
      Scounts[[9*(i-1)+j]] <- length(apriori(data, parameter=list(support=10*min-i*min, conf=10*min-j*min,minlen=3,maxlen=5), 
                                             appearance=list(rhs=target, default="lhs"))@lhs)
    }
  }
  counts <- matrix(Scounts, nrow=9, ncol=9)
  counts <- data.frame(counts, row.names=c(seq(max,min,-min)))
  colnames(counts) <- c(seq(max,min,-min))
  return(counts)
}

# Determine rules that lead to Yes and rules that lead to No
ruleCounts(ARM, "Yes", 0.1, 0.9)
# support of 0.1 and confidence of 0.9 seems the best combo
Yesrules <- apriori(ARM, parameter = list(support=0.1, conf=0.9, minlen=3, maxlen=5), appearance = list(rhs="Yes", default="lhs"))
arules::inspect(head(sort(Yesrules, by="lift"),10))
plot(sort(Yesrules, by="count", decreasing=TRUE)[1:20], method="graph", engine="htmlwidget")

ruleCounts(ARM, "No", 0.1,0.9)
# support of 0.2 and confidence of 0.9 are best here
Norules <- apriori(ARM, parameter = list(support=0.2, conf=0.9, minlen=2,maxlen=4), appearance = list(rhs="No", default="lhs"))
arules::inspect(head(sort(Norules, by="lift"),25))
plot(sort(Norules, by="count", decreasing=TRUE)[1:20], method="graph", engine="htmlwidget")

#Clustering
# Silhouette method
fviz_nbclust(KY_TN_Uway[, -c(1,2,35)], kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

# Elbow method
fviz_nbclust(KY_TN_Uway[, -c(1,2,35)], kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

# Gap statistic
fviz_nbclust(KY_TN_Uway[, -c(1,2,35)], kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")

# Hierarchical clustering
dMatrix1 <- dist(KY_TN_Uway[,-c(1,2,35)], method="euclidean")
groups1 <- hclust(dMatrix1, method="ward.D")
plot(groups1, cex=0.9, hang=1)
rect.hclust(groups1,k=4)

dMatrix2 <- dist(KY_TN_Uway[,-c(1,2,35)], method="cosine")
groups2 <- hclust(dMatrix2, method="ward.D")
plot(groups2, cex=0.9, hang=1)
rect.hclust(groups2,k=4)

dMatrix3 <- dist(KY_TN_Uway[,-c(1,2,35)], method="euclidean")
groups3 <- hclust(dMatrix3, method="complete")
plot(groups3, cex=0.9, hang=1)
rect.hclust(groups3, k=4)

dMatrix4 <- dist(KY_TN_Uway[,-c(1,2,35)], method="cosine")
groups4 <- hclust(dMatrix4, method="complete")
plot(groups4, cex=0.9, hang=1)
rect.hclust(groups4,k=4)

# k means with 4 Clusters
k4 <- kmeans(KY_TN_Uway[,-c(1,2,35)], centers=4)
KY_TN_Uway$k4Cluster <- k4$cluster #Add cluster IDs to main data
fviz_cluster(k4, data=KY_TN_Uway[,-c(1,2,35,36)])
table(KY_TN_Uway$UW, KY_TN_Uway$k4Cluster)

melt <- melt(KY_TN_Uway[,c(3,13,15,16,34,36)], id.var="k4Cluster")
ggplot(melt, aes(x=variable, y=value))+ geom_boxplot(aes(fill=k4Cluster)) + facet_wrap(~ variable, scales="free")

# Prep data for supervised learning

# Function to normalize data
nor <- function(x)
{
  (x-min(x))/(max(x)-min(x))
}

KY_TN_Norm <- as.data.frame(lapply(KY_TN_Uway[,3:34], nor))
KY_TN_Norm <- data.frame(KY_TN_Norm, UW=KY_TN_Uway$UW)
plot(hclust(dist(KY_TN_Norm[,-33], method="cosine"), method="complete"), cex=0.9,hang=1)
rect.hclust(groups4,k=4)

#Function for model prediction metrics
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


M <- KY_TN_Norm
sampleSize <- round(nrow(KY_TN_Norm)/5)

#Model 1#####################################
set.seed(05221981)
samples1 <- sample(1:nrow(M),sampleSize, replace=FALSE)
train1 <- M[-samples1,]
test1 <- M[samples1,]

#Naive Bayes
NB1 <- naiveBayes(UW ~ ., data=train1)
NB1_predict <- predict(NB1, test1)
NBpred1 <- prediction(as.numeric(NB1_predict), as.numeric(test1$UW))
NBperf1 <- performance(NBpred1, measure="tpr", x.measure="fpr")
NBplot1 <- (plot(NBperf1, main= "ROC curve for Naive Bayes", col="blue", lwd=3)+ abline(coef=c(0,1)))
(NB1_pred_table <- table(NB1_predict, test1$UW))
NB1_acc <- (sum(diag(NB1_pred_table)/sum(NB1_pred_table)))
print(paste0("Naive Bayes Accuracy: ", NB1_acc))
model_metrics(NB1_pred_table,c("NO","Yes"))

#Decision Tree
DT1 <- rpart(UW ~ ., data = train1, method = 'class')
fancyRpartPlot(DT1)
predicted1= predict(DT1, test1, type="class")
(DT_pred_table1 <- table(Predicted=predicted1, True=test1$UW))
DT_acc1 <- (sum(diag(DT_pred_table1))/sum(DT_pred_table1))
print(paste0("Decision Tree Accuracy: ", DT_acc1))
model_metrics(DT_pred_table1, c("No","Yes"))

# SVM
tune1 <- tune.svm(x=train1[,-33], y=train1[,33], type="C-classification", kernel = "polynomial", degree=2, cost=10^(1:3), gamma=c(0.1,1,10), coef0=c(0.1,1,10))
gamma1 <- tune1$best.parameters$gamma
coef01 <- tune1$best.parameters$coef0
cost1 <- tune1$best.parameters$cost
SVM1 <- svm(UW ~ ., data=train1, kernel="polynomial", degree=2, gamma=gamma1, coef0=coef01, cost=cost1, scale=FALSE)
SVM_pred1<- predict(SVM1, test1[,-33], type="class")
(SVM1_pred_table <- table(SVM_pred1, test1$UW))
SVM1_acc <- (sum(diag(SVM1_pred_table)/sum(SVM1_pred_table)))
print(paste0("SVM accuracy: ", SVM1_acc))
model_metrics(SVM1_pred_table, c("No","Yes"))

# knn

# Function to determine optimal number of k for knn
k.optm <- function (trainSet, testSet, label)
{
  k.acc <- c()
  for (i in 1: round(sqrt(nrow(trainSet))))
  {
    knn.mod <- class::knn(train = trainSet[,-label], test = testSet[,-label], cl = trainSet[,label], k=i)
    k.acc[i] <- (sum(diag(table(knn.mod,testSet[,label]))/sum(table(knn.mod,testSet[,label]))))
  }
  k.acc
}

k.optm1 <- k.optm(train1, test1, 33)
K1 <- which((k.optm1)==max(k.optm1), arr.ind=TRUE)
plot(k.optm1, type="b", xlab="K- Value", ylab="Accuracy") + abline(v=min(K1), lty=4)
knn1 <- class::knn(train = train1[,-33], test = test1[,-33], cl = train1[,33], k=min(K1))
(KTable1 <- table(knn1, test1[,33]))
knn1_acc <- (sum(diag(KTable1)/sum(KTable1)))
print(paste0("knn accuracy: ", knn1_acc))
model_metrics(KTable1, c("No", "Yes"))

# Random Forest
RF1 <- randomForest(UW ~ ., train1)
RF1_pred <- predict(RF1, test1[,-33])
(RFTable1 <- table(RF1_pred, test1[,33]))
hist(treesize(RF1))
varImpPlot(RF1)
RF1_acc <- sum(diag(RFTable1))/sum(RFTable1)
print(paste0("Random Forst accuracy: ", RF1_acc))
model_metrics(RFTable1, c("No","Yes"))


# Model 2
set.seed(10261981)
samples2 <- sample(1:nrow(M),sampleSize, replace=FALSE)
train2 <- M[-samples2,]
test2 <- M[samples2,]

#Naive Bayes
NB2 <- naiveBayes(UW ~ ., data=train2)
NB2_predict <- predict(NB2, test2)
NBpred2 <- prediction(as.numeric(NB2_predict), as.numeric(test2$UW))
NBperf2 <- performance(NBpred2, measure="tpr", x.measure="fpr")
NBplot2 <- (plot(NBperf2, main= "ROC curve for Naive Bayes", col="blue", lwd=3)+ abline(coef=c(0,1)))
(NB2_pred_table <- table(NB2_predict, test2$UW))
NB2_acc <- (sum(diag(NB2_pred_table)/sum(NB2_pred_table)))
print(paste0("Naive Bayes Accuracy: ", NB2_acc))
model_metrics(NB2_pred_table,c("No","Yes"))

#Decision Tree
DT2 <- rpart(UW ~ ., data = train2, method = 'class')
fancyRpartPlot(DT2)
predicted2= predict(DT2, test2, type="class")
(DT_pred_table2 <- table(Predicted=predicted2, True=test2$UW))
DT_acc2 <- (sum(diag(DT_pred_table2))/sum(DT_pred_table2))
print(paste0("Decision Tree Accuracy: ", DT_acc2))
model_metrics(DT_pred_table2, c("No","Yes"))

# SVM
tune2 <- tune.svm(x=train2[,-33], y=train2[,33], type="C-classification", kernel = "polynomial", degree=2, cost=10^(1:3), gamma=c(0.1,1,10), coef0=c(0.1,1,10))
gamma2 <- tune2$best.parameters$gamma
coef02 <- tune2$best.parameters$coef0
cost2 <- tune2$best.parameters$cost
SVM2 <- svm(UW ~ ., data=train2, kernel="polynomial", degree=2, gamma=gamma2, coef0=coef02, cost=cost2, scale=FALSE)
SVM_pred2<- predict(SVM2, test2[,-33], type="class")
(SVM2_pred_table <- table(SVM_pred2, test2$UW))
SVM2_acc <- (sum(diag(SVM2_pred_table)/sum(SVM2_pred_table)))
print(paste0("SVM accuracy: ", SVM2_acc))
model_metrics(SVM2_pred_table, c("No","Yes"))

# knn
k.optm2 <- k.optm(train2, test2, 33)
K2 <- min(which((k.optm2)==max(k.optm2), arr.ind=TRUE))
plot(k.optm2, type="b", xlab="K- Value", ylab="Accuracy") + abline(v=K2, lty=3)
knn2 <- class::knn(train = train2[,-33], test = test2[,-33], cl = train2[,33], k=K2)
(KTable2 <- table(knn2, test2[,33]))
knn2_acc <- (sum(diag(KTable2)/sum(KTable2)))
print(paste0("knn accuracy: ", knn2_acc))
model_metrics(KTable2, c("No", "Yes"))

# Random Forest
RF2 <- randomForest(UW ~ ., train2)
RF2_pred <- predict(RF2, test2[,-33])
(RFTable2 <- table(RF2_pred, test2[,33]))
hist(treesize(RF2))
varImpPlot(RF2)
RF2_acc <- sum(diag(RFTable2))/sum(RFTable2)
print(paste0("Random Forst accuracy: ", RF2_acc))
model_metrics(RFTable2, c("No","Yes"))


# Model 3
set.seed(032514)
samples3 <- sample(1:nrow(M),sampleSize, replace=FALSE)
train3 <- M[-samples3,]
test3 <- M[samples3,]

#Naive Bayes
NB3 <- naiveBayes(UW ~ ., data=train3)
NB3_predict <- predict(NB3, test3)
NBpred3 <- prediction(as.numeric(NB3_predict), as.numeric(test3$UW))
NBperf3 <- performance(NBpred3, measure="tpr", x.measure="fpr")
NBplot3 <- plot(NBperf3, main= "ROC curve for Naive Bayes", col="blue", lwd=3)+ abline(coef=c(0,1))
(NB3_pred_table <- table(NB3_predict, test3$UW))
NB3_acc <- (sum(diag(NB3_pred_table)/sum(NB3_pred_table)))
print(paste0("Naive Bayes Accuracy: ", NB3_acc))
model_metrics(NB3_pred_table,c("No","Yes"))

#Decision Tree
DT3 <- rpart(UW ~ ., data = train3, method = 'class')
fancyRpartPlot(DT3)
predicted3= predict(DT3, test3, type="class")
(DT_pred_table3 <- table(Predicted=predicted3, True=test3$UW))
DT_acc3 <- (sum(diag(DT_pred_table3))/sum(DT_pred_table3))
print(paste0("Decision Tree Accuracy: ", DT_acc3))
model_metrics(DT_pred_table3, c("No","Yes"))

# SVM
tune3 <- tune.svm(x=train3[,-33], y=train3[,33], type="C-classification", kernel = "polynomial", degree=2, cost=10^(1:3), gamma=c(0.1,1,10), coef0=c(0.1,1,10))
gamma3 <- tune3$best.parameters$gamma
coef03 <- tune3$best.parameters$coef0
cost3 <- tune3$best.parameters$cost
SVM3 <- svm(UW ~ ., data=train3, kernel="polynomial", degree=2, gamma=gamma3, coef0=coef03, cost=cost3, scale=FALSE)
SVM_pred3<- predict(SVM3, test3[,-33], type="class")
(SVM3_pred_table <- table(SVM_pred3, test3$UW))
SVM3_acc <- (sum(diag(SVM3_pred_table)/sum(SVM3_pred_table)))
print(paste0("SVM accuracy: ", SVM3_acc))
model_metrics(SVM3_pred_table, c("No","Yes"))

# knn
k.optm3 <- k.optm(train3, test3, 33)
K3 <- min(which((k.optm3)==max(k.optm3), arr.ind=TRUE))
plot(k.optm3, type="b", xlab="K- Value", ylab="Accuracy") + abline(v=K3, lty=3)
knn3 <- class::knn(train = train3[,-33], test = test3[,-33], cl = train3[,33], k=K3)
(KTable3 <- table(knn3, test3[,33]))
knn3_acc <- (sum(diag(KTable3)/sum(KTable3)))
print(paste0("knn accuracy: ", knn3_acc))
model_metrics(KTable3, c("No", "Yes"))

# Random Forest
RF3 <- randomForest(UW ~ ., train3)
RF3_pred <- predict(RF3, test3[,-33])
(RFTable3 <- table(RF3_pred, test3[,33]))
hist(treesize(RF3))
varImpPlot(RF3)
RF3_acc <- sum(diag(RFTable3))/sum(RFTable3)
print(paste0("Random Forst accuracy: ", RF3_acc))
model_metrics(RFTable3, c("No","Yes"))

# Apply models to Ohio
OH_Norm <- as.data.frame(lapply(OH_Uway[,2:33],nor))
OH_Norm <- data.frame(OH_Norm, UW=OH_Uway$UW)

# Naive Bayes with model 3
OH_NB_Pred <- predict(NB3, OH_Norm)
OHNBpred <- prediction(as.numeric(OH_NB_Pred), as.numeric(OH_Norm$UW))
OHNBperf <- performance(OHNBpred, measure="tpr", x.measure="fpr")
OHNBplot <- plot(OHNBperf, main= "ROC curve for Naive Bayes", col="blue", lwd=3)+ abline(coef=c(0,1))
(OH_NB_PredTable <- table(OH_Uway$UW, OH_NB_Pred))
OH_NB_Acc <- (sum(diag(OH_NB_PredTable)/sum(OH_NB_PredTable)))
print(paste0("Naive Bayes Accuracy: ", OH_NB_Acc))
model_metrics(OH_NB_PredTable, c("No","Yes"))

# DECISION TREE with model 3
DT_OH <- predict(DT3, OH_Norm[,-33], type="class")
(DT_OH_table <- table(Predicted=DT_OH, OH_Uway$UW))
DT_OH_acc <- sum(diag(DT_OH_table))/sum(DT_OH_table)
print(paste0("Ohio Decision Tree Accuract: ", DT_OH_acc))
model_metrics(DT_OH_table, c("No","Yes"))

# SVM with model 1
SVM_test_OH <- predict(SVM1, OH_Norm[,-33], type="class")
(OHTable <- table(SVM_test_OH, OH_Uway$UW))
SVM_OH_acc <- (sum(diag(OHTable)/sum(OHTable)))
print(paste0("SVM accuracy: ", SVM_OH_acc))
model_metrics(OHTable, c("No","Yes"))

# knn with model 3
k.optmOH <- k.optm(train3, OH_Norm, 33)
K_OH <- min(which((k.optmOH)==max(k.optmOH), arr.ind=TRUE))
plot(k.optmOH, type="b", xlab="K- Value", ylab="Accuracy") + abline(v=K_OH, lty=3)
knnOH <- class::knn(train = train3[,-33], test = OH_Norm[,-33], cl = train1[,33], k=K_OH)
(KTableOH <- table(knnOH, OH_Norm[,33]))
knnOH_acc <- (sum(diag(KTableOH)/sum(KTableOH)))
print(paste0("knn accuracy: ", knnOH_acc))
model_metrics(KTableOH, c("No", "Yes"))
                                              
# random forest with model 3
RF_OH_pred <- predict(RF3, OH_Norm[,-33])
(OH_RFTable <- table(RF_OH_pred, OH_Norm[,33]))
hist(treesize(RF_OH))
varImpPlot(RF_OH)
RFOH_acc <- sum(diag(OH_RFTable))/sum(OH_RFTable)
print(paste0("Random Forst accuracy: ", RFOH_acc))
model_metrics(OH_RFTable, c("No","Yes"))

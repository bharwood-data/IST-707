library(tm)
library(stringr)
library(wordcloud)
library(slam)
library(quanteda)
library(SnowballC)
library(arules)
library(proxy)
library(cluster)
library(stringi)
library(proxy)
library(Matrix)
library(tidytext)
library(plyr) 
library(ggplot2)
library(factoextra) 

setwd("E:\\Documents\\IST 707")

FedCorpus <- Corpus(DirSource("Data\\Fed_Corpus"))
getTransformations()
ndocs <- length(FedCorpus)

summary(FedCorpus)
meta(FedCorpus[[1]])
meta(FedCorpus[[1]],5)

(minTermFreq <- ndocs * 0.01)
(maxTermFreq <- ndocs * .50)

Fed_dtm <- DocumentTermMatrix(FedCorpus,
              control=list(
                wordlengths=c(4,10),
                removePunctuation = TRUE,
                removeNumbers = TRUE,
                remove_separators = TRUE,
                bounds = list(golbal=c(minTermFreq, maxTermFreq))
              ))

(WordFreq <- colSums(as.matrix(Fed_dtm)))

Fed_M <- as.matrix(Fed_dtm)
(Fed_M[1:15,1:10])
Fed_M_N1 <- apply(Fed_M, 1, function(i) round(i/sum(i),3))
(Fed_M_N1[1:15,1:10])
Fed_Matrix_Norm <- t(Fed_M_N1)
(Fed_Matrix_Norm[1:15,1:10])

Fed_DF <- as.data.frame(Fed_M)
head(Fed_DF)
str(Fed_DF)
nrow(Fed_DF)

Fed_DF_N <- as.data.frame(Fed_Matrix_Norm)

wordcloud(colnames(Fed_M), Fed_M, max.words=2000)

distMatrix_E <- dist(Fed_M,method="euclidean")
groups_E <- hclust(distMatrix_E,method="ward.D")
plot(groups_E, cex=0.9, hang=-1)
rect.hclust(groups_E, k=2)

distMatrix_C <- dist(Fed_M, method="cosine")
groups_C <- hclust(distMatrix_C,method="ward.D")
plot(groups_C, cex=0.9, hang=-1)
rect.hclust(groups_C, k=2)
plot(groups_C, cex=0.9, hang=-1)
rect.hclust(groups_C, k=3)

distMatrix_C_norm <- dist(Fed_M_Norm, method="cosine")
groups_C_n <- hclust(distMatrix_C_norm,method="ward.D")
plot(groups_C_n, cex=0.9, hang=-1)
rect.hclust(groups_C_n, k=2)
plot(groups_C_n, cex=0.9, hang=-1)
rect.hclust(groups_C_n, k=3)
plot(groups_C_n, cex=0.9, hang=-1)
rect.hclust(groups_C_n, k=4)

distance0 <- get_dist(Fed_M_norm,method = "euclidean")
fviz_dist(distance0, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
distance1 <- get_dist(Fed_M_norm,method = "manhattan")
fviz_dist(distance1, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
distance2 <- get_dist(Fed_M_norm,method = "pearson")
fviz_dist(distance2, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
distance3 <- get_dist(Fed_M_norm,method = "canberra")
fviz_dist(distance3, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
distance4 <- get_dist(Fed_M_norm,method = "spearman")
fviz_dist(distance4, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

kmeansFIT_1 <- kmeans(Fed_M, centers=3)

Fed_m <- Fed_M[-c(63:70),]
Fed_m_n <- t(Fed_M_N1)[-c(63:70),]

distMatrix_E <- dist(Fed_m, method="euclidean")
groups_E <- hclust(distMatrix_E,method="ward.D")
plot(groups_E, cex=0.9, hang=-1)
rect.hclust(groups_E, k=2)

distMatrix_C <- dist(Fed_m, method="cosine")
groups_C <- hclust(distMatrix_C,method="ward.D")
plot(groups_C, cex=0.9, hang=-1)
rect.hclust(groups_C, k=2)

distMatrix_C_norm <- dist(Fed_m_n, method="cosine")
groups_C_n <- hclust(distMatrix_C_norm,method="ward.D")
plot(groups_C_n, cex=0.9, hang=-1)
rect.hclust(groups_C_n, k=2)



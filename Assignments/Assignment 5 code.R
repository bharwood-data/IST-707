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
library(tibble)
library(rpart)
library(rattle)

setwd("E:\\Documents\\IST 707")

FedCorpus <- Corpus(DirSource("Data\\Fed_Corpus"))
getTransformations()
ndocs <- length(FedCorpus)


FedCorpus <- tm_map(FedCorpus, content_transformer(tolower))
FedCorpus <- tm_map(FedCorpus, removeNumbers)
FedCorpus <- tm_map(FedCorpus, removePunctuation)
FedCorpus <- tm_map(FedCorpus, removeWords, c(MyStopwords, STOPS))
FedCorpus <- tm_map(FedCorpus, stripWhitespace)
minTermFreq <- 30
maxTermFreq <- 1000
MyStopwords <- c("will","one","two", "may","less","publius","Madison","Alexand",
                 "Alexander", "James", "Hamilton", "hamilton", "Jay", "well","might",
                 "without","small", "single", "several", "but", "very", "can", "must", 
                 "also", "any", "and", "are", "however", "into", "almost", "can","for", 
                 "add", "Author", "author", "alexand", "alexander", "jame", "james", "madison" )
STOPS <- stopwords('english')

Fed_dtm <- DocumentTermMatrix(FedCorpus,
                              control = list(
                                stopwords = TRUE,
                                wordLengths=c(3, 15),
                                removePunctuation = T,
                                removeNumbers = T,
                                tolower=T,
                                stemming = T,
                                remove_separators = T,
                                stopwords = MyStopwords,
                                removeWords=STOPS,
                                removeWords=MyStopwords,
                                bounds = list(global = c(minTermFreq, maxTermFreq))
                              ))
Fed_dtm <- removeSparseTerms(Fed_dtm, 0.99)

Fed <- as.matrix(Fed_dtm)
WordFreq <- colSums(as.matrix(Fed_dtm))
(head(WordFreq))
(length(WordFreq))
ord <- order(WordFreq)
(WordFreq[head(ord)])

(Row_Sum_Per_doc <- rowSums((as.matrix(Fed_dtm))))

Fed_M <- as.matrix(Fed_dtm)
Fed_M_N1 <- apply(Fed_M, 1, function(i) round(i/sum(i),3))
Fed_Matrix_Norm <- t(Fed_M_N1)
Fed_DF <- as.data.frame(as.matrix(Fed_Matrix_Norm))
Fed_DF <- Fed_DF[-c(66:70),]
Fed_DF1 <- tibble::rownames_to_column(Fed_DF)

names(Fed_DF1)[1] <- "Author"
Fed_DF1[1:11,1]="disp"
Fed_DF1[12:65,1]="ham"
Fed_DF1[66:80,1]="mad"

DispCloud <- wordcloud(colnames(Fed), Fed[11,], rot.per = .35, colors = brewer.pal(5, "Set1"))
HamCloud <- wordcloud(colnames(Fed),Fed[50:53,], rot.per = .35, colors = brewer.pal(5, "Set1"))
MadCloud <- wordcloud(colnames(Fed), Fed[63:66,], rot.per = .35, colors = brewer.pal(5, "Set1"))
trainRatio <- 0.6
set.seed(121804)
sample <- sample.int(n=nrow(Fed_DF1), size=floor(trainRatio*nrow(Fed_DF1)), replace=FALSE)
train <- Fed_DF1[sample,]
test <- Fed_DF1[-sample,]

training_tree1 <- rpart(Author ~ ., data=train, method="class")
summary(training_tree1)
predict1 <- predict(training_tree1, test, type = "class")
rsq.rpart(training_tree1)
fancyRpartPlot(training_tree1)

table(Authorship=predict1, true=test$Author)

training_tree2 <- rpart(Author ~ ., data=train, method="class", control=rpart.control(cp=0, minsplit = 2, maxdepth = 5))
summary(training_tree2)
predict2 <- predict(training_tree2, test, type="class")
rsq.rpart(training_tree2)
fancyRpartPlot(training_tree2) 

table(Authorship=predict2, true=test$Author)

tree <- rpart(Author~., data=Fed_DF1[-c(1:11),], method="class")
predict4 <- predict(tree, Fed_DF1[1:11,], type="class")
predict4
rownames(Fed[1:11,])
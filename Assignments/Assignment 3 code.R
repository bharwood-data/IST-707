## load libraries
library(ggplot2)
library(reshape2)
library(corrplot)
library(DescTools)
library(car)
library(pastecs)
library(dplyr)
library(arules)
library(arulesViz)
library(gridExtra)

## load, inspect, and clean data
bankData <- read.csv("E:/Documents/IST 707/Data/bankdata_csv_all.csv")
str(bankData)

sum(is.na(bankData)==TRUE)

bankData <- bankData[,-1]
bankData$age <- cut(bankData$age, breaks =c(10,20,30,40,50,60,Inf), labels=c("teens", "twenties", "thirties", "forties", "fifties", "sixties"))
m <- min(bankData$income)
M <- max(bankData$income)
bins = 3
width=(M-m)/bins
bankData$income <- case_when(bankData$income <= m +width ~ "tier 1", bankData$income > m + width & bankData$income <= M - width ~ "tier 2", bankData$income > M - width ~ "tier 3")
unique(bankData$children)
bankData$children <- dplyr::case_when(bankData$children == 0 ~"no kids", bankData$children == 1 ~ "1 kid", bankData$children == 2 ~ "2 kids", bankData$children == 3 ~ "3 kids")
bankData$married=dplyr::recode(bankData$married, YES="married", NO = "single")
bankData$car=dplyr::recode(bankData$car, YES="car", NO = "no car")
bankData$save_act=dplyr::recode(bankData$save_act, YES="savings", NO ="no savings")
bankData$current_act=dplyr::recode(bankData$current_act, YES="current", NO ="not current")
bankData$mortgage=dplyr::recode(bankData$mortgage, YES="mortgage", NO ="no mortgage")
bankData$pep=dplyr::recode(bankData$pep, YES ="pep", NO ="no pep")

## plots
agePlot <- ggplot(bankData, aes(x=age)) + geom_bar(fill="blue") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
sexPlot <- ggplot(bankData, aes(x=sex)) + geom_bar() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
pepPlot <- ggplot(bankData, aes(x=pep)) + geom_bar() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
mortgagePlot <- ggplot(bankData, aes(x=mortgage)) + geom_bar() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
currentPlot <- ggplot(bankData, aes(x=current_acct)) + geom_bar() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
savePlot <- ggplot(bankData, aes(x=save_acct)) + geom_bar() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
marriedPlot <- ggplot(bankData, aes(x=married)) + geom_bar() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
childrenPlot <- ggplot(bankData, aes(x=children)) + geom_bar(fill="steelblue") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
carPlot <- ggplot(bankData, aes(x=car)) + geom_bar() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
incomePlot <- ggplot(bankData, aes(x=income)) + geom_bar(fill="red") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
regionPlot <- ggplot(bankData, aes(x=region)) + geom_bar(fill="green") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
savePlot <- ggplot(bankData, aes(x=save_act)) + geom_bar() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
currentPlot <- ggplot(bankData, aes(x=current_act)) + geom_bar() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
grid.arrange(agePlot, sexPlot, regionPlot, incomePlot, marriedPlot, carPlot, childrenPlot, savePlot, currentPlot, mortgagePlot, pepPlot, ncol=4)

## write cleaned data to new file, then re-load as transaction for ARM
write.csv(bankData, "E:/Documents/IST 707/Data/Bank")
bank <- read.transactions("E:/Documents/IST 707/Data/Bank", format="basket", sep=",", header=TRUE, rm.duplicates=FALSE)

## initial ARM
rules <- apriori(bank, parameter=list(support=0.3, conf=0.7, minlen=2))
inspect(rules)
rules1 <- apriori(bank, parameter=list(support=0.3, conf=0.5, minlen=2))
inspect(rules1)
rules2 <- apriori(bank, parameter=list(support=0.3, conf=0.3, minlen=2))
inspect(rules2)

## functions to run multiple apriori with varying support and confidence to find good thresholds
ruleCounts <- function()
{
  Scounts <- c()
  for (i in 1:9)
  {
    for (j in 1:9)
      {
      Scounts[[9*(i-1)+j]] <- length(apriori(bank, parameter=list(support=1-i*.1, conf=1-j*.1,minlen=2))@lhs)
      }
  }
  counts <- matrix(Scounts, nrow=9, ncol=9)
  counts <- data.frame(counts, row.names=c(seq(0.9,0.1,-0.1)))
  colnames(counts) <- c(seq(0.9,0.1,-0.1))
  return(counts)
}

PEPCounts <- function()
{
  Pcounts <- c()
  for (i in 5:9)
  {
    for (j in 1:9)
    {
      Pcounts[[9*(i-5)+j]] <- length(apriori(bank, parameter=list(support=1-i*.1, conf=1-j*.1,minlen=2), appearance=list(rhs="pep", default="lhs"))@lhs)
    }
  }
  counts <- matrix(Pcounts, nrow=9, ncol=5)
  counts <- data.frame(counts, row.names=c(seq(0.9,0.1,-0.1)))
  colnames(counts) <- c(seq(0.5,0.1,-0.1))
  return(counts)
}

## ARM with focused RHS
pepRules <- apriori(bank, parameter(support=0.1, conf=0.1, minlen=2), appearance=list(rhs="pep", default="lhs"))
plot(pepRules, method="graph")
pepRulesC <- sort(pepRules, by="count", decreasing=TRUE)
inspect(pepRulesC[1:20])
pepRulesL <- sort(pepRules, by="lift", decreasing=TRUE)
inspect(pepRulesL[1:20])
inspect(pepRulesL[73:93])

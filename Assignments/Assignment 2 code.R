## Package loading
library(ggplot2)
library(reshape2)
library(corrplot)
library(DescTools)
library(car)
library(pastecs)
library(dplyr)

## Read in data - I use the file.choose() command because for some reason the "working directory" thing eludes me
schools <- read.csv(file.choose())

## Add sum column, change column names, and quick data view with descriptive statistics
schools$enrolled <- apply(schools[3:8], 1, sum)
colnames(schools) <- c("School", "Section", "VA", "M", "B", "MB", "VB", "C","E") 

head(schools)

stat.desc(schools[,3:9])

## Subset data by school
A <- subset(schools, schools$School=="A")
B <- subset(schools, schools$School=="B")
C <- subset(schools, schools$School=="C")
D <- subset(schools, schools$School=="D")
E <- subset(schools, schools$School=="E")

## Example plot of school data
A1 <- melt(A, id.vars="section") ## this converts the data frame into a LONG data type that ggplot can use
ggplot(A1[14:104,], aes(x=Section, y=value, fill=variable)) + geom_bar(stat="identity",position="dodge") +
  scale_fill_brewer(palette = "Blues")+scale_x_continuous(breaks=seq(1,13,1)) + xlab("Section") + ylab("Students")

## Plot total number of students in each school by progress level, then plot the reverse
ggplot(melt(schoolTotals[,-c(2,8)]), aes(x=variable, y=value, fill=School)) +
  geom_bar(stat="identity", position="dodge")+scale_fill_brewer(palette="Oranges") +xlab("Progress") +ylab("Students")

ggplot(melt(schoolTotals[,-c(2,8)],id.vars="School"), aes(x=School, y=value, fill=variable)) +
  geom_bar(stat="identity", position="dodge")+scale_fill_brewer(palette="Greens") + xlab("School") + ylab("Students")

## Determine school totals and percentage of enrolled students in each progress level
schoolTotals <- aggregate(schools[,3:9], by=list(School = schools$School), FUN = sum)
schoolPercentages <- data.frame(schoolTotals[,1:2], t(apply(schoolTotals[,3:7],1,function(i) round(i/sum(i),2))))

## Plot correlation of progress variables only to check for collinearity in anticipation of discriminant analysis
corrplot.mixed(cor(schools[,4:8]))

## Levene test for homogeneity of variance priot to ANOVA
leveneTest(M~School, data=schools)
leveneTest(B~School, data=schools)
leveneTest(MB~School, data=schools)
leveneTest(VB~School, data=schools)
leveneTest(C~School, data=schools)

## Progress variable interaction plot
School <- factor(schools1$School[which(schools1$variable!="VA")])
Progress <- factor(schools1$variable[which(schools1$variable != "VA")])
interaction.plot(Progress,School,schools1$value[which(schools1$variable!="VA")], type="b", 
                 leg.bg = "beige", leg.bty = "o", col=c(1:5), lwd=2, xlab="Progress", 
                 ylab = "Average students", main="Interaction Plot")

## ANOVA test and effect size calculation for each progress variable
summary(aov(C ~ School, schools))
EtaSq(aov(M~School, schools))
summary(aov(M ~ School, schools))
EtaSq(aov(B~School, schools))
summary(aov(B ~ School, schools))
EtaSq(aov(MB~School, schools))
summary(aov(MB ~ School, schools))
EtaSq(aov(VB~School, schools))
summary(aov(VB ~ School, schools))
EtaSq(aov(C~School, schools))

## Scheffe post hoc tests
ScheffeTest(aov(M~School, schools))
ScheffeTest(aov(B~School, schools))
ScheffeTest(aov(MB~School, schools))
ScheffeTest(aov(VB~School, schools))
ScheffeTest(aov(C~School, schools))
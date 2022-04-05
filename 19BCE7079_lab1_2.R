#NAME : Hariprasad K K
#REG NO : 19BCE7079
#dataset link :https://www.kaggle.com/datasets/jonatancr/airports
install.packages("ClusterR")
install.packages("cluster")
library(ClusterR)
library(cluster)
 x=read.csv("airports.csv")
 View(x)
 na.omit(x)
 x1 <- as.numeric(as.factor(x))
 set.seed(240)
 kmeans.re <- kmeans(x1, centers = 3, nstart = 20)
 kmeans.re
 kmeans.re$cluster
 cm <- table(x1, kmeans.re$cluster)
 cm
kmeans.re$centers
 kmeans.re$centers[,]
 y_kmeans <- kmeans.re$cluster
 Accuracy1= (kmeans.re$betweenss/kmeans.re$totss)*100
 Accuracy1
#with preprocessing
 x=read.csv("airports.csv")
 View(x)
 x$first<-factor(x$first,levels=c('a','b'),labels=c(1,2))
 x$X1<-factor(x$Goroka.Airport,levels=c('a','b'),labels=c(1,2))
 x=x[,!names(x) %in% c("tenth")]
 x$Pacific.Port_Moresby <- paste(x$airport,x$OurAirports)
 x=x[,!names(x) %in% c("X10")]
 sum(is.na(x$X1))
 sum(is.na(x$Goroka.Airport))
 sum(is.na(x$Goroka))
 sum(is.na(x$Papua.New.Guinea))
 sum(is.na(x$Papua.New.Guinea))
 sum(is.na(x$GKA))
 sum(is.na(x$AYGA))
 sum(is.na(x$X.6.081689834590001))
 sum(is.na(x$X145.391998291))
 sum(is.na(x$X5282))
 sum(is.na(x$U))
 sum(is.na(x$Pacific.Port_Moresby))
 sum(is.na(x$airport))
 sum(is.na(x$OurAirports))
 x$X1[is.na(x$X1)]<-round(mean(as.numeric(x$X1),na.rm=TRUE))
 x$Goroka.Airport[is.na(x$Goroka.Airport)]<-round(mean(as.numeric(x$Goroka.Airport),na.rm=TRUE))
 sum(is.na(x))
 View(sum)
 x1 <- as.numeric(as.factor(x))

#using Kmeans 
set.seed(240)
 kmeans.re <- kmeans(x1, centers = 3, nstart = 20)
 kmeans.re
 kmeans.re$cluster cm <- table(x1, kmeans.re$cluster)
cm
Accuracy2= (kmeans.re$betweenss/kmeans.re$totss)*100
Accuracy


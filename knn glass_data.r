glass=read.csv(file.choose())
View(glass)
summary(glass)
dimnames(glass)
glass$Type<-factor(glass$Type, levels=c(1,2,3,5,6,7), 
                   labels=c("bwfp","bwnfp","vwfp","containers",
                            "tableware","headlamps"))
View(glass)
str(glass)
sum(is.null(glass))
plot(glass)
##using box plot for knowning outlayers
boxplot(glass)
boxplot(glass$RI)
boxplot(glass$Na)
boxplot(glass$Mg)
boxplot(glass$Si)
boxplot(glass$Al)
boxplot(glass$K)
boxplot(glass$Ca)
boxplot(glass$Ba)
boxplot(glass$Fe)
##using histogram plot 
hist(glass$RI)
hist(glass$Na)
hist(glass$Mg)
hist(glass$Al)
hist(glass$Si)
hist(glass$K)
hist(glass$Ca)
hist(glass$Ba)
hist(glass$Fe)
#######
library(ggplot2)
ggplot(data = glass,aes(x=glass$RI,fill=glass$Type))+
  geom_density(alpha=0.9,color='black')
###
ggplot(data = glass,aes(x=glass$Na,fill=glass$Type))+
  geom_density(alpha=0.9,color='black')
##
ggplot(data = glass,aes(x=glass$Al,fill=glass$Type))+
  geom_density(alpha=0.9,color='black')
##
ggplot(data = glass,aes(x=glass$Si,fill=glass$Type))+
  geom_density(alpha=0.9,color='black')
##
ggplot(data = glass,aes(x=glass$K,fill=glass$Type))+
  geom_density(alpha=0.9,color='black')
##
ggplot(data = glass,aes(x=glass$Ca,fill=glass$Type))+
  geom_density(alpha=0.9,color='black')
##
ggplot(data = glass,aes(x=glass$Ba,fill=glass$Type))+
  geom_density(alpha=0.9,color='black')
##
ggplot(data = glass,aes(x=glass$Fe,fill=glass$Type))+
  geom_density(alpha=0.9,color='black')
##
ggplot(data = glass,aes(x=glass$Type,fill=glass$Type))+
  geom_density(alpha=0.9,color='black')
#####
# Spliting training and test dataset
set.seed(1234)
id=sample(2,nrow(glass),replace = T,prob = c(0.8,0.2))
testing=glass[id==1,]
training=glass[id==2,]
###k-nearest nighbours method
library(caret)
library(pROC)
library(mlbench)
library(class)
set.seed(222)
ctrl <- trainControl(method="repeatedcv",repeats = 3,number = 10)
knnFit <- train(Type~., data = training, method = "knn", trControl = ctrl,
                preProcess = c("center","scale"), tuneLength = 20)
knnFit
plot(knnFit)
varImp(knnFit)
pred=predict(knnFit,newdata = testing)
con=confusionMatrix(pred,testing$Type)
con
#improving accuracy
set.seed(222)
ctrl1 <- trainControl(method="repeatedcv",repeats = 3,number = 1)
knnFit1 <- train(Type~., data = training, method = "knn", trControl = ctrl,
                preProcess = c("center","scale"), tuneLength = 20)
knnFit1
plot(knnFit1)
varImp(knnFit1)
pred=predict(knnFit1,newdata = testing)
con1=confusionMatrix(pred,testing$Type)
con1

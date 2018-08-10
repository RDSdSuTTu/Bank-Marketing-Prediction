
#title: "Bank Marketing Project"
#author: "Rupam Das"





#importing data from the system
a<-read.csv("C:/Users/Rupam Das/Desktop/Project/bank.csv",sep = ";")
str(a)


#probability table of dependent variable
prop.table(table(a$y))

#checking number of nas
#checking NA values columnwise
sum(is.na(a))
sapply(a, function(x) sum(is.na(x)))# number of nas

# data manipulation and creating data sets for different analysis
#scaling the numerical variables
a1<-a[,-17]

for(i in c("age","balance","duration","campaign", "pdays", "previous")){
  a1[,i]<- scale(a1[,i])
}
#str(a1)

#converting the factors to numeric values
for(i in c("job","education","marital", "default", "housing","loan","contact","month","poutcome")){
  a1[,i]<- as.numeric(a1[,i])
}
str(a1)
#head(a1,10)

#visualization and exploratory analysis
library(corrplot)
bank<- a1
bank<- cbind(a1, a$y)
bank$`a$y`<-as.numeric(bank$`a$y`)
colnames(bank)[17]<-"class"
corr<- cor(bank)
round(corr,2)
corrplot(corr, method = "circle", number.cex = 0.55)

#Education level wise Yes/No
bar<-table(a$y, a$education)
barplot(bar, main="Education Level wise Yes/No", col=c("darkblue","red"),
        legend = rownames(bar), ylim = c(0,2500))
#prop.table(table(a$education, a$y))

#Month wise Yes/No
bar<-table(a$y, a$month)
barplot(bar, main="Month wise Yes/No"
        , col=c("darkblue","red"),
        legend = rownames(bar), ylim = c(0,1500), cex.names=.7, las = 2)
#prop.table(table(a$month, a$y))

#Job wise Yes/No
bar<-table(a$y, a$job)
barplot(bar, main="Job wise Yes/No"
        , col=c("darkblue","red"),
        legend = rownames(bar), ylim = c(0,1000), cex.names=.7, las = 2)
#prop.table(table(a$job, a$y))

# contact typewise Yes/No
bar<-table(a$y, a$contact)
barplot(bar, main="contact typewise Yes/No"
        , col=c("darkblue","red"),
        legend = rownames(bar), ylim = c(0,3000), cex.names=.7, las = 1)
#prop.table(table(a$contact, a$y))

#Data Partition for modelbuilding
set.seed(12)
trainrow<- sample(1:nrow(a1), 0.7*nrow(a1))
train<- a1[trainrow, ]
test<- a1[-trainrow, ]
str(train)


str(test)

#capturing labels of training and test data set
train.labels<- a[1:3164,17]
test.labels<- a[3165:4521,17]

prop.table(table(train.labels))
prop.table(table(test.labels))

### K-NN MODEL
#elbow test for deciding the number of K for K-NN model
plot.wgss = function(mydata, maxc) {
  wss = numeric(maxc)
  for (i in 1:maxc) wss[i] = kmeans(mydata,centers=i, nstart = 10)$tot.withinss 
  plot(1:maxc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares", main="Scree Plot") }
plot.wgss(a1, 10) # Elbow test


#Building the K-NN model and confusion matrix
library(class)
pred1<- knn(train = train, test = test, cl = train.labels, k = 3)
#summary(pred1)
library(caret)
confusionMatrix(pred1,test.labels)
#cor(as.numeric(test.labels), as.numeric(pred1))


#modifying data for further analysis/models
a2<- cbind(a1,a$y)
colnames(a2)[17]<-"convert"
#str(a2)


#data partition for train and test
library(caret)
library(e1071)
set.seed(5)
partition<-createDataPartition(y = a2$convert, p=0.7, list= F)
train<- a2[partition,]
test<- a2[-partition,]
#str(train)
#str(test)


#Logistic regression taking all variables
model <- glm(convert ~., data = train, family = binomial(logit))
summary(model)


predscore<- predict(model, test, type = "response")
#head(predscore)


k<-cbind(test,predscore)
clasify <- array(c(99))
for(i in 1:nrow(k)){
  if(k[i,18]<0.3){  
    clasify[i]<-"no"
  } else{
    clasify[i]<-"yes"
  } 
}

confusionMatrix(clasify,(k$convert))
#cor(as.numeric(k$convert),as.numeric(as.factor(clasify)))

#summary(model)


#Logistic regression taking significant and high weight variables
model1<-glm(convert ~ age  + education +  housing + loan + contact + duration + campaign + pdays + previous + poutcome + month , data = train, family = binomial(logit))
summary(model1)

predscore1<- predict(model1, test, type = "response")
#head(predscore1)
k1<-cbind(test,predscore1)
clasify1 <- array(c(99))
for(i in 1:nrow(k1)){
  if(k1[i,18]<0.3){ 
    clasify1[i]<-"no"
  } else{
    clasify1[i]<-"yes"
  } 
}

confusionMatrix(clasify1,k1$convert)
#cor(as.numeric(k1$convert),as.numeric(as.factor(clasify1)))

#Logistic regression taking only significant variables
sigmodel<-glm(convert ~ age  +  housing + loan + contact + duration + campaign + pdays + previous , data = train, family = binomial(logit)) ####
#summary(sigmodel)


sigpredscore<- predict(sigmodel, test, type = "response")
#head(predscore1)
ks<-cbind(test,sigpredscore)
sclasify <- array(c(99))
for(i in 1:nrow(ks)){
  if(ks[i,18]<0.3){ 
    sclasify[i]<-"no"
  } else{
    sclasify[i]<-"yes"
  } 
}

confusionMatrix(sclasify,ks$convert) #### getting same result
#cor(as.numeric(ks$convert),as.numeric(as.factor(sclasify)))

#Linear discriminant analysis/model and confusion matrix
library(MASS)
model2<- lda(convert ~., data = train)
#model2

pred2<- predict(model2, test)$class

table(pred2,test$convert)

confusionMatrix(pred2,test$convert)
#cor(as.numeric(pred2),as.numeric(test$convert))


#support vector machine model and confusion matrix
#library(e1071)
library(kernlab)
svmodel<- ksvm(convert ~ ., train, kernel = "rbfdot", gamma = 13)
#svmodel1<- svm(convert ~ ., train, kernel = "sigmoid")
#summary(svmodel)
pred3<- predict(svmodel,test)
confusionMatrix(pred3, test$convert)
#cor(as.numeric(pred3), as.numeric(test$convert))


#Decision tree model using c50 and confusion matrix
library(C50)
model3<- C5.0(train[-17], train$convert, trials = 11) 
pred4<- predict(model3,test, type = "class")
confusionMatrix((pred4),test$convert)
#cor(as.numeric(pred4), as.numeric(test$convert))
#summary(model3)


#ddecision tree using rpart and confusion matrix
library(rpart)
model4<- rpart(convert ~., data = train, method = "class")


pred5 <- predict(model4, test, type  = "class")


confusionMatrix(pred5,test$convert)
#cor(as.numeric(test$convert), as.numeric(pred5))


##data processing for applying neural network concept
library(neuralnet)
a3<-a2
#str(a3)
a3$convert<- as.numeric(a3$convert)
set.seed(5)
trainrow<- sample(1:nrow(a3), 0.7*nrow(a3))
ntrain<- a3[trainrow, ]
ntest<- a3[-trainrow, ]


##neural network model, prediction, and confusion matrix

net.a3 <-neuralnet(convert ~ age + job + marital + education + default + balance + housing + loan + contact + day + month + duration + campaign + pdays + previous + poutcome, data = ntrain, hidden = 3, learningrate.limit = NULL, learningrate = 0.00001)

#plot(net.a3)
model_results <-compute(net.a3, ntest[1:16])

predict_convert<- model_results$net.result

nclasify <- array(c(99))
for(i in 1:nrow(predict_convert)){
  if(predict_convert[i,1]<1.5){   
    nclasify[i]<-"1"
  } else{
    nclasify[i]<-"2"
  } 
}

confusionMatrix(nclasify, ntest$convert)



######################### TELECOM CUSTOMER ANALYSIS #############################


###Invoking of the necessary Libraries ####
install.packages("readxl")
library(readxl)
install.packages("class")
library(class)
install.packages("psych")
library(psych)
install.packages("ggplot2")
library(ggplot2)
install.packages("caTools")
library(caTools)
install.packages("ineq")
library(ineq)
install.packages("caret")
library(caret)
install.packages("ROCR")
library(ROCR)
install.packages("KODAMA")
library(KODAMA)
install.packages("Hmisc")
library(Hmisc)
install.packages("InformationValue")
library(InformationValue)
install.packages("e1071")
library(e1071)
### Setting up the Working directory ###
setwd("C:/R programs great lakes/P Model/project")
getwd()
#### Importing the dataset and creation of the dataframe #####
cell = read_xlsx("Cellphone1.xlsx")
View(cell)
#### EDA of dataset ####
dim(cell)
str(cell)
head(cell)
tail(cell)
summary(cell)
### Conversion of numericals to factors ####
cell$Churn = as.factor(cell$Churn)
cell$ContractRenewal = as.factor(cell$ContractRenewal)
cell$DataPlan = as.factor(cell$DataPlan)

### Uni-Variate Analysis ####
### Analysis of Independent Numerical variables ###
hist.data.frame(cell)
### Analysis of Independent Categorical variables ###
table(cell$ContractRenewal)
prop.table(table(cell$ContractRenewal))*100
qplot(ContractRenewal,fill = ContractRenewal,data = cell)
table(cell$DataPlan)
prop.table(table(cell$DataPlan))*100
qplot(DataPlan,fill = DataPlan,data = cell)
### Analysis of Dependent variables ###
table(cell$Churn)
prop.table(table(cell$Churn))*100
qplot(Churn,fill = Churn,data = cell)

##### Bi-Variate Analysis #####
### Dependent variable with Independent Categorical variable ###
qplot(ContractRenewal,fill = Churn,data = cell)
qplot(DataPlan,fill = Churn,data = cell,geom = "bar")
### Independent Numerical variables with Independent Categorical variables ###
qplot(OverageFee,fill = DataPlan,data = cell)
qplot(DayMins,fill = ContractRenewal,data = cell)
qplot(MonthlyCharge,fill = DataPlan,data = cell)
qplot(CustServCalls,AccountWeeks,col = DataPlan,data = cell)
qplot(OverageFee,RoamMins,fill = DataPlan,data = cell,geom = "area")
### Dependent variables with Numerical variables ###
qplot(MonthlyCharge,fill = Churn,data = cell)
qplot(CustServCalls,fill = Churn,data = cell,geom = "density")
qplot(DayMins,DataUsage,fill = Churn,data = cell,geom = "boxplot")

### Checking for Missing Values ###
sum(is.na(cell))

### Checking for the outliers ####
boxplot(cell[,-c(1,3,4)])

### Checking for Multicollinearity ###
### Correlation Matrix and Correlation Plot ###
cor.plot(cell[,-c(1,3,4)],numbers = TRUE)
### Checking the Eigen Values ####
Eigen = eigen(cor(cell[,-c(1,3,4)]))
Eigen$values
### Checking the Scatter Plots ###
plot(cell[,-c(1,3,4)])


#### Splitting of data into Training and Testing set(70-30) as per industry standards ####
set.seed(77)
indices = sample(nrow(cell),0.70*nrow(cell),replace = FALSE)
cell.train = cell[indices,]
cell.test = cell[-indices,]
dim(cell.train)
dim(cell.test)
#### Building a logistic regression model ####
reg = glm(Churn~.,data = cell.train,family = "binomial")
summary(reg)
reg1 = glm(Churn~ContractRenewal+RoamMins+CustServCalls+DataPlan
          +MonthlyCharge*DataUsage+DayMins*MonthlyCharge
          +OverageFee*MonthlyCharge -MonthlyCharge -OverageFee-DayMins,data = cell.train
          ,family = "binomial")
summary(reg1)

### Logistic Regression Predictions - I (Train on Train data)####)
reg.train.predict = predict(reg1,cell.train,type = "response")
plot(cell.train$Churn,reg1$fitted.values)
abline(a = 0.30,b = 0)
reg.train.response = ifelse(reg.train.predict > 0.3,1,0)
reg.train.response = as.factor(reg.train.response)
cell.train$Churn = as.factor(cell.train$Churn)
### Confusion Matrix Logistic Regression(Train on Train data) ###
library(caret)
caret::confusionMatrix(cell.train$Churn,reg.train.response,positive = "1")
#### ROC Logistic Regression(Train on Train data) ######
reg.train.obj = prediction(reg.train.predict,cell.train$Churn)
pref.reg.train = performance(reg.train.obj,"tpr","fpr")
plot(pref.reg.train)
### AUC Logistic Regression(Train on Train data)#####
pref.reg.train = performance(reg.train.obj,"tpr","fpr")
auc.reg.train = performance(reg.train.obj,"auc")
auc.reg.train = as.numeric(auc.reg.train@y.values)
print(auc.reg.train)
#### KS Logistic Regression(Train on Train data) ######
print(max(pref.reg.train@y.values[[1]] - pref.reg.train@x.values[[1]]))
### Gini Logistic Regression(Train on Train data) ####
gini.reg.train = ineq(reg.train.predict,"gini")
print(gini.reg.train)
### Concordance Regression(Train on Train data) ###
reg.train.x = cell.train$Churn
reg.train.y = reg.train.predict
Concordance(actuals = reg.train.x,predictedScores = reg.train.y)

### Logistic Regression Predictions - II (Train on Test data)####
reg.test.predict = predict(reg1,cell.test,type = "response")
plot(cell.test$Churn,reg.test.predict)
abline(a = 0.20,b = 0)
reg.test.response = ifelse(reg.test.predict > 0.20,1,0)
reg.test.response = as.factor(reg.test.response)
cell.test$Churn = as.factor(cell.test$Churn)
### Confusion Matrix Logistic Regression(Train on Test data) ###
caret::confusionMatrix(cell.test$Churn,reg.test.response,positive = "1")
#### ROC Logistic Regression(Train on Test data) ######
reg.test.obj = prediction(reg.test.predict,cell.test$Churn)
pref.reg.test = performance(reg.test.obj,"tpr","fpr")
plot(pref.reg.test)
### AUC Logistic Regression(Train on Test data)#####
auc.reg.test = performance(reg.test.obj,"auc")
auc.reg.test = as.numeric(auc.reg.test@y.values)
print(auc.reg.test)
#### KS Logistic Regression(Train on Test data) ######
print(max(pref.reg.test@y.values[[1]] - pref.reg.test@x.values[[1]]))
### Gini Logistic Regression(Train on Test data)####
gini.reg.test = ineq(reg.test.predict,"gini")
print(gini.reg.test)
### Concordance Logistic Regression(Train on Test data) ###
reg.test.x = cell.test$Churn
reg.test.y = reg.test.predict
Concordance(actuals = reg.test.x,predictedScores = reg.test.y)
### Plotting Actuals vs. Predicted ###
plot(cell.test$Churn,reg.test.response,xlab = "Actuals",ylab = "Predicted")

#### Building a Naive Bayes model ####
set.seed(77)
nb.cell.train = naiveBayes(Churn~.,data = cell.train)
print(nb.cell.train)
#### Making predictions Naive Bayes (Train on Train) #####
nb.train.response = predict(nb.cell.train,newdata = cell.train,type = 'class')
nb.train.predict= predict(nb.cell.train,newdata = cell.train,type = 'raw')
nb.train.predict = as.data.frame(nb.train.predict)
### Confusion Matrix Naive Bayes (Train on Train) ###
caret::confusionMatrix(nb.train.response,cell.train$Churn,positive = "1")
### Building ROC cuvre Naive Bayes (Train on Train) ####
nb.train.obj = prediction(nb.train.predict$`1`,cell.train$Churn)
nb.train.perf = performance(nb.train.obj,"tpr","fpr")
plot(nb.train.perf)
### AUC Naive Bayes(Train on Train) ####
nb.train.auc = performance(nb.train.obj,"auc")
nb.train.auc = as.numeric(nb.train.auc@y.values)
print(nb.train.auc)
### KS Naive Bayes(Train on Train)####
print(max(nb.train.perf@y.values[[1]] - nb.train.perf@x.values[[1]]))
### GINI Naive Bayes(Train on Train) ####
nb.train.gini = ineq(nb.train.predict$`1`,"gini")
print(nb.train.gini)
### Concordance Ratio Naive Bayes(Train on Train) ###
nb.train.x = cell.train$Churn
nb.train.y = nb.train.predict$`1`
Concordance(actuals = nb.train.x,predictedScores = nb.train.y)

#### Making predictions on test data Naive Bayes(Train on Test) #####
nb.test.response = predict(nb.cell.train,newdata = cell.test,type = 'class')
nb.test.predict= predict(nb.cell.train,newdata = cell.test,type = 'raw')
nb.test.predict = as.data.frame(nb.test.predict)
### Confusion matrix Naive Bayes (Train on Test) ###
caret::confusionMatrix(nb.test.response,cell.test$Churn,positive = "1")
### Building ROC cuvre Naive Bayes(Train on test) ####
nb.test.obj = prediction(nb.test.predict$`1`,cell.test$Churn)
nb.test.perf = performance(nb.test.obj,"tpr","fpr")
plot(nb.test.perf)
### AUC Naive Bayes(Train on test) ####
nb.test.auc = performance(nb.test.obj,"auc")
nb.test.auc = as.numeric(nb.test.auc@y.values)
print(nb.test.auc)
### KS Naive Bayes(Train on test)####
print(max(nb.test.perf@y.values[[1]] - nb.test.perf@x.values[[1]]))
### GINI Naive Bayes(Train on Test) ####
nb.test.gini = ineq(nb.test.predict$`1`,"gini")
print(nb.test.gini)
### Concordance Naive Bayes(Train on Test) ###
nb.test.x = cell.test$Churn
nb.test.y = nb.test.predict$`1`
Concordance(actuals = nb.test.x,predictedScores = nb.test.y)
### Plotting Actuals vs. Predicted ###
plot(cell.test$Churn,nb.test.response,xlab = "Actuals",ylab = "Predicted")


#### Building A KNN model ####
#### Training model on Training data KNN (train on train) ####
knn.train.p = knn(cell.train,cell.train,cl = cell.train$Churn,k = 48,prob = TRUE)
knn.train.c = knn(cell.train,cell.train,cell.train$Churn,k=48)
knn.train.prob = attributes(knn.train.p)$prob
knn.prob.df = data.frame(knn.train.prob,knn.train.c)
knn.prob.df$knn.train.prob[knn.train.c == "0"] = 1 - knn.prob.df$knn.train.prob[knn.train.c == "0"]
knn.train.predict = knn.prob.df$knn.train.prob
knn.train.response = knn.prob.df$knn.train.c
### Confusion Matrix KNN(Train on Train) ####
caret::confusionMatrix(knn.train.response,cell.train$Churn,positive = "1")
### ROC Curve KNN(Train on Train) ###
knn.train.obj = prediction(knn.train.predict,cell.train$Churn)
knn.train.perf = performance(knn.train.obj,"tpr","fpr")
plot(knn.train.perf)
### AUC Curve KNN(Train on Train) ###
knn.train.auc = performance(knn.train.obj,"auc")
knn.train.auc = as.numeric(knn.train.auc@y.values)
print(knn.train.auc)
### KS Value KNN(Train on Train) ####
print(max(knn.train.perf@y.values[[1]] - knn.train.perf@x.values[[1]]))
### GINI KNN(Train on Train) ####
knn.train.gini = ineq(knn.train.predict,"gini")
print(knn.train.gini)
### Concordance KNN (Train on Train) ###
knn.train.x = cell.train$Churn
knn.train.y = knn.train.predict
Concordance(actuals = knn.train.x,predictedScores = knn.train.y)

#### Training model on testing data KNN(train on test) ####
knn.test.p = knn(cell.train,cell.test,cl = cell.train$Churn,k = 31,prob = TRUE)
knn.test.c = knn(cell.train,cell.test,cell.train$Churn,k=31)
knn.test.prob = attributes(knn.test.p)$prob
knn.prob.df = data.frame(knn.test.prob,knn.test.c)
knn.prob.df$knn.test.prob[knn.test.c == "0"] = 1 - knn.prob.df$knn.test.prob[knn.test.c == "0"]
knn.test.predict = knn.prob.df$knn.test.prob
knn.test.response = knn.prob.df$knn.test.c
### Confusion Matrix KNN (train on test) ###
caret::confusionMatrix(knn.test.response,cell.test$Churn,positive = "1")
### ROC Curve KNN(train on test) ###
knn.test.obj = prediction(knn.test.predict,cell.test$Churn)
knn.test.perf = performance(knn.test.obj,"tpr","fpr")
plot(knn.test.perf)
### AUC Curve KNN(train on test) ###
knn.test.auc = performance(knn.test.obj,"auc")
knn.test.auc = as.numeric(knn.test.auc@y.values)
print(knn.test.auc)
### KS Value KNN(train on test) ####
print(max(knn.test.perf@y.values[[1]] - knn.test.perf@x.values[[1]]))
### GINI KNN(train on test) ####
knn.test.gini = ineq(knn.test.predict,"gini")
print(knn.test.gini)
### Concordance KNN (Train on Test) ###
knn.test.x = cell.test$Churn
knn.test.y = knn.test.predict
Concordance(actuals = knn.test.x,predictedScores = knn.test.y)
### Plotting a graph between actuals vs. predicted ###
plot(cell.test$Churn,knn.test.response,xlab = "Actuals",ylab = "Predicted")

install.packages("C50")
install.packages("tree")
install.packages("caret")
install.packages("gmodels")
install.packages("party")
install.packages("knitr")
install.packages("png")
library(C50)
library(tree)
library(caret)
library(gmodels)
library(party)
library(knitr)
library(png)


FraudCheck <- read.csv("Fraud_check.csv")
View(FraudCheck)
hist(FraudCheck$Taxable.Income)

# Splitting data into training and testing.
# splitting the data based on Income
Risky_Good = ifelse(FraudCheck$Taxable.Income<= 30000, "Risky", "Good")
FC = data.frame(FraudCheck,Risky_Good)
View(FC)

FC_train <- FC[1:300,]

FC_test <- FC[301:600,]

###Using Party Function 

png(file = "decision_tree.png")
opall_tree = ctree(Risky_Good ~ Undergrad + Marital.Status + City.Population + 
                     Work.Experience + Urban, data = FC)
summary(opall_tree)

plot(opall_tree)
# From the above tree, It looks like the data has 20 % of Risky patients and 80 % good patients


# using the training Data 

png(file = "decision_tree.png")
op_tree = ctree(Risky_Good ~ Undergrad + Marital.Status + City.Population + 
                  Work.Experience + Urban, data = FC_train)
summary(op_tree)

plot(op_tree)

pred_tree <- as.data.frame(predict(op_tree,newdata=FC_test))
pred_tree["final"] <- NULL
pred_test_df <- predict(op_tree,newdata=FC_test)


mean(pred_test_df==FC_test$Risky_Good) # Accuracy = 82 %

CrossTable(FC_test$Risky_Good,pred_test_df)

confusionMatrix(FC_test$Risky_Good,pred_test_df)

#From this we are getting 82% accuracy which is very good.




#With C5.0 package model building
# Building model on training data 
income_train <- C5.0(Risky_Good~.,data= FC_train)
windows()
plot(income_train) # Tree graph
# Training accuracy
pred_train <- predict(income_train,FC_train)

length(FC_train)
length(pred_train)
FC_train
pred_train

mean(FC_train$Risky_Good == pred_train) # 100% Accuracy


confusionMatrix(pred_train,FC_train$Risky_Good)
#This train model is giving us 100% accuracy 

pred_test <- predict(income_train,newdata=FC_test) # predicting on test data
mean(pred_test==FC_test$Risky_Good) # 100% accuracy 

confusionMatrix(pred_test,FC_test$Risky_Good)
#This train model is giving us 100% accuracy 


# Cross tablez
CrossTable(FC_test$Risky_Good,pred_test)

# We are getting 82% accuracy with tree package for modelling and 100% accuracy with c5.0 Package for same splitting of data.

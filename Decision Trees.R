###### Script for analyzing the data with decision trees #####

# Date: 2017-05-11
# Author: Sokrates Lamprou
# LiU-ID: andla830@student.liu.se
# Course: 729G40: Kandidatuppsats
# University: Linköping University

#Libraries
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(DMwR)
library(caret)
library(rpart)



#Functions
################# Decimal function #############################
specify_decimal <- function(x, k) format(round(x, k), nsmall=k)


################## Accuracy measures ######################
meas <- function (response, predicted, threshold = 0.5) 
{
  ### checks
  if(length(response) != length(predicted)) 
    stop("Response and predicted must have the same length.\n")
  if(length(labels <- levels(factor(response))) != 2) 
    stop("Response must have two levels.\n")
  if(cl <- class(predicted) == "factor" | class(predicted) == "character")
  {
    if(lev <- length(levels(factor(predicted))) != 2)
      stop("predicted must have two levels.\n")
    predicted <- as.numeric(predicted)
  }
  ###
  
  splitted <- split(predicted, response)
  negatives <- splitted[[as.character(labels[1])]]
  n.negatives <- length(negatives)
  positives <- splitted[[as.character(labels[2])]]
  n.positives <- length(positives)
  
  TN <- sum(positives >= threshold)
  FP <- sum(negatives >= threshold)
  TP <- sum(negatives < threshold)
  FN <- sum(positives < threshold)
  
  ACCURACY <- (TP+TN) / (TP + TN + FP + FN)
  PRECISION <- TP/(TP+FP)
  RECALL <- TP/(TP+FN)
  F <- 2 * (RECALL*PRECISION/(RECALL+PRECISION))
  message("TP: ", TP)
  message("FP: ", FP)
  message("TN: ", TN)
  message("FN: ", FN)
  out <- list(Call=match.call(), threshold=threshold,
              precision= (PRECISION * 100),
              recall = (RECALL*100),
              accuracy = (ACCURACY*100),
              F=(F*100))
  class(out) <- "meas"
  out
}

####print method for accuracy measures
print.meas <- function(x, ...)
{
  cat("\n")
  cat("Call: \n")
  print(x$Call)
  cat("\n")
  cat("Examples are labelled as positive when predicted is greater than", x$threshold,"\n")
  cat("\n")
  
  cat( paste("precision: ", sprintf("%.2f",x$precision)," %","\n", sep="") )
  cat( paste("recall: ", sprintf("%.2f",x$recall)," %","\n",sep="") )
  cat( paste("accuracy: ", sprintf("%.2f",x$accuracy)," %","\n", sep="") )
  cat( paste("F: ", sprintf("%.2f",x$F)," %","\n", sep="") )
  
  
  
}

################## Accuracy measures########################

################# Oversampling the data ###################
library(DMwR)
smote_data <- data.frame(lapply(data, as.factor), stringsAsFactors=TRUE)


newData <- SMOTE(Riskdrinker~ ., smote_data,perc.over = 150, perc.under = 400)
table(newData$Riskdrinker)

################# Oversampling the data ###################



################## Dividing and preparing data ############

set.seed(42)
s = sample(957,200) #477 är hälften, 191 = 20%, 300  ~ 32%

dataset_large_train = dataset_large[-s,] 
dataset_large_test = dataset_large[s,] 

table(dataset_large_test$Riskdrinker)
table(dataset_large_train$Riskdrinker)

training = data[-s,] 
test = data[s,] 
message("Size of training: ", dim(training))
message("Size of test: ", dim(test))
# Show the number of positive vs negative in test versus training dataset. 
table(test$Riskdrinker)
table(training$Riskdrinker)
View(test)
table(training$Vegan)



################## Dividing and preparing data #####


################## Decision trees ################## 


#The model
treeimb <- rpart(Riskdrinker ~ ., data = training, method="class", parms = list(split = "information"))
treeimb

#ROC
library("ROCR")
pred5 <- predict(treeimb, test, type = "prob")
pred5_2 = prediction(pred5[,2], test$Riskdrinker)
plot(performance(pred5_2, "tpr", "fpr"))
abline(0, 1, lty = 2)
V
#PLOT ROC
dtm_prob <- predict(treeimb, newdata=test, type="vector")
dtm_prob_pred <- prediction(pred5, test$Riskdrinker)
dtm_performance <- performance(dtm_prob_pred, measure = "tpr", x.measure = "fpr") #x.measure, or y.measure
plot(perf_pred_probi)
#



#print rules
summary(treeimb)
printcp(treeimb)

#plotcp
plotcp(treeimb)


#Plot the tree

plot_treeimb <- fancyRpartPlot(treeimb,main="Decision Tree for Riskdrinkers", tweak=0.7, cex=0.6)
prp(treeimb, extra=6, box.palette="auto")
plotcp(treeimb)
zp <- prune(treeimb, cp = 0.1)


#AUC

dtm_auc <- predict(treeimb, test, type = "prob")
dtm_auc_pred <- prediction(dtm_auc[,2], test$Riskdrinker)
#AUC VALUE
t1.auc <- performance(dtm_auc_pred, "auc")
t1.auc <- t1.auc@y.values[[1]]
t1.auc

#
#dtm_probab <- predict(treeimb, newdata=test, type="prob")
#dtm_probab_pred <- prediction(dtm_probab, test_log$Riskdrinker)
#perf_pred_probi <- performance(pred_probi, measure = "tpr", x.measure = "fpr") #x.measure, or y.measure
#plot(perf_pred_probi)
#

#AUC
#auc1 = performance(pred_probi, measure = "auc")
#auc2 <- auc1@y.values[[1]]
#auc2


# List the variable importance
vi = data.frame(treeimb$variable.importance)
#print variable importance
vi

#Make a prediction of the test test (for a manual metrics).
pred <- predict(treeimb, newdata = test, type="class")

# Make a prediction of the test test (for ROSE PACKAGE)
pred.treeimb <- predict(treeimb, newdata = test)

#Summary of the predictions
meas(test$Riskdrinker, pred.treeimb[,2])
#Make a matrix
confMat <- table(pred, test$Riskdrinker)
confMat
#Summary and plots the confusion matrix
summary_Cm = confusionMatrix(confMat)
#Print cf_summary
print(summary_Cm)

#Summary
print(treeimb)

#printCP
printcp(treeimb)

#Evaluation#
############

#Calculations
p1 = specify_decimal(confMat[1,1] / (confMat[1,1] + confMat[2,1])*100, 1)
r1 = specify_decimal(confMat[1] / (confMat[1] + confMat[1,2])*100, 1)
a1 = specify_decimal((confMat[1]+confMat[2,2]) / (confMat[1] + confMat[1,2] + confMat[2,1] + confMat[2,2])*100, 1)


f1 = specify_decimal(2* (as.double(p1) * as.double(r1)) /(as.double(p1)+as.double(r1)),1)



#Precision
message("Precision: ", p1, " %")

#Recall
message("Recall: ", r1, " %")

#Accuracy
message("Accuracy: ", a1, " %")

#F
message("f: ", f1, " %")



################## Pruned Decision trees ###########################


# Prune the decision tree 
pdt_model<- prune(treeimb, cp = treeimb$cptable[which.min(treeimb$cptable[,"xerror"]),"CP"])

#Print rules
print(pdt_model)
plotcp(pdt_model)
summary(pdt_model)

# Plot the pruned decision tree 
pruned_plot = fancyRpartPlot(pdt_model,main="Decision Tree for Riskdrinkers", tweak=1.2)



#Variable Importance for the pruned tree
data.frame(pdt_model$variable.importance)

#ROC

pruned_pred <- predict(pdt_model, test, type = "prob")
pruned_pred_ROC = prediction(pred5[,2], test$Riskdrinker)
plot(performance(pruned_pred_ROC, "tpr", "fpr"))
abline(0, 1, lty = 2)

#ROC
pdt_prob <- predict(pdt_model, newdata=test, type="vector")
pdt_prob_pred <- prediction(pdt_prob, test$Riskdrinker)
pdt_performance <- performance(pdt_prob_pred, measure = "tpr", x.measure = "fpr") #x.measure, or y.measure
plot(pdt_performance)

#AUC for pruned tree

pdt_auc <- predict(pdt_model, test, type = "prob")
pdt_auc_pred <- prediction(pdt_auc[,2], test$Riskdrinker)
#AUC VALUE
pdt.auc <- performance(pdt_auc_pred, "auc")
pdt.auc <- pdt.auc@y.values[[1]]
pdt.auc





#Make a prediction of the test test (for a manual metrics).
pred_pdt <- predict(pdt_model, newdata = test, type="class")

# Make a prediction of the test test (for ROSE PACKAGE)
pred.treeimb_pdt <- predict(pdt_model, newdata = test)

#Summary of the predictions
pruned_meas <- meas(test$Riskdrinker, pred.treeimb_pdt[,2])
#print
pruned_meas

#Make a matrix
confMat_pruned <- table(pred_pdt, test$Riskdrinker)

#Summary and plots the confusion matrix
summary_Cm_pruned = confusionMatrix(confMat_pruned)
#Print summary_Cm_pruned
print(summary_Cm_pruned)

#Summary
print(pdt_model)

#printCP
printcp(pdt_model)

#Evaluation#
############

#Calculations

precision <- specify_decimal(confMat_pruned[1,1] / (confMat_pruned[1,1] + confMat_pruned[2,1])*100, 1)
recall <- specify_decimal(confMat_pruned[1] / (confMat_pruned[1] + confMat_pruned[1,2])*100, 1)

accuracy <- specify_decimal((confMat_pruned[1]+confMat_pruned[2,2]) / (confMat_pruned[1] + confMat_pruned[1,2] + confMat_pruned[2,1] + confMat_pruned[2,2])*100, 1)
f1 <- specify_decimal((as.double(precision) * as.double(recall)) /(as.double(precision)+as.double(recall)),1)

#Precision
message("Precision: ", precision, " %")
#Recall
message("Recall: ",recall, " %")
#Accuracy
message("Accuracy: ",accuracy , " %")
#F
message("F: ",f1, " %")

################## Pruned Decision trees ###########################

################## Cross validated Decision trees with k-fold and repetitions ###########################

#Preparing the dataframes

learn <- data.frame(training)
testing <- data.frame(test)

#Adjustments

#Crossvalidation k-fold 10
cvCtrl <- trainControl(method="cv", number=5)

#Crossvalidation k-fold 10 - 3 repetitions
cvCtrl_rep <- trainControl(method = "repeatedcv", number = 5, repeats = 3)

# Create a cross validated model
cv_model <- train(Riskdrinker ~.,learn,method='rpart',
                  parms = list(split = "information"),
                  metric = "Accuracy",
                  tuneLength = 30,
                  trControl = cvCtrl)


help(importance)
#Rules
cv_model

#Variable Importance for the cross validated model
varImp(cv_model)

#Plot the CV model
fancyRpartPlot(cv_model$finalModel)

plot(cv_model, log=x)


#ROC
cv_pred_ROC <- predict(cv_model, testing, type = "prob")
cv_pred_ROC_model = prediction(cv_pred_ROC[,2], testing$Riskdrinker)
plot(performance(cv_pred_ROC_model, "tpr", "fpr"))
abline(0, 1, lty = 2)





#Predictions
#Manual metrics
cv_pred.treeimb <- predict(cv_model, newdata = testing)



# Make a prediction of the test test (for ROSE PACKAGE)
cv_p <- predict(cv_model, newdata = testing, type ="prob")

#Summary of the predictions for ROSE
meas(testing$Riskdrinker, cv_p[,2])



#Make a matrix
cv_p_mat <- table(cv_pred.treeimb, test$Riskdrinker)



#Plot matrix

#Summary and plots the confusion matrix
cv_p_mat_confusion = confusionMatrix(cv_p_mat)
#Print summary_Cm_pruned
print(cv_p_mat_confusion)


#Evaluation#
############

#Calculations

cv_p1 <- specify_decimal(cv_p_mat[1,1] / (cv_p_mat[1,1] + cv_p_mat[2,1])*100, 1)
cv_r1 <- specify_decimal(cv_p_mat[1] / (cv_p_mat[1] + cv_p_mat[1,2])*100, 1)

cv_a1 <- specify_decimal((cv_p_mat[1]+cv_p_mat[2,2]) / (cv_p_mat[1] + cv_p_mat[1,2] + cv_p_mat[2,1] + cv_p_mat[2,2])*100, 1)
cv_f1 <- specify_decimal((as.double(cv_p1) * as.double(cv_r1)) /(as.double(cv_p1)+as.double(cv_r1)),1)

#Precision
message("Precision: ", cv_p1, " %")
#Recall
message("Recall: ",cv_r1, " %")
#Accuracy
message("Accuracy: ",cv_a1 , " %")
#F
message("F: ",cv_f1, " %")



################## Cross validated Decision trees with k-fold and repetitions ###########################


################## C5.0 experiments ###########################

library(C50)
library(partykit)

c5_algorithm = C5.0(as.factor(Riskdrinker)~., training)

c5_algorithm

#Summary
summary(c5_algorithm)


#Variable Importance for the cross validated model
varImp(c5_algorithm)
C5imp(c5_algorithm)


#Plot the tree - doesn't work lol.
myTree2 <- C50:::as.party.C5.0(c5_algorithm)
plot_c5 <- fancyRpartPlot(c5_algorithm,main="Decision Tree for Riskdrinkers", tweak=1.5)

#Prediction
#ROSE
c5_algorithm_pred <- predict(c5_algorithm, newdata = test, type ="prob")

#Manual metrics
c5_algorithm_pred_manual <- predict(c5_algorithm, newdata = test)


#Summary of the predictions for ROSE
meas(test$Riskdrinker, c5_algorithm_pred[,2])


#Manual table
#Make a matrix
c5_algorithm_mat <- table(c5_algorithm_pred_manual, test$Riskdrinker)
c5_algorithm_mat
#Summary and plots the confusion matrix
c5_algorithm_mat_confusion = confusionMatrix(c5_algorithm_mat)
#Print summary_Cm_pruned
print(c5_algorithm_mat_confusion)


#Evaluation#
############

#Calculations

pc5 <- specify_decimal(c5_algorithm_mat[1,1] / (c5_algorithm_mat[1,1] + c5_algorithm_mat[2,1])*100, 1)
rc5 <- specify_decimal(c5_algorithm_mat[1] / (c5_algorithm_mat[1] + c5_algorithm_mat[1,2])*100, 1)

ac5 <- specify_decimal((c5_algorithm_mat[1]+c5_algorithm_mat[2,2]) / (c5_algorithm_mat[1] + c5_algorithm_mat[1,2] + c5_algorithm_mat[2,1] + c5_algorithm_mat[2,2])*100, 1)
fc5 <- specify_decimal((as.double(pc5) * as.double(rc5)) /(as.double(pc5)+as.double(rc5)),1)

#Precision
message("Precision: ", pc5, " %")
#Recall
message("Recall: ",rc5, " %")
#Accuracy
message("Accuracy: ",ac5 , " %")
#F
message("F: ",fc5, " %")



########## C5.0 with Caret #########

#Create model
c5.0_mod <- train(Riskdrinker ~ ., data = learn, method = "C5.0")

# See the results
c5.0_mod

#plot model
plot(c5.0_mod$finalModel)
plot(c5.0_mod, log=x)
plot_treeimb <- fancyRpartPlot(c5.0_mod$finalModel,main="Decision Tree for Riskdrinkers", tweak=1.5)

#ROC
c.50_pred <- predict(c5.0_mod, testing, type = "prob")
c50_predic = prediction(c.50_pred[,2], testing$Riskdrinker)
plot(performance(c50_predic, "tpr", "fpr"))
abline(0, 1, lty = 2)


#Summaries of C5.0 Model
summary(c5.0_mod$finalModel)


#Variable importance
varImp(c5.0_mod$finalModel)
varImp(c5.0_mod)

# variable Importance
C5imp(c5.0_mod$finalModel, metric="usage") #metric = Usage or splits.

#Prediction

#Manual metrics
c5.0_mod_pred_manual <- predict(c5.0_mod, newdata = testing)

#ROSE
c5.0_mod_pred_rose <- predict(c5.0_mod, newdata = testing, type ="prob")

#Summary of the predictions for ROSE
meas(testing$Riskdrinker, c5.0_mod_pred_rose[,2])

#Manual table
#Make a matrix
c5.0_mod_table <- table(c5.0_mod_pred_manual, testing$Riskdrinker)
c5.0_mod_table
#Summary and plots the confusion matrix
c5.0_mod_table_confusion = confusionMatrix(c5.0_mod_table)
#Print summary_Cm_pruned
print(c5.0_mod_table_confusion)


#Calculations

pre <- specify_decimal(c5.0_mod_table[1,1] / (c5.0_mod_table[1,1] + c5.0_mod_table[2,1])*100, 1)
rec <- specify_decimal(c5.0_mod_table[1] / (c5.0_mod_table[1] + c5.0_mod_table[1,2])*100, 1)

acc <- specify_decimal((c5.0_mod_table[1]+c5.0_mod_table[2,2]) / (c5.0_mod_table[1] + c5.0_mod_table[1,2] + c5.0_mod_table[2,1] + c5.0_mod_table[2,2])*100, 1)
f11 <- specify_decimal((as.double(pre) * as.double(rec)) /(as.double(pre)+as.double(rec)),1)

#Precision
message("Precision: ", pre, " %")
#Recall
message("Recall: ",rec, " %")
#Accuracy
message("Accuracy: ",acc , " %")
#F
message("F: ",f11, " %")


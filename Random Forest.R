###### Script for analyzing the data with decision trees #####

# Date: 2017-05-11
# Author: Sokrates Lamprou
# LiU-ID: andla830@student.liu.se
# Course: 729G40: Kandidatuppsats
# University: Linköping University

#Libraries

library(party)
library(randomForest)
library(caret)

set.seed(42)

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
  message("TN: ", TN)
  message("FP: ", FP)
  message("TP: ", TP)
  message("FN: ", FN)
  
  ACCURACY <- (TP+TN) / (TP + TN + FP + FN)
  PRECISION <- TP/(TP+FP)
  RECALL <- TP/(TP+FN)
  F <- RECALL*PRECISION/(RECALL+PRECISION)
  
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


################## Dividing and preparing data ############

s = sample(957,200) #477 är hälften, 191 = 20%, 300  ~ 32%
training = data[-s,] 
test = data[s,] 
message("Size of training: ", dim(training))
message("Size of test: ", dim(test))
# Show the number of positive vs negative in test versus training dataset. 
table(test$Riskdrinker)
table(training$Riskdrinker)
View(test)
################## Dividing and preparing data #####

#Make the dataset to factors (for training)
View(training)
View(factor_test)

factor_train <- data.frame(lapply(training, as.factor), stringsAsFactors=TRUE)
factor_test <- data.frame(lapply(test, as.factor), stringsAsFactors=TRUE)

table(factor_test$Riskdrinker)
table(factor_train$Riskdrinker)

#Preparing the dataframes (for testing)

learn <- data.frame(training)
testing <- data.frame(test)


################## Random Forest model #############

rf_model <- randomForest(Riskdrinker~., 
                      data = factor_train, 
                      proximity=TRUE,
                      ntree=1000,
                      parms = list(split = "information"))





#importance=TRUE,
#keep.forest=TRUE,
#keep.forest=TRUE,
#proximity=TRUE,

#Variable importance plot
varImpPlot(rf_model)

rf_model



MDSplot(rf_model, factor_test$Riskdrinker)
MDSplot(rf_model,  factor_test$Riskdrinker, palette=rep(1, 3), pch=as.numeric( factor_test$Riskdrinker))
hist(treesize(rf_model))

hist(factor_train$Riskdrinker)

help()

plot(rf_model)
plot(rf_model, log="y")

#AUC value
library(caTools)

auc = colAUC(rf_auc, factor_test$Riskdrinker)
auc
help(colAUC)
#
yhat<-predict(rf_model, factor_test, type="prob")
yhat1<-prediction(yhat[,2], factor_test$Riskdrinker)
auc<-performance(yhat1,"auc")
slot(auc,"y.values")



rf_auc <- predict(rf_model, factor_test, type = "prob")
rf_auc_pred <- prediction(rf_auc[,2], factor_test$Riskdrinker)
#AUC VALUE
rf_auc1 <- performance(rf_auc_pred, "auc")
rf_auc1 <- rf_auc1@y.values[[1]]
rf_auc1

#ROC
perf_ROC_rf=performance(rf_auc_pred,"tpr","fpr") #plot the actual ROC curve
plot(perf_ROC_rf, main="ROC plot")
text(0.5,0.5,paste("AUC = ",format(AUC, digits=5, scientific=FALSE)))

rf_prob <- predict(rf_model, newdata=factor_test)
rf_prob_pred <- prediction(rf_prob, factor_test$Riskdrinker)
pdt_performance <- performance(pdt_prob_pred, measure = "tpr", x.measure = "fpr") #x.measure, or y.measure
plot(pdt_performance)




#Factor all the variables. Stupid bugs...
common <- intersect(names(factor_train), names(factor_test))

for (p in common) { 
  if (class(factor_train[[p]]) == "factor") { 
    levels(factor_test[[p]]) <- levels(factor_train[[p]]) 
  } 
}
#Factor all the variables. Stupid bugs...


#Manual metrics
rf_model_manual <- predict(rf_model, newdata = factor_test)

#ROSE
rf_model_rose <- predict(rf_model, newdata = factor_test, type ="prob")

#Summary of the predictions for ROSE
meas(factor_test$Riskdrinker, rf_model_rose[,2])


#Make a matrix
rf_mat <- table(rf_model_manual, factor_test$Riskdrinker)

#Plot matrix

#Summary and plots the confusion matrix
rf_mat_confusion = confusionMatrix(rf_mat)
#Print summary_Cm_pruned
print(rf_mat_confusion)


#Evaluation#
############

#Calculations

rf_p1 <- specify_decimal(rf_mat[1,1] / (rf_mat[1,1] + rf_mat[2,1])*100, 2)
rf_r1 <- specify_decimal(rf_mat[1] / (rf_mat[1] + rf_mat[1,2])*100, 2)

rf_a1 <- specify_decimal((rf_mat[1]+rf_mat[2,2]) / (rf_mat[1] + rf_mat[1,2] + rf_mat[2,1] + rf_mat[2,2])*100, 2)
rf_f1 <- specify_decimal((as.double(rf_p1) * as.double(rf_r1)) /(as.double(rf_p1)+as.double(rf_r1)),2)

#Precision
message("Precision: ", rf_p1, " %")
#Recall
message("Recall: ",rf_r1, " %")
#Accuracy
message("Accuracy: ",rf_a1 , " %")
#F
message("F: ",rf_f1, " %")


############# Carets Random Forest model #################

#Cross validation with/without repeats, method = repeatedcv/cv. 
#method="rf"/"C5.0"
cv10_3 <- trainControl(method = "repeatedcv", number =10, repeats =3, classProbs = TRUE)
cv10 <- trainControl(method = "cv", number =10, classProbs = TRUE)


random_forest <- train(Riskdrinker ~., learn, method = "rf", metric = "ROC",ntree=1000,
               tunelength = 10, trControl = cv10_3)

#Summary parms = list(split = "information"),
print(random_forest)
random_forest


#Variable importance
varImp(random_forest)

plot(random_forest) #no log.



#Manual metrics
random_forest_man <- predict(random_forest, newdata = testing)

#ROSE
random_forest_rose <- predict(random_forest, newdata = testing, type ="prob")

#Summary of the predictions for ROSE
meas(testing$Riskdrinker, random_forest_rose[,2])


#Make a matrix
random_forest_mat <- table(random_forest_man, testing$Riskdrinker)
random_forest_mat
#Plot matrix

#Summary and plots the confusion matrix
random_forest_con_mat = confusionMatrix(random_forest_mat)
#Print summary_Cm_pruned
print(random_forest_con_mat)


#Evaluation#
############

#Calculations

random_p <- specify_decimal(random_forest_mat[1,1] / (random_forest_mat[1,1] + random_forest_mat[2,1])*100, 1)
random_r <- specify_decimal(random_forest_mat[1,1] / (random_forest_mat[1,1] + random_forest_mat[1,2])*100, 1)

random_a <- specify_decimal((random_forest_mat[1,1]+random_forest_mat[2,2]) / (random_forest_mat[1] + random_forest_mat[1,2] + random_forest_mat[2,1] + random_forest_mat[2,2])*100, 1)
random_f <- specify_decimal(as.double(random_p) * as.double(random_r) /(as.double(random_p)+as.double(random_r)),1)

#Precision
message("Precision: ", random_p, " %")
#Recall
message("Recall: ",random_r, " %")
#Accuracy
message("Accuracy: ",random_a , " %")
#F
message("F: ",random_f, " %")



############# Carets Random Forest model #################


############# Cforest Random Forest model #################



control_cf=cforest_unbiased(ntree=500, mtry=3)
fit <- cforest(Riskdrinker~., data = factor_train, control = control_cf)

############# Cforest Random Forest model #################

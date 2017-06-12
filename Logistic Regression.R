###### Script for analyzing the data with decision trees #####

# Date: 2017-05-12
# Author: Sokrates Lamprou
# LiU-ID: andla830@student.liu.se
# Course: 729G40: Kandidatuppsats
# University: Linköping University

#Libraries

library(vcd)
install.packages("ICC", repos="http://R-Forge.R-project.org")
install.packages("fmsb")

library(ICC)
library(fmsb)
library(ROCR)

library(ISLR)

1 - pchisq(837.37, 756)
1 - pchisq(400.72, 713)
1 - pchisq(0.95, 713)

1 - pchisq(837.37 - 400.72, 756 - 713)



library(RColorBrewer)
library(pscl)
library(caret)
library(boot)
library(rattle)
install.packages("apaTables", dep=T)
library(apaTables)

install.packages("apaStyle",repos = c("http://rstudio.org/_packages","http://cran.rstudio.com"))
library(apaStyle)
install.packages("apaStyle")
install.packages('apaTables_1.5.0', lib='C:/Users/Adde/AppData/Local/Temp/RtmpAtr8Lj/downloaded_packages')


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

################# Oversampling the data ###################
library(DMwR)
smote_data <- data.frame(lapply(data, as.factor), stringsAsFactors=TRUE)


newData <- SMOTE(Riskdrinker~ ., smote_data,perc.over = 150, perc.under = 400)
table(newData$Riskdrinker)

################# Oversampling the data ###################
################# Replace "Yes" with 1 and "No" with 0. ###################

data_regression = data
library(plyr)
data_regression$Riskdrinker <- revalue(data_regression$Riskdrinker, c("Ja"=1))
data_regression$Riskdrinker <- revalue(data_regression$Riskdrinker, c("Nej"=0))
View(data_regression)
table(data_regression$Riskdrinker)
table(data$Riskdrinker)


View(log_test)
View(training)



################## Dividing and preparing data ############
data = log_data

set.seed(42)
s = sample(957,200) #477 är hälften, 191 = 20%, 300  ~ 32%
log_training = data[-s,] 
log_test = data[s,] 
message("Size of training: ", dim(training))
message("Size of test: ", dim(test))
# Show the number of positive vs negative in test versus training dataset. 
table(test$Riskdrinker)
table(training$Riskdrinker)

#Preparing the dataframes

learn <- data.frame(training)
testing <- data.frame(test)
View(test)

table(learn$Riskdrinker)
table(testing$Riskdrinker)

################## Dividing and preparing data #####

library(plyr)
training = read.table("training.txt")
test = read.table("test_tab.txt")
train_log <- training
test_log <- test

training$Riskdrinker <- revalue(training$Riskdrinker, c("Ja"=1))
training$Riskdrinker  <- revalue(training$Riskdrinker, c("Nej"=0))
head(training$Riskdrinker)

test$Riskdrinker <- revalue(test$Riskdrinker, c("Ja"=1))
test$Riskdrinker  <- revalue(test$Riskdrinker, c("Nej"=0))
head(test$Riskdrinker)

View(train_log)

factor_train_log <- data.frame(lapply(train_log, as.factor), stringsAsFactors=TRUE)
factor_test_log <- data.frame(lapply(test_log, as.factor), stringsAsFactors=TRUE)


################## Logistic Regression GLM Model - 1 #####

glm_model <- glm(as.factor(Riskdrinker) ~.,family=binomial(link='logit'),data=training)
apa.aov.table(anova, anova_table)

apa.reg.table(glm_model)

apa.cor.table(anova, table.number=10)


apa.reg.table(anova, filename="Table_Summary_APA.doc", table.number=2)

warnings()

View(train_log)

aja = cv.glm(train_log, glm_model, K=10)
summary(glm_model)
anova = anova(glm_model, test="Chisq")


summary(glm_model)
# Plotting a lot of stuffs ^^ 
plot(glm_model)

logLik(glm_model)

#Anova Chisq
anova(glm_model, test="Chisq")
chisq.test(glm_model)

#
exp(coefficients(glm_model))
ci<-confint(glm_model)
exp(ci)

#Nagelkerkes förklaringsgrad
NagelkerkeR2(glm_model)
require(fmsb)
NagelkerkeR2(glm_model)


#pR2  # look for 'McFadden'
pR2(glm_model)

#Check variable importance.
varImps <- varImp(glm_model)
varImps

#ROC
log_pred <- predict(glm_model, factor_test, type = "terms")
log_pred_roc = prediction(log_pred[,2], factor_test$Riskdrinker)
plot(performance(log_pred_roc, "tpr", "fpr"))
abline(0, 1, lty = 2)

#ROC
library(pROC)

f1 = roc(Riskdrinker ~ fHealth, data=training) 
plot(f1, col="red")

#
# Compute AUC for predicting Class with the model -> ROC CURVE!!!
probi <- predict(glm_model, newdata=test_log, type="response")
pred_probi <- prediction(probi, test_log$Riskdrinker)
perf_pred_probi <- performance(pred_probi, measure = "tpr", x.measure = "fpr") #x.measure, or y.measure
plot(perf_pred_probi)
#

#AUC
auc1 = performance(pred_probi, measure = "auc")
auc2 <- auc1@y.values[[1]]
auc2
auc = colAUC(probi, test_log$Riskdrinker)
auc

rf_auc <- predict(glm_model, test_log, type = "prob")



#Predictions
test.probs <-predict(glm_model, test_log, type='response')
pred.logit <- rep('Ja',length(test.probs))
pred.logit[test.probs>=0.5] <- 'Nej'
test.probs


glm_model_pred = predict(glm_model, test_log)
glm_model_pred

library(ggplot2)
library(plotROC)




glm_model_table = table(test_log$Riskdrinker, glm_model_pred) # Försök fixa en matrix för logistisk regression
glm_model_table
str(factor_test_log$Riskdrinker)
str(glm_model_pred)


#Summary and plots the confusion matrix
glm_model_table_confusion = confusionMatrix(glm_model_pred, test_log)
str(glm_model_pred)
str(test_log)
glm_frame <- data.frame(glm_model_pred)

#Print summary_Cm_pruned
print(glm_model_table_confusion)



#Make a prediction of the test test (for ROSE PACKAGE)
glm_model_pred_rose <- predict(glm_model, newdata = test_log)

#Summary of the predictions
glm_model_pred_rose_meas <- meas(test_log$Riskdrinker, glm_model_pred_rose) #[,2]
glm_model_pred_rose_meas

#Make a matrix
glm_frame = data.frame(glm_model_table[,2])
glm_frame




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






################## Logistic Regression GLM Model - 1 #####




################## Logistic Regression Caret Model - 1 ##### Cross validation

#Logistic Regression Model

ctrl_repet <- trainControl(method = "repeatedcv", number = 10, repeats = 3, savePredictions = TRUE)
ctrl <- trainControl(method = "cv", number = 10, savePredictions = TRUE)


mod_fit_cross_val <- train(as.factor(Riskdrinker)~., data=factor_train_log, method="glm",
                   family="binomial",trControl = ctrl, tuneLength = 5)
warnings()


#Summary
summary(mod_fit_cross_val)

#Plot not working
plot(mod_fit_cross_val)


#Variable importance
varImp(mod_fit_cross_val)

# Compute AUC for predicting Class with the model -> ROC CURVE!!!

mod_fit_cross_val_auc <- predict(mod_fit_cross_val, newdata=factor_test_log, type="prob")
mod_fit_cross_val_auc_pred <- prediction(mod_fit_cross_val_auc[,2], factor_test_log$Riskdrinker) #[,2]
perf_pred_probi <- performance(mod_fit_cross_val_auc_pred, measure = "tpr", x.measure = "fpr") #x.measure, or y.measure
plot(perf_pred_probi)


#Make a prediction of the test test (for ROSE PACKAGE)
cv_mod_fit <- predict(mod_fit_cross_val, newdata = factor_test_log)

#Summary of the predictions
glm_model_cv_rose_pred <- meas(factor_test_log$Riskdrinker, cv_mod_fit) #[,2]
glm_model_pred_rose_meas


# Predictions
#pred2 = predict(mod_fit_cross_val, newdata=factor_test_log, type="prob")
#pred2

#Predictions for table:
predictions = predict(mod_fit_cross_val, newdata=factor_test_log)

#Make table
pred1_table = table(predictions, factor_test_log$Riskdrinker)
pred1_table

#Summary and plots the confusion matrix
pred1_table_confusion = confusionMatrix(pred1_table)
pred1_table_confusion = confusionMatrix(predictions,factor_test_log$Riskdrinker)
#Print summary_Cm_pruned
print(pred1_table_confusion)


prec_l <- specify_decimal(pred1_table[1,1] / (pred1_table[1,1] + pred1_table[2,1])*100, 2)
rec_l <- specify_decimal(pred1_table[1] / (pred1_table[1] + pred1_table[1,2])*100, 2)

acc_l <- specify_decimal((pred1_table[1]+pred1_table[2,2]) / (pred1_table[1] + pred1_table[1,2] + pred1_table[2,1] + pred1_table[2,2])*100, 2)
f_l <- specify_decimal((as.double(prec_l) * as.double(rec_l)) /(as.double(prec_l)+as.double(rec_l)),2)

#Precision
message("Precision: ", prec_l, " %")
#Recall
message("Recall: ",rec_l, " %")
#Accuracy
message("Accuracy: ",acc_l , " %")
#F
message("F: ",f_l, " %")

################## Logistic Regression Caret Model - 1 #####


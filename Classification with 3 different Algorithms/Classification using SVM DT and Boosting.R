
# set directory
setwd("E:/AML - BUAN 6341")

# install necessary packages if missing
if(!require("DMwR")){
  cat(" \n DMwR package not found.. Hence installing...")
  install.packages("DMwR")
}

if(!require("caret")){
  cat(" \n caret package not found.. Hence installing...")
  install.packages("caret")
}

library(DMwR) #for SMOTE
library(caret)
library(caTools)

# load dataset
dt1 <- read.csv("adult.csv", header = TRUE)
dt2 <- read.csv("HR.csv", header = TRUE)

# Data Cleanup for dt1 Dataset
#remove unnecessary column
dt1$education.num <- NULL

# update missing values with NA
dt1[dt1 == '?'] <- NA
#omit missing value
dt1 <- na.omit(dt1)

dt1$income<-ifelse(dt1$income=='>50K',1,0)
dt1$workclass<-ifelse(dt1$workclass=='?','Unknown',as.character(dt1$workclass))
colnames(dt1)[13]<-'Country'
colnames(dt1)[11]<-'CapitalLoss'
colnames(dt1)[12]<-'Hours'
colnames(dt1)[5]<-'Marital'
colnames(dt1)[10]<-'CapitalGain'

#convert categorical data in nummerical equivalent
cato<-c('education','Marital','occupation','relationship','race','sex','Country')
for(each in cato){
  dt1[,each]<-as.integer(dt1[,each])
}

class<-unique(dt1$workclass)
for(i in 1:length(dt1$age)){
  dt1$workclass[i]<-which(class %in% dt1$workclass[i])
}
dt1$workclass<-as.numeric(dt1$workclass)
#factored output
dt1$income<-as.factor(as.character(dt1$income))

# removing class imbalance from both datasets
dt1_smoted <- SMOTE(income~.,data = dt1, k=5, perc.over = 300,perc.under = 150)

dt1_smoted[,sapply(dt1_smoted, is.numeric)] <-round(dt1_smoted[,sapply(dt1_smoted, is.numeric)],0)
write.csv(file="adult_smoted.csv",dt1_smoted)

#Data Cleanup for HR dataset
#convert categorical data in nummerical equivalent
cato_dt2<-c("Department", "salary")
for(each in cato_dt2){
  dt2[,each]<-as.integer(dt2[,each])
}

dt2$left <- as.factor(as.character(dt2$left))
dt2_smoted <- SMOTE(left~., data = dt2, k=5, perc.over = 300,perc.under = 150)

# rounding off to integer values
dt2_smoted$number_project <-trunc(dt2_smoted$number_project)
dt2_smoted$average_montly_hours <-trunc(dt2_smoted$average_montly_hours)
dt2_smoted$time_spend_company <-trunc(dt2_smoted$time_spend_company)
dt2_smoted$Work_accident <-trunc(dt2_smoted$Work_accident)
dt2_smoted$promotion_last_5years <-trunc(dt2_smoted$promotion_last_5years)
dt2_smoted$Department <-trunc(dt2_smoted$Department)
dt2_smoted$salary <-trunc(dt2_smoted$salary)
write.csv(file="HR_smoted.csv",dt2_smoted)

# Split the data into training and test
set.seed(1000)
intrain_dt1 <- createDataPartition(y = dt1_smoted$income, p= 0.7, list = FALSE)
train_dt1 <- dt1_smoted[intrain_dt1,]
test_dt1 <- dt1_smoted[-intrain_dt1,]

# Split the data into training and test
set.seed(1000)
intrain_dt2 <- createDataPartition(y = dt2_smoted$left, p= 0.7, list = FALSE)
train_dt2 <- dt2_smoted[intrain_dt2,]
test_dt2 <- dt2_smoted[-intrain_dt2,]

cat(" \n data preparation done")
#############################################################################
# SVM
#############################################################################
if(!require("e1071")){
  cat(" \n e1071 package not found.. Hence installing...")
  install.packages("e1071")
}
library(e1071)  #for SVM
cat(" \n svm started for census dataset")
#svm with kernal linear, polynomial, sigmoid for adult census
svm_lnr_model <-  svm(income~ ., data=train_dt1, kernel = "linear")
svm_pol_model_adult <-  svm(income~ ., data=train_dt1, kernel = "polynomial") 
svm_sgmd_model <- svm(income~ ., data=train_dt1, kernel = "sigmoid") 

pred_train1 <-predict(svm_lnr_model,train_dt1)
cat('\n svm model with linear kernal accuracy: ', mean(pred_train1==train_dt1$income))

pred_train2 <-predict(svm_pol_model_adult,train_dt1)
cat('\n svm model with linear polynomial accuracy: ', mean(pred_train2==train_dt1$income))

pred_train3 <-predict(svm_sgmd_model,train_dt1)
cat(' \n svm model with linear sigmoid accuracy: ', mean(pred_train3==train_dt1$income))

#test set predictions
pred_test_adult <-predict(svm_pol_model_adult,test_dt1)

print('confusion Matrix for Train for Adult Census:')
print(confusionMatrix(train_dt1$income, pred_train2))
adult_svm_accuracy_tr <- mean(pred_train2==train_dt1$income)
cat(' \n Train model accuracy for Adult Census: ' ,adult_svm_accuracy_tr)

print('confusion Matrix for test for Adult Census:')
print(confusionMatrix(test_dt1$income, pred_test_adult))
adult_svm_accuracy_te <- mean(pred_test_adult==test_dt1$income)
cat(' \n Test model accuracy for Adult Census: ' ,adult_svm_accuracy_te)


pred_svm_adult <- prediction(predictions = as.numeric(pred_test_adult),labels = as.numeric(test_dt1$income))
perf_svm_adult <- performance(pred_svm_adult, "tpr", "fpr")
auc_svm_adult <- performance(pred_svm_adult,"auc")
auc_svm_adult <- round(as.numeric(auc_svm_adult@y.values),3)
print(paste('AUC of Support Vector Machine for Adult:',auc_svm_adult))

cat(" \n svm started for HR dataset")

#svm with kernal linear, polynomial, sigmoid for HR
svm_lnr_model <-  svm(left~., data = train_dt2, kernel = "linear") 
svm_pol_model <-  svm(left~., data = train_dt2, kernel = "polynomial") 
svm_sgmd_model <- svm(left~., data = train_dt2, kernel = "sigmoid") 

pred_train1 <-predict(svm_lnr_model,train_dt2)
cat('\n svm model with linear kernal accuracy: ', mean(pred_train1==train_dt2$left))

pred_train2 <-predict(svm_pol_model,train_dt2)
cat(' \n svm model with linear polynomial accuracy: ', mean(pred_train2==train_dt2$left))

pred_train3 <-predict(svm_sgmd_model,train_dt2)
cat(' \n svm model with linear sigmoid accuracy: ', mean(pred_train3==train_dt2$left))

#test set predictions
pred_test <- predict(svm_pol_model,test_dt2, type="response")
pred_svm_hr <- prediction(predictions = as.numeric(pred_test),labels = as.numeric(test_dt2$left))
perf_svm_hr <- performance(pred_svm_hr, "tpr", "fpr")
auc_svm_hr <- performance(pred_svm_hr,"auc")
auc_svm_hr <- round(as.numeric(auc_svm_hr@y.values),3)
print(paste('AUC of Support Vector Machine for HR:',auc_svm_hr))

print('confusion Matrix for train for HR:')
print(confusionMatrix(train_dt2$left, pred_train2))
hr_svm_accuracy_tr <- mean(pred_train2==train_dt2$left)
cat(' \n Train model accuracy for HR: ' ,hr_svm_accuracy_tr)

print('confusion Matrix for test for HR:')
print(confusionMatrix(test_dt2$left, pred_test))
hr_svm_accuracy <- mean(pred_test==test_dt2$left)
cat(' \n Test model accuracy for HR: ' ,hr_svm_accuracy)

cat(" \n svm done")
#############################################################################
# Decision Tree
#############################################################################
cat(" \n Decision Tree started")
if(!require("rpart")){
  cat(" \n rpart package not found.. Hence installing...")
  install.packages("rpart")
}

if(!require("rattle")){
  cat(" \n rattle package not found.. Hence installing...")
  install.packages("rattle")
}
library(rattle)
library(rpart)
library(rpart.plot)

#for adult census

treeFit_dt1<- rpart(income~.,data=train_dt1,parms =  list(split = 'gini') ,method = 'class')
prd_dt1 <- predict(treeFit_dt1, test_dt1, type="class")
accurracy_dt1_gini <- sum(test_dt1$income==prd_dt1)/length(prd_dt1)
cat("\n Accuracy of model for Adult Census with split Gini: " ,accurracy_dt1_gini)

treeFit_dt1<- rpart(income~.,data=train_dt1,parms =  list(split = 'information') ,method = 'class')
prd_dt1 <- predict(treeFit_dt1, test_dt1, type="class")
accurracy_dt1_inf <- sum(test_dt1$income==prd_dt1)/length(prd_dt1)
cat("\n Accuracy of model for Adult Census with split information: " ,accurracy_dt1_inf)
cat(" \n Accuracy is higher with information split")
plotcp(treeFit_dt1)
#print(asRules(treeFit_dt1))
fancyRpartPlot(treeFit_dt1, main="Decision Tree for income prediction")

cat(" \n prune the tree to different CP")
pfit_td1<- prune(treeFit_dt1, cp= 0.011)
prd_dt1 <- predict(pfit_td1, test_dt1, type="class")
accurracy_dt1_1 <- sum(test_dt1$income==prd_dt1)/length(prd_dt1)
cat("\n Accuracy for CP: 0.011 is ", accurracy_dt1_1)

pfit_td1<- prune(treeFit_dt1, cp= 0.001)
prd_dt1 <- predict(pfit_td1, test_dt1, type="class")
accurracy_dt1_2 <- sum(test_dt1$income==prd_dt1)/length(prd_dt1)
cat("\n Accuracy for CP: 0.001 is ", accurracy_dt1_2)

pfit_td1<- prune(treeFit_dt1, cp= treeFit_dt1$cptable[which.min(treeFit_dt1$cptable[,"xerror"]),"CP"])
prd_dt1 <- predict(pfit_td1, test_dt1, type="class")
accurracy_dt1_3 <- sum(test_dt1$income==prd_dt1)/length(prd_dt1)
cat("\n Accuracy for CP  ", treeFit_dt1$cptable[which.min(treeFit_dt1$cptable[,"xerror"]),"CP"])
print(accurracy_dt1_3)

cat("\n The Confusion Matrix: \n")
print(confusionMatrix(test_dt1$income, prd_dt1))

cat("\n Test Model accuracy for Adult Census", accurracy_dt1_3)

# Evaluate model performance. 
# ROC Curve: requires the ROCR package.
if(!require("ROCR")){
  cat(" \n ROCR package not found.. Hence installing...")
  install.packages("ROCR")
}
library(ROCR)


# ROC Curve: requires the ggplot2 package.
if(!require("ggplot2")){
  cat(" \n ggplot2 package not found.. Hence installing...")
  install.packages("ggplot2")
}
library(ggplot2, quietly=TRUE)
cat(" \n Generate an ROC Curve for the rpart model on test dataset.")

dt1pr <- predict(treeFit_dt1, newdata=test_dt1)[,2]
no.miss1   <- na.omit(test_dt1$income)
miss.list1 <- attr(no.miss1, "na.action")
attributes(no.miss1) <- NULL

if (length(miss.list1))
{
  pred <- prediction(dt1pr[-miss.list1], no.miss1)
} else
{
  pred <- prediction(dt1pr, no.miss1)
}
pe <- performance(pred, "tpr", "fpr")
au <- performance(pred, "auc")@y.values[[1]]
pd <- data.frame(fpr=unlist(pe@x.values), tpr=unlist(pe@y.values))
p <- ggplot(pd, aes(x=fpr, y=tpr))
p <- p + geom_line(colour="red")
p <- p + xlab("False Positive Rate") + ylab("True Positive Rate")
p <- p + ggtitle("ROC Curve Decision Tree for adult Census: Test income")
p <- p + theme(plot.title=element_text(size=10))
p <- p + geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey")
p <- p + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
                  label=paste("AUC =", round(au, 2)))
print(p)

# Calculate the area under the curve for the plot.
pred <- prediction(dt1pr, no.miss1)
performance(pred, "auc")

#for HR
treeFit_dt2<- rpart(left~.,data=train_dt2,method = 'class', parms = list(split = 'information'))
prd_dt2 <- predict(treeFit_dt2, test_dt2, type="class")
accurracy_dt2_inf <- sum(test_dt2$left==prd_dt2)/length(prd_dt2)
cat("\n Accuracy of model for HR with split Information: " ,accurracy_dt2_inf)

treeFit_dt2<- rpart(left~.,data=train_dt2,method = 'class', parms = list(split = 'gini'))
#print(treeFit_dt2)
plotcp(treeFit_dt2)
prd_dt2 <- predict(treeFit_dt2, test_dt2, type="class")
accurracy_dt2_gini <- sum(test_dt2$left==prd_dt2)/length(prd_dt2)
cat("\n Accuracy of model for HR with split Gini: " ,accurracy_dt2_gini)

fancyRpartPlot(treeFit_dt2, main="Decision Tree for Employee left check")
plot(treeFit_dt2, uniform=TRUE,main="Decision Tree: HR ")
text(treeFit_dt2, use.n=TRUE, all=TRUE, cex= 0.7)
#print(asRules(treeFit_dt2))

# prune the tree to different CP
pfit_td2<- prune(treeFit_dt2, cp= 0.028)
prd_dt2 <- predict(pfit_td2, test_dt2, type="class")
accurracy_dt2_1 <- sum(test_dt2$left==prd_dt2)/length(prd_dt2)
cat("\n Accuracy for CP: 0.028 is ", accurracy_dt2_1)

pfit_td2<- prune(treeFit_dt2, cp= 0.015)
prd_dt2 <- predict(pfit_td2, test_dt2, type="class")
accurracy_dt2_2 <- sum(test_dt2$left==prd_dt2)/length(prd_dt2)
cat("\n Accuracy for CP: 0.015 is ", accurracy_dt2_2)

pfit_td2<- prune(treeFit_dt2, cp= treeFit_dt2$cptable[which.min(treeFit_dt2$cptable[,"xerror"]),"CP"])
prd_dt2 <- predict(pfit_td2, test_dt2, type="class")
accurracy_dt2_3 <- sum(test_dt2$left==prd_dt2)/length(prd_dt2)
cat("\n Accuracy for CP:  ", treeFit_dt2$cptable[which.min(treeFit_dt2$cptable[,"xerror"]),"CP"])
print(accurracy_dt2_3)

cat("\n The Confusion Matrix: \n")
print(confusionMatrix(test_dt2$left, prd_dt2))

dt2pr <- predict(treeFit_dt2, newdata=test_dt2)[,2]
no.miss2   <- na.omit(test_dt2$left)
miss.list2 <- attr(no.miss2, "na.action")
attributes(no.miss2) <- NULL

if (length(miss.list2))
{
  pred2 <- prediction(dt2pr[-miss.list2], no.miss2)
} else
{
  pred2 <- prediction(dt2pr, no.miss2)
}
pe2 <- performance(pred2, "tpr", "fpr")
au2 <- performance(pred2, "auc")@y.values[[1]]
pd2 <- data.frame(fpr=unlist(pe2@x.values), tpr=unlist(pe2@y.values))
q <- ggplot(pd2, aes(x=fpr, y=tpr))
q <- q + geom_line(colour="red")
q <- q + xlab("False Positive Rate") + ylab("True Positive Rate")
q <- q + ggtitle("ROC Curve Decision Tree for Employee lefts: Test")
q <- q + theme(plot.title=element_text(size=10))
q <- q + geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey")
q <- q + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
                  label=paste("AUC =", round(au2, 2)))
print(q)


# Calculate the area under the curve for the plot.
pred2 <- prediction(dt2pr, no.miss2)
cat('\n AUC for HR Test: ', round(au2, 2))
performance(pred2, "auc")

cat(" \n Decision Tree Done")
#############################################################################
# Boosting
#############################################################################

cat(" \n boosting started")

if(!require("h2o")){
  cat(" \n h2o package not found.. Hence installing...")
  install.packages("h2o")
}
library(h2o)
h2o.init(nthreads = -1, max_mem_size = "2G")
h2o.no_progress()

## For Adult census dataset
df <- h2o.importFile(path = "adult_smoted.csv")
df['C1'] <- NULL

## pick a response for the supervised problem
response_adult <- "income"

## the response variable is an integer, convert it into a categorical/factor for binary classification
df[[response_adult]] <- as.factor(df[[response_adult]])           

## use all other columns (except for the name) as predictors
predictors_adult <- setdiff(names(df), c(response_adult, "name")) 


splits_adult <- h2o.splitFrame(
  data = df, 
  ratios = c(0.6,0.2),   ## only need to specify 2 fractions, the 3rd is implied
  destination_frames = c("train.hex", "valid.hex", "test.hex"), seed = 1234
)
train_adult <- splits_adult[[1]]
valid_adult <- splits_adult[[2]]
test_adult  <- splits_adult[[3]]

hyper_params_adult = list( max_depth = c(4,6,8,12,16,20) ) 

grid <- h2o.grid(
  ## hyper parameters
  hyper_params = hyper_params_adult,
  
  ## full Cartesian hyper-parameter search
  search_criteria = list(strategy = "Cartesian"),
  
  ## which algorithm to run
  algorithm="gbm",
  
  ## identifier for the grid, to later retrieve it
  grid_id="depth_grid",
  
  ## standard model parameters
  x = predictors_adult, 
  y = response_adult, 
  training_frame = train_adult, 
  validation_frame = valid_adult,
  
  ## more trees is better if the learning rate is small enough 
  ## here, use "more than enough" trees - we have early stopping
  ntrees = 10000,                                                            
  
  ## smaller learning rate is better
  ## since we have learning_rate_annealing, we can afford to start with a bigger learning rate
  learn_rate = 0.05,                                                         
  
  ## learning rate annealing: learning_rate shrinks by 1% after every tree 
  ## (use 1.00 to disable, but then lower the learning_rate)
  learn_rate_annealing = 0.99,                                               
  
  ## sample 80% of rows per tree
  sample_rate = 0.8,                                                       
  
  ## sample 80% of columns per split
  col_sample_rate = 0.8, 
  
  ## fix a random number generator seed for reproducibility
  seed = 1234,                                                             
  
  ## early stopping once the validation AUC doesn't improve by at least 0.01% for 5 consecutive scoring events
  stopping_rounds = 5,
  stopping_tolerance = 1e-4,
  stopping_metric = "AUC", 
  
  ## score every 10 trees to make early stopping reproducible (it depends on the scoring interval)
  score_tree_interval = 10                                                
)

## sort the grid models by decreasing AUC
sortedGrid_adult <- h2o.getGrid("depth_grid", sort_by="auc", decreasing = TRUE)    
#Sorted Grid will show AUC factor for model under different depths for tree
#This will help to understand impact of the depth of Tree on ROC
print('Grid with models with different tree depths and their respective ROC ordered ROC desc')
print(sortedGrid_adult)

cat(" \n get the model with max accuracy") 
gbm_adult <- h2o.getModel(sortedGrid_adult@model_ids[[1]])
cat("\n Model Summary: Train & Valid: Adult Census \n")
print(summary(gbm_adult))
gbm_adult_roc_valid <-h2o.auc(h2o.performance(gbm_adult, newdata = valid_adult))
gbm_accuracy_adult_valid <- gbm_adult@model$validation_metrics@metrics$max_criteria_and_metric_scores[4,3]
cat(" \n validation accuracy: ", gbm_accuracy_adult_valid)
## Model accuracy for test dataset
boost_adult_roc <- h2o.performance(gbm_adult, newdata = test_adult)
cat("Test Model: Adult Census: Summary \n")
print(boost_adult_roc)
gbm_roc_adult_test <- h2o.auc(h2o.performance(gbm_adult, newdata = test_adult))
cat("\n Accuracy for Test GMB: Adult Dataset")
print(boost_adult_roc@metrics$max_criteria_and_metric_scores[4,])
gbm_accuracy_adult_test <- boost_adult_roc@metrics$max_criteria_and_metric_scores[4,3]
cat(" \n Test Accuracy for Adult Census: ", gbm_accuracy_adult_test)

############## FOr HR Dataset  ###############

## For Adult census dataset
df2 <- h2o.importFile(path = "HR_smoted.csv")
df2['C1'] <- NULL

## pick a response for the supervised problem
response_hr <- "left"

## the response variable is an integer, convert it into a categorical/factor for binary classification
df2[[response_hr]] <- as.factor(df2[[response_hr]])           

## use all other columns (except for the name) as predictors
predictors_hr <- setdiff(names(df2), c(response_hr, "name")) 


splits_hr <- h2o.splitFrame(
  data = df2, 
  ratios = c(0.6,0.2),   ## only need to specify 2 fractions, the 3rd is implied
  destination_frames = c("train.hex", "valid.hex", "test.hex"), seed = 1234
)
train_HR <- splits_hr[[1]]
valid_HR <- splits_hr[[2]]
test_HR  <- splits_hr[[3]]

hyper_params_hr = list( max_depth = c(4,6,8,12,16,20) ) ##faster for larger datasets

gridHR <- h2o.grid(
  ## hyper parameters
  hyper_params = hyper_params_hr,
  
  ## full Cartesian hyper-parameter search
  search_criteria = list(strategy = "Cartesian"),
  
  ## which algorithm to run
  algorithm="gbm",
  
  ## identifier for the grid, to later retrieve it
  grid_id="depth_grid_HR",
  
  ## standard model parameters
  x = predictors_hr, 
  y = response_hr, 
  training_frame = train_HR, 
  validation_frame = valid_HR,
  
  ## more trees is better if the learning rate is small enough 
  ## here, use "more than enough" trees - we have early stopping
  ntrees = 10000,                                                            
  
  ## smaller learning rate is better
  ## since we have learning_rate_annealing, we can afford to start with a bigger learning rate
  learn_rate = 0.05,                                                         
  
  ## learning rate annealing: learning_rate shrinks by 1% after every tree 
  ## (use 1.00 to disable, but then lower the learning_rate)
  learn_rate_annealing = 0.99,                                               
  
  ## sample 80% of rows per tree
  sample_rate = 0.8,                                                       
  
  ## sample 80% of columns per split
  col_sample_rate = 0.8, 
  
  ## fix a random number generator seed for reproducibility
  seed = 1234,                                                             
  
  ## early stopping once the validation AUC doesn't improve by at least 0.01% for 5 consecutive scoring events
  stopping_rounds = 5,
  stopping_tolerance = 1e-4,
  stopping_metric = "AUC", 
  
  ## score every 10 trees to make early stopping reproducible (it depends on the scoring interval)
  score_tree_interval = 10                                                
)

## sort the grid models by decreasing AUC
sortedGrid_HR <- h2o.getGrid("depth_grid_HR", sort_by="auc", decreasing = TRUE)    
#Sorted Grid will show AUC factor for model under different depths for tree
#This will help to understand impact of the depth of Tree on ROC
print('Grid with models with different tree depths and their respective ROC ordered ROC desc')
print(sortedGrid_HR)

cat(" \n get the model with max accuracy")
gbm_hr <- h2o.getModel(sortedGrid_HR@model_ids[[1]])
cat("\n Model Summary: Train & Valid: HR \n")
print(summary(gbm_hr))
cat("\n Accuracy  \n")
print(gbm_hr@model$validation_metrics@metrics$max_criteria_and_metric_scores[4,])
gbm_accuracy_hr_valid <- gbm_hr@model$validation_metrics@metrics$max_criteria_and_metric_scores[4,3]
auc_gbm_hr_train <-h2o.auc(h2o.performance(gbm_hr, newdata = valid_HR))
cat(" \n validation accuracy: ", gbm_accuracy_hr_valid)

## Model accuracy for test dataset
boost_hr_roc <- h2o.performance(gbm_hr, newdata = test_HR)
cat("\n \n Test Model: HR: Summary \n")
print(boost_hr_roc)
cat("\n Test HR model accuracy: ")
print(boost_hr_roc@metrics$max_criteria_and_metric_scores[4,])
auc_gbm_hr_test <- h2o.auc(h2o.performance(gbm_hr, newdata = test_HR))
gbm_accuracy_hr_test <- boost_hr_roc@metrics$max_criteria_and_metric_scores[4,3]
cat(" \n Test Accuracy for HR: ", gbm_accuracy_hr_test)

###################################################################
# MOdel Comparison
###################################################################


x_model <- c(1,2,3,4,5,6,7,8,9,10,11,12)
  
color_val <- c("adult svm tr","adult svm te", 
           "hr svm tr", "hr svm te", 
           "adult tree tr", "adult tree te",
           "hr tree tr", "hr tree te",
           "adult gbm valid", "adult gbm test", 
           "hr gbm valid", "hr gbm test")

library(ggplot2)
y_model <- c(adult_svm_accuracy_tr,adult_svm_accuracy_te, 
             hr_svm_accuracy_tr, hr_svm_accuracy, 
             accurracy_dt1_inf, accurracy_dt1_3,
             accurracy_dt2_gini, accurracy_dt2_3,
             gbm_accuracy_adult_valid, gbm_accuracy_adult_test, 
             gbm_accuracy_hr_valid, gbm_accuracy_hr_test)

accuracy_plot <- data.frame(X =x_model,
                  Y =y_model,
                  models = color_val)

print(ggplot(accuracy_plot) + geom_point(aes(x=X,y=Y,color=models),size=4))

# Plotting the three curves
plot(perf_svm_adult, main = "ROC curves for the models", col='blue')
plot(perf_svm_hr,add=TRUE, col='red')
plot(pe, add=TRUE, col='green3')
plot(pe2, add=TRUE, col='darkmagenta')
legend('bottom', c("SVM Adult", "SVM HR", "Tree Adult", "Tree HR"), fill = c('blue','red','green3','darkmagenta'), bty='n')

plot(boost_adult_roc, main = 'ROC curve for Boosting for Adult Census', col='blue')
plot(boost_hr_roc, main='ROC Curve for boosting for HR', col='red')

cat("\n Project last line reached... Completed Successfully.")

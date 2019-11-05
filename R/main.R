##################################################################################
#
# Course:       32513 - Advanced Data Analytics Algorithms
# Week:         
# data source:  Kaggle
# dataset:      Stay Alert! Ford Challenge
# Filters:      
#
# Comments:     
# From:         
#
##################################################################################

# install.packages("gsubfn")
# install.packages("caret")
# install.packages("mlbench")
# install.packages("psych")
# install.packages("dplyr")
# install.packages("xgboost")
# install.packages("fscaret", dependencies = c("Depends","Suggests"))

library(caret)
library(mlbench)
library(fscaret)
#library(xlsx)
library(dplyr)
library(psych)
library(corrplot)
library(xgboost)
library(ROCR)
library(pROC)

#######################################################################
#                                                                     #
#                          1 - LOADING DATA                           #
#                                                                     #
#######################################################################

##### Setting the path to the correct folder #####
main_path <- "C:/DATA/Personal/UTS/Sem 2/Advanced Algo/Assignment 2"
setwd(main_path)

##### Reading the file #####
data <- read.csv("fordTrain.csv",
                 header=TRUE, stringsAsFactors=FALSE, na.strings = c("NA", ""),
                 strip.white = TRUE, blank.lines.skip=TRUE, skip=0)

validatedata <- read.csv("fordTest.csv",
                 header=TRUE, stringsAsFactors=FALSE, na.strings = c("NA", ""),
                 strip.white = TRUE, blank.lines.skip=TRUE, skip=0)

summary(data)
summary(validatedata)

#######################################################################
#                                                                     #
#                        2 - DATA UNDERSTANDING                       #
#                                                                     #
#######################################################################

#To ensure steps are repeatable
set.seed(131)

#No of records
nrow(data)

#No of attributes
ncol(data)

#No missing values
summary(data)

#Formatting Dataset
dput(names(data))
data<-data[c("TrialID", "ObsNum", "P1", "P2", "P3", "P4", "P5", 
             "P6", "P7", "P8", "E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8", 
             "E9", "E10", "E11", "V1", "V2", "V3", "V4", "V5", "V6", "V7", 
             "V8", "V9", "V10", "V11", "IsAlert")]

#Descriptive Statistics
psych::describe(data, fast = FALSE )


##### Printing the distribution of Result #####
counts <- table(data$IsAlert)
countsframe<-as.data.frame(counts)
png(height=500, width=700, pointsize=20, file="barplot_IsAlert.png")
ggplot(countsframe, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = '#000099') +
  geom_text(aes(label = sprintf("%.2f%%", Freq/sum(Freq) * 100))  , 
            vjust = -.5)+
  scale_size_area() +
ggtitle("Alertness Distribution") + xlab("IsAlert") + ylab("Frequency")
dev.off()

##### Histogram and Box Plots #####

png(height=3000, width=3000, pointsize=40, file="hist_boxplot_indvarP1_P4.png")
par(mfrow=c(4,2))

hist(data[, 3], main = "Histogram of P1", xlab="Physiological 1", ylab="Frequency", col = "blue")
boxplot(data[, 3], main = "Boxplot of P1", xlab="Physiological 1", col = "blue")

hist(data[, 4], main = "Histogram of P2", xlab="Physiological 2", ylab="Frequency", col = "blue")
boxplot(data[, 4], main = "Boxplot of P2", xlab="Physiological 2", col = "blue")

hist(data[, 5], main = "Histogram of P3", xlab="Physiological 3", ylab="Frequency", col = "blue")
boxplot(data[, 5], main = "Boxplot of P3", xlab="Physiological 3", col = "blue")

hist(data[, 6], main = "Histogram of P4", xlab="Physiological 4", ylab="Frequency", col = "blue")
boxplot(data[, 6], main = "Boxplot of P4", xlab="Physiological 4", col = "blue")

dev.off()

png(height=3000, width=3000, pointsize=40, file="hist_boxplot_indvarP5_P8.png")
par(mfrow=c(4,2))

hist(data[, 7], main = "Histogram of P5", xlab="Physiological 5", ylab="Frequency", col = "blue")
boxplot(data[, 7], main = "Boxplot of P5", xlab="Physiological 5", col = "blue")

hist(data[, 8], main = "Histogram of P6", xlab="Physiological 6", ylab="Frequency", col = "blue")
boxplot(data[, 8], main = "Boxplot of P6", xlab="Physiological 6", col = "blue")

hist(data[, 9], main = "Histogram of P7", xlab="Physiological 7", ylab="Frequency", col = "blue")
boxplot(data[, 9], main = "Boxplot of P7", xlab="Physiological 7", col = "blue")

hist(data[, 10], main = "Histogram of P8", xlab="Physiological 8", ylab="Frequency", col = "blue")
boxplot(data[, 10], main = "Boxplot of P8", xlab="Physiological 8", col = "blue")

dev.off()


png(height=4000, width=3000, pointsize=40, file="hist_boxplot_indvarE1_E5.png")
par(mfrow=c(5,2))

hist(data[, 11], main = "Histogram of E1", xlab="Environmental 1", ylab="Frequency", col = "blue")
boxplot(data[, 11], main = "Boxplot of E1", xlab="Environmental 1", col = "blue")

hist(data[, 12], main = "Histogram of E2", xlab="Environmental 2", ylab="Frequency", col = "blue")
boxplot(data[, 12], main = "Boxplot of E2", xlab="Environmental 2", col = "blue")

hist(data[, 13], main = "Histogram of E3", xlab="Environmental 3", ylab="Frequency", col = "blue")
boxplot(data[, 13], main = "Boxplot of E3", xlab="Environmental 3", col = "blue")

hist(data[, 14], main = "Histogram of E4", xlab="Environmental 4", ylab="Frequency", col = "blue")
boxplot(data[, 14], main = "Boxplot of E4", xlab="Environmental 4", col = "blue")

hist(data[, 15], main = "Histogram of E5", xlab="Environmental 5", ylab="Frequency", col = "blue")
boxplot(data[, 15], main = "Boxplot of E5", xlab="Environmental 5", col = "blue")

dev.off()

png(height=4500, width=3000, pointsize=40, file="hist_boxplot_indvarE6_E11.png")
par(mfrow=c(6,2))

hist(data[, 16], main = "Histogram of E6", xlab="Environmental 6", ylab="Frequency", col = "blue")
boxplot(data[, 16], main = "Boxplot of E6", xlab="Environmental 6", col = "blue")

hist(data[, 17], main = "Histogram of E7", xlab="Environmental 7", ylab="Frequency", col = "blue")
boxplot(data[, 17], main = "Boxplot of E7", xlab="Environmental 7", col = "blue")

hist(data[, 18], main = "Histogram of E8", xlab="Environmental 8", ylab="Frequency", col = "blue")
boxplot(data[, 18], main = "Boxplot of E8", xlab="Environmental 8", col = "blue")

hist(data[, 19], main = "Histogram of E9", xlab="Environmental 9", ylab="Frequency", col = "blue")
boxplot(data[, 19], main = "Boxplot of E9", xlab="Environmental 9", col = "blue")

hist(data[, 20], main = "Histogram of E10", xlab="Environmental 10", ylab="Frequency", col = "blue")
boxplot(data[, 20], main = "Boxplot of E10", xlab="Environmental 10", col = "blue")

hist(data[, 21], main = "Histogram of E11", xlab="Environmental 11", ylab="Frequency", col = "blue")
boxplot(data[, 21], main = "Boxplot of E11", xlab="Environmental 11", col = "blue")

dev.off()

png(height=4000, width=3000, pointsize=40, file="hist_boxplot_indvarV1_V5.png")
par(mfrow=c(5,2))

hist(data[, 22], main = "Histogram of V1", xlab="Vehicular 1", ylab="Frequency", col = "blue")
boxplot(data[, 22], main = "Boxplot of V1", xlab="Vehicular 1", col = "blue")

hist(data[, 23], main = "Histogram of V2", xlab="Vehicular 2", ylab="Frequency", col = "blue")
boxplot(data[, 23], main = "Boxplot of V2", xlab="Vehicular 2", col = "blue")

hist(data[, 24], main = "Histogram of V3", xlab="Vehicular 3", ylab="Frequency", col = "blue")
boxplot(data[, 24], main = "Boxplot of V3", xlab="Vehicular 3", col = "blue")

hist(data[, 25], main = "Histogram of V4", xlab="Vehicular 4", ylab="Frequency", col = "blue")
boxplot(data[, 25], main = "Boxplot of V4", xlab="Vehicular 4", col = "blue")

hist(data[, 26], main = "Histogram of V5", xlab="Vehicular 5", ylab="Frequency", col = "blue")
boxplot(data[, 26], main = "Boxplot of V5", xlab="Vehicular 5", col = "blue")

dev.off()


png(height=4500, width=3000, pointsize=40, file="hist_boxplot_indvarV6_V11.png")
par(mfrow=c(6,2))

hist(data[, 27], main = "Histogram of V6", xlab="Vehicular 6", ylab="Frequency", col = "blue")
boxplot(data[, 27], main = "Boxplot of V6", xlab="Vehicular 6", col = "blue")

hist(data[, 28], main = "Histogram of V7", xlab="Vehicular 7", ylab="Frequency", col = "blue")
boxplot(data[, 28], main = "Boxplot of V7", xlab="Vehicular 7", col = "blue")

hist(data[, 29], main = "Histogram of V8", xlab="Vehicular 8", ylab="Frequency", col = "blue")
boxplot(data[, 29], main = "Boxplot of V8", xlab="Vehicular 8", col = "blue")

hist(data[, 30], main = "Histogram of V9", xlab="Vehicular 9", ylab="Frequency", col = "blue")
boxplot(data[, 30], main = "Boxplot of V9", xlab="Vehicular 9", col = "blue")

hist(data[, 31], main = "Histogram of V10", xlab="Vehicular 10", ylab="Frequency", col = "blue")
boxplot(data[, 31], main = "Boxplot of V10", xlab="Vehicular 10", col = "blue")

hist(data[, 32], main = "Histogram of V11", xlab="Vehicular 11", ylab="Frequency", col = "blue")
boxplot(data[, 32], main = "Boxplot of V11", xlab="Vehicular 11", col = "blue")

dev.off()

##### Overlaid Histograms #####

data_alert<-data[data$IsAlert == '1',]
data_not_alert<-data[data$IsAlert == '0',]

png(height=4000, width=3000, pointsize=40, file="hist_boxplot_MostlyZeroes.png")
par(mfrow=c(4,2))

hist(data_alert[, 7], main = "Histogram Overlay of P5", xlab="Physiological 5", ylab="Frequency", col=rgb(.5,.8,1,0.5))
hist(data_not_alert[, 7], col = rgb(1,.5,.4,.5), add=T)
legend("topright", c("Alert", "Not Alert"), col=c(rgb(.5,.8,1,0.5), rgb(1,.5,.4,.5)), lwd=10)
box()

#Remove 73 records for values >= 228812?
hist(data_alert[, 8], main = "Histogram Overlay of P6", xlab="Physiological 6", ylab="Frequency", col=rgb(.5,.8,1,0.5))
hist(data_not_alert[, 8], col = rgb(1,.5,.4,.5), add=T)
legend("topright", c("Alert", "Not Alert"), col=c(rgb(.5,.8,1,0.5), rgb(1,.5,.4,.5)), lwd=10)
box()

hist(data_alert[, 13], main = "Histogram Overlay of E3", xlab="Environmental 3", ylab="Frequency", col = rgb(.5,.8,1,0.5))
hist(data_not_alert[, 13], col = rgb(1,.5,.4,.5), add=T)
legend("topright", c("Alert", "Not Alert"), col=c(rgb(.5,.8,1,0.5), rgb(1,.5,.4,.5)), lwd=10)
box()

hist(data_alert[, 17], main = "Histogram Overlay of E7", xlab="Environmental 7", ylab="Frequency", col = rgb(.5,.8,1,0.5))
hist(data_not_alert[, 17], col = rgb(1,.5,.4,.5), add=T)
legend("topright", c("Alert", "Not Alert"), col=c(rgb(.5,.8,1,0.5), rgb(1,.5,.4,.5)), lwd=10)
box()

hist(data_alert[, 18], main = "Histogram Overlay of E8", xlab="Environmental 8", ylab="Frequency", col = rgb(.5,.8,1,0.5))
hist(data_not_alert[, 18], col = rgb(1,.5,.4,.5), add=T)
legend("topright", c("Alert", "Not Alert"), col=c(rgb(.5,.8,1,0.5), rgb(1,.5,.4,.5)), lwd=10)
box()

hist(data_alert[, 21], main = "Histogram Overlay of E11", xlab="Environmental 11", ylab="Frequency", col = rgb(.5,.8,1,0.5))
hist(data_not_alert[, 21], col = rgb(1,.5,.4,.5), add=T)
legend("topright", c("Alert", "Not Alert"), col=c(rgb(.5,.8,1,0.5), rgb(1,.5,.4,.5)), lwd=10)
box()

hist(data_alert[, 25], main = "Histogram Overlay of V4", xlab="Vehicular 4", ylab="Frequency", col = rgb(.5,.8,1,0.5))
hist(data_not_alert[, 25], col = rgb(1,.5,.4,.5), add=T)
legend("topright", c("Alert", "Not Alert"), col=c(rgb(.5,.8,1,0.5), rgb(1,.5,.4,.5)), lwd=10)
box()

hist(data_alert[, 32], main = "Histogram Overlay of V11", xlab="Vehicular 11", ylab="Frequency", col = rgb(.5,.8,1,0.5))
hist(data_not_alert[, 32], col = rgb(1,.5,.4,.5), add=T)
legend("topright", c("Alert", "Not Alert"), col=c(rgb(.5,.8,1,0.5), rgb(1,.5,.4,.5)), lwd=10)
box()

dev.off()


########### Repeated Measure Analysis ################
hist_data <- data %>% count(TrialID)
png(height=400, width=800, pointsize=20, file="hist_repeateddriver.png")
hist(hist_data$n, main = "Histogram of repeated measures for each driver", 
     xlab="Number of Repeated Measures by Driver", ylab="Frequency", col = "blue"
     , breaks=seq(1160,1220,by=10))
dev.off()


############ Correlation Analysis #####################
cor_data_full <- data[,!(names(data) %in% c("IsAlert","TrialID","ObsNum","P8","V7","V9"))]

##### Computing the correlation matrix ######
cor_mat_full <- cor(cor_data_full, use="complete.obs")

##### Computing the correlation matrix ######
png(height=1350, width=1500, pointsize=20, file="correlation_analysis2.png")
corrplot.mixed(cor_mat_full, lower="number", upper="circle")
dev.off()


#######################################################################
#                                                                     #
#                         3 - DATA PREPARATION                        #
#                                                                     #
#######################################################################

# Convert any potential factors in the data through heuristic.
# If number of unique values in dataset is less than specified threshold
# then treat as categorical data
auto_convert_factors <- function(data, cat_threshold=10, cols_ignore=list()) {
  
  for (col in names(data)) {
    if (!is.factor(data[[col]]) && 
        length(unique(data[[col]])) <= cat_threshold && 
        !is.element(col, cols_ignore)) {
      data[[col]] <- as.factor(data[[col]])
      cat(col, " converted to factor\n")
    }
  }
  data
}

isAlertData <- auto_convert_factors(data, 10, cols_ignore = list('IsAlert'))

#Dropping P8, V7, V9 as they are all zeroes. Dropping TrailID and ObsNum as they are unique ids.

isAlertData<-isAlertData[c("P1", "P2", "P3", "P4", "P5", 
             "P6", "P7", "E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8", 
             "E9", "E10", "E11", "V1", "V2", "V3", "V4","V5", "V6", "V8",
             "V10", "V11", "IsAlert")]

datadummy<-dummyVars("~.",data=isAlertData,fullRank = F)

datatemp<-as.data.frame(predict(datadummy,isAlertData))

head(datatemp)

summary(datatemp)

#Partitioning dataset in to training and test dataset.

splitIndexMulti <- createDataPartition(datatemp$IsAlert, p=.01, list = FALSE, times = 2)

trainDataset <- datatemp[splitIndexMulti[,1],]
testDataset <- datatemp[splitIndexMulti[,2],]

dim(trainDataset)
dim(testDataset)

summary(trainDataset)

#Variable Importance

fsModels2 <- c("glm","gbm","treebag","ridge","lasso","rf","xgbLinear")
myFs2<-fscaret(trainDataset, testDataset, myTimeLimit = 40, preprocessData = TRUE,
              Used.funcRegPred = fsModels2, with.labels = TRUE,
              supress.output=FALSE, no.cores = 2, installReqPckg = TRUE)

names(myFs2)
myFs2$VarImp
myFs2$PPlabels
myFs2$VarImp$matrixVarImp.MSE

results <- myFs2$VarImp$matrixVarImp.MSE
results$Input_no <- as.numeric(results$Input_no)
results <- results[c("SUM","SUM%","ImpGrad","Input_no")]
myFs2$PPlabels$Input_no <-  as.numeric(rownames(myFs2$PPlabels))
results <- merge(x=results, y=myFs2$PPlabels, by="Input_no", all.x=T)
results <- results[c('Labels', 'SUM')]
results <- subset(results,results$SUM !=0)
results <- results[order(-results$SUM),]
print(results)


#######################################################################
#                                                                     #
#                         4 - MODELLING                               #
#                                                                     #
#######################################################################

# Uses caret library, doing Automatic grid search (possible to do manual one as well)

modelTrain<-trainDataset
modelTrain$IsAlert <- as.factor(ifelse(modelTrain$IsAlert == 1,'Y','N'))

modelTest<-testDataset
modelTest$IsAlert <- as.factor(ifelse(modelTest$IsAlert == 1,'Y','N'))

#Defining training control
control <- trainControl(
  method          = "repeatedcv",
  number          = 5,
  repeats         = 2,
  search          = "grid",
  classProbs      = TRUE,
  summaryFunction = twoClassSummary, #ROC AUC 
  verboseIter     = TRUE
)

## Before variable removal

model1 <- train(IsAlert ~ ., 
                data = modelTrain[,!(names(modelTrain) %in% c("TrialID","ObsNum","P8","V7","V9"))], 
                method = "gbm",          
                metric = "ROC",
                na.action = na.pass,
                preProcess = c("center", "scale", "medianImpute"),
                trControl = control)


model2 <- train(IsAlert ~ ., 
                data = modelTrain[,!(names(modelTrain) %in% c("TrialID","ObsNum","P8","V7","V9"))], 
                method = "xgbLinear",          
                metric = "ROC",
                na.action = na.pass,
                preProcess = c("center", "scale", "medianImpute"),
                trControl = control)

## After variable removal (Highly correlated: P4 and V1 removed)
model3 <- train(IsAlert ~ ., 
                data = modelTrain[,!(names(modelTrain) %in% c("TrialID","ObsNum","P8","V7","V9","P4","V1"))], 
                method = "gbm",          
                metric = "ROC",
                na.action = na.pass,
                preProcess = c("center", "scale", "medianImpute"),
                trControl = control)


model4 <- train(IsAlert ~ ., 
                data = modelTrain[,!(names(modelTrain) %in% c("TrialID","ObsNum","P8","V7","V9","P4","V1"))], 
                method = "xgbLinear",          
                metric = "ROC",
                na.action = na.pass,
                preProcess = c("center", "scale", "medianImpute"),
                trControl = control)

print(model5)

# Check out the hyperparameters 
print(model1)

# Look at ROC results versus hyperparameter grid
plot(model1)

# Check out the hyperparameters 
print(model2)

# Look at ROC results versus hyperparameter grid
plot(model2)

# Check out the hyperparameters 
print(model3)

# Look at ROC results versus hyperparameter grid
plot(model3)

# Check out the hyperparameters 
print(model4)

# Look at ROC results versus hyperparameter grid
plot(model4)


######## Results and Interpretation ####################

# Model 1
# Get predictions
test_results1 <- predict(model1, 
                        modelTest, 
                        na.action = na.pass, 
                        type = "prob")

test_results1$obs <- modelTest$IsAlert
test_results1$pred <- predict(model1,modelTest, na.action = na.pass)

# Confusion Matrix
confusionMatrix(test_results1$pred,test_results1$obs, positive="Y")

# AUROC - This is what we compare with other models and select the best one
twoClassSummary(test_results1, lev = c("Y","N"))

# Plot ROC Curve
roc_results1 <- roc(modelTest$IsAlert, 
                   predict(model1, modelTest, type="prob")[,1],
                   levels = rev(levels(modelTest$IsAlert)))

roc_results1
plot(roc_results1, print.thres = c(.5), type = "S",
     print.thres.pattern = "%.3f (Spec = %.3f, Sens = %.3f)",
     print.thres.cex = .8,
     legacy.axes = TRUE)
legend("topright", c("GBM Full"))


# Model 2
# Get predictions
test_results2 <- predict(model2, 
                        modelTest, 
                        na.action = na.pass, 
                        type = "prob")

test_results2$obs <- modelTest$IsAlert
test_results2$pred <- predict(model2,modelTest, na.action = na.pass)

# Confusion Matrix
confusionMatrix(test_results2$pred,test_results2$obs, positive="Y")

# AUROC - This is what we compare with other models and select the best one
twoClassSummary(test_results2, lev = c("Y","N"))

# Plot ROC Curve
roc_results2 <- roc(modelTest$IsAlert, 
                   predict(model2, modelTest, type="prob")[,1],
                   levels = rev(levels(modelTest$IsAlert)))
legend("topright", c("XGBoost Full"))

roc_results2
plot(roc_results2, print.thres = c(.5), type = "S",
     print.thres.pattern = "%.3f (Spec = %.3f, Sens = %.3f)",
     print.thres.cex = .8,
     legacy.axes = TRUE)

# Model 3
# Get predictions
test_results3 <- predict(model3, 
                        modelTest, 
                        na.action = na.pass, 
                        type = "prob")

test_results3$obs <- modelTest$IsAlert
test_results3$pred <- predict(model3,modelTest, na.action = na.pass)

# Confusion Matrix
confusionMatrix(test_results3$pred,test_results3$obs, positive="Y")

# AUROC - This is what we compare with other models and select the best one
twoClassSummary(test_results3, lev = c("Y","N"))

# Plot ROC Curve
roc_results3 <- roc(modelTest$IsAlert, 
                   predict(model3, modelTest, type="prob")[,1],
                   levels = rev(levels(modelTest$IsAlert)))

roc_results3
plot(roc_results3, print.thres = c(.5), type = "S",
     print.thres.pattern = "%.3f (Spec = %.3f, Sens = %.3f)",
     print.thres.cex = .8,
     legacy.axes = TRUE)
legend("topright", c("GBM Reduced"))

# Model 4
# Get predictions
test_results4 <- predict(model4, 
                        modelTest, 
                        na.action = na.pass, 
                        type = "prob")

test_results4$obs <- modelTest$IsAlert
test_results4$pred <- predict(model4,modelTest, na.action = na.pass)

# Confusion Matrix
confusionMatrix(test_results4$pred,test_results4$obs, positive="Y")

# AUROC - This is what we compare with other models and select the best one
twoClassSummary(test_results4, lev = c("Y","N"))

# Plot ROC Curve
roc_results4 <- roc(modelTest$IsAlert, 
                   predict(model4, modelTest, type="prob")[,1],
                   levels = rev(levels(modelTest$IsAlert)))

roc_results4
plot(roc_results4, print.thres = c(.5), type = "S",
     print.thres.pattern = "%.3f (Spec = %.3f, Sens = %.3f)",
     print.thres.cex = .8,
     legacy.axes = TRUE)
legend("topright", c("XGBoost Reduced"))

dev.off()



##### Preparing the validation dataset : Begin #######
isAlertDataTest <- auto_convert_factors(validatedata, 10, cols_ignore = list('IsAlert'))

#Dropping P8, V7, V9 as they are all zeroes. Dropping TrailID and ObsNum as they are unique ids.

isAlertDataTest<-isAlertDataTest[c("P1", "P2", "P3", "P4", "P5", 
                           "P6", "P7", "E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8", 
                           "E9", "E10", "E11", "V1", "V2", "V3", "V4","V5", "V6", "V8",
                           "V10", "V11", "IsAlert")]

datadummyTest<-dummyVars("~.",data=isAlertDataTest,fullRank = F)

datatempTest<-as.data.frame(predict(datadummyTest,isAlertDataTest))

##### Preparing the validation dataset : End #######

modelValidate<-datatempTest
modelValidate$IsAlert <- as.factor(ifelse(modelValidate$IsAlert == 1,'Y','N'))


########### The real test  #################

# Model 1
# Get predictions
test_results5 <- predict(model1, 
                         modelValidate, 
                         na.action = na.pass, 
                         type = "prob")

test_results5$obs <- modelValidate$IsAlert
test_results5$pred <- predict(model1,modelValidate, na.action = na.pass)

# Confusion Matrix
confusionMatrix(test_results5$pred,test_results5$obs, positive="Y")

# AUROC - This is what we compare with other models and select the best one
twoClassSummary(test_results5, lev = c("Y","N"))

# Plot ROC Curve
roc_results5 <- roc(modelValidate$IsAlert, 
                    predict(model1, modelValidate, type="prob")[,1],
                    levels = rev(levels(modelValidate$IsAlert)))

roc_results5
plot(roc_results5, print.thres = c(.5), type = "S",
     print.thres.pattern = "%.3f (Spec = %.3f, Sens = %.3f)",
     print.thres.cex = .8,
     legacy.axes = TRUE)
legend("topright", c("GBM Full"))


# Model 2
# Get predictions
test_results6 <- predict(model2, 
                         modelValidate, 
                         na.action = na.pass, 
                         type = "prob")

test_results6$obs <- modelValidate$IsAlert
test_results6$pred <- predict(model2,modelValidate, na.action = na.pass)

# Confusion Matrix
confusionMatrix(test_results6$pred,test_results6$obs, positive="Y")

# AUROC - This is what we compare with other models and select the best one
twoClassSummary(test_results6, lev = c("Y","N"))

# Plot ROC Curve
roc_results6 <- roc(modelValidate$IsAlert, 
                    predict(model2, modelValidate, type="prob")[,1],
                    levels = rev(levels(modelValidate$IsAlert)))

roc_results6
plot(roc_results6, print.thres = c(.5), type = "S",
     print.thres.pattern = "%.3f (Spec = %.3f, Sens = %.3f)",
     print.thres.cex = .8,
     legacy.axes = TRUE)
legend("topright", c("XGBoost Full"))


# Model 3
# Get predictions
test_results7 <- predict(model3, 
                         modelValidate, 
                         na.action = na.pass, 
                         type = "prob")

test_results7$obs <- modelValidate$IsAlert
test_results7$pred <- predict(model3,modelValidate, na.action = na.pass)

# Confusion Matrix
confusionMatrix(test_results7$pred,test_results7$obs, positive="Y")

# AUROC - This is what we compare with other models and select the best one
twoClassSummary(test_results7, lev = c("Y","N"))

# Plot ROC Curve
roc_results7 <- roc(modelValidate$IsAlert, 
                    predict(model3, modelValidate, type="prob")[,1],
                    levels = rev(levels(modelValidate$IsAlert)))

roc_results7
plot(roc_results7, print.thres = c(.5), type = "S",
     print.thres.pattern = "%.3f (Spec = %.3f, Sens = %.3f)",
     print.thres.cex = .8,
     legacy.axes = TRUE)
legend("topright", c("GBM Reduced"))

# Model 4
# Get predictions
test_results8 <- predict(model4, 
                         modelValidate, 
                         na.action = na.pass, 
                         type = "prob")

test_results8$obs <- modelValidate$IsAlert
test_results8$pred <- predict(model4,modelValidate, na.action = na.pass)

# Confusion Matrix
confusionMatrix(test_results8$pred,test_results8$obs, positive="Y")

# AUROC - This is what we compare with other models and select the best one
twoClassSummary(test_results8, lev = c("Y","N"))

# Plot ROC Curve
roc_results8 <- roc(modelValidate$IsAlert, 
                    predict(model4, modelValidate, type="prob")[,1],
                    levels = rev(levels(modelValidate$IsAlert)))

roc_results8
plot(roc_results8, print.thres = c(.5), type = "S",
     print.thres.pattern = "%.3f (Spec = %.3f, Sens = %.3f)",
     print.thres.cex = .8,
     legacy.axes = TRUE)
legend("topright", c("XGBoost Reduced"))

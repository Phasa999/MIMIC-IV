# MIMIC-IV

install.packages("pROC")
install.packages('ROCR')
install.packages("car")

library(caret)
library(ellipsis)
library(e1071)
library(pROC)
library(ROCR)

# Delete missing value
data6 <- na.omit(data6)

summary(data6)

# Join tables
df <- merge(x=data6, y=transfers, by ="subject_id",all.x = TRUE)

summary(df)

# Delete duplicate data
df <- df[!duplicated(df$transfer_id), ]

# Delete missing value=0
df <- subset(df,hr>0)
df <- subset(df,Temp>0)
df <- subset(df,resprate>0)
df <- subset(df,o2sat>0)
df <- subset(df,sbp>0)
df <- subset(df,dbp>0)
df <- subset(df,ed_hours>0)

summary(df)

# Check how many pt. is death -> pt. death = 12,333 cases
pt_death <- table(df$death)
pt_death

# Change string into ...
df$eventtype <- as.factor(df$eventtype)
df$Sex <- as.factor(df$Sex)
df$race <- as.factor(df$race)
df$insurance <- as.factor(df$insurance)
df$ed_4 <- as.factor(df$ed_4)

# Death vs others factors
# death vs eventtype
source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")
c <- crosstab(df, row.vars="eventtype", col.vars="death", type="f")
c
# Show results in %
c <- crosstab(df, row.vars="eventtype", col.vars="death", type="r")
c

# death vs sex
c <- crosstab(df, row.vars="Sex", col.vars="death", type="r")
c

# death vs race
c <- crosstab(df, row.vars="race", col.vars="death", type="r")
c
# death vs insurance
c <- crosstab(df, row.vars="insurance", col.vars="death", type="r")
c

# death vs ED > 4hrs
# 0 = ED < 4hrs = death 2,513 cases
# 1 = ED > 4hrs = death 6,886 cases
c <- crosstab(df, row.vars="ed_4", col.vars="death", type="f")
c

# Show result in %
# 0 = ED < 4hrs = death 3.57%
# 1 = ED > 4hrs = death 1.31%

c <- crosstab(df, row.vars="ed_4", col.vars="death", type="r")
c

# death vs age
# 0 = Alive (everage age = 56 yrs); <- came to ED & still alive
# 1 = Death (everage age = 70 yrs)
aggregate(df$Age, list(df$death), FUN=mean)

# death vs duration of stay in ED$
# 0 = Alive (everage age = 11.12 hrs); <- came to ED & still alive
# 1 = Death (everage age = 7.36 hrs)
aggregate(df$ed_hours, list(df$death), FUN=mean)

# Death VS vital sign
# Change Heart rate to hr
# df$hr <- df$`Heart Rate`
colnames(df)[colnames(df) == "Heart Rate"] ="hr"
summary(df)

# death vs heart rate, 1 - death, 0 - alive
aggregate(df$hr, list(df$death), FUN=mean)

# Death vs blood pressure
# sbp, dbp
aggregate(df$sbp, list(df$death), FUN=mean)
aggregate(df$dbp, list(df$death), FUN=mean)

#O2 Sat
aggregate(df$o2sat, list(df$death), FUN=mean)

# rr
aggregate(df$resprate, list(df$death), FUN=mean)

# -----------------------------------------------------------------
# Fit in Logistic Regression model
model1 <- glm(death ~ Sex+Age+Temp+hr+resprate+o2sat+sbp+dbp+ed_4+ed_hours
              +eventtype, data = df, family = "binomial")

summary(model1)
# Sex - M have mortality chance > F 
# Age - Higher age have higher mortality chance
# Temp - Higher temp have lower mortality chance           
# hr - Higher heart rate have higher mortality chance
# resprate - Higher rr have higher mortality chance
# o2sat - Higher o2sat have lower mortality chance
# sbp - Higher sbp have lower mortality chance
# dbp - Higher dbp have lower mortality chance
# ed_41 - ED stay >4 hrs have lower mortality chance
# ed_hours- Longer ED stay have lower mortality chance
# eventtypedischarge - Not significant= mortality chance of discarge is not different to admitted      
# eventtypeED - ED have lower mortality chance compared to admitted 
# eventtypetransfer - transfer to other departments have higher mortality chance compared to admitted

library(car)
# Check for multi-collinearity; if vif<5 = OK
vif(model1) 

# Check accuracy
library(dplyr)
prob <- model1 %>% predict(df,type = "response")

# cut off at 0.1 
predict.classes <- ifelse(prob >= 0.1, "Death", "Alive")

tab <- table(predict.classes, df$death)
tab

accuracy <- function(x){sum(diag(x)/(sum(rowSums(x))))*100}
accuracy(tab)

# ------------ CONCLUSION of Logistic Model --------------
# The accuracy of true positive is at 0%. Overall accuracy is 98.42%
# This model is good in true negative
# Cut off at 0.5 - cannot predict true positive
# Cut off at 0.25 - can slightly predict true positive 
# Cut off at 0.1 - can predict better true positive, but also contains more false positive
# Overall, this model is not suitable
# -----------------------------------------------------------------

library(e1071)
#Try to use NaiveBayes model
class_naive <- naiveBayes(death ~ Sex+Age+Temp+hr+resprate+o2sat+sbp+dbp+ed_4+ed_hours
                          +eventtype, data = df)
y_pred <- predict(class_naive, newdata = df)

cm <- table(df$death, y_pred)
cm
accuracy(cm)

specificity = 2602/(2602+33369)*100
specificity

# ------------ CONCLUSION of NaiveBayes Model --------------
# The accuracy of true positive is at 7.23%. Overall accuracy is 93.27%
# Sensitivity = TP/(TP+FN)
# Specificity = TN/(TN+FP)

specificity = 2602/(2602+33369)
specificity
# Specificity = TN/(TN+FP)

# ------------------Random Forest------------------
library(dplyr)
library(randomForest)

rf <- randomForest(death ~ Sex+Age+ed_4+ed_hours,data = df, 
                   proximity=TRUE, na.action = na.roughfix)
# RF model is not good in predicting binary outcome data

# ------------------ 1st Attempt of trying xgboost ----------------------------
# ------------------ NOT WORKING->Have to drop unnecessary variables-----------
# ------------------ For result->Jump to see 2nd attempt ----------------------

library(xgboost)

# Convert data
df2 <- df
dim(df2$death)
summary(df2)




#--!!!--- DO NOT RUN UNTIL 2nd Attempt
#--!!!--- DO NOT RUN UNTIL 2nd Attempt
#--!!!--- DO NOT RUN UNTIL 2nd Attempt
#--!!!--- DO NOT RUN UNTIL 2nd Attempt ----------------!!!---------------------
class(df2$death)[1]
xg <- xgboost(data= as.matrix(df$death), label = df$death,max.depth=2,eta=1,nthread=2,nrounds = 2,objective="binary:logistic")

# Predict
xg_pred <- predict(xg,as.matrix(df2$death))

# Get the probability 
print(head(xg_pred))

# Classification
prediction <- as.numeric(xg_pred > 0.5)
print(head(prediction))

# Check model's performance
tab <- table(prediction, df2$death)
tab

# Try cut off at 0.3
prediction <- as.numeric(xg_pred > 0.3)
print(head(prediction))

# Check model's performance
tab <- table(prediction, df2$death)
tab

# Try cut off at 0.04 
prediction <- as.numeric(xg_pred > 0.04)
print(head(prediction))

# Check model's performance
tab <- table(prediction, df2$death)
tab

#Divide data set to Train and Test
ran <- sample(1:nrow(df), 0.8*nrow(df))

df_train = df[ran,1:24]
df_test = df[-ran,1:24]

dim(df_train$death)

class(df_train$death)[1]
xg <- xgboost(data= as.matrix(df_train$death), label = df_train$death,max.depth=2,eta=1,nthread=2,nrounds = 2,objective="binary:logistic")

# Predict
xg_pred <- predict(xg,as.matrix(df_test$death))

# Get the probability 
print(head(xg_pred))

# Classification
prediction <- as.numeric(xg_pred > 0.5)
print(head(prediction))

# Check model's performance
tab <- table(prediction, df_test$death)
tab

# Check accuracy
accuracy(tab)

# ------------------ 2nd Attempt of trying xgboost -----------------------------
#----------------------------Try xgboost again----------------------------------
# Drop variables that we don't use
df2$subject_id <- NULL
df2$row_id <- NULL
df2$seq_num <- NULL
df2$icd_code <- NULL
df2$icd_version <- NULL
df2$icd_title <- NULL
df2$race <- NULL
df2$insurance <- NULL
df2$pain <- NULL
df2$ed_stay_id <- NULL

#----------Divide data set to Train and Test-----------
# Random data
ran <- sample(1:nrow(df2), 0.8*nrow(df2))

df_train = df2[ran, 1:14]
df_test = df2[-ran,1:14]

summary(df2)

y_train <- df_train$death
x_train <- df_train
x_train$death <- NULL
x_train$transfer_id <- NULL

y_test <- df_test$death
x_test <- df_test
x_test$death <- NULL
x_test$transfer_id <- NULL

# Fit in xgboost model--------------------------------
xg <- xgboost(data = data.matrix(x_train[,-1]), label = y_train,max.depth=6,eta=1,nthread=2,nrounds = 2,objective="binary:logistic")

# Predict
xg_pred <- predict(xg,data.matrix(x_test[,-1]))

# Get the probability 
print(head(xg_pred))

# Classification
prediction <- as.numeric(xg_pred > 0.08)
print(head(prediction))

# Check model's performance 
tab <- table(prediction, y_test)
tab

sensitivity = 323/(323+984)
sensitivity

specificity = 105522/(105522+1194)
specificity
# With xgboost, the specificity is at 31%, sensitivity is at 97%

# Check accuracy
accuracy(tab)

print(head(y_test))

# Plot a ROC curve
ROCR_pred_test <- prediction(prediction,y_test)

ROCR_perf_test <- performance(ROCR_pred_test,'tpr','fpr')

plot(ROCR_perf_test,colorize=TRUE,print.cutoffs.at=seq(0.1,by=0.1))

# Calculate the AUC Value
autual_values <- roc(y_test,prediction)
auc(autual_values)

# ------------ CONCLUSION of xgboost/Cut off at 0.08 max.depth = 6--------------
# The accuracy of true positive is at 7.23%. Overall accuracy is 93.27%
# Sensitivity = TP/(TP+FN)
# Specificity = TN/(TN+FP)

# Try cut off at 0.5 - CANNOT PREDICT DEATH
prediction <- as.numeric(xg_pred > 0.5)
print(head(prediction))

# Try cut off at 0.1 
prediction <- as.numeric(xg_pred > 0.1)
print(head(prediction))

# Check model's performance
tab <- table(prediction, y_test)
tab

# Check accuracy 
accuracy(tab)

sensitivity = 389/(389+853)
sensitivity

specificity = 104531/(104531+2139)
specificity

# Specificity = TN/(TN+FP)
# Sensitivity, specificity and accuracy are described in terms of TP, TN, FN and FP.
# Sensitivity = TP/(TP + FN) = (Number of true positive assessment)/(Number of all positive assessment) 
# Specificity = TN/(TN + FP) = (Number of true negative assessment)/(Number of all negative assessment) 
# Accuracy = (TN + TP)/(TN+TP+FN+FP) = (Number of correct assessments)/Number of all assessments)

# Compare sensitivity of different models

# Fit in Logistic Regression -----------------------------------------------
model1 <- glm(death ~ Sex+Age+Temp+hr+resprate+o2sat+sbp+dbp+ed_4+ed_hours
              +eventtype, data = df_train, family = "binomial")

summary(df_train)

prob <- model1 %>% predict(df_test,type = "response")
# predict.classes <- ifelse(prob >= 0.08, "Death", "Alive")
predict.classes <- as.numeric(prob >= 0.05)

print(head(predict.classes))

summary(predict.classes)

predict.classes <- replace(predict.classes, is.na(predict.classes),0)
summary(predict.classes)

tab <- table(predict.classes, y_test)
tab

accuracy <- function(x){sum(diag(x)/(sum(rowSums(x))))*100}
accuracy(tab)

sensitivity = 197/(197+1113)
sensitivity

specificity = 104139/(104139+2463)
specificity

accuracy(tab)

# Plot a ROC curve
ROCR_pred_test_logistic <- prediction(predict.classes,y_test)

ROCR_perf_test_logistic <- performance(ROCR_pred_test_logistic,'tpr','fpr')

plot(ROCR_perf_test_logistic,colorize=TRUE,print.cutoffs.at=seq(0.1,by=0.1))

# Calculate the AUC Value
autual_values <- roc(y_test,predict.classes)
auc(autual_values)

# --------Compare with NaiveBayes-----------------------------------------------
# Fit in NaiveBayes model
library(e1071)
class_naive <- naiveBayes(death ~ Sex+Age+Temp+hr+resprate+o2sat+sbp+dbp+ed_4+ed_hours
                          +eventtype, data = df_train)
# Predict
y_pred <- predict(class_naive, newdata = df_test)
summary(y_pred)

# Show confusion matrix
cm <- table( y_pred, df_test$death)
cm

sensitivity = 133/(133+1177)
sensitivity

specificity = 103569/(103569+3033)
specificity

accuracy(cm)

# Plot a ROC curve
ROCurve<-roc(y_test,as.numeric(y_pred))
plot(ROCurve)
## We got AUC of 0.9423
auc(ROCurve)

# -----
ROCR_pred_test_nb <- prediction(list(y_pred),y_test)
summary(y_test)

ROCR_perf_test_nb <- performance(ROCR_pred_test_nb,'tpr','fpr')

plot(ROCR_perf_test_nb,colorize=TRUE,print.cutoffs.at=seq(0.1,by=0.1))

# Calculate the AUC Value
autual_values <- roc(y_test,y_pred)
auc(autual_values)

# ------------------------------Table 1, overall--------------------------------
# install.packages("tableone")
install.packages("tableone")
#Load package
library(tableone)

# Define the variables to include in the table
vars <- c('death','Sex','Age','Temp','resprate','o2sat','sbp','dbp','ed_4','ed_hours','eventtype')

# Create the table
table1 <- CreateTableOne(vars = vars, data = df, strata = "death", test = FALSE)
                    
# Print the table
print(table1, nonnormal = vars, exact = "fisher")
# -----------------------

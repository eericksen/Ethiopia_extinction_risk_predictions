## --------------------------------------------------------------------------
## Random Forest Modeling
## --------------------------------------------------------------------------
class(final_predictors$prediction)
final_predictors$prediction <- as.factor(final_predictors$prediction)

## --------------------------------------------------------------------------
## ALL PREDICTORS 
## --------------------------------------------------------------------------
set.seed(15)
class(final_predictors$observed_threat)
final_predictors$observed_threat <- as.factor(final_predictors$observed_threat)
new_data <- createDataPartition(final_predictors$observed_threat,
                                 p = 0.7,
                                 list = FALSE)
train_df <- final_predictors[new_data, ]
test_df  <- final_predictors[-new_data, ]
set.seed(15)
rf_model_train <- randomForest(observed_threat ~ 
                                 total_flor_regions + 
                                 protected + 
                                 hfp_quant_1to4 +
                                 hfp_delta_class5
                                 ,
                               data = train_df,
                               ntree = 500,
                               importance = TRUE,
                               proximity = TRUE)

best_tree <- which.min(rf_model_train$err.rate[,"OOB"])
cat("Lowest OOB error at tree ~", best_tree, "\n")
varImpPlot(rf_model_train, main="Variable Importance")

pred_class  <- predict(rf_model_train, newdata = test_df)
obs_class   <- test_df$observed_threat
cm <- confusionMatrix(pred_class, obs_class, positive = "T")
print(cm)

# Which species is it getting wrong?
test_df$predicted_threat_rf <- predict(rf_model_train, newdata = test_df, type = "response")

wrong <- is.na(test_df$predicted_threat_rf) | is.na(test_df$observed_threat) |
  test_df$predicted_threat_rf != test_df$observed_threat

misclassified <- test_df[wrong, ]

misclassified[, c("genus", "species", "observed_threat", "predicted_threat_rf")]

# Variable importance plot
varImpPlot(rf_model_train)

vip_data <- vip(rf_model_train, type = 1, plot = FALSE)$data
vip_data

# AUC
pred1 <- predict(rf_model_train,type = "prob")

perf <-  ROCR::prediction(pred1[,2], train_df$observed_threat)
auc_perf <- ROCR::performance(perf, "auc")
auc_value <- auc_perf@y.values[[1]]

print(auc_value)

pred3 <- ROCR::performance(perf, "tpr","fpr")

plot(pred3, main="ROC Curve for Random Forest", col=2, lwd=2)

abline(a=0, b=1, lwd=2, lty=2, col="gray")

## --------------------------------------------------------------------------
## HFP DELTA CLASS 5 AND FLORISTIC REGION (MODEL 1)
## --------------------------------------------------------------------------

set.seed(15)
class(final_predictors$observed_threat)
final_predictors$observed_threat <- as.factor(final_predictors$observed_threat)
new_data <- createDataPartition(final_predictors$observed_threat,
                                p = 0.7,
                                list = FALSE)
train_df <- final_predictors[new_data, ]
test_df  <- final_predictors[-new_data, ]
set.seed(15)
rf_model_train <- randomForest(observed_threat ~ 
                                 total_flor_regions + 
                                 hfp_delta_class5
                               ,
                               data = train_df,
                               ntree = 500,
                               importance = TRUE,
                               proximity = TRUE)


best_tree <- which.min(rf_model_train$err.rate[,"OOB"])
cat("Lowest OOB error at tree ~", best_tree, "\n")
varImpPlot(rf_model_train, main="Variable Importance")

pred_class  <- predict(rf_model_train, newdata = test_df)
obs_class   <- test_df$observed_threat
cm <- confusionMatrix(pred_class, obs_class, positive = "T")
print(cm)

# Which species is it getting wrong?
test_df$predicted_threat_rf <- predict(rf_model_train, newdata = test_df, type = "response")

wrong <- is.na(test_df$predicted_threat_rf) | is.na(test_df$observed_threat) |
  test_df$predicted_threat_rf != test_df$observed_threat

misclassified <- test_df[wrong, ]

misclassified[, c("genus", "species", "observed_threat", "predicted_threat_rf")]

# Variable Importance Plot
varImpPlot(rf_model_train)

vip_data <- vip(rf_model_train, type = 1, plot = FALSE)$data
vip_data

# AUC
pred1 <- predict(rf_model_train,type = "prob")

perf <-  ROCR::prediction(pred1[,2], train_df$observed_threat)

auc_perf <- ROCR::performance(perf, "auc")
auc_value <- auc_perf@y.values[[1]]

print(auc_value)

pred3 <- ROCR::performance(perf, "tpr","fpr")

plot(pred3, main="ROC Curve for Random Forest", col=2, lwd=2)

abline(a=0, b=1, lwd=2, lty=2, col="gray")

## --------------------------------------------------------------------------
## HFP QUANTILE CLASSES 1-4, PA, AND FLORISTIC REGION (MODEL 2)
## --------------------------------------------------------------------------

set.seed(15)
class(final_predictors$observed_threat)
final_predictors$observed_threat <- as.factor(final_predictors$observed_threat)
new_data <- createDataPartition(final_predictors$observed_threat,
                                p = 0.7,
                                list = FALSE)
train_df <- final_predictors[new_data, ]
test_df  <- final_predictors[-new_data, ]
set.seed(15)
rf_model_train <- randomForest(observed_threat ~ 
                                 total_flor_regions + 
                                 hfp_quant_1to4 +
                                 protected
                               ,
                               data = train_df,
                               ntree = 500,
                               importance = TRUE,
                               proximity = TRUE)


best_tree <- which.min(rf_model_train$err.rate[,"OOB"])
cat("Lowest OOB error at tree ~", best_tree, "\n")
varImpPlot(rf_model_train, main="Variable Importance")

pred_class  <- predict(rf_model_train, newdata = test_df)
obs_class   <- test_df$observed_threat
cm <- confusionMatrix(pred_class, obs_class, positive = "T")
print(cm)

# Which species is it getting wrong?
test_df$predicted_threat_rf <- predict(rf_model_train, newdata = test_df, type = "response")

wrong <- is.na(test_df$predicted_threat_rf) | is.na(test_df$observed_threat) |
  test_df$predicted_threat_rf != test_df$observed_threat

misclassified <- test_df[wrong, ]

misclassified[, c("genus", "species", "observed_threat", "predicted_threat_rf")]

# Variable Importance Plot
varImpPlot(rf_model_train)

vip_data <- vip(rf_model_train, type = 1, plot = FALSE)$data
vip_data

# AUC
pred1 <- predict(rf_model_train,type = "prob")

perf <-  ROCR::prediction(pred1[,2], train_df$observed_threat)

auc_perf <- ROCR::performance(perf, "auc")
auc_value <- auc_perf@y.values[[1]]

print(auc_value)

pred3 <- ROCR::performance(perf, "tpr","fpr")

plot(pred3, main="ROC Curve for Random Forest", col=2, lwd=2)

abline(a=0, b=1, lwd=2, lty=2, col="gray")

## --------------------------------------------------------------------------
## HFP QUANTILE CLASSES 1-4 AND FLORISTIC REGION (MODEL 3)
## --------------------------------------------------------------------------

set.seed(15)
class(final_predictors$observed_threat)
final_predictors$observed_threat <- as.factor(final_predictors$observed_threat)
new_data <- createDataPartition(final_predictors$observed_threat,
                                p = 0.7,
                                list = FALSE)
train_df <- final_predictors[new_data, ]
test_df  <- final_predictors[-new_data, ]
set.seed(15)
rf_model_train <- randomForest(observed_threat ~ 
                                 total_flor_regions + 
                                 hfp_quant_1to4
                               ,
                               data = train_df,
                               ntree = 500,
                               importance = TRUE,
                               proximity = TRUE)


best_tree <- which.min(rf_model_train$err.rate[,"OOB"])
cat("Lowest OOB error at tree ~", best_tree, "\n")
varImpPlot(rf_model_train, main="Variable Importance")

pred_class  <- predict(rf_model_train, newdata = test_df)
obs_class   <- test_df$observed_threat
cm <- confusionMatrix(pred_class, obs_class, positive = "T")
print(cm)

# Which species is it getting wrong?
test_df$predicted_threat_rf <- predict(rf_model_train, newdata = test_df, type = "response")

wrong <- is.na(test_df$predicted_threat_rf) | is.na(test_df$observed_threat) |
  test_df$predicted_threat_rf != test_df$observed_threat

misclassified <- test_df[wrong, ]

misclassified[, c("genus", "species", "observed_threat", "predicted_threat_rf")]

# Variable importance
varImpPlot(rf_model_train)

vip_data <- vip(rf_model_train, type = 1, plot = FALSE)$data
vip_data

# AUC
pred1 <- predict(rf_model_train,type = "prob")

perf <-  ROCR::prediction(pred1[,2], train_df$observed_threat)

auc_perf <- ROCR::performance(perf, "auc")
auc_value <- auc_perf@y.values[[1]]

print(auc_value)

pred3 <- ROCR::performance(perf, "tpr","fpr")

plot(pred3, main="ROC Curve for Random Forest", col=2, lwd=2)

abline(a=0, b=1, lwd=2, lty=2, col="gray")
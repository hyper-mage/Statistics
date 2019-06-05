# scratch code for proj4
# code that may or may not be used

# for question 2
# b
fit <- glm(Purchase ~StoreID, family = binomial, data = train)
# Estimated probabilities for test data
prob <- predict(fit, test, type = "response")
# Predicted classes (using 0.5 cutoff)
pred <- ifelse(prob >= 0.5, "MM", "CH")
# Validated Test error rate
1 - mean(pred == test[, "Purchase"])
# Confusion matrix and (sensitivity, specificity)
con.mat = table(test[, "Purchase"], pred)
confusionMatrix(con.mat)
# Verify Sensitivity
con.mat[1]/sum(con.mat[1],con.mat[2])
# Verify Specificity
con.mat[4]/sum(con.mat[3],con.mat[4])
# Verify Overall misclassification, (FN+FP)/(N+P)
# equal to test error rate == 1 - accuracy
sum(con.mat[2],con.mat[3])/sum(con.mat)
# Show ROC curve
library(pROC)
roc <- roc(test[, "Purchase"], prob, levels = c("MM","CH"))
plot(roc, legacy.axes = T)

# c
library(MASS)
set.seed(1)
fit = lda(Purchase ~ ., data = train)
# Estimated probabilities for test data
prob <- predict(fit, test)
# Validated Test error rate
1 - mean(prob$class == test[, "Purchase"])
# Confusion matrix and (sensitivity, specificity)
con.mat = table(test$Purchase, prob$class)
confusionMatrix(con.mat)
# Verify Sensitivity
con.mat[1]/sum(con.mat[1],con.mat[2])
# Verify Specificity
con.mat[4]/sum(con.mat[3],con.mat[4])
# Verify Overall misclassification, (FN+FP)/(N+P)
# equal to test error rate = 1 - accuracy
sum(con.mat[2],con.mat[3])/sum(con.mat)
# Show ROC curve
library(pROC)
roc <- roc(test[, "Purchase"], prob$posterior[,2], levels = c("MM","CH"))
plot(roc, legacy.axes = T)

# d
library(MASS)
set.seed(1)
fit = qda(Purchase ~ ., data = train)
# Estimated probabilities for test data
prob <- predict(fit, test)
# Validated Test error rate
1 - mean(prob$class == test[, "Purchase"])
# Confusion matrix and (sensitivity, specificity)
con.mat = table(test$Purchase, prob$class)
confusionMatrix(con.mat)
# Verify Sensitivity
con.mat[1]/sum(con.mat[1],con.mat[2])
# Verify Specificity
con.mat[4]/sum(con.mat[3],con.mat[4])
# Verify Overall misclassification, (FN+FP)/(N+P)
# equal to test error rate = 1 - accuracy
sum(con.mat[2],con.mat[3])/sum(con.mat)
# Show ROC curve
library(pROC)
roc <- roc(test[, "Purchase"], prob$posterior[,2], levels = c("MM","CH"))
plot(roc, legacy.axes = T)

# e
#####


# question 3 ----

# CP
plot(fit.summary$cp, xlab = "Number of Variables", ylab = "Cp", 
     type = "l")
point = which.min(fit.summary$cp)
points(point, fit.summary$cp[point],
       col = "red", cex = 2, pch = 8)
# BIC
plot(fit.summary$bic, xlab = "Number of Variables", ylab = "BIC", 
     type = "l")
point = which.min(fit.summary$bic)
points(point, fit.summary$bic[point], col = "red", cex = 2, pch = 8)

# all the weird plots, not sure how to interpret
plot(fit.full, scale = "r2")
plot(fit.full, scale = "adjr2")
plot(fit.full, scale = "Cp")
plot(fit.full, scale = "bic")
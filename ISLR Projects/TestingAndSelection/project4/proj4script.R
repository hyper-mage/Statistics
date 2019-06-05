# Project 4

# question 1 (a) ----
# Make a scatterplot of gpa against act and comment on the strength of linear relationship
# between the two variables.
gpa = read.csv("gpa.csv", header = T)
library(corrplot)
head(gpa)
str(gpa)
summary(gpa)
pairs(subset(gpa))
round(cor(subset(gpa)), 2)

# There is a positive correlation between gpa and act
# the relationship is not very strong (0.27)

# question 1 (b) ----
# Let ρ denote the population correlation between gpa and act. Provide a point estimate of
# ρ, bootstrap estimates of bias and standard error of the point estimate, and 95% confidence
# interval computed using percentile bootstrap. Interpret the results
library(boot)
corr(gpa)
# either function below will work
corr.fn <- function(data, i=c(1:length(data))) {
  result <- data[i,] 
  return(cor(result$gpa, result$act))
}
corr.fn <- function(data, i=c(1:length(data))) {
  result = corr(data[i,])
  return(result)
}
set.seed(1)
corr.boot <- boot(gpa, corr.fn, R = 1000)
# Point estimate, bootstrap bias, and bootstrap se
corr.boot
# 95% bootstrap conf int
boot.ci(corr.boot, type = "perc")
# verified below
sort(corr.boot$t)[c(25, 975)]

# our original ρ is contained in the bootstrap interval and the bias is only
# 0.004196114 which implies good bootstrap estimates.

# question 1 (c) ----
# Fit a simple linear regression model for predicting gpa on the basis of act. Provide the least
# square estimates of the regression coefficients, standard errors of the estimates, and 95% confidence intervals of the coefficients. Perform model diagnostics to verify the model assumptions
# and comment on the results.

fit = lm(gpa~act, data = gpa)
# least square estimate of coefficients, SE, and 95% CI
summary(fit)
confint(fit)
par(mfrow = c(2,2))
plot(fit)

# it appears the residual plot could do better via a transformation but not bad
# the qq plot is not bad but does show some outliers.

# question 1 (d) ----
# Use nonparametric bootstrap to compute the standard errors and 95% confidence intervals
# (using percentile bootstrap) mentioned in part (c) and compare the two sets of results.

library(boot)
fit.fn <- function(data, index) {
  result <- coef(lm(gpa ~ act , data = gpa, subset = index))
  return(result)
}
set.seed(1)
fit.boot = boot(gpa, fit.fn, R = 1000)
fit.boot
boot.ci(fit.boot, type = "perc", index = 1) # intercept
boot.ci(fit.boot, type = "perc", index = 2) # act

# The SE from the bootstrap estimates are higher than the linear model
# The 95% CI from the bootstrap is wider than the linear model

# question 2 (a) ----
# Examine the three store-related variables in the data — StoreID, STORE, and Store7. Should
# all the three variables be in the model? Regardless of your conclusion, use only StoreID among
# the three, and treat it as a categorical predictor for the remainder of this problem.

library(ISLR)
names(OJ)
OJ = subset(OJ, select = c(Purchase, StoreID, Store7, STORE))
str(OJ)
pairs(OJ)
fit = glm(Purchase~., family = binomial, data = OJ)
summary(fit)
rm(OJ)

# It appears that only StoreID and factor level "No" (intercept) from
# Store7 have any significance when all predictors are used.
# examined all three, but use StoreID among the three and drop the rest
newOJ = subset(OJ, select = c(-Store7, -STORE))

# I do not see any reason to force MM = 1 and CH = 0
# When I experimented, I was able to produce the same results
newOJ$StoreID = as.factor(newOJ$StoreID)
str(newOJ)
# split data 50/50, train/test
set.seed(1)
n = nrow(newOJ)
sampler = sample(1:n, n/2) # n/2 is the 50/50 splitter
train = newOJ[sampler, ]
test = newOJ[-sampler, ]

# question 2 (b) ----
# Fit a logistic regression model. In addition, compute the confusion matrix, sensitivity, specificity,
# and overall misclassification rate based on the data, plot the ROC curve, and provide an estimate
# of the test error rate using 10-fold cross-validation.

# 10 fold cv
  # library(crossval) # could not make sense of its inner workings
  # Even their examples needed tailoring, and even after that
  # it was not very intuitive.  I instead proceed with Caret package.
  # When using K-fold CV, we use the original data set unsplit.
  # However, I split the data beforehand so I could make a predict set
  # with the test data.  But in general this package's function should
  # do its own K splits as it trains/tests on the data.
  # A rather robust way of estimating accuracy (or test error rate for us)
library(caret)
set.seed(1)
tc <- trainControl(method = "cv", number = 10, verboseIter = F)
model <- train(Purchase~., train,
               method="glm", family="binomial", trControl = tc)
model
# 10-fold CV estimated test error rate = 1 - overall misclassification
# also, this is 1 - accuracy = test error rate (estimated)
glm.err = 1 - model$results[,2]
prob = predict(model, test, type = "prob")
# pred = predict(model, test) # or
pred <- ifelse(prob$MM >= 0.5, "MM", "CH")
con.mat = table(test[, "Purchase"], pred)
confusionMatrix(con.mat)
library(pROC)
roc <- roc(test[, "Purchase"], prob$MM, levels = c("MM","CH"))
plot(roc, legacy.axes = T)

# question 2 (c) ----
# now do lda

# 10 fold cv
library(caret)
set.seed(1)
model <- train(Purchase~., train,
               method="lda", trControl = tc)
model
lda.err = 1 - model$results[,2]
prob = predict(model, test, type = "prob")
pred <- ifelse(prob$MM >= 0.5, "MM", "CH")
confusionMatrix(table(test$Purchase, pred))
library(pROC)
roc <- roc(test$Purchase, prob$MM, levels = c("MM","CH"))
plot(roc, legacy.axes = T)

# question 2 (d) ----
# now do qda

# 10 fold cv
library(caret)
set.seed(1)
model <- train(Purchase~., train,
               method="qda", trControl = tc)
model
qda.err = 1 - model$results[,2]
prob = predict(model, test, type = "prob")
pred <- ifelse(prob$MM >= 0.5, "MM", "CH")
confusionMatrix(table(test$Purchase, pred))
library(pROC)
roc <- roc(test$Purchase, prob$MM, levels = c("MM","CH"))
plot(roc, legacy.axes = T)

# question 2 (e) ----
# do KNN here

# 10 fold cv
library(caret)
set.seed(1)
model <- train(Purchase~., train,
               method="knn", trControl = tc)
model # optimal k is 5, caret automatically uses this k
knn.err = 1 - model$results[,2]
prob = predict(model, test, type = "prob")
pred <- ifelse(prob$MM >= 0.5, "MM", "CH")
confusionMatrix(table(test$Purchase, pred))
library(pROC)
roc <- roc(test$Purchase, prob$MM, levels = c("MM","CH"))
plot(roc, legacy.axes = T)

# question 2 (f) ----
# compare the test error rates from each model

rbind(glm.err, lda.err, qda.err, knn.er = min(knn.err))


# quesiton 3 (a) ----
# Perform an exploratory analysis of the data.

library(ISLR)
library(leaps)
set.seed(1)
auto = subset(Auto, select = -name)
# eda
str(auto)
summary(auto)
pairs(subset(auto))
cor(auto)
# seems to be right skewed
hist(auto$mpg)

# here we look at what would be worthy predictor variables.  However,
# we are using all of them and whittling down with variable selection methods.

# question 3 (b) ----
# fit a MLR
fit = lm(mpg~., data = auto)
summary(fit)
# lets refit using only significant values seen in summary
anova(fit, lm(mpg~.-cylinders-horsepower-acceleration, data = auto))
# we reject the null saying they are equal thus we use the reduced model
fit = lm(mpg~.-cylinders-horsepower-acceleration, data = auto)
names(summary(fit))

# question 3 (c) ----
# best subset method

totpred <- ncol(auto) - 1
fit.full <- regsubsets(mpg~., auto, nvmax = totpred)
fit.summary <- summary(fit.full)
fit.summary
best.rse = fit.summary$rsq

# Plot model fit measures for best model of each size against size
par(mfrow = c(2, 2))

# rsq
plot(fit.summary$rsq, xlab = "Number of Variables", ylab = "RSQ", 
     type = "l")
point = as.numeric(which.max(fit.summary$rsq))
points(point, fit.summary$rsq[point],
       col = "red", cex = 2, pch = 8)
# Adjusted R^2
plot(fit.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", 
     type = "l")
point = as.numeric(which.max(fit.summary$adjr2))
points(point, fit.summary$adjr2[point],
       col = "red", cex = 2, pch = 8)

# rsq says our best model is all predictors, this is bc it lessens the error
# with each additional predictor.  Not a good indicator.
# Adjr2 says our very best is at 6, however the elbow can be seen around 2 or 3
# usually the elbow is our best bet with consideration to complexity

# Get coefficients of best model for a given size
coef(fit.full, 6)

# question 3 (d) ----
# forward stepwise selection

fit.fwd = regsubsets(mpg~., auto, nvmax = totpred, method = "forward")
fit.summary <- summary(fit.fwd)
fit.summary

# Plot model fit measures for best model of each size against size
# rsq
plot(fit.summary$rsq, xlab = "Number of Variables", ylab = "RSQ", 
     type = "l")
point = as.numeric(which.max(fit.summary$rsq))
points(point, fit.summary$rsq[point],
       col = "red", cex = 2, pch = 8)
# Adjusted R^2
plot(fit.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", 
     type = "l")
point = as.numeric(which.max(fit.summary$adjr2))
points(point, fit.summary$adjr2[point],
       col = "red", cex = 2, pch = 8)

# Get coefficients of best model for a given size
coef(fit.fwd, 6)

# question 3 (e) ----
# backward stepwise selection

fit.bwd = regsubsets(mpg~., auto, nvmax = totpred, method = "backward")
fit.summary <- summary(fit.fwd)
fit.summary

# Plot model fit measures for best model of each size against size
# rsq
plot(fit.summary$rsq, xlab = "Number of Variables", ylab = "rsq", 
     type = "l")
point = as.numeric(which.max(fit.summary$rsq))
points(point, fit.summary$rsq[point],
       col = "red", cex = 2, pch = 8)
# Adjusted R^2
plot(fit.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", 
     type = "l")
point = as.numeric(which.max(fit.summary$adjr2))
points(point, fit.summary$adjr2[point],
       col = "red", cex = 2, pch = 8)

# Get coefficients of best model for a given size
coef(fit.bwd, 6)

# question 3 (f) ----
# we compare each model using LOOCV from caret

library(caret)
set.seed(1)
tc <- trainControl(method = "loocv", verboseIter = F)
model <- train(mpg~., auto,
               method="lm", trControl = tc)
model
# lowest RMSE is 2.556369
prob = predict(model, auto)
# pred = predict(model, test) # or
con.mat = table(auto$mpg, prob)
con.mat

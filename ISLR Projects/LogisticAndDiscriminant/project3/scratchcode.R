# Project 3, STAT 6340 scratch code
set.seed(1)
# library and function for plotting decision boundaries later
library(caret)
decisionplot <- function(model, data, class = NULL, predict_type = "class",
                         resolution = 175, showgrid = TRUE, ...) {
  
  if(!is.null(class)) cl <- data[,class] else cl <- 1
  data <- data[,1:2]
  k <- length(unique(cl))
  
  plot(data, col = as.integer(cl)+1L, pch = as.integer(cl)+1L, ...)
  
  # make grid
  r <- sapply(data, range, na.rm = TRUE)
  xs <- seq(r[1,1], r[2,1], length.out = resolution)
  ys <- seq(r[1,2], r[2,2], length.out = resolution)
  g <- cbind(rep(xs, each=resolution), rep(ys, time = resolution))
  colnames(g) <- colnames(r)
  g <- as.data.frame(g)
  
  ### guess how to get class labels from predict
  ### (unfortunately not very consistent between models)
  p <- predict(model, g, type = predict_type)
  if(is.list(p)) p <- p$class
  p <- as.factor(p)
  
  if(showgrid) points(g, col = as.integer(p)+1L, pch = ".")
  
  z <- matrix(as.integer(p), nrow = resolution, byrow = TRUE)
  # contour(xs, ys, z, add = TRUE, drawlabels = FALSE,
  #         lwd = 1, levels = (1:(k-1))+.5)
  
  invisible(z)
}
# ,out.extra='angle=90'
# flips figures 90 degrees

# question 1, part a ----
# explore the data, find a response variable for prediction
library(corrplot)
admission = read.csv("admission.csv", header = T)
# delete out the useless x varialbes
admission$X = admission$X.1 = admission$X.2 = admission$X.3 <- NULL
head(admission)
str(admission)
summary(admission)
pairs(subset(admission))
round(cor(subset(admission)), 2)
corrplot(cor(subset(admission)), method = "number", type = "upper")

# question 1 part b ----
# apply lda, make a decision boundary, and compute confusion matrix

# put equation for decision boundary here

library(MASS)
attach(admission)
library(dplyr)
# first we make a logical vector to sort out our test and train data
# since we want the last 5 of each Group category we must filter by category
admission_test = dplyr::bind_rows(tail(dplyr::filter(admission, Group==1), 5),
                                  tail(dplyr::filter(admission, Group==2), 5),
                                  tail(admission, 5))
# now we make a new vector, a logic vector and set all values in the test set to TRUE
admission_test$logic = TRUE
# merge the logic vector into admission and call it merger, this now sets the test obs to true in admission
merger = dplyr::left_join(admission, admission_test)
# flip the values of NA to true and flip the test true values to false.  T = train, F = test
merger$logic = is.na(merger$logic)
# now we construct our train/test sets from the logic vector
train = cbind(GPA, GMAT, Group)[merger$logic, ]
test = cbind(GPA, GMAT, Group)[!merger$logic, ]
train = as.data.frame(train)
test = as.data.frame(test)

# now we can perform LDA and superimpose the decision boundary!
lda.fit <- lda(Group ~ GPA + GMAT, data = train)
lda.pred <- predict(lda.fit, test)
n.grid <- 50
x1.grid <- seq(f = min(test[, 1]), t = max(test[, 1]), l = n.grid)
x2.grid <- seq(f = min(test[, 2]), t = max(test[, 2]), l = n.grid)
grid <- expand.grid(x1.grid, x2.grid)
colnames(grid) <- colnames(test[,1:2])
pred.grid <- predict(lda.fit, grid)

par(mfrow = c(1,2))
model <- lda(Group ~ GPA+GMAT, data=train) # simple and pretty version for graphing
# plot on test set with decision boundaries
prob1 <- matrix(pred.grid$posterior[, 1], nrow = n.grid, ncol = n.grid, byrow = F)
prob2 <- matrix(pred.grid$posterior[, 2], nrow = n.grid, ncol = n.grid, byrow = F)
plot(test[,1:2], col = ifelse(test$Group != 1, ifelse(test$Group == 2, "green", "blue"), "red"))
contour(x1.grid, x2.grid, prob1, levels = 0.5, labels = "", xlab = "", ylab = "", 
        main = "", add = T)
contour(x1.grid, x2.grid, prob2, levels = 0.5, labels = "", xlab = "", ylab = "", 
        main = "", add = T)
legend("bottomright", legend=c("2, No", "3, Maybe", "1, Yes"),
       col=c("green", "blue", "red"), lty=1, cex=0.8, bg="transparent")
title("Test Data")
decisionplot(model, test, class = "Group", main = "LDA")
# now let us see what the plot looks like on the training set
plot(train[,1:2], col = ifelse(train$Group != 1, ifelse(train$Group == 2, "green", "blue"), "red"))
contour(x1.grid, x2.grid, prob1, levels = 0.5, labels = "", xlab = "", ylab = "", 
        main = "", add = T)
contour(x1.grid, x2.grid, prob2, levels = 0.5, labels = "", xlab = "", ylab = "", 
        main = "", add = T)
legend("topleft", legend=c("2, No", "3, Maybe", "1, Yes"),
       col=c("green", "blue", "red"), lty=1, cex=0.8, bg="transparent")
title("Training Data")
decisionplot(model, train, class = "Group", main = "LDA")
# Cool it matches the handout in elearning :D
# now let us see on the full data
plot(admission[,1:2], col = ifelse(admission$Group != 1, ifelse(admission$Group == 2, "green", "blue"), "red"))
contour(x1.grid, x2.grid, prob1, levels = 0.5, labels = "", xlab = "", ylab = "", 
        main = "", add = T)
contour(x1.grid, x2.grid, prob2, levels = 0.5, labels = "", xlab = "", ylab = "", 
        main = "", add = T)
legend("topleft", legend=c("2, No", "3, Maybe", "1, Yes"),
       col=c("green", "blue", "red"), lty=1, cex=0.8, bg="transparent")
title("Admission Data")
decisionplot(model, admission, class = "Group", main = "LDA")

# the confusion matrix for train
lda.pred.train <- predict(lda.fit, train)
con.mat.train = table(lda.pred.train$class, train$Group)
con.mat.train
# everything about how the following happens is noted under the comments in the confusion matrix for test
class1MC = sum(con.mat.train[2],con.mat.train[3])/sum(con.mat.train)
class2MC = sum(con.mat.train[4],con.mat.train[6])/sum(con.mat.train)
class3MC = sum(con.mat.train[7],con.mat.train[8])/sum(con.mat.train)
sum(class1MC,class2MC,class3MC)

# the confusion matrix for test
con.mat.test = table(lda.pred$class, test$Group)
con.mat.test
# Overall misclassification needs to be calculated such that we:
# for each column j, the sum of values along i when i != j is divided by sum of all elements
# for the misclassification of class "1", we would sum rows 2 and 3 along column 1 over total obs
# like this: (0+1)/sum(con.mat.test) = misclassification of 1.
# we then add each class's misclassification up (because of same denominator) and get total
class1MC = sum(con.mat.test[2],con.mat.test[3])/sum(con.mat.test)
class2MC = sum(con.mat.test[4],con.mat.test[6])/sum(con.mat.test)
class3MC = sum(con.mat.test[7],con.mat.test[8])/sum(con.mat.test)
LDAmc = sum(class1MC,class2MC,class3MC)

# We notice that the misclassification occurs 20% of the time in the test set, this is relatively high.
# whereas the misclassification for train was 0.04285714.  This is expected since the model fit was
# done on the training set



# question 1 part c ----
# now we can perform QDA and superimpose the decision boundary!
model <- qda(Group~ GPA + GMAT, data = train)
qda.fit <- qda(Group ~ GPA + GMAT, data = train)
qda.pred <- predict(qda.fit, test)
pred.grid = predict(qda.fit, grid)

# plot on test set with decision boundaries
prob1 <- matrix(pred.grid$posterior[, 1], nrow = n.grid, ncol = n.grid, byrow = F)
prob2 <- matrix(pred.grid$posterior[, 2], nrow = n.grid, ncol = n.grid, byrow = F)
plot(test[,1:2], col = ifelse(test$Group != 1, ifelse(test$Group == 2, "green", "blue"), "red"))
contour(x1.grid, x2.grid, prob1, levels = 0.5, labels = "", xlab = "", ylab = "", 
        main = "", add = T)
contour(x1.grid, x2.grid, prob2, levels = 0.5, labels = "", xlab = "", ylab = "", 
        main = "", add = T)
legend("bottomright", legend=c("2, No", "3, Maybe", "1, Yes"),
       col=c("green", "blue", "red"), lty=1, cex=0.8, bg="transparent")
title("Test Data")
decisionplot(model, test, class = "Group", main = "QDA")
# now let us see what the plot looks like on the training set
plot(train[,1:2], col = ifelse(train$Group != 1, ifelse(train$Group == 2, "green", "blue"), "red"))
contour(x1.grid, x2.grid, prob1, levels = 0.5, labels = "", xlab = "", ylab = "", 
        main = "", add = T)
contour(x1.grid, x2.grid, prob2, levels = 0.5, labels = "", xlab = "", ylab = "", 
        main = "", add = T)
legend("topleft", legend=c("2, No", "3, Maybe", "1, Yes"),
       col=c("green", "blue", "red"), lty=1, cex=0.8, bg="transparent")
title("Training Data")
decisionplot(model, train, class = "Group", main = "QDA")
# now let us see on the full data
plot(admission[,1:2], col = ifelse(admission$Group != 1, ifelse(admission$Group == 2, "green", "blue"), "red"))
contour(x1.grid, x2.grid, prob1, levels = 0.5, labels = "", xlab = "", ylab = "", 
        main = "", add = T)
contour(x1.grid, x2.grid, prob2, levels = 0.5, labels = "", xlab = "", ylab = "", 
        main = "", add = T)
legend("topleft", legend=c("2, No", "3, Maybe", "1, Yes"),
       col=c("green", "blue", "red"), lty=1, cex=0.8, bg="transparent")
title("Admission Data")
decisionplot(model, admission, class = "Group", main = "QDA")

# the confusion matrix for train
qda.pred.train <- predict(qda.fit, train)
con.mat.train = table(qda.pred.train$class, train$Group)
con.mat.train
class1MC = sum(con.mat.train[2],con.mat.train[3])/sum(con.mat.train)
class2MC = sum(con.mat.train[4],con.mat.train[6])/sum(con.mat.train)
class3MC = sum(con.mat.train[7],con.mat.train[8])/sum(con.mat.train)
sum(class1MC,class2MC,class3MC)

# the confusion matrix for test
con.mat.test = table(qda.pred$class, test$Group)
con.mat.test
class1MC = sum(con.mat.test[2],con.mat.test[3])/sum(con.mat.test)
class2MC = sum(con.mat.test[4],con.mat.test[6])/sum(con.mat.test)
class3MC = sum(con.mat.test[7],con.mat.test[8])/sum(con.mat.test)
QDAmc = sum(class1MC,class2MC,class3MC)

# We notice that the misclassification occurs 13% of the time in the test set, this is lower than lda.
# The misclassification for train was 0.02857143.  This is also lower than lda's.

# question 1 part d ----
library(class)
# now we can perform KNN and superimpose the decision boundary!
ks = c(seq(1, nrow(test), by = 1))
nks = length(ks)
err.rate.train = numeric(length = nks)
err.rate.test = numeric(length = nks)
names(err.rate.train) = names(err.rate.test) = ks
for (i in seq(along = ks)) {
  mod.train = knn(train[,1:2], train[,1:2], train$Group, k = ks[i])
  mod.test = knn(train[,1:2], test[,1:2], train$Group, k = ks[i])
  err.rate.train[i] = 1 - sum(mod.train == train$Group)/length(train$Group)
  err.rate.test[i] = 1 - sum(mod.test == test$Group)/length(test$Group)
}
# Now we want to find the optimal k using a min function
result <- data.frame(ks, err.rate.train, err.rate.test)
result[err.rate.test == min(result$err.rate.test), ]
# It appears we can get away with k = 6, our optimal k
knn.fit <- knn(train[,1:2], test[,1:2], train$Group, k = 6, prob = T)
# the following knn.prob would really only get used for an ROC curve, nonetheless its nice to see it work for this data
knn.prob <- attr(knn.fit, "prob") # prob is voting fraction for winning class
# the following is not the same as knn.prob <- ifelse(knn.fit == 1, knn.prob, 1 - knn.prob)
knn.prob <- ifelse(knn.fit != 1, 
                   ifelse(knn.fit == 2, 1-knn.prob[knn.fit==2],1-knn.prob[knn.fit==3]),
                   knn.prob) # now it is voting fraction for Group == 1
# We now have enough information to compute the confusion matrix
# the confusion matrix for train
knn.fit.train <- knn(train[,1:2], train[,1:2], train$Group, k = 6, prob = T)
con.mat.train = table(knn.fit.train, train$Group)
con.mat.train
class1MC = sum(con.mat.train[2],con.mat.train[3])/sum(con.mat.train)
class2MC = sum(con.mat.train[4],con.mat.train[6])/sum(con.mat.train)
class3MC = sum(con.mat.train[7],con.mat.train[8])/sum(con.mat.train)
sum(class1MC,class2MC,class3MC)

# the confusion matrix for test
con.mat.test = table(knn.fit, test$Group)
con.mat.test
class1MC = sum(con.mat.test[2],con.mat.test[3])/sum(con.mat.test)
class2MC = sum(con.mat.test[4],con.mat.test[6])/sum(con.mat.test)
class3MC = sum(con.mat.test[7],con.mat.test[8])/sum(con.mat.test)
KNNmc = sum(class1MC,class2MC,class3MC)

# We notice that the misclassification occurs 33% of the time in the test set, this is worse than lda and qda.
# The misclassification for train was 0.3.  This is nearly as bad as its test set!

# now plot for knn
model <- knn3(Group ~ GPA+GMAT, data=train, k = 6)
pred.grid = predict(model, grid)

# plot on test set with decision boundaries
prob1 <- matrix(pred.grid[, 1], nrow = n.grid, ncol = n.grid, byrow = F)
prob2 <- matrix(pred.grid[, 2], nrow = n.grid, ncol = n.grid, byrow = F)
plot(test[,1:2], col = ifelse(test$Group != 1, ifelse(test$Group == 2, "green", "blue"), "red"))
contour(x1.grid, x2.grid, prob1, levels = 0.5, labels = "", xlab = "", ylab = "", 
        main = "", add = T)
contour(x1.grid, x2.grid, prob2, levels = 0.5, labels = "", xlab = "", ylab = "", 
        main = "", add = T)
legend("bottomright", legend=c("2, No", "3, Maybe", "1, Yes"),
       col=c("green", "blue", "red"), lty=1, cex=0.8, bg="transparent")
title("Test Data")
decisionplot(model, test, class = "Group", main = "kNN (6)")
# it does not look like the decision boundary worked correctly on either method here
# We have suppressed the next graphs due to this problem.

# question 1 part e ----
models = c("Misclassification test Rate")
rbind(models, LDAmc, QDAmc, KNNmc)
# QDA gives a nice result, but LDA isn't very far off and is technically a simpler method.
# I would personally recommend LDA on the basis of simplicity as well as leniency
# for the no and maybe categories :)



# question 2 part a ----
library(corrplot)
bankruptcy = read.csv("bankruptcy.csv", header = T)
# delete out the useless x varialbes
bankruptcy$X = bankruptcy$X.1 <- NULL
head(bankruptcy)
str(bankruptcy)
summary(bankruptcy)
pairs(subset(bankruptcy))
round(cor(subset(bankruptcy)), 2)
corrplot(cor(subset(bankruptcy)), method = "number", type = "upper")
# it appears that all variables but x4 would be good to predict group with

# question 2 part b ----
attach(bankruptcy)
fit1 <- glm(Group ~., family = binomial, data = bankruptcy)
summary(fit1)
# par(mfrow = c(2,2))
# plot(fit1)
fit2 = glm(Group ~X1+X3, family = binomial, data = bankruptcy)
summary(fit2)
fit3 = glm(Group ~ 1, family = binomial, data = bankruptcy)
summary(fit3)
# compare fit 2 and 1
anova(fit2, fit1, test = "Chisq")
# since we accept the H0 we keep the reduced model fit2
# compare fit 2 with fit 3 (null model)
anova(fit2, fit3, test = "Chisq")
# since we reject the H0 we keep the full model fit2
# Since we are using all of the data as training data, there is no need to split
# now we interpret the regression coefficients...


# question 3 part a ----
# put latex equation for decision boundary here

# if we proceed with our training data == test data then our following
# outcomes on each model will be the same (tested) almost as if it were copied and pasted
# so we will split the data down the middle, 50/50 training/testing.  
# There is no motivation for using 50/50, simply using for fun

# split data 50/50, train/test
set.seed(1)
n = nrow(bankruptcy)
sampler = sample(1:n, n/2) # n/2 is the 50/50 splitter
train = bankruptcy[sampler, ]
test = bankruptcy[-sampler, ]

# create model
fit = glm(Group ~X1+X3, family = binomial, data = train)

# Estimated probabilities for test data
prob <- predict(fit, test, type = "response")

# Predicted classes (using 0.5 cutoff)
pred <- ifelse(prob >= 0.5, "nonbankrupt", "bankrupt")

# Test error rate
1 - mean(pred == test[, "Group"])

# Confusion matrix and (sensitivity, specificity)
# `+' = nonbankrupt, `-' = bankrupt
con.mat = table(pred, test[, "Group"])
con.mat

#       PRED CLASS
# TRUE     TN FP
# CLASS    FN TP
# Sensitivity, TP/P = 1
11/(11+0)
con.mat[4]/sum(con.mat[3],con.mat[4])
# Specificity, TN/N = 0.9167
11/(11+1)
con.mat[1]/sum(con.mat[1],con.mat[2])
# Overall misclassification, (FN+FP)/(N+P) = 0.0435
# or (1-sens)*[P/(P+N)]+(1-spec)*[N/(P+N)] = 0.0435
(1+0)/(12+11)
sum(con.mat[2],con.mat[3])/sum(con.mat)
# very very low misclassification
# either we have overly fit the model or our train/test split is not good
# or our model is just that good...

# ROC curve
# case = '+' (or nonbankrupt, 1) , control = '-' (or bankrupt, 0)
library(pROC)
roc <- roc(test[, "Group"], prob, levels = c(0, 1))
plot(roc, legacy.axes = T)
lr_red = c("LR reduced", sum(con.mat[2],con.mat[3])/sum(con.mat), roc$auc)

# question 3 part b ----
# put latex equation for decision boundary here

# create model with all predictors
fit = glm(Group ~., family = binomial, data = train)

# Estimated probabilities for test data
prob <- predict(fit, test, type = "response")

# Predicted classes (using 0.5 cutoff)
pred <- ifelse(prob >= 0.5, "nonbankrupt", "bankrupt")

# Test error rate
1 - mean(pred == test[, "Group"])

# Confusion matrix and (sensitivity, specificity)
# `+' = nonbankrupt, `-' = bankrupt
con.mat = table(pred, test[, "Group"])
con.mat

#       PRED CLASS
# TRUE     TN FP
# CLASS    FN TP
# Sensitivity, TP/P = 1
con.mat[4]/sum(con.mat[3],con.mat[4])
# Specificity, TN/N = 0.75
con.mat[1]/sum(con.mat[1],con.mat[2])
# Overall misclassification, (FN+FP)/(N+P) = 0.1304348
# or (1-sens)*[P/(P+N)]+(1-spec)*[N/(P+N)] = 0.1304348
sum(con.mat[2],con.mat[3])/sum(con.mat)

# ROC curve
# case = '+' (or nonbankrupt, 1) , control = '-' (or bankrupt, 0)
roc <- roc(test[, "Group"], prob, levels = c(0, 1))
plot(roc, legacy.axes = T)
lr_ful = c("LR full", sum(con.mat[2],con.mat[3])/sum(con.mat), roc$auc)

# Misclassification is much higher than the previous model
# It would appear that variable selection plays a large role in making a good model
# Our specificity has fallen greatly due to this overfitted model
# but our sensitivity is the same.  Also the ROC curve is further from the left corner
# which indicates a drop in performance using this model
# question 3 part c ----
# put latex equation for decision boundary here

# create model with all predictors
library(MASS)
fit = lda(Group ~ ., data = train)

# Estimated probabilities for test data
# lda function already has the next steps within, so we can compute con.mat directly from this prob
prob <- predict(fit, test) # seems to result in same output with/without type = "response"

# Predicted classes (using 0.5 cutoff)
pred <- ifelse(prob$posterior[,2] >= 0.5, "nonbankrupt", "bankrupt")

# Test error rate
1 - mean(pred == test[, "Group"])

# Confusion matrix and (sensitivity, specificity)
# `+' = nonbankrupt, `-' = bankrupt
con.mat = table(pred, test[, "Group"])
con.mat

#       PRED CLASS
# TRUE     TN FP
# CLASS    FN TP
# Sensitivity, TP/P = 1
con.mat[4]/sum(con.mat[3],con.mat[4])
# Specificity, TN/N = 0.67
con.mat[1]/sum(con.mat[1],con.mat[2])
# Overall misclassification, (FN+FP)/(N+P) = 0.173913
# or (1-sens)*[P/(P+N)]+(1-spec)*[N/(P+N)] = 0.173913
sum(con.mat[2],con.mat[3])/sum(con.mat)

# ROC curve
# case = '+' (or nonbankrupt, 1) , control = '-' (or bankrupt, 0)
library(pROC)
roc <- roc(test[, "Group"], prob$posterior[,2], levels = c(0, 1))
plot(roc, legacy.axes = T)
LDA = c("LDA", sum(con.mat[2],con.mat[3])/sum(con.mat), roc$auc)

# question 3 part d ----
# put latex equation for decision boundary here

# create model with all predictors
library(MASS)
fit = qda(Group ~ ., data = train)

# Estimated probabilities for test data
# qda function already has the next steps within, so we can compute con.mat directly from this prob
prob <- predict(fit, test) # seems to result in same output with/without type = "response"

# Predicted classes (using 0.5 cutoff)
pred <- ifelse(prob$posterior[,2] >= 0.5, "nonbankrupt", "bankrupt")

# Test error rate
1 - mean(pred == test[, "Group"])

# Confusion matrix and (sensitivity, specificity)
# `+' = nonbankrupt, `-' = bankrupt
con.mat = table(pred, test[, "Group"])
con.mat

#       PRED CLASS
# TRUE     TN FP
# CLASS    FN TP
# Sensitivity, TP/P = 0.9090909
con.mat[4]/sum(con.mat[3],con.mat[4])
# Specificity, TN/N = 0.6666667
con.mat[1]/sum(con.mat[1],con.mat[2])
# Overall misclassification, (FN+FP)/(N+P) = 0.2173913
# or (1-sens)*[P/(P+N)]+(1-spec)*[N/(P+N)] = 0.2173913
sum(con.mat[2],con.mat[3])/sum(con.mat)

# ROC curve
# case = '+' (or nonbankrupt, 1) , control = '-' (or bankrupt, 0)
library(pROC)
roc <- roc(test[, "Group"], prob$posterior[,2], levels = c(0, 1))
plot(roc, legacy.axes = T)
QDA = c("QDA", sum(con.mat[2],con.mat[3])/sum(con.mat), roc$auc)

# question 3 part e ----
# From previous parts we look at who has the best AUC (area under the curve) and lowest misclassification
# To summarize here are the results
names = c("Model", "Misclassification Rate", "Area Under the Curve")
models = rbind(names, lr_red, lr_ful, LDA, QDA)
models
# It appears that the reduced logistic regression model is the best selection of all choices
# due to having small misclassification rate and high AUC
# This is a nice expectation because logistic regression is very good at modelling binary responses
# We only have bankrupt and not bankrupt to fit.  It also makes sense that the reduced model 
# did better than the full model since it avoided overfitting problems typically associated
# with using all predictors.
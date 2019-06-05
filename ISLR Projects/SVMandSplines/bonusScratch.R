admission = read.csv("admission.csv", header = T)
lidar = read.csv("lidar.csv", header = T)
library(ISLR)
caravan = Caravan
# s.caravan = scale(caravan[, -86]) #86 column is purchase
# test = 1:1000
# train = (-test)
# xtrain = caravan[train,]
# xtest = caravan[test,]
# ytrain = caravan$Purchase[train]
# ytest = caravan$Purchase[test]

toyify = function(data) {
  set.seed(1)
  sampler = sample(1:nrow(data), 582)
  data[sampler,]
}

toyCaravan = toyify(caravan)
test = 1:100
train = (-test)
xtrain = caravan[train,-86]
xtest = caravan[test,]
ytrain = caravan$Purchase[train]
ytest = caravan$Purchase[test]

library(e1071)
attach(toyCaravan)
svmfit = svm(Purchase~., data = toyCaravan, kernel = "linear", cost = 0.1, scale = T) 
summary(svmfit)

# for question 1(a) ----
tune.out = tune(svm, Purchase~., data = toyCaravan, kernel = "linear",
                ranges = list(cost = c(0.001,0.01,0.1,1,5,10,100)))
summary(tune.out)
bestmod = tune.out$best.model
pred = predict(bestmod, xtest)
caret::confusionMatrix(pred,ytest, positive = 'Yes')

# question 1(b) ----
tune.out = tune(svm, Purchase~., data = toyCaravan, kernel = "polynomial", degree = 2,
                ranges = list(cost = c(0.001,0.01,0.1,1,5,10,100)))
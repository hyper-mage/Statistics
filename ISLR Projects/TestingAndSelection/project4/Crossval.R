# 10 fold cv
library(crossval)
# set up lda prediction function
predfun.lda = function(train.x, train.y, test.x, test.y, negative)
{
  require("MASS") # for lda function
  
  lda.fit = lda(train.x, grouping=train.y)
  ynew = predict(lda.fit, test.x)$class
  
  # count TP, FP etc.
  out = confusionMatrix(table(test.y, ynew), negative = negative)
  
  return( out )
}
X = as.matrix(sleep[,1, drop=FALSE]) # increase in hours of sleep
Y = sleep[,2] # drug given
cv.out = crossval(predfun.lda, X, Y, K=5, B=20, negative="control",verbose = F)

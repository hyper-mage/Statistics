We explore some new methods in this project. Namely, Lasso and Ridge regression as well as
Principal Component Analysis (PCA), Principal Component regression, and Partial Least Squares.

Ridge and Lasso introduce a penalty function based on a distance metric. Ridge uses L2 norm (squared)
whereas Lasso uses L1 norm (absoluted). This penalty function helps weight each variable by
importance in the model, those that are less important tend to zero. Lasso has an additional
characteristic, variable selection because some predictors can truly be zero given the method.

PCA allows us to dimensionalize our data if we have far too many predictors or our # of predictors
exceeds our sample size. PCA also shows us our most important predictors and how they relate with
one another. An added bonus, PCA naturally seeks to find the most information from our dataset
while reducing complexity. This gives us the notion of how much variance can be explained.

PCR and PLS are tools for modeling datasets using components instead of predictors.
These components have some proportion of variance that explains the dataset as well as the
response. This is how we determine how effective the model can be for testing sets. Too much
variance explained can lead to overfitting, so caution is always advised. Reducing our complexity
is both a way to avoid overfitting and a way to simplify our model.

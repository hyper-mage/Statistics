The aim of this project is to experiment with different testing methods and variable selection.

We use modeling techniques familiar to us from the previous projects: Linear regression,
logistic regression, LDA, QDA, and KNN.

Bootstrapping is a technique for optimizing our test error rate. The idea is simple but effective.
Simply resample your dataset a large amount of times, randomly, and produce an average of all
the resamples of your desired statistic. This asymptotically approaches the underlying mean
from your distribution of your dataset. The con here is you can never get an outlying observation.
In other words you can never predict what you cannot expect.

Cross validation is an extension of a test/train split routine. It has much more editability behind
it. To understand CV we first want to understand what is a test/train split. Typically, in most
machine learning set ups we have a dataset given to us and we want to learn from this dataset
in order to make predictions in the real world. In order to test our model we may seek to split
our data from the beginning so that there is a testing split which will act as unseen data for our
model. These splits can be anything from 50/50 to 80/20, the choice is rather amibiguous.
CV takes this method and makes it a bit more robust. A K-fold CV is the same exact thing but K
determines how many times we train and test. We split our data into K partitions, select a partition
(call it i) and train the other K-1 partitions, then test on the chosen ith partition, repeat K times.
We will have a test error rate for each K as well as an overall error rate. This helps alleviate
some of the ambiguity and deviation of a normal test/train split set up.

Variable selection is a techinque to determine the best model for a Linear Regression approach.
Best subset selection will always find the best model in regards to R-square and complexity.
However, it can be very impractical for large number of predictors.
Forward and backward stepwise selection will find a good model much faster in these cases.
Forward relates to adding predictors to a model while backwards starts with a full model.
The idea is the same for both, but can arrive at different answers. Simply optimize R-square gain
(or whatever method CP, AIC, BIC).

Project4.html will have the best format for checking out the project.

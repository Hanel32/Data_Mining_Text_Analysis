###############################################################################
# Generate a data set:                                                        #
#   - n = 1000                                                                #
#   - p = 2                                                                   #
#                                                                             #
#   Which can be interpreted as a 2nd degree polynomial with 1000 data points.#
#   notice; x1^2-x2^2 which is, indeed, a second degree.                      # 
###############################################################################
set.seed(1)
x1=runif(1000)-0.5
x2=runif(1000)-0.5
y=1*(x1^2-x2^2>0)

###############################################################################
# Create a training data set with 200 random samples. Plot the training obser-#
# vations, colored according to their class labels. Your plot should display  #
# X1 on the x-axis, and X2 on the y-axis.                                     #
###############################################################################
set.seed(1)
x1=runif(200)-0.5
x2=runif(200)-0.5
y=1*(x1^2-x2^2>0)
plot(x1, x2, col=(y+1), pch=(y+19), cex=2)

###############################################################################
# Fit a support vector classifier (tuned) to the data with X1 and X2 as predi-#
# ctors. Obtain a class prediction for each test observation. Plot the traini-#
# ng and test observations, colored according to the predicted class labels   #
#                                                                             #
# A note here:                                                                #
#    These data have a polynomial of order 2 relationship. A simpler linear   #
#    kernal is not going to be sufficient to correctly classify.              #
###############################################################################
# Import the SVM library
install.packages("e1071")
library(e1071)

# Bind x1, x2 into a column matrix, print it out (optionally)
x = cbind(x1, x2)
x

# Create a dataframe, print it out (optionally)
dat = data.frame(x=x, y=as.factor(y))
dat

# Create a test/train split
index     = 1:nrow(x)
testindex = sample(index, trunk(length(index)/3))
testset   = x[textindex,  ]
trainset  = x[-textindex, ]

# Tune the SVM
t.out=tune(svm, y~., data=dat, kernel="linear", ranges=list(cost=10^(-3:3)))
summary(t.out)

# The tune I got was:
#    - The best cost       : 10
#    - The best performance: 0.45
#
# Derive the best fit model, and plot.


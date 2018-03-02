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

# Prepare the data for the SVM
# Import the SVM library
install.packages("e1071")
library(e1071)

# Bind x1, x2 into a column matrix, print it out (optionally)
x = cbind(x1, x2)
x

# Create a dataframe, print it out (optionally)
dat = data.frame(x=x, y=as.factor(y))
dat

# Designate the sample for train
train=sample(200,100)

###############################################################################
# Fit a support vector classifier (tuned) to the data with X1 and X2 as predi-#
# ctors. Obtain a class prediction for each test observation. Plot the traini-#
# ng and test observations, colored according to the predicted class labels   #
#                                                                             #
# A note here:                                                                #
#    These data have a polynomial of order 2 relationship. A simpler linear   #
#    kernal is not going to be sufficient to correctly classify.              #
###############################################################################

# Tune the SVM, output the results
tl.out=tune(svm, y~., data=dat[train,], kernel="linear", ranges=list(cost=10^(-4:4)))
summary(tl.out)
bestl = tl.out$best.model
plot(bestl, dat)

# Get predictions for training set
lin_train_ypred = predict(bestl, newdata=dat[train,])
table(predict=lin_train_ypred, truth=dat[train, "y"])

# Get predictions for test set
lin_test_ypred  = predict(bestl, newdata=dat[-train,])
table(predict=lin_test_ypred, truth=dat[-train, "y"])

# Plot training set
plot(bestl, dat[train,])

# Plot testing set
plot(bestl, dat[-train,])

###############################################################################
# Fit a support vector classifier (tuned) using a polynomial kernel. Obtain a #
# class prediction for each training and test observation. Plot the training  #
# and test observations, colored according to predicted class labels.         #
###############################################################################

# Tune the SVM, output the results
tp.out=tune(svm, y~., data=dat[train,], kernel="polynomial", ranges=list(cost=10^(-1:2), degree=(1:4)))
summary(tp.out)
bestp = tp.out$best.model
plot(bestp, dat)

# Get predictions for training set
poly_train_ypred = predict(bestp, newdata=dat[train,])
table(predict=poly_train_ypred, truth=dat[train, "y"])

# Get predictions for test set
poly_test_ypred  = predict(bestp, newdata=dat[-train,])
table(predict=poly_test_ypred, truth=dat[-train, "y"])

# Plot training set
plot(bestp, dat[train,])

# Plot testing set
plot(bestp, dat[-train,])

###############################################################################
# Fit a support vector classifier (tuned) using a radial kernel. Obtain a     #
# class prediction for each training and test observation. Plot the training  #
# and test observations, colored according to predicted class labels.         #
###############################################################################

# Tune the SVM, output the results
tr.out=tune(svm, y~., data=dat[train,], kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(0.5, 1:4)))
summary(tr.out)
bestr = tr.out$best.model

# Get predictions for training set
radial_train_ypred = predict(bestr, newdata=dat[train,])
table(predict=radial_train_ypred, truth=dat[train, "y"])

# Get predictions for test set
radial_test_ypred  = predict(bestr, newdata=dat[-train,])
table(predict=radial_test_ypred, truth=dat[-train, "y"])

# Plot training set
plot(bestr, dat[train,])

# Plot testing set
plot(bestr, dat[-train,])


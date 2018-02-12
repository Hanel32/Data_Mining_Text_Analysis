library(MASS)
library(ISLR)

###############################################
#           Simple Linear Regression          #
###############################################
# Fix the data set                            #
fix(Boston)
names(Boston)
attach(Boston)

# Signify the appropriate data using Attach()
lm.fit=lm(medv~lstat, data=Boston)            # Fit the model
lm.fit                                        # Output Coefficients and lstat
summary(lm.fit)                               # Output a summary of the fit
names(lm.fit)                                 # Output the pieces of information stored in lm.fit
coef(lm.fit)                                  # Essentially the same as lm.fit in output

# Next, we can perform a confidence interval on the fit.
confint(lm.fit)                               # Outputs a 95% two-sided P-test with Mu and lstat

# Using predict(), we can predict confidence intervals for a given sequence.
predict(lm.fit, data.frame(lstat=(c(5, 10, 15))), interval="confidence")
predict(lm.fit, data.frame(lstat=(c(5, 10, 15))), interval="prediction")

# Now that we've got the fit, we can begin plotting.
plot(lstat, medv)                             # Plots the initial variables against eachother
abline(lm.fit)                                # Plots the A-B line produced by the linear fit
abline(lm.fit, lwd=3)                         # Increases the line width to 3 of the abline
abline(lm.fit, lwd=3, col="red")              # Changes the abline's color to red
plot(lstat, medv, col="red")                  # Plots lstat x medv in red
plot(lstat, medv, pch=20)                     # Plots lstat x medv with clean dots
plot(lstat, medv, pch="+")                    # Plots lstat x medv in addition operators
plot(1:20, 1:20, pch=1:20)                    # Just a nice little graphical plot of all options
                                              # of possible characters to plot with.

# In order to view multiple plots, we'll split the viewing space into four panes.
par(mfrow=c(2,2))                             # Creates four spaces for plotting
plot(lm.fit)                                  # Plots the first four variables of lm.fit

# Alternatively, you can compute and plot the residuals manually
plot(predict(lm.fit), residuals(lm.fit))      # Plots residuals against the predicted values
plot(predict(lm.fit), rstudent(lm.fit))       # Plots rstudent against the predicted values
plot(hatvalues(lm.fit))                       # Computes and plots leverage statistics
which.max(hatvalues(lm.fit))                  # Gives the maximum prediction of the fit model

###############################################
#         Multiple Linear Regression          #
###############################################
# Fit like lm(y~x1+x2+x3), essentially, your output and predictor variables.
lm.fit=lm(medv~lstat+age, data=Boston)        # Plot the multiple regression with lstat + age = medv
summary(lm.fit)                               # Output the linear fit summary

# Instead, to plot all variables as predictors for a singular variable...:
lm.fit=lm(medv~., data=Boston)                # Fits all variables as predictors to medv
summary(lm.fit)                               # Outputs the summary of the linear model

# In order to use vif(), must install car package
install.packages("car")
library(car)

# Calculate the Variance Inflation Factor of the linear model
vif(lm.fit)
lm.fit1=lm(medv~.-age, data=Boston)           # Fits all variables as predictors to medv, except age.
summary(lm.fit1)

# Alternatively, to update a model and remove a variable, you can do:
lm.fit1=update(lm.fit, ~.-age)

###############################################
#             Interaction Terms               #
###############################################
summary(lm(medv~lstat*age, data=Boston))      # Includes the interaction terms between lstat and age

###############################################
#  Non-Linear Transformations of Predictors   #
###############################################
lm.fit2=lm(medv~lstat+I(lstat^2))             # Make a linear model w/ lstat and lstat^2 as predictors
summary(lm.fit2)                              # Output a summary

# Note, the near-zero p-value of lstat^2 (.003) suggests it leads to an improved model
# In order to quantify its superiority over lstat, we can use anova()
lm.fit=lm(medv~lstat)                         # Fits lstat as a predictor to medv
anova(lm.fit, lm.fit2)                        # Compare the performance of the two linear models

par(mfrow=c(2,2))                             # Create a 2x2 panel for plotting
plot(lm.fit2)                                 # Plot the lstat^2 model

# Sometimes, we may want an even higher polynomial fit
lm.fit5=lm(medv~poly(lstat, 5))               # Fits medv
summary(fit5)

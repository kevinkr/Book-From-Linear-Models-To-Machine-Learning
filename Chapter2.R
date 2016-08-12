#Chapter 2
#Linear Regression Models
##
###
#2.6.3 Example: Bike-Sharing Data
#Let’s form some confidence intervals from the bike-sharing data
lmout <- lm(reg ~ temp+temp2+workingday+clearday,data = shar)
summary(lmout)
#
#Call:
#lm(formula = reg ~ temp + temp2 + workingday + clearday, data = shar)
#
#Residuals:
#  Min       1Q   Median       3Q      Max 
#-1919.02  -388.12    44.17   460.25  1497.08 

#Coefficients:
#            Estimate    Std. Error t value Pr(>|t|)    
#(Intercept)   -1362.56     232.82  -5.852 1.09e-08 ***
#  temp        11059.20     988.08  11.193  < 2e-16 ***
#  temp2       -7636.40    1013.90  -7.532 4.08e-13 ***
#  workingday    685.99      71.00   9.661  < 2e-16 ***
#  clearday      518.95      69.52   7.465 6.34e-13 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 626.3 on 360 degrees of freedom
#Multiple R-squared:  0.6548,	Adjusted R-squared:  0.651 
#F-statistic: 170.7 on 4 and 360 DF,  p-value: < 2.2e-16

#We estimate that a working day adds about 686 riders to the day’s ridership.
#An approximate 95% confidence interval for the population value for this
#effect is
#685.99 ± 1.96 · 71.00 = (546.83,825.15)
#This is a disappointingly wide interval, but it shouldn’t surprise us. 
#After all, it is based on only 365 data points.
lmout$residuals

#The R2 quantity in the output of lm() is a measure of how well our model
#predicts Y .

#The adjusted-R2 statistic is aimed at serving as a less biased version of the
#ordinary R2

####
#Cross validation
#
#First, a quick review of cross-validation: Say we have n observations in
#our data set. With cross-validation, we randomly partition the data into a
#training set and a validation set, of k and n - k observations, respectively��� k observations, respectively.
#We fit our model to the training set, and use the result to predict in the
#validation set, and then see how well those predictions turned out.

#Clearly there is an issue of the choice of k. If k is large, our validation
#set will be too small to obtain an accurate estimate of predictive ability.
#That is not a problem if k is small, but then we have a subtler problem:
#  We are getting an estimate of strength of our model when constructed on
#k observations, but in the end we wish to use all n observations.

#One solution is the Leaving One Out Method (LOOM). Here we set k = n − 1,
#but apply the training/validation process to all possible (n − 1,1) partitions. 
#The name alludes to the fact that LOOM repeatedly omits one observation, 
#predicting it from fitting the model to the remaining observation. 
#This gives us “the best of both worlds”: We have n validation points, 
#the best possible, and the training sets are of size n − 1, i.e., 
#nearly full-sized.

#There is an added benefit that the same code to implement this method 
#can be used to implement the jackknife. The latter is a resampling technique. 
#To see what it does, let’s look at a more general technique called the 
#bootstrap, which is a method to empirically compute standard errors.

#####
#Skip ahead to section 4.4.2
###

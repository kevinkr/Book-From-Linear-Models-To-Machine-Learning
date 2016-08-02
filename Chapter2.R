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

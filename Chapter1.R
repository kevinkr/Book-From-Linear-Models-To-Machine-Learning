library(freqparcoord)
data(mlb)
head(mlb) 
 
#partition the Weight variable into groups according to values
#of the Height variable, and then compute the mean weight in each group.
muhats <- tapply (mlb$Weight, mlb$Height, mean)
muhats
plot(muhats)
#Finding confidence intervals
#get n
tapply (mlb$Weight, mlb$Height, length)
#get standard deviation
tapply (mlb$Weight, mlb$Height, sd)
#get CI for non-parametric approach
#that is the CI for the mean of the values for height 72
#An approximate 95% CI for (72), for example, is then
#190.3596 +/- 1.96 * 17.56349/sqrt(150)
#or about (187.6,193.2).

#parametric approach using linear model
#create linear model
#mean weight = c + d * height
lmout <- lm(mlb$Weight ~ mlb$Height)
lmout

#Call:
#lm(formula = mlb$Weight ~ mlb$Height)
#
#Coefficients:
#  (Intercept)   mlb$Height  
#   -151.133        4.783 
#
#therefore, muhat = -151.133 + 4.783*height
#use R func coef to fetch coefficients
coef(lmout) %*% c(1, 72)
#         [,1]
#[1,] 193.2666

#confidence interval
tmp <- c(1,72)
sqrt (tmp %*% vcov (lmout) %*% tmp) 
#          [,1]
#[1,] 0.6859655
#therefore
193.2666 + 1.96*0.6859655
193.2666 - 1.96*0.6859655
#So, an approximate 95% CI for µ(72) under this model would be about
#(191.9,194.6)

##################
#Multipredictor Linear Models
#Let's consider a parametric model for the baseball data,
#mean weight = c + d * height + e * age
#
#estimation of coefficients
#predict weight from height and age
lm(mlb$Weight ~ mlb$Height + mlb$Age )

#Call:
#  lm(formula = mlb$Weight ~ mlb$Height + mlb$Age)

#Coefficients:
#  (Intercept)   mlb$Height      mlb$Age  
#   -187.6382       4.9236       0.9115 

#muhat(t1, t2) =  187.6382 + 4.9236 t1 + 0.9115 t2 (1.14)
#where t1 and t2 are height and age, respectively.

#We estimate that, on average (a key qualier), 
#each extra inch in  height corresponds to almost 5 pounds of additional weight.
#We estimate that, on average, each extra year of age corresponds to
#almost a pound in extra weight.

#######
#Nonparametric Regression Estimation: k-NN
z <- mlb[mlb$Height == 72 & mlb$Age == 25, ]
z
###no data points
#The k-Nearest Neighbor (k-NN) method for estimating regression functions
#is simple: Find the k data points in our sample that are closest to the
#desired prediction point, and average their Y values.

#(see book)

##########
#After Fitting a Model, How Do We Use
#It for Prediction?
#
########################
#Rough Rule of Thumb (Tukey): For a data set consisting
#of n observations, use fewer than sqrt(n) predictors.
########################
##
##
#Cross-Validation
#

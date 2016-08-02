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

#̂μ(t1, t2) =  187.6382 + 4.9236 t1 + 0.9115 t2 (1.14)
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
#Toward that end, it is common to articially create a set of \new" data and
#try things out. Instead of using all of our collected data as our training set,
#we set aside part of it to serve as simulated \new" data. This is called the
#validation set or test set. The remainder will be our actual training data.
#In other words, we randomly partition our original data, taking one part as
#our training set and the other part to play the role of new data. We t our
#model, or models, to the training set, then do prediction on the test set,
#pretending its response variable values are unknown. We then compare to
#the real values. This will give us an idea of how well our models will predict
#in the future. This is called cross-validation.
xvalpart <- function (data ,p) {
  n <- nrow(data) 
  ntrain <- round(p*n) 
  trainidxs <- sample(1:n, ntrain, replace=FALSE) 
  valididxs <- setdiff(1:n,trainidxs) 
  list (train=data[trainidxs, ], valid=data [valididxs,]) 
}

#cross-validation for linear models
# arguments : 
# 
# data: full data 
# ycol: column number of resp. var. 
# predvars: column numbers of predictors 
# p: prop. for training set 
# meanabs: see ’value’ below
# value: if meanabs is TRUE, the mean absolute 
# prediction error; otherwise, an R list 
# containing pred., real Y
xvallm <- function(data, ycol, predvars, p, meanabs=TRUE){
  tmp <- xvalpart(data, p) 
  train <- tmp$train 
  valid <- tmp$valid 
  # fit model to training data
  trainy <- train[,ycol ] 
  trainpreds <- train[,predvars ] 
  # we’ll be using matrices, e.g. in lm()
  trainpreds <- as.matrix(trainpreds) 
  lmout <- lm(trainy ~ trainpreds) 
  # apply fitted model to validation data
  validpreds <- as.matrix(valid[,predvars ]) 
  predy <- cbind(1,validpreds)%*% coef(lmout) 
  realy <- valid [,ycol]
  if (meanabs) return(mean(abs(predy - realy))) 
  list (predy = predy, realy = realy) 
}

#Let's try cross-validtion on the weight/height/age data, using mean absolute
#prediction error as our criterion for prediction accuracy:
xvallm(mlb, 5, c(4,6), 2/3)


##################
#Bike sharing data
#################
#https://archive.ics.uci.edu/ml/datasets/Bike+Sharing+Dataset
#
shar <- read.csv(file="Data/day.csv", header=T)
#focus on first year
shar <- shar [1:365,]
#shorten registered to reg
names(shar)[15] <- "reg"

#Linear Modeling of μ(t)
#Let’s first check whether the ridership/temperature relation seems 
#nonlinear, as we have speculated:
plot(shar$temp, shar$reg)

#add squared temperature as a new variable
shar$temp2 <- shar$temp^2
lm(shar$reg ~ shar$temp + shar$temp2)
#Call:
#  lm(formula = shar$reg ~ shar$temp + shar$temp2)

#Coefficients:
#  (Intercept)    shar$temp   shar$temp2  
#     -378.9       9841.8      -6169.8 

#The presence of nominal data
#Such situations are generally handled by setting up what are called indicator
#variables or dummy variables. The former term alludes to the fact that our
#variable will indicate whether a certain condition holds or not, with 1 coding
#the yes case and 0 indicating no.

#We could, for instance, set up such a variable for Tuesday data:
shar$tues <- as.integer (shar$weekday == 2)
#Indeed, we could define six variables like this, one for each of the days
#Monday through Saturday.
#Let’s opt for a simpler analysis, in which we distinguish 
#only between weekend days and week days, i.e. define a dummy 
#variable that is 1 for Monday through Friday, and 0 for the other days
#Actually, those who assembled the data set already defined such a 
#variable, which they named workingday.

#We incorporate this into our linear model:
#mean reg = c + d × temp + e × temp2 + f workingday 

#add one more variable
shar$clearday <- as.integer (shar$weathersit == 1)

#our regression model will be
#mean reg = B0 + B1 temp + B2 temp^2 + B3 workingday + B4 clearday
lmout <- lm(reg ~ temp+temp2+workingday+clearday,data = shar)
lmout

#Call:
#lm(formula = reg ~ temp + temp2 + workingday + clearday, data = shar)

#Coefficients:
#  (Intercept)         temp        temp2   workingday     clearday  
#     -1362.6        11059.2      -7636.4    686.0         518.9 

#In other words, estimated regression function is
#̂μ=-1362.6 + 11059.2t1+-7636.4t2+686t3+518.9t4

#So, what should we predict for number of riders on the type of day described
#at the outset of this chapter|Sunday, sunny, 62 degrees Fahrenheit? First,
#note that the temp variable is scaled to [0,1], as
#Celsius temperature - minimum / (maximum = minimum)
coef(lmout) %∗% c(1,0.525,0.525^2,0,1)

#1.11 Interaction Terms
#Let's take another look at (1.23), specifically the term involving the variable
#workingday, a dummy indicating a nonholiday Monday through Friday.
#Our estimate for B3 turned out to be 988.5, meaning that, holding temperature
#and the other variables fixed, there are 988.5 additional riders on
#workingdays.
#But implicit in this model is that the workingday effect is the same on
#low-temprerature days as on warmer days. For a broader model that does
#not make this assumption, we could add an interaction term, consisting of
#a product of workingday and temp:
#mean reg = B0 + B1 temp + B2 temp^2 + B3 workingday + B4 clearday + B5 temp * workingday
#How does this model work? Let's illustrate it with a new data set.

#1.11 Example: Salaries of Female Programmers and Engineers
#This data is from the 2000 U.S. Census, consisting of 20,090 programmers
#and engineers in the Silicon Valley area. The data set is included in the
#freqparcoord package on CRAN. Suppose we are working toward a Description
#goal, specidically the eddects of gender on wage income.

#As with our bike-sharing data, we'll add a quadratic term, in this case
#on the age variable, reflecting the fact that many older programmers and
#engineers encounter trouble finding work after age 35 or so. Let's restrict
#our analysis to workers having at least a Bachelor's degree, and look at
#the variables age, age2, sex (coded 1 for male, 2 for female), wkswrked
#(number of weeks worked), ms, phd and wageinc:
library (freqparcoord)
data(prgeng)
prgeng$age2 <- prgeng$age^2
edu <- prgeng$educ
prgeng$ms <- as.integer(edu == 14)
prgeng$phd <- as.integer(edu == 16)
prgeng$fem <- prgeng$sex - 1
tmp <- prgeng[edu >= 13,]
#select specific columns age, age2, wkswrkd, ms, phd, fem, wageinc
pe <- tmp[,c(1,12,9,13,14,15,8)]
pe <- as.matrix(pe)

#out model is:
#mean wageinc = B0 +B1 age + B2 age^2 + B3 wkswrkd + B4 ms + B5 phd + B6 fem
#we find the following 
summary(lm(pe[,7] ~ pe [,-7]))

#Call:
#lm(formula = pe[, 7] ~ pe[, -7])

#Residuals:
#  Min     1Q Median     3Q    Max 
#-99386 -20889  -4567  12467 286564 

#Coefficients:
#               Estimate Std.      Error   t value  Pr(>|t|)    
#(Intercept)     -87162.556      4716.088 -18.482   <2e-16 ***
#  pe[, -7]age       4189.304    234.335  17.877   <2e-16 ***
#  pe[, -7]age2       -43.479      2.668 -16.293   <2e-16 ***
#  pe[, -7]wkswrkd   1312.234     29.325  44.748   <2e-16 ***
#  pe[, -7]ms        9845.719    843.128  11.678   <2e-16 ***
#  pe[, -7]phd      17403.523   1762.154   9.876   <2e-16 ***
#  pe[, -7]fem     -11176.740    912.206 -12.252   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 45440 on 14059 degrees of freedom
#Multiple R-squared:  0.2035,	Adjusted R-squared:  0.2031 
#F-statistic: 598.6 on 6 and 14059 DF,  p-value: < 2.2e-16

#The results are striking in terms of gender: With age, education and so on
#held constant, women are estimated to have incomes about $11,177 lower
#than comparable men.

#But this analysis implicitly assumes that the female wage deficit is, 
#for instance, uniform across educational levels. 
#Being female makes a β6 difference, no matter what the values of ms and
#phd are. To generalize our model in this regard, let’s define two 
#interaction variables

msfem <- pe[,4] * pe[,6]
phdfem <- pe[,5] * pe[,6]
pe <- cbind(pe, msfem, phdfem)

#Our model is now:
#mean wageinc = B0 +B1 age + B2 age^2 + B3 wkswrkd + B4 ms + B5 phd + B6 fem
#               + B7 msfem + B8 phdfem

#So now instead of there being one number for female effect, there are two
#Female effect for Master's degree holders  B6 + B7
#Female effect for Phd degree holders  B6 + B8
#So, let's rerun the regression analysis:
summary(lm(pe[,7] ~ pe [,-7]))

#Call:
#lm(formula = pe[, 7] ~ pe[, -7])

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-101176  -20938   -4494   12461  284988 

#Coefficients:
#                   Estimate    Std Error t value   Pr(>|t|)    
#(Intercept)        -87499.793   4715.343 -18.556  < 2e-16 ***
#  pe[, -7]age       4183.402    234.244  17.859  < 2e-16 ***
#  pe[, -7]age2       -43.439      2.667 -16.285  < 2e-16 ***
#  pe[, -7]wkswrkd   1312.160     29.313  44.763  < 2e-16 ***
#  pe[, -7]ms       11060.653    965.016  11.462  < 2e-16 ***
#  pe[, -7]phd      19726.664   1907.382  10.342  < 2e-16 ***
#  pe[, -7]fem      -9091.230   1121.816  -8.104 5.75e-16 ***
#  pe[, -7]msfem    -5088.779   1975.841  -2.575  0.01002 *  
#  pe[, -7]phdfem  -14831.582   4957.859  -2.992  0.00278 ** 
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 45420 on 14057 degrees of freedom
#Multiple R-squared:  0.2043,	Adjusted R-squared:  0.2038 
#F-statistic: 451.1 on 8 and 14057 DF,  p-value: < 2.2e-16

#The estimated values of the two female effects are -9091.230 -5088.779 =
#-14180.01, and 9091.230 -14831.582 = -23922.81. A few points jump out
#here:
#   Once one factors in educational level, the gender gap is seen to be
#even worse than before.
#   The gap is worse at the PhD level than the Master's, likely because
#of the generally higher wages for the latter.


###############
#1.12 Classiffcation Techniques
#
#Recall that the regression function is the conditional mean:
# mu(t) = E(Y | X=t)
#In the clasification case, Y is an indicator variable, we know its mean
#is the probability that Y = 1. In other words,
# mu(t) = P(Y=1 | x=t)

#An intuitive strategy  would be to guess that Y = 1 if the conditional
#probability of 1 is greater than 0.5, and guess 0 otherwise.

#1.12.2 Example: Bike-Sharing Data
#Let’s take as our example the situation in which ridership is above 3,500
#bikes, which we will call HighUsage:
shar$highuse <- as.integer (shar$reg > 3500)

#the most common model for conditional probability is logistic regression:
#probability of High usae = l(= B0 + B1 temp + B2 temp^2 + B3 workingday + B4 clearday)

#R provides the glm() (“generalized linear model”) function for several nonlinear
#model families, including the logistic, which is designated 
#via family = binomial:
glmout <- glm(highuse ~ temp+temp2+workingday+clearday, data=shar, family=binomial)
glmout

#Call:  glm(formula = highuse ~ temp + temp2 + workingday + clearday, 
#family = binomial, data = shar)

#Coefficients:
#  (Intercept)         temp        temp2   workingday     clearday  
#  -18.263           45.909      -36.231        3.570        1.961  

#Degrees of Freedom: 364 Total (i.e. Null);  360 Residual
#Null Deviance:	    446.8 
#Residual Deviance: 245.8 	AIC: 255.8

tmp <- coef(glmout)%*%c(1,0.525,0.525^2,0,1)
1/(1+exp(-tmp ))

#So, our parametric model gives an almost identical result here to the one
#arising from k-NN, about a 10% probability of HighUsage.

#1.14 Code Complements
gender <- c("m", "f", "m", "f", "m", "f")
height <- c(66,62,66,58,67,53)
x <- data.frame(gender, height)
x
#Now let’s split by gender:
xs <- split(x,x$gender)
xs
#find the mean heights in each gender
mean(xs$f$height)
mean(xs$m$height)

#But with tapply(), we can combine the two operations:
tapply(x$height, x$gender, mean)
#let's say we wanted to find mean and standard deviation
tapply(x$height, x$gender, function(w) c(mean(w), sd(w)))

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

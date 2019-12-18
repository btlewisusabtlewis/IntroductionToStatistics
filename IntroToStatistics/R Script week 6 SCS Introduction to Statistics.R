# SCS Introduction to Statistics Script file - week 6
# Linear and logistic regression

# We always start with the same steps:
# 1: Set a working directory
#setwd("Documents/R")

# Load (and perhaps install) relevant packages:
# If you have installed these before, you do not need to install them again. If you haven't installed them before, make sure you install them now
# install.packages("reshape")
# install.packages("car"); install.packages("QuantPsyc"); 
# install.packages("ggplot2"); install.packages("pastecs");
# install.packages("psych")
# New packages:
install.packages("mlogit")
# You have to load the packages each time you start a new R (Studio) session
library(reshape)
library(boot); library(car); library(QuantPsyc); 
library(ggplot2); library(pastecs); library(psych)
library(mlogit) 

# We continue to work with the dataframe that we created last week: AgeAnxietyGender_nooutliers.csv
#If you saved this last week, you can open it directly:
AgeAnxietyGender_nooutliers<-read.csv("AgeAnxietyGender_nooutliers.csv", header = TRUE)

# Last week we examined the correlation between two variables.
# This week we will look at regression!

# We run a (linear) regression analysis using the lm() function – lm stands for ‘linear model’. 
# This function takes the general form:
# newModel<-lm(outcome ~ predictor(s), data = dataFrame, na.action = an action))

age.anxiety<-lm(Anxiety ~ Age, data = AgeAnxietyGender_nooutliers)

# We have created an object called age.anxiety that contains the results of our analysis. 
# We can show the object by executing:
summary(age.anxiety)

# We can get the Pearson correlation by taking the square root of R2:
sqrt(0.0005892)

# > sqrt(0.0005892)
# [1] 0.02427344
# This is similar to what we have seen before!
# calculate Pearson correlation coefficient.

cor.test(AgeAnxietyGender_nooutliers$Age, AgeAnxietyGender_nooutliers$Anxiety, method = "pearson")

# Now suppose your outcome is dichotomous: your variable can have only 2 values. 
# We use a (hypothetical) example of intergenerational transmission of crime.

# Open the dataframe:
IG<-read.csv("IG_lecture_6.csv", header = TRUE)

# We can look at the data using the head() function, which shows the first six rows of the dataframe:
head(IG)

# To do logistic regression, we use the glm() function. 
# glm() is very similar to the lm() function we just learned.
# glm() stands for 'generalized linear model'
# The general form is 
# newModel<-glm(outcome ~ predictor(s), data = dataFrame, family = name of a distribution, na.action = an action))
# The difference with the lm() model is ", family = name of a distribution"
# this refers to the type of distribution you are working with. Some distributions are Gaussian ("normal" distribution!), binomial, poisson, gamma
# 'family' allows us to tell R the detail of the kind of regression that we want to do.
# If we were doing an ordinary (linear) regression, we would specify this option to Gaussian (which is another name for the normal distribution)
# Logistic regression is based on a binomial distribution, so we need to set this option to 'family = binomial()'

# In our example, we have two variables: 'Parentconvicted' and 'Kidconvicted'
# 'Parentconvicted' is our predictor, 'Kidconvicted' is our outcome variable.
IGmodel <- glm(Kidconvicted ~ Parentconvicted, data = IG, family = binomial())
summary(IGmodel)
# To get the coefficients of the model we use:
IGmodel$coefficients
# To get the odds ratio - the exponent of our coefficient, we use:
exp(IGmodel$coefficients)
# To get confidence intervals for our parameters (coefficients) we use the confint() function
# We can also exponentiate these with the exp() function. 
# To get the confidence intervals, execute:
exp(confint(IGmodel))

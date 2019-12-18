# SCS Introduction to Statistics Script file - week 5

# We always start with the same steps:
# 1: Set a working directory
#setwd("Documents/R")

getwd()
  
# Load (and perhaps install) relevant packages:
# If you have installed these before, you do not need to install them again. If you haven't installed them before, make sure you install them now
# install.packages("reshape")
# install.packages("car"); install.packages("QuantPsyc"); 
# install.packages("ggplot2"); install.packages("pastecs");
# install.packages("psych")
# You have to load the packages each time you start a new R (Studio) session
library(reshape)
library(boot); library(car); library(QuantPsyc); 
library(ggplot2); library(pastecs); library(psych)

# We continue to work with the AgeAnxiety dataframe. 
# If you saved this last week, you can open it directly (adjust the path to your own situation):
AgeAnxiety<-read.csv("AgeAnxiety.csv", header = TRUE)

# If you didn't save it, you'll have to go through the steps that we went through last week to get there:
QuizResponses<-read.csv("Quiz_Responses.csv", header = TRUE)
# Rename the variables I am using so the names are shorter
QuizResponses <- rename(QuizResponses, c(How.old.are.you.="Age", How.much.do.you.agree.with..Statistics.makes.me.cry="CryStats", I.dream.that.Pearson.is.attacking.me.with.correlation.coefficients="PearsonAngst", I.slip.into.a.coma.whenever.I.see.an.equation="EquationComa")) 
# now create a summary variable for the Anxiety
QuizResponses$Anxiety <- (QuizResponses$CryStats + QuizResponses$PearsonAngst + QuizResponses$EquationComa)/3
# We can create a new dataframe that contains only these variables:

AgeAnxiety <- QuizResponses[, c("Age","CryStats","PearsonAngst", "EquationComa", "Anxiety")]

# Let's save this dataframe
write.csv(AgeAnxiety, "AgeAnxiety.csv")

# Last week we created a histogram for the frequency distribution of Anxiety. 
# This week we want to make a boxplot - another useful way to show the distribution of your data.
# Boxplots are made up of a box and two whiskers.
# The box shows:
#  The median
# The upper and lower quartile
# The limits within which the middle 50% of scores lie.
# The whiskers show
# The range of scores
# The limits within which the top and bottom 25% of scores lie

# We want to create a box plot for the variable Age.
# You can create a very simple boxplot using the boxplot command:
boxplot(AgeAnxiety$Age, xlab="Age", col="darkblue")


# Let's also look at the Age distribution using a histogram:

# First: Create the plot object:
HistogramAge <- ggplot(AgeAnxiety, aes(Age)) 

# ggplot(AgeAnxiety, aes(Age)): tells R to plot the Age variable for the AgeAnxiety dataframe

# Then: Add the graphical layer:
HistogramAge + geom_histogram(binwidth=5) + labs(x = "Age", y = "Frequency") 

# We have three outliers that we want to delete.
# We want to include all variables, but only the cases where the ages is at least 18
AgeAnxiety_nooutliers <- subset(AgeAnxiety, Age > 17)

# We can use the function 'subset'. It returns the subset as a data frame.
# This function takes the general form
# newDataframe <- subset(oldDataframe, cases to retain, select = c(list of variables))

# You might first want to check which variables are currently included in your dataframe
names(AgeAnxiety)

# We want to include all variables, but only the cases where the ages is at least 18
AgeAnxiety_nooutliers <- subset(AgeAnxiety, Age > 17)

# You can also use this command to select only certain variables.
# Let's use the full dataframe Quizresponses and only include 
# "Age"          "CryStats"     "PearsonAngst" "EquationComa" "Anxiety" and what.is.your.gender.

AgeAnxietyGender_nooutliers <- subset(QuizResponses, Age > 18, select = c("Age","CryStats","PearsonAngst","EquationComa","Anxiety","What.is.your.gender."))
 
# Save this as a dataframe so you can easily get back to it in future sessions
write.csv(AgeAnxietyGender_nooutliers, "AgeAnxietyGender_nooutliers.csv")

# Create a boxplot without the outliers

boxplot(AgeAnxiety_nooutliers$Age, xlab="Age", col="red")

# And a histogram

HistogramAge <- ggplot(AgeAnxiety_nooutliers, aes(Age)) 
HistogramAge + geom_histogram(binwidth=3) + labs(x = "Age", y = "Frequency") 

# You can also rerun the descriptives command without the outliers (see last week)
describe(cbind(AgeAnxiety_nooutliers$Age, AgeAnxiety_nooutliers$Anxiety))

stat.desc(cbind(AgeAnxiety_nooutliers$Age, AgeAnxiety_nooutliers$Anxiety), basic = FALSE, norm = TRUE) 

# Correlation
# I am curious whether anxiety regarding statistics is related to people’s age. 
# In my case, I have two continuous variables and I will calculate Pearson correlation coefficient.

cor.test(AgeAnxiety_nooutliers$Age, AgeAnxiety_nooutliers$Anxiety, method = "pearson")





# Extra options - not discussed in class, but useful!
# As I've said before, there are different ways to do similar things in R:
# boxplots
# We have more options when we use ggplot2. - I will not discuss this in class.
# For example you can show a boxplot for different groups, e.g. gender

# First we create an object again.
# We call this object (our boxplot) AgeBoxplot (but you could give it any name you like)

AgeBoxplot <- ggplot(QuizResponses, aes(What.is.your.gender.,Age))

# Then we tell R that we want to see a boxplot:
AgeBoxplot +geom_boxplot()

# We have three outliers that we want to delete.
# We only want to include ages above 18.

# Let's try doing this by creating a vector.
Age_valid <- AgeAnxiety$Age > 18

# Then use this vector to create a subset
AgeAnxiety[Age_valid,]

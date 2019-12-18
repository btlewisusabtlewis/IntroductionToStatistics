# SCS Introduction to Statistics Script file - week 4

# First step: set a working directory, so R knows where to look up and save files

# Shows the working directory (wd) 
getwd()

# Set the working directory
#  adjust the location to the location where you have stored the relevant R files:
#setwd("C:/myfolder/data")

# open the dataframe with the quiz responses (this file should be in the same working directory folder)
QuizResponses<-read.csv("Quiz_Responses.csv", header = TRUE)

# To show your data
#QuizResponses
# To show variable names
names(QuizResponses)

# Install and load relevant packages before class
#install.packages("reshape")
#install.packages("car"); install.packages("QuantPsyc"); 
#install.packages("ggplot2"); install.packages("pastecs");
#install.packages("psych")
library(reshape)
library(boot); library(car); library(QuantPsyc); 
library(ggplot2); library(pastecs); library(psych)

# “Install” will download and install (you only need to do this once)
# “library” will load the package (you have to do this every time you start a new R session)

# Rename the variables I am using so the names are shorter
QuizResponses <- rename(QuizResponses, c(How.old.are.you.="Age", How.much.do.you.agree.with..Statistics.makes.me.cry="CryStats", I.dream.that.Pearson.is.attacking.me.with.correlation.coefficients="PearsonAngst", I.slip.into.a.coma.whenever.I.see.an.equation="EquationComa")) 

# show variable names to see whether it works. It does!
names(QuizResponses)

# now create a summary variable for the Anxiety
# Anxiety – I will be using several questions of the quiz to create a mean. I decided to use the following questions:
#-	“I dream that Pearson is attacking me with correlation coefficients”
#-	“I slip into a coma whenever I see an equation”
#-	“Statistics makes me cry”
# A lower score on these variables indicates more anxiety. I want to take the mean score of these three questions to create my anxiety variable.
# I can simply do this by telling R the function it should use
# E.g. mydata$mean <- (mydata$x1 + mydata$x2)/2

QuizResponses$Anxiety <- (QuizResponses$CryStats + QuizResponses$PearsonAngst + QuizResponses$EquationComa)/3

# Options that I will not discuss in class, but that migh be useful for you when using R or R studio
# RUN THIS IF YOU WANT TO FOLLOW ALONG DURING THE CLASS
# Optionally, you can save this as a new file
write.csv(QuizResponses, "Quiz_responses_newnames.csv")

# Selecting parts of a dataframe
# Sometimes you only want to select a small portion of your data. 
# In our case, we might just want the variables we are working with 
# (the original three anxiety ones included). 
# We can create a new dataframe that contains only these variables:

AgeAnxiety <- QuizResponses[, c("Age","CryStats","PearsonAngst", "EquationComa", "Anxiety")]

# If you look at the dataframe (with the command below and when you click on 
# AgeAnxiety in the Environmen tab), you'll see that it only contains the 
# variables we specified.
AgeAnxiety
names(AgeAnxiety)

# Let's save this dataframe
write.csv(AgeAnxiety, "AgeAnxiety.csv")
# or as: write.table(AgeAnxiety, "AgeAnxiety.txt", sep="\t", row.names = FALSE)

# If we then want to open this at a later point again:
#AgeAnxiety<-read.csv("~/Dropbox/Statistics/R/AgeAnxiety.csv", header = TRUE)
names(AgeAnxiety)

# CONTINUE HERE:
# CREATE A HISTOGRAM - FREQUENCY DISTRIBUTION!
# Now we want to create a histogram of both our predictor and outcome variable to check the distribution
# Chapter 4 of Field, Miles & Field describes in great detail how to create graphs
# In this class, I will only show some essential basic graphs, because we only
# have limited time, but please consult Chapter 4 to learn more about graphs.

# We use the package ggplot2 so first have to install and then load it:

#install.packages("ggplot2")
library(ggplot2)

# In ggplot2 a plot is made up of layers
# The anatomy of a graph consists of different elements

# First: Create the plot object:
Histogram <- ggplot(AgeAnxiety, aes(Anxiety)) 

# ggplot(AgeAnxiety, aes(Anxiety)): tells R to plot the Anxiety varible for the AgeAnxiety dataframe

# Then: Add the graphical layer:
Histogram + geom_histogram(binwidth = 0.4 ) + labs(x = "Anxiety", y = "Frequency")

# We can add an extra layer with the standard normal curve to this plot:
# First make sure that the histogram including our aesthetic functions has been saved as an object:
Anx_Hist <-Histogram + geom_histogram(binwidth = 0.4, aes(y = ..density..), colour = "black", fill = "black") + labs(x = "Anxiety", y = "density")
#Anx_Hist <- Histogram + geom_histogram(binwidth = 0.4 ) + labs(x = "Anxiety", y = "Frequency")

# Then add the normal curve to this histogram
Anx_Hist + stat_function(fun = dnorm, args = list(mean = mean(AgeAnxiety$Anxiety, na.rm = TRUE), sd = sd(AgeAnxiety$Anxiety, na.rm = TRUE)), colour = "black", size = 1)

# The stat_function() command draws the normal curve using the function dnorm(). 
# This function basically returns the probability (i.e. the density) for a given value from a normal 
# distribution of known mean and standard deviation.
# mean(AgeAnxiety$Anxiety, na.rm = TRUE): specifies the mean as being the mean of the Anxiety variable after removing any missing values
# sd(AgeAnxiety$Anxiety, na.rm = TRUE): specifies the standard deviation as being that of the Anxiety variable
# , colour = "black", size = 1): sets the line color to black and the line width as 1.
# You can also build up this plot in just one command - I just showed you in steps so it's hopefully easier to understand
# (and you immediately learn about the idea of objects)

#Anx_Hist + stat_function(fun = dnorm, args = list(mean = mean(AgeAnxiety$Anxiety), sd = sd(AgeAnxiety$Anxiety)), colour = "black", size = 1)

# We can also use numbers to spot normality
# We use values of skew/kurtosis
# 0 in a normal distribution
# Convert to z (by dividing value by SE)

# Important: Do not use this with large samples! 
# Because the tests are likely to be significant even when skew and kurtosis values are not too 
# different from normal.  
# See Field, Miles, & Field, p. 176: Cramming Sam's Tips: Skew and kurtosis

# desribe(dataframe$variable) will give you descriptives for your variable
describe(AgeAnxiety$Age)

# You can use the cbind() function to combine two or more variables:
# For more info on the cbind() function (pasting columns of data together),
# see R's Souls' Tip 3.5 'The list() and cbind() functions, p. 83 Field, Miles, & Field.

describe(cbind(AgeAnxiety$Age, AgeAnxiety$Anxiety))

# We can also use stat.desc from the pastecs package to get the descriptives
# (see also 5.5.2 Quantifying Normality with numbers, p. 173)


stat.desc(cbind(AgeAnxiety$Age, AgeAnxiety$Anxiety), basic = FALSE, norm = TRUE) 



# EXTRAS

# We can adjust all kinds of aesthetics in the plot, see also Chapter 4.

Histogram + geom_histogram(aes(y=..density..), colour = "black", fill = "white") + labs(x = "Anxiety", y = "density")

# geom_histogram(aes(y=..density..), colour = "black", fill = "white"): this command plots the histogram, 
# sets the line colour to be black and the fill colour to be white. 

Histogram + geom_histogram(binwidth = 0.4 ) + labs(x = "Anxiety", y = "Frequency")

# + geom_histogram(binwidth = 0.4 ): sets the binwidth of each bin to 0.4 which looks better than the original
# You can play with this to see the differences
Histogram + geom_histogram(binwidth = 0.25 ) + labs(x = "Anxiety", y = "Frequency")
Histogram + geom_histogram(binwidth = 0.5 ) + labs(x = "Anxiety", y = "Frequency")
Histogram + geom_histogram(binwidth = 0.33 ) + labs(x = "Anxiety", y = "Frequency")
# We can also combine these commands:
Histogram + geom_histogram(binwidth = 0.4, aes(y = ..density..), colour = "black", fill = "white") + labs(x = "Anxiety", y = "density")

# And we can play with the colors:
Histogram + geom_histogram(binwidth = 0.4, aes(y = ..density..), colour = "blue", fill = "red") + labs(x = "Anxiety", y = "density")
Histogram + geom_histogram(binwidth = 0.4, aes(y = ..density..), colour = "blue", fill = "blue") + labs(x = "Anxiety", y = "density")

# Or go for plain black
Histogram + geom_histogram(binwidth = 0.4, aes(y = ..density..), colour = "black", fill = "black") + labs(x = "Anxiety", y = "density")



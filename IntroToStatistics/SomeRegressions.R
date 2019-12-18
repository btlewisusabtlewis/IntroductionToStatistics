library(utils)
library(caret)
library(hydroGOF)
# coding example 1
set.seed(60)
realEstate=read.csv("realEstate.csv",header=TRUE,
                    stringsAsFactors = FALSE)
training=realEstate[seq(1,nrow(realEstate),2),c("beds","baths","sq__ft","price")]
testing=realEstate[seq(2,nrow(realEstate),2),c("beds","baths","sq__ft","price")]
# train a regression model
model=train(price ~.,training,method="rf")
# test the model on testing
predictedPrice=predict(model,newdata = testing)
# measure RMSE
# sqrt(sum((testing$price - predictedPrice)^2 ,na.rm = TRUE ) /
#        nrow(testing) )
rmse(testing$price,predictedPrice)


############### classification example
training=iris[seq(1,nrow(iris),2),c("Sepal.Length","Petal.Length",
                                    "Species")]
testing=iris[seq(2,nrow(iris),2),c("Sepal.Length","Petal.Length",
                                    "Species")]
model=train(Species ~.,training,method="kknn")
predictedSpecies=predict(model,testing)

sum(predictedSpecies==testing$Species)/nrow(testing)


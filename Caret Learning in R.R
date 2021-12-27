install.packages("caret")
library(caret)
#install.packages("ellipse")

#Load dataset
iris

#Know your dataset
str(iris)

#Visualization of the data

## The following plots are used to check the relationship between categorical
## variables and numerical
featurePlot(x=iris[,1:4], y=iris$Species,plot = "pairs")
featurePlot(x=iris[,1:4], y=iris$Species, plot = "ellipse", auto.key= list(columns=3))
featurePlot(x=iris[,1:4], y=iris$Species, plot = c("boxplot"), auto.key= list(columns=3), 
            scale=list(y=list(relation="free")), labels=c('Species', 'length in cm'))

#Note : scatterplot is used to check the relationship between numerical variables
featurePlot(x=iris[,1:3], y=iris[,4], plot = "scatter",type=c('p','smooth'),
            auto.key= list(columns=3),pch=16, lwd=2)

# Correlation
## call the correlation library

library(corrplot)

## To cal correlation coefficient
cor_iris = cor(iris[,1:4])
cor_iris

## To show visualize the correlation 
corrplot(cor_iris, method = 'ellipse', type = 'upper')

# To decide the model, we use cross validation
trc = trainControl(method = "repeatedcv", number = 10, repeats = 3)
trc

#Fit model
## To fit model we use lm-> linear regression, since predictors are more than one
mod_lm = train(Sepal.Length~.,iris, method="lm", trControl=trc)
mod_lm
summary(mod_lm)

## To explore the model more
mod_lm$resample
mod_lm$results

#Diagnostic of model
plot(mod_lm$finalModel)

##Test model by comparing known and predicted
obs=predict(mod_lm, iris)
plot(iris$Sepal.Length,obs)
abline(0,1)

##Plot the model and actual value together
plot(1:nrow(iris), iris$Sepal.Length)
points(1:nrow(iris),obs, col="red")

#To predict from the observations not from training data
df_new = data.frame(Sepal.Width=c(4.8,4.6), Petal.Length=c(3.4,6.9),
                    Petal.Width=c(1.7,2.4), Species=c('setosa',"virginica"))
predict(mod_lm,df_new)

#Fit Classification Model using random forest
mod_rf = train(Species~.,iris, method="rf", trControl=trc)
mod_rf
##to predict the species of flower from the model
obs= predict(mod_rf, iris)
confusionMatrix(iris$Species, obs)

#Fit Classification Model using gbm
mod_gbm = train(Species~.,iris, method="gbm", trControl=trc)
mod_gbm
##to predict the species of flower from the model
obs= predict(mod_gbm, iris)
confusionMatrix(iris$Species, obs)

#Fit Classification Model using lvq
mod_lvq = train(Species~.,iris, method="lvq", trControl=trc)
mod_lvq
##to predict the species of flower from the model
obs= predict(mod_lvq, iris)
confusionMatrix(iris$Species, obs)

#Fit Classification Model using svmRadial
mod_svm = train(Species~.,iris, method="svmRadial", trControl=trc)
mod_svm
##to predict the species of flower from the model
obs= predict(mod_svm, iris)
confusionMatrix(iris$Species, obs)

##For Visualization
plot(mod_svm)

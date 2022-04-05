#### Kaggle Challenge####

##### Prediction of Patient Heart Disease#####
setwd("~/R/train.csv")
data <- read.csv("train.csv")
View(data)
str(data)
data <- data[-1]
View(data)
ncol(data)
nrow(data)

#### Exploratory Analysis #####
summary(data)
pairs(data[,2:5])

library(corrplot)
cor <- cor(data[,-c(1,17:28)],use="everything", method=c("pearson"))
head(cor)
corrplot(cor, method = "number", type = "full")

##### Principal Component Analysis #####
pca<-prcomp(data[,-c(1,17:28)])
attributes(pca)
summary(pca)
pca
View(pca$x)
pca_data <-data.frame(pca$x)
head(pca_data$PC1)

##### Splitting the data into Train and Test Data #####
split <- sample(2, nrow(data), replace=T, prob =c(0.8,0.2))

train <- data[split==1,]
test <- data[split==2,]

##### Building Regression Model ######

model<- lm(score~., 
           data= train)
summary(model)
attributes(model)
plot(model$fitted.values)
    #### Interpretation of the result###
#The model shows V10..V18 and V21, have significant effect on the score of patient.p<0.05

##### Prediction using train data######
predict <- predict(model, train)
head(predict)
head(train$score)

plot(1:nrow(train), train$score)
points(1:nrow(train), predict, col="red")



library(caret)
train_control <-trainControl(method = "boot")
model1 <- train(score~., 
               data= train, 
               method = "lm",
               trainContrl = train_control)
summary(model1)
attributes(model1)
plot(model1$finalModel)

predict1 <- predict(model1, train)
head(predict1) 
head(train$score)


new_data <- read.csv("test.csv")
View(new_data)
new_score <- predict(model, new_data)
head(new_score)

id <- new_data$Id
result <- data.frame(id, new_score)
head(result)
colnames(result) <- c("id","new_score")
head(result)
write.csv(result, "result.csv")


###### Predicting the IQ of a student#####

std <- read.csv("student.csv")
head(std)
str(std)

#######Storing relevant data into new variable######
std <- std[,3:9]
head(std)
std$GEN_Fac <- as.factor(std$GENDER)
str(std)

######Correlation########
cor_sbj <- cor(std[,2:6])
summary(cor_sbj)

library(corrplot)
corrplot(cor_sbj, method = "number", type = "upper")

######Graphical Exploration######
pairs(std[,2:6])
featurePlot(x=std[,2:6], y=std[,7], plot = c("scatter"),
            type=c('p','smooth'),
            auto.key= list(columns=1),pch=16, lwd=2)

######Prediction#####
partition <- sample(2, nrow(std), replace = T, prob = c(0.8, 0.2))

train <- std[partition==1,]
validate <- std[partition==2,]

#######Building the Regression model######
reg <- lm(INTELIGENCE~MATH+ENGLISH+BIOLOGY+ECONOMICS+AGRIC+GENDER, 
          data=train)
summary(reg)

#######Predicting from train data########
pred<- predict(reg,train)
head(pred)
head(train$INTELIGENCE)

########Predicting from validate data#######
pred_test <- predict(reg,validate)
table(pred_test,validate$INTELIGENCE)
head(pred_test)
head(validate$INTELIGENCE)

#########Predicting from new data set#######
new_data <- data.frame(GENDER=c("MALE", "MALE", "FEMALE"),
                       MATH=c(99, 34, 70),
                       ENGLISH=c(72,100,80),
                       BIOLOGY=c(100,80,50),
                       ECONOMICS=c(90,88,100),
                       AGRIC=c(97,50,98))
pred_new_data <- predict(reg, new_data)
data.frame(pred_new_data)

#########Plotting actual data with prediction########
plot(1:nrow(train), train$INTELIGENCE)
points(1:nrow(train),pred, col="red")

-----------------------------------------------------------------------

##########Using Caret library for Prediction#######

library(caret)

########Building cross-validation########
train_ctrl <- trainControl(method = "boot")

##########Building the Regression model#########
model <- train(INTELIGENCE~MATH+ENGLISH+BIOLOGY
               +ECONOMICS+AGRIC+GENDER,data=std,
               method="lm", trControl=train_ctrl)
model
summary(model)
attributes(model)
plot(model$finalModel)


#########Predicting the data########
pre <- predict(model, std)
head(pre)
head(std$INTELIGENCE)
plot(std$INTELIGENCE,pre)
abline(0,1)

########Building the Random forest#######
model_1 <- train(INTELIGENCE~MATH+ENGLISH+BIOLOGY
               +ECONOMICS+AGRIC+GENDER,data=std,
               method="rf", trControl=train_ctrl)
model_1

#######Predict#######
pre_1 <- predict(model_1, std)
head(pre_1)
head(std$INTELIGENCE)




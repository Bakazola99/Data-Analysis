binary <- read.csv("binary.csv")
str(binary)

binary$admit <- as.factor(binary$admit)
binary$rank <- as.factor(binary$rank)

str(binary)

######### Data Summary##########
summary(binary)

###############Partition################

partition <- sample(2, nrow(binary), replace = TRUE, prob = c(0.8, 0.2))

train <- binary[partition==1,]
test = binary[partition==2,]

############## Decision Tree Model###########
colnames(binary)
tree <- ctree(admit~.,data=train)
tree
plot(tree)

######### Prediction ##########
adm <- predict(tree, train)
adm

########### Accuracy of the Model ##########
tab <- table(prediction = adm, actual = train$admit)
tab
summary(tab)

############# Logistic Regression#######
model <- glm(admit~gpa+rank, data=train, family = "binomial")
model
summary(model)

######## Prediction #######
pre <- predict(model, train, type = "response")
head(pre)
head(train)

pred <- predict(model, test, type = "response")
head(pred)
head(test)

######### Confusion Matrix
pre1 = as.factor(ifelse(pre>0.5,1,0))
t <- table(prediction=pre1, actual=train$admit)
t

# Create a confusion matrix
confusion_matrix <- confusionMatrix(train$admit, pre1)
confusion_matrix


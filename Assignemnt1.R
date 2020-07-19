##Load data from local csv file
path = '/Users/yangli/OneDrive/MMAI/MMAI831/AIOS1_adv_sales.csv'
#path = 'C:/Users/y437l/OneDrive/MMAI/MMAI831/AIOS1_adv_sales.csv'
data <- read.csv(file =path)
#drop index column
data <- subset(data, select = -c(X))
##No missing data
#basic descriptive statistics
summary(data[,c(1:6)])
data[,1:6]<- scale(data[,1:6])
#check data suitability
#generalized pair grapjs to check for bivariate correlations
library(gpairs)
gpairs(data[,c(1:6)])

###seems like there is no correlation relationship between independent variables
library(corrplot)
corrplot(cor(data[,c(1:6)]), method = "color", type="full", addgrid.col = "red",
         addshade = "positive", addCoef.col = "black")
##The goal of a sales driver analysis is to discover relationships between
##the sale volume with features of the the price and number of stores for the 
## product and different type of advertisements.

###split the data
train_end <- floor(0.75*nrow(data))
test_star <- train_end+1
train_data <- data[0:train_end,]
test_data <- data[test_star:nrow(data),]
  
##Fitting the model
##Start with simple linear regression
model1<-lm(sales~price,data=train_data)
summary(model1)

#predict test data
model1_test_result <- predict(model1, newdata=test_data)
TSS1 <- sum((test_data$sales-mean(test_data$sales))^2)
RSS1 <- sum((test_data$sales-model1_test_result)^2)
test_R_squared1 = 1 - (RSS1/TSS1)
###########################
#train R squared:0.06081
#test R squared:0.04407
###########################

##Multiple linear regression with two factors
library(lmtest)
model2<-lm(sales~price+store,data=train_data)
##Anova table,F-test
anova(model2)
#R squared
summary(model2)
# check heteroskedasticity
par(mfrow=c(1,1))
plot(model2)
plot(model2$residuals)
###Breusch-pagen test
bptest(model2)
###Durbin-Watson test serial correlation
dwtest(model2)

#predict test data
model2_test_result <- predict(model2, newdata=test_data)
TSS2 <- sum((test_data$sales-mean(test_data$sales))^2)
RSS2 <- sum((test_data$sales-model2_test_result)^2)
test_R_squared2 = 1 - (RSS2/TSS2)
###########################
#train R squared:0.3237
#test R squared:0.3121
###########################

##Multiple linear regression with three factors
model3<-lm(sales~price+store+billboard,data=train_data)
##Anova table,F-test
anova(model3)
#R squared
summary(model3)
# check heteroskedasticity
par(mfrow=c(1,1))
plot(model3)
plot(model3$residuals)
###Breusch-pagen test
bptest(model3)
###Durbin-Watson test serial correlation
dwtest(model3)

#predict test data
model3_test_result <- predict(model3, newdata=test_data)
TSS3 <- sum((test_data$sales-mean(test_data$sales))^2)
RSS3 <- sum((test_data$sales-model3_test_result)^2)
test_R_squared3 = 1 - (RSS3/TSS3)
###########################
#train R squared:0.84
#test R squared:0.854
###########################

##Multiple linear regression with four factors
model4<-lm(sales~price+store+billboard+printout,data=train_data)
##Anova table,F-test
anova(model4)
#R squared
summary(model4)
# check heteroskedasticity
par(mfrow=c(1,1))
plot(model4)
###Breusch-pagen test
bptest(model4)
###Durbin-Watson test serial correlation
plot(model4$residuals)
dwtest(model4)

#predict test data
model4_test_result <- predict(model4, newdata=test_data)
TSS4 <- sum((test_data$sales-mean(test_data$sales))^2)
RSS4 <- sum((test_data$sales-model4_test_result)^2)
test_R_squared4 = 1 - (RSS4/TSS4)
###########################
#train R squared:0.84
#test R squared:0.854
###########################

##Multiple linear regression with five factors
model5<-lm(sales~price+store+billboard+printout+sat,data=train_data)
##Anova table,F-test
anova(model5)
#R squared
summary(model5)
# check heteroskedasticity
par(mfrow=c(1,1))
plot(model5)
plot(model5$residuals)
###Breusch-pagen test heteroskedasticity
bptest(model5)
###Durbin-Watson test serial correlation
dwtest(model5)

#predict test data
model5_test_result <- predict(model5, newdata=test_data)
TSS5 <- sum((test_data$sales-mean(test_data$sales))^2)
RSS5 <- sum((test_data$sales-model5_test_result)^2)
test_R_squared5 = 1 - (RSS5/TSS5)
###########################
#train R squared:0.9135
#test R squared:0.9158
###########################

##Multiple linear regression with six factors
model6<-lm(sales~price+store+billboard+printout+sat+comp,data=train_data)
##Anova table,F-test
anova(model6)
#R squared
summary(model6)
# check heteroskedasticity
par(mfrow=c(1,1))
plot(model6)
plot(model6$residuals)
###Breusch-pagen test heteroskedasticity
bptest(model6)
###Durbin-Watson test serial correlation
dwtest(model6)

#predict test data
model6_test_result <- predict(model6, newdata=test_data)
TSS6 <- sum((test_data$sales-mean(test_data$sales))^2)
RSS6 <- sum((test_data$sales-model6_test_result)^2)
test_R_squared6 = 1 - (RSS6/TSS6)
###########################
#train R squared:0.9201
#test R squared:0.92018
###########################

##Multiple linear regression with five factors
model5_1<-lm(sales~price+store+billboard+sat+comp,data=train_data)
##Anova table,F-test
anova(model5_1)
#R squared
summary(model5_1)
# check heteroskedasticity
par(mfrow=c(1,1))
plot(model5_1)
plot(model5_1$residuals)
###Breusch-pagen test heteroskedasticity
bptest(model5_1)
###Durbin-Watson test serial correlation
dwtest(model5_1)

#predict test data
model5_1_test_result <- predict(model5_1, newdata=test_data)
TSS5_1 <- sum((test_data$sales-mean(test_data$sales))^2)
RSS5_1 <- sum((test_data$sales-model5_1_test_result)^2)
test_R_squared5_1 = 1 - (RSS5_1/TSS5_1)
###########################
#train R squared:0.9201
#test R squared:0.92026
###########################
##Multiple linear regression with nine factors
model9<-lm(sales~price+store+billboard+printout+sat+comp
           +store:billboard+store:printout+billboard:printout,data=train_data)
##Anova table,F-test
anova(model9)
#R squared
summary(model9)
# check heteroskedasticity
par(mfrow=c(1,1))
plot(model9)
plot(model9$residuals)
###Breusch-pagen test heteroskedasticity
bptest(model9)
###Durbin-Watson test serial correlation
dwtest(model9)

#predict test data
model9_test_result <- predict(model9, newdata=test_data)
TSS9 <- sum((test_data$sales-mean(test_data$sales))^2)
RSS9 <- sum((test_data$sales-model9_test_result)^2)
test_R_squared9 = 1 - (RSS9/TSS9)
###########################
#train R squared:0.9259
#test R squared:0.92839
###########################

##Multiple linear regression with six factors
model6_1<-lm(sales~price+store+billboard+sat+comp
           +store:billboard,data=train_data)
##Anova table,F-test
anova(model6_1)
#R squared
summary(model6_1)
# check heteroskedasticity
par(mfrow=c(1,1))
plot(model6_1)
plot(model6_1$residuals)
###Breusch-pagen test heteroskedasticity
bptest(model6_1)
###Durbin-Watson test serial correlation
dwtest(model6_1)

#predict test data
model6_1_test_result <- predict(model6_1, newdata=test_data)
TSS6_1 <- sum((test_data$sales-mean(test_data$sales))^2)
RSS6_1 <- sum((test_data$sales-model6_1_test_result)^2)
test_R_squared6_1 = 1 - (RSS6_1/TSS6_1)
###########################
#train R squared:0.9257
#test R squared:0.9284961
###########################
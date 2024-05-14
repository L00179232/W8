var<-lm( weights = height, data = women)
var
summary(var)
View(women)
# y ~ b0 + b1x
# weight ~ -.85.52+3.45* Height\
View(cars)
lm(dist~speed,data = cars)
summary(lm(dist~speed,data = cars)
)

#Scatter plot
str(cars)
windows(20,12)
scatter.smooth(x = cars$speed,
               y = cars$dist, main = "dist ~ speed")
#box plot
windows(20,12)
par(mfrow=c(1,3))
boxplot(x = cars$dist)
boxplot(x = cars$speed)

# density plot
windows(20,12)
par(mfrow=c(1,3))

plot(density(cars$speed),
     main="density plot for speed",
     ylab = "Frequency")
polygon(density(cars$speed),col="blue")
#density plot for distance
plot(density(cars$dist),
     main="density plot for distance",
     ylab = "Frequency")
polygon(density(cars$dist),col="red")

#corelation of speed and distance 
cor(cars$speed,cars$dist)
cor(cars)

#build the linear regression 
attach(cars)
linearMod <- lm(dist~ speed)
linearMod

#regression equation distance ~-17.579+3.932*speed

#summary of model 
summary(linearMod)
AIC(linearMod)
BIC(linearMod)

#sample choose a random sample 
no_of_records <- sample(1:nrow(cars),0.8 * nrow(cars))
str(no_of_records)

#model training data
training_data <- cars[ no_of_records,]
training_data

# test data
testing_data <- cars[-no_of_records,]
testing_data

#Build the model on training data
lr_model <- lm(dist ~ speed, data = training_data)

lr_model
summary(lr_model)

#distance ~ -18.10 + 3.93 * Speed
#distance 

#predict distance from testing data
dist_predict <- predict(lr_model, testing_data )
dist_predict

# make actual_predicteds dataframe
actuals_predict<-data.frame(cbind(actuals=testing_data$dist,
                                  predicted=dist_predict))

attach(actuals_predict)
correlation_accuracy<-cor(actuals,predicted)
correlation_accuracy

#min and max accuracy 
min_max_accuracy <- mean(apply(actuals_predict, 1, min)/
                         apply(actuals_predict,1,max))
min_max_accuracy

#MAPE -- mean Absolute Percentage error
attach(actuals_predict)
mape <- mean(abs(predicted - actuals )/ actuals)
mape

#K fold cross validation
install.packages("DAAG")
library(DAAG)

windows(20,10)
cvResults <- suppressWarnings(CVlm(data = cars,
                                   form.lm = dist~ speed,
                                   m = 5,
                                   dots = FALSE,
                                   seed = 29,
                                   legend.pos = "topleft",
                                   printit = FALSE,
                                   main = "samll symbols are predicted values while 
                                   bigger ones are actuals"))
cvResults

saveRDS(lr_model,"./cars_model.rds")

lr_modelss<-readRDS("./cars_model.rds")

lr_modelss
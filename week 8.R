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
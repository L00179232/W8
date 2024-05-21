states <- as.data.frame(state.x77)
str(states)


# Renaming Life Exp and HS Grad variables as "Life_Exp" and "HS_Grad"

colnames(states)[colnames(states) == "Life Exp"] <- "Life_Exp"
colnames(states)[colnames(states) == "HS Grad"] <- "HS_Grad"

# Q3a
# Examine initial linearity between variables in the dataset

library(psych)
windows(20,10)

pairs.panels(states,
             smooth = FALSE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = FALSE,    # If TRUE, draws ellipses
             method = "spearman",# Correlation method (also "pearson" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)          # If TRUE, adds confidence intervals



# Examine linearity in more detail using scatter plots

windows(20,12)
par(mfrow= c(4,2))

scatter.smooth(x = states$Population,
               y = states$Murder,
               xlab = "Population (,000)",
               ylab = "Murder %", main = "Correlation of murder ~ population")

scatter.smooth(x = states$Income ,
               y = states$Murder,
               xlab = "Income  (,000)",
               ylab = "Murder %", main = "Correlation of murder ~ Income ")


scatter.smooth(x = states$Illiteracy,
               y = states$Murder,
               main = "Correlation of Murder ~ Illiteracy",
               xlab = "Illiteracy %",
               ylab = "Murder %")

scatter.smooth(x = states$Life_Exp ,
               y = states$Murder,
               main = "Correlation of Murder ~ Life_Exp ",
               xlab = "Life_Exp ",
               ylab = "Murder %")

scatter.smooth(x = states$HS_Grad ,
               y = states$Murder,
               main = "Correlation of Murder ~ HS_Grad ",
               xlab = "HS_Grad ",
               ylab = "Murder %")

scatter.smooth(x = states$Frost,
               y = states$Murder,
               main = "Correlation of Murder ~ Frost",
               xlab = "Frost",
               ylab = "Murder %")

scatter.smooth(x = states$Area,
               y = states$Murder,
               main = "Correlation of Murder ~ Area",
               xlab = "Area",
               ylab = "Murder %")


# Examining correlation between murder and Independent variables


cor(states)

attach(states)
# Examining the other variables
paste("Correlation for Murder and Population: ", round(cor(Murder, Population),2))
paste("Correlation for Murder and Income: ", round(cor(Murder, Income),2))
paste("Correlation for Murder and Illiteracy: ", round(cor(Murder, Illiteracy),2))
paste("Correlation for Murder and Life Exp: ", round(cor(Murder, Life_Exp),2))
paste("Correlation for Murder and HS Grad: ", round(cor(Murder, HS_Grad),2))
paste("Correlation for Murder and Frost: ", round(cor(Murder, Frost),2))
paste("Correlation for Murder and Area: ", round(cor(Murder, Area),2))

# It appears that the variables 'income' and 'Area' have very low correlation with Murder. 
# if needed  we can remove it from the model( not mandatory). 

windows(20,10)
par(mfrow = c(4,2))
attach(states)

boxplot(Murder,
        main = "Murder") # box plot for 'Murder'
boxplot(Income,
        main = "Income") # box plot for 'Income'
boxplot(Population,
        main = "Population") # box plot for 'Population'
boxplot(Illiteracy,
        main = "Illiteracy") # box plot for 'Illiteracy'
boxplot(Life_Exp,
        main = "Life_Exp") # box plot for 'Life_Exp'
boxplot(HS_Grad,
        main = "HS_Grad") # box plot for 'HS_Grad'
boxplot(Frost,
        main = "Frost") # box plot for 'Frost'
boxplot(Area,
        main = "Area") # box plot for 'Area'


outlier_values <- boxplot.stats(Population)$out 
paste("Population outliers:", paste(outlier_values, sep = ","))

#remove population outliers
states<- subset(states,
                 states$Population != 21198
                 & states$Population != 11197
                 & states$Population != 18076
                 & states$Population != 11860
                 & states$Population != 12237)

outlier_values <- boxplot.stats(states$Population)$out 
paste("Population outliers:", paste(outlier_values, sep = ","))

states<- subset(states,
                states$Population != 9111
                & states$Population != 10735)

outlier_values <- boxplot.stats(states$Income)$out 
paste("income:", paste(outlier_values, sep = ","))

states<- subset(states,
                states$Income != 6315)

outlier_values <- boxplot.stats(states$Area)$out 
paste("Area:", paste(outlier_values, sep = ","))

#check for the normality 
library(e1071)
windows(30,20)
par(mfrow = c(4,2))
attach(states)

#skewness of less than (<) -1 or greater than (>) 1 = highly skewed
# -1 to -0.5 and 0.5 to 1 = moderately skewed 
#skeweness of -0.5 to 0.5 = approx sysetrical 


plot(density(states$Murder),
     main = "Density plot : Murder",
     ylab = "Frequency",xlab = "Murder",
     sub = paste("skewness : ", round(e1071::skewness(states$Murder),2)))
     polygon(density(states$Murder),col = 'red')
     
plot(density(states$Population),
          main = "Density plot : Population",
          ylab = "Frequency",xlab = "Population",
          sub = paste("skewness : ", round(e1071::skewness(states$Population),2)))
     polygon(density(states$Population),col = 'red')
     
plot(density(states$Income),
          main = "Density plot : Income",
          ylab = "Frequency",xlab = "Income",
          sub = paste("skewness : ", round(e1071::skewness(states$Income),2)))
     polygon(density(states$Income),col = 'red')

plot(density(states$Illiteracy),
          main = "Density plot : Illiteracy",
          ylab = "Frequency",xlab = "Illiteracy",
          sub = paste("skewness : ", round(e1071::skewness(states$Illiteracy),2)))
     polygon(density(states$Illiteracy),col = 'red')

plot(density(states$Life_Exp),
          main = "Density plot : Life_Exp",
          ylab = "Frequency",xlab = "Life_Exp",
          sub = paste("skewness : ", round(e1071::skewness(states$Life_Exp),2)))
     polygon(density(states$Life_Exp),col = 'red')

plot(density(states$HS_Grad),
          main = "Density plot : HS_Grad",
          ylab = "Frequency",xlab = "HS_Grad",
          sub = paste("skewness : ", round(e1071::skewness(states$HS_Grad),2)))
     polygon(density(states$HS_Grad),col = 'red')

plot(density(states$Frost),
          main = "Density plot : Frost",
          ylab = "Frequency",xlab = "HS_Grad",
          sub = paste("skewness : ", round(e1071::skewness(states$Frost),2)))
     polygon(density(states$Frost),col = 'red')

plot(density(states$Area),
          main = "Density plot : Area",
          ylab = "Frequency",xlab = "Area",
          sub = paste("skewness : ", round(e1071::skewness(states$Area),2)))
     polygon(density(states$Area),col = 'red')
#check normality for 
     shapiro.test(states$Murder)
     shapiro.test(states$Population)
     shapiro.test(states$Income)
     shapiro.test(states$Illiteracy)
     shapiro.test(states$Frost)
     shapiro.test(states$HS_Grad)
     shapiro.test(states$Area)

#install.packages("MASS")
library(MASS)
View(states)
box_cox_transform <- boxcox(Murder~Population)
box_cox_transform
windows(20,10)
lamda <-box_cox_transform$x[which.max(box_cox_transform$y)]
lamda
normalised_population <- (Murder^lamda-1)/lamda
normalised_population
hist(normalised_population)
shapiro.test(normalised_population)

#modifed the variable population
states$Population_new <- normalised_population
shapiro.test(states$Population_new)

View(states)

#need to transform illiterancy
attach(states)
box_cox_transform <- boxcox(Murder~Illiteracy)
windows(20,10)
lamda <-box_cox_transform$x[which.max(box_cox_transform$y)]
lamda
normalised_illiterancy <- (Murder^lamda-1)/lamda
normalised_illiterancy
hist(normalised_illiterancy)
shapiro.test(normalised_illiterancy)

#modifed the variable population
states$illiterancy_new <- normalised_illiterancy
shapiro.test(states$Illiteracy_new)

#need to transform illiterancy
attach(states)
box_cox_transform <- boxcox(Murder~HS_Grad)
windows(20,10)
lamda <-box_cox_transform$x[which.max(box_cox_transform$y)]
lamda
normalised_HS_Grad <- (Murder^lamda-1)/lamda
normalised_HS_Grad
hist(normalised_HS_Grad)
shapiro.test(normalised_HS_Grad)

#modifed the variable HS_Grad
states$HS_Grad_new <- normalised_HS_Grad
shapiro.test(states$HS_Grad_new)

#Regression model using transformed variables 
str(states)
attach(states)
model_2 <- lm( Murder ~ 
               Income + 
                 Population_new + 
                 illiterancy_new + 
               Life_Exp + 
               HS_Grad + 
               Frost + 
               Area)
model_2
summary(model_2)

Murder ~ .9606 + 2.998 *Population_new + 25.93 * illiterancy_new -27.86 * HS_Grad_new 


               
               
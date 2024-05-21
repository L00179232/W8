
salary_data <- read.csv("salary.csv", na = "")
str(salary_data)

View(salary_data)

summary(salary_data)

windows(20,10)

pairs(salary_data)

#install.packages("psych")

library(psych)

pairs.panels(salary_data,
             
             smooth = FALSE,      # If TRUE, draws loess smooths
             
             scale = FALSE,      # If TRUE, scales the correlation text font
             
             density = TRUE,     # If TRUE, adds density plots and histograms
             
             ellipses = FALSE,    # If TRUE, draws ellipses
             
             method = "pearson",# Correlation method (also "pearson" or "kendall")
             
             pch = 21,           # pch symbol
             
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             
             cor = TRUE,         # If TRUE, reports correlations
             
             jiggle = FALSE,     # If TRUE, data points are jittered
             
             factor = 2,         # Jittering factor
             
             hist.col = 4,       # Histograms color
             
             stars = TRUE,       # If TRUE, adds significance level with stars
             
             ci = TRUE) 

model <- lm(Salary ~ Years + 
              
              Rating + 
              
              Number.of.Subjects,
            
            data = salary_data)

model

summary(model)

windows(20,10)

par(mfrow=c(4,2))


scatter.smooth(x=salary_data$CaseNum,
               
               y=salary_data$Salary,
               
               main = "Scatter Plot of CaseNum vs. Salary",
               
               xlab = "CaseNum",
               
               ylab = "Salary")

scatter.smooth(x=salary_data$Years,
               
               y=salary_data$Salary,
               
               main = "Scatter Plot of Years vs. Salary",
               
               xlab = "Years",
               
               ylab = "Salary")

scatter.smooth(x=salary_data$Rating,
               
               y=salary_data$Salary,
               
               main = "Scatter Plot of Rating vs. Salary",
               
               xlab = "Rating",
               
               ylab = "Salary")               

scatter.smooth(x=salary_data$Number.of.Subjects,
               
               y=salary_data$Salary,
               
               main = "Scatter Plot of No of sub vs. Salary",
               
               xlab = "no of sub",
               
               ylab = "Salary")

correlation_matrix<-cor(salary_data)

windows(20,16)

corPlot(correlation_matrix)


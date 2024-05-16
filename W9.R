states <-as.data.frame(state.x77)
states
View(states)
str(states)
windows(20,10)
pairs(states)
#install.packages("psych")
library(psych)

#renaming the colnmames
colnames(states)[colnames(states) == "Life Exp"]<-"Life_Exp"
         colnames(states)[colnames(states) == "HS Grad"]<-"HS_Grad"
#examine initial linearity 
windows(20,10)
pairs.panels(states,
            smooth = FALSE,      
            scale = FALSE,      
            density = TRUE,     
            ellipses = FALSE,    
            method = "pearson",
            pch = 21,           
            lm = FALSE,         
            cor = TRUE,         
            jiggle = FALSE,   
            factor = 2,         
            hist.col = 4,       
            stars = TRUE,       
            ci = TRUE)   
#model desgin
attach(states)
model <- lm(Murder~ 
              Population +
              Income +
              lliteracy+
              Life_Exp +
              HS_Grad +
                Frost+
                Area,
            data = states)
            
model(summary)
#scatter plot
scatter.smooth(x = states$Population,
               y = states$Murder,
               main = "Correlation of murder ~ population")
scatter.smooth(x = states$Income,
               y = states$Murder,
               main = "Correlation of income")
scatter.smooth(x = states$Illiteracy,
               y = states$Murder,
               main = "Correlation of Illiteracy ~ population")
scatter.smooth(x = states$Life_Exp,
               y = states$Murder,
               main = "Correlation of life_exp ~ population")
scatter.smooth(x = states$HS_Grad,
               y = states$Murder,
               main = "Correlation of HS_Grad ~ population")
scatter.smooth(x = states$Frost,
               y = states$Murder,
               main = "Correlation of frost ~ population")
scatter.smooth(x = states$Area,
               y = states$Murder,
               main = "Correlation of Area ~ population")






               
               
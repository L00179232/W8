insurance <- read_csv("insurance.csv")
View(insurance)
attach(insurance)

str(insurance)
insurance$sex<-factor(sex,
                      levels = c("male","female"),
                      ordered = FALSE)

insurance$smoker<-factor(smoker,
                      levels = c("yes","no"),
                      ordered = FALSE)
insurance$region<-factor(region,
                      levels = c("northeast","northwest","southeast","southwest"),
                      ordered = FALSE)
install.packages("psych")
library(psych)

windows(20,10)
pairs.panels(insurance,
             smooth = TRUE,
             scale=FALSE,
             density=TRUE,
             ellipses=TRUE,
             method="spearman",
             pch=21,
             lm=FALSE,
             cor=TRUE,
             jiggle=FALSE,
             factor=2,
             hist.col=4,
             stars=TRUE,
             ci=TRUE)
attach(insurance)
model<-lm(charges~
            age+
            sex+
            bmi+
            children+
            smoker+
            region)
model
summary(model)
saveRDS(model,"./insurance_model.rds")

charges ~ 11778.7 + 256.9 * age +

    
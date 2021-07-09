getwd()
library(corrplot)
library(epitools)
low <- read.table('lowbwt.txt', header = F, sep = '')
colnames(low) <- c('Identification Code','LBW','Age of Mother','Weight of Mother',
                   'Race','Smoking','Premature Labor','Hypertension',
                   'Uterine Irritability','# of Physician Visits','Birth Weight')
head(low)
par(mfrow=c(2,4))
hist(low$`Age of Mother`, xlab = 'Age of Mother', 
     main = 'Age of Mother during Birth', col = 'Black')
hist(low$`Birth Weight`, xlab = 'Birth Weight', 
     main = 'Distribution of Birth Weight', col = 'Black')
hist(low$Smoking, xlab = 'Smoking Status', main = 'Distribution of Smoking Status',
     col = 'Black', ylim = c(0,120))
hist(low$Race, xlab = 'Race(White, Black, Other)', 
     main = 'Distribution of Race', col = 'Black', ylim = c(0,100))
hist(low$`Premature Labor`, xlab = 'History of Premature Labor',
     main = 'Distribution of Premature Labor', col = 'Black')
hist(low$Hypertension, xlab = 'History of Hypertension',
     main = 'Distribution of Hypertension', ylim = c(0,200),col = 'Black')
hist(low$`Uterine Irritability`, xlab = 'History of Uterine Irritability',
     main = 'Distribution of Uterine Irritability', col = 'Black')
hist(low$`# of Physician Visits`, xlab = 'Number of Physician Visits',
     main = 'Distribution of Physician Visits',col = 'Black')
par(mfrow=c(1,1))
corrplot(cor(low[,-c(1,11)]))
corrplot(cor(low))



#I removed identification code and birthweight from my model due to 
#identification code not being necessary and birth weight being directly 
#correlated to the response variable low birth weight 
set.seed(65875)
low2 <- low[,-c(1,11)]
low2 <- low2[sample(nrow(low2)),] 
model1 <- glm(LBW~`Age of Mother`+`Weight of Mother`+factor(Race)+Smoking+
                `Premature Labor`+Hypertension+`Uterine Irritability`+
                `# of Physician Visits`,family = binomial(link='logit'),
              data = low2)
summary(model1)

par(mfrow=c(2,2))
hist(model1$residuals, xlab = 'Residuals', main = 'Histogram of Residuals')
plot(model1$residuals, ylab = 'Residuals', main = 'Fitted Value')
abline(h=0)
qqnorm(model1$residuals)
qqline(model1$residuals)
plot(model1$residuals, type = 'l', main = 'Observation Order')
abline(h=0)
par(mfrow=c(1,1))

#We can see here that this model is ok for our data based on the
#distribution of the residuals. Lets try using the stepAIC() function to remove
#any unnecessary models.
library(MASS)
bestmodel <- stepAIC(model1, direction = 'backward')
# we can see here that the AIC value is minimized when we have Weight of Mother,
#Race, Smoking, Premature Labor, Hypertension and Uterine Irritability as our
#explanatory variables.

model2 <- glm(LBW~`Weight of Mother` + factor(Race) + Smoking + `Premature Labor` + 
                Hypertension + `Uterine Irritability`, 
              family = binomial(link='logit'), data = low2)
summary(model2)

exp(model2$coefficients)

par(mfrow=c(2,2))
hist(model2$residuals, xlab = 'Residuals', main = 'Histogram of Residuals')
plot(model2$residuals, ylab = 'Residuals', main = 'Fitted Value')
abline(h=0)
qqnorm(model2$residuals)
qqline(model2$residuals)
plot(model2$residuals, type = 'l', main = 'Observation Order')
abline(h=0)
par(mfrow=c(1,1))
#It doesnt look like this didn't do much in regards to the distribution of the
#residuals
#Now lets see which model is actually better
anova(model1, model2)
#This shows us that the condensed model is no better than the full model

#Playing with lasso regression
library(glmnet)
library(plotmo)
cv.lambda <- cv.glmnet(as.matrix(low2[,-1]), as.matrix(low2[,1]), alpha = 1, 
                       nfolds = 3, type.measure = 'mse')
plot(cv.lambda)
round(log(cv.lambda$lambda.min),3) #This corresponds to the 14th iteration
model3 <- glmnet(as.matrix(low2[,-1]), as.matrix(low2[,1]), alpha = 1,
                 family = binomial)
lassomodel <- glmnet(as.matrix(low2[,-1]), as.matrix(low2[,1]), alpha = 1,
                               family = binomial, lambda=cv.lambda$lambda.min)
summary(model3)
coef(lassomodel)
exp(coef(lassomodel))
plot_glmnet(model3, main = 'Evolution of Beta Coefficients for Low Birth Weight')
  abline(v=log(cv.lambda$lambda.min), lty = 'dashed')
  
#Testing Two Models Against Eachother
anova(model2, model3, test='LRT')
  
#Logistic Regression Statistical Machine
x.test <- model.matrix(LBW~., data = low2)[,-1]
probabilities <- predict(lassomodel, newx = x.test)
yhat <- ifelse(probabilities>0.5,1,0)
ylow <- low2[,1]
confusionmatrix <- table(ylow,yhat)
confusionmatrix
sum(diag(confusionmatrix) / sum(confusionmatrix)) #accuracy 
1-sum(diag(confusionmatrix) / sum(confusionmatrix)) #error

#Confusion Matrix for stepAIC model
x.test2 <- model.matrix(LBW~., data = low2)[,-1]
probabilities2 <- predict(model2, newx = x.test)
yhat2 <- ifelse(probabilities2>0.5,1,0)
ylow2 <- low2[,1]
confusionmatrix2 <- table(ylow2,yhat2)
confusionmatrix2
sum(diag(confusionmatrix2) / sum(confusionmatrix2)) #accuracy 
1-sum(diag(confusionmatrix2) / sum(confusionmatrix2)) #error




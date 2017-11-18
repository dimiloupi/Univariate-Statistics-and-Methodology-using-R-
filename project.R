##in collaboration with B095152
#importing the data
cwk_data <- read.csv("https://is.gd/ex1617")

#take a look at the dataset
library(psych)
describe(cwk_data)
#cleaning the outliers by setting their value to NA
#correct the type of a variable
cwk_data$Perf1[cwk_data$Perf1 < 0] <- NA
cwk_data$Perf2 <- as.factor(cwk_data$Perf2)
levels(cwk_data$Perf2)<-c(0, 1, NA)
cwk_data$LifeSat[cwk_data$LifeSat >= 7] <- NA
cwk_data$Diet <- as.factor(cwk_data$Diet)
cwk_data$Age[cwk_data$Age < 18] <- NA
cwk_data$Sex<-as.factor(cwk_data$Sex)
levels(cwk_data$Sex)<-c(1,2, NA)




##NEW DATA BASE WITH NO OUTLIERS##
newdata <- na.omit(cwk_data)
describe(newdata)



##TRYING TO FIND THE PREDICTORS OF PERF1##


#checking for interaction between the variables
cor(newdata$Age, newdata$BMI)
plot(newdata$Age, newdata$BMI)

#fit a model using Age and BMI as X-variables
regr1_Age_BMI <- lm(formula = newdata$Perf1 ~ newdata$Age + newdata$BMI, data = newdata)
summary(regr1_Age_BMI)
plot(regr1_Age_BMI)

#BMI as a predictor of Perf1
regr1_BMI<- lm(formula = newdata$Perf1 ~ newdata$BMI , data = newdata)
with(newdata, plot(Perf1 ~ BMI, main="Correlation between Perf1 and BMI", col="blue"))
abline(regr1_BMI)
summary(regr1_BMI)

#Age as a predictor of Perf1
regr1_Age<- lm(formula = newdata$Perf1 ~ newdata$Age , data = newdata)
with(newdata, plot(Perf1 ~ Age, main="Correlation between Perf1 and Age", col="red"))
abline(regr1_Age)
summary(regr1_Age)

#LifeSat as a predictor of Perf1
regr1_LifeSat <- lm(formula = newdata$Perf1 ~ newdata$LifeSat , data = newdata)
with(newdata, plot(Perf1 ~ LifeSat, main="Correlation between Perf1 and LifeSat", col="darkgreen"))
abline(regr1_LifeSat)
#regression model with all 3 variables
regr1_Age_BMI_LifeSat <- lm(formula = newdata$Perf1 ~ newdata$Age + newdata$BMI + newdata$LifeSat, data = newdata)
summary(regr1_Age_BMI_LifeSat)

##############################################################################
##Evaluate the models for Perf1##


anova(regr1_Age_BMI, regr1_Age_BMI_LifeSat)


###############################################################################
##Trying to find the predictors for Perf2##

regr2_BMI <- glm(formula = newdata$Perf2 ~ newdata$BMI , family = binomial(), data = newdata)
with(newdata, plot.default(BMI ~ Perf2, main="Correlation between Perf2 and BMI"))
summary(regr2_BMI)#glm function because Perf2 is binomial

regr2_BMI_Age <- glm(formula=newdata$Perf2  ~ newdata$BMI + newdata$Age , family = binomial(), data = newdata)
summary(regr2_BMI_Age)

regr2_BMI_Age_LifeSat <- glm(formula=newdata$Perf2 ~ newdata$BMI + newdata$Age + newdata$LifeSat, family = binomial(), data = newdata)
summary(regr2_BMI_Age_LifeSat)

regr2_BMI_Age_LifeSat_Alc <- glm(formula=newdata$Perf2 ~ newdata$Age + newdata$BMI + newdata$Alc + newdata$LifeSat, family = binomial(), data = newdata)
summary(regr2_BMI_Age_LifeSat_Alc)



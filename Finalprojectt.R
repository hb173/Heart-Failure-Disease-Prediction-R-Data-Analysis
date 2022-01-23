#Load Data
library(arm)
library(pROC)
library(e1071)
library(caret)
library(ggplot2)
require(gridExtra)

heart <- read.csv("~/Downloads/heart.csv")
heart_test <- read.csv("~/Downloads/heart.csv")
describe(heart_test)

##Summary
summary(heart)

#Factoring Categorical Variables
heart$Sex<- factor(heart$Sex)
heart$ChestPainType<- factor(heart$ChestPainType)
heart$ST_Slope<- factor(heart$ST_Slope)
heart$ExerciseAngina<- factor(heart$ExerciseAngina)
heart$RestingECG<- factor(heart$RestingECG)
heart$HeartDisease<-factor(heart$HeartDisease)

#Mean center the numerical predictors to avoid Mutli.Col
heart$RestingBP <- heart$RestingBP - mean(heart$RestingBP)
heart$Cholesterol <- heart$Cholesterol - mean(heart$Cholesterol)
heart$FastingBS <- heart$FastingBS - mean(heart$FastingBS)
heart$MaxHR <- heart$MaxHR - mean(heart$MaxHR)
heart$Oldpeak <- heart$Oldpeak - mean(heart$Oldpeak)


str(heart)
#table(heart$HeartDisease)

#factor+conti=boxplot, continous+continous= geompoint, factor+factor=table
#EDA
# Geom_box Plot for Factor and Continious
ggplot(heart, aes(x=HeartDisease,y=Age, fill=HeartDisease))+geom_boxplot()+ 
  labs(title="Age vs Heart Disease",x="HeartDisease",y="Age")


ggplot(heart, aes(x=HeartDisease,y=RestingBP))+geom_boxplot()
ggplot(heart, aes(x=HeartDisease,y=Cholesterol))+geom_boxplot() 
ggplot(heart, aes(x=HeartDisease,y=FastingBS))+geom_boxplot()
ggplot(heart, aes(x=HeartDisease,y=MaxHR))+geom_boxplot()
ggplot(heart, aes(x=HeartDisease,y=Oldpeak))+geom_boxplot()

table(heart[,c("Sex","HeartDisease")])
table(heart[,c("Sex","HeartDisease")])/sum(table(heart[,c("Sex","HeartDisease")]))
apply(table(heart[,c("Sex","HeartDisease")])/sum(table(heart[,c("Sex","HeartDisease")])),
      2,function(x) x/sum(x)) 
tapply(heart$Sex, heart$HeartDisease, function(x) table(x)/sum(table(x)))
chisq.test(table(heart[,c("Sex","HeartDisease")])) 


table(heart[,c("ST_Slope","HeartDisease")])
table(heart[,c("ST_Slope","HeartDisease")])/sum(table(heart[,c("ST_Slope","HeartDisease")]))
apply(table(heart[,c("ST_Slope","HeartDisease")])/sum(table(heart[,c("ST_Slope","HeartDisease")])),
      2,function(x) x/sum(x)) 
tapply(heart$ST_Slope, heart$HeartDisease, function(x) table(x)/sum(table(x)))
chisq.test(table(heart[,c("ST_Slope","HeartDisease")])) 

table(heart[,c("ST_Slope","Sex")])
table(heart[,c("ST_Slope","Sex")])/sum(table(heart[,c("ST_Slope","Sex")]))
apply(table(heart[,c("ST_Slope","Sex")])/sum(table(heart[,c("ST_Slope","Sex")])),
      2,function(x) x/sum(x)) 
tapply(heart$ST_Slope, heart$Sex, function(x) table(x)/sum(table(x)))
chisq.test(table(heart[,c("ST_Slope","Sex")])) 

#age, chl, hyp -> (age, chl), (age, hyp), (chl, hyp) #continious, continious no
ggplot(heart, aes(x=HeartDisease,y=Cholesterol,fill=HeartDisease))+geom_boxplot()+facet_wrap(~Sex)
 
  labs(title="Age vs Heart Disease",x="HeartDisease",y="Age")#yes
ggplot(heart, aes(x=HeartDisease,y=Cholesterol))+geom_boxplot()+geom_smooth()+facet_wrap(~ST_Slope) #yes
ggplot(heart, aes(x=HeartDisease,y=Age))+geom_boxplot()+geom_smooth()+facet_wrap(~Sex) #yes
ggplot(heart, aes(x=HeartDisease,y=Age))+geom_boxplot()+geom_smooth()+facet_wrap(~ST_Slope) #yes
ggplot(heart, aes(x=Cholesterol,y=MaxHR))+geom_point()+geom_smooth() #yes
ggplot(heart, aes(x=Cholesterol,y=Oldpeak))+geom_point()+geom_smooth() #yes


par(mfrow=c(1,1))
binnedplot(y=heart$MaxHR,heart$Cholesterol,xlab="mage",ylim=c(0,1),col.pts="navy",
           ylab ="Premature",main="Binned Premature and Mage",
           col.int="white")

heart$Sex<- factor(heart$Sex)
heart$ChestPainType<- factor(heart$ChestPainType)
heart$ST_Slope<- factor(heart$ST_Slope)
heart$ExerciseAngina<- factor(heart$ExerciseAngina)
heart$RestingECG<- factor(heart$RestingECG)
heart$HeartDisease<-factor(heart$HeartDisease)


table(heart_test$HeartDisease)


heartreg <- glm(HeartDisease ~ Sex + ChestPainType+ Age+ST_Slope +	Cholesterol +
                  FastingBS + MaxHR + Oldpeak+
                  Sex:ST_Slope+Sex:Cholesterol+Cholesterol:ST_Slope+
                  Cholesterol:FastingBS+Cholesterol:MaxHR+FastingBS:MaxHR+
                  MaxHR:Oldpeak+Age:Sex+Age:Cholesterol+Age:FastingBS, data=heart, family = binomial)



#binned residual plots # transformation k jarurat hay ya nahi hay


#aic
back<-step(heartreg, direction = "both", trace=FALSE)
back$call
summary(heartreg)
anova(heartreg)
str(smoking)

#Step
null_model <- glm(HeartDisease~1, data=heart, family=binomial)
step(null_model,scope=formula(heartreg),direction= "both",
     trace=0)


newone3<-glm( HeartDisease ~ Sex  +ChestPainType+ ST_Slope + Cholesterol + 
                FastingBS + MaxHR + Oldpeak + Cholesterol:FastingBS + Cholesterol:MaxHR + 
                FastingBS:MaxHR , family = binomial, 
              data = heart)
anova(newone3,newone2)

Although Stepwise regression using AIC suggested to include Age as a predictor in my final model, on controlling for the rest of the predictors, it is 
insignifact at p=0.05. Hence we can conclude that the predictive effect of Age has been controlled out by the inclusion of the other predictors.

dim(heart)
describe(heart)



anova(newone,newone2)
summary(newone3)

residy <- residuals(newone3)
binnedplot(x=fitted(newone3),y=residy,xlab="Pred. probabilities")


On the Y axis, points are randomly scattered around 0, and there is no obvious linear trend, although outliers exist. 
We do notice some clustering on the X axis. While this does not violate our assumptions, it can indicate that the model is missing 
additional predictors. 





binnedplot(x=heart$RestingB,y=residy,xlab="Pred. probabilities")
binnedplot(x=heart$Cholesterol,y=residy,xlab="Pred. probabilities")
binnedplot(x=heart$MaxHR,y=residy,xlab="Pred. probabilities")
binnedplot(x=heart$Oldpeak,y=residy,xlab="Pred. probabilities")

Confy <- confusionMatrix(as.factor(ifelse(fitted(newone) >= 0.566, "1","0")),
                         as.factor(heart$HeartDisease),positive = "1")
Confy$table
Confy$overall["Accuracy"];
Confy$byClass[c("Sensitivity","Specificity")] #True positive rate and True negative rate

#mean(heart$HeartDisease)
#Confy <- confusionMatrix(as.factor(ifelse(fitted(newone) >= mean(heart$HeartDisease), "1","0")),
                         #as.factor(heart$HeartDisease),positive = "1")
roc(heart$HeartDisease,fitted(newone),plot=T,print.thres="best",legacy.axes=T,
    print.auc =T,col="red3")



confint.default(newone)   #on log odds scale
exp(confint.default(newone))   #on odds scale
summary(newone)
vif(newone)


exp(newone$coefficients)
unique(heart$ChestPainType)


The odds of males having a heart disease is 5 times the odds of females having heart disease. 
The odds of having heartdisease for chestpaintypeATA patients is 0.13 times the odds of having heart disease for patients with chestpaintypeASY.
The odds of having heartdisease for chestpaintypeNAP patients is 0.16 times  the odds of having heart disease for patients with chestpaintypeASY.
The odds of having heartdisease for chestpaintypeTA patients is 0.18 times the odds of having heart disease for patients with chestpaintypeASY.
THe odds of having heartdisease for patients with ST_SlopeFlat patients is 4.7 times odds of having heart disease for patients with ST_SlopeDown.
THe odds of having heartdisease for patients with ST_SlopeUp  is 0.33 times odds of having heart disease for patients with ST_SlopeDown.
The odds of having heartdisease for patients with cholesterol is 1.1 times odds of having heart diesease for patients without Cholesterol
THe odds of having heart disease for patients with oldpeak is 5.1 times odds of having heart disease for patients without Oldpeak
THe odds of having heart disease for patients with MaxHR is 0.99 times odds of having heart disease for patients without MaxHR
Holding all else constant, as OldPeak increases a unit from its mean, on average, the risk of heart disease increases by 1.63 times. 
Holding alll else constant and patient with average cholestrol level, the risk of heart disease increases by 0.99 times



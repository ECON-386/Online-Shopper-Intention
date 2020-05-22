#Exploratory Analysis#

url1 <-"https://raw.githubusercontent.com/kfaenza/Online-Shopper-Intention/master/online_shoppers_intention-2.csv"
download.file(url1,"online_shoppers_intention_2")
View(online_shoppers_intention_2)
summary(online_shoppers_intention_2)
pairs(online_shoppers_intention_2[1:10], col=factor(online_shoppers_intention_2$Revenue))
cov(online_shoppers_intention_2[1:10])
cor(online_shoppers_intention_2[1:10])
plot(ExitRates ~ Administrative, online_shoppers_intention_2)
head(online_shoppers_intention_2)
cor(online_shoppers_intention_2)

pairs(online_shoppers_intention_2[,5:8], col=factor(online_shoppers_intention_2$Month))
pairs(online_shoppers_intention_2[,5:8], col=factor(online_shoppers_intention_2$Month))
cov(online_shoppers_intention_2[1:10])
plot(ExitRates ~ BounceRates, online_shoppers_intention_2)
plot(ProductRelated ~ ProductRelated_Duration, online_shoppers_intention_2)
plot(Administrative_Duration ~ BounceRates, col=factor(online_shoppers_intention_2$Revenue), online_shoppers_intention_2)
plot(Administrative_Duration ~ Administrative, col=factor(online_shoppers_intention_2$Revenue), online_shoppers_intention_2)
plot(ProductRelated_Duration ~ Administrative_Duration, col=factor(online_shoppers_intention_2$Revenue), online_shoppers_intention_2)
plot(ProductRelated_Duration ~ Administrative, col=factor(online_shoppers_intention_2$Revenue), online_shoppers_intention_2)
plot(ProductRelated_Duration ~ Administrative, col=factor(online_shoppers_intention_2$Revenue), online_shoppers_intention_2)
plot(ProductRelated_Duration ~ PageValues, col=factor(online_shoppers_intention_2$Revenue), online_shoppers_intention_2)

#REGRESSION MODELS#

url2 <- "https://raw.githubusercontent.com/ECON-386/Online-Shopper-Intention/master/training_set.csv"
download.file(url2,"training_set")
View(training_set)

MODEL1<-lm(formula = ExitRates ~ Administrative+ Administrative_Duration+ Informational+ Informational_Duration+ ProductRelated+ ProductRelated_Duration+ BounceRates+ PageValues+ SpecialDay, data= training_set)
summary(MODEL1)
summary(MODEL1$residuals)
confint(MODEL1)

MODEL2<-lm(formula = ExitRates ~ Administrative+ Administrative_Duration+ ProductRelated+ ProductRelated_Duration+ BounceRates+ PageValues+ SpecialDay, data= training_set)
summary(MODEL2)
summary(MODEL2$residuals)
confint(MODEL2)

MODEL3<-lm(log1p(ExitRates)~ Administrative+ Administrative_Duration+ Informational+ Informational_Duration+ ProductRelated+ ProductRelated_Duration+ BounceRates+ PageValues+ SpecialDay, data= training_set)
summary(MODEL3)
summary(MODEL3$residuals)
confint(MODEL3)

MODEL4<-lm(formula = log1p(ExitRates)~ Administrative+ Administrative_Duration+ ProductRelated+ ProductRelated_Duration+ BounceRates+ PageValues+ SpecialDay, data= training_set)
summary(MODEL4)
summary(MODEL4$residuals)
confint(MODEL4)

MODEL5<-lm(formula = log1p(ExitRates)~  log1p(Administrative)+ log1p(Administrative_Duration)+ log1p(ProductRelated)+ log1p(ProductRelated_Duration)+ log1p(BounceRates)+ log1p(PageValues)+ log1p(SpecialDay), data= training_set)
summary(MODEL5)
summary(MODEL5$residuals)
confint(MODEL5)
exp(confint(MODEL5))

MODEL6<-lm(formula = ExitRates~ log1p(Administrative)+ log1p(Administrative_Duration)+ log1p(ProductRelated)+ log1p(ProductRelated_Duration)+ log1p(BounceRates)+ log1p(PageValues)+ log1p(SpecialDay), data= training_set)
summary(MODEL6)
summary(MODEL6$residuals)
confint(MODEL6)
exp(confint(MODEL6))

#REGRESSION VALIDATION#
url3 <-"https://raw.githubusercontent.com/ECON-386/Online-Shopper-Intention/master/Testing_Set.csv"
download.file(url3,"Testing_Set")
View(Testing_Set)

RMSE_IN_M1<-sqrt(sum(MODEL1$residuals^2)/length(MODEL1$residuals))  
RMSE_OUT_M1<-sqrt(sum((predict(MODEL1, Testing_Set)-Testing_Set$ExitRates)^2)/length(Testing_Set)) 

RMSE_IN_M1  
RMSE_OUT_M1

RMSE_IN_M2<-sqrt(sum(MODEL2$residuals^2)/length(MODEL2$residuals))  
RMSE_OUT_M2<-sqrt(sum((predict(MODEL2, Testing_Set)-Testing_Set$ExitRates)^2)/length(Testing_Set)) 

RMSE_IN_M2
RMSE_OUT_M2

RMSE_IN_M3<-sqrt(sum(MODEL3$residuals^2)/length(MODEL3$residuals))  
RMSE_OUT_M3<-sqrt(sum((predict(MODEL3, Testing_Set)-Testing_Set$ExitRates)^2)/length(Testing_Set)) 

RMSE_IN_M3
RMSE_OUT_M3

RMSE_IN_M4<-sqrt(sum(MODEL4$residuals^2)/length(MODEL4$residuals))  
RMSE_OUT_M4<-sqrt(sum((predict(MODEL4, Testing_Set)-Testing_Set$ExitRates)^2)/length(Testing_Set)) 

RMSE_IN_M4
RMSE_OUT_M4

RMSE_IN_M5<-sqrt(sum(MODEL5$residuals^2)/length(MODEL5$residuals))  
RMSE_OUT_M5<-sqrt(sum((predict(MODEL5, Testing_Set)-Testing_Set$ExitRates)^2)/length(Testing_Set)) 

RMSE_IN_M5
RMSE_OUT_M5

RMSE_IN_M6<-sqrt(sum(MODEL6$residuals^2)/length(MODEL6$residuals))  
RMSE_OUT_M6<-sqrt(sum((predict(MODEL6, Testing_Set)-Testing_Set$ExitRates)^2)/length(Testing_Set)) 

RMSE_IN_M6
RMSE_OUT_M6

VecOfOnes<-rep(1,times=dim(training_set)[1])
X<-cbind(as.matrix(VecOfOnes), as.matrix(unlist(training_set[,1])),as.matrix(unlist(training_set[,2])),as.matrix(unlist(training_set[,3])), as.matrix(unlist(training_set[,4])),as.matrix(unlist(training_set[,5])), as.matrix(unlist(training_set[,6])), as.matrix(unlist(training_set[,7])),as.matrix(unlist(training_set[,9])),as.matrix(unlist(training_set[,10]))) 
y<-as.matrix(training_set[,8])
PseudoInverse<-solve(t(X)%*%X)%*%t(X)  
Beta<-PseudoInverse%*%y 
Beta

#VERIFYING SOLUTION MATCHES#
VecOfOnes<-rep(1,times=dim(Testing_Set)[1])
X_test<-cbind(as.matrix(VecOfOnes), as.matrix(unlist(Testing_Set[,1])),as.matrix(unlist(Testing_Set[,2])),as.matrix(unlist(Testing_Set[,3])), as.matrix(unlist(Testing_Set[,4])),as.matrix(unlist(Testing_Set[,5])), as.matrix(unlist(Testing_Set[,6])), as.matrix(unlist(Testing_Set[,7])),as.matrix(unlist(Testing_Set[,9])),as.matrix(unlist(Testing_Set[,10]))) 

pred_in<-X%*%Beta
pred_out<-X_test%*%Beta
RMSE_IN_verify<-sqrt(sum((pred_in-training_set$ExitRates)^2)/length(pred_in)) 
RMSE_OUT_verify<-sqrt(sum((pred_out-Testing_Set$ExitRates)^2)/length(pred_out))
RMSE_IN_verify
RMSE_OUT_verify

#CLASSIFICATION MODELS#

CMODEL1<- glm(Revenue ~ SpecialDay +Month + OperatingSystems + Browser + Region + TrafficType + VisitorType + Weekend, data = training_set, family = "binomial")
summary(CMODEL1)

signal1<-predict(CMODEL1, training_set)
pred_prob1<-(1/(1+exp(-signal1)))
View(pred_prob1)
confint(CMODEL1) 
point_conf_table1<-cbind(CMODEL1$coefficients, confint(CMODEL1))
point_conf_table1
exp(point_conf_table1)

class(training_set$Month)
class(training_set$VisitorType)
class(training_set$Weekend)
class(training_set$Browser)
class(training_set$Region)
class(training_set$SpecialDay)
class(training_set$TrafficType)
class(training_set$Revenue)
class(training_set$OperatingSystems)

training_set$catBrowser <- factor(training_set$Browser)
head(training_set)
class(training_set$catBrowser)
Testing_Set$catBrowser <- factor(Testing_Set$Browser)

training_set$catRegion <- factor(training_set$Region)
head(training_set)
class(training_set$catRegion)
Testing_Set$catRegion <- factor(Testing_Set$Region)

training_set$catTrafficType <- factor(training_set$TrafficType)
head(training_set)
class(training_set$catTrafficType)
Testing_Set$catTrafficType <- factor(Testing_Set$TrafficType)
class(Testing_Set$catTrafficType)

training_set$catSpecialDay <- factor(training_set$SpecialDay)
head(training_set)
class(training_set$catSpecialDay)
Testing_Set$catSpecialDay <- factor(Testing_Set$SpecialDay)

training_set$catOperatingSystems <- factor(training_set$OperatingSystems)
head(training_set)
class(training_set$catOperatingSystems)
Testing_Set$catOperatingSystems <- factor(Testing_Set$OperatingSystems)

training_set$catVisitorType <-factor(training_set$VisitorType)
head(training_set)$catVisitorType
class(training_set$catVisitorType)
Testing_Set$catVisitorType <-factor(Testing_Set$VisitorType)

CMODEL1.1<- glm(formula= Revenue~ Weekend + VisitorType + catTrafficType + catRegion +catBrowser + catOperatingSystems + catSpecialDay + Month, data=training_set, family="binomial")
summary(CMODEL1.1)

signal1.1<-predict(CMODEL1.1, training_set)
pred_prob1.1<-(1/(1+exp(-signal1.1)))
View(pred_prob1.1)
confint(CMODEL1.1) 
point_conf_table1.1<-cbind(CMODEL1.1$coefficients, confint(CMODEL1.1))
point_conf_table1.1
exp(point_conf_table1.1)

#VALIDATION MODEL 1.1#
confusionMatrix(table(predict(CMODEL1.1, training_set, type="response") >= 0.5,
                      training_set$Revenue == 1))


confusionMatrix(table(predict(CMODEL1.1, Testing_Set, type="response") >= 0.5,
                      Testing_Set$Revenue == 1))


CMODEL2<- glm(Revenue~ Weekend+ VisitorType + catTrafficType + catRegion + catSpecialDay + Month, data=training_set, family= "binomial")
summary(CMODEL2)

signal2<-predict(CMODEL2, training_set)
pred_prob2<-(1/(1+exp(-signal2)))
View(pred_prob2)
confint(CMODEL2) 
point_conf_table2<-cbind(CMODEL2$coefficients, confint(CMODEL2))
point_conf_table2
exp(point_conf_table2)

#VALIDATION MODEL 2#
confusionMatrix(table(predict(CMODEL2, training_set, type="response") >= 0.5,
                      training_set$Revenue == 1))


confusionMatrix(table(predict(CMODEL2, Testing_Set, type="response") >= 0.5,
                      Testing_Set$Revenue == 1))

class(training_set$Revenue)
class(Testing_Set$Revenue)
training_set$Revenue <- as.logical(training_set$Revenue)
class(training_set$Revenue)


CMODEL3<- glm(Revenue~ Administrative+ Administrative_Duration+ Informational+ Informational_Duration+ ProductRelated+ ProductRelated_Duration+ BounceRates+ ExitRates+ PageValues+ Weekend + VisitorType + catTrafficType + catRegion +catBrowser + catOperatingSystems + catSpecialDay + Month, data = training_set, family = "binomial")
summary(CMODEL3)
signal3<-predict(CMODEL3, training_set)
pred_prob3<-(1/(1+exp(-signal3)))
View(pred_prob3)
exp(cbind(CMODEL3$coefficients, confint(CMODEL3)))

#VALIDATION MODEL 3#
confusionMatrix(table(predict(CMODEL3, training_set, type="response") >= 0.5,
                      training_set$Revenue == 1))

confusionMatrix(table(predict(CMODEL3, Testing_Set, type="response") >= 0.5,
                      Testing_Set$Revenue == 1))

CMODEL4<- glm(Revenue~ ExitRates+ PageValues+ Weekend + VisitorType + catTrafficType + Month, data = training_set, family = "binomial")
summary(CMODEL4)
signal4<-predict(CMODEL4, training_set)
pred_prob4<-(1/(1+exp(-signal4)))
View(pred_prob4)
exp(cbind(CMODEL4$coefficients, confint(CMODEL4)))

#VALIDATION MODEL 4#
confusionMatrix(table(predict(CMODEL4, training_set, type="response") >= 0.5,
                      training_set$Revenue == 1))

confusionMatrix(table(predict(CMODEL4, Testing_Set, type="response") >= 0.5,
                      Testing_Set$Revenue == 1))






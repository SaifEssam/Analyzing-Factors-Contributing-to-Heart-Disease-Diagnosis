##Libraries we need 
library(DescTools)
library(dplyr)
library(descr)
library(MESS) 
library(epitools)
library(vcd)
library(PerformanceAnalytics)
library(caret)
library(pROC)
library(plotROC)
library(ROCR)
library(pscl)
library(lmtest)
library(ResourceSelection)
library(ggplot2)
library(e1071)
library(caTools)
library(class)
library(rpart)
library(rpart.plot)
##################################################################################################################
##examining the data and true types of the variable 
data<-read.csv("heart.csv",header=T)
View(data) #to see variables 
str(data) #to examine if variables are truly defined the categorical variables defined as factor 
apply(data[c(2,3,6,7,9,11,12)],2,table)
#####################################################################################################################
#############data prepration ####################
###missing and outliers 
summary(data) # to see if there is a missing
boxplot(data[,c(1,4,5,8,10)]) # i have outliers in variables RestingBP,Cholesterol 
Q1 <- quantile(data$Cholesterol, .25) ; Q3 <- quantile(data$Cholesterol, .75)
IQR <- IQR(data$Cholesterol)
no_outliers2 <- subset(data, data$Cholesterol> (Q1 - 1.5*IQR) & data$Cholesterol< (Q3 + 1.5*IQR))
Q1 <- quantile(data$RestingBP, .25) ; Q3 <- quantile(data$RestingBP, .75)
IQR <- IQR(data$RestingBP)
no_outliers2 <- subset(data, data$RestingBP> (Q1 - 1.5*IQR) & data$RestingBP< (Q3 + 1.5*IQR))
nrow(data)/nrow(no_outliers2)*100 #outliers represent approximately 5% of data 

mean(no_outliers2$Age,trim=.10) ;mean(no_outliers2$RestingBP,trim=.10) ;mean(no_outliers2$Cholesterol,trim=.10);mean(no_outliers2$Oldpeak,trim=.10)
#53.50142,130.3063,204.3575,0.7220798 "compare results with the mean in outliers

### recoding the variables 
no_outliers2$Sex<-recode(no_outliers2$Sex,"F"=0,"M"=1)
no_outliers2$ChestPainType<-recode(no_outliers2$ChestPainType,"ASY"=1,"NAP"=2,"ATA"=3,"TA"=4)
no_outliers2$RestingECG<-recode(no_outliers2$RestingECG,"Normal"=1,"LVH"=2,"ST"=3)
no_outliers2$ExerciseAngina<-recode(no_outliers2$ExerciseAngina,"N"=0,"Y"=1)
no_outliers2$ST_Slope<-recode(no_outliers2$ST_Slope,"Flat"=1,"Up"=2,"Down"=3)

#####correcting types of the variables
no_outliers2$Sex<-as.factor(no_outliers2$Sex)
no_outliers2$ChestPainType<-as.factor(no_outliers2$ChestPainType)
no_outliers2$HeartDisease<-as.factor(no_outliers2$HeartDisease)
no_outliers2$FastingBS<-as.factor(no_outliers2$FastingBS)
no_outliers2$RestingECG<-as.factor(no_outliers2$RestingECG)
no_outliers2$ExerciseAngina<-as.factor(no_outliers2$ExerciseAngina)
no_outliers2$ST_Slope<-as.factor(no_outliers2$ST_Slope)

#####################################################################################################################
#######descriptive and simple plots ################
###descriptive meassures
apply(no_outliers2[c(2,3,6,7,9,11,12)],2,table) #table for qualitative variables
apply(no_outliers2[c(1,4,5,8,10)],2,Freq) #table for quantitative variables
#####################################################################################################################
#simple plots 
#plots for gender
freq1 <- table(no_outliers2$Sex)
labelsG<-c("FEMALE","MALE"); x<-c(193,725)
barplot(freq1,names.arg = labelsG,ylab = "Frequency",
        main = "barplot for Gender",col = "lightblue",border = "pink")
pie(x,labelsG,main="Gender",col=c("pink","lightblue"))
#plots for chestpaintype
freq2<-table(no_outliers2$ChestPainType)
labelsG<-c("ASY","NAP"," ATA","TA"); x<-c(496,699,203,173,46)
barplot(freq2,names.arg = labelsG,ylab = "Frequency",
        main = "barplot for chestpaintype",col = "red",border = "blue")
pie(x,labelsG,main="ChestPainType",col=c("pink","blue","red","green"))
#plots for FastingBS
labelsG<-c("0","1"); x<-c(704,214)
pie(x,labelsG,main="FastingBS",col=c("blue","red"))
freq3<-table(no_outliers2$FastingBS)
barplot(freq3,names.arg = labelsG,ylab = "Frequency",
        main = "barplot for FastingBS",col = "red",border = "blue")
#plots for restingECG
labelsG<-c("Normal","LVH","ST"); x<-c(552,188,178)
pie(x,labelsG,main="RestingECG",col=c("yellow","blue","red"))
freq4<-table(no_outliers2$RestingECG)
barplot(freq4,names.arg = labelsG,ylab = "Frequency",
        main = "barplot for RestingECG",col = "red",border = "blue")
#plots for ExerciseAngina
labelsG<-c("N","Y"); x<-c(547,371)
pie(x,labelsG,main="ExerciseAngina",col=c("red","blue"))
freq5<-table(no_outliers2$ExerciseAngina)
barplot(freq5,names.arg = labelsG,ylab = "Frequency",
        main = "barplot for RestingECG",col = "blue")
#plots for st slope
labelsG<-c("Flat","Up","Down"); x<-c(460,395,63)
pie(x,labelsG,main="ST_Slope",col=c("lightblue","orange","pink"))
freq6<-table(no_outliers2$ST_Slope)
barplot(freq6,names.arg = labelsG,ylab = "Frequency",main = "barplot for ST_slope",col = "lightblue",border="pink")
#plots for heart disease 
labelsG<-c("0","1"); x<-c(410,508)
pie(x,labelsG,main="HeartDisease",col=c("lightgreen","orange"))
freq7<-table(no_outliers2$HeartDisease)
barplot(freq7,names.arg = labelsG,ylab = "Frequency",main = "barplot for HeartDisease",col = "lightgreen",border="orange")
#plots for quantitative variables 1.age
boxplot(no_outliers2$Age,main="boxplot for age",col="lightblue")
hist(no_outliers2$Age,col="lightblue",main="histogram for age")
#2.restingbp
boxplot(no_outliers2$RestingBP,main="boxplot for RestingBP",col="lightgreen")
hist(no_outliers2$RestingBP,col="lightgreen",main="histogram for RestingBP")
#3.Cholesterol
par(mfrow=c(1,3))
boxplot(no_outliers2$Cholesterol,main="boxplot for Cholesterol",col="green")
hist(no_outliers2$Cholesterol,col="green",main="histogram for Cholesterol")
den <- density(no_outliers2$Cholesterol)
plot(den, frame = FALSE, main = "Density plot for cholestrol", xlab = 'cholestrol')
polygon(den, col = "green")
#4.Max HR
boxplot(no_outliers2$MaxHR,main="boxplot for MaxHR",col="pink")
hist(no_outliers2$MaxHR,col="pink",main="histogram for MaxHR")
den <- density(no_outliers2$MaxHR)
plot(den, frame = FALSE, main = "Density plot for MaxHR", xlab = 'MaxHR')
polygon(den, col = "pink")
#5.Oldpeak
boxplot(no_outliers2$Oldpeak,main="boxplot for Oldpeak",col="orange")
hist(no_outliers2$Oldpeak,col="orange",main="histogram for Oldpeak")
den <- density(no_outliers2$Oldpeak)
plot(den, frame = FALSE, main = "Density plot for Oldpeak", xlab = 'Oldpeak')
polygon(den, col = "orange")
par(mfrow=c(1,1))
#########################################################################################################
###association and correlation with histogram and boxplots 
attach(no_outliers2)
CrossTable(Sex,HeartDisease,prop.r =T,prop.c =T,prop.chisq =F,prop.t=T,row.labels =TRUE)
CrossTable(ExerciseAngina,HeartDisease,prop.r =T,prop.c =T,prop.chisq =F,prop.t=T,row.labels =TRUE)
breaks<-c(0,15,30,45,60,77)
categorize_age<-cut(no_outliers2$Age,breaks)
CrossTable(categorize_age,HeartDisease,prop.r =T,prop.c =T,prop.chisq =F,prop.t=T,row.labels =TRUE)
CrossTable(Sex,ChestPainType,prop.r =T,prop.c =T,prop.chisq =F,prop.t=T,row.labels =TRUE)
#association between two variables 
a<-table(Sex,HeartDisease) ; oddsratio.wald(a) #for 2 binary variables
b<-table(ExerciseAngina,HeartDisease) ;oddsratio.wald(b)
c<-table(categorize_age,HeartDisease) ; gkgamma(c)
d<-table(Sex,ChestPainType) ;assocstats(d) #gamma check chestpaintype
chart.Correlation(no_outliers2[,c(1,4,5,8,10)],histogram =T) 
#graphs for pairs 
par(mar=c(5, 4, 4, 8), xpd=TRUE)
labelsf<-c("no heart disease","heart disease")
labelsG<-c("FEMALE","MALE")
bi_freq <- table(no_outliers2$HeartDisease,no_outliers2$Sex)
barplot(bi_freq,beside = TRUE,col = c("lightblue","lightgreen"),
        names.arg = labelsG,ylab = "Frequency",main = "heart disease Distribution by sex")
legend("right",inset=c(-0.5,-0.4),legend=labelsf,fill = c("lightblue","lightgreen"))
###
bi_freq2 <- table(no_outliers2$HeartDisease,categorize_age)
labels_age<-c("0-15","15-30","30-45","45-60","60-77")
barplot(bi_freq2,beside = TRUE,col = c("lightblue","lightgreen"),names.arg = labels_age,
        ylab = "Frequency",main = "heart disease Distribution by age")
legend("topright",inset=c(-0.3,0), 
       legend=labelsf, fill = c("lightblue","lightgreen"))
###
labelst<-c("No","yes")
bi_freq2 <- table(no_outliers2$HeartDisease,no_outliers2$ExerciseAngina)
barplot(bi_freq2,beside = TRUE,col = c("pink","blue"),
        names.arg = labelst,ylab = "Frequency",main = "heart disease by exercise")
legend("right",inset=c(-0.5,0),legend=labelsf, fill = c("pink","blue")) 
####
par(mfrow=c(1,1))
labelsh<-c("NAP","ASY","ATA","TA")
bi_freq4 <- table(no_outliers2$ChestPainType,no_outliers2$Sex)
barplot(bi_freq4,beside = TRUE,col = c("pink","blue","green","lightblue"),
        names.arg = labels_age,ylab = "Frequency",main = "Chest pain type by Gender")
legend(x=8,y=350,legend=labelsh, fill = c("pink","blue","green","lightblue")) 
###
pairs(~no_outliers2$MaxHR + no_outliers2$RestingBP,
      labels = c('heart.rate', 'blood.pressure'),
      upper.panel = NULL,
      pch = 21,
      bg = c("blue", "red","orange","pink","lightblue")[unclass(categorize_age)])
##quantitative 
boxplot(Cholesterol ~Sex ,data=no_outliers2 ,main="Boxplot for Cholesterol per sex",col="lightblue")
boxplot(Cholesterol~ExerciseAngina ,data=no_outliers2 ,main="Boxplot for Cholesterol per exercise",col="lightblue")
boxplot(MaxHR~ExerciseAngina ,data=no_outliers2 ,main="Boxplot for MaxHR per exercise",col="lightblue")

#histogram
par(mfrow = c(2, 2))
hist(Cholesterol,main="histogram for Cholestrol",col="lightblue")
hist(MaxHR,main="histogram for MaxHR",col="orange")
hist(Oldpeak,main="histogram for Oldpeak",col="pink")
hist(RestingBP,main="histogram for Restingbloodpressure",col="lightgreen")

#####################################################################################################################
###logistic regression 
#. Split the dataset into a training set and a testing test with 70% of the data used for training and 30%
#for testing.
set.seed(1)
sample <- sample(c(TRUE, FALSE), nrow(no_outliers2), replace=TRUE, prob=c(0.7,0.3))
train  <- no_outliers2[sample, ]
test   <- no_outliers2[!sample, ]
#######################################################################################################
###################################################################################################################
##fitting logistic on train
glm_full<-glm(train$HeartDisease ~ ., data = train, family = binomial)

summary(glm_full)
car::vif(glm_full)
pscl::pR2(glm_full)["McFadden"]
##########################################################################################################
#backward model 
selection<-step(glm_full,direction = "backward")

glm_full_2<-glm(train$HeartDisease~train$Sex+train$ChestPainType
                +train$Cholesterol+train$FastingBS+train$MaxHR
                +train$ExerciseAngina+train$Oldpeak, data = train, family = "binomial")
summary(glm_full_2)
car::vif(glm_full_2)
pscl::pR2(glm_full_2)["McFadden"]
############################################################################################################
#stepwise model
selection_2<-step(glm_full,direction = "both")
summary(selection_2)
pscl::pR2(selection_2)["McFadden"]
car::vif(selection_2)
################################################################################################
####chooosing the best model 
lrtest(glm_full,glm_full_2)
lr_test <- lrtest(glm_full,selection_2)
#########################################################################################################
####predicting using test data set 
predictedlogit <- predict(selection_2, test,type = 'response')
####################################################################################################
#model performance evaluation 
hist(predictedlogit)
pred<-prediction(predictedlogit,test$HeartDisease)
roc<-performance(pred,"acc")
plot(roc)
abline(h=0.8834586,v=0.4154449)
######################################################################################################
#classification table
predictclass<-ifelse(predictedlogit>=0.4154449  ,1,0)
confusionMatrix(test$HeartDisease, factor(predictclass))
CM= table(test$HeartDisease , predictclass)
###############################################################################
###ROC curve and area under curve 
test$logit_probability <- predictedlogit

test$Predicted = ifelse(test$logit_probability > 0.4154449, 1,0)

xtabs(~test$Predicted + test$HeartDisease)

mean(test$Predicted==test$HeartDisease) 
#he percentage of correct classification = 97.23333%
# 5. Produce the ROC curve of the test set and get the Area Under the Curve.

roc_area <- roc(test$HeartDisease, test$logit_probability)
roc_area


test$default_num <- as.numeric(test$HeartDisease)-1

ggplot(test,aes(m = logit_probability, d = default_num))+ 
  geom_roc(cutoffs.at = c(seq(0,1, by = 0.1)),color = "darkgreen")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1))+labs(
    title = "ROC Curve ",x = "1 - Specificity" ,
    y = "Sensitivity") + theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "#333333"),
    text = element_text(color = "#333333"),
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12))

#######################################################################################################################
#######Machine learning and algorithms
#### prediction using KNN algorithm for prediction
test$Predicted=NULL
train$predicted=NULL
test$logit_probability=NULL
test$default_num=NULL
misClassError<-c()
t<-seq(1,15,1)
for(i in 1:length(t)){classifier_knn <- knn(train = train,
                                            test = test,
                                            cl = train$HeartDisease,
                                            k = i);
misClassError[i] <- mean(classifier_knn != test$HeartDisease)}
which.min(misClassError)



plot(1:15, misClassError, type = 'b', pch = 25, col = 'darkred', xlab = 'k', ylab = 'Misclassification Error')


### optimal K=9

classifier_knn <- knn(train = train,
                      test = test,
                      cl = train$HeartDisease,
                      k = 9)
misClassError <- mean(classifier_knn != test$HeartDisease)
print(paste('Accuracy =', 1-misClassError))

table(test$HeartDisease,classifier_knn)
confusionMatrix(test$HeartDisease, classifier_knn)

#######################################################################################################################
###Descion tree
train2<-train
train2$predicted=NULL
model <- rpart(train2$HeartDisease ~ ., data = train2, method = "class")
rpart.plot(model)

## feature importance

importances <- varImp(model)
importances %>% arrange(desc(Overall))

## Making predictions
##make sure to specify type = "class" for everything to work correctly
preds <- predict(model, newdata = test, type = "class")
preds

## confusion matrix
confusionMatrix(test$HeartDisease, preds)


##############################################################################################################
###for loop 

patients <- seq(1,876,1)
results <- {}
for (i in 1:length(patients)) {
  results[i] <- ifelse(no_outliers2$Cholesterol[i] >= 200 & no_outliers2$MaxHR[i]> 220- no_outliers2$
                         Age[i]&no_outliers2$RestingBP[i]>120,"GO TO DOCTOR", "need more invistigation")}


k<-as.data.frame(results)
data2<-cbind(k,no_outliers2)

results








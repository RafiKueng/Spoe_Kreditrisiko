
install.packages("Hmisc") #important
install.packages("DescTools") #imp
install.packages("rpart") #imp
install.packages("rpart.plot") #imp
install.packages("ggplot2")
install.packages("lattice")
install.packages("RColorBrewer") #imp.
install.packages("corrplot")
install.packages("psych")
install.packages("GGally")
install.packages("corrr")
install.packages("ggcorrplot")
install.packages("gridExtra")   #imp.
install.packages("grid")     #imp.
install.packages("gmodels")   #imp.
install.packages("nnet") 
install.packages("magrittr")   
install.packages("partykit")
install.packages("rattle")

#loading all necessary libraries
library(Hmisc) #important
library(DescTools) #imp
library(rpart) #imp
library(rpart.plot) #imp
library(ggplot2)
library(lattice)
library(RColorBrewer) #imp.
library(corrplot)
library(psych)
library(GGally)
library(corrr)
library(ggcorrplot)
library(gridExtra)   #imp.
library(grid)     #imp.
library(gmodels)   #imp.
library(nnet)   #imp.
library(magrittr)



####### Neural Network and Fitting/Diagnostics/Descreptive Statistics in General
###
a) Fitting a neural network model
require(nnet)
tst <- nnet(1000/MPG.city ~ Weight + Origin + Type, Cars93, linout = T, size = 0, 
            decay = 0, rang = 0, skip = T, trace = T)
# weights:  8
initial  value 213926.871110 
iter  10 value 1560.625594
final  value 1461.849465 
converged

coef(tst)
b->o      i1->o      i2->o      i3->o      i4->o      i5->o 
6.0150986  0.0133782 -1.2029033 -0.6509524  0.7844866 -1.4885942 
i6->o      i7->o 
2.9627636  2.3876101 



b) Fitting a linear regression model
tst.reg <- lm(MPG.city ~ Weight + Origin + Type, Cars93)
par(mfrow = c(2, 2), pty = "s")
plot(tst.reg)

par(mfrow = c(1, 1))

library(car)
qqPlot(lm(MPG.city ~ Weight + Origin + Type, data = Cars93), simulate = FALSE, 
       main = "Q-Q Plot", labels = FALSE, xlab = "t Quantiles", ylab = "Studentized Residuals")

###boxcox ist sehr gut um zu sehen in welchem 95% bereich der loglikelihood akzeptabel ist
boxcox(MPG.city ~ Weight + Origin + Type, data = Cars93)


tst1 <- lm(1000/MPG.city ~ Weight + Origin + Type, Cars93)
coef(tst1)

par(mfrow = c(2, 2), pty = "s")
plot(tst1)

par(mfrow = c(1, 1))
qqPlot(lm(1000/MPG.city ~ Weight + Origin + Type, data = Cars93), simulate = FALSE, 
       main = "Q-Q Plot", labels = FALSE, xlab = "t Quantiles", ylab = "Studentized Residuals")

######## Finish Fitting and Diagnostics


#find out which model (, binomial regression rain forest, neural network, vector machine etc.) is the best for our data
#response variable is whether we give a credit or not
-> recommended classification tree, it's mmuch easier to explain than f.e. blackbox neural neutworks' und for allem, predi tion accuracy ist gut! und anschaulich für kunde
-> kleinster baum nehmen, 2 äste
-> dont use to sophisticated models! oder vorsichtig damit

#Presentation
3 kurze schritte:
  - was ist das problem? problem erklären
  - welche modelle habe ich angewendet
  - conclusion warum das oder das Modell letztlich angewendet wurde.



###for presentation, only show the most important results (not trial which doesnt give a good output, neural networks f.e.)


##Homework
#1) Exploratory Data Analysis




######### START



#1. Preparation of the data
#TODO : in EDUCATION Var gibts es 1 obs. mit -1 -> ändern in 1, da wahrscheinlich tippfehler
# DITO GUARANTOR 1 obs. mit 2 statt 0/1

gc <- read.csv2("C:/Users/Spörri/Desktop/Marc UniNE/SS 17 Seminar of Applied Statistics/Homework/GermanCredit.csv",dec=".",header=T)
quanti<-c(3,11,23)  #select the columns of the quantitative variables
#I'm considering INSTALL_RATE (var nr. 14), num_credits (27) and num_dependents (29) as a categorial variable, since 
#none of these 3 variables has more than 4 ausprägungen, especially num_dependents with only 2 ausprägungen
quali<-1:31
quali<-quali[-quanti]
RESPONSE<-as.factor(gc[,32])
gc.quali<-apply(gc[,quali],2,as.factor)
gc.quali
gc.quali[,8][gc.quali[,8]== -1] <- 1 #cleaning the var. EDUCATION because there was one typing error (obs. 37 with "-1" instead of presumably "1"); "-1" represents no category for this variable
gc.quali[,17][gc.quali[,17]== 2] <- 1 #cleaning the var. GUARANTOR because there was one typing error (obs. 234 with "2" instead of presumably "1"); "2" represents no category for this variable
summary(gc.quali) #summary for qualitative variables
gc.quanti<-gc[,quanti]
gc.quanti
summary(gc.quanti) #summary for the quantitative variables
gc<-cbind(gc.quali,gc.quanti,RESPONSE)
gc
attach(gc)

##Descriptive Statistics
str(gc)
head(gc)
summary(gc)
summary(RESPONSE~.,data=gc)
describe(gc)
#as_tibble(gc) #not working could not find function "as_tibble"
Desc(gc)
Desc(RESPONSE~.,data=gc)


###2. Selection of the most important covariates

##2.1 Descriptives Stats of the qualitatives var
#adjusting the categorial variables by (x-min)/(max-min)
showing the barplots after adjusting

##2.2 selection of the qualitative var: crossing all qualitative variables with the RESPONSE variable
#24 crosstables

CrossTable(CHK_ACCT, RESPONSE, digits=2, max.width = 5, expected=FALSE, prop.r=TRUE, prop.c=TRUE, prop.t=TRUE, prop.chisq=TRUE)
CrossTable(CHK_ACCT, RESPONSE, digits=2, expected=T,prop.chisq=TRUE)
#keep the variable since p-value of the chi^2 test is <0.05
                
CrossTable(HISTORY, RESPONSE, digits=2, max.width = 5, expected=FALSE, prop.r=TRUE, prop.c=TRUE, prop.t=TRUE, prop.chisq=TRUE)
CrossTable(HISTORY, RESPONSE, digits=2, expected=T,prop.chisq=TRUE)
#keep the variable since p-value of the chi^2 test is <0.05

CrossTable(NEW_CAR, RESPONSE, digits=2, max.width = 5, expected=FALSE, prop.r=TRUE, prop.c=TRUE, prop.t=TRUE, prop.chisq=TRUE)
CrossTable(NEW_CAR, RESPONSE, digits=2, expected=T,prop.chisq=TRUE)
#keep the variable since p-value of the chi^2 test is <0.05

CrossTable(USED_CAR, RESPONSE, digits=2, max.width = 5, expected=FALSE, prop.r=TRUE, prop.c=TRUE, prop.t=TRUE, prop.chisq=TRUE)
CrossTable(USED_CAR, RESPONSE, digits=2, expected=T,prop.chisq=TRUE)
#keep the variable since p-value of the chi^2 test is <0.05

CrossTable(FURNITURE, RESPONSE, digits=2, max.width = 5, expected=FALSE, prop.r=TRUE, prop.c=TRUE, prop.t=TRUE, prop.chisq=TRUE)
CrossTable(FURNITURE, RESPONSE, digits=2, expected=T,prop.chisq=TRUE)
#-> eliminate Variable! PVALUE is 0.5

CrossTable(EDUCATION, RESPONSE, digits=2, max.width = 5, expected=FALSE, prop.r=TRUE, prop.c=TRUE, prop.t=TRUE, prop.chisq=TRUE)
CrossTable(EDUCATION, RESPONSE, digits=2, expected=T,prop.chisq=TRUE)
#keep the variable since p-value of the chi^2 test is <0.05

CrossTable(RETRAINING, RESPONSE, digits=2, max.width = 5, expected=FALSE, prop.r=TRUE, prop.c=TRUE, prop.t=TRUE, prop.chisq=TRUE)
CrossTable(RETRAINING, RESPONSE, digits=2, expected=T,prop.chisq=TRUE)
#-> eliminate Variable! PVALUE is 0.25

CrossTable(SAV_ACCT, RESPONSE, digits=2, max.width = 5, expected=FALSE, prop.r=TRUE, prop.c=TRUE, prop.t=TRUE, prop.chisq=TRUE)
CrossTable(SAV_ACCT, RESPONSE, digits=2, expected=T,prop.chisq=TRUE)
#keep the variable since p-value of the chi^2 test is <0.05

CrossTable(EMPLOYMENT, RESPONSE, digits=2, max.width = 5, expected=FALSE, prop.r=TRUE, prop.c=TRUE, prop.t=TRUE, prop.chisq=TRUE)
CrossTable(EMPLOYMENT, RESPONSE, digits=2, expected=T,prop.chisq=TRUE)
#keep the variable since p-value of the chi^2 test is <0.05

CrossTable(INSTALL_RATE, RESPONSE, digits=2, max.width = 5, expected=FALSE, prop.r=TRUE, prop.c=TRUE, prop.t=TRUE, prop.chisq=TRUE)
CrossTable(INSTALL_RATE, RESPONSE, digits=2, expected=T,prop.chisq=TRUE)
#TRTOZDEM BEHALTEN da 4 Kategorien, p-value 0.14

CrossTable(MALE_DIV, RESPONSE, digits=2, max.width = 5, expected=FALSE, prop.r=TRUE, prop.c=TRUE, prop.t=TRUE, prop.chisq=TRUE)
CrossTable(MALE_DIV, RESPONSE, digits=2, expected=T,prop.chisq=TRUE)
#trotzdem behalten, p-value 0.113

CrossTable(MALE_SINGLE, RESPONSE, digits=2, max.width = 5, expected=FALSE, prop.r=TRUE, prop.c=TRUE, prop.t=TRUE, prop.chisq=TRUE)
CrossTable(MALE_SINGLE, RESPONSE, digits=2, expected=T,prop.chisq=TRUE)
#keep the variable since p-value of the chi^2 test is <0.05

CrossTable(MALE_MAR_or_WID, RESPONSE, digits=2, max.width = 5, expected=FALSE, prop.r=TRUE, prop.c=TRUE, prop.t=TRUE, prop.chisq=TRUE)
CrossTable(MALE_MAR_or_WID, RESPONSE, digits=2, expected=T,prop.chisq=TRUE)
#-> eliminate Variable! PVALUE is 0.53

CrossTable(CO.APPLICANT, RESPONSE, digits=2, max.width = 5, expected=FALSE, prop.r=TRUE, prop.c=TRUE, prop.t=TRUE, prop.chisq=TRUE)
CrossTable(CO.APPLICANT, RESPONSE, digits=2, expected=T,prop.chisq=TRUE)
#keep anyway, but p-value is only 0.047 and yates continuity correction 0.07

CrossTable(GUARANTOR, RESPONSE, digits=2, max.width = 5, expected=FALSE, prop.r=TRUE, prop.c=TRUE, prop.t=TRUE, prop.chisq=TRUE)
CrossTable(GUARANTOR, RESPONSE, digits=2, expected=T,prop.chisq=TRUE)
#-> eliminate Variable! PVALUE is 0.2

CrossTable(PRESENT_RESIDENT, RESPONSE, digits=2, max.width = 5, expected=FALSE, prop.r=TRUE, prop.c=TRUE, prop.t=TRUE, prop.chisq=TRUE)
CrossTable(PRESENT_RESIDENT, RESPONSE, digits=2, expected=T,prop.chisq=TRUE)
#-> eliminate Variable! PVALUE is 0.86

CrossTable(REAL_ESTATE, RESPONSE, digits=2, max.width = 5, expected=FALSE, prop.r=TRUE, prop.c=TRUE, prop.t=TRUE, prop.chisq=TRUE)
CrossTable(REAL_ESTATE, RESPONSE, digits=2, expected=T,prop.chisq=TRUE)
#keep the variable since p-value of the chi^2 test is <0.05

CrossTable(PROP_UNKN_NONE, RESPONSE, digits=2, max.width = 5, expected=FALSE, prop.r=TRUE, prop.c=TRUE, prop.t=TRUE, prop.chisq=TRUE)
CrossTable(PROP_UNKN_NONE, RESPONSE, digits=2, expected=T,prop.chisq=TRUE)
#keep the variable since p-value of the chi^2 test is <0.05

CrossTable(OTHER_INSTALL, RESPONSE, digits=2, max.width = 5, expected=FALSE, prop.r=TRUE, prop.c=TRUE, prop.t=TRUE, prop.chisq=TRUE)
CrossTable(OTHER_INSTALL, RESPONSE, digits=2, expected=T,prop.chisq=TRUE)
#keep the variable since p-value of the chi^2 test is <0.05

CrossTable(RENT, RESPONSE, digits=2, max.width = 5, expected=FALSE, prop.r=TRUE, prop.c=TRUE, prop.t=TRUE, prop.chisq=TRUE)
CrossTable(RENT, RESPONSE, digits=2, expected=T,prop.chisq=TRUE)
#keep the variable since p-value of the chi^2 test is <0.05

CrossTable(OWN_RES, RESPONSE, digits=2, max.width = 5, expected=FALSE, prop.r=TRUE, prop.c=TRUE, prop.t=TRUE, prop.chisq=TRUE)
CrossTable(OWN_RES, RESPONSE, digits=2, expected=T,prop.chisq=TRUE)
#keep the variable since p-value of the chi^2 test is <0.05

CrossTable(NUM_CREDITS, RESPONSE, digits=2, max.width = 5, expected=FALSE, prop.r=TRUE, prop.c=TRUE, prop.t=TRUE, prop.chisq=TRUE) 
CrossTable(NUM_CREDITS, RESPONSE, digits=2, expected=T,prop.chisq=TRUE)
#war numerische Variable mit 4 Ausprägungen, im crosstabling aber nur p-wert von 0.445 erreicht

CrossTable(JOB, RESPONSE, digits=2, max.width = 5, expected=FALSE, prop.r=TRUE, prop.c=TRUE, prop.t=TRUE, prop.chisq=TRUE)
CrossTable(JOB, RESPONSE, digits=2, expected=T,prop.chisq=TRUE)
#p-value 0.6, keep anyway due to the fact of 4 categories

CrossTable(NUM_DEPENDENTS, RESPONSE, digits=2, max.width = 5, expected=FALSE, prop.r=TRUE, prop.c=TRUE, prop.t=TRUE, prop.chisq=TRUE)
CrossTable(NUM_DEPENDENTS, digits=2, expected=T,prop.chisq=TRUE)
#war als numerische Variable kodiert mit nur 2 Ausprägungen

CrossTable(TELEPHONE, RESPONSE, digits=2, max.width = 5, expected=FALSE, prop.r=TRUE, prop.c=TRUE, prop.t=TRUE, prop.chisq=TRUE)
CrossTable(TELEPHONE, RESPONSE, digits=2, expected=T,prop.chisq=TRUE)
#eliminate, p-value only .24

CrossTable(FOREIGN, RESPONSE, digits=2, max.width = 5, expected=FALSE, prop.r=TRUE, prop.c=TRUE, prop.t=TRUE, prop.chisq=TRUE)
CrossTable(FOREIGN, RESPONSE, digits=2, expected=T,prop.chisq=TRUE)
#keep the variable since p-value of the chi^2 test is <0.05


##2.3 Descriptives Stats of the qualitatives var
boxplot(split(DURATION,RESPONSE))


#Selection of the quantiative var. multicollinearity tests and logistic regression

#no Multicollinearity Problems 
library(psych)
gc.quanti #including only DURATION, AMOUNT, AGE since they are considered as the only real quantitative covariates
cor(gc.quanti) #correlation matrix
#We can see here that we have hardly no multicollinearity problems, the most dramatic case is 0.624, but still acceptable do use them together in the upcoming models

#inklusive der abhängigen Variable am schluss in eckigen klammern -> ändert nichts am muster!!
pairs(gc.quanti, pch = 21, labels = names(gc.quanti), bg = c("red", "green3", 
                                                               "blue")[RESPONSE])


pairs(gc.quanti, pch = 21, labels = names(gc.quanti), bg = c("red", "yellow", "blue"))
pairs.panels(gc.quanti)  #use
library(GGally)
ggpairs(gc.quanti) #use
library(corrplot)
corrplot.mixed(cor(gc.quanti), order = "hclust", tl.col = "black") #(use)
library(ggcorrplot)
ggcorrplot(cor(gc.quanti), p.mat = cor_pmat(gc.quanti), hc.order = TRUE, type = "lower")

#boxplots and histograms for the quantitative variables
 hist(DURATION)
 boxplot(split(DURATION,RESPONSE))
 hist(AMOUNT)
 boxplot(split(AMOUNT,RESPONSE))
 hist(AGE)
 boxplot(split(AGE,RESPONSE))
 
mache multiple histograms -> alle quant. var hist und box auf einmal in je einer graphik (par?)

INSTALL_RATE_quant<-as.numeric(INSTALL_RATE)
NUM_DEPENDENTS_quant<-as.numeric(NUM_DEPENDENTS)
NUM_CREDITS_quant<-as.numeric(NUM_CREDITS)
                 
##binomial logistic regression
# as we have seen above multicollinearity is not that big of a problem, so we do without concerns a binary log. reg. with all the 3 quantitative variables including, they shouldn't disturb each other too much
logreg_only_new_3_quant<-glm(RESPONSE~DURATION+AMOUNT+AGE,family=binomial(link="logit"),data=gc)
                 summary(logreg_new_3_quant)
logreg_original_6_quant<-glm(RESPONSE~DURATION+AMOUNT+AGE+INSTALL_RATE_quant+NUM_DEPENDENTS_quant+NUM_CREDITS_quant,family=binomial(link="logit"),data=gc)
                 summary(logreg_original_6_quant)
logreg_4_quant<-glm(RESPONSE~DURATION+AMOUNT+AGE+INSTALL_RATE_quant,family=binomial(link="logit"),data=gc)
                 summary(logreg_4_quant)
                 
                 logregcredits<-glm(RESPONSE~NUM_CREDITS_quant,family=binomial(link="logit"),data=gc)
                 summary(logregcredits)  #nicht signifikant
                 logregdependents<-glm(RESPONSE~NUM_DEPENDENTS_quant,family=binomial(link="logit"),data=gc)
                 summary(logregdependents) #nicht signifikant
                 logregAMOUNT<-glm(RESPONSE~AMOUNT,family=binomial(link="logit"),data=gc)
                 summary(logregAMOUNT) #hochsignifikant

 RESPONSE~.                
                 
###3 Modelling
###3.1 Classification Tree
# a) fitting the model
require(rpart)
set.seed(123456)
gc.ct<-rpart(RESPONSE~.,method="class",data=gc,cp=0.001)

gc.ct <- rpart(RESPONSE ~ CHK_ACCT+DURATION+HISTORY+NEW_CAR+USED_CAR+RADIO.TV+EDUCATION+AMOUNT+SAV_ACCT+EMPLOYMENT+INSTALL_RATE+MALE_SINGLE+CO.APPLICANT+REAL_ESTATE+PROP_UNKN_NONE+AGE+OTHER_INSTALL+RENT+OWN_RES+JOB+FOREIGN, data = gc, method = "class", control = rpart.control(minsplit = 4, 
                                                                                    cp = 1e-05))
gc.ct <- rpart(RESPONSE ~ CHK_ACCT+DURATION+HISTORY+NEW_CAR+USED_CAR+RADIO.TV, method = "class", data = gc, control = rpart.control(minsplit = 4, 
                                                                                                                                                                                                                                                                                     cp = 1e-05))
gc.ct
head(summary(gc.ct) )                             

par(pty = "s")
plot(gc.ct, cex = 1)
text(gc.ct, cex = 0.8)

#c) The Complexity Table
printcp(gc.ct)

par(pty = "s")
plotcp(gc.ct)

#d) Pruning Trees
par(pty = "s")
with(gc.ct, plot(cptable[, 5], xlab = "Tree Number", ylab = "Resubstitution Error (R)", 
                   type = "b"))
par(pty = "s")
with(gc.ct, plot(cptable[, 4], xlab = "Tree Number", ylab = "Cross-Validated Error (R(cv))", 
                   type = "b"))

par(pty = "s")
plotcp(gc.ct)
with(gc.ct, {
  lines(cptable[, 2] + 1, cptable[, 3], type = "b", col = "red")
  legend(3, 1, c("Resub. Error", "CV Error", "min(CV Error)+1SE"), lty = c(1, 
                                                                           1, 2), col = c("red", "black", "black"), bty = "n")
})

gc.prune <- prune(gc.ct, cp = 0.1)
summary(gc.prune)


require(rpart.plot)
par(pty="s")
cols<-ifelse(gc.ct$frame$yval,"yellow","darkred")
prp(gc.prune,
    main="",
    extra=104,
    nn=TRUE,
    fallen.leaves=TRUE,
    branch=0.5,
    faclen=0,
    trace=1,
    shadow.col="gray",
    branch.lty=3,
    split.cex=1.2,
    split.prefix="is ",
    split.suffix="?",
    col=cols, border.col=cols,
    split.box.col="lightgray",
    split.border.col="darkgray",
    split.round=0.5)


require("partykit")
plot(as.party(gc.prune),tp_args= list(id=FALSE))

par(pty="s")
trellis.par.set(theme=col.whitebg())
plot3<-xyplot()


#e) Predictions
library(knitr)
gc.pred <- predict(gc.prune, type = "class")
table(gc.pred, RESPONSE)

library(gmodels)
CrossTable(x = RESPONSE, y = gc.pred, prop.chisq = FALSE)




                 
summary(RESPONSE~.,data=gc)

gc.ct<-rpart(RESPONSE~.,method="class",data=gc,cp=0.001)
gc.ct<-rpart(RESPONSE~., main="",type=4,extra=4,faclen=0)
cols<-ifelse          #better graph


print(gc.ct)
summary(gc.ct)





#looking for the best parameter of complexity

subset(fit.ct.cv$results,subset=cp==fit.ct.cv$bestTune$cp)

plot(fit.ct.cv)
subset(fit.ct.recv$results,subset=cp==fit.ct.recv$bestTune$cp)

plot(fit.ct.recv)
subset(fit.ct.boot$results,subset=cp==fit.ct.boot$bestTune$cp)

plot(fit.ct.boot)
#use boottrap to because decision tree is not stable


##CART model decision tree













##3.2 Random Forest

library(rattle)
data(gc)
str(gc)
head(gc)
tail(gc)
summary(gc)

data<-na.omit(gc)
nobs<-nrow(data)
form<-formula(RESPONSE ~ CHK_ACCT+DURATION+HISTORY+NEW_CAR+USED_CAR+RADIO.TV+EDUCATION+AMOUNT+SAV_ACCT+EMPLOYMENT+INSTALL_RATE+MALE_SINGLE+CO.APPLICANT+REAL_ESTATE+PROP_UNKN_NONE+AGE+OTHER_INSTALL+RENT+OWN_RES+JOB+FOREIGN)
target<-all.vars(form)[1]
vars<- -grep('test',names(data))
set.seed(33)
train<-sample(nobs,0.7*nobs) #selecting 70% of all
train

library(randomForest)
gcRF<-randomForest(formula=form, data=data[train,vars],
                   ntree=500,mtry=4, #we try 4 variables
                   importance=TRUE, #information about which variables are important
                   localImp=TRUE,
                   na.action=na.roughfix, #imputes missing values
                   replace=FALSE)
gcRF
#the data will be split. some part is used for building the classficiation trees, the rest is used for calculationg the error rate

#d) examining the results using the print fct
print(gcRF) #confusion matrix: rows= reality, columns = predicted
#e obtaining a part of the variable importance 
head(round(importance(gcRF),2)) #most important variables? 
varImpPlot(gcRF)   #use

#f)diagnostic tool: error rate data, which is stored as the err.rate component
round(head(gcRF$err.rate,15),4)
plot(gcRF)   #use shows the change in errora rate as more trees are added to the forest. we can see that after
#around ~20trees the error keeps more or less stable at around 0.25, there is no need to add further trees than 20

#g)recording the number of trees that vote no and yes within the ensemble for a particular observation
head(gcRF$votes)
#the numbers are reported as proportions and so add up to 1 for each observation, as we can confirm
head(apply(gcRF$votes,1,sum))

#h) analyzing the confusion matrix, recording the disagreement between the final model's predictions and the
#actual outcomes of the training observations

gcRF$confusion #use  #same as print(gcRF)?
#conclusion: model is not good in prediction of a person with bad creditrating

#i)Finally analyzing the confusion matrix for the Test Dataset   
#TODO problem mit grep und dessen pattern wahrscheinlich, -> help dazu ans hauen
library(gmodels)
vars.pred=-grep('test2', names(data))
vars.pred
gc.pred=predict(gcRF,data[-train,vars.pred], type="class")
gc.pred
CrossTable(x=data$RESPONSE[-train],y=gc.pred, prop.chisq=F)







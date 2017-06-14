
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
library(magrittr)
library(nnet)   #imp.
library(NeuralNetTools)
library(neuralnet)






#find out which model (, binomial regression rain forest, neural network, vector machine etc.) is the best for our data
#response variable is whether we give a credit or not
-> recommended classification tree, it's mmuch easier to explain than f.e. blackbox neural neutworks' und for allem, predi tion accuracy ist gut! und anschaulich für kunde
-> kleinster baum nehmen, 2 äste
-> dont use too sophisticated models! oder vorsichtig damit

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
#gc.quali[,8][gc.quali[,8]== -1] <- 1 #cleaning the var. EDUCATION because there was one typing error (
#gc.quali[,17][gc.quali[,17]== 2] <- 1 #cleaning the var. GUARANTOR because there was one typing error (
summary(gc.quali) #summary for qualitative variables
gc.quanti<-gc[,quanti]
gc.quanti[,2]<-log(gc.quanti[,2])  #logarithmise the variable AMOUNT because it is heavily right-skewed
gc.quanti

#obs. 37 with "-1" for EDUCATION; "-1" represents no category for this variable
#obs. 234 with "2" for GUARANTOR; "2" represents no category for this variable
#obs. 537 with "125" for AGE, which is unplausible
gc.quanti<-gc.quanti[-c(37,234,537),] #deleting obs. 37, 234 537 due to the fact of inplausible values
gc.quali<-gc.quali[-c(37,234,537),]  
RESPONSE<-RESPONSE[-c(37)]   
RESPONSE<-RESPONSE[-c(234)]
RESPONSE<-RESPONSE[-c(537)]
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
#TO DO: I schould showing the barplots here after this adjusting

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

CrossTable(RADIO.TV, RESPONSE, digits=2, max.width = 5, expected=FALSE, prop.r=TRUE, prop.c=TRUE, prop.t=TRUE, prop.chisq=TRUE)
CrossTable(RADIO.TV, RESPONSE, digits=2, expected=T,prop.chisq=TRUE)

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


##2.2 Selection of the quantiative var. multicollinearity tests and logistic regression

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
par(mfrow=c(1,3)) 
hist(DURATION)
hist(AMOUNT)
hist(AGE)
par(mfrow=c(1,3))
boxplot(split(DURATION,RESPONSE),xlab="Credit Rating",ylab="DURATION OF CREDIT in months", notch=T,varwidth=T,col=c("red","blue"))
boxplot(split(AMOUNT,RESPONSE),xlab="Credit Rating",ylab="CREDIT AMOUNT in log(DM)", notch=T,varwidth=T,col=c("red","blue"))
boxplot(split(AGE,RESPONSE),xlab="Credit Rating",ylab="AGE in years", notch=T,varwidth=T,col=c("red","blue"))
 

INSTALL_RATE_quant<-as.numeric(INSTALL_RATE)
NUM_DEPENDENTS_quant<-as.numeric(NUM_DEPENDENTS)
NUM_CREDITS_quant<-as.numeric(NUM_CREDITS)


                 
##binomial logistic regression
# as we have seen above multicollinearity is not that big of a problem, so we do without concerns a binary log. reg. with all the 3 quantitative variables including, they shouldn't disturb each other too much
logreg_only_amount<-glm(RESPONSE~AMOUNT,family=binomial(link="logit"),data=gc)
summary(logreg_only_amount)
#AMOUNT<-exp(AMOUNT)  #for testing with original AMOUNT (not logarithmized)
logreg_only_amount<-glm(RESPONSE~exp(AMOUNT),family=binomial(link="logit"),data=gc)
summary(logreg_only_amount)
logreg_only_new_3_quant<-glm(RESPONSE~DURATION+AMOUNT+AGE,family=binomial(link="logit"),data=gc)
                 summary(logreg_only_new_3_quant)
#AMOUNT<-exp(AMOUNT)  #for testing with original AMOUNT (not logarithmized)                 
logreg_only_new_3_quant<-glm(RESPONSE~DURATION+exp(AMOUNT)+AGE,family=binomial(link="logit"),data=gc)
                 summary(logreg_only_new_3_quant)
                 
logreg_original_6_quant<-glm(RESPONSE~DURATION+AMOUNT+AGE+INSTALL_RATE_quant+NUM_DEPENDENTS_quant+NUM_CREDITS_quant,family=binomial(link="logit"),data=gc)
summary(logreg_original_6_quant)
#AMOUNT<-exp(AMOUNT)  #for testing with original AMOUNT (not logarithmized)                 
logreg_original_6_quant<-glm(RESPONSE~DURATION+exp(AMOUNT)+AGE+INSTALL_RATE_quant+NUM_DEPENDENTS_quant+NUM_CREDITS_quant,family=binomial(link="logit"),data=gc)
                 summary(logreg_original_6_quant)
logreg_4_quant<-glm(RESPONSE~DURATION+AMOUNT+AGE+INSTALL_RATE_quant,family=binomial(link="logit"),data=gc)
                 summary(logreg_4_quant)
                 
                 logregcredits<-glm(RESPONSE~NUM_CREDITS_quant,family=binomial(link="logit"),data=gc)
                 summary(logregcredits)  #nicht signifikant
                 logregdependents<-glm(RESPONSE~NUM_DEPENDENTS_quant,family=binomial(link="logit"),data=gc)
                 summary(logregdependents) #nicht signifikant
                 logregAMOUNT<-glm(RESPONSE~AMOUNT,family=binomial(link="logit"),data=gc)
                 summary(logregAMOUNT) #hochsignifikant

                 
                 
                 
                 
                 # These are the sets of variables we can compare
                 
               
                 # the selected variables at the end of the variable selection processus
                 #the selected variables are:
                 #CHK_ACCT
                 #HISTORY
                 #NEW_CAR
                 #USED_CAR
                 #RADIO_TV
                 #EDUCATION
                 #SAV_ACCT
                 #EMPLOYMENT
                 #MALE_SINGLE
                 #CO-APPLICANT
                 #REAL_ESTATE
                 #PROP_UNKN_NONE
                 #OTHER_INSTALL
                 #RENT
                 #OWN_RES
                 #FOREIGN
                 #GUARANTOR
                 #DURATION
                 #AGE
                 #RESPONSE
                 model1 <- gc[,c(2,4,5,7,8,10,11,14,16,17,19,20,21,22,23,28,29,31,32)]
                  #model2 with AMOUNT
                 model2 <- gc[,c(2,4,5,7,8,10,11,14,16,17,19,20,21,22,23,28,29,30,31,32)]
                 #model3 with HISTORY and AMOUNT
                 model3 <- gc[,c(2,3,4,5,7,8,10,11,14,16,17,19,20,21,22,23,28,29,30,31,32)]
                 # Full model
                 gc
                 #-----------------------
                 # Data Partition
                 #-----------------------
                 # We are going to compare predictions made using different models
                 # partitioning data into training and testing datasets
                 set.seed(256)
                 ind <- sample(2, nrow(gc), replace = TRUE, prob = c(0.7, 0.3))
                 # Creates the (training and testing) datasets for each model using the same partition

                 training_full <- gc[ind==1,]
                 training1 <- model1[ind==1,]
                 training2 <- model2[ind==1,]
                 training3 <- model2[ind==1,]
                 testing_full <- gc[ind==2,]
                 testing1 <- model1[ind==2,]
                 testing2 <- model2[ind==2,]
                 testing3 <- model2[ind==2,]
summary(training1)

#creating the corresponding formulas to prepare the upcoming modelling processus
gcform_model1=RESPONSE ~ CHK_ACCT+DURATION+NEW_CAR+USED_CAR+RADIO.TV+EDUCATION+SAV_ACCT+EMPLOYMENT+MALE_SINGLE+CO.APPLICANT+REAL_ESTATE+PROP_UNKN_NONE+AGE+OTHER_INSTALL+RENT+OWN_RES+FOREIGN+GUARANTOR
#gcform_model2
gcform_model2=RESPONSE ~ CHK_ACCT+DURATION+AMOUNT+NEW_CAR+USED_CAR+RADIO.TV+EDUCATION+SAV_ACCT+EMPLOYMENT+MALE_SINGLE+CO.APPLICANT+REAL_ESTATE+PROP_UNKN_NONE+AGE+OTHER_INSTALL+RENT+OWN_RES+FOREIGN+GUARANTOR
#gcform_model3 with HISTORY and AMOUNT
gcform_model3=RESPONSE ~ CHK_ACCT+HISTORY+DURATION+AMOUNT+NEW_CAR+USED_CAR+RADIO.TV+EDUCATION+SAV_ACCT+EMPLOYMENT+MALE_SINGLE+CO.APPLICANT+REAL_ESTATE+PROP_UNKN_NONE+AGE+OTHER_INSTALL+RENT+OWN_RES+FOREIGN+GUARANTOR
#full model with all variables
gcform_full=RESPONSE ~ CHK_ACCT+DURATION+HISTORY+NEW_CAR+USED_CAR+FURNITURE+RADIO.TV+EDUCATION+RETRAINING+AMOUNT+SAV_ACCT+EMPLOYMENT+INSTALL_RATE+MALE_DIV+MALE_SINGLE+MALE_MAR_or_WID+CO.APPLICANT+GUARANTOR+PRESENT_RESIDENT+REAL_ESTATE+PROP_UNKN_NONE+AGE+OTHER_INSTALL+RENT+OWN_RES+JOB+NUM_DEPENDENTS+TELEPHONE+FOREIGN

                 
###3 Modelling Processus
###3.1 Classification Tree

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



# Classification tree from R code of Workshop 2
library(knitr)
library(gmodels)
library(rpart)

set.seed(256)

#gcform_reduced=RESPONSE ~ CHK_ACCT+DURATION+HISTORY+NEW_CAR+USED_CAR+RADIO.TV+EDUCATION+AMOUNT+SAV_ACCT+EMPLOYMENT+INSTALL_RATE+MALE_SINGLE+CO.APPLICANT+REAL_ESTATE+PROP_UNKN_NONE+AGE+OTHER_INSTALL+RENT+OWN_RES+JOB+FOREIGN
#gcform_all=RESPONSE ~ CHK_ACCT+DURATION+HISTORY+NEW_CAR+USED_CAR+FURNITURE+RADIO.TV+EDUCATION+RETRAINING+AMOUNT+SAV_ACCT+EMPLOYMENT+INSTALL_RATE+MALE_DIV+MALE_SINGLE+MALE_MAR_or_WID+CO.APPLICANT+GUARANTOR+PRESENT_RESIDENT+REAL_ESTATE+PROP_UNKN_NONE+AGE+OTHER_INSTALL+RENT+OWN_RES+JOB+NUM_DEPENDENTS+TELEPHONE+FOREIGN
#gcform_all=regression formula for all variables (without obs) due to the fact that unfortunately RESPONSE~. produces errors

#training1<-training2
#testing1<-testing2
#gcform_model1<-gcform_model2
#summary(training1)

#setting cp close to 0 to get a tree with as many branches as possible
gc.ct<-rpart(formula=gcform_model1,method="class",data=training1,cp=0.001)
print(gc.ct)
summary(gc.ct)

gc.ct$variable.importance #CHK_ACCT is the most important variable, followed by DURATION
summary(gc.ct)

gc.ct<-gc %>%
  rpart(formula=gcform_model1,method="class",cp=0.001) %>%
  summary()

#par(mar=numeric(4))
par(mar=c(0.5, 1, 0.5, 1))
plot(gc.ct,uniform=TRUE)
text(gc.ct,use.n=TRUE,all=TRUE,cex=0.6)

# Plot the Resubstitution Error (rel error)  against tree size
with(gc.ct, plot(cptable[,4], xlab = "Number of Trees", ylab = "Cross-validation error", type = "b"))

options(digits=5)
printcp(gc.ct)
plotcp(gc.ct) #here we can see that x-val-relative error falls beyond the critical value of 0.93 at the 5th tree


# Using the 1-SE method to determine the cp-value
cp<-gc.ct$cptable
cp  
opt<-which.min(gc.ct$cptable[,"xerror"])
opt  
r<-cp[, 4][opt]+cp[, 5][opt]
r
rmin<-min(seq(1:dim(cp)[1])[cp[, 4] < r])
rmin
cp0<-cp[rmin,1]
cp0
cat("size chosen was",cp[rmin,2]+1,"\n")  #therefore the optimal size is 5
gc.ct1<-prune(gc.ct,cp=1.01*cp0)
summary(gc.ct1)

x<-factor(predict(gc.ct1,type="class"))
table(true=RESPONSE,predicted=x)

####falsche prediction
library(gmodels)
gc.pred<-predict(gc.ct1,type="class")
CrossTable(x = RESPONSE, y = gc.pred, prop.chisq=FALSE)
#######

plot(gc.ct1,branch=0.4,uniform=TRUE)
text(gc.ct1,digits=3,use.n=TRUE,cex=0.6)

library(rpart.plot)
rpart.plot(gc.ct1,main="")
prp(gc.ct1,main="",type=4,extra=4,faclen=0)
rpart.plot(gc.ct1,main="",extra=104,under=TRUE,faclen=0)

cols<-ifelse(gc.ct1$frame$yval,"green4","darkred")
prp(tree1_prun
    ,main="CART model tree"
    ,extra=104           # display prob and percent of obs
    ,nn=TRUE             # display the node numbers
    ,fallen.leaves=TRUE  # put the leaves on the bottom of the page
    ,branch=.5           # change angle of branch lines
    ,faclen=0            # do not abbreviate factor levels
    ,trace=1             # print the automatically calculated cex
    ,shadow.col="gray"   # shadows under the leaves
    ,branch.lty=3        # draw branches using dotted lines
    ,split.cex=1.2       # make the split text larger than the node text
    ,split.prefix="is "  # put "is " before split text
    ,split.suffix="?"    # put "?" after split text
    ,col=cols, border.col=cols   # green if survived
    ,split.box.col="lightgray"   # lightgray split boxes (default is white)
    ,split.border.col="darkgray" # darkgray border on split boxes
    ,split.round=.5)

# prediction
pred_tree1 <- predict(gc.ct1, newdata = testing1, type = "class")
# getting the same confusion matrix
tree1_cm <- table(pred_tree1, testing1$RESPONSE)
tree1_cm
# Misclassification error
error_tree1_prun <- 1 - sum(diag(tree1_cm))/sum(tree1_cm) 

round(prop.table(tree1_cm),2)
error_tree1_prun


##end of classification tree



##3.2 Random Forest
#model1<-model2
#training1<-training2
#gcform_model1<-gcform_model2

library(rattle)
str(model1)
head(model1)
tail(model1)
summary(model1)

data<-na.omit(model1)
nobs<-nrow(data)
form<-formula(gcform_model1)
#all variabless included
target<-all.vars(form)[1]
vars<-names(data)
set.seed(256)
train<-sample(nobs,0.7*nobs) #selecting 70% of all as training data set
train

library(randomForest)
gcRF<-randomForest(formula=form, data=data[train,],
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
library(gmodels)
vars.pred<-names(data)
gc.pred=predict(gcRF,data[-train,vars.pred], type="class")
gc.pred
CrossTable(x=RESPONSE[-train],y=gc.pred, prop.chisq=T)




#3.4 Neural Networks



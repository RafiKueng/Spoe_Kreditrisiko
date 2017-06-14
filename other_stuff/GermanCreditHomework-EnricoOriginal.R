#-------------------------------
# Introduction
#-------------------------------
rm(list=ls())

# Loading data
data <- read.csv2("C:/Users/Sp�rri/Desktop/Marc UniNE/SS 17 Seminar of Applied Statistics/Homework/GermanCredit.csv",dec=".",header=T)

# First things first: getting data into the right format

# Based on the data set description pdf we transform the variables into adequate formats.
# Categorical variables are defined as factors in r.
# The labels are included to make the output analysis more clear and communicative.
# Categorical variables conveying similar meanings were merged.

# Note: two variables (EDUCATION & GUARANTOR) had one case with values not in the description
# They were recoded as 1 for "Yes"
data$EDUCATION[which(data$EDUCATION == -1)] <- 1
data$GUARANTOR[which(data$GUARANTOR == 2)] <- 1

#---------------------------------------------------
# Creating new variables from merging similar variables
#---------------------------------------------------
# Merging all credit purpose variables into one categorical variable
data$USED_CAR[which(data$USED_CAR==1)] <- 2
data$FURNITURE[which(data$FURNITURE==1)] <- 3
data$RADIO.TV[which(data$RADIO.TV==1)] <- 4
data$EDUCATION[which(data$EDUCATION==1)] <- 5
data$RETRAINING[which(data$RETRAINING==1)] <- 6
# New variable credit PURPOSE
PURPOSE = cbind(data$NEW_CAR,data$USED_CAR,data$FURNITURE,data$RADIO.TV,data$EDUCATION,data$RETRAINING)
max(rowSums(PURPOSE)) # no misspecification
data$PURPOSE = apply(PURPOSE,1,max)
data$PURPOSE <- factor(data$PURPOSE, labels = c("Not known",
                                      "New car",
                                      "Used car",
                                      "Furniture",
                                      "Radio.TV",
                                      "Education",
                                      "Retraining"))

# Merging all male marital status variables into one categorical variable
data$MALE_DIV[which(data$MALE_DIV==1)] <- 1
data$MALE_SINGLE[which(data$MALE_SINGLE==1)] <- 2
data$MALE_MAR_or_WID[which(data$MALE_MAR_or_WID==1)] <- 3
# New variable MALE_STATUS
MALE_STATUS <- cbind(data$MALE_DIV,data$MALE_SINGLE,data$MALE_MAR_or_WID)
max(rowSums(MALE_STATUS)) # no misspecification
data$MALE_STATUS <- apply(MALE_STATUS,1,max)
# "Unknown" are possibly females in the dataset
data$MALE_STATUS <- factor(data$MALE_STATUS, labels = c("Unknown",
                                              "Divorced",
                                              "Single",
                                              "Mar_Wid"))

# Merging OWN_RES and RENT
data$OWN_RES[which(data$OWN_RES==1)] <- 2
data$OWN_RENT <- apply(cbind(data$RENT,data$OWN_RES),1,max)

# cleaning up environment
rm(PURPOSE,MALE_STATUS)
#------------------------------------
# Binary factors recode
#------------------------------------
data$NEW_CAR <- factor(data$NEW_CAR, labels = c("No","Yes"))
data$USED_CAR <- factor(data$USED_CAR, labels = c("No","Yes"))
data$FURNITURE <- factor(data$FURNITURE, labels = c("No","Yes"))
data$RADIO.TV <- factor(data$RADIO.TV, labels = c("No","Yes"))
data$EDUCATION <- factor(data$EDUCATION, labels = c("No","Yes"))
data$RETRAINING <- factor(data$RETRAINING, labels = c("No","Yes"))
data$MALE_DIV <- factor(data$MALE_DIV , labels = c("No","Yes"))
data$MALE_SINGLE <- factor(data$MALE_SINGLE , labels = c("No","Yes"))
data$MALE_MAR_or_WID <- factor(data$MALE_MAR_or_WID , labels = c("No","Yes"))
data$CO.APPLICANT <- factor(data$CO.APPLICANT , labels = c("No","Yes"))
data$GUARANTOR <- factor(data$GUARANTOR , labels = c("No","Yes"))
data$REAL_ESTATE <- factor(data$REAL_ESTATE , labels = c("No","Yes"))
data$PROP_UNKN_NONE <- factor(data$PROP_UNKN_NONE , labels = c("No","Yes"))
data$OTHER_INSTALL <- factor(data$OTHER_INSTALL , labels = c("No","Yes"))
data$RENT <- factor(data$RENT , labels = c("No","Yes"))
data$OWN_RES <- factor(data$OWN_RES , labels = c("No","Yes"))
data$TELEPHONE <- factor(data$TELEPHONE , labels = c("No","Yes"))
data$FOREIGN <- factor(data$FOREIGN , labels = c("No","Yes"))
# Response variable is labeled differently so analysis output is more clear and communicative
data$RESPONSE <- factor(data$RESPONSE , labels = c("Bad Credit","Good Credit"))
#------------------------------------
# NOMINAL (UNORDERED) VARIABLE
#------------------------------------
# CHK_ACCT: It is odd but category “no checking account” have a high proportion of Good Credit response
# Keep it as Nominal since one of the categories does not fits in oreder levels
data$CHK_ACCT <- factor(data$CHK_ACCT , labels = c(" < 0"," 0 < . < 200", " >= 200", " no check"))
# HISTORY: Category “critical account is ambiguous, hence we do use this variable as Nominal, not as Ordinal. 
# Additionally, this category strangely has a high percentage of Good Credit in the response variable
data$HISTORY <- factor(data$HISTORY, ordered=F)
# SAV_ACCT: It is strange that “unknown account” has the higher proportion of Good Credit
# Use it as Nominal since "unknown account" does not fit order
data$SAV_ACCT <- factor(data$SAV_ACCT, labels = c(" < 100"," 100 <= . < 500", " 500 <= . <= 1000", " >= 1000", " unknown"))
#------------------------------------
# ORDINAL (UNORDERED) VARIABLE
#------------------------------------
# EMPLOYMENT: 
data$EMPLOYMENT <- factor(data$EMPLOYMENT, ordered = TRUE, labels = c(" unemployed"," 1 y"," 1 <= . < 4 y", " 4 <= . < 7 y", " >= 7 y"))
# PRESENT_RESIDENT: 
data$PRESENT_RESIDENT <- factor(data$PRESENT_RESIDENT, ordered = TRUE, labels = c(" <= 1 y"," 1 < . <= 2 y", " 2 < . <= 3 y", "  > 4 y"))
# JOB:
data$JOB <- factor(data$JOB, ordered = TRUE, labels = c("unemplyed","unskilled", "skilled", "manager"))
#------------------------------------
# NUMERICAL
#------------------------------------
# AGE:
summary(data$AGE)
# Max AGE 125 (suspect error) corrected to 25
data$AGE[which(data$AGE==max(data$AGE))] <- 25 
# NUM_CREDITS and NUM_DEPENDENTS
#------------------------------------
# Exploratory data analysis
#------------------------------------

dim(data)
names(data) # note the the three new variables are in the final three columns

attach(data) # now we are not goin to make any other change in the data set we can attach it
# Using contingency tables, chis.test, t-test and Levene's test to assess relationship between response and predictor variable
library(gmodels)

# CHK_ACCT -> significant relation with RESPONSE
table(CHK_ACCT)
prop.table(table(CHK_ACCT, RESPONSE),2)
par(las=1)
barplot(prop.table(table(data$CHK_ACCT, RESPONSE),2), cex.names = 0.8, beside = T)
# As we can see in the barplot, among those who have Good Credit "no checking account" is much higher, what very strange
CrossTable(CHK_ACCT, RESPONSE, digits=2, expected=T)

# DURATION -> significant relation with RESPONSE
boxplot(split(DURATION,RESPONSE))
# We note that Bad credit have a bigger mean duration ...
# Also, Good credit has many outliers, meaning DURATION may not help a lot to predict this class
# QUESTION: Do having outliers in good credit will result in bad predictions for this class?
summary(DURATION) # the smaller the DURATION the higher is the chance of having Good Credit
library(car)
leveneTest(DURATION ~ RESPONSE) # null hypotheses that the population variances are equal
# So variances are not equal and we can not use a t-test.
# If we do it anyway, we have significant differences
x <- data[RESPONSE == "Good Credit",2]
y <- data[RESPONSE == "Bad Credit",2]
t.test(x,y) # the true difference in means is not equal to zero
# I would try a model with and without this variable

# HISTORY -> significant relation with RESPONSE
table(HISTORY)
prop.table(table(HISTORY, RESPONSE),2)
barplot(prop.table(table(HISTORY, RESPONSE),2), cex.names = 0.8, beside = T)
# We see that Good credit has a much higher proportion of "critical account" which is very strange
CrossTable(HISTORY, RESPONSE, digits=2, expected=T,prop.chisq=TRUE)

# Credit PURPOSE variables (merged variables into PURPOSE)
# NEW_CAR -> significant relation with RESPONSE
CrossTable(NEW_CAR, RESPONSE, digits=2, expected=T,prop.chisq=TRUE)
# USED_CAR -> significant relation with RESPONSE
CrossTable(USED_CAR, RESPONSE, digits=2, expected=T,prop.chisq=TRUE)
# FURNITURE -> NON significant relation with RESPONSE
CrossTable(FURNITURE, RESPONSE, digits=2, expected=T,prop.chisq=TRUE)
# RADIO.TV -> significant relation with RESPONSE
CrossTable(RADIO.TV, RESPONSE, digits=2, expected=T,prop.chisq=TRUE)
# EDUCATION -> significant relation with RESPONSE
CrossTable(EDUCATION, RESPONSE, digits=2, expected=T,prop.chisq=TRUE)
# RETRAINING -> NON significant relation with RESPONSE
CrossTable(RETRAINING, RESPONSE, digits=2, expected=T,prop.chisq=TRUE)

# PURPOSE -> significant relation with RESPONSE
table(PURPOSE)
prop.table(table(PURPOSE,RESPONSE),2)
barplot(prop.table(table(PURPOSE, RESPONSE),2), cex.names = 0.8, beside = T)
# Education and Retraining are the less common purposes
# Bad Credit people want a New car
# Good Credit people want Radio.Tv or also a New car
CrossTable(PURPOSE, RESPONSE, digits=2, expected=T,prop.chisq=TRUE)

# AMOUNT -> significant relation with RESPONSE ... BUT
boxplot(split(AMOUNT,RESPONSE)) # boxes overlap completely meaning the difference is probably not strong
# most people ask for small credits but we have lots of outliers
leveneTest(AMOUNT ~ RESPONSE) # null hypotheses that the population variances are equal
# not equal variance
hist(AMOUNT[RESPONSE=="Good Credit"])
hist(AMOUNT[RESPONSE=="Bad Credit"])
# See that Bad credit hist indicates they ask slightly higher AMOUNTs
x <- data[RESPONSE == "Good Credit",10]
y <- data[RESPONSE == "Bad Credit",10]
t.test(x,y) # the true difference in means is not equal to zero
# The t-test shows a difference between means, 
# HOWEVER, we have many outliers (skewness being strong on both categories) 
# and I am not sure if this can influenciate the test result
# IDEA: create some ranges and treat it as a categorical
summary(cut(data$AMOUNT, c(0,1000,2000,3000,4000,5000,6000.10000,1000000)))
CrossTable(cut(data$AMOUNT, c(0,1000,2000,3000,4000,5000,100000)), RESPONSE, digits=2, expected=T,prop.chisq=TRUE)
# And it stills significant
# As we are not sure I would make two models with and without

# SAV_ACCT -> significant relation with RESPONSE
table(SAV_ACCT)
prop.table(table(SAV_ACCT,RESPONSE),2)
barplot(prop.table(table(SAV_ACCT, RESPONSE),2), cex.names = 0.8, beside = T)
# Strangely "unknown" has a higher proportion of Good Credit
CrossTable(SAV_ACCT, RESPONSE, digits=2, expected=T,prop.chisq=TRUE)

# EMPLOYMENT -> significant relation with RESPONSE
table(EMPLOYMENT)
prop.table(table(EMPLOYMENT, RESPONSE),2)
barplot(prop.table(table(EMPLOYMENT, RESPONSE),2), cex.names = 0.8, beside = T)
# As expected, Good Credit are people working for longer periods
CrossTable(EMPLOYMENT, RESPONSE, digits=2, expected=T,prop.chisq=TRUE)

# INSTALL_RATE -> NON significant relation with RESPONSE
boxplot(split(INSTALL_RATE,RESPONSE)) # VERY ODD!
# Is it CATEGORICAL? ... What are the meaning of the four categories? ...let us treat it as categorical
table(INSTALL_RATE) # it seems this is actually a categorical variable and not a numerical one
prop.table(table(INSTALL_RATE,RESPONSE),2)
barplot(prop.table(table(INSTALL_RATE, RESPONSE),2), cex.names = 0.8, beside = T)
CrossTable(INSTALL_RATE, RESPONSE, digits=2, expected=T,prop.chisq=TRUE)

# MALE_DIV -> NON significant relation with RESPONSE
CrossTable(MALE_DIV, RESPONSE, digits=2, expected=T,prop.chisq=TRUE)
# MALE_SINGLE -> significant relation with RESPONSE
CrossTable(MALE_SINGLE, RESPONSE, digits=2, expected=T,prop.chisq=TRUE)
# MALE_MAR_or_WID -> NON significant relation with RESPONSE
CrossTable(MALE_MAR_or_WID, RESPONSE, digits=2, expected=T,prop.chisq=TRUE)

# MALE_STATUS -> significant relation with RESPONSE
table(MALE_STATUS)
prop.table(table(MALE_STATUS,RESPONSE),2)
barplot(prop.table(table(MALE_STATUS, RESPONSE),2), cex.names = 0.8, beside = T)
# Good Credit has more single men than Bad credit
CrossTable(MALE_STATUS, RESPONSE, digits=2, expected=T,prop.chisq=TRUE)

# CO.APPLICANT -> would use in the model since much higher among Good Credit people
table(CO.APPLICANT)
prop.table(table(CO.APPLICANT,RESPONSE),2)
barplot(prop.table(table(CO.APPLICANT, RESPONSE),2), cex.names = 0.8, beside = T)
# Significant (at 95%) ONLY without Yates' continuity correction
# But still significant at 90%
CrossTable(CO.APPLICANT, RESPONSE, digits=2, expected=T,prop.chisq=TRUE)

# GUARANTOR (!) NON SIGNIFICANT
table(GUARANTOR)
prop.table(table(GUARANTOR,RESPONSE),2)
barplot(prop.table(table(GUARANTOR, RESPONSE),2), cex.names = 0.8, beside = T)
# but since p-value with Yates' correction is higher than 10% I would not keep it in the model
CrossTable(GUARANTOR, RESPONSE, digits=2, expected=T,prop.chisq=TRUE)

# PRESENT_RESIDENT -> NON significant relation with RESPONSE
table(PRESENT_RESIDENT)
prop.table(table(PRESENT_RESIDENT,RESPONSE),2)
barplot(prop.table(table(PRESENT_RESIDENT, RESPONSE),2), cex.names = 0.8, beside = T)
# We see similar patterns
CrossTable(PRESENT_RESIDENT, RESPONSE, digits=2, expected=T,prop.chisq=TRUE)

# REAL_ESTATE -> significant relation with RESPONSE
table(REAL_ESTATE)
prop.table(table(REAL_ESTATE,RESPONSE),2)
barplot(prop.table(table(REAL_ESTATE, RESPONSE),2), cex.names = 0.8, beside = T)
CrossTable(REAL_ESTATE, RESPONSE, digits=2, expected=T,prop.chisq=TRUE)

# PROP_UNKN_NONE -> significant relation with RESPONSE
table(PROP_UNKN_NONE)
prop.table(table(PROP_UNKN_NONE,RESPONSE),2)
barplot(prop.table(table(PROP_UNKN_NONE, RESPONSE),2), cex.names = 0.8, beside = T)
CrossTable(PROP_UNKN_NONE, RESPONSE, digits=2, expected=T,prop.chisq=TRUE)

# AGE -> significant relation with RESPONSE 
# using the t-test there is a difference between means for AGE
boxplot(split(AGE,RESPONSE)) # boxes overlap almost entirely meaning difference is probably not strong
leveneTest(AGE ~ RESPONSE) # null hypotheses that the population variances are equal
# equal variance
hist(AGE[RESPONSE=="Good Credit"])
# Good credit are slightly older
hist(AGE[RESPONSE=="Bad Credit"])
names(data)
x <- data[RESPONSE == "Good Credit",22]
y <- data[RESPONSE == "Bad Credit",22]
t.test(x,y) # the true difference in means is not equal to zero

# OTHER_INSTALL -> significant relation with RESPONSE
table(OTHER_INSTALL)
prop.table(table(OTHER_INSTALL,RESPONSE),2)
barplot(prop.table(table(OTHER_INSTALL, RESPONSE),2), cex.names = 0.8, beside = T)
CrossTable(OTHER_INSTALL, RESPONSE, digits=2, expected=T,prop.chisq=TRUE)

# RENT -> significant relation with RESPONSE
table(RENT)
prop.table(table(RENT,RESPONSE),2)
barplot(prop.table(table(RENT, RESPONSE),2), cex.names = 0.8, beside = T)
CrossTable(RENT, RESPONSE, digits=2, expected=T,prop.chisq=TRUE)

# OWN_RES -> significant relation with RESPONSE
table(OWN_RES)
prop.table(table(OWN_RES,RESPONSE),2)
barplot(prop.table(table(OWN_RES, RESPONSE),2), cex.names = 0.8, beside = T)
CrossTable(OWN_RES, RESPONSE, digits=2, expected=T,prop.chisq=TRUE)

# OWN_RENT (merged) -> significant relation with RESPONSE
table(OWN_RENT)
prop.table(table(OWN_RENT,RESPONSE),2)
barplot(prop.table(table(OWN_RENT, RESPONSE),2), cex.names = 0.8, beside = T)
CrossTable(OWN_RENT, RESPONSE, digits=2, expected=T,prop.chisq=TRUE) # significant relation with RESPONSE

# NUM_CREDITS -> NON significant relation with RESPONSE
summary(NUM_CREDITS)
# it is a discrete variable
table(NUM_CREDITS)
prop.table(table(NUM_CREDITS,RESPONSE),2)
barplot(prop.table(table(NUM_CREDITS, RESPONSE),2), cex.names = 0.8, beside = T)
# use a contingency table
CrossTable(NUM_CREDITS, RESPONSE, digits=2, expected=T,prop.chisq=TRUE)
# there are cells with less than 5 observation meaning chis.test may have errors

# JOB -> NON significant relation with RESPONSE
table(JOB)
prop.table(table(JOB,RESPONSE),2)
barplot(prop.table(table(JOB, RESPONSE),2), cex.names = 0.8, beside = T)
CrossTable(JOB, RESPONSE, digits=2, expected=T,prop.chisq=TRUE)

# NUM_DEPENDENTS -> NON significant relation with RESPONSE
summary(NUM_DEPENDENTS)
table(NUM_DEPENDENTS) # it is a discrete variable
prop.table(table(NUM_DEPENDENTS,RESPONSE),2)
barplot(prop.table(table(NUM_DEPENDENTS, RESPONSE),2), cex.names = 0.8, beside = T)
CrossTable(NUM_DEPENDENTS, RESPONSE, digits=2, expected=T,prop.chisq=TRUE)

# TELEPHONE -> NON significant relation with RESPONSE
table(TELEPHONE)
prop.table(table(TELEPHONE,RESPONSE),2)
barplot(prop.table(table(TELEPHONE, RESPONSE),2), cex.names = 0.8, beside = T)
CrossTable(TELEPHONE, RESPONSE, digits=2, expected=T,prop.chisq=TRUE)

# FOREIGN -> significant relation with RESPONSE
table(FOREIGN)
prop.table(table(FOREIGN,RESPONSE),2)
barplot(prop.table(table(FOREIGN, RESPONSE),2), cex.names = 0.8, beside = T)
CrossTable(FOREIGN, RESPONSE, digits=2, expected=T,prop.chisq=TRUE)

detach(data)

#-----------------------
# Variable selections
#-----------------------
# These are the sets of variables we can compare

# Full model
full <- data[,c(1,2,3,10,11,12,13,17,18,19,20,21,22,23,26,27,28,29,30,31,32,33,34)]
# first selection based on cross analysis with RESPONSE
model1 <- data[,c(1,2,3,10,11,12,17,20,21,22,23,30,31,32,33,34)]
# Excludes DURATION and AMOUNT, since both variables have outliers that may influence predictions
model2 <- data[,c(1,3,11,12,17,20,21,22,23,30,31,32,33,34)]
# Using the full model to construct the logistic regression
mylogit_full <- glm(RESPONSE ~ ., data = full, family = "binomial")
mylogit_full$aic
summary(mylogit_full)
# Select only variable which are significant at 0.05
model3 <- data[,c(1,2,3,10,11,12,13,18,19,23,30,31,32,33)]

# Here one can inspect the names of the variables in each model
names(data)
names(full)
names(model1)
names(model2)
names(model3)

#-----------------------
# Data Partition
#-----------------------
# We are going to compare predictions made using different models
# logistic regression; classification trees; random forests; neural network
# for that we partitioned data into training and testing datasets
set.seed(1234)
ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
# Creates the (training and testing) datasets for each model using the same partition
training <- data[ind==1,]
training_full <- full[ind==1,]
training1 <- model1[ind==1,]
training2 <- model2[ind==1,]
training3 <- model3[ind==1,]
testing <- data[ind==2,]
testing_full <- full[ind==2,]
testing1 <- model1[ind==2,]
testing2 <- model2[ind==2,]
testing3 <- model3[ind==2,]
#-----------------------
# Logistic Model
#-----------------------

# Now we construct a logistic regression for each model (set of variables) using just training data
# We compare which one has the beteer AIC
# After we predict using testing data, and compare the missclassification errors
# Finally, we select the model with smaller error which will be lately compared with the other classification models

mylogit <- glm(RESPONSE ~ ., data = training_full, family = "binomial")
mylogit1 <- glm(RESPONSE ~ ., data = training1, family = "binomial")
mylogit2 <- glm(RESPONSE ~ ., data = training2, family = "binomial")
mylogit3 <- glm(RESPONSE ~ ., data = training3, family = "binomial")
summary(mylogit)
summary(mylogit1)
summary(mylogit2)
summary(mylogit3)
mylogit$aic
mylogit1$aic
mylogit2$aic
mylogit3$aic
# Predictions
# Model FULL
logit_pred <- predict(mylogit, newdata = testing_full, type = "response")
logit_model_pred <- rep("0", length(testing_full$RESPONSE))
logit_model_pred[logit_pred > 0.5] <- "1"
logit_cm <- table(logit_model_pred, testing_full$RESPONSE)
# Misclassification error
logit_error <- 1 - sum(diag(logit_cm))/sum(logit_cm)

# Model 1
logit_pred1 <- predict(mylogit1, newdata = testing1, type = "response")
# Confusion matrix
logit_model_pred1 <- rep("0", length(testing1$RESPONSE))
logit_model_pred1[logit_pred1 > 0.5] <- "1"
logit_cm1 <- table(logit_model_pred1, testing1$RESPONSE)
# Misclassification error
logit_error1 <- 1 - sum(diag(logit_cm1))/sum(logit_cm1)

# Model 2
logit_pred2 <- predict(mylogit2, newdata = testing2,type = "response")
# Confusion matrix
logit_model_pred2 <- rep("0", length(testing2$RESPONSE))
logit_model_pred2[logit_pred2 > 0.5] <- "1"
logit_cm2 <- table(logit_model_pred2, testing2$RESPONSE)
# Misclassification error
logit_error2 <- 1 - sum(diag(logit_cm2))/sum(logit_cm2)

# Model 3
logit_pred3 <- predict(mylogit3, newdata = testing3,type = "response")
# Confusion matrix
logit_model_pred3 <- rep("0", length(testing3$RESPONSE))
logit_model_pred3[logit_pred3 > 0.5] <- "1"
logit_cm3 <- table(logit_model_pred3, testing3$RESPONSE)
# Misclassification error
logit_error3 <- 1 - sum(diag(logit_cm3))/sum(logit_cm3)

# Comparing errors
logit_error
logit_error1
logit_error2
logit_error3
# Best model is the third one
# Nevertheless, error is too high 26.1%
# and we do not do so well in classifying Bad credit

# Comparing all confusion matrices
round(prop.table(logit_cm),2)
round(prop.table(logit_cm1),2)
round(prop.table(logit_cm2),2)
round(prop.table(logit_cm3),2)
# The error of classifying someone with Bad credit as Good credit is higher than
# the error of classifying someone with Good credit as Bad credit

# More over it seems we do not have a big difference between models 1 and 2
# Hence, we will keep using only models 1 and 3 for comparisons

#------------------------------------
# Classification Trees
#------------------------------------

library(rpart)
library(rpart.plot)

# First use a negative number as cp, so r will grow the biggest possible tree
set.seed(1234)
tree1 <- rpart(RESPONSE ~ ., data = training1, method = "class", cp = -1)
# This is too big to plot
# rpart.plot(tree1)
tree1$variable.importance
summary(tree1) # note that CHK_ACCT has an improvement much higher than the other primary splits

# This tree has a large number number of nodes and a "relatively" good resubstitution error
# However it is not true that the tree wit the lowest resubstitution error provide the best classification
# On the opposite, a large classification tree would leed to a poor classification as it will probably be biased
# There are different approches to decide how to prune the tree

# When rpart grows a tree it performs 10-fold cross validation on the data
# The cross validation error rates and standard deviations are displayed in the columns xerror and xstd respectively
printcp(tree1)
# The rel error of each iteration of the tree is the fraction of mislabeled elements 
# in the iteration relative to the fraction of mislabeled elements in the root.

# Plot the RESUBSTITUTION ERROR (rel error)  against tree size
# this figure represent a measure of error , however it is not the best criteria 
with(tree1, plot(cptable[,3], xlab = "Tree Number", ylab = "Resubstitution Error (R)", type = "b"))
# As previously explained, we should not base our decision on the resubstitution error
with(tree1, plot(cptable[,4], xlab = "Tree Number", ylab = "Cross-validated error (R(cv))", type = "b"))
# From this graph we get the smallest xerror is in the fourth tree
# We will decide how to prune our classification tree with the 10-fold cross validation criteria. 
# This measure better help in understanding the prediction error

# Plot resubstitution error and the cross validated error rate all together
plotcp(tree1, ylim=c(0.8, 1.1))
lines(tree1$cptable[,3], type = "b", col = "red") 
legend("bottomright", c("Resub. Erorr", "CV Error", "min (CV Error)+1SE"), lty = c(1,1,2), col = c("red","black", "black"), 
       bty = "n", cex = 0.6)
# From the graph we see that we could prune using a cp in between 0.016 to 0.028
# so we could use the same cp we have used before
# So we select the third three
tree1_prun <- prune(tree1,cp=0.021)
rpart.plot(tree1_prun)

# prediction
pred_tree1 <- predict(tree1_prun, newdata = testing1, type = "class")
# getting the same confusion matrix
tree1_cm <- table(pred_tree1, testing1$RESPONSE)
# Misclassification error
error_tree1_prun <- 1 - sum(diag(tree1_cm))/sum(tree1_cm) 

round(prop.table(tree1_cm),2)
error_tree1_prun

# PRUNING (alternatively)
# As a rule of thumb, it’s best to prune a decision tree using the cp of smallest tree 
# that is within one standard deviation of the tree with the smallest xerror.
# Here, the best xerror is 0.95833 with standard deviation 0.060626. 
printcp(tree1)
0.9218750 + 0.059865492
# = 0.981740492
# So, we want the smallest tree with xerror less than 0.981740492 
# This is the tree with cp = 0.0208333333, so we’ll want to prune our tree with a cp slightly 
# greater than than 0.021.

# The goal of our classification is not only to the one of minimazing the missclassification error
# We aim also in minimazing the risk of classifying an applicant as "good creditor" when in fact 
# it is not the case
# In order to reach this purpose we are ready to decrease the "Good creditor" mispclassification 
# error at the expense of the Bad credit one

lossmatrix <- matrix(c(0,2,1,0), byrow=TRUE, nrow=2)
set.seed(1234)
loss_tree1 <- rpart(RESPONSE ~ ., data = training1, parms = list(loss = lossmatrix), cp=-1)
# Pruning loss_tree
plotcp(loss_tree1, ylim=c(0.3, 1.4))
lines(loss_tree1$cptable[,3], type = "b", col = "red") 
legend("topright", c("Resub. Erorr", "CV Error", "min (CV Error)+1SE"), lty = c(1,1,2), col = c("red","black", "black"), 
       bty = "n", cex = 0.6)
# we get strange graph
# not sure how to deal with that
# I will just ignore first xerror
printcp(loss_tree1)
0.80208333 + 0.051565899
# 0.853649229
# we just the sixth three
loss_tree1 <- prune(loss_tree1, cp = 0.014)
rpart.plot(loss_tree1)
# For comparison
rpart.plot(tree1_prun) # and we get less people classified as Good credit

# prediction
pred_loss_tree1 <- predict(loss_tree1, newdata = testing1, type = "class")
loss_tree1_cm <- table(pred_loss_tree1, testing1$RESPONSE)
# Misclassification error
error_loss_tree1_prun <- 1 - sum(diag(loss_tree1_cm))/sum(loss_tree1_cm) 

# Using model 3
set.seed(1234)
tree3 <- rpart(RESPONSE ~ ., data = training3, method = "class", cp = -1)
plotcp(tree3, ylim=c(0.8, 1.1))
lines(tree3$cptable[,3], type = "b", col = "red") 
legend("bottomright", c("Resub. Erorr", "CV Error", "min (CV Error)+1SE"), lty = c(1,1,2), col = c("red","black", "black"), 
       bty = "n", cex = 0.6)
printcp(tree3)
0.91145833 + 0.059640452
# so we select the second tree
tree3_prun <- prune(tree3, cp = 0.037)
rpart.plot(tree3_prun)
pred_tree3_prun <- predict(tree3_prun, newdata = testing3, type = "class")
tree3_cm <- table(pred_tree3_prun, testing3$RESPONSE)
error_tree3_prun <- 1 - sum(diag(tree3_cm))/sum(tree3_cm) 

# Loss matrix in model 3
lossmatrix <- matrix(c(0,2,1,0), byrow=TRUE, nrow=2)
set.seed(1234)
loss_tree3 <- rpart(RESPONSE ~ ., data = training3, parms = list(loss = lossmatrix), cp=-1)
# Pruning loss_tree
plotcp(loss_tree3, ylim=c(0.3, 1.4))
lines(loss_tree3$cptable[,3], type = "b", col = "red") 
legend("topright", c("Resub. Erorr", "CV Error", "min (CV Error)+1SE"), lty = c(1,1,2), col = c("red","black", "black"), 
       bty = "n", cex = 0.6)
# we get strange graph
# not sure how to deal with that
# I will just ignore first xerror
printcp(loss_tree3)
0.83854167 + 0.052178012
# 0.890719682
# we just the fourth three
loss_tree3 <- prune(loss_tree3, cp = 0.019)
rpart.plot(loss_tree3)
# For comparison
rpart.plot(tree3_prun) # and we get less people classified as Good credit

# prediction
pred_loss_tree3 <- predict(loss_tree3, newdata = testing3, type = "class")
loss_tree3_cm <- table(pred_loss_tree3, testing3$RESPONSE)
# Misclassification error
error_loss_tree3_prun <- 1 - sum(diag(loss_tree3_cm))/sum(loss_tree3_cm) 

# Comparing both models
round(prop.table(tree1_cm),2)
round(prop.table(loss_tree1_cm),2)
round(prop.table(tree3_cm),2)
round(prop.table(loss_tree3_cm),2)
# And we clearly see that we have decreased the the misclassification error
# bad credit
error_tree1_prun
error_loss_tree1_prun
error_tree3_prun
error_loss_tree3_prun
# strangely we get the same errors for the trees with the loss matrix

#------------------------------------
# Random Forest
#------------------------------------

# In order to cope with the problem of instability of the classification tree and to try to 
# improve the bad habit of the classfication tree of overfitting the training data we use also the Random forest

nobs <- nrow(training1)
form <- formula((training1$RESPONSE)~.) # response variable
target <- all.vars(form)[1] # we select our response variable

library(randomForest)

# ntree=500: number of trees
# number of tree: the more the better. 500 is considered as sufficient
set.seed(1234)
RF1 <- randomForest(formula=form, data=training1, ntree=500, mtry=3,
                importance=T, localImp=T, na.action=na.roughfix, replace=F)

# we examinate the result. It is the confusion Matrix whcih explain the disagreement btw the 
#final's model prediction and the actual outcome of the training data
print(RF1)

# obtain importance variables
head(round(importance(RF1),2))

# nice graphical visualization of the variables importance with
# both the MeanDecreaseAccuracy and MeanDecreaseGini
varImpPlot(RF1)
# for MeanDecreaseAccuracy CHK_ACCT is by far the most important variable
# for MeanDecreaseGini CHK_ACCT and DURATION are the most relevants

# Error rate
# 00B: overall classification rate
# BAD credit:Error in classifying bad creditors
# good: erro in classifying good creditors -- this is the one we mainly care about
round(head(RF1$err.rate,15),4)

# error plot --> change in error rate as more trees are added to the forest
# Red:bad credit  Green:good credit    Black:overall misspecification
plot(RF1)

# it records the number of trees that vote NO and YES for a particula observation
head(RF1$votes)
# it sums up to 1 (by row)
head(apply(RF1$votes,1,sum))

# Prediction
predRF1 <- predict(RF1,testing1, type="class")
RF1_cm <- table(predRF1, testing1$RESPONSE)
error_RF1 <- 1-sum(diag(RF1_cm))/sum(RF1_cm) # misclassification error

round(prop.table(RF1_cm),2)
# in the training we do really bad cause we have 23% of Bad credit classified as Good
error_RF1

# with the random forest we obtain superior results:
# over come problem overfitting. If training datset is 
# limited or if it is not good representation our model would be overfitted and performance could be poor
# it is a good algorithm to solve bagging and random selection problems that we might experience 
# in classification trees

# the result is quite satisfactory: >10% mispecification for a good creditor!
prop.table(RF1$confusion[,1:2])
# but not in the training data

nobs <- nrow(training3)
form <- formula((training3$RESPONSE)~.) # response variable
target <- all.vars(form)[1] # we select our response variable

set.seed(1234)
RF3 <- randomForest(formula=form, data=training3, ntree=500, mtry=3,
                    importance=T, localImp=T, na.action=na.roughfix, replace=F)

print(RF3)
varImpPlot(RF3)
plot(RF1)
# Prediction
predRF3 <- predict(RF3,testing3, type="class")
RF3_cm <- table(predRF3, testing3$RESPONSE)
error_RF3 <- 1-sum(diag(RF3_cm))/sum(RF3_cm) # misclassification error

#-----------------------------------------------------------------
# cut off random forest
#----------------------------------------------------------------
set.seed(1234)
RF1 <- randomForest(formula=form, data=training1, ntree=500, mtry=3,
                    importance=T, localImp=T, na.action=na.roughfix, replace=F)

predRF1 <- predict(RF1,testing1, type="class")
RF1_cm <- table(predRF1, testing1$RESPONSE)
error_RF1 <- 1-sum(diag(RF1_cm))/sum(RF1_cm) # misclassification error

set.seed(1234)
RF1_loss <- randomForest(formula=form, data=training1, ntree=500, mtry=3,
                         importance=T, localImp=T, na.action=na.roughfix, replace=F, cutoff=c(.3,.7))

predRF1_loss <- predict(RF1_loss,testing1, type="class")
RF1_cm_loss <- table(predRF1_loss, testing1$RESPONSE)
error_RF1_loss <- 1-sum(diag(RF1_cm_loss))/sum(RF1_cm_loss) # misclassification error

# Comparing
prop.table(RF1_cm_loss)
round(prop.table(RF1_cm,1),1)
round(prop.table(RF1_cm_loss,1),2)
round(prop.table(RF1_cm,1),2)

error_RF1
error_RF3

#------------------------------------
# Neural Networs
#------------------------------------

require(nnet)

neur1 <- nnet(RESPONSE ~ ., data = training1, size = 2, rang = 0.1, decay = 5e-04, 
     maxit = 200)

plot(tst)

library(NeuralNetTools)
par(mar = numeric(4), family = "serif")
plotnet(neur1, pos_col = "darkgreen", neg_col = "darkblue")

table(true = testing1$RESPONSE, predicted = predict(tst, testing1, type = "class"))
table(predicted = predict(tst, testing1, type = "class"), true = testing1$RESPONSE)
table(true = iris.test$Species, predicted = predict(iris.net, iris.test, type = "class"))


# ################################
# Preparing data
data <- read.csv("C:/Users/Sp�rri/Desktop/Marc UniNE/SS 17 Seminar of Applied Statistics/Homework/GermanCredit.csv") 
# Need to have data in the range from 0 to 1
# For all binary variables this is already done
# No need to convert variables to factor/categorical
# Min-Max Normalization
# For the numerical variables
data$DURATION <- (data$DURATION - min(data$DURATION))/(max(data$DURATION) - min(data$DURATION))
data$AMOUNT <- (data$AMOUNT - min(data$AMOUNT))/(max(data$AMOUNT) - min(data$AMOUNT))
data$INSTALL_RATE <- (data$INSTALL_RATE - min(data$INSTALL_RATE))/(max(data$INSTALL_RATE) - min(data$INSTALL_RATE))
data$AGE <- (data$AGE - min(data$AGE))/(max(data$AGE) - min(data$AGE))
data$NUM_DEPENDENTS <- (data$NUM_DEPENDENTS - min(data$NUM_DEPENDENTS))/(max(data$NUM_DEPENDENTS) - min(data$NUM_DEPENDENTS))
# For the non binary categorical variables
data$CHK_ACCT <- (data$CHK_ACCT - min(data$CHK_ACCT))/(max(data$CHK_ACCT) - min(data$CHK_ACCT))
data$HISTORY <- (data$HISTORY - min(data$HISTORY))/(max(data$HISTORY) - min(data$HISTORY))
data$SAV_ACCT <- (data$SAV_ACCT - min(data$SAV_ACCT))/(max(data$SAV_ACCT) - min(data$SAV_ACCT))
data$EMPLOYMENT <- (data$EMPLOYMENT - min(data$EMPLOYMENT))/(max(data$EMPLOYMENT) - min(data$EMPLOYMENT))
data$PRESENT_RESIDENT <- (data$PRESENT_RESIDENT - min(data$PRESENT_RESIDENT))/(max(data$PRESENT_RESIDENT) - min(data$PRESENT_RESIDENT))
data$JOB <- (data$JOB - min(data$JOB))/(max(data$JOB) - min(data$JOB))

str(data)

# Neural Networks
# First I will just run a model with the variables seen as important when doing the classification tree
library(neuralnet)
set.seed(1234)
n <- neuralnet(RESPONSE ~ CHK_ACCT + SAV_ACCT + DURATION,
               data = data,
               hidden = 1,
               err.fct = "ce",
               linear.output = FALSE)
plot(n) # We get a completely connected neural network

# Prediction
output <- compute(n, data[,c("CHK_ACCT","SAV_ACCT","DURATION")])
head(output$net.result)
head(data[,31])

head(data[1,])
# How the first value is calculated?
in4 <- -1.1152 + (0.333333333*-0.52187) + (0*-0.33767) + (0.6470588235*0.81918) # input to node 4
out4 <- 1/(1+exp(-in4)) # sigmoid function
in5 <- 5.15315 + (out4*-18.74068)
out5 <- 1/(1+exp(-in5)) # get first value for net.result
data$RESPONSE[1]

# Confusion Matrix & Misclassification Error - training data
output <- compute(n, data[,c("CHK_ACCT","SAV_ACCT","DURATION")])
p1 <- output$net.result
pred1 <- ifelse(p1>0.5, 1, 0)
tab1 <- table(pred1, data$RESPONSE)
tab1 # off diagonals are the misclassifications
sum(diag(tab1))/sum(tab1) # accuracy
1-sum(diag(tab1))/sum(tab1) # misclassification error

# Confusion Matrix & Misclassification Error - testing data
output <- compute(n, testing[,c("CHK_ACCT","SAV_ACCT","DURATION")])
p2 <- output$net.result
pred2 <- ifelse(p2>0.5, 1, 0)
tab2 <- table(pred2, testing$RESPONSE)
tab2 # off diagonals are the misclassifications
sum(diag(tab2))/sum(tab2) # accuracy
1-sum(diag(tab2))/sum(tab2) # misclassification error

# More neurons in one hiden layer
set.seed(333)
n <- neuralnet(RESPONSE ~  CHK_ACCT+SAV_ACCT+DURATION,
               data = training,
               hidden = 5,
               err.fct = "ce",
               linear.output = FALSE,
               lifesign = "full",
               rep = 5)
plot(n, rep = 2) # 2 repetition has the smaller error among all networks

# More neurons and more hiden layers
set.seed(333)
n <- neuralnet(RESPONSE ~  CHK_ACCT+SAV_ACCT+DURATION,
               data = training,
               hidden = c(3,5),
               err.fct = "ce",
               linear.output = FALSE)
# it is not sure that adding more layers will improve your model
plot(n)

# Neural network advantages
# Robust to noisy data
# Neural networks disadvantages
# Less interpretable than other models as classification trees
# longer training times

#------------------------------------
# Support Vector Machine
#------------------------------------

# Preparing data
data <- read.csv("/home/steiner/Downloads/Stats Seminar/Homework/GermanCredit.csv")

# First we can use the data as in classification tree BUT not recommended
library(e1071)
mymodel <- svm(RESPONSE ~ ., data = data)
summary(mymodel) # get 608 support vectors
# it uses radial kernel function (best one for doing classification),
# could use linear or polynomial or sigmoid functions but tends to give higher misclassification errors depending on your data
# for plotting need to select just two dimensions
plot(mymodel,data=data,DURATION~AMOUNT) # crosses represent the support vectors
# in our case plotting is not very useful

# Confusion matrix and misclassification error
pred <- predict(mymodel, data)
tab <- table(Prediction = pred, Actual = data$RESPONSE)
# add diagonal and divide by total to get accuracy
sum(tab[1,1],tab[2,2])/1000
# sum off diagonals to get misclassification error
sum(tab[1,2],tab[2,1])/1000 # not bad!
1 - sum(tab[1,1],tab[2,2])/1000
1 - sum(diag(tab))/sum(tab)

# Tuning: choosing our best model
tmodel <- tune(svm,RESPONSE~., data=data,
     ranges = list(epsilon = seq(0,1,0.4), cost = 2^(2:4))) # if cost too high we get overfitting, if too low we get underfitting and unaccuracy
# take care if data set too big and epsilon*cost too many it can take a long time to find the optimal model
plot(tmodel)
summary(tmodel) # look at best performance parameters (10-fold cross validation used)

mymodel <- tmodel$best.model
summary(mymodel)
plot(mymodel,data=data,DURATION~AMOUNT)

# Numerical data: also need to be rescaled or normalized
# Doubt: scale or normalize???
normalize <- (data$DURATION - min(data$DURATION))/(max(data$DURATION) - min(data$DURATION))
scaling <- scale(data$DURATION) 
summary(normalize)
summary(scaling)

# Min-Max normalization
data$DURATION <- (data$DURATION - min(data$DURATION))/(max(data$DURATION) - min(data$DURATION))
data$AMOUNT <- (data$AMOUNT - min(data$AMOUNT))/(max(data$AMOUNT) - min(data$AMOUNT))
data$INSTALL_RATE <- (data$INSTALL_RATE - min(data$INSTALL_RATE))/(max(data$INSTALL_RATE) - min(data$INSTALL_RATE))
data$AGE <- (data$AGE - min(data$AGE))/(max(data$AGE) - min(data$AGE))
data$NUM_DEPENDENTS <- (data$NUM_DEPENDENTS - min(data$NUM_DEPENDENTS))/(max(data$NUM_DEPENDENTS) - min(data$NUM_DEPENDENTS))

# For the non binary categorical variables
# Need to recode them as binary numerical. For instance, a variable with 3 categories will be recoded as three binary variables

# https://www.youtube.com/watch?v=EOhAKMwburY
# https://www.youtube.com/watch?v=1RecjImtImY
# https://www.youtube.com/watch?v=I4z3yjoEADY
# https://www.youtube.com/watch?v=aS1O8EiGLdg

# https://www.youtube.com/watch?v=xrAg3FLQ0ZI
# https://www.youtube.com/watch?v=fDjKa7yWk1U

# https://www.youtube.com/watch?v=tU3Adlru1Ng
# https://www.youtube.com/watch?v=dJclNIN-TPo
# http://statistics.berkeley.edu/computing/r-t-tests

# https://www.youtube.com/watch?v=-Vs9Vae2KI0 

# https://www.youtube.com/watch?v=pS5gXENd3a4
# https://www.youtube.com/watch?v=pS5gXENd3a4
# https://stats.stackexchange.com/questions/82923/mixing-continuous-and-binary-data-with-linear-svm
# https://www.quora.com/How-can-l-apply-an-SVM-for-categorical-data

#Fall 2023
#Homework 8; Lab 7

#Question 01: Md Muhibul Islam; Mohammed A. Al Muhaymin; Zakaria Sule

#Question 2:

library(plyr)
library(dplyr)
library(tidyverse)
library(haven)
attach(acs2021)
summary(acs2021)

getwd()
levels_n <- read.csv("IND_levels.csv")
names(levels_n) <- c("New_Level","levels_orig")
acs2021$IND <- as.factor(acs2021$IND)
levels_orig <- levels(acs2021$IND) 
levels_new <- join(data.frame(levels_orig),data.frame(levels_n))

acs2021$public_work <- acs2021$IND 
levels_public <- read.csv("publicwork_recode.csv")
names(levels_public) <- c("levels_orig","New_Level")
levels_new_pub <- join(data.frame(levels_orig),data.frame(levels_public))


levels(acs2021$IND) <- levels_new$New_Level
levels(acs2021$public_work) <- levels_new_pub$New_Level
summary(levels(acs2021$IND))
summary(levels_n)
summary(names(levels))
acs2021$public_work_num <- as.numeric(acs2021$public_work == "work for public, stable")
table(acs2021$public_work,acs2021$public_work_num)
print

#("The subgroup we have chosen to look at is people 25-35, in the labor force, working year round, fulltime.")
#Here we will consider people 25-35, in labor force, working year round,and married.
#We changed the subset because it is giving problems; there was no data 
use_varb <- (acs2021$AGE>=25) & (acs2021$AGE<=35) & (acs2021$LABFORCE == 2) & (acs2021$WKSWORK2 >4) & (acs2021$MARST == 1)
dat_use <- subset(acs2021,use_varb)
summary(dat_use)
#The dat_use has  amount 5944 obs

ols_out1 <- lm(public_work_num ~ female + educ_hs + educ_somecoll + educ_college + educ_advdeg + AGE, data=dat_use)
summary(ols_out1)
#The P value is very indicating there is some significance, All of the parameters Pr(>|t|) < 0.05

# Lab 7
# from last time:
ols_out1 <- lm(public_work_num ~ female + educ_hs + educ_somecoll + educ_college + educ_advdeg + AGE, data = dat_use)
summary(ols_out1)
##H not = Variable has no effect on public work
#H alternative = Variable has some effect on public work
#Consider alpha = 0.05
#R squared is 0.07115 and probability in female  and advanced degree is smaller than the p value. For them
#we can reject the null hypothesis


pred_vals_ols1 <- predict(ols_out1, dat_use)
pred_model_ols1 <- (pred_vals_ols1 > mean(pred_vals_ols1))
table(pred = pred_model_ols1, true = dat_use$public_work_num)
#Here we it is not true and predicted to be not true was 2109, predicted to be false but actually true was 647,
#predicted to be true was actually true was 1805, predicted to be true and actually true was 1383

# logit 
model_logit1 <- glm(public_work_num ~ female + educ_hs + educ_somecoll + educ_college + educ_advdeg + AGE, data = dat_use, family = binomial
)
summary(model_logit1)

#
pred_vals <- predict(model_logit1, dat_use, type = "response")
pred_model_logit1 <- (pred_vals > 0.5)
table(pred = pred_model_logit1, true = dat_use$public_work_num)
#Here we it is not true and predicted to be not true was 3490, predicted to be false but actually true was 1415,
#predicted to be true was actually false was 424, predicted to be true and actually true was 615

#We will try different preditc vals

pred_vals <- predict(model_logit1, dat_use, type = "response")
pred_model_logit1 <- (pred_vals > 0.25)
table(pred = pred_model_logit1, true = dat_use$public_work_num)
#Has less amount that predicted was false and it is actuakky false
#predicted to be false but actually true
#Has more false postive than predict vals > 0.5
#Has more predicted to be true but actually true

pred_vals <- predict(model_logit1, dat_use, type = "response")
pred_model_logit1 <- (pred_vals > 0.75)
table(pred = pred_model_logit1, true = dat_use$public_work_num)
#Has really high predicted to be false and actually false
#Has really predicted false, but actually true
#PUMA
dat_use$PUMA_factor <- as.factor(dat_use$PUMA)

d_pub_work <- data.frame(model.matrix(~ dat_use$public_work_num)) 

d_female <- data.frame(model.matrix(~ dat_use$female))
d_educ_hs <- data.frame(model.matrix(~ dat_use$educ_hs))
d_educ_somecoll <- data.frame(model.matrix(~ dat_use$educ_somecoll))
d_educ_college <- data.frame(model.matrix(~ dat_use$educ_college))
d_educ_advdeg <- data.frame(model.matrix(~ dat_use$educ_advdeg))
d_age <- data.frame(model.matrix(~ dat_use$AGE))
d_PUMA <- data.frame(model.matrix(~ dat_use$PUMA_factor))

#Confirmed sum of it is equal to 0
sum( colSums(d_PUMA) == 0)

#Put Together
dat_for_analysis_sub <- data.frame(
  d_pub_work[,2], # need [] since model.matrix includes intercept term
  d_female[,2],
  d_educ_hs[,2],
  d_educ_somecoll[,2],
  d_educ_college[,2],
  d_educ_advdeg[,2],
  d_age[,2],
  d_PUMA[,2:145] )

#Analysis
names(dat_for_analysis_sub)
names(dat_for_analysis_sub) <- sub("dat_use.","",names(dat_for_analysis_sub)) # drops each repetition of dat_use

names(dat_for_analysis_sub)[1] <- "pub_work"
names(dat_for_analysis_sub)[2] <- "female"
names(dat_for_analysis_sub)[3:6] <- c("HS","SomeColl","College","AdvDeg")
names(dat_for_analysis_sub)[7] <- "Age"

names(dat_for_analysis_sub)

#TraiN data
install.packages("standarize")
require("standardize")
set.seed(654321)
NN <- length(dat_for_analysis_sub$pub_work)

#10 percent was used sum of iterations was not zero, so we used 0.35
restrict_1 <- (runif(NN) < 0.35) # use 10% as training data, ordinarily this would be much bigger but start small
summary(restrict_1)
dat_train <- subset(dat_for_analysis_sub, restrict_1)
dat_test <- subset(dat_for_analysis_sub, !restrict_1)

# again check this below, should be zero
sum( colSums(dat_train) == 0)
#Confirmed it is actually zero

#Check
fmla_sobj <- reformulate( names(dat_for_analysis_sub[2:151]), response = "pub_work")

sobj <- standardize(fmla_sobj, dat_train, family = binomial)

s_dat_test <- predict(sobj, dat_test)


#Final
model_lpm1 <- lm(sobj$formula, data = sobj$data)
summary(model_lpm1)
pred_vals_lpm <- predict(model_lpm1, s_dat_test)
pred_model_lpm1 <- (pred_vals_lpm > mean(pred_vals_lpm))
table(pred = pred_model_lpm1, true = dat_test$pub_work)

# logit 
model_logit1 <- glm(sobj$formula, family = binomial, data = sobj$data)
summary(model_logit1)
pred_vals <- predict(model_logit1, s_dat_test, type = "response")
pred_model_logit1 <- (pred_vals > 0.5)
table(pred = pred_model_logit1, true = dat_test$pub_work)
#The coefficcients are more significant in this model than previous model
#i.e education of high school

#Question 3:

#For our final project, we read some articles.
#We found that the labor market is going well and there is optism in the market
#We are thinking of asking questions to labor market such as
#Is there a desparity between race, gender, and where they in relation to people age 25 to 35 getting a job?
#We are still looking for data


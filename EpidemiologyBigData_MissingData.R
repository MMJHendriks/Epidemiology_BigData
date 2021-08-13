# EPIDEMIOLOGY AND BIG DATA: DEALING WITH MISSING DATA 
Formula <- formula(hosp ~ vacc + DM + cvd + pulm + I(log(contact)) + age + sex)

#install.packages(c("VIM", "mnormt", "mice"))

library("VIM")
library("mnormt")
library("mice")

packageVersion("mice")
getwd()
setwd("~/ADS/Epidemology and Big Data/R assignments week 48")
load("miss.data.uni.RData") # if the data is in your working directory
set.seed(111111)
summary(miss.data.uni)
str(miss.data.uni)

pctMissing <- function(x) round(colMeans(is.na(x)) * 100, 2) 
pct.missing <- pctMissing(miss.data.uni)
pct.missing
# so only data in cvd is missing 

miss.cvd <- is.na(miss.data.uni$cvd) # Indicators of missing information on cvd.
aggr(miss.data.uni, numbers = TRUE) #figure to inspect the presence of missingness

#Q1 How many observations do we have? What percentage observations is missing, on each variable?
#We have a total of 40000 observations. The missingness on the variables varies 
#from 0% to 24.48%. On average, 3.06%  (24.48/8 variables) of all variables are missing.

### simple methods for handling missing values 
#For all imputation methods, we will store the estimated 
#regression coefficient for influenza vaccination and the SE
results.vacc <- data.frame("b" = numeric(), "se" = numeric())

#CCA
CCAmodel <- glm(Formula, family = binomial(), data = miss.data.uni) #CCA is default
CCAmodel
# what is the difference between a lm and a generalized lm? 
#q2 how many participants were used for estimating parameters?
#The number of residual degrees of freedom is 30199 and 8 parameters were estimated. 
#Therefore, 30207 patients were used in this model development, 
#meaning that 24.48% of the data was discarded, even though on average only 3.06% was missing.

results.vacc["CCA",] <- c(coef(CCAmodel)["vacc"], coef(summary(CCAmodel))["vacc", "Std. Error"])
results.vacc["CCA",]

#Q3 ow does CCA affect the distribution of the variable cvd? 
#mean has decreased, cor as decrease a little bit, sd is the same but with less cases

#install.packages("questionr")
library("questionr")
odds.ratio(CCAmodel)
# look at vacc OR  -> 1.859-01 = 0.19  under the 2.5% for lower bound and 97,5 for upper - 95% CI
# outcome is being hospitalised, a decrease in odd of being hospitalised for 81 percent 

#Q5 Does CCA  yield unbiased estimates for the regression coefficients and SE? 
#If CVD values are MCAR, the CCA approach will yield unbiased estimates for the 
#regression coefficients and standard errors. However, due to the exclusion of 
#many participants, estimates will be very imprecise. (less cases?)


### drop covariates - omit cvd 
dropmodel <- glm(hosp ~ vacc + DM + pulm + I(log(contact)) + age + sex, data = miss.data.uni, family = binomial()) 
dropmodel
results.vacc["Drop", ] <- c(coef(dropmodel)["vacc"], coef(summary(dropmodel))["vacc", "Std. Error"])
#Q6 what is the key problem with this approach? 
# we no longer adjust our analysis for a key confounder, thereby potentially 
#introducing bias in the adjusted odds ratio of influenza vaccination.

### mean imputation 
mean.imputed.data <- miss.data.uni 
mean.imputed.data$cvd[miss.cvd] <- mean(miss.data.uni$cvd, na.rm = TRUE)
summary(mean.imputed.data)

mean(miss.data.uni$cvd, na.rm = TRUE) # Results from CCA
mean(mean.imputed.data$cvd) # Results from mean imputation
#Q7 How does mean imputation affect the mean of the imputed variable cvd?
#the means are unchanged as the imputed value is equal to the mean 

sd(miss.data.uni$cvd, na.rm = TRUE) # Results from CCA
sd(mean.imputed.data$cvd) # Results from mean imputation
#q8 how does mean imputation affect the standard deviation of the imputed variable cvd? 
# standard deviations are now smaller, as all imputed values lie at zero distance to the mean 
# this is a too optimistic estimate of SE i.e. true error for population 

cor(miss.data.uni, use = "complete.obs")["cvd", "vacc"]
cor(mean.imputed.data)["cvd", "vacc"]
#How does mean imputation affect the correlation between the imputed variable cvd and the treatment vacc?
#The correlations have become weaker, as the imputed values do not covary with other variables.
#so it is no longer if cvd is higger, for people who are not vaccinated??

#10  Estimate the adjusted odds ratio for influenza vaccination in the imputed data. 
#Do you expect an unbiased estimate of the effect of vaccination?
#an unbiased estimate of the error of the effect of vaccination?
meanmodel <- glm(Formula, data = mean.imputed.data, family = binomial())
meanmodel

odds.ratio(meanmodel)
# odds ratio = 0.679 with CI 0.514, 0.906
# means, a decrease in odd of being hospitalised for 32 percent??
#answer: No, the associations are distorted. The SE is irrelevant, because the estimate is biased.

results.vacc["Mean imputation", ] <- c(coef(meanmodel)["vacc"], 
                                       coef(summary(meanmodel))["vacc", "Std. Error"])

#########################
#REGRESSION
#As cvd represents a binary variable, we can use logistic regression analysis 
#to generate predictions for the missing values. The dependent variable is then cvd, 
#and the independent variables are all remaining variables including the outcome hosp.

### predict
imp.outcome <- "cvd"
imp.predictors <- "hosp + vacc + DM + pulm + I(log(contact)) + age + sex"
imp.formula <- formula(paste(imp.outcome, "~", imp.predictors) )
impmodel1   <- glm(imp.formula, data = miss.data.uni, family = binomial()) #model 

#For binary variables, this predicted value represents a probability (type = "response"). 
#Imputed values can then be generated as follows
regression1.data <- miss.data.uni
regression1.data$cvd[miss.cvd] <- prob.cvd2 <- predict(
  impmodel1, newdata = miss.data.uni[miss.cvd, ], type = "response")
#We can now inspect the original and imputed values of cvd (which are the ones that differ from 0 or 1):
cvd.data <- data.frame(cvd.original = miss.data.uni$cvd, cvd.imputed = regression1.data$cvd)
head(cvd.data)

#Q11  How does imputation affect the mean, 
#the standard deviation and the correlation of the imputed variable cvd?
mean(regression1.data$cvd)  #0.4965  original: 0.467
sd(regression1.data$cvd)    #0.4477
cor(regression1.data)["cvd", "vacc"]   #0.1356
#Answer: The standard deviation of imputed variables are still too small. 
#Conversely, the correlations have substantially increased 
#and the means no longer fully correspond (slight increase).

#now estimate effect of influenza vaccination 
regression1model <- glm(Formula, data = regression1.data, family = binomial())
regression1model 

results.vacc["Regression 1", ] <- c(coef(regression1model)["vacc"], 
                                    coef(summary(regression1model))["vacc", "Std. Error"])
#Q 12 Do you think this approach yields valid estimates for the effect of influenza vaccination?
#When observations are only missing for one variable, this method could produce 
#valid estimates for the regression coefficients. However, uncertainty in 
#imputed values is completely ignored. For instance, in reality we rarely 
#observe the 'expected' values due to random variation. Ignoring this 
#uncertainty may lead to standard errors that are too small.


### Predict + Noise 
#For binary outcomes, we can add model-based noise by generating the imputed value 
#from a binomial distribution of one 'trial' (size = 1), conditional on covariates:
N <- sum(miss.cvd) # Number of missing cvd
regression2.data <- miss.data.uni
regression2.data$cvd[miss.cvd] <- rbinom(n = N, size = 1, prob = prob.cvd2)

cvd.data <- data.frame(cvd.original = miss.data.uni$cvd, cvd.imputed1 = regression1.data$cvd, cvd.imputed2 = regression2.data$cvd)
head(cvd.data)
cvd.data
  

#Q13. How does imputation affect the mean, the standard deviation and the 
#correlation of the imputed variable cvd?
#The distribution for the imputed variable cvd is now fairly similar to the distribution in the original data.

regression2model <- glm(Formula, data = regression2.data, family = binomial())
regression2model 

## Residual Deviance: 2897  AIC: 2913
results.vacc["Regression 2", ] <- c(coef(regression2model)["vacc"], coef(
  summary(regression2model))["vacc", "Std. Error"])
results.vacc["Regression 2", ]


###  Predict + noise + parameter uncertainty
# Rather that directly using the estimated regression coefficients (intercept and slope) 
# for generating predicted values for cvd, we can add variability in these coefficients
# by relating to their standard error.

#For each patient, generate a random draw of regression coefficients from the 
#multivariate Student-t distribution (rmt() from the mnormt package). Ideally, 
#we only need to do this for patients with missing values (denoted by miss.cvd)
beta.i3 <- rmt(N, mean=impmodel1$coef, S=vcov(impmodel1), df=impmodel1$df.residual)
head(beta.i3)

r3.pred.data <- model.matrix(formula(paste("~", imp.predictors)), data=miss.data.uni[miss.cvd,])
head(r3.pred.data)

#calulate the linear predictors and use the inverse logit to produce the probabilities of cvd
prob.cvd3 <- rep(NA, N)
for (i in 1:N) {
  prob.cvd3[i] <- 1/(1+exp(-r3.pred.data[i,] %*% beta.i3[i,]))
}

#we again draw cvd from a binomial distribution to account for sampling variation:
regression3.data <- miss.data.uni
regression3.data$cvd[miss.cvd] <- rbinom(n = N, size = 1, prob = prob.cvd3)

cvd.data <- data.frame(cvd.original = miss.data.uni$cvd, cvd.imputed1 = regression1.data$cvd, cvd.imputed2 = regression2.data$cvd, cvd.imputed3 = regression3.data$cvd)
head(cvd.data)
# mean, sd and correl seem to be accurate (not to distinct from original)

regression3model <- glm(Formula, data = regression3.data, family = binomial())
regression3model 
results.vacc["Regression 3", ] <- c(coef(regression3model)["vacc"], coef(summary(regression3model))["vacc", "Std. Error"])


### Multiple imputation
# approach before ignores any uncertainty arising from imputation
#In order to preserve all uncertainty arising from imputation, we need to 
#repeat the sampling procedures many times, and generate many imputed datasets. 
#Then, values that can reliably be imputed will not vary much across imputed datasets, 
#whereas  values that are difficult to impute will substantially vary across imputed datasets. 
n.imp <- 50 # Number of predictions per patient.

results.vacc4 <- as.data.frame(matrix(NA, ncol = 5, nrow = n.imp)) # Save distribution of vacc for each imputed dataset
colnames(results.vacc4) <- c("mean", "sd", "cor", "beta.vacc", "se.vacc")

# Initialize full dataset
regression4.data <- miss.data.uni

# Initiatize data for which imputations are needed
r4.pred.data <- model.matrix(formula(paste("~", imp.predictors)), data=miss.data.uni[miss.cvd,])

# Multiple Imputation
for (j in 1:n.imp) { 
  beta.i4 <- rmt(N, mean=impmodel1$coef, S=vcov(impmodel1), df=impmodel1$df.residual)
  prob.cvd4 <- rep(NA, N)
  for (i in 1:N) {
    prob.cvd4[i] <- 1/(1+exp(-r4.pred.data[i,] %*% beta.i4[i,]))
  }
  
  regression4.data$cvd[miss.cvd] <- rbinom(n = N, size = 1, prob = prob.cvd4)
  regression4model   <- glm(Formula, data = regression4.data, family = binomial())
  
  # Save the results
  results.vacc4$mean[j] <- mean(regression4.data$cvd)
  results.vacc4$sd[j] <- sd(regression4.data$cvd)
  results.vacc4$cor[j] <- cor(regression4.data)["cvd", "vacc"]
  results.vacc4$beta.vacc[j] <- coef(regression4model)["vacc"]
  results.vacc4$se.vacc[j]    <- coef(summary(regression4model))["vacc", "Std. Error"]
}
#the 50 imputed datasets
print(results.vacc4)

#Rubin has proposed some rules to combine the results across datasets. For 
#simple statistics such as the mean or sd, we can simply take the average:
apply(results.vacc4[,c("mean", "sd", "cor", "beta.vacc")], 2, mean) #what does the 2 do???

#The standard error for the pooled regression coefficient for vacc is thus given as:
Qbar <- mean(results.vacc4$beta.vacc)
U <- sum(results.vacc4$se.vacc**2)/n.imp
B <- sum((results.vacc4$beta.vacc - Qbar)**2)/(n.imp-1)
se.beta.vacc <- sqrt(U + (1+1/n.imp)*B)
se.beta.vacc
# Store results
results.vacc["Regression 4", ] <- c(Qbar, se.beta.vacc)

### Multiple Imputation in mice()
data.mice <- model.frame(formula(paste("~ 0 + ", imp.predictors, "+", imp.outcome)), data = miss.data.uni, na.action = 'na.pass')
data.mice$cvd <- as.factor(data.mice$cvd)
colnames(data.mice)[5] <- "logContact"
head(data.mice)

# Initialize Imputation Model
setup.imp <- mice(data.mice, maxit=0)

# Lets ensure logistic regression is used for imputation
setup.imp$method["cvd"] <- "logreg"

# Start the imputation. No Gibbs sampler is needed (hence maxit=1)
dat.imp <- mice(data.mice, method = setup.imp$method, m = n.imp, maxit = 1, printFlag = F)

# Start the analyses
regression5model <- with(data=dat.imp, exp=glm(hosp ~ vacc + DM + cvd + pulm + logContact + age + sex, family = binomial()))

# Use Rubin's rules
pooledEst <- pool(regression5model)

# Store results
results.vacc["Regression 5", ] <- c(pooledEst$pooled$estimate[2], sqrt(pooledEst$pooled$t[2]))
results.vacc["Regression 5", ]
  ### all you need to use here is 1 mice, 2 with, 3 pool 


################################
#COMPARISON
load("full.data.RData")
fullmodel <- glm(Formula, data = FULL.data, family = binomial())
results.vacc["Original data", ] <- c(coef(fullmodel)["vacc"], coef(summary(fullmodel))["vacc", "Std. Error"])

#Q14 For which method are the obtained parameter estimates closest to 
# the estimates of the full data model?
results.vacc

#percentage different estimate obtained diff strategies and estim full dataset
PCT.diff <- function(x, ref = ncol(x)) 
{
  x <- t(as.matrix(x))
  y <- round(t(((x[ , -ref] - x[ , ref]) / x[, ref])[, -ref] * 100), 2)
  y[ , ] <- paste(y, "%", sep = "")
  noquote(y)
}

PCT.diff(results.vacc)
#regression 1 the closes for b and 2nd closes for se - regression 4 closest for se
  #why does mice() give you such a different se? coincidence? 
#conclusion: similar performance, but simply because it is only missing for one variable 

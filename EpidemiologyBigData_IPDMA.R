# EPIDEMIOLOGY AND BIG DATA: individual participant data meta-analysis (IPD-MA)

# In 2006, Rovers et al. (2006) performed IPD-MA of six randomized trials of the effects of 
# antibiotics in children with acute otitis media. Their aim was to identify subgroups of 
# children who would and would not benefit more than others from treatment with antibiotics. 

#install.packages("metafor")
#install.packages("lme4")
#install.packages("logistf")
library("metafor")
library("lme4")
library("logistf")
load(choose.files())  #to open data set 

#Explore the data 
summary(ds.final)
#Q1 summarise active and control group data
summary(ds.final[ds.final$STUDY==1 & ds.final$TREAT==0,])
summary(ds.final[ds.final$STUDY==1 & ds.final$TREAT==1,])

#log-binomial model to estimate a relative risk (RR) in each trial:
fit.unadj1 <- glm(POUTCOME ~ TREAT, data = ds.final, 
                  family = binomial(link="log"), subset = STUDY==1)
fit.unadj1

#Q5 compute RELATIVE RISK for antibiotics treatment -> exponent coefficient
exp(coefficients(fit.unadj1)["TREAT"])
  # get confidence interval 
exp(confint(fit.unadj1, "TREAT"))

#Q6 calculate an adjusted treatment effect for trial 1. 
fit.adj1 <- glm(POUTCOME ~ TREAT + AGE + GENDER + BILAT_0, 
                data = ds.final, 
                family = binomial(link = "log"), 
                subset = STUDY==1)
exp(c(coefficients(fit.adj1)["TREAT"], confint(fit.adj1, "TREAT")))

#### TWO-STAGE MA
# each trail first analysed in seperation; log-binominal model to estimated adjusted RR

studies <- sort(unique(ds.final$STUDY))
ad.results <- data.frame(logRR=NULL, logRR.se=NULL)

for (i in 1:length(studies)) {
  fit <- glm(POUTCOME ~ TREAT + AGE + GENDER + BILAT_0, 
             data = ds.final, 
             family = binomial(link="log"), 
             subset = STUDY==studies[i])
  ad.results[i, "logRR"] <- coefficients(fit)["TREAT"]
  ad.results[i, "logRR.se"] <- sqrt(vcov(fit)["TREAT","TREAT"]) #variance-covariance matrix
}

ad.results
#plot summarizing the different treatment effects:
with(ad.results, forest(x = logRR, sei = logRR.se, 
                        transf = exp, refline = 1,
                        xlab = "Relative Risk", 
                        main = "Use of antibiotics"))


  ### FIXED EFFECTS 
# summary effect is then given by ??
fe.2stage <- rma(yi = logRR, sei = logRR.se, method = "FE", data = ad.results)
summary(fe.2stage)
#Q9 summary estimate for the RR & 95% CI
# summary estimate RR is exp(-0.452). (model results estimate)
#The 95% confidence interval ranges from exp(-0.452 - 1.96 x 0.077) to exp(-0.452 + 1.96 x 0.077).

  ### RANDOM EFFECTS
#Q10 summary estimate for RR and 95% CI
re.2stage <- rma(yi = logRR, sei = logRR.se, method = "REML", data = ad.results)
summary(re.2stage)
  #answer
exp(-0.4664)
exp(-0.4664-1.96*0.112)
exp(-0.4664+1.96*0.112)
exp(c("RR" = as.numeric(coef(re.2stage)),    # CONFIDENCE INTERVAL 
      "2.5 %" = (predict(re.2stage))$ci.lb, 
      "97.5 %" = (predict(re.2stage))$ci.ub))

#Adjust between-study heterogeneity -> sidik-jonkman Hartung-Knapp
re.2stage.knha <- rma(yi = logRR, sei = logRR.se, method = "REML", test = "knha", 
                      data = ad.results)
summary(re.2stage.knha)

exp(c("RR" = as.numeric(coef(re.2stage.knha)), 
      "2.5 %" = (predict(re.2stage.knha))$ci.lb, 
      "97.5 %" = (predict(re.2stage.knha))$ci.ub))

#Q12 Do you think there is reason to assume statistical heterogeneity? 
    #Extract the estimated between-study standard deviation and I2 statistic. 
level <- 0.05
crit <- qt(c(level/2, 1-(level/2)), df = (re.2stage.knha$k-2))
mu <- re.2stage.knha$b[[1]]
tau2 <- re.2stage.knha$tau2
sigma2 <- vcov(re.2stage.knha)
mu + crit * c(sqrt(tau2 + sigma2))
#Q13 for 95% PI
exp(mu + crit * c(sqrt(tau2 + sigma2)))


##### INVESTIGATING EFFECT MODIFIERS
#META REGRESSION
mean.age <- mean.bilat_0 <- array(NA, dim=length(studies))
for (i in 1:length(studies)) {
  mean.age[i] <- mean(ds.final[ds.final$STUDY==studies[i], "AGE"])
  mean.bilat_0[i] <- mean(ds.final[ds.final$STUDY==studies[i], "BILAT_0"])
}
ad.results <- cbind(ad.results, mean.age, mean.bilat_0)
ad.results

remr.bilat0 <- rma(yi = logRR, sei = logRR.se, mods = mean.bilat_0, 
                   method = "REML", test = "knha", data = ad.results)
summary(remr.bilat0)

#### POOLING OF WITHIN-TRAIL COVARIATE INTERACTION 
ad2.results <- data.frame(logRR = NULL, logRR.se = NULL, 
                          beta.int = NULL, beta.int.se = NULL)

for (i in 1:length(studies)) {
  fit <- glm(POUTCOME ~ TREAT + AGE + GENDER + BILAT_0 + TREAT:BILAT_0, 
             data = ds.final, 
             family = binomial(link="log"), 
             subset = STUDY==studies[i])
  ad2.results[i, "logRR"] <- coefficients(fit)["TREAT"]
  ad2.results[i, "logRR.se"] <- sqrt(vcov(fit)["TREAT","TREAT"])
  ad2.results[i, "beta.int"] <- coefficients(fit)["TREAT:BILAT_0"]
  ad2.results[i, "beta.int.se"] <- sqrt(vcov(fit)["TREAT:BILAT_0","TREAT:BILAT_0"])
}

ad2.results

# apply Firth correction to deal with small sample size
#library(logistf)
ds.study3 <- subset(ds.final, STUDY==studies[3])
fit <- logistf(POUTCOME ~ TREAT + AGE + GENDER + BILAT_0 + TREAT:BILAT_0, 
               data = ds.study3)
index1 <- which(names(coefficients(fit))=="TREAT")
index2 <- which(names(coefficients(fit))=="TREAT:BILAT_0")

ad2.results[3, "logRR"] <- coefficients(fit)[index1]
ad2.results[3, "logRR.se"] <- sqrt(vcov(fit)[index1, index1])
ad2.results[3, "beta.int"] <- coefficients(fit)[index2]
ad2.results[3, "beta.int.se"] <- sqrt(vcov(fit)[index2, index2])
ad2.results

# presence of treatment-covariate interaction use regression coef, bela.int, summarize estimates using RE MA
repwt.bilat0  <- rma(yi = beta.int, sei = beta.int.se, 
                     method = "REML", 
                     test="knha", 
                     data = ad2.results)
summary(repwt.bilat0)


#################################################################################
### Common treatment effect, common baseline risk -> biased
fit1 <- glm(POUTCOME ~ TREAT + AGE + GENDER + BILAT_0, 
            data = ds.final, 
            family = binomial(link="log"))
summary(fit1)

### Common treatment effect, fixed baseline risks
ds.final$SID <- as.factor(ds.final$STUDY)
fit2 <- glm(POUTCOME ~ 0 + SID + TREAT + AGE:SID + GENDER:SID + BILAT_0:SID, 
            data = ds.final, 
            family = binomial(link="log"))
summary(fit2)

exp(-0.463883)
exp(-0.463883-1.96*0.076568)
exp(-0.463883+1.96*0.076568)
exp(c("RR" = as.numeric(coef(fit2)),    # why does this not work 
      "2.5 %" = (predict(fit2))$ci.lb, 
      "97.5 %" = (predict(fit2))$ci.ub))

### Random treatment effect, fixed baseline risks
fit3 <- glmer(POUTCOME ~ 0 + SID + TREAT + (0 + TREAT|SID) + 
                AGE:SID + GENDER:SID + BILAT_0:SID, 
              data = ds.final, 
              family = binomial(link="log"))
summary(fit3)


### INVESTIGATING EFFECT MODIFIERS
### merging within-study and across-study interactions
fit2a.em <- glm(POUTCOME ~ 0 + SID + AGE:SID + GENDER:SID + BILAT_0:SID + BILAT_0*TREAT, 
                data = ds.final, 
                family = binomial(link="log"))
summary(fit2a.em)

### separating within/study and across-study interactions
ds.final$MEAN_BILAT_0 <- ds.final$C_BILAT_0 <- NA

studies <- sort(unique(ds.final$STUDY))

for (i in 1:length(studies)) {
  sel.pats <- which(ds.final$STUDY==studies[i])
  
  # Calculate prevalence of BILAT_0 in each trial
  ds.final$MEAN_BILAT_0[sel.pats] <- mean(ds.final$BILAT_0[sel.pats])
  
  # Center the covariate values for BILAT_0
  ds.final$C_BILAT_0[sel.pats] <- ds.final$BILAT_0[sel.pats] - 
    mean(ds.final$BILAT_0[sel.pats])
}

fit2b.em <- glm(POUTCOME ~ 0 + SID + # trial-specific intercept term
                  TREAT + # common treatment effect
                  AGE:SID + # trial-specific term for AGE
                  GENDER:SID + # trial-specific term for GENDER
                  BILAT_0:SID + # trial-specific term for BILAT_0
                  C_BILAT_0:TREAT +  # common within-trial interaction
                  TREAT:MEAN_BILAT_0 , # beteen-trial interaction
                data = ds.final, 
                family = binomial(link="log"))
summary(fit2b.em)

# ramdom TE
fit3b.em <- glmer(POUTCOME ~ 0 + SID + # trial-specific intercept term
                    TREAT + (0 + TREAT|SID) + # random treatment effect
                    AGE:SID + # trial-specific term for AGE
                    GENDER:SID + # trial-specific term for GENDER
                    BILAT_0:SID + # trial-specific term for BILAT_0
                    C_BILAT_0:TREAT + # common within-trial interaction
                    TREAT:MEAN_BILAT_0, # between-trial interaction
                  data = ds.final, family = binomial(link="log"))
summary(fit3b.em)

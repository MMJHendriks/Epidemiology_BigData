# EPIDEMIOLOGY AND BIG DATA: individual participant data meta-analysis (IPD-MA) 2

# we use Individual Participant Data (IPD) sets from 9 studies that recruited consecutive outpatients with suspected deep vein thrombosis (Geersing et al. 2014)
# GOAL: developing and validating a model to predict the presence of DVT using IPD originating from 9 studies.

install.packages(c("MASS", "Hmisc", "rms", "data.table", "glmmLasso", "metafor", 
                   "metamisc", "mnormt", "pROC", "car","mvmeta", "glmnet"))

library(MASS)
library(data.table)
library(mnormt)
library(mvmeta)
load(file.choose())
summary(ipd)

head(ipd);dim(ipd)  # show first six rows and dimensions of dataset
n <- unique(length(table(ipd$studyname))) # determine number of studies
data.table(ipd[,-1])[, lapply(.SD, mean), by = studyname] # summarize data per study
data.table(ipd[,-1])[, lapply(.SD, sum), by = studyname]

    ### CHAPTER 2 
### Univariable associations/ predictor effects
# Select data
dat.sel <- subset(ipd,select=-c(studyname,studyid,care,country,dvt,typdim))

# define a matrix to store coefficients
uni.coef <- matrix(data = NA, nrow = n, 
                   ncol = ncol(dat.sel))
colnames(uni.coef) <- names(dat.sel)
rownames(uni.coef) <- unique(ipd$studyname)

for(i in 1:n){
  # select data from study i out of n
  ipd.uni <- subset(ipd, ipd$studyid==i)
  # determine the association between predictor j and dvt in study i
  for(j in 1:ncol(dat.sel)){
    dat.uni <- subset(ipd.uni, select = -c(studyname,studyid,care,country,dvt,typdim))
    ipd.uni$x <- as.matrix(dat.uni[j])
    fit.uni <- glm(dvt ~ x, data = ipd.uni, family = binomial)
    uni.coef[i,j] <- round(coef(fit.uni)[2], digits=4)
  }
}
uni.coef

  ### CHAPTER 3
### multivariable associations
explanatory <- c("age","par","tend","leg","pit","altdiagn","histdvt",
                 "malign","surg","calfdif3","vein","oachst","sex","notraum","ddimdich")

#3.1 predictor selection using LESSO on the overall dataset (ignoring clustering)
library(glmnet)
# Set random seed
set.seed(123)
# determine penalization
lasso.fit <- cv.glmnet(x = as.matrix(subset(ipd,select=explanatory)), 
                       y = as.matrix(subset(ipd,select=c(dvt))),
                       family="binomial", type.measure="mse", alpha=1, nfolds=10)

# print coefficients
coef(lasso.fit, s = "lambda.1se")

#3.2 predictor selection using LASSO with mixed effects
library(glmmLasso)

fmla <- as.formula(paste("dvt ~", paste(explanatory, collapse="+")))
ipd$study <- as.factor(ipd$studyname)

# Lets try 2 different penalties here.
# The optimal penalty can be identified through cross-validation
lm1 <- glmmLasso(fmla, rnd = list(study=~1+par+tend+leg+calfdif3+notraum+ddimdich), 
                 lambda=10, data = ipd, family=binomial())
lm2 <- glmmLasso(fmla, rnd = list(study=~1+par+tend+leg+calfdif3+notraum+ddimdich), 
                 lambda=100, data = ipd, family=binomial())

data.frame(lm1=coef(lm1), lm2=coef(lm2))

xs.final <- c("malign","surg","calfdif3","vein","oachst","sex","notraum","ddimdich")
ipd2 <- ipd[,c("dvt","studyid","studyname",xs.final)]
head(ipd2)

  ### CHAPTER 4
###Internal-external cross-validation
model.perf.val <- data.frame(validation.study=unique(ipd2$studyname), 
                             cstat.lb=NA, cstat=NA, cstat.ub=NA, 
                             citl=NA, citl.se=NA, 
                             calslope=NA, calslope.se=NA, 
                             O=NA, E=NA, N=NA)

library(pROC)
library(rms)

pdf("calibration_dvt_stacked.pdf")
for(i in 1:n){
  # define development and validation datasets
  ipd.dev <- subset(ipd2,ipd2$studyid!=i,select=-c(studyid,studyname))
  ipd.val <- subset(ipd2,ipd2$studyid==i,select=-c(studyid,studyname))
  
  # fit model in development data
  fit.dev <- lrm(dvt ~ ., data = ipd.dev)
  
  predy.val <- predict(fit.dev, type="fitted", newdata=ipd.val)
  predlp.val <- predict(fit.dev, type="lp", newdata=ipd.val)
  
  # determine calibration slope in validation data
  fit.val.slope <- glm(dvt ~ predlp.val, data=ipd.val, family="binomial")
  
  # determine calibration-in-the-large in validation data
  fit.val.citl <- glm(dvt ~ 1, offset=predlp.val, data=ipd.val, family="binomial")
  
  # determine discriminative performance (c statistic) in development and validation sets
  cstat.val <- roc(ipd.val$dvt, predy.val,ci=T)$ci[1:3]
  
  # determine calibration in development set (plots + intercept / slope)
  val.prob(predy.val, ipd.val$dvt,g=10,
           xlab = paste("Predicted Probabiliy Validation, in study =",paste(i)),
           ylab = paste("Actual Probabiliy Validation, in study =",paste(i)))
  
  model.perf.val$validation.study[i] <- (ipd$studyname[which(ipd$studyid==i)])[1]
  model.perf.val$cstat.lb[i] <- cstat.val[1]
  model.perf.val$cstat[i] <- cstat.val[2]
  model.perf.val$cstat.ub[i] <- cstat.val[3]
  model.perf.val$citl[i] <- fit.val.citl$coef[1]
  model.perf.val$citl.se[i] <- sqrt(diag(vcov(fit.val.citl)))[1]
  model.perf.val$calslope[i] <- fit.val.slope$coef[2]
  model.perf.val$calslope.se[i] <- sqrt(diag(vcov(fit.val.slope)))[2]
  model.perf.val$O[i] <- sum(ipd.val$dvt)
  model.perf.val$E[i] <-sum(predy.val)
  model.perf.val$N[i] <- dim(ipd.val)[1]
}
dev.off() # this statement terminates the PDF statement

metapred(data = ipd2, strata = "studyname", formula = f, scope = f, family = binomial)

  ### CHAPTER 5 synthesis of performance results
### 5.1 visual assessment 
apply(model.perf.val, 2, FUN = median)[c("cstat","citl","calslope")]

library(metafor)
library(metamisc)
metafor::forest(x = model.perf.val$citl, sei = model.perf.val$citl.se, 
                refline = 0, xlab = "Calibration-in-the-large")
metafor::forest(x = model.perf.val$calslope, sei = model.perf.val$calslope.se, 
                refline = 1, xlab = "Calibration Slope")
metafor::forest(x = model.perf.val$cstat, ci.lb = model.perf.val$cstat.lb, 
                ci.ub = model.perf.val$cstat.ub, 
                refline = NA, xlab = "Area under the ROC curve")

# O:E ratio
# Calculate the log O:E ratio and its standard error
oe.ad <- oecalc(O=O, E=E, N=N, data=model.perf.val, g="log(OE)")
model.perf.val$logOE <- oe.ad$theta
model.perf.val$logOE.se <- oe.ad$theta.se

metafor::forest(x = model.perf.val$logOE, sei = model.perf.val$logOE.se, 
                atrans = exp, xlab = "Total O:E ratio")

###5.2 MA of model performance
# calibration-in-the-large
model.CITL <- rma(yi = citl, sei = citl.se, method = "REML", 
                  test = "knha", data = model.perf.val)
summary(model.CITL)

# total O:E ratio 
model.OE <- with(model.perf.val, valmeta(measure = "OE", O = O, E = E, N = N))
model.OE

# calibration slope 
model.CS <- rma(yi = calslope, sei = calslope.se, 
                method = "REML", test = "knha", data = model.perf.val)
summary(model.CS)

# concordance statistic
model.cstat <- valmeta(cstat = cstat, cstat.cilb = cstat.lb, cstat.ciub = cstat.ub, 
                       data = model.perf.val)
model.cstat

  ### CHAPTER 6 meta-regression
# define study level characteristics of interest
model.perf.val$care <- model.perf.val$age.m <- NA

# determine the care, country or mean age per study
for(i in 1:n) { 
  model.perf.val$care[i] <- mean(ipd$care[ipd$studyname==model.perf.val$validation.study[i]])
  model.perf.val$age.m[i] <- mean(ipd$age[ipd$studyname==model.perf.val$validation.study[i]])
}

# calibration-in-the-large
rma(yi = citl, sei = citl.se, mods = care, method = "REML", test = "knha", data = model.perf.val) 
rma(yi = citl, sei = citl.se, mods = age.m, method = "REML", test = "knha", data = model.perf.val) 

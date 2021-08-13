# EPIDEMIOLOGY AND BIG DATA: MIXED MODELS
library(foreign)
library(gdata)
library(nlme)
library(psych)
library(ggplot2)
library(car)
getwd()
load("school.Rdata")
summary(london)

### QUESTION 1 A
ggplot(london, aes(x=standlrt, y=normexam))+
  geom_point(xlab=)
# or
plot(london$standlrt, london$normexam,
     xlab="standardized London Reading Test score", 
     ylab="normalized exam score", main="All schools together", cex.main=1.15, pch=20)

### QUESTION B; simple linear regression
simple<-(lm(normexam ~ standlrt, data=london))
summary(simple)

### QUESTION C 
p <- ggplot(data = london, aes(x = standlrt, y = normexam))
p + geom_point() + geom_smooth(method=lm, se = FALSE) + facet_wrap(~school)

### QUESTION D
persch <- lmList(normexam~standlrt| school, data=london)
mean(coef(persch)[,1])
mean(coef(persch)[,2])
sd(coef(persch)[,1])
sd(coef(persch)[,2])

### QUESTION E  linear mixed model with random intercept to predict exam scores using the LRT scores
sch.lme.1 <- lme(fixed=normexam~standlrt, random=~1 | school, data=london, method="ML")
summary(sch.lme.1)

### QUESTION F Add a random slope to the model
sch.lme.2 <- lme(fixed=normexam~standlrt, random=~standlrt | school, data=london, method="ML")
summary(sch.lme.2)
VarCorr(sch.lme.2)
  ### first few RANDOM EFFECTS of the new model (for 6 schools)
head(ranef(sch.lme.2))

### QUESTION G Add child- (gender) and school-level explanatory variables.
sch.lme.3 <- lme(normexam~standlrt + factor(gender), random=~standlrt | school, data=london, method="ML")
summary(sch.lme.3)
  # add school gender and school average 
sch.lme.4 <- lme(normexam~standlrt + factor(gender)+ factor(schgend) + factor(schav),
                 random=~standlrt | school, data=london, method="ML")
summary(sch.lme.4)
  
  # get approximate (Wald) confidence intervals,
intervals(sch.lme.4)

######################################################
### EXERCISE 2
# Since baseline (pre-randomization) DBP (dbp) will likely be associated with post-treatment 
# DBP and will reduce the variation in the outcome (thereby increasing our power to detect a treatment effect), we wish to include it here as a covariate.
load("dbplast.Rdata")
### A
trial <- dbplast
p1 <- ggplot(data = trial, aes(x = treat, y = dbp, group=center))
p1 + geom_point() + facet_wrap(~center)
  # No immediately discernible differences between the two treatments 
p2 <- ggplot(data = trial, aes(x = dbp1, y = dbp, group=center))
p2 + geom_point() + facet_wrap(~center)
  # no strong relation pre-randomization dbp1 and final dbp

### B 
#use a model with only a random intercept per center. This model assumes
#centers have different mixes of patients with (on average) higher or lower 
#blood pressure, but that the trend between DBP and baseline and the 
#difference between treatments is the same in every center. 
lme.1 <- lme(fixed=dbp ~ dbp1 + factor(treat), random=~1|center,
             data=trial, method="ML")
summary(lme.1)
  # Patients on treatment 2 (Nifedipine) have, on average, a 1.1 mmHg lower DBP than patients on treatment 1 
  # or every 1 mmHg increase in baseline DBP, a patient has, on average, 
  # a 0.17 (95% CI: -0.08 - 0.43) mmHg increase in DBP at the end of the study. 
  # but both not statistically significant 
  # The intercept, 74.10, is the average DBP at the end of the study for a 
  # patient on tx 1 with a baseline DBP of 0(!).  ????????????

  # there is quite a bit more variance in diastolic blood pressure 
  # among patients within centers (residual squared) than among the different centers (StdDev)

### C Make a new baseline dbp variable, centered around its mean !!!!!!
trial$cdbp1 <- trial$dbp1 - mean(trial$dbp1)
lme.2 <- lme(fixed=dbp ~ cdbp1 + factor(treat), random=~1|center,
             data=trial, method="ML")
summary(lme.2)
 ### so what is the difference?????

#############################################################
### QUESTION 3

load("crossover.Rdata")
cross <- crossover
tapply(cross$Y,cross$DRUG,mean)
tapply(cross$Y, cross$DRUG, sd)
tapply(cross$Y,cross$PERIOD,mean)
tapply(cross$Y, cross$PERIOD, sd)
  ### what does the period say? should not differ too much right?

### D; mixed model random intercept per patient
cross.lme1 <- lme(Y~factor(DRUG) + factor(PERIOD), 
                  random= ~1|PATIENT, method="ML", data=cross)
summary(cross.lme1)
intervals(cross.lme1)

###################################################
### EXERCISE 4 difference boys and girls, same for single-sex or mixed-gender schools
  # make new variable mixed gender (schgend = 1 vs 2 or 3)
london$mixed <- as.numeric(london$schgend==1)
table(london$mixed,london$schgend) # check new variable

# mixed model with random intercept & random slope, plus gender, school gender & school avg
sch.lme.sub <- lme(normexam~standlrt + factor(gender)+ factor(mixed) + factor(schav) +
                     factor(gender)*factor(mixed), random=~standlrt | school, data=london, method="ML")
summary(sch.lme.sub)

################################################################
################################################################
### EXERCISE 5
##linear mixed models analyses of the Reisby dataset, using time as a continuous variable. 
getwd()
setwd("~/ADS/Epidemology and Big Data/MixedlModelsPart1.2")
# A
load(file = "reisbywide.Rdata")
load(file = "reisbylong.Rdata")
reisby.long$id <- factor(reisby.long$id)
reisby.long$time <- reisby.long$week
reisby.long$week <- factor(reisby.long$week)

head(reisby.wide)
head(reisby.long, n=10)

summary(reisby.wide)
describeBy(x=reisby.wide[,3:8], group=reisby.wide$endo, skew=FALSE)
?describeBy()
#Let's look at the correlations of HDRS scores over the six time points:
round(cor(reisby.wide[,3:8],use="pairwise.complete.obs"),digits=3)

p <- ggplot(data = reisby.long, aes(x = week, y = hdrs, group = id))
p + geom_line()
p + geom_line() + facet_grid(. ~ endo)
p + geom_line() + stat_summary(aes(group = 1), geom = "point", 
                               fun.y = mean, shape = 17, size = 3) + facet_grid(. ~ endo)
p + geom_line() + stat_smooth(aes(group = 1)) + stat_summary(aes(group = 1), 
                                                             geom = "point", fun.y = mean, shape = 17, size = 3) + facet_grid(. ~ endo)

p + geom_point() + geom_line() + facet_wrap(~ id)
#with interpolated curves:
p + geom_point() + facet_wrap(~ id) + stat_smooth(method = "lm", se = FALSE)

# B
#There is a main effect of endo (e.g. people with endogenous depression generally
# have higher/lower HDRS score than those with exogenous depression). Test H0: endo=0.
#There is an interaction between endo and time (e.g. people with endogenous depression 
# have a different time trend for HDRS score than people with exogenous depression). 
# Test H0: time:endo=0.

#C
#mixed model with fixed effects for time (for now, we're modelling time as continuous and linear),
#endo and the interaction of time*endo, and just a random intercept per person:
lme.ril<-lme(fixed=hdrs ~ time*endo, random=~1|id, data=reisby.long, na.action="na.omit", method="ML")
summary(lme.ril)

newd <- reisby.long[,c("id", "endo", "time")]
newd$pred1 <- predict(lme.ril, newdata=newd)
pp1 <- ggplot(data = newd, aes(x = time, y = pred1, group = id))
pp1 + geom_line()
#parallel. This is becuase the time*endo interaction is nearly 0, so the differences 
#in slope between the people with enodgenous and exogenous depression is hardly visible 
ggplot(data = newd, aes(x = time, y = pred1, group = id, color=factor(endo))) + geom_line()
# not exactly parallel 

# D
#fit a mixed model with fixed effects for time, endo, and their interaction, 
#plus a random intercept & random slope for time(continuous/linear) per subject:
lme.ris<-update(lme.ril, random=~time|id)
summary(lme.ris)
intervals(lme.ris)

newd$pred2 <- predict(lme.ris, newdata=newd)
pp2 <- ggplot(data = newd, aes(x = time, y = pred2, group = id))
pp2 + geom_line()


###############################################################
### EXERCISE 6
#load data 
data(Blackmore) #make sure library is loaded
?Blackmore
#A Examine the time variable (age).
head(Blackmore, 20)

## DON'T REALLY UNDERSTAND WHAT THIS DOES 
Blackmore$log.exercise <- log2(Blackmore$exercise + 5/60)
pat <- with(Blackmore, sample(unique(subject[group == "patient"]), 20))
Pat.20 <- groupedData(log.exercise ~ age | subject,
                      data=Blackmore[is.element(Blackmore$subject, pat),])
con <- with(Blackmore, sample(unique(subject[group == "control"]), 20))
Con.20 <- groupedData(log.exercise ~ age | subject,
                      data=Blackmore[is.element(Blackmore$subject, con),])

# Fox's code for the individual plots of 20 control subjects & 20 subjects with anorexia
print(plot(Con.20, main="Control Subjects",
           xlab="Age", ylab="log2 Exercise",
           ylim=1.2*range(Con.20$log.exercise, Pat.20$log.exercise),
           layout=c(5, 4), aspect=1.0),
      position=c(0, 0, 0.5, 1), more=TRUE)
print(plot(Pat.20, main="Patients",
           xlab="Age", ylab="log2 Exercise",
           ylim=1.2*range(Con.20$log.exercise, Pat.20$log.exercise),
           layout=c(5, 4), aspect=1.0),
      position=c(0.5, 0, 1, 1))

# (You could also make these individual plots using ggplot2, only then it's hard to get them side-by-side)
pc <- ggplot(data = Con.20, aes(x = age, y = log.exercise)) + ggtitle("Control Subjects") + ylim(1.2*range(Con.20$log.exercise, Pat.20$log.exercise))
pc + geom_point() + geom_line() + facet_wrap(~subject)

pp <- ggplot(data = Pat.20, aes(x = age, y = log.exercise)) + ggtitle("Patients") + ylim(1.2*range(Con.20$log.exercise, Pat.20$log.exercise))
pp + geom_point() + geom_line() + facet_wrap(~subject)

#install.packages('ggpubr')
# it could be done using the ggarrange function from the ggpubr package
ggpubr::ggarrange(pc + geom_point() + geom_line() + facet_wrap(~subject),
                  pp + geom_point() + geom_line() + facet_wrap(~subject))

#The time variable (age) is measured more continuously than in the Reisby dataset. 
#Though most girls are measured at ages 8, 10, 12, 14 and 16, this is not always
#the case. Some girls are measured at ages between these "categories" of age.

#USE age-8 in the models
# because All girls are measured starting at the age of 8, so age=8 becomes
# the "zero" point for time.
# C Interpret the coefficients of the 3rd model (bm.lme.3).
# Mixed models
# 1. Fixed: age, group & interaction; Random: intercept, age
bm.lme.1 <- lme(log.exercise ~ I(age - 8)*group,
                random = ~ I(age - 8) | subject, data=Blackmore, method="ML")
summary(bm.lme.1)

# 2. Fixed: age, group & interaction; Random: intercept only
bm.lme.2 <- update(bm.lme.1, random = ~ 1 | subject)
anova(bm.lme.1, bm.lme.2) # test for random slopes

# 3. Fixed: age, group & interaction; Random: age only
bm.lme.3 <- update(bm.lme.1, random = ~ I(age - 8) - 1 | subject)
anova(bm.lme.1, bm.lme.3) # test for random intercepts
summary(bm.lme.3)

#look at fixed effects
#Patients and controls did not differ in time spent exercising at age 8 (p = 0.1327). 
#There was a small, statistically significant increase in exercise for the control
#group (p = 0.0384), and a considerably larger increase among patients. The 
#trends for the two groups differed significantly (p < 0.00005).

# figure 8
# fit regressions of log.exercise on age for each subject
pat.list <- lmList(log.exercise ~ I(age - 8) | subject,
                   subset = group=="patient", data=Blackmore)
con.list <- lmList(log.exercise ~ I(age - 8) | subject,
                   subset = group=="control", data=Blackmore)
pat.coef <- coef(pat.list)
con.coef <- coef(con.list)
old <- par(mfrow=c(1, 2))
boxplot(pat.coef[,1], con.coef[,1], main="Intercepts",  #boxplots
        names=c("Patients", "Controls"))
boxplot(pat.coef[,2], con.coef[,2], main="Slopes",
        names=c("Patients", "Controls"))
par(old)

# calculate fitted values for patients and controls across the range of ages
# We first make a new prediction dataset, with ages running from 8 to 18 for both groups
# Then we use the predict command, with level=0 for fixed effects
pdata <- expand.grid(age=seq(8, 18, by=2), group=c("patient", "control"))
pdata$log.exercise <- predict(object=bm.lme.3, newdata=pdata, level=0)
pdata$exercise <- 2^pdata$log.exercise - 5/60
pdata

# Plot the fitted values "by hand" (Fig 9)
plot(pdata$age, pdata$exercise, type="n",
     xlab="Age (years)", ylab="Exercise (hours/week)")
points(pdata$age[1:6], pdata$exercise[1:6], type="b", pch=19, lwd=2)
points(pdata$age[7:12], pdata$exercise[7:12], type="b", pch=22, lty=2, lwd=2)
legend("topleft", c("Patients", "Controls"), pch=c(19, 22),
       lty=c(1,2), lwd=2, inset=0.05)

# E - statistical methods
# Time spent exercising was log (base 2) transformed due to right skewness and 
# heterogeneity of variances. A linear mixed effects model was used to account 
# for multiple measurements per subject. Fixed effects were added for age 
# (centered at age=8), group and an age*group interaction; a random intercept 
# per girl was added. For interpreation, predicted values were transformed back 
# to the original scale and plotted."





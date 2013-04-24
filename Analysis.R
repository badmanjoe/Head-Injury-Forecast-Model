


### TEEEEST

#  Adjust the following paths to be consistent with the working directory and file
#  structure of local computer                 

list.files()  #files in my working directory?  
objects()                                     
setwd("~/Documents/R Analysis/Head Injury Forecast Model/Data/")
load("HeadInjuryData.rda") # Baseline data

library(Zelig)
library(Hmisc)
library(fBasics)
library(gmodels)
library(vcd)
library(ltm)

FOLLOWUP.1 <- FOLLOWUP1
################################################################################
########                      Functions & Operations                  ##########
################################################################################

#The following functions are used to create new variables for the head injury paper

sro_fun <- function (x) #Function allows a quick recoding of outcome variables to be dichotomous
     {
     ifelse(x > 0, 1, 0)
     }

bs <- basicStats

cat_cat_assoc <- function(x, y) #Function runs bivariate correlation for categorical x categorical variables
     {
          x.table <- CrossTable(x, y, prop.r=FALSE, prop.c=TRUE,
                        prop.t=FALSE, prop.chisq=FALSE)
          x.effect <- assocstats(xtabs(~ y + x))
          print(x.table)
          print(x.effect)           
     }

cat_cont_assoc <- function(x, y) #Function runs bivariate correlation for categorical x continuous variables
     {
          x.test <- t.test(y ~ x)
          x.mean <- tapply(y, x, mean, na.rm=TRUE)
          x.sd <- tapply(y, x, sd, na.rm=TRUE)
          x.biserial <- biserial.cor(y, x, use = "complete.obs")

  
          print(x.test) 
          print(x.mean)
           print(x.sd)
          print(x.biserial)
     }

logit_simple <- function(x,y)
      {
     
          logit.simple <- glm(formula = y~x, data = FOLLOWUP.1, family = binomial("logit"))
          logit.simple.OR <- exp(cbind(OR = coef(logit.simple), confint(logit.simple)))
     
          print(logit.simple.OR)     
     }

logit_multivariate_socio <- function(x,y)
     {logit.multivariate.socio <- glm(formula = y ~ x + ageFirstOffense + age + ethnic + gender + globalSeverity + 
               alcdrugDependence, 
               data = FOLLOWUP.1, family = binomial("logit"))
        logit.multivariate.socio.tab <- exp(cbind(OR = coef(logit.multivariate.socio), confint(logit.multivariate.socio)))
        print(logit.multivariate.socio.tab)
     }

logit_multivariate_full <- function(x,y)
     {logit.multivariate.full <- glm(formula = y ~ x + ageFirstOffense + age + ethnic + gender + globalSeverity + 
               alcdrugDependence + violenceExp + peerDelinquency + enrolledSchool, 
               data = FOLLOWUP.1, family = binomial("logit"))
        logit.multivariate.full.tab <- exp(cbind(OR = coef(logit.multivariate.full), confint(logit.multivariate.full)))
        print(logit.multivariate.full.tab)
     }


poisson_simple <- function(x,y)
      {
     poisson.simple <- glm(formula = y ~ x, data = FOLLOWUP.1, family = poisson("log"))
     poisson.simple.IRR <- exp(cbind(IRR = coef(poisson.simple), confint(poisson.simple)))
          
     print(poisson.simple.IRR)
     }
     

poisson_multivariate_socio <- function(x,y)
      {
     poisson.multivariate.socio <- glm(formula = y ~ x + ageFirstOffense + age + ethnic + gender + globalSeverity + 
               alcdrugDependence, data = FOLLOWUP.1, family = poisson("log"))
     poisson.multivariate.socio.IRR <- exp(cbind(IRR = coef(poisson.multivariate.socio), confint(poisson.multivariate.socio)))
          
     print(poisson.multivariate.socio.IRR)
     }

poisson_multivariate_full <- function(x,y)
      {
     poisson.multivariate.socio <- glm(formula = y ~ x + ageFirstOffense + age + ethnic + gender + globalSeverity + 
               alcdrugDependence + violenceExp + peerDelinquency + enrolledSchool, data = FOLLOWUP.1, family = poisson("log"))
     poisson.multivariate.socio.IRR <- exp(cbind(IRR = coef(poisson.multivariate.socio), confint(poisson.multivariate.socio)))
          
     print(poisson.multivariate.socio.IRR)
     }




#Functions for doing subgroup analyses.  All these functions are the same as the other functions 
#but exclude gender

logit_simple_nog <- function(x,y)
      {
          logit.simple <- glm(formula = y~x, family = binomial("logit"))
          logit.simple.OR <- exp(cbind(OR = coef(logit.simple), confint(logit.simple)))
     
          print(logit.simple.OR)     
     }

logit_multivariate_socio_nog <- function(x,y)
     {logit.multivariate.socio.nog <- glm(formula = y ~ x + ageFirstOffense+ age + ethnic + globalSeverity + 
               alcdrugDependence, 
               family = binomial("logit"))
        logit.multivariate.socio.nog.tab <- exp(cbind(OR = coef(logit.multivariate.socio.nog), confint(logit.multivariate.socio.nog)))
        print(logit.multivariate.socio.nog.tab)
     }

logit_multivariate_full_nog <- function(x,y)
     {logit.multivariate.full <- glm(formula = y ~ x + ageFirstOffense + age + ethnic +  globalSeverity + 
               alcdrugDependence + violenceExp + peerDelinquency + enrolledSchool, family = binomial("logit"))
        logit.multivariate.full.tab <- exp(cbind(OR = coef(logit.multivariate.full), confint(logit.multivariate.full)))
        print(logit.multivariate.full.tab)
     }


poisson_simple_nog <- function(x,y)
      {
     poisson.simple <- glm(formula = y ~ x, data = FOLLOWUP.1, family = poisson("log"))
     poisson.simple.IRR <- exp(cbind(IRR = coef(poisson.simple), confint(poisson.simple)))
          
     print(poisson.simple.IRR)
     }
     

poisson_multivariate_socio_nog <- function(x,y)
      {
     poisson.multivariate.socio <- glm(formula = y ~ x + ageFirstOffense + age + ethnic + globalSeverity + 
     alcdrugDependence, family = poisson("log"))
     
     poisson.multivariate.socio.IRR <- exp(cbind(IRR = coef(poisson.multivariate.socio), confint(poisson.multivariate.socio)))
          
     print(poisson.multivariate.socio.IRR)
     }

poisson_multivariate_full_nog <- function(x,y)
      {
     poisson.multivariate.socio <- glm(formula = y ~ x + ageFirstOffense + age + ethnic + globalSeverity + 
               alcdrugDependence + violenceExp + peerDelinquency + enrolledSchool, family = poisson("log"))
     poisson.multivariate.socio.IRR <- exp(cbind(IRR = coef(poisson.multivariate.socio), confint(poisson.multivariate.socio)))
          
     print(poisson.multivariate.socio.IRR)
     }



################################################################################
########                      Functions & Operations                  ##########
################################################################################





bs <- basicStats  #handy function for summarizing variables

cat_cat_assoc <- function(x, y) #Function runs bivariate correlation for categorical x categorical variables
     {
          x.table <- CrossTable(x, y, prop.r=FALSE, prop.c=TRUE,
                        prop.t=FALSE, prop.chisq=FALSE)
          x.effect <- assocstats(xtabs(~ y + x))
          print(x.table)
          print(x.effect)           
     }

cat_cont_assoc <- function(x, y) #Function runs bivariate correlation for categorical x continuous variables
     {
          x.test <- t.test(y ~ x)
          x.mean <- tapply(y, x, mean, na.rm=TRUE)
          x.sd <- tapply(y, x, sd, na.rm=TRUE)
          x.biserial <- biserial.cor(y, x, use = "complete.obs")

  
          print(x.test) 
          print(x.mean)
           print(x.sd)
          print(x.biserial)
     }


#  Functions fo unadjusted and adjusted analysis - FULL SAMPLE  #

sro_fun <- function (x) #Function allows a quick recoding of outcome variables to be dichotomous
     {
     ifelse(x > 0, 1, 0)
     }

logit_simple <- function(x,y)
      {
     
          logit.simple <- glm(formula = y~x, data = FOLLOWUP.1, family = binomial("logit"))
          logit.simple.OR <- exp(cbind(OR = coef(logit.simple), confint(logit.simple)))
     
          print(logit.simple.OR)     
     }

logit_multivariate_socio <- function(x,y)
     {logit.multivariate.socio <- glm(formula = y ~ x + ageFirstOffense + age + ethnic + gender + globalSeverity + 
               alcdrugDependence, 
               data = FOLLOWUP.1, family = binomial("logit"))
        logit.multivariate.socio.tab <- exp(cbind(OR = coef(logit.multivariate.socio), confint(logit.multivariate.socio)))
        print(logit.multivariate.socio.tab)
     }

logit_multivariate_full <- function(x,y)
     {logit.multivariate.full <- glm(formula = y ~ x + ageFirstOffense + age + ethnic + gender + globalSeverity + 
               alcdrugDependence + violenceExp + peerDelinquency + enrolledSchool, 
               data = FOLLOWUP.1, family = binomial("logit"))
        logit.multivariate.full.tab <- exp(cbind(OR = coef(logit.multivariate.full), confint(logit.multivariate.full)))
        print(logit.multivariate.full.tab)
     }






poisson_simple <- function(x,y)
      {
     poisson.simple <- glm(formula = y ~ x, data = FOLLOWUP.1, family = poisson("log"))
     poisson.simple.IRR <- exp(cbind(IRR = coef(poisson.simple), confint(poisson.simple)))
          
     print(poisson.simple.IRR)
     }
     

poisson_multivariate_socio <- function(x,y)
      {
     poisson.multivariate.socio <- glm(formula = y ~ x + ageFirstOffense + age + ethnic + gender + globalSeverity + 
               alcdrugDependence, data = FOLLOWUP.1, family = poisson("log"))
     poisson.multivariate.socio.IRR <- exp(cbind(IRR = coef(poisson.multivariate.socio), confint(poisson.multivariate.socio)))
          
     print(poisson.multivariate.socio.IRR)
     }

poisson_multivariate_full <- function(x,y)
      {
     poisson.multivariate.socio <- glm(formula = y ~ x + ageFirstOffense + age + ethnic + gender + globalSeverity + 
               alcdrugDependence + violenceExp + peerDelinquency + enrolledSchool, data = FOLLOWUP.1, family = poisson("log"))
     poisson.multivariate.socio.IRR <- exp(cbind(IRR = coef(poisson.multivariate.socio), confint(poisson.multivariate.socio)))
          
     print(poisson.multivariate.socio.IRR)
     }




#  Functions fo unadjusted and adjusted analysis - GENDER  #
#  These functions are to be used with the gender-specific data files.  Multivariate models
#  exclude gender as the predictor variable.  




logit_simple_nog <- function(x,y)
      {
     
          logit.simple <- glm(formula = y~x, family = binomial("logit"))
          logit.simple.OR <- exp(cbind(OR = coef(logit.simple), confint(logit.simple)))
     
          print(logit.simple.OR)     
     }

logit_multivariate_socio_b <- function(x,y)
     {logit.multivariate.socio.b <- glm(formula = y ~ x + ageFirstOffense + age + ethnic + globalSeverity + 
               alcdrugDependence, data = FOLLOWUP.1.BOYS,
               family = binomial("logit"))
        logit.multivariate.socio.b.tab <- exp(cbind(OR = coef(logit.multivariate.socio.b), confint(logit.multivariate.socio.b)))
        print(logit.multivariate.socio.b.tab)
     }

logit_multivariate_full_b <- function(x,y)
     {logit.multivariate.full.b <- glm(formula = y ~ x + ageFirstOffense + age + ethnic +  globalSeverity + 
               alcdrugDependence + violenceExp + peerDelinquency + enrolledSchool, family = binomial("logit"))
        logit.multivariate.full.tab <- exp(cbind(OR = coef(logit.multivariate.full), confint(logit.multivariate.full)))
        print(logit.multivariate.full.tab)
     }






poisson_simple_nog <- function(x,y)
      {
     poisson.simple <- glm(formula = y ~ x, data = FOLLOWUP.1, family = poisson("log"))
     poisson.simple.IRR <- exp(cbind(IRR = coef(poisson.simple), confint(poisson.simple)))
          
     print(poisson.simple.IRR)
     }
     

poisson_multivariate_socio_nog <- function(x,y)
      {
     poisson.multivariate.socio <- glm(formula = y ~ x + ageFirstOffense + age + ethnic + globalSeverity + 
               alcdrugDependence, family = poisson("log"))
     poisson.multivariate.socio.IRR <- exp(cbind(IRR = coef(poisson.multivariate.socio), confint(poisson.multivariate.socio)))
          
     print(poisson.multivariate.socio.IRR)
     }

poisson_multivariate_full_nog <- function(x,y)
      {
     poisson.multivariate.socio <- glm(formula = y ~ x + ageFirstOffense + age + ethnic + globalSeverity + 
               alcdrugDependence + violenceExp + peerDelinquency + enrolledSchool, family = poisson("log"))
     poisson.multivariate.socio.IRR <- exp(cbind(IRR = coef(poisson.multivariate.socio), confint(poisson.multivariate.socio)))
          
     print(poisson.multivariate.socio.IRR)
     }


################################################################################
########        PREPARATION OF DATA FILES AND VARIABLES               ##########
################################################################################
#Construction of key outcome varialbes

FOLLOWUP.1$allAny.F1 <- with(FOLLOWUP.1, as.factor(sro_fun(allProportion.F1))) #Dichotomize offending behavior (y/n) based on proportion of full list
FOLLOWUP.1$nodrugAny.F1 <- with(FOLLOWUP.1, as.factor(sro_fun(nodrugProportion.F1))) #Dichotomize offending behavior (y/n) based on proportion of full list
FOLLOWUP.1$aggressiveAny.F1 <- with(FOLLOWUP.1, as.factor(sro_fun(aggressiveProportion.F1))) #Dichotomize offending behavior (y/n) based on proportion of full list 
FOLLOWUP.1$incomeAny.F1 <- with(FOLLOWUP.1, as.factor(sro_fun(incomeProportion.F1))) #Dichotomize offending behavior (y/n) based on proportion of full list

#Construction of independent variables
FOLLOWUP.1$headInjury <- as.numeric(FOLLOWUP.1$headInjury)
FOLLOWUP.1$headInjury <- with(FOLLOWUP.1, ifelse(headInjury == 2, 1, 0))


FOLLOWUP.1$alc_r <- with(FOLLOWUP.1, as.numeric(alcoholDep))
FOLLOWUP.1$drug_r <- with(FOLLOWUP.1, as.numeric(drugDep))
FOLLOWUP.1$alcdrugDependence <- with(FOLLOWUP.1, ifelse(alc_r ==2   | drug_r == 2, 1, 0))


#  This section creates the data files specific to Males and Females.  These scripts should be run
#  after processing the new variables to ensure they are included in the analyses.  

FOLLOWUP.1.BOYS <- subset(FOLLOWUP.1, gender == "(1) Male")
FOLLOWUP.1.GIRLS <- subset(FOLLOWUP.1, gender == "(2) Female")

FOLLOWUP.1.BOYS$gender <- ifelse(FOLLOWUP.1.BOYS$gender == "(2) Female", NA, 1)
FOLLOWUP.1.GIRLS$gender <- ifelse(FOLLOWUP.1.GIRLS$gender == "(1) Male", NA, 0)

dim(FOLLOWUP.1.BOYS)
dim(FOLLOWUP.1.GIRLS)




################################################################################
########                      SUMMARY STATS (TABLE 1)                 ##########
################################################################################

attach(FOLLOWUP.1)
bs(age)
summary(ethnic)/length(ethnic)

summary(gender)/length(gender)

bs(globalSeverity)

bs(ageFirstOffense)

bs(peerDelinquency)

summary(enrolledSchool)/length(enrolledSchool)

table(alcdrugDependence)/length(alcdrugDependence)
table(alcdrugDependence)

#  cat_cont_assoc <- function(x, y)
#  cat_cat_assoc <- function(x, y)

cat_cont_assoc(headInjury, age)
cat_cat_assoc(headInjury, ethnic)
cat_cat_assoc(headInjury, gender)
cat_cont_assoc(headInjury, globalSeverity)
cat_cat_assoc(headInjury, alcdrugDependence)
cat_cat_assoc(headInjury, enrolledSchool)


cat_cont_assoc(headInjury,ageFirstOffense)
cat_cont_assoc(headInjury, peerDelinquency)
cat_cont_assoc()
cat_cont_assoc()

cat_cat_assoc(headInjury, allAny.F1)

detach(FOLLOWUP.1)

################################################################################
########                      UNADJUSTED ANALYSIS                     ##########
################################################################################

#Full Sample - Incidence
with(FOLLOWUP.1, logit_simple(headInjury, allAny.F1))
with(FOLLOWUP.1, logit_simple(headInjury, nodrugAny.F1))
with(FOLLOWUP.1, logit_simple(headInjury, aggressiveAny.F1))
with(FOLLOWUP.1, logit_simple(headInjury, incomeAny.F1))

#Males - incidence
with(FOLLOWUP.1.BOYS, logit_simple_nog(headInjury, allAny.F1))
with(FOLLOWUP.1.BOYS, logit_simple_nog(headInjury, nodrugAny.F1))
with(FOLLOWUP.1.BOYS, logit_simple_nog(headInjury, aggressiveAny.F1))
with(FOLLOWUP.1.BOYS, logit_simple_nog(headInjury, incomeAny.F1))


#Females - incidence
with(FOLLOWUP.1.GIRLS, logit_simple_nog(headInjury, allAny.F1))
with(FOLLOWUP.1.GIRLS, logit_simple_nog(headInjury, nodrugAny.F1))
with(FOLLOWUP.1.GIRLS, logit_simple_nog(headInjury, aggressiveAny.F1))
with(FOLLOWUP.1.GIRLS, logit_simple_nog(headInjury, incomeAny.F1))

#Full sample - Frequency
with(FOLLOWUP.1, poisson_simple(headInjury, allFrequency.F1))
with(FOLLOWUP.1, poisson_simple(headInjury, nodrugFrequency.F1))
with(FOLLOWUP.1, poisson_simple(headInjury, aggressiveFrequency.F1))
with(FOLLOWUP.1, poisson_simple(headInjury, incomeFrequency.F1))

#Males - Frequency
with(FOLLOWUP.1.BOYS, poisson_simple_nog(headInjury, allFrequency.F1))
with(FOLLOWUP.1.BOYS, poisson_simple_nog(headInjury, nodrugFrequency.F1))
with(FOLLOWUP.1.BOYS, poisson_simple_nog(headInjury, aggressiveFrequency.F1))
with(FOLLOWUP.1.BOYS, poisson_simple_nog(headInjury, incomeFrequency.F1))

#Females - Frequency
with(FOLLOWUP.1.GIRLS, poisson_simple_nog(headInjury, allFrequency.F1))
with(FOLLOWUP.1.GIRLS, poisson_simple_nog(headInjury, nodrugFrequency.F1))
with(FOLLOWUP.1.GIRLS, poisson_simple_nog(headInjury, aggressiveFrequency.F1))
with(FOLLOWUP.1.GIRLS, poisson_simple_nog(headInjury, incomeFrequency.F1))


################################################################################
########                      ADJUSTED ANALYSIS -I                    ##########
################################################################################

#Full sample - incidence
with(FOLLOWUP.1, logit_multivariate_socio(headInjury, allAny.F1))
with(FOLLOWUP.1, logit_multivariate_socio(headInjury, nodrugAny.F1))
with(FOLLOWUP.1, logit_multivariate_socio(headInjury, aggressiveAny.F1))
with(FOLLOWUP.1, logit_multivariate_socio(headInjury, incomeAny.F1))

#Male - incidence
allAny.socio.boys <- glm(allAny.F1 ~ headInjury + ageFirstOffense + age + ethnic + globalSeverity + alcdrugDependence, 
               data = FOLLOWUP.1.BOYS,
               family = binomial("logit"))
        
     allAny.socio.boys.tab <- exp(cbind(OR = coef(allAny.socio.boys), confint(allAny.socio.boys)))
     print(allAny.socio.boys.tab)

nodrugAny.socio.boys <- glm(nodrugAny.F1 ~ headInjury + ageFirstOffense + age + ethnic + globalSeverity + alcdrugDependence, 
               data = FOLLOWUP.1.BOYS,
               family = binomial("logit"))
        
     nodrugAny.socio.boys.tab <- exp(cbind(OR = coef(nodrugAny.socio.boys), confint(nodrugAny.socio.boys)))
     print(nodrugAny.socio.boys.tab)

aggressiveAny.socio.boys <- glm(aggressiveAny.F1 ~ headInjury + ageFirstOffense + age + ethnic + globalSeverity + alcdrugDependence, 
               data = FOLLOWUP.1.BOYS,
               family = binomial("logit"))
        
     

aggressiveAny.socio.boys.tab <- exp(cbind(OR = coef(aggressiveAny.socio.boys), confint(aggressiveAny.socio.boys)))
     print(aggressiveAny.socio.boys.tab)


incomeAny.socio.boys <- glm(incomeAny.F1 ~ headInjury + ageFirstOffense + age + ethnic + globalSeverity + alcdrugDependence, 
               data = FOLLOWUP.1.BOYS,
               family = binomial("logit"))
        
     

incomeAny.socio.boys.tab <- exp(cbind(OR = coef(incomeAny.socio.boys), confint(incomeAny.socio.boys)))
     print(incomeAny.socio.boys.tab)



#Female - incidence

allAny.socio.girls <- glm(allAny.F1 ~ headInjury + ageFirstOffense + age + ethnic + globalSeverity + alcdrugDependence, 
               data = FOLLOWUP.1.GIRLS,
               family = binomial("logit"))
        
     allAny.socio.girls.tab <- exp(cbind(OR = coef(allAny.socio.girls), confint(allAny.socio.girls)))
     print(allAny.socio.girls.tab)

nodrugAny.socio.girls <- glm(nodrugAny.F1 ~ headInjury + ageFirstOffense + age + ethnic + globalSeverity + alcdrugDependence, 
               data = FOLLOWUP.1.GIRLS,
               family = binomial("logit"))
        
     nodrugAny.socio.girls.tab <- exp(cbind(OR = coef(nodrugAny.socio.girls), confint(nodrugAny.socio.girls)))
     print(nodrugAny.socio.girls.tab)

aggressiveAny.socio.girls <- glm(aggressiveAny.F1 ~ headInjury + ageFirstOffense + age + ethnic + globalSeverity + alcdrugDependence, 
               data = FOLLOWUP.1.GIRLS,
               family = binomial("logit"))
        
     

aggressiveAny.socio.girls.tab <- exp(cbind(OR = coef(aggressiveAny.socio.girls), confint(aggressiveAny.socio.girls)))
     print(aggressiveAny.socio.girls.tab)


incomeAny.socio.girls <- glm(incomeAny.F1 ~ headInjury + ageFirstOffense + age + ethnic + globalSeverity + alcdrugDependence, 
               data = FOLLOWUP.1.GIRLS,
               family = binomial("logit"))
        
     

incomeAny.socio.girls.tab <- exp(cbind(OR = coef(incomeAny.socio.girls), confint(incomeAny.socio.girls)))
     print(incomeAny.socio.girls.tab)



#Full sample - frequency
with(FOLLOWUP.1, poisson_multivariate_socio(headInjury, allFrequency.F1))
with(FOLLOWUP.1, poisson_multivariate_socio(headInjury, nodrugFrequency.F1))
with(FOLLOWUP.1, poisson_multivariate_socio(headInjury, aggressiveFrequency.F1))
with(FOLLOWUP.1, poisson_multivariate_socio(headInjury, incomeFrequency.F1))

#Male - frequency

allFrequency.socio.boys <- glm(allFrequency.F1 ~ headInjury + ageFirstOffense + age + ethnic + globalSeverity + alcdrugDependence, 
               data = FOLLOWUP.1.BOYS,
               family =  poisson("log"))
        
     allFrequency.socio.boys.tab <- exp(cbind(OR = coef(allFrequency.socio.boys), confint(allFrequency.socio.boys)))
     print(allFrequency.socio.boys.tab)

nodrugFrequency.socio.boys <- glm(nodrugFrequency.F1 ~ headInjury + ageFirstOffense + age + ethnic + globalSeverity + alcdrugDependence, 
               data = FOLLOWUP.1.BOYS,
               family = poisson("log"))
        
     nodrugFrequency.socio.boys.tab <- exp(cbind(OR = coef(nodrugFrequency.socio.boys), confint(nodrugFrequency.socio.boys)))
     print(nodrugFrequency.socio.boys.tab)

aggressiveFrequency.socio.boys <- glm(aggressiveFrequency.F1 ~ headInjury + ageFirstOffense + age + ethnic + globalSeverity + alcdrugDependence, 
               data = FOLLOWUP.1.BOYS,
               family = poisson("log"))
        
     

aggressiveFrequency.socio.boys.tab <- exp(cbind(OR = coef(aggressiveFrequency.socio.boys), confint(aggressiveFrequency.socio.boys)))
     print(aggressiveFrequency.socio.boys.tab)


incomeFrequency.socio.boys <- glm(incomeFrequency.F1 ~ headInjury + ageFirstOffense + age + ethnic + globalSeverity + alcdrugDependence, 
               data = FOLLOWUP.1.BOYS,
               family = poisson("log"))
        
     

incomeFrequency.socio.boys.tab <- exp(cbind(OR = coef(incomeFrequency.socio.boys), confint(incomeFrequency.socio.boys)))
     print(incomeFrequency.socio.boys.tab)

     

#Female - frequency
allFrequency.socio.girls <- glm(allFrequency.F1 ~ headInjury + ageFirstOffense + age + ethnic + globalSeverity + alcdrugDependence, 
               data = FOLLOWUP.1.GIRLS,
               family =  poisson("log"))
        
     allFrequency.socio.girls.tab <- exp(cbind(OR = coef(allFrequency.socio.girls), confint(allFrequency.socio.girls)))
     print(allFrequency.socio.girls.tab)

nodrugFrequency.socio.girls <- glm(nodrugFrequency.F1 ~ headInjury + ageFirstOffense + age + ethnic + globalSeverity + alcdrugDependence, 
               data = FOLLOWUP.1.GIRLS,
               family = poisson("log"))
        
     nodrugFrequency.socio.girls.tab <- exp(cbind(OR = coef(nodrugFrequency.socio.girls), confint(nodrugFrequency.socio.girls)))
     print(nodrugFrequency.socio.girls.tab)

aggressiveFrequency.socio.girls <- glm(aggressiveFrequency.F1 ~ headInjury + ageFirstOffense + age + ethnic + globalSeverity + alcdrugDependence, 
               data = FOLLOWUP.1.GIRLS,
               family = poisson("log"))
        
     

aggressiveFrequency.socio.girls.tab <- exp(cbind(OR = coef(aggressiveFrequency.socio.girls), confint(aggressiveFrequency.socio.girls)))
     print(aggressiveFrequency.socio.girls.tab)


incomeFrequency.socio.girls <- glm(incomeFrequency.F1 ~ headInjury + ageFirstOffense + age + ethnic + globalSeverity + alcdrugDependence, 
               data = FOLLOWUP.1.GIRLS,
               family = poisson("log"))
        
     

incomeFrequency.socio.girls.tab <- exp(cbind(OR = coef(incomeFrequency.socio.girls), confint(incomeFrequency.socio.girls)))
     print(incomeFrequency.socio.girls.tab)

     


################################################################################
########                      ADJUSTED ANALYSIS -II                   ##########
################################################################################

#Full sample - incidence     
with(FOLLOWUP.1, logit_multivariate_full(headInjury, allAny.F1))
with(FOLLOWUP.1, logit_multivariate_full(headInjury, nodrugAny.F1))
with(FOLLOWUP.1, logit_multivariate_full(headInjury, aggressiveAny.F1))
with(FOLLOWUP.1, logit_multivariate_full(headInjury, incomeAny.F1))


#Male - incidence
allAny.full.boys <- glm(allAny.F1 ~  headInjury + ageFirstOffense + age + ethnic +  globalSeverity + 
               alcdrugDependence + violenceExp + peerDelinquency + enrolledSchool, 
               data = FOLLOWUP.1.BOYS,
               family = binomial("logit"))
        
     allAny.full.boys.tab <- exp(cbind(OR = coef(allAny.full.boys), confint(allAny.full.boys)))
     print(allAny.full.boys.tab)

nodrugAny.full.boys <- glm(nodrugAny.F1 ~ headInjury + ageFirstOffense + age + ethnic +  globalSeverity + 
               alcdrugDependence + violenceExp + peerDelinquency + enrolledSchool, 
               data = FOLLOWUP.1.BOYS,
               family = binomial("logit"))
        
     nodrugAny.full.boys.tab <- exp(cbind(OR = coef(nodrugAny.full.boys), confint(nodrugAny.full.boys)))
     print(nodrugAny.full.boys.tab)

aggressiveAny.full.boys <- glm(aggressiveAny.F1 ~ headInjury + ageFirstOffense + age + ethnic +  globalSeverity + 
               alcdrugDependence + violenceExp + peerDelinquency + enrolledSchool, 
               data = FOLLOWUP.1.BOYS,
               family = binomial("logit"))
        
     

aggressiveAny.full.boys.tab <- exp(cbind(OR = coef(aggressiveAny.full.boys), confint(aggressiveAny.full.boys)))
     print(aggressiveAny.full.boys.tab)


incomeAny.full.boys <- glm(incomeAny.F1 ~ headInjury + ageFirstOffense + age + ethnic +  globalSeverity + 
               alcdrugDependence + violenceExp + peerDelinquency + enrolledSchool, 
               data = FOLLOWUP.1.BOYS,
               family = binomial("logit"))
        
     

incomeAny.full.boys.tab <- exp(cbind(OR = coef(incomeAny.full.boys), confint(incomeAny.full.boys)))
     print(incomeAny.full.boys.tab)

     
    

#Female - incidence


allAny.full.girls <- glm(allAny.F1 ~  headInjury + ageFirstOffense + age + ethnic +  globalSeverity + 
               alcdrugDependence + violenceExp + peerDelinquency + enrolledSchool, 
               data = FOLLOWUP.1.GIRLS,
               family = binomial("logit"))
        
     allAny.full.girls.tab <- exp(cbind(OR = coef(allAny.full.girls), confint(allAny.full.girls)))
     print(allAny.full.girls.tab)

nodrugAny.full.girls <- glm(nodrugAny.F1 ~ headInjury + ageFirstOffense + age + ethnic +  globalSeverity + 
               alcdrugDependence + violenceExp + peerDelinquency + enrolledSchool, 
               data = FOLLOWUP.1.GIRLS,
               family = binomial("logit"))
        
     nodrugAny.full.girls.tab <- exp(cbind(OR = coef(nodrugAny.full.girls), confint(nodrugAny.full.girls)))
     print(nodrugAny.full.girls.tab)

aggressiveAny.full.girls <- glm(aggressiveAny.F1 ~ headInjury + ageFirstOffense + age + ethnic +  globalSeverity + 
               alcdrugDependence + violenceExp + peerDelinquency + enrolledSchool, 
               data = FOLLOWUP.1.GIRLS,
               family = binomial("logit"))
        
     

aggressiveAny.full.girls.tab <- exp(cbind(OR = coef(aggressiveAny.full.girls), confint(aggressiveAny.full.girls)))
     print(aggressiveAny.full.girls.tab)


incomeAny.full.girls <- glm(incomeAny.F1 ~ headInjury + ageFirstOffense + age + ethnic +  globalSeverity + 
               alcdrugDependence + violenceExp + peerDelinquency + enrolledSchool, 
               data = FOLLOWUP.1.GIRLS,
               family = binomial("logit"))
        
     

incomeAny.full.girls.tab <- exp(cbind(OR = coef(incomeAny.full.girls), confint(incomeAny.full.girls)))
     print(incomeAny.full.girls.tab)




#Full sample - frequency
with(FOLLOWUP.1, poisson_multivariate_full(headInjury, allFrequency.F1))
with(FOLLOWUP.1, poisson_multivariate_full(headInjury, nodrugFrequency.F1))
with(FOLLOWUP.1, poisson_multivariate_full(headInjury, aggressiveFrequency.F1))
with(FOLLOWUP.1, poisson_multivariate_full(headInjury, incomeFrequency.F1))

allFrequency.full.boys <- glm(allFrequency.F1 ~  headInjury + ageFirstOffense + age + ethnic +  globalSeverity + 
               alcdrugDependence + violenceExp + peerDelinquency + enrolledSchool, 
               data = FOLLOWUP.1.BOYS,
               family = poisson("log"))
        
     allFrequency.full.boys.tab <- exp(cbind(OR = coef(allFrequency.full.boys), confint(allFrequency.full.boys)))
     print(allFrequency.full.boys.tab)

nodrugFrequency.full.boys <- glm(nodrugFrequency.F1 ~ headInjury + ageFirstOffense + age + ethnic +  globalSeverity + 
               alcdrugDependence + violenceExp + peerDelinquency + enrolledSchool, 
               data = FOLLOWUP.1.BOYS,
               family = poisson("log"))
        
     nodrugFrequency.full.boys.tab <- exp(cbind(OR = coef(nodrugFrequency.full.boys), confint(nodrugFrequency.full.boys)))
     print(nodrugFrequency.full.boys.tab)

aggressiveFrequency.full.boys <- glm(aggressiveFrequency.F1 ~ headInjury + ageFirstOffense + age + ethnic +  globalSeverity + 
               alcdrugDependence + violenceExp + peerDelinquency + enrolledSchool, 
               data = FOLLOWUP.1.BOYS,
               family = poisson("log"))
        
     

aggressiveFrequency.full.boys.tab <- exp(cbind(OR = coef(aggressiveFrequency.full.boys), confint(aggressiveFrequency.full.boys)))
     print(aggressiveFrequency.full.boys.tab)


incomeFrequency.full.boys <- glm(incomeFrequency.F1 ~ headInjury + ageFirstOffense + age + ethnic +  globalSeverity + 
               alcdrugDependence + violenceExp + peerDelinquency + enrolledSchool, 
               data = FOLLOWUP.1.BOYS,
               family = poisson("log"))
        
     

incomeFrequency.full.boys.tab <- exp(cbind(OR = coef(incomeFrequency.full.boys), confint(incomeFrequency.full.boys)))
     print(incomeFrequency.full.boys.tab)

     
    

#Female - incidence


allFrequency.full.girls <- glm(allFrequency.F1 ~  headInjury + ageFirstOffense + age + ethnic +  globalSeverity + 
               alcdrugDependence + violenceExp + peerDelinquency + enrolledSchool, 
               data = FOLLOWUP.1.GIRLS,
               family = poisson("log"))
        
     allFrequency.full.girls.tab <- exp(cbind(OR = coef(allFrequency.full.girls), confint(allFrequency.full.girls)))
     print(allFrequency.full.girls.tab)

nodrugFrequency.full.girls <- glm(nodrugFrequency.F1 ~ headInjury + ageFirstOffense + age + ethnic +  globalSeverity + 
               alcdrugDependence + violenceExp + peerDelinquency + enrolledSchool, 
               data = FOLLOWUP.1.GIRLS,
               family = poisson("log"))
        
     nodrugFrequency.full.girls.tab <- exp(cbind(OR = coef(nodrugFrequency.full.girls), confint(nodrugFrequency.full.girls)))
     print(nodrugFrequency.full.girls.tab)

aggressiveFrequency.full.girls <- glm(aggressiveFrequency.F1 ~ headInjury + ageFirstOffense + age + ethnic +  globalSeverity + 
               alcdrugDependence + violenceExp + peerDelinquency + enrolledSchool, 
               data = FOLLOWUP.1.GIRLS,
               family = poisson("log"))
        
     

aggressiveFrequency.full.girls.tab <- exp(cbind(OR = coef(aggressiveFrequency.full.girls), confint(aggressiveFrequency.full.girls)))
     print(aggressiveFrequency.full.girls.tab)


incomeFrequency.full.girls <- glm(incomeFrequency.F1 ~ headInjury + ageFirstOffense + age + ethnic +  globalSeverity + 
               alcdrugDependence + violenceExp + peerDelinquency + enrolledSchool, 
               data = FOLLOWUP.1.GIRLS,
               family = poisson("log"))
        
     

incomeFrequency.full.girls.tab <- exp(cbind(OR = coef(incomeFrequency.full.girls), confint(incomeFrequency.full.girls)))
     print(incomeFrequency.full.girls.tab)


################################################################################
########                ADJUSTED ANALYSIS WITH ZELIG                  ##########
################################################################################

attach(FOLLOWUP.1)
any.inc.1 <- na.omit(data.frame(headInjury, allAny.F1))


all.logit.z <- zelig(allAny.F1 ~ headInjury, 
     data = any.inc.1, model = "logit", cite = FALSE)
     
x.no <-setx(all.logit.z, headInjury = 0)
x.yes <-setx(all.logit.z, headInjury = 1)

s.out <- sim(all.logit.z, x = x.no, x1 = x.yes)

























anyOffense.socioDem <-zelig(offenseFull.di.F1) ~ offenseFull.B + headInjury + age + ethnic + gender,  
          model = "logit",
          data = FOLLOWUP.1,
          cite = FALSE)    

anyOffense.clinical <-zelig(offenseFull.di.F1 ~ offenseFull.B + headInjury + age + ethnic + gender + globalSeverity +
          alcoholDep + drugDep,  
          model = "logit",
          data = FOLLOWUP.1,
          cite = FALSE)   
                     
anyOffense.clinical <-zelig(offenseFull.di.F1 ~ offenseFull.B + headInjury + age + ethnic + gender + globalSeverity +
          alcoholDep + drugDep + neighborhoodCondition + peerDelinquency + caringAdult,  
          model = "logit",
          data = FOLLOWUP.1,
          cite = FALSE)   



                    


                     
                     
                     
                     
                 














demog.only <- zelig(followup.1 ~ S0AGE + ethnic + S0SGEND,
model = "logit", data = PATH, cite=FALSE)

     coef(summary(demog.only))
     confint(demog.only)

demog.clinical <- zelig(followup.1 ~ S0AGE + as.factor(S0ETHN_R) + S0SGEND + S0HEADIN,
model = "logit", data = full.DATA.COMPLETE, cite=FALSE)



                     
#The following expressions transform the betas to OR's
demog.clinical.tab1 <- coef(summary(demog.clinical))
demog.clinical.tab1[, "Estimate"] <- exp(coef(demog.clinical))
print(demog.clinical.tab1)
                     
#Create a data set with predictors set at desired levels

#The following isn't working
predDat <- with(full.DATA.COMPLETE, 
                    expand.grid(S0AGE = c(14,17), 
                                S0ETHN_R = 1,
                                S0SGEND = 1,
                                S0HEADIN = 1))

cbind(predDAT, predict(demog.clinical, type = "response",
                      se.fit = TRUE, interval = "confidence",
                      newdata = predDat))

x.yes <- setx(demog.clinical, S0HEADIN = 1)
x.no <- setx(demog.clinical, S0HEADIN = 2)
s.out <- sim(demog.clinical, x=x.yes, x1 = x.no)
                     
summary(demog.clinical)



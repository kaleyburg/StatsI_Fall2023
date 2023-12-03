#####problem set 4###


# Detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()




# Load libraries and install packages



install.packages(car)
library(car)
data(Prestige)
help(Prestige)





install.packages('stargazer')
library(stargazer)
# Set working directory for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

install.packages('tidyverse')
library('tidyverse')


#quesiton 1 ####




#making variable 'type' into a binary prestige variable

Prestige['type']

head(Prestige$type)

Prestige$professional <- ifelse(Prestige$type == 'prof', 1 , 0)

Prestige[, c("type", "professional")]

stargazer(as.data.frame.matrix(table(Prestige[, c("type", "professional")])), title = "Table of professional and type", summary = FALSE)
table(Prestige[, c("type", "professional")])


Prestige['professional']


#linear reg with prestige as an outcome and income, 
#professional, and the interaction of the two as predictors 



# Fit model


model_int <- lm(prestige~income + 
                  professional + 
                  income*professional, data=Prestige)

summary(model_int)
plot(model_int)


stargazer(model_int)


##ggplot(Prestige, aes(imwbcnt, euftf_re, group = edu_cat)) +
  ##geom_point(aes(colour = edu_cat)) + # Add points
  ##geom_line(data = df_na, aes(y = model1$fitted.values, colour = edu_cat)) # Add regression lines



##plotcoefs <- model_int$coefficients
##abline(plotcoefs[1] + plotcoefs[2], plotcoefs[3], col=2, lwd=2)


#prediction equation



# prestige  = 21.14 + 0.003*income + 37.78*professional 
# - 0.002*income*professional




#(d) Interpret the coefficient for income.
    #for those in a blue or white collar profession,
    #with every 1 unit increase in income, prestige score
    #increases, on average, by 0.003 units, while holding
    #all other variables in the model constant




#Interpret the coefficient for professional
    #For poor individuals in a professional profession, the
    #average prestige score is 37.78 points higher than for
    #poor individuals in a blue or white collar profession




min(Prestige$income)



max(Prestige$income)



plot(model_int)



summary(model_int$coefficients)



#What is the effect of a $1,000 increase in income on 
#prestige score for professional occupations? 
#In other words, we are interested in the marginal effect of 
#income when the variable professional takes the value of 
#1. Calculate the change in ˆy associated
#with a $1,000 increase in income based on your answer for (c)





# prestige  = 21.14 + 0.003*income + 37.78*professional 
# - 0.002*income*professional




# prestige  = 21.14 + 0.003*0 + -37.78*1 
# - 0.002*0*1




zerodol <- 21.1422589 + 0.0031709*0 + 37.7812800*1 - 0.0023257*0*1
zerodol




# prestige  = 21.14 + 0.003*(1000) + 37.7812800*(1)
# - 0.002*1000*1



thousdol <- 21.1422589 + 0.0031709*1000 + 37.7812800*1 - 0.0023257*1000*1
thousdol




thousdol - zerodol




marginaleffectdol <- thousdol - zerodol



marginaleffectdol



#marg effect is 0.8452



#What is the effect of changing one’s occupations from 
#non-professional to professional when her income is $6,000? 
#We are interested in the marginal effect of professional
#jobs when the variable income takes the value of 6, 000. 
#Calculate the change in y based on your answer for (c).




nonprofmarg <- 21.1422589 + 0.0031709*6000 + 37.7812800*0 - 0.0023257*6000*0


nonprofmarg




profmarg <- 21.1422589 + 0.0031709*6000 + 37.7812800*1 - 0.0023257*6000*1



profmarg



margeffectprof <- profmarg- nonprofmarg


margeffectprof


#marg effect is 23.82708


# prestige  = 21.14 + 0.003*income + 37.78*professional


# - 0.002*income*professional



###quesiton 2 ###




#beta/se = t value

# n is number of obs

# k is number of variables



TS1 <- (0.042)/(0.016)
TS2 <- (0.042)/(0.013)
TSconst <- (0.302)/(0.011)

n = 131
k = 2

pval1 <- 2*pt(abs(TS1), n-k, lower.tail = F)
pval2 <- 2*pt(abs(TS2), n-k, lower.tail = F)
pvalconst <- 2*pt(abs(TSconst), n-k, lower.tail = F)
 
print(pval1)
print(pval2)
print(pvalconst)

TS1
TS2
TSconst



#Use the results from a linear regression to determine 
#whether having these yard signs in a precinct affects vote share 
#(e.g., conduct a hypothesis test with α = .05).




#this is for pval1




#The p-value for this beta coefficient is 0.009711646, which is
#less than 0.05. Therefore, we can reject the null hypothesis
#that states that there is no relationship between having a
#yard sign in a precinct and the vote share of Ken Cuccinell
#this beta indicates that having a yard sign in the precinct
#compared to not having a yard sign in the precinct is, on
#average, associated with a 0.042 increase in vote share 
#for Cuccinelli




#Use the results to determine whether being next to precincts 
#with these yard signs affects vote share (e.g., conduct a 
#hypothesis test with α = .05).





#The p-value for this beta coefficient is 0.001566685, which is
#less than 0.05. Therefore, we can reject the null hypothesis
#that states that there is no relationship between having a
#yard sign next to a precinct and the vote share of Ken Cuccinell
#this beta indicates that having a yard sign next to a precinct
#compared to not having a yard sign next to a precinct is, on
#average, associated with a 0.042 increase in vote share 
#for Cuccinelli






#c) Interpret the coefficient for the constant term substantively




#The coefficient for the constant term is 0.302. The 
#p-value associated with this is 1.013866e-55. Therefore,
#the analysis for the constant term is as follows:
#when there are no yard signs placed in or next to precints,
#the vote share value is 0.302 units. This value is significant
#at the 0.001 level, as the p-value is significantly small.
#we can therefore reject the null hypothesis that states that
#the constant value is 0.




sprintf("%.200f",1.013866e-55 )





#(d) Evaluate the model fit for this regression. 
#What does this tell us about the importance
#of yard signs versus other factors that are not modeled?
 


 
R2=0.094

ftest <- (R2/(k-1)) / ((1 - R2)/(n-k))

df1 <- k-1
df2 <- n - k - 1

fpval <- df(ftest, df1, df2)


fpval

ftest
























#things from slides that i used to write my formulas

# Load data
load("C:/Users/kburg/OneDrive/Documents/GitHub/StatsI_Fall2023/datasets/anes.Rdata")

# Subset data to relevant variables
anes <- anes[complete.cases(anes$caseid), ]

# Select relevant variables
reg_DF <- anes[, c("white", "female", "age", "partyid", "bushiraq")]

# Estimate regression by hand
lm_by_hand <- function(inputDF, covariates, outcome) {
  
  # Create matrices
  X <- as.matrix(cbind(rep(1, nrow(inputDF)), inputDF[, covariates]))
  Y <- inputDF[, outcome]
  
  #slide 23 and pg 72 af text for formula
  
  # Calculate betas
  betas <- solve(t(X) %*% X) %*% t(X) %*% Y
  rownames(betas)[1] <- "Intercept"
  
  n <- nrow(inputDF)
  k <- ncol(X)
  
  # Calculate SEs for betas
  sigma_squared <- sum((Y - X %*% betas)^2) / (n - k)
  
  # Create variance-covariance matrix for betas
  var_covar_mat <- sigma_squared * solve(t(X) %*% X)
  
  # Standard errors for coefficient estimates
  SEs <- sqrt(diag(var_covar_mat))
  
}

# Example usage:
# Replace 'my_data', 'c("covariate1", "covariate2")', and 'outcome_var' with your actual data and variable names.
result <- lm_by_hand(my_data, c("covariate1", "covariate2"), "outcome_var")
print(result)

383-5




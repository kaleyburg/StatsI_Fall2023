#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# read in data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2023/main/datasets/incumbents_subset.csv")

#voteshare = incumbent vote share/incumbent electoral success
#difflog = difference in campaign spending between incumbent and
  #challenger
#presvote = vote share of pres candidate of the incumbent party



### exercise 1 ### ------

#regression where outcome variable is voteshare
#and explanatory variable is difflog



vote_diff <- lm(voteshare ~ difflog, data=inc.sub)






#plot

png(file = "difflog_on_voteshare.png")
plot(inc.sub$difflog, inc.sub$voteshare, pch=16,col = c("black"), 
     main = paste(
       strwrap(
         "Relationship Between Campaign Spending and Incumbent Vote Share",
         width = 50
       ),
       collapse = "\n"
     ),
     xlab="Difference in Campaign Spending Between Incumbent 
     and Challenger", ylab="Incumbent Vote Share")
abline(vote_diff)
dev.off()



#residuals in seperate object



ex1resids <- vote_diff$residuals

print(ex1resids)

print(summary(vote_diff))



#pred equation:

# y = 0.579 + 0.042b
#incumbent elect. success =  0.579031 + 0.041666*difference in
#campaign spending

###exercise 2 #### -----

#regression where outcome is presvote and explain is difflog

pres_diff <- lm(presvote ~ difflog, data=inc.sub)
#remove the summary part

#plot



png(file = "difflog_on_pres_diff.png")
plot(inc.sub$difflog, inc.sub$presvote, pch=16,col = c("black"), 
     main=paste(
       strwrap(
         "Relationship Between Campaign Spending and Presidential Vote Share",
         width = 50
       ),
       collapse = "\n"
     ),
     xlab="Difference in Campaign Spending Between Incumbent 
     and Challenger", ylab="Presidential Candidate Vote Share")
abline(pres_diff)
dev.off()


#residuals in seperate object

ex2resids <- pres_diff$residuals
plot(ex2resids)
print(summary(pres_diff))



#pred equation:

# y = 0.508 + 0.024b
#presidential candidate vote share =  
#0.507585 + 0.023837*difference in
#campaign spending



###exercise 3 #### -----



#regression where outcome is voteshare and explain is presvote



vote_pres <- lm(voteshare ~ presvote, data=inc.sub)



#plot



png(file = "presvote_on_voteshare.png")
plot(inc.sub$presvote, inc.sub$voteshare, pch=16,col = c("black"), 
     main = paste(
       strwrap(
         "Relationship Between Incumbent and Presidential Vote Share",
         width = 50
       ),
       collapse = "\n"
     ),
     xlab="Presidential Vote Share", ylab="Incumbent Vote Share")
abline(vote_pres)
dev.off()



#residuals in seperate object



ex3resids <- vote_pres$residuals
print(ex1resids)
print(summary(vote_pres))



#pred equation:



# y = 0.441 + 0.388b


#vote share of pres candidate = 0.441 + 0.388*incumbent electoral
#success



####exercise 4 #### ------ 
 


#regression with ex1 resids as outcome and ex2 resids as explain



resid_reg <- lm(ex1resids ~ ex2resids)


print(resid_reg)


#plot 



png(file = "resids_regression.png")
plot(ex2resids, ex1resids, pch=16,col = c("black"), 
     main = paste(
       strwrap(
         "Relationship Between Voteshare-Difflog and Presvote-Difflog Regressions",
         width = 50
       ),
       collapse = "\n"
     ),
     xlab="Difflog and Presvote Residuals", ylab="Difflog and Voteshare Residuals")
abline(resid_reg)
dev.off()


#prediction equation



print(summary(resid_reg))



sprintf("%.20f",-5.934e-18)



sprintf("%.20f", 2.569e-01)




#y = -0.000 + 0.257b


#residuals for difflog on voteshare(reg1) = -0.000 + 0.257*
  #residuals for difflog on presvote(reg2)

#a 1 point increase in error on the second regression,
#is associated with a 0.257 increase on the first regression





####exercise 5 #### -------



#regression with outcome - voteshare


#and explans as difflog and presvote



multireg <- lm(voteshare ~ presvote + difflog, data=inc.sub)



print(multireg)



#prediction equation:



# y = -0.449 + 0.257b1 + 0.355b2



print(multireg$coefficients)

summary(multireg)
summary(resid_reg)

print(resid_reg$coefficients)

plot(ex1resids)


plot(ex2resids)



print(summary(resid_reg))



print(summary(multireg))




#residuals saved from q1 are how far away
#are the unexplained variation left between voteshare and difflog
# so its the leftover unexplianed variation of difflog
#on various outcomes
#in model 4, interpretation: unexplained variation in 
#model 1 is linear associated with unexplained variation in 
#model 2
#in multiple linear regression we are calculating the 
#partial effect
#amount of covariance between outcome voteshare and presvote
#that is not explained by difflog
#the amount of covariation between outcome voteshare and presvote
#that is not explained by difflog


#---
sum(ex2resids)



sprintf("%.20f",1.029157e-14)


sum(ex1resids)


sprintf("%.20f",-1.682552e-14)



sqrt(277766746)


###stargazer###

library("stargazer")
stargazer(vote_diff)
stargazer(pres_diff)
stargazer(vote_pres)
stargazer(resid_reg)
stargazer(multireg)
stargazer(multireg, keep.stat = "all")

stargazer(multireg, type = "text")
stargazer(resid_reg, type = "text", keep.stat = "ser")

?stargazer
?summmary


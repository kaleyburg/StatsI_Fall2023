#####################
# load libraries
# set wd
# clear global .envir
#####################

getwd()
setwd("C:/Users/kburg/OneDrive/Documents/GitHub/StatsI_Fall2023/problemSets/PS02/my_answers_kb")



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
#pkgTest <- function(pkg){
# new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
#  if (length(new.pkg)) 
#    install.packages(new.pkg,  dependencies = TRUE)
#  sapply(pkg,  require,  character.only = TRUE)
#}
# here is where you load any necessary packages

###### Question 1: Political Science ######

polscidata <- matrix(c(14, 7, 6, 7, 7, 1), nrow = 2, ncol = 3)
colnames(polscidata) <- c("Not Stopped", "Bribe requested", " Stopped/given warning")
rownames(polscidata) <- c("Upper class", "Lower class")
polscidata


#ok this was kinda useless, having it as a dataframe
#makes the work harder so im leaving it in a matrix
#but anyway heres the code to make it a dataframe 
polscidf <- as.data.frame(polscidata)
colnames(polscidf) <- c("Not Stopped", "Bribe requested", " Stopped/given warning")
rownames(polscidf) <- c("Upper class", "Lower class")
polscidf
#this is kinda useless for my analysis
prop.table(polscidf)
prop.table(polscidata)
#they're the same though
# Each value interpreted as estimated probabilities of two specific
# values of each of the variables co-occurring together

#now i add the sums
tablewmarg <- addmargins(polscidata)
tablewmarg
#we can use this to check, and then do the math by hand
14/42
6/42

tablewmarg
tablewmarg[10]
#i was seeing how to subset this

#ok using this to get the expected values so that i can
#notice patterns and make my for loops
#https://www.tutorialspoint.com/how-to-find-the-sum-of-rows-columns-and-total-in-a-matrix-in-r#:~:text=To%20find%20the%20sum%20of%20row%2C%20columns%2C%20and%20total%20in,%2C%20chi%E2%88%92square%20testing%20etc.
#well this didnt work at all
#####ignore ########
?rowsum
rowSums(polscidata)
colSums(polscidata)[1]
colSums(polscidata)[2]
colSums(polscidata)[3]
rowSums(polscidata)[1]
rowSums(polscidata)[2]

sum(polscidata)

(tablewmarg[10]/tablewmarg[12])*tablewmarg[3]
(14/42)*21
(tablewmarg[11]/tablewmarg[12])*tablewmarg[3]

(tablewmarg[10]/tablewmarg[12])*tablewmarg[6]
(tablewmarg[11]/tablewmarg[12])*tablewmarg[6]

(tablewmarg[10]/tablewmarg[12])*tablewmarg[9]
(tablewmarg[11]/tablewmarg[12])*tablewmarg[9]
(7/42)*21


sum(polscidata[1,])
#####end ignore #######

#making an empty matrix for new values https://www.geeksforgeeks.org/how-to-create-an-empty-matrix-in-r/
#and I have the same labels as the original
#also having the comma at the start of the matrix
#code gives me a warning
#so i got rid of it and it worked
{expected <- matrix(nrow = 2, ncol = 3)
colnames(expected) <- c("Not Stopped", "Bribe requested", " Stopped/given warning")
rownames(expected) <- c("Upper class", "Lower class")}

#checking the length to use in my for loop
#but I didn't end up using it
length(polscidata)

#seeing if this sums what I want it to (first row)
#to use in my for loop
sum(polscidata[1,])

#this worked!!
#so what i did was iterate through value of the matrix
#and create the rowsums, colsums, and totalsums
#and then i add this to the empty matrix i made earlier
for (i in 1:nrow(polscidata)) {
   rowsum <- sum(polscidata[i, ]) 
for (j in 1:ncol(polscidata)) {
  colsum <- sum(polscidata [ ,j])
   totalsum <- sum(polscidata)
   expected[i,j] <- (rowsum/totalsum)*colsum
}
}

#and we can print it to see 
expected
#copying other matrix to be called observed
observed <- polscidata

#now look at both
observed
expected

#this all looks good
#ok making X^2 value now
#formula is sum(((observed-expected)^2)/expected)
#making sure that length returns 6
length(observed)
#making a new variable that starts at 0 to use in my for loop
X2 <- 0
for (i in 1:length(observed)) {
  value <- sum(((observed[i] - expected[i])^2)/expected[i])
  X2 <- X2 + value
}
X2

#what this code does is it iterates through my matrices and sums
#together the (obs-exp)^2 and then divides this by the exp
#and then after each iteration it adds this value to my x2
#so that X2 ends up being the total sum


#then I used code from class for p value 
#pchisq(x^2, df = (rows-1)(columns-1),lower.tail=FALSE)
#df = (rows - 1)(columns - 1)

#first made df variable
df <- (nrow(observed) - 1)*(ncol(observed) - 1)

#making sure its 2
df

#now use the code from class
pvalue <- pchisq(X2, df,lower.tail=FALSE)

#checking it
pvalue
#Now calculate the p-value from the test statistic you just 
#created (in R).2 What do you conclude if α = 0.1

#if alpha = 0.1 we fail to reject the null hypothesis because
#our p value of 0.15 is greater than the alpha value
#of 0.1
#what this means is that we fail to reject the null
#hypothesis that solicitation of bribe and class are
#independent

#Calculate the standardized residuals
#for each cell and put them in the table below.


###ignore###
{stdresid <- matrix(nrow = 2, ncol = 3)
  colnames(stdresid) <- c("Not Stopped", "Bribe requested", " Stopped/given warning")
  rownames(stdresid) <- c("Upper class", "Lower class")}


rowsum <- rowSums(observed)
rowsum
colsum <- colSums(observed)
colsum
totalsum
#end ignore####
#######ignore########


  for (j in 1:nrow(observed)) {
    rowsum <- sum(observed [j, ])
      for (k in 1:ncol(observed)) {
        colsum <- sum(observed [, k])
      }
    totalsum <- sum(observed)
    denominator <- (expected[i]
    *(1 - (expected[i]*(rowsum[j] - totalsum)))
    *(1 - (expected[i]*(colsum[k] - totalsum))))
    stdresid <- numerator/denominator
  }


######end ignore######

#now the last part is finding the standardized residuals
#ok so the numerator of the equation is just each observed-expected
#so i made a new matrix with these values
unstandresid <- observed - expected
unstandresid
numerator <- unstandresid

#then i made an empty matrix called denominator
{denominator <- matrix(0, nrow = nrow(observed), ncol = ncol(observed))
colnames(denominator) <- c("Not Stopped", "Bribe requested", " Stopped/given warning")
rownames(denominator) <- c("Upper class", "Lower class")}

#and now i combine the old for loop that i used to get the expected
#values to get the matrix filled with denominators
for (i in 1:nrow(observed)) {
  rowsum <- sum(observed[i, ]) 
  for (j in 1:ncol(observed)) {
    colsum <- sum(observed [ ,j])
    totalsum <- sum(observed)
    denominator[i,j] <- ((1-(rowsum/totalsum))*
      (1-(colsum/totalsum))*
      (rowsum/totalsum)*colsum)^(1/2)
}
}   
#and now i have 2 matrices, 1 with numerator and other with 
#denominator
#so i can put them together into a new matrix by dividing
#numerator by denominator
numerator
denominator

#heres this new matrix
stdresids <- numerator/denominator      
stdresids

#and then I check it with the chisq.test function
#and I get the same thing
chitest <- chisq.test(observed)
chitest$stdres  





####Question 2 ####

#used code from last time but changed to read.csv because
#i saw that its a csv file not a table
econdf <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv", header=T)
econdf

#plot just to see
plot(econdf$reserved, econdf$water)

#now fit linear regression model
summary(lm(econdf$reserved~econdf$water))
#or (hannah told us this is better practice)
summary(lm(reserved~water, data=econdf))

#saving as an object
model <- summary(lm(reserved~water, data=econdf))
model

#making p value easier to read
#got all of this from tutorial script
sprintf("%.20f",2e-16)

#coefficient value is 0.0018240
#equation would be 0.0018240X + 0.3028613
#0.3028613 is the intercept
#this means that for every 1 increase in X, the expected
#average increase in Y is 0.0018240

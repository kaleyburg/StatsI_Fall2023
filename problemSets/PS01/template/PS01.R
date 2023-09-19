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

lapply(c(),  pkgTest)

#####################
# Problem 1
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

#mean of sample
y_mean <- mean(y)
print(y_mean)

#standard deviation of sample
y_sd <- sd(y)
print(y_sd)

#length of sample
n <- length(y)
print(n)

#standard error of sample
y_se <- y_sd/(sqrt(n))
print(y_se)


#A t-score multiplied by the estimated standard error 
#gives the margin of error for a confidence interval for the mean.
  #from the readings

#need to find t score for .90 confidence coefficient

#degrees of freedom for y
df_y <- n-1
print(df_y)

#t score for .90
#first thing is 1-conf coefficent / 2 (or alpha / 2)
#second thing is degrees of freedom
#third thing is looking at the upper tail because we did alpha/2?

?qt


t90 <- qt((1 - .90)/2, df_y, lower.tail = FALSE)

lower_90 <- y_mean - (t90 * (y_se))
upper_90 <- y_mean + (t90 * (y_se))


confint90 <- c(lower_90, upper_90)

print(confint90)


#part 2
#hypothesis test at alpha = 0.05 to see whether this sample is higher
#than the average IQ of 100

#pg 150 of textbook, formula to find t value is sample mean - population mean
#divided by the standard error
#standard error is std dev over root n

#already have se calculated from before

#Step 2: hypothesis:
  #null hypo: μ = 100
  #alt hypo: μ > 100

#step 3 calculate test statistic

y_t <- ((y_mean - 100)/y_se)

#This is a negative value and our alternative hypothesis only asks
#if it is GREATER than the population mean, so we already fail to 
#reject null hypothesis?
#but anyway here is calculating p anyway

absy_t <- abs((y_mean - 100)/y_se)
absy_t

#step 4 calculate p value

p_y <- pt(absy_t, df_y, lower.tail = F)
p_y

#step 5, we fail to reject the null hypothesis because our P value is
#.278 which means the probability of this occuring by chance was 27.8%
#and our alpha value was 0.05, so we fail to reject the null hypothesis

#just to check, going to find the critical t needed for alpha

t95 <- qt((1 - .95), df_y, lower.tail = F)
t95

#around 1.71 or higher would be needed to reject null
#what we have below approximately is not high enough, p too low

p_ytest <- pt(0.59, df_y, lower.tail = F)
p_ytest




#####################
# Problem 2
#####################

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2023/main/datasets/expenditure.txt", header=T)

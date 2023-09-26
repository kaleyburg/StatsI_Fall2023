#####################
# load libraries
# set wd
# clear global .envir
#####################
getwd()
setwd("C:/Users/kburg/OneDrive/Documents/GitHub/StatsI_Fall2023/problemSets
      /PS01/my_answers")
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
#in qt first element is 1-conf coefficent / 2 (or alpha / 2)
#second element is degrees of freedom
#third thing is looking at the upper tail because we used (1-.90)/2
  #(0.05) aka we are trying to find the t score for where 5% lies 
  #in the upper tail (this will give us a positive t score)
#so lower.tail = F in order 

?qt

t90 <- qt((1 - .90)/2, df_y, lower.tail = FALSE)
#OR we can do this which is using .95 instead (half of the confidence
#coefficient because we want to divide it between both tails)
#for this we are trying to find the t score where 95% of the area
#under the curve will fall, and so we are using lower.tail = T (
#which is the default for this function) and this will give us the
#same t score we got above

?qt
t90_alt <- qt(.95, df_y)

#now we can get the lower and upper bounds
#by adding and subtracting the t score minus the standard error to
#the mean

lower_90 <- y_mean - (t90 * (y_se))
upper_90 <- y_mean + (t90 * (y_se))

#now we just put it together into a single object

confint90 <- c(lower_90, upper_90)

#ta-da 

print(confint90)
round(confint90, 2)

#part 2
#hypothesis test at alpha = 0.05 to see whether this sample is higher
#than the average IQ of 100

#pg 150 of textbook, formula to find t value is sample mean - population mean
#divided by the standard error
#standard error is std dev over root n

#already have se calculated from before

#Step 2: hypothesis:
  #null hypo: μ </= 100
  #alt hypo: μ > 100

#step 3 calculate test statistic

y_t <- ((y_mean - 100)/y_se)
y_t
print(y_t)
#This is a negative value and our alternative hypothesis only asks
#if it is GREATER than the population mean, so we already fail to 
#reject null hypothesis?
#but anyway here is calculating p anyway


#step 4 calculate p value for this t score, we are looking in the
#upper tail because our null is that the mean IQ at the school
#is HIGHER than the population mean of 100

p_y <- pt(y_t, df_y, lower.tail = F)
p_y

#step 5, we fail to reject the null hypothesis because our P value is
#.722 which means the probability of this occuring by chance was 72.2%
#and our alpha value was 0.05, so we fail to reject the null hypothesis

#just to show this further, this is to find the critical value of t
#in order to reject the null in this example

t95 <- qt((1 - .95), df_y, lower.tail = F)
t95

#around 1.71 or higher would be needed to reject null
#our score of around -0.59 is a lot lower than this score

#we can also do it this way (from tutorial):

?t.test
t.test(y, mu = 100, alternative = "greater")

#we get the same thing, yay!


#####################
# Problem 2
#####################

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2023/main/datasets/expenditure.txt", header=T)
str(expenditure)
head(expenditure)


#plotting the relationships between Y, X1, X2, and X3

?plot

#https://thomasleeper.com/Rcourse/Tutorials/plotcolors.html
#found this website for setting colors

plot(expenditure$X1, expenditure$Y, pch=16,col = c("blue"))
plot(expenditure$X2, expenditure$Y, pch=16,col = c("red"))
plot(expenditure$X3, expenditure$Y, pch=16,col = c("green"))
plot(expenditure$X1, expenditure$X2, pch=16,col = c("orange"))
plot(expenditure$X1, expenditure$X3, pch=16,col = c("purple"))
plot(expenditure$X2, expenditure$X3, pch=16,col = c("pink"))

#all appear to be positively correlated with each other, except X2
#and Y seem to be slightly U shaped, same with X2 and X3

#plotting Y and Region

plot(expenditure$Region, expenditure$Y, pch=16,col = c("lightblue"))


reg_1 <- expenditure[expenditure$Region == "1",]
reg_2 <- expenditure[expenditure$Region == "2",]
reg_3 <- expenditure[expenditure$Region == "3",]
reg_4 <- expenditure[expenditure$Region == "4",]

reg_1_mean <- mean(reg_1$Y)
reg_2_mean <- mean(reg_2$Y)
reg_3_mean <- mean(reg_3$Y)
reg_4_mean <- mean(reg_4$Y)

reg_means <- c(reg_1_mean, reg_2_mean, reg_3_mean, reg_4_mean)

plot(reg_means)

#region 4 has the highest per capita expenditure on housing 
#assistance

#plotting the relationship between Y and X1
#I found this https://hbctraining.github.io/Intro-to-R/lessons/basic_plots_in_r.html#:~:text=Basic%20plots%20in%20R,function%20for%20plotting%20x%2Dy%20data.
plot(expenditure$X1, expenditure$Y, pch=16,col = c("lightgreen"))
plot(expenditure$X1, expenditure$Y, expenditure$Region, pch=16,col = c("blue"))
{plot(expenditure$X1, expenditure$Y, pch=16,
     col=c("red", "purple", "green", "blue")[expenditure$Region])
legend("topleft", pch=16, col=c("red", "purple", "green", "blue"),
       c("1", "2", "3", "4"), cex=0.8,
       title="Region")}

#ok so I couldn't find anywhere how to change the shapes
#of this plot, only the colors
#so here is that with ggplot

library(tidyverse)
#using code from coding camp
#and using code from here too https://sscc.wisc.edu/sscc/pubs/dvr/three-variables.html


expenditure %>%
  filter(Region %in% c("1", "2", "3", "4")) %>%
  group_by(Region) %>%
  ggplot(aes(X1, Y, color = as.factor(Region), shape = as.factor(Region))) +
  geom_point()
  theme_classic() +
  labs(title = "test")

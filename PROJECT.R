install.packages("car")
# loading the AAD dataset to read and analyze data from it
load("AAD.RData")
# displaying content of dataset
str(AAD)
# producing summaries of each attribute in AAD
summary(AAD)
# checking for nulls in dataset
sum(is.na(AAD))
# calculating mean, median, minimum, maximum, first and third quartile 
# for numerical variables. 
mean(AAD$D1.Shannon.diversity)
median(AAD$D1.Shannon.diversity)
min(AAD$D1.Shannon.diversity)
max(AAD$D1.Shannon.diversity)
quantile(AAD$D1.Shannon.diversity, c(0.25,0.75))

mean(AAD$D6.Shannon.diversity)
median(AAD$D6.Shannon.diversity)
min(AAD$D6.Shannon.diversity)
max(AAD$D6.Shannon.diversity)
quantile(AAD$D6.Shannon.diversity, c(0.25,0.75))

mean(AAD$D1.Chao1.diversity)
median(AAD$D1.Chao1.diversity)
min(AAD$D1.Chao1.diversity)
max(AAD$D1.Chao1.diversity)
quantile(AAD$D1.Chao1.diversity, c(0.25,0.75))

mean(AAD$D6.Chao1.diversity)
median(AAD$D6.Chao1.diversity)
min(AAD$D6.Chao1.diversity)
max(AAD$D6.Chao1.diversity)
quantile(AAD$D6.Chao1.diversity, c(0.25,0.75))

mean(AAD$D1.D6.Jaccard.distance)
median(AAD$D1.D6.Jaccard.distance)
min(AAD$D1.D6.Jaccard.distance)
max(AAD$D1.D6.Jaccard.distance)
quantile(AAD$D1.D6.Jaccard.distance, c(0.25,0.75))

# calculate a frequency table for the categorical variables existing
table(AAD$Antibiotic.class)
table(AAD$Outcome)
# Calculate the correlation coefficient between D1 Shannon and D6 Shannon  
cor(AAD$D1.Shannon.diversity, AAD$D6.Shannon.diversity)
# Calculate the correlation coefficient between D1 Chao D6 Chao
cor(AAD$D1.Chao1.diversity, AAD$D6.Chao1.diversity)

# Generate a bar chart of a categorical variable for the Outcome (AAD, CDI ,ND)
barplot(table(AAD$Outcome), xlab= "Outcome", ylab="Frequency", main="Outcome Barplot")

# Generate a bar chart graph with mean Outcome in BOL, FQ, OBL 
barplot(tapply(AAD$D1.Shannon.diversity,list(AAD$Outcome), mean), xlab="Antibiotics",
        ylab="Mean Outcome", main="Mean Outcome Barplot")

barplot(tapply(AAD$D6.Shannon.diversity,list(AAD$Antibiotic.class), mean), xlab="Antibiotics",
        ylab="Mean Outcome", main="Mean Outcome Barplot")

# Make a histogram of a continuous variable: “D1 Shannon” as well as “D6 Shannon”
hist(AAD$D1.Shannon.diversity, xlab="D1.Shannon", ylab="Frequency", 
     main="D1.Shannon Histogram")
hist(AAD$D6.Shannon.diversity, xlab="D6.Shannon", ylab="Frequency", 
     main="D6.Shannon Histogram")
# Make a scatterplot of 2 continuous variables D1 Shannon and D6 Shannon
plot(D1.Shannon.diversity[Antibiotic.class=="OBL"]~D6.Shannon.diversity[Antibiotic.class=="OBL"],
     data= AAD, xlab="D6.Shannon", ylab="D1.Shannon", main="D1.Shannon and D6.Shannon Scatterplot")

plot(AAD$D1.Shannon.diversity[AAD$Antibiotic.class=="FQN"],AAD$D6.Shannon.diversity[AAD$Antibiotic.class=="FQN"],
     col=6,xlab="D1.Shannon.diversity",ylab="D6.Shannon.diversity",main="Scatterplot")
points(D6.Shannon.diversity[Antibiotic.class=="FQN"]~D1.Shannon.diversity[Antibiotic.class=="FQN"],
     data= AAD)
points(D6.Shannon.diversity[Antibiotic.class=="PBL"]~D1.Shannon.diversity[Antibiotic.class=="PBL"],
     data= AAD)
# add the regression lines for each antibiotics
abline(lm(AAD$D6.Shannon.diversity[AAD$Antibiotic.class=="OBL"]~
            AAD$D1.Shannon.diversity[AAD$Antibiotic.class=="OBL"]))
abline(lm(AAD$D6.Shannon.diversity[AAD$Antibiotic.class=="FQN"]~
            AAD$D1.Shannon.diversity[AAD$Antibiotic.class=="FQN"]))
abline(lm(AAD$D6.Shannon.diversity[AAD$Antibiotic.class=="PBL"]~
            AAD$D1.Shannon.diversity[AAD$Antibiotic.class=="PBL"]))

# Make a boxplot of Jacard distance
boxplot(AAD$D1.D6.Jaccard.distance~AAD$Antibiotic.class,data=AAD,main="Boxplot per Antibiotics",
        xlab="Antibiotics",ylab="D1.D6.Jaccard.distance")

# make separate boxplots per Antbiotics

# Explore the data for any existing outliers, identify them using boxplot
# Outliers can be found in numerical variables and not categorical ones
boxplot(AAD$D1.Shannon.diversity, main="D1 Shannon Boxplot")
# extracting outliers in D1.Shannon and detecting their number
length(boxplot.stats(AAD$D1.Shannon.diversity)$out)
# there are about 12 outliers in D1.Shannon below the minimum value 

boxplot(AAD$D6.Shannon.diversity)
# extracting outliers in D6.Shannon and detecting their number
length(boxplot.stats(AAD$D6.Shannon.diversity)$out)
# there are about 26 outliers in D6.Shannon below the minimum value 

boxplot(AAD$D1.Chao1.diversity)
# extracting outliers in D1.Chao1 and detecting their number
length(boxplot.stats(AAD$D1.Chao1.diversity)$out)
# there are about 9 outliers in D1.Chao1 above the maximum value 

boxplot(AAD$D6.Chao1.diversity)
# extracting outliers in D6.Chao1 and detecting their number
length(boxplot.stats(AAD$D6.Chao1.diversity)$out)
# there are about 3 outliers in D6.Chao1 above the maximum value

boxplot(AAD$D1.D6.Jaccard.distance)
# extracting outliers in Jaccard distance and detecting their number
length(boxplot.stats(AAD$D1.D6.Jaccard.distance)$out)
# there are NO outliers in Jaccard distance

# Check the normality of variables using QQ plot
qqnorm(AAD$D1.Shannon.diversity)
qqline(AAD$D1.Shannon.diversity)

qqnorm(AAD$D6.Shannon.diversity)
qqline(AAD$D6.Shannon.diversity)

qqnorm(AAD$D1.Chao1.diversity)
qqline(AAD$D1.Chao1.diversity)

qqnorm(AAD$D6.Chao1.diversity)
qqline(AAD$D6.Chao1.diversity)

qqnorm(AAD$D1.D6.Jaccard.distance)
qqline(AAD$D1.D6.Jaccard.distance)

# Check the normality of variables using histogram
hist(AAD$D1.Shannon.diversity, xlab="D1 Shannon", main="D1 Shannon Histogram")
hist(AAD$D6.Shannon.diversity, xlab="D6 Shannon", main="D6 Shannon Histogram")
hist(AAD$D1.Chao1.diversity, xlab="D1 Chao1", main="D1 Chao1 Histogram")
hist(AAD$D6.Chao1.diversity, xlab="D6 Chao1", main="D6 Chao1 Histogram")
hist(AAD$D1.D6.Jaccard.distance, xlab="Jacard Distance", main="Jacard Distance Histogram")

# Check the normality of variables using Shapiro Test
shapiro.test(AAD$D1.Shannon.diversity)
shapiro.test(AAD$D6.Shannon.diversity)
shapiro.test(AAD$D1.Chao1.diversity)
shapiro.test(AAD$D6.Chao1.diversity)
shapiro.test(AAD$D1.D6.Jaccard.distance)
# according to the 3 tests done above on the numerical variables, data are found to 
# be not normal, where according to shapiro test, the p-value of all of them are
# less than 0.05 so there is a significant difference and we have enough evidence
# to reject the null hypothesis (normality).

# Check the homoscedasticity using Bartlett test test.
bartlett.test(list(AAD$D1.Shannon.diversity,AAD$D6.Shannon.diversity))
var.test(AAD$D1.Shannon.diversity,AAD$D6.Shannon.diversity)
# according to the bartlett and F tests between D1 and D6 Shannon, the p-value = 2.533e-06,
# which means that there is a significant difference so we have enough evidence to
# reject the null hypothesis, where the variance of both of them are not similar.

# Check the homoscedasticity using residuals plots 
plot(lm(D6.Chao1.diversity~D1.Chao1.diversity, data= AAD))

# Check the homoscedasticity using Bartlett test and F-test
bartlett.test(list(AAD$D1.Chao1.diversity,AAD$D6.Chao1.diversity))
var.test(AAD$D1.Chao1.diversity,AAD$D6.Chao1.diversity)
# according to the bartlett and F tests between D1 and D6 Chao1, the p-value = 0.03624
# which means that there is a significant difference so we have enough evidence
# to reject the null hypothesis, where the variance of both of them are not similar.


# Calculate the 90% confidence interval for the means of Jacard distance   
confint(lm(D1.D6.Jaccard.distance~Antibiotic.class, AAD), level=0.90)
# Calculate the 95% confidence interval for the means of Jacard distance   
confint(lm(D1.D6.Jaccard.distance~Antibiotic.class, AAD))
# Calculate the 99% confidence interval for the means of Jacard distance   
confint(lm(D1.D6.Jaccard.distance~Antibiotic.class, AAD), level=0.99)

# another way using t-test
t.test(AAD[AAD$Antibiotic.class=="OBL",]$D1.D6.Jaccard.distance,conf.level = .90)
# or by using another method

# sample mean, standard deviation, sample size, standard error of Jacard distance
sample_mean= mean(AAD[AAD$Antibiotic.class=="OBL",]$D1.D6.Jaccard.distance)
s= sd(tapply(AAD$D1.D6.Jaccard.distance, AAD$Antibiotic.class=="OBL", mean ))
n= length(tapply(AAD$D1.D6.Jaccard.distance, AAD$Antibiotic.class=="OBL", mean ))
SE= s/sqrt(n)
# Margin of error tells you how many percentage points your results will differ
# from the real population value. (Difference between uppermost limit of interval  
# and sample mean)
# Also it can be calculated by z*SE where z is the critical value from 
# the standard normal distribution corresponding to a 90% confidence level
# z = 1.96 for a 95% confidence level, z = 2.58 for a 99% confidence level z = 1.645
# for a 90% confidence level
margin= 1.645*SE
# calculating uppermost and lowermost levels of confidence intervals
lowerlimit= sample_mean-margin
upperlimit= sample_mean+margin
# 90 percent confidence interval: 0.6498326 - 0.6946749

# Calculate the 95% confidence interval for the means of Jacard distance 
# another way of calculating margin
margin= qt(0.975,df=n-1)*s/sqrt(n)
lowerlimit= sample_mean-margin
upperlimit= sample_mean+margin

# OR
t.test(AAD$D1.D6.Jaccard.distance)
# 95 percent confidence interval: 0.6372288 - 0.6708239

# Calculate the 99% confidence interval for the means of Jacard distance   
t.test(AAD$D1.D6.Jaccard.distance, conf.level = .99)

# OR
margin= 2.58*SE

lowerlimit= sample_mean-margin
upperlimit= sample_mean+margin

# 99 percent confidence interval: 0.6319042 - 0.6761484

# by increasing confidence level, lowermost limits decrease and uppermost limits  
# increase resulting in the increase of interval width to include as much
# values as possible, so consequently certainty increases.

# We hypothesis that Chao/Shannon at day 6 different between CDI vs ND.
t.test(AAD$D6.Chao1.diversity[AAD$Outcome=="CDI"], 
       AAD$D6.Chao1.diversity[AAD$Outcome=="ND"], var.equal=T)

t.test(AAD$D6.Shannon.diversity[AAD$Outcome=="CDI"], 
       AAD$D6.Shannon.diversity[AAD$Outcome=="ND"], var.equal=T)

# Assess whether the previous test assumptions have been met for the test.

# Anova
library(car)
library(report)
AnovaModel = aov(D1.D6.Jaccard.distance~Antibiotic.class, AAD)
summary(AnovaModel)
report(AnovaModel)
coef(AnovaModel)

# Posthoc
TukeyHSD(AnovaModel) 
plot(TukeyHSD(AnovaModel))


#_______ 7. Linear Regression  _______

#Fit a linear regression to the data and interpret the regression coefficient

#Multiple Regression:-
linear_reg <- lm(D6.Chao1.diversity~D1.Chao1.diversity+Antibiotic.class, data = AAD)

summary(linear_reg)
library(car)
avPlots(linear_reg)

# Calculate the confidence interval for the slope(Bonus)
confint(linear_reg, level = 0.95)
